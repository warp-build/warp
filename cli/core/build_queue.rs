use super::Event;
use super::*;
use dashmap::DashSet;
use futures::StreamExt;
use std::sync::Arc;
use thiserror::*;
use tracing::*;

#[derive(Error, Debug)]
pub enum QueueError {
    #[error("Cannot queue the //... target")]
    CannotQueueTargetAll,

    #[error(transparent)]
    WorkspaceScannerError(WorkspaceScannerError),

    #[error(transparent)]
    FileScannerError(FileScannerError),

    #[error(transparent)]
    BuildfileError(anyhow::Error),

    #[error(transparent)]
    DependencyCycle(BuildResultError),
}

/// A thread-safe queue for compilation targets, to be consumed by workers.
///
#[derive(Debug)]
pub struct BuildQueue {
    workspace: Workspace,

    /// Targets currently being built.
    busy_targets: Arc<DashSet<LabelId>>,

    /// Targets currently being built.
    in_queue_targets: Arc<DashSet<LabelId>>,

    /// The queue from which workers pull work.
    inner_queue: Arc<crossbeam::deque::Injector<Task>>,

    /// A backup queue used for set-aside targets.
    wait_queue: Arc<crossbeam::deque::Injector<Task>>,

    /// Targets already built.
    build_results: Arc<BuildResults>,

    /// An event channel where we can publish queue events.
    event_channel: Arc<EventChannel>,

    /// The labels that have been successfully queued.
    all_queued_labels: Arc<DashSet<LabelId>>,

    label_registry: Arc<LabelRegistry>,

    build_opts: BuildOpts,
}

impl BuildQueue {
    #[tracing::instrument(
        name = "BuildQueue::new",
        skip(event_channel, build_results, label_registry)
    )]
    pub fn new(
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        workspace: Workspace,
        label_registry: Arc<LabelRegistry>,
        build_opts: BuildOpts,
    ) -> BuildQueue {
        BuildQueue {
            workspace,
            busy_targets: Arc::new(DashSet::new()),
            in_queue_targets: Arc::new(DashSet::new()),
            inner_queue: Arc::new(crossbeam::deque::Injector::new()),
            wait_queue: Arc::new(crossbeam::deque::Injector::new()),
            build_results,
            event_channel,
            all_queued_labels: Arc::new(DashSet::default()),
            label_registry,
            build_opts,
        }
    }

    #[tracing::instrument(name = "BuildQueue::next", skip(self))]
    pub fn next(&self) -> Option<Task> {
        loop {
            let task = if let crossbeam::deque::Steal::Success(task) = self.inner_queue.steal() {
                task
            } else if self.busy_targets.is_empty() {
                if let crossbeam::deque::Steal::Success(task) = self.wait_queue.steal() {
                    task
                } else {
                    return None;
                }
            } else {
                return None;
            };

            // If the target is already computed or being computed, we can skip it
            // and try to fetch the next one immediately.
            //
            // When the queue empties up, this will return a None, but otherwise
            // we'll go through a bunch of duplicates, discarding them.
            if self.busy_targets.contains(&task.label)
                || self.build_results.is_label_built(task.label)
            {
                continue;
            }

            // But if it is yet to be built, we mark it as busy
            self.busy_targets.insert(task.label);
            self.in_queue_targets.remove(&task.label);
            return Some(task);
        }
    }

    #[tracing::instrument(name = "BuildQueue::ack", skip(self))]
    pub fn ack(&self, task: Task) {
        self.busy_targets.remove(&task.label);
    }

    #[tracing::instrument(name = "BuildQueue::nack", skip(self))]
    pub fn nack(&self, task: Task) {
        self.busy_targets.remove(&task.label);
        self.wait_queue.push(task);
    }

    #[tracing::instrument(name = "BuildQueue::is_queue_empty", skip(self))]
    pub fn is_empty(&self) -> bool {
        self.inner_queue.is_empty()
    }

    #[tracing::instrument(name = "BuildQueue::queue", skip(self))]
    pub fn queue(&self, task: Task) -> Result<(), QueueError> {
        let label = task.label;
        debug!(
            "Queued {:?} {:#?}",
            label,
            self.label_registry.get_label(label)
        );
        if self.label_registry.get_label(label).is_all() {
            return Err(QueueError::CannotQueueTargetAll);
        }
        if self.build_results.is_label_built(label)
            || self.busy_targets.contains(&label)
            || self.in_queue_targets.contains(&label)
        {
            return Ok(());
        }
        self.build_results.add_expected_target(label);
        self.in_queue_targets.insert(label);
        self.inner_queue.push(task);
        self.all_queued_labels.insert(label);
        Ok(())
    }

    pub fn queue_deps(&self, task: Task, deps: &[LabelId]) -> Result<(), QueueError> {
        self.build_results
            .add_dependencies(task.label, deps)
            .map_err(QueueError::DependencyCycle)?;

        for dep in deps {
            self.queue(Task::build(*dep))?;
        }

        Ok(())
    }

    pub async fn queue_entire_workspace(&self) -> Result<usize, QueueError> {
        debug!("Queueing all targets...");
        self.event_channel.send(Event::QueueingWorkspace);

        let mut buildfiles = self
            .workspace
            .scanner()
            .find_build_files(self.build_opts.concurrency_limit)
            .await
            .map_err(QueueError::WorkspaceScannerError)?;

        while let Some(buildfile_path) = buildfiles.next().await {
            let buildfile_path = buildfile_path.map_err(QueueError::FileScannerError)?;

            let label = Label::builder()
                .with_workspace(&self.workspace)
                .from_path(buildfile_path.clone())
                .unwrap();

            let buildfile = Buildfile::from_label(&label).await;

            match buildfile {
                Err(err) => {
                    self.event_channel
                        .send(Event::BadBuildfile(buildfile_path, err));
                    continue;
                }
                Ok(buildfile) => {
                    for target in buildfile.targets {
                        if self.build_opts.goal.includes(&target) {
                            let label = target.label.change_workspace(&self.workspace);
                            let label = self.label_registry.register_label(&label);
                            let goal = self.build_opts.goal;
                            self.queue(Task { label, goal })?;
                        }
                    }
                }
            }
        }

        let target_count = self.all_queued_labels.len();
        self.event_channel
            .send(Event::QueuedTargets(target_count as u64));
        Ok(target_count)
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::BTreeMap, path::PathBuf};

    use super::*;

    async fn make_target(label: LabelId) -> ExecutableTarget {
        let rule = Rule::new(
            "test_rule".to_string(),
            "TestRule".to_string(),
            vec![],
            ConfigSpec::default(),
            RuleConfig::default(),
            Runnable::NotRunnable,
            Pinned::Pinned,
            Portability::Portable,
        );
        let cfg = RuleConfig::default();
        let target = Target::new(label, &rule.name, cfg);
        ExecutableTarget::new(
            &ExecutionEnvironment::new(),
            &rule,
            &target,
            &[],
            &[],
            &[],
            ExecutionResult::default(),
        )
        .await
        .unwrap()
    }

    async fn make_manifest(target: &ExecutableTarget) -> TargetManifest {
        TargetManifest::from_validation_result(
            chrono::Utc::now(),
            &ValidationStatus::Cached,
            &PathBuf::from("."),
            BTreeMap::default(),
            target,
        )
    }

    #[test]
    fn queue_emptiness() {
        let final_target = LabelId::new("//test/0");
        let q = BuildQueue::new(
            final_target.clone(),
            Arc::new(BuildResults::new()),
            Arc::new(EventChannel::new()),
            Workspace::default(),
        );

        assert!(q.is_empty());
        q.queue(final_target).unwrap();
        assert!(!q.is_empty());
        q.next().unwrap();
        assert!(q.is_empty());
    }

    #[test]
    fn contiguous_duplicates_are_discarded() {
        let final_target = LabelId::new("//test/0");
        let q = BuildQueue::new(
            final_target.clone(),
            Arc::new(BuildResults::new()),
            Arc::new(EventChannel::new()),
            Workspace::default(),
        );

        assert!(q.is_empty());
        // first good target
        q.queue(final_target.clone()).unwrap();
        // queue duplicates!
        q.queue(final_target.clone()).unwrap();
        q.queue(final_target.clone()).unwrap();
        q.queue(final_target).unwrap();

        // our queue should have stuff in it
        assert!(!q.is_empty());

        // so we can get the first instance of our label
        q.next().unwrap();
        // and we should see it is empty because we didn't immediately queue duplicates
        assert!(q.is_empty());
    }

    #[tokio::test]
    async fn already_built_targets_are_ignored_when_queueing_new_targets() {
        let final_target = LabelId::new("//test/0");
        let br = Arc::new(BuildResults::new());
        let q = BuildQueue::new(
            final_target.clone(),
            br.clone(),
            Arc::new(EventChannel::new()),
            Workspace::default(),
        );

        assert!(q.is_empty());
        // we load up a target that is already built
        let target = make_target(final_target.clone()).await;
        let manifest = make_manifest(&target).await;
        br.add_computed_target(final_target.clone(), manifest, target);
        q.queue(final_target).unwrap();

        // our queue should still be empty
        assert!(q.is_empty());
    }

    #[tokio::test]
    async fn once_a_target_is_built_we_discard_it() {
        let final_target = LabelId::new("//test/0");
        let br = Arc::new(BuildResults::new());
        let q = BuildQueue::new(
            final_target.clone(),
            br.clone(),
            Arc::new(EventChannel::new()),
            Workspace::default(),
        );

        assert!(q.is_empty());
        // we load up a target that hasn't been built yet
        q.queue(final_target.clone()).unwrap();
        q.queue(final_target.clone()).unwrap();
        q.queue(final_target.clone()).unwrap();
        assert!(!q.is_empty());

        // then it gets built
        let target = make_target(final_target.clone()).await;
        let manifest = make_manifest(&target).await;
        br.add_computed_target(final_target.clone(), manifest, target);
        // so every future queue entry for that target gets discarded
        assert!(q.next().is_none());
        // and we'll have an empty queue
        assert!(q.is_empty());
    }

    #[test]
    fn nexting_a_target_requires_a_nack_to_get_it_again() {
        let final_target = LabelId::new("//test/0");
        let br = Arc::new(BuildResults::new());
        let q = BuildQueue::new(
            final_target.clone(),
            br,
            Arc::new(EventChannel::new()),
            Workspace::default(),
        );

        assert!(q.is_empty());
        // we load up a target that hasn't been built yet
        q.queue(final_target.clone()).unwrap();
        q.queue(final_target.clone()).unwrap();
        assert!(!q.is_empty());

        // when we next-it the first time, we get the final target out
        assert!(q.next().is_some());
        // if we next-it again, it'll be skipped
        assert!(q.next().is_none());
        // but if we nack it, we are telling the queue this target needs
        // to be consumed again
        q.nack(final_target);
        // so we can consume it
        assert!(q.next().is_some());
    }
}
