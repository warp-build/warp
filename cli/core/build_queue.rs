use super::Event;
use super::*;
use dashmap::DashMap;
use futures::StreamExt;
use std::sync::atomic::*;
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
    busy_targets: Arc<DashMap<Label, ()>>,

    /// Targets currently being built.
    in_queue_targets: Arc<DashMap<Label, ()>>,

    /// The queue from which workers pull work.
    inner_queue: Arc<crossbeam::deque::Injector<Label>>,

    /// A backup queue used for set-aside targets.
    wait_queue: Arc<crossbeam::deque::Injector<Label>>,

    /// Targets already built.
    build_results: Arc<BuildResults>,

    /// An event channel where we can publish queue events.
    event_channel: Arc<EventChannel>,

    /// The number of targets that have been successfully queued.
    target_count: AtomicUsize,
}

impl BuildQueue {
    #[tracing::instrument(name = "BuildQueue::new", skip(event_channel, build_results))]
    pub fn new(
        target: Label,
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        workspace: Workspace,
    ) -> BuildQueue {
        BuildQueue {
            inner_queue: Arc::new(crossbeam::deque::Injector::new()),
            wait_queue: Arc::new(crossbeam::deque::Injector::new()),
            in_queue_targets: Arc::new(DashMap::new()),
            busy_targets: Arc::new(DashMap::new()),
            target_count: AtomicUsize::new(0),
            event_channel,
            build_results,
            workspace,
        }
    }

    #[tracing::instrument(name = "BuildQueue::next", skip(self))]
    pub fn next(&self) -> Option<Label> {
        if let crossbeam::deque::Steal::Success(label) = self.inner_queue.steal() {
            return self.handle_next(label);
        }
        if let crossbeam::deque::Steal::Success(label) = self.wait_queue.steal() {
            return self.handle_next(label);
        }
        None
    }

    fn handle_next(&self, label: Label) -> Option<Label> {
        // If the target is already computed or being computed, we can skip it
        // and try to fetch the next one immediately.
        //
        // When the queue empties up, this will return a None, but otherwise
        // we'll go through a bunch of duplicates, discarding them.
        if self.build_results.is_target_built(&label) || self.busy_targets.contains_key(&label) {
            return self.next();
        }
        // But if it is yet to be built, we mark it as busy
        self.busy_targets.insert(label.clone(), ());
        self.in_queue_targets.remove(&label);
        Some(label)
    }

    pub fn ack(&self, label: &Label) {
        self.busy_targets.remove(label);
    }

    pub fn nack(&self, label: Label) {
        self.busy_targets.remove(&label);
        self.wait_queue.push(label);
    }

    #[tracing::instrument(name = "BuildQueue::is_queue_empty", skip(self))]
    pub fn is_empty(&self) -> bool {
        self.inner_queue.is_empty()
    }

    #[tracing::instrument(name = "BuildQueue::queue", skip(self))]
    pub fn queue(&self, label: Label) -> Result<(), QueueError> {
        if label.is_all() {
            return Err(QueueError::CannotQueueTargetAll);
        }
        if self.build_results.is_target_built(&label)
            || self.busy_targets.contains_key(&label)
            || self.in_queue_targets.contains_key(&label)
        {
            return Ok(());
        }
        self.build_results.add_expected_target(label.clone());
        self.in_queue_targets.insert(label.clone(), ());
        self.inner_queue.push(label);
        Ok(())
    }

    pub fn queue_deps(&self, label: &Label, deps: &[Label]) -> Result<(), QueueError> {
        self.build_results
            .add_dependencies(label.clone(), deps)
            .map_err(QueueError::DependencyCycle)?;

        for dep in deps {
            self.queue(dep.clone())?;
        }

        Ok(())
    }

    pub async fn queue_entire_workspace(&self, max_concurrency: usize) -> Result<(), QueueError> {
        debug!("Queueing all targets...");
        self.event_channel.send(Event::QueueingWorkspace);

        let mut buildfiles = self
            .workspace
            .scanner()
            .find_build_files(max_concurrency)
            .await
            .map_err(QueueError::WorkspaceScannerError)?;

        while let Some(path) = buildfiles.next().await {
            let path = path.map_err(QueueError::FileScannerError)?;
            let relative_path = path
                .parent()
                .unwrap()
                .strip_prefix(&self.workspace.paths.workspace_root)
                .unwrap();

            let buildfile = Buildfile::from_file(&path, relative_path).await;

            match buildfile {
                Err(err) => {
                    self.event_channel.send(Event::BadBuildfile(path, err));
                    continue;
                }
                Ok(buildfile) => {
                    for target in buildfile.targets {
                        self.queue(target.label.clone())?;
                    }
                }
            }
        }
        let target_count = self.target_count.load(Ordering::Acquire);
        self.event_channel
            .send(Event::QueuedTargets(target_count.try_into().unwrap()));
        debug!("Queued {} targets...", target_count);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    async fn dummy_target(label: Label) -> ExecutableTarget {
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
            ExecutionResult::default(),
        )
        .await
        .unwrap()
    }

    #[test]
    fn queue_emptiness() {
        let final_target = Label::new("//test/0");
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
        let final_target = Label::new("//test/0");
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
        let final_target = Label::new("//test/0");
        let br = Arc::new(BuildResults::new());
        let q = BuildQueue::new(
            final_target.clone(),
            br.clone(),
            Arc::new(EventChannel::new()),
            Workspace::default(),
        );

        assert!(q.is_empty());
        // we load up a target that is already built
        br.add_computed_target(
            final_target.clone(),
            dummy_target(final_target.clone()).await,
        );
        q.queue(final_target).unwrap();

        // our queue should still be empty
        assert!(q.is_empty());
    }

    #[tokio::test]
    async fn once_a_target_is_built_we_discard_it() {
        let final_target = Label::new("//test/0");
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
        br.add_computed_target(
            final_target.clone(),
            dummy_target(final_target.clone()).await,
        );
        // so every future queue entry for that target gets discarded
        assert!(q.next().is_none());
        // and we'll have an empty queue
        assert!(q.is_empty());
    }

    #[test]
    fn nexting_a_target_requires_a_nack_to_get_it_again() {
        let final_target = Label::new("//test/0");
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
