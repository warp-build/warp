/*
use super::*;
use anyhow::anyhow;
use dashmap::DashMap;
use futures::StreamExt;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::*;

/// A LocalWorker orchestrates a local build.
///
/// The LocalWorker is in charge of actually executing a BuildGraph in the
/// context of a Workspace.
///
/// This struct essentially has the core logic that defines how the system
/// builds your projects.
///
/// It will:
///
/// 1. Ready all the relevant toolchains for this particular BuildGraph
/// //2. Iterate over the BuildGraph, executing runnable rules
/// 3. Iterate over the BuildGraph, executing buildable rules in a Sandbox,
///    and updatting the Cache accordingly
///
pub struct LocalWorker {
    /// The workspace this worker is currently executing.
    pub workspace: Workspace,

    pub cache: LocalCache,

    pub rule_exec_env: RuleExecEnv,

    pub dep_graph: DepGraph,

    pub targets: Vec<Target>,
}

impl LocalWorker {
    pub fn from_workspace(workspace: Workspace) -> LocalWorker {
        let toolchain_provides_map: Arc<DashMap<Label, HashMap<String, String>>> =
            Arc::new(DashMap::new());
        LocalWorker {
            rule_exec_env: RuleExecEnv::new(&workspace, toolchain_provides_map),
            cache: LocalCache::new(&workspace),
            workspace,
            targets: vec![],
            dep_graph: DepGraph::default(),
        }
    }

    #[tracing::instrument(name = "LocalWorker::load_rules", skip(self))]
    async fn load_rules(&mut self) -> Result<(), anyhow::Error> {
        self.rule_exec_env.setup()?;
        let built_in_rules = zap_ext::TOOLCHAINS
            .iter()
            .chain(zap_ext::RULES.iter())
            .map(|(name, src)| (name.to_string(), src.to_string()));

        let custom_rules = (self.workspace.local_toolchains.iter())
            .chain(self.workspace.local_rules.iter())
            .map(|rule| {
                let name = format!("file://{}", rule.to_str().unwrap());
                let src = std::fs::read_to_string(rule).unwrap();
                (name, src)
            });

        for (name, src) in built_in_rules.chain(custom_rules) {
            self.rule_exec_env.load(&name, Some(src)).await?
        }
        Ok(())
    }

    #[tracing::instrument(name = "LocalWorker::prepare", skip(self))]
    pub async fn prepare(&mut self, target: &Label) -> Result<(), anyhow::Error> {
        self.load_rules().await?;

        let mut targets = (*self.rule_exec_env.toolchain_manager)
            .read()
            .unwrap()
            .targets();

        let scanner = WorkspaceScanner::from_workspace(&self.workspace);

        let mut buildfiles = scanner.find_build_files(10).await?;
        while let Some(build_file) = buildfiles.next().await {
            let buildfile = Buildfile::from_file(
                &self.workspace.paths.workspace_root,
                &build_file?,
                self.rule_exec_env.rule_map.clone(),
            )?;
            for target in buildfile.targets {
                targets.push(target);
            }
        }
        debug!("Found {} build targets...", targets.len());

        let mut dep_graph = DepGraph::from_targets(&targets)?;
        dep_graph.scope(&target)?;

        self.dep_graph = dep_graph;
        self.targets = targets;

        Ok(())
    }

    #[tracing::instrument(name = "LocalWorker::compute_nodes", skip(self))]
    pub async fn compute_nodes(&mut self) -> Result<Vec<ComputedTarget>, anyhow::Error> {
        let mut walker = self.dep_graph.walk();

        let mut nodes = vec![];
        while let Some(idx) = walker.next(&self.dep_graph._inner_graph) {
            let target = self.dep_graph.get(idx);
            let sealed_target = {
                let find_node = |label| (&self.dep_graph).find_node(&label).clone();
                self.rule_exec_env.compute_target(target, &find_node)?
            };
            self.dep_graph.put(idx, sealed_target.clone());
            nodes.push(sealed_target);
        }

        Ok(nodes)
    }

    pub async fn run(&mut self, event_channel: Arc<EventChannel>) -> Result<u32, anyhow::Error> {
        self.execute(ExecutionMode::BuildAndRun, event_channel)
            .await
    }

    pub async fn build(&mut self, event_channel: Arc<EventChannel>) -> Result<u32, anyhow::Error> {
        self.execute(ExecutionMode::OnlyBuild, event_channel).await
    }

    #[tracing::instrument(name = "LocalWorker::execute", skip(self))]
    pub async fn execute(
        &mut self,
        mode: ExecutionMode,

        event_channel: Arc<EventChannel>,
    ) -> Result<u32, anyhow::Error> {
        let mut walker = self.dep_graph.walk();

        let mut targets = 0;

        while let Some(idx) = walker.next(&self.dep_graph._inner_graph) {
            let target = self.dep_graph.get(idx);
            let sealed_target = {
                let find_node = |label| (&self.dep_graph).find_node(&label).clone();
                self.rule_exec_env.compute_target(target, &find_node)?
            };
            let node = &self.dep_graph.put(idx, sealed_target);

            let name = node.label().clone();

            match self.cache.is_cached(&node).await? {
                CacheHitType::Global(_) => {
                    debug!("Skipping {}. Nothing to do.", name.to_string());
                    continue;
                }
                CacheHitType::Local(_) => {
                    if node.target.kind() == TargetKind::Runnable
                        && mode == ExecutionMode::BuildAndRun
                    {
                        debug!("Skipping {}, we're in running mode.", name.to_string());
                    } else {
                        debug!("Skipping {}, but promoting outputs.", name.to_string());
                        self.cache
                            .promote_outputs(&node, &self.workspace.paths.local_outputs_root)
                            .await?;
                        continue;
                    }
                }
                CacheHitType::Miss { .. } => {
                    debug!("Cache miss! Proceeding to build...");
                }
            }

            let result = if node.target.is_local() {
                let mut sandbox = LocalSandbox::for_node(&self.workspace, node.clone());

                let result = {
                    let find_node = |label| (&self.dep_graph).find_node(&label).clone();
                    sandbox
                        .run(&self.cache, &find_node, mode, event_channel.clone())
                        .await?
                };

                match result {
        ValidationStatus::Valid => {
        self.cache.save(&sandbox).await?;
        // sandbox.clear_sandbox()?;
        targets += 1;
        Ok(())
        }
        ValidationStatus::NoOutputs if node.outs().is_empty() => {
        // sandbox.clear_sandbox()?;
        targets += 1;
        Ok(())
        }
        ValidationStatus::NoOutputs => Err(anyhow!(
        "Expected {} outputs, but found none.",
        node.outs().len()
        )),
        ValidationStatus::Pending => Err(anyhow!(
        "Node {} is somehow still pending...",
        &name.to_string()
        )),
        ValidationStatus::Invalid {
        expected_but_missing,
        unexpected_but_present,
        ..
        } => Err(
        anyhow!("Node {} expected the following but missing outputs: {:?}\n\ninstead it found the following unexpected outputs: {:?}",
        &name.to_string(), expected_but_missing, unexpected_but_present)),
        }
            } else {
                debug!("Building global target...");
                node.execute(
                    &self.workspace.paths.global_archive_root,
                    &self.workspace.paths.global_cache_root,
                    &self.workspace.paths.global_sandbox_root,
                    ExecutionMode::OnlyBuild,
                    event_channel.clone(),
                )
                .await
            };

            result?
        }

        Ok(targets)
    }
}
*/
