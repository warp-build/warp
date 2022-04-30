use super::*;
use anyhow::anyhow;
use log::*;

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
        LocalWorker {
            rule_exec_env: RuleExecEnv::new(&workspace),
            cache: LocalCache::new(&workspace),
            workspace,
            targets: vec![],
            dep_graph: DepGraph::default(),
        }
    }

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

    pub async fn prepare(&mut self, target: &Label) -> Result<(), anyhow::Error> {
        self.load_rules().await?;

        let mut targets = (*self.rule_exec_env.toolchain_manager)
            .read()
            .unwrap()
            .targets();

        for build_file in &self.workspace.build_files {
            let buildfile = Buildfile::from_file(
                &self.workspace.paths.workspace_root,
                &build_file,
                &self.rule_exec_env.rule_map,
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

    pub async fn execute(&mut self) -> Result<u32, anyhow::Error> {
        let mut walker = self.dep_graph.walk();

        let mut targets = 0;

        while let Some(idx) = walker.next(&self.dep_graph._inner_graph) {
            let target = self.dep_graph.get(idx);
            let sealed_target = self
                .rule_exec_env
                .compute_target(target, &self.dep_graph)?;
            let node = &self.dep_graph.put(idx, sealed_target);

            let name = node.label().clone();
            debug!("About to build {:?}...", name.to_string());
            debug!("with sources {:?}...", &node.srcs());
            debug!("with dependencies {:?}...", &node.deps());

            match self.cache.is_cached(&node)? {
                CacheHitType::Global => {
                    debug!("Skipping {}. Nothing to do.", name.to_string());
                    continue;
                }
                CacheHitType::Local => {
                    debug!("Skipping {}, but promoting outputs.", name.to_string());
                    self.cache
                        .promote_outputs(&node, &self.workspace.paths.local_outputs_root)?;
                    continue;
                }
                CacheHitType::Miss => {
                    debug!("Cache miss! Proceeding to build...");
                }
            }

            let result = if node.target.is_local() {
                let mut sandbox = LocalSandbox::for_node(&self.workspace, &node);
                match sandbox.run(&self.cache, &self.dep_graph)? {
                    ValidationStatus::Valid => {
                        self.cache.save(&sandbox)?;
                        sandbox.clear_sandbox()?;
                        targets += 1;
                        Ok(())
                    }
                    ValidationStatus::NoOutputs if node.outs().is_empty() => {
                        sandbox.clear_sandbox()?;
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
                )
            };

            result?
        }

        Ok(targets)
    }
}
