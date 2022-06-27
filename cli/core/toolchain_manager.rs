use super::*;
use dashmap::DashMap;

#[derive(Debug, Clone, Default)]
pub struct ToolchainManager {
    configs: DashMap<String, RuleConfig>,
    toolchains: DashMap<Label, Target>,
}

impl ToolchainManager {
    pub fn new(cfgs: Vec<RuleConfig>) -> ToolchainManager {
        let configs = DashMap::new();
        for cfg in cfgs {
            configs.insert(cfg.name.clone(), cfg);
        }
        ToolchainManager {
            configs,
            toolchains: DashMap::new(),
        }
    }

    #[tracing::instrument(name = "ToolchainManager::register_toolchain", skip(self))]
    pub fn register_toolchain(&self, rule: Rule) {
        let label = Label::new(rule.name());
        if let Some(config) = self.configs.get(&label.name()) {
            let target = Target::new(label.clone(), &rule, config.value().clone());
            self.toolchains.insert(label, target);
        }
    }

    pub fn get(&self, label: &Label) -> Option<Target> {
        self.toolchains.get(&label).map(|r| r.value().clone())
    }

    /*
    pub fn targets(&self) -> Vec<Target> {
        let mut targets = vec![];
        for entry in self.toolchains.iter() {
            targets.push(entry.value().as_target().clone());
        }
        targets
    }

    pub fn active_toolchains(&self) -> Vec<Toolchain> {
        self.toolchains
            .iter()
            .map(|entry| entry.value().clone())
            .collect()
    }

    pub fn available_toolchains(&self) -> Vec<Label> {
        self.available_toolchains
            .iter()
            .map(|entry| entry.key().clone())
            .collect()
    }
    */
}
