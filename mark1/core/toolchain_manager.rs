use super::*;
use dashmap::DashMap;
use std::sync::Arc;
use thiserror::*;

#[derive(Debug, Clone)]
pub struct ToolchainManager {
    toolchains: DashMap<LabelId, RuleConfig>,
    label_registry: Arc<LabelRegistry>,
}

#[derive(Error, Debug)]
pub enum ToolchainManagerError {}

impl ToolchainManager {
    pub fn new(
        workspace: &Workspace,
        label_registry: Arc<LabelRegistry>,
    ) -> Result<Self, ToolchainManagerError> {
        let toolchains = DashMap::new();

        for (string, rule_config) in workspace.toolchain_configs.clone() {
            let label: Label = format!("https://rules.warp.build/toolchains/{}", string)
                .parse()
                .unwrap();
            let label_id = label_registry.register_label(label);
            toolchains.insert(label_id, rule_config);
        }

        Ok(Self {
            toolchains,
            label_registry,
        })
    }

    pub fn get(&self, label: LabelId) -> Option<RuleConfig> {
        self.toolchains.get(&label).map(|r| (*r).clone())
    }

    // TODO(@ostera): this should be part of the WorkspaceManager
    pub async fn register_workspace(
        &self,
        workspace: &Workspace,
    ) -> Result<(), ToolchainManagerError> {
        let toolchain_manager = Self::new(workspace, self.label_registry.clone())?;
        for (label_id, dep) in toolchain_manager.toolchains.into_iter() {
            if self.toolchains.contains_key(&label_id) {
                continue;
            }
            self.toolchains.insert(label_id, dep);
        }
        Ok(())
    }
}
