use super::*;
use dashmap::DashMap;
use toml::Value;

#[derive(Clone, Default, Debug)]
pub struct WorkspaceAliases {
    aliases: DashMap<String, Value>,
}

impl WorkspaceAliases {
    pub fn new(aliases: DashMap<String, Value>) -> WorkspaceAliases {
        WorkspaceAliases { aliases }
    }

    pub fn handle_target(&self, alias: String) -> Label {
        if let Some(found_alias) = self.aliases.get(&alias) {
            let label: Label = found_alias.to_string().into();
            label
        } else {
            let label: Label = alias.into();
            label
        }
    }
}
