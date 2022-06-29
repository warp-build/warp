use super::*;
use toml::{map::Map, Value};

#[derive(Clone, Default, Debug)]
pub struct WorkspaceAliases {
    aliases: Map<String, Value>,
}

impl WorkspaceAliases {
    pub fn new(aliases: Map<String, Value>) -> WorkspaceAliases {
        WorkspaceAliases { aliases }
    }

    pub fn empty() -> WorkspaceAliases {
        WorkspaceAliases {
            aliases: toml::map::Map::new(),
        }
    }

    pub fn fetch_target(&self, alias: String) -> Label {
        if let Some(found_alias) = self.aliases.get(&alias) {
            let label: Label = found_alias.to_string().into();
            label
        } else {
            let label: Label = alias.into();
            label
        }
    }
}
