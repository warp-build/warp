use super::*;
use log::*;
use dashmap::DashMap;
use std::path::PathBuf;

#[derive(Debug, Clone, Default)]
pub struct ToolchainManager {
    toolchains: DashMap<String, Toolchain>,
    archives: DashMap<String, Archive>,
    available_toolchains: DashMap<Label, ()>,
}

impl ToolchainManager {
    pub fn new(arcs: Vec<Archive>) -> ToolchainManager {
        let mut archives = DashMap::new();
        for archive in arcs {
            archives.insert(archive.name().to_string(), archive);
        }
        ToolchainManager {
            toolchains: DashMap::new(),
            available_toolchains: DashMap::new(),
            archives
        }
    }

    pub fn register_toolchain(&self, rule: Rule, cache_root: PathBuf) {
        let label = Label::new(rule.name());
        if let Some(archive) = self.archives.get(&label.name()) {
            let toolchain =
                Toolchain::new(rule, archive.value().clone().with_cache_root(cache_root));
            self.toolchains.insert(label.to_string(), toolchain);
        } else {
            self.available_toolchains.insert(label, ());
        }
    }

    pub fn get(&self, label: &str) -> Option<Toolchain> {
        self.toolchains.get(label).map(|r| r.value().clone())
    }

    pub fn get_archive(&self, name: &str) -> Option<Archive> {
        self.archives.get(name).map(|r| r.value().clone())
    }

    pub fn targets(&self) -> Vec<Target> {
        let mut targets = vec![];
        for entry in self.toolchains.iter() {
            targets.push(entry.value().as_target().clone());
        }
        targets
    }

    pub fn archives(&self) -> Vec<Archive> {
        self.archives
            .iter()
            .map(|entry| entry.value().clone())
            .collect()
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
}
