use crate::sync::*;
use crate::Config;
use dashmap::DashMap;
use std::path::{Path, PathBuf};
use thiserror::Error;
use tracing::*;
use url::Url;

/// A registry of tricorders with methods to find the right one based on the existence of certain
/// paths.
///
#[derive(Debug, Clone)]
pub struct TricorderRegistry {
    tricorders: DashMap<String, Arc<Url>>,
}

impl TricorderRegistry {
    pub fn new(config: Config) -> Self {
        // NOTE(@ostera): This is currently hard-coded as we are only working with the Erlang/Elixir
        // tricorders, but should become a remote registry like the toolchains registry in the future.
        let default_host = config.public_store_metadata_url().to_string();
        let data = [
            (
                format!("{default_host}tricorders/test/manifest.json")
                    .parse::<Url>()
                    .unwrap(),
                vec!["warp_test"],
            ),
            (
                format!("{default_host}tricorders/beam/manifest.json")
                    .parse::<Url>()
                    .unwrap(),
                vec!["ex", "exs", "erl", "hrl", "rebar.config", "erl.mk"],
            ),
            (
                format!("{default_host}tricorders/rust/manifest.json")
                    .parse::<Url>()
                    .unwrap(),
                vec!["rs", "Cargo.toml"],
            ),
        ];

        let tricorders = DashMap::default();

        for (name, exts) in data {
            let name = Arc::new(name);
            for ext in exts {
                tricorders.insert(ext.to_string(), name.clone());
            }
        }

        Self { tricorders }
    }

    #[tracing::instrument(name = "TricorderRegistry::find_by_path", skip(self), ret)]
    pub fn find_by_path(&self, path: &Path) -> Result<Option<Url>, TricorderRegistryError> {
        if let Some(ext) = path.extension() {
            if let Some(url) = self.find_tricorder(ext.to_str().unwrap()) {
                return Ok(Some(url));
            };
        };

        if let Some(file_name) = path.file_name() {
            if let Some(url) = self.find_tricorder(file_name.to_str().unwrap()) {
                return Ok(Some(url));
            };
        }

        Ok(None)
    }

    fn find_tricorder(&self, name: &str) -> Option<Url> {
        if let Some(tricorder) = self.tricorders.get(name) {
            return Some((**tricorder).clone());
        }
        None
    }
}

#[derive(Error, Debug)]
pub enum TricorderRegistryError {
    #[error("Could not find tricorder when searching with path {path:?}")]
    CouldNotFindTricorderByPath { path: PathBuf },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_tricorder_by_file_name() {
        let config = Config::default();
        let tricorder = TricorderRegistry::new(config);

        let url = tricorder
            .find_by_path(Path::new("tricorders/rust/Cargo.toml"))
            .unwrap()
            .unwrap();

        assert_eq!(
            url,
            "https://store.warp.build/tricorders/rust/manifest.json"
                .parse()
                .unwrap()
        );
    }
}
