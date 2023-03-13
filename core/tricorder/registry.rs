use crate::store::ManifestUrl;
use crate::{sync::*, Config};
use fxhash::FxHashMap;
use std::path::{Path, PathBuf};
use thiserror::*;
use tracing::*;
use url::Url;

/// A registry of tricorders with methods to find the right one based on the existence of certain
/// paths.
///
#[derive(Debug)]
pub struct TricorderRegistry {
    tricorders: FxHashMap<String, Arc<Url>>,
}

impl TricorderRegistry {
    pub fn new(config: Config) -> Self {
        // NOTE(@ostera): This is currently hard-coded as we are only working with the Erlang/Elixir
        // tricorders, but should become a remote registry like the toolchains registry in the future.
        let default_host = config.public_store_metadata_url().to_string();
        let data = [
            (
                format!("{default_host}tricorder/test/manifest.json")
                    .parse::<Url>()
                    .unwrap(),
                vec!["warp_test"],
            ),
            (
                format!("{default_host}tricorder/beam/manifest.json")
                    .parse::<Url>()
                    .unwrap(),
                vec![
                    "ex",
                    "exs",
                    "erl",
                    "hrl",
                    "rebar.config",
                    "erl.mk",
                    "rebar.lock",
                    "mix.lock",
                ],
            ),
            (
                format!("{default_host}tricorder/rust/manifest.json")
                    .parse::<Url>()
                    .unwrap(),
                vec!["rs", "Cargo.toml"],
            ),
        ];

        let mut tricorders = FxHashMap::default();

        for (name, exts) in data {
            let name = Arc::new(name);
            for ext in exts {
                tricorders.insert(ext.to_string(), name.clone());
            }
        }

        Self { tricorders }
    }

    #[tracing::instrument(name = "TricorderRegistry::find_by_path", skip(self))]
    pub async fn find_by_path(
        &self,
        path: &Path,
    ) -> Result<Option<ManifestUrl>, TricorderRegistryError> {
        if let Some(ext) = path.extension() {
            return Ok(self.find_tricorder(ext.to_str().unwrap()));
        };

        if let Some(file_name) = path.file_name() {
            return Ok(self.find_tricorder(file_name.to_str().unwrap()));
        }

        Ok(None)
    }

    fn find_tricorder(&self, name: &str) -> Option<ManifestUrl> {
        if let Some(tricorder) = self.tricorders.get(name) {
            let tricorder = tricorder.clone();
            let tricorder = (*tricorder).clone();
            return Some(tricorder.try_into().unwrap());
        }
        None
    }
}

#[derive(Error, Debug)]
pub enum TricorderRegistryError {
    #[error("Could not find tricorder when searching with path {path:?}")]
    CouldNotFindTricorderByPath { path: PathBuf },
}
