use crate::{sync::*, Config};
use fxhash::FxHashMap;
use std::path::{Path, PathBuf};
use thiserror::*;
use url::Url;

use crate::store::ManifestUrl;

/// A registry of tricorders with methods to find the right one based on the existence of certain
/// paths.
///
pub struct TricorderRegistry {
    tricorders: FxHashMap<String, Arc<Url>>,
}

impl TricorderRegistry {
    pub fn new(config: Config) -> Self {
        // NOTE(@ostera): This is currently hard-coded as we are only working with the Erlang/Elixir
        // tricorders, but should become a remote registry like the toolchains registry in the future.
        let default_host = config.public_store_metadata_url().to_string();
        let data = [(
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
        )];

        let mut tricorders = FxHashMap::default();

        for (name, exts) in data {
            let name = Arc::new(name);
            for ext in exts {
                tricorders.insert(ext.to_string(), name.clone());
            }
        }

        Self { tricorders }
    }

    pub async fn find_by_path(&self, path: &Path) -> Result<ManifestUrl, TricorderRegistryError> {
        let ext = path.extension().unwrap().to_str().unwrap();
        if let Some(tricorder) = self.tricorders.get(ext) {
            let tricorder = tricorder.clone();
            let tricorder = (*tricorder).clone();
            return Ok(tricorder.try_into().unwrap());
        }
        Err(TricorderRegistryError::CouldNotFindTricorderByPath { path: path.into() })
    }
}

#[derive(Error, Debug)]
pub enum TricorderRegistryError {
    #[error("Could not find tricorder when searching with path {path:?}")]
    CouldNotFindTricorderByPath { path: PathBuf },
}
