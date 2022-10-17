use super::*;
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::io::BufReader;
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::*;
use tokio::fs;
use url::Url;

pub const REMOTE_TOOLCHAINS_REGISTRY_URL: &str = "https://pkgs.warp.build/toolchains/registry.json";

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct ToolchainId {
    pub language: String,
    pub version: String,
}

#[derive(Debug, Clone)]
pub struct Toolchain {
    pub id: ToolchainId,
    pub lifter: Option<Label>,
    pub config: RuleConfig,
    pub deps: Vec<ToolchainId>,
}

/// A Toolchains Registry defines which toolchains we have available.
///
#[derive(Debug)]
pub struct ToolchainsRegistry {
    archive_manager: ArchiveManager,
    remote_url: Url,

    toolchains: BTreeMap<ToolchainId, Toolchain>,
}

#[derive(Error, Debug)]
pub enum ToolchainsRegistryError {
    #[error("Could not parse file: {0:?}")]
    ParseError(serde_json::Error),

    #[error("Could not print file: {0:#?}")]
    PrintError(serde_json::Error),

    #[error(transparent)]
    ArchiveManagerError(ArchiveManagerError),

    #[error("Could not read registry file at {file:?} due to: {err:?}")]
    FileReadError { file: PathBuf, err: std::io::Error },
}

impl ToolchainsRegistry {
    pub async fn fetch(
        workspace_paths: &WorkspacePaths,
        event_channel: Arc<EventChannel>,
    ) -> Result<Self, ToolchainsRegistryError> {
        let mut toolchains = BTreeMap::new();
        let archive_manager = ArchiveManager::from_paths(workspace_paths, event_channel);
        let remote_url = Url::parse(REMOTE_TOOLCHAINS_REGISTRY_URL).unwrap();

        let (path, _) = archive_manager
            .download(&remote_url, "json")
            .await
            .map_err(ToolchainsRegistryError::ArchiveManagerError)?;

        let file =
            fs::File::open(&path)
                .await
                .map_err(|err| ToolchainsRegistryError::FileReadError {
                    file: path.clone(),
                    err,
                })?;

        let reader = json_comments::StripComments::new(BufReader::new(file.into_std().await));
        let json: BTreeMap<String, serde_json::Value> =
            serde_json::from_reader(reader).map_err(ToolchainsRegistryError::ParseError)?;

        for (language, vsn_cfgs) in json {
            let vsn_cfgs = vsn_cfgs.as_object().unwrap();

            for (version, config) in vsn_cfgs {
                let object = config.as_object().unwrap();

                let toolchain = Toolchain {
                    id: ToolchainId {
                        language: language.to_string(),
                        version: version.to_string(),
                    },

                    deps: {
                        let default = serde_json::Map::default();
                        object
                            .get("deps")
                            .and_then(|o| o.as_object())
                            .unwrap_or(&default)
                            .iter()
                            .map(|(k, v)| ToolchainId {
                                language: k.to_string(),
                                version: v.as_str().unwrap().to_string(),
                            })
                            .collect()
                    },

                    lifter: object
                        .get("lifter")
                        .and_then(|s| s.as_str())
                        .map(Label::new),

                    config: {
                        let mut object = object.clone();
                        object.remove("deps");
                        object.remove("lifter");
                        let config = serde_json::Value::Object(object);
                        TryFrom::try_from(config.clone()).unwrap()
                    },
                };
                toolchains.insert(toolchain.id.clone(), toolchain);
            }
        }

        Ok(Self {
            toolchains,
            archive_manager,
            remote_url,
        })
    }

    pub fn toolchains(&self) -> Vec<Toolchain> {
        self.toolchains.values().cloned().collect()
    }

    pub fn get_toolchain_by_id(&self, id: &ToolchainId) -> Option<Toolchain> {
        dbg!(&id);
        self.toolchains.get(id).cloned()
    }

    pub fn flatten_dependencies(&self, mut toolchains: Vec<Toolchain>) -> Vec<Toolchain> {
        let mut visited: Vec<ToolchainId> = toolchains.iter().map(|t| t.id.clone()).collect();
        let mut pending: Vec<ToolchainId> =
            toolchains.iter().flat_map(|t| t.deps.clone()).collect();

        loop {
            if pending.is_empty() {
                break;
            }
            if let Some(toolchain_id) = pending.pop() {
                if visited.contains(&toolchain_id) {
                    continue;
                }
                visited.push(toolchain_id.clone());

                let toolchain = self.get_toolchain_by_id(&toolchain_id).unwrap();

                for dep in toolchain.deps.iter().filter(|d| !visited.contains(d)) {
                    pending.push(dep.clone());
                }

                toolchains.push(toolchain);
            }
        }

        toolchains
    }
}
