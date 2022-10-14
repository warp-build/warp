use super::*;
use fxhash::*;
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;
use tokio::fs;
use tokio::io::AsyncReadExt;
use url::Url;

pub const REMOTE_TOOLCHAINS_REGISTRY_URL: &str = "https://pkgs.warp.build/toolchains/registry.json";

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct Toolchains {
    toolchains: FxHashMap<String, FlexibleRuleConfig>,
}

/// A Toolchains Registry defines which toolchains we have available.
///
#[derive(Debug)]
pub struct ToolchainsRegistry {
    available_toolchains: Toolchains,
    archive_manager: ArchiveManager,
    remote_url: Url,
}

impl Toolchains {
    fn _add_toolchain_to_config_from_name_and_version(
        &self,
        dep_name: &String,
        dep_version: &str,
        config: &mut BTreeMap<String, FlexibleRuleConfig>,
    ) {
        let FlexibleRuleConfig(dep_config) = self.toolchains.get(dep_name).unwrap();

        //TODO: i think we should just use OpenSSL_1_1_1q
        let dep_version = if dep_version.clone() == "OpenSSL_1_1_1q" {
            "1.1.1q"
        } else {
            dep_version
        };

        let dep_version_args = dep_config.get(dep_version).unwrap();

        config.insert(
            dep_name.clone(),
            FlexibleRuleConfig({
                let mut dep_config_map = BTreeMap::new();
                dep_config_map.insert("version".to_string(), dep_version.into());
                match dep_version_args {
                    toml::Value::Table(args) => {
                        for k in args.keys() {
                            let key_to_add = k.clone();
                            if key_to_add != "deps".to_string() {
                                dep_config_map.insert(key_to_add, args.get(k).unwrap().clone());
                            }
                        }
                    }
                    _ => (),
                }
                dep_config_map
            }),
        );
    }

    pub fn add_dep_to_config(
        &self,
        dep: &toml::Value,
        config: &mut BTreeMap<String, FlexibleRuleConfig>,
    ) {
        match dep {
            toml::Value::Table(dep_spec) => {
                let dep_name = dep_spec.keys().last().unwrap();
                let dep_version = dep_spec.get(dep_name).unwrap().as_str().unwrap();

                self._add_toolchain_to_config_from_name_and_version(dep_name, dep_version, config);
            }
            _ => (),
        }
    }

    pub fn add_toolchain_to_config(
        &self,
        toolchain: &str,
        config: &mut BTreeMap<String, FlexibleRuleConfig>,
    ) {
        let FlexibleRuleConfig(chosen_toolchain_config) = self.toolchains.get(toolchain).unwrap();

        // NOTE(diogo): this only works because we only have one version per toolchain and
        // chosen_toolchain only consists of the toolchain name. Once we allow specifying the name
        // and version, we should change this to find the version as well.
        let version = chosen_toolchain_config.keys().last().unwrap();

        match chosen_toolchain_config.get(version) {
            Some(toml::Value::Table(version_config)) => match version_config.get("deps") {
                Some(toml::Value::Array(deps)) => {
                    dbg!(&deps);
                    for dep in deps {
                        self.add_dep_to_config(dep, config)
                    }
                }
                _ => (),
            },
            _ => (),
        }

        self._add_toolchain_to_config_from_name_and_version(
            &toolchain.to_string(),
            version,
            config,
        );
    }
}

impl ToolchainsRegistry {
    pub fn new(workspace_paths: &WorkspacePaths) -> Self {
        ToolchainsRegistry {
            available_toolchains: Toolchains::default(),
            archive_manager: ArchiveManager::from_paths(workspace_paths),
            remote_url: Url::parse(REMOTE_TOOLCHAINS_REGISTRY_URL).unwrap(),
        }
    }

    async fn fetch(&self) -> Result<PathBuf, anyhow::Error> {
        let (path, _) = self
            .archive_manager
            .download(&self.remote_url, "json")
            .await?;

        Ok(path)
    }

    async fn parse_downloaded_file(&mut self, path: &PathBuf) -> Result<(), anyhow::Error> {
        let mut file = fs::File::open(path).await?;

        let mut bytes = vec![];
        file.read_to_end(&mut bytes).await?;

        self.available_toolchains.toolchains = serde_json::from_slice(&bytes)?;

        Ok(())
    }

    pub fn show_toolchains(&self) -> Vec<&str> {
        let mut toolchains: Vec<&str> = vec![];
        for key in self.available_toolchains.toolchains.keys() {
            toolchains.push(key.as_str());
        }

        toolchains
    }

    pub fn config_from_chosen_toolchains(
        &self,
        chosen_toolchains: Vec<String>,
    ) -> BTreeMap<String, FlexibleRuleConfig> {
        dbg!(&chosen_toolchains);
        let mut toolchains_config: BTreeMap<String, FlexibleRuleConfig> = BTreeMap::new();
        for toolchain in self.available_toolchains.toolchains.keys() {
            if chosen_toolchains.contains(&toolchain) {
                self.available_toolchains
                    .add_toolchain_to_config(toolchain, &mut toolchains_config);
            }
        }

        toolchains_config
    }

    pub async fn ready(&mut self) -> Result<(), anyhow::Error> {
        let path = self.fetch().await?;

        self.parse_downloaded_file(&path).await?;

        Ok(())
    }
}
