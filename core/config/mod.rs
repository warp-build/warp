mod config_file;
mod host_env;

use super::*;
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Instant;
use thiserror::*;

pub use config_file::*;
use host_env::*;
use url::Url;

pub const WARPFILE: &str = "Warpfile";

/// A collection of flags and options that affect how Warp runs. This is not specific to a build,
/// it relates to Warp itself.
///
#[derive(Builder, Debug, Clone)]
#[builder(build_fn(error = "ConfigError"))]
pub struct Config {
    /// Never access the network, but continue working if possible.
    #[builder(default = "self.default_offline()")]
    offline: bool,

    /// The current working directory of warp
    #[builder(default = "self.default_invocation_dir()?")]
    invocation_dir: PathBuf,

    /// The current user executing warp
    #[builder(default = "self.default_current_user()")]
    current_user: String,

    /// The time at which this configuration was created. We will use this to compute the total
    /// build time.
    #[builder(default = "self.default_created_at()")]
    created_at: Instant,

    /// The environment at the time this config was created. This is separated to assist with
    /// testing.
    #[builder(default = "self.default_env()?")]
    env: HashMap<String, String>,

    /// The uppermost directory in the file system in which to search for config files.
    #[builder(setter(into, strip_option), default = "None")]
    search_stop_path: Option<PathBuf>,

    /// The maximum number of local workers to spawn in the local worker pool.
    #[builder(default = "self.default_max_workers()")]
    max_local_workers: usize,

    /// The root of warp's operating directory. By default this is `/warp`
    #[builder(default = "self.default_warp_root()")]
    warp_root: PathBuf,

    /// The host environment in which warp is currently running.
    #[builder(default)]
    host_env: HostEnv,

    /// The location of the public store CDN
    #[builder(default = "self.default_public_store_cdn_url()")]
    public_store_cdn_url: Url,

    /// The location of the public store Manifests
    #[builder(default = "self.default_public_store_metadata_url()")]
    public_store_metadata_url: Url,

    /// The location of the public rule store
    #[builder(default = "self.default_public_rule_store_url()")]
    public_rule_store_url: Url,

    /// The location of the rule store in the current host.
    #[builder(default = "self.default_rule_store_root()")]
    rule_store_root: PathBuf,

    /// The location of the artifact store in the current host.
    #[builder(default = "self.default_artifact_store_root()")]
    artifact_store_root: PathBuf,

    /// The location of the archives in the current host.
    #[builder(default = "self.default_archive_root()")]
    archive_root: PathBuf,

    /// The HTTP Client to be used across the application.
    /// NOTE(@ostera): this is safe to clone since it is really an [Arc] to a client pool.
    #[builder(default = "self.default_http_client()")]
    http_client: reqwest::Client,
}

impl Default for Config {
    fn default() -> Self {
        Self::builder().build().unwrap()
    }
}

impl Config {
    pub fn builder() -> ConfigBuilder {
        ConfigBuilder::default()
    }

    pub fn offline(&self) -> bool {
        self.offline
    }

    pub fn invocation_dir(&self) -> &PathBuf {
        &self.invocation_dir
    }

    pub fn created_at(&self) -> Instant {
        self.created_at
    }

    pub fn env(&self) -> &HashMap<String, String> {
        &self.env
    }

    pub fn search_stop_path(&self) -> Option<&PathBuf> {
        self.search_stop_path.as_ref()
    }

    pub fn max_local_workers(&self) -> usize {
        self.max_local_workers
    }

    pub fn warp_root(&self) -> &PathBuf {
        &self.warp_root
    }

    pub fn current_user(&self) -> &str {
        self.current_user.as_ref()
    }

    pub fn host_env(&self) -> &HostEnv {
        &self.host_env
    }

    pub fn public_store_cdn_url(&self) -> &Url {
        &self.public_store_cdn_url
    }

    pub fn public_store_metadata_url(&self) -> &Url {
        &self.public_store_metadata_url
    }

    pub fn public_rule_store_url(&self) -> &Url {
        &self.public_rule_store_url
    }

    pub fn artifact_store_root(&self) -> &PathBuf {
        &self.artifact_store_root
    }

    pub fn rule_store_root(&self) -> &PathBuf {
        &self.rule_store_root
    }

    pub fn http_client(&self) -> &reqwest::Client {
        &self.http_client
    }

    pub fn archive_root(&self) -> &PathBuf {
        &self.archive_root
    }
}

impl ConfigBuilder {
    fn _warp_root(&self) -> PathBuf {
        self.warp_root
            .clone()
            .unwrap_or_else(|| self.default_warp_root())
    }

    fn default_warp_root(&self) -> PathBuf {
        PathBuf::from("/warp")
    }

    fn default_archive_root(&self) -> PathBuf {
        self._warp_root().join("archives")
    }

    fn default_artifact_store_root(&self) -> PathBuf {
        self._warp_root().join("store")
    }

    fn default_rule_store_root(&self) -> PathBuf {
        self._warp_root().join("rules")
    }

    fn default_public_rule_store_url(&self) -> Url {
        "https://rules.warp.build/rules/".parse::<Url>().unwrap()
    }

    fn default_public_store_metadata_url(&self) -> Url {
        "https://store.warp.build".parse::<Url>().unwrap()
    }

    fn default_public_store_cdn_url(&self) -> Url {
        "https://public.store.warp.build".parse::<Url>().unwrap()
    }

    fn default_offline(&self) -> bool {
        false
    }

    fn default_max_workers(&self) -> usize {
        num_cpus::get()
    }

    fn default_current_user(&self) -> String {
        whoami::username()
    }

    fn default_invocation_dir(&self) -> Result<PathBuf, ConfigError> {
        std::env::current_dir().map_err(ConfigError::CouldNotGetCurrentDir)
    }

    fn default_created_at(&self) -> Instant {
        Instant::now()
    }

    fn default_http_client(&self) -> reqwest::Client {
        reqwest::Client::new()
    }

    fn default_env(&self) -> Result<HashMap<String, String>, ConfigError> {
        let env = std::env::vars_os()
            .filter_map(|(k, v)| match (k.into_string(), v.into_string()) {
                (Ok(k), Ok(v)) => Some((k, v)),
                _ => None,
            })
            .collect();
        Ok(env)
    }
}

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error(transparent)]
    CouldNotGetCurrentDir(std::io::Error),

    #[error("Attempted to build a Config struct while missing fields: {0:?}")]
    BuilderError(derive_builder::UninitializedFieldError),
}

impl From<derive_builder::UninitializedFieldError> for ConfigError {
    fn from(err: derive_builder::UninitializedFieldError) -> Self {
        Self::BuilderError(err)
    }
}
