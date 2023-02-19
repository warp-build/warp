mod config_file;

use super::*;
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Instant;
use thiserror::*;

pub use config_file::*;

pub const WARPFILE: &str = "Warpfile";

/// A collection of flags and options that affect how Warp runs. This is not specific to a build,
/// it relates to Warp itself.
///
#[derive(Builder, Debug, Clone)]
#[builder(build_fn(error = "ConfigError"))]
pub struct Config {
    /// Never access the network, but continue working if possible.
    offline: bool,

    /// The current working directory of warp
    invocation_dir: PathBuf,

    /// The current user executing warp
    current_user: String,

    /// The time at which this configuration was created. We will use this to compute the total
    /// build time.
    created_at: Instant,

    /// The environment at the time this config was created. This is separated to assist with
    /// testing.
    env: HashMap<String, String>,

    /// The uppermost directory in the file system in which to search for config files.
    #[builder(setter(into, strip_option))]
    search_stop_path: Option<PathBuf>,

    /// The maximum number of local workers to spawn in the local worker pool.
    max_local_workers: usize,

    /// The root of warp's operating directory. By default this is `/warp`
    warp_root: PathBuf,
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
}

impl ConfigBuilder {
    fn default_warp_root() -> PathBuf {
        PathBuf::from("/warp")
    }

    fn default_offline() -> bool {
        false
    }

    fn default_max_workers() -> usize {
        num_cpus::get()
    }

    fn default_current_user() -> String {
        whoami::username()
    }

    fn default_invocation_dir() -> Result<PathBuf, ConfigError> {
        std::env::current_dir().map_err(ConfigError::CouldNotGetCurrentDir)
    }

    fn default_created_at() -> Instant {
        Instant::now()
    }

    fn default_env() -> Result<HashMap<String, String>, ConfigError> {
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
