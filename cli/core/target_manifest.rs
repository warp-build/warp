use super::*;
use serde_derive::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;

pub const MANIFEST_FILE: &str = "Manifest.json";

#[derive(Error, Debug)]
pub enum TargetManifestError {
    #[error("Could not parse Manifest file: {0:?}")]
    ParseError(serde_json::Error),

    #[error("Could not print Manifest file: {0:#?}")]
    PrintError(serde_json::Error),

    #[error(transparent)]
    IOError(std::io::Error),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildStamps {
    #[serde(with = "serde_iso8601")]
    pub plan_started_at: chrono::DateTime<chrono::Utc>,
    #[serde(with = "serde_iso8601")]
    pub plan_ended_at: chrono::DateTime<chrono::Utc>,
    #[serde(with = "serde_duration")]
    pub plan_elapsed_time: chrono::Duration,
    #[serde(with = "serde_iso8601")]
    pub build_started_at: chrono::DateTime<chrono::Utc>,
    #[serde(with = "serde_iso8601")]
    pub build_completed_at: chrono::DateTime<chrono::Utc>,
    #[serde(with = "serde_duration")]
    pub build_elapsed_time: chrono::Duration,
}

impl Default for BuildStamps {
    fn default() -> Self {
        Self {
            plan_started_at: Default::default(),
            plan_ended_at: Default::default(),
            plan_elapsed_time: chrono::Duration::zero(),
            build_started_at: Default::default(),
            build_completed_at: Default::default(),
            build_elapsed_time: chrono::Duration::zero(),
        }
    }
}

// NOTE(@ostera): DO NOT REORDER FIELDS. TOML doesn't support arbitrary values _after_
// a table has been defined. This means that after the first composite value (BuildStamps,
// BTreeMap, etc), you can't have simple values (String, bool, Vec).
//
#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct TargetManifest {
    pub label: Label,

    pub rule_name: String,

    pub hash: String,

    #[serde(default)]
    pub cached: bool,

    #[serde(default)]
    pub is_valid: bool,

    #[serde(default)]
    pub srcs: Vec<PathBuf>,

    #[serde(default)]
    pub outs: Vec<PathBuf>,

    #[serde(default)]
    pub buildstamps: BuildStamps,

    #[serde(default)]
    pub provides: BTreeMap<String, PathBuf>,

    #[serde(default)]
    pub deps: BTreeMap<String, String>,

    #[serde(default)]
    pub runtime_deps: BTreeMap<String, String>,

    #[serde(default)]
    pub transitive_deps: BTreeMap<String, String>,

    #[serde(default)]
    pub toolchains: BTreeMap<String, String>,

    #[serde(default)]
    pub env: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TargetManifestFile {
    pub manifest: TargetManifest,
}

impl TargetManifest {
    #[tracing::instrument(name = "TargetManifest::from_validation_result")]
    pub fn from_validation_result(
        build_started_at: chrono::DateTime<chrono::Utc>,
        validation: &ValidationStatus,
        store_path: &Path,
        env: BTreeMap<String, String>,
        target: &ExecutableTarget,
        build_results: &BuildResults,
    ) -> Self {
        let outs = if let ValidationStatus::Valid { outputs } = validation {
            outputs.clone()
        } else {
            target.outs.iter().cloned().collect()
        };

        let provides: BTreeMap<String, PathBuf> = target
            .provides
            .iter()
            .map(|(name, p)| (name.clone(), store_path.join(p)))
            .collect();

        let deps = target
            .deps
            .iter()
            .flat_map(|d| build_results.get_manifest(*d))
            .map(|d| (d.label.to_string(), d.hash.to_string()))
            .collect();

        let runtime_deps = target
            .runtime_deps
            .iter()
            .flat_map(|d| build_results.get_manifest(*d))
            .map(|d| (d.label.to_string(), d.hash.to_string()))
            .collect();

        let transitive_deps = target
            .transitive_deps
            .iter()
            .flat_map(|d| build_results.get_manifest(*d))
            .map(|d| (d.label.to_string(), d.hash.to_string()))
            .collect();

        let toolchains = target
            .toolchains
            .iter()
            .flat_map(|d| build_results.get_manifest(*d))
            .map(|d| (d.label.to_string(), d.hash.to_string()))
            .collect();

        let build_completed_at = chrono::Utc::now();

        let buildstamps = BuildStamps {
            build_started_at,
            build_completed_at,
            build_elapsed_time: build_completed_at.signed_duration_since(build_started_at),

            plan_started_at: target.target_plan_started_at,
            plan_ended_at: target.target_plan_ended_at,
            plan_elapsed_time: target
                .target_plan_ended_at
                .signed_duration_since(target.target_plan_started_at),
        };

        Self {
            buildstamps,
            cached: false,
            deps,
            env,
            hash: target.hash.clone(),
            is_valid: matches!(&validation, ValidationStatus::Valid { .. }),
            label: target.label.clone().into(),
            outs,
            provides,
            rule_name: target.rule.name.clone(),
            srcs: target.srcs.iter().cloned().collect(),
            toolchains,
            transitive_deps,
            runtime_deps,
        }
    }

    pub fn env_map(&self) -> HashMap<String, String> {
        self.env.clone().into_iter().collect()
    }

    #[tracing::instrument(name = "TargetManifest::from_file")]
    pub async fn from_file(path: &Path) -> Result<Self, TargetManifestError> {
        let mut file = fs::File::open(path)
            .await
            .map_err(TargetManifestError::IOError)?;

        let mut bytes = vec![];
        file.read_to_end(&mut bytes)
            .await
            .map_err(TargetManifestError::IOError)?;

        let target_manifest_file: TargetManifestFile =
            serde_json::from_slice(&bytes).map_err(TargetManifestError::ParseError)?;

        Ok(target_manifest_file.manifest)
    }

    #[tracing::instrument(name = "TargetManifest::write")]
    pub async fn write(&self, root: &Path) -> Result<(), TargetManifestError> {
        let mut manifest = self.clone();
        manifest.cached = true;

        let json = serde_json::to_string_pretty(&TargetManifestFile { manifest })
            .map_err(TargetManifestError::PrintError)?;

        fs::write(&root.join(MANIFEST_FILE), json)
            .await
            .map_err(TargetManifestError::IOError)
    }

    pub fn placeholder(label: Label) -> Self {
        Self {
            label,
            ..Self::default()
        }
    }
}

impl std::hash::Hash for TargetManifest {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl Eq for TargetManifest {}

impl PartialEq for TargetManifest {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
    }
}

mod serde_iso8601 {
    use chrono::DateTime;
    use chrono::Utc;
    use serde::{de::Visitor, Deserializer, Serializer};

    struct DateTimeVisitor;
    impl<'de> Visitor<'de> for DateTimeVisitor {
        type Value = DateTime<Utc>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a date string is expected to follow RFC3339 / ISO8601")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            let date = DateTime::parse_from_rfc3339(v)
                .map_err(|err| E::custom(format!("{:?}", err)))?
                .with_timezone(&Utc);
            Ok(date)
        }
    }

    pub fn serialize<S>(date: &DateTime<Utc>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&date.to_rfc3339())
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<DateTime<Utc>, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_string(DateTimeVisitor)
    }
}

mod serde_duration {
    use chrono::Duration;
    use serde::{de::Visitor, Deserializer, Serializer};

    struct DurationVisitor;
    impl<'de> Visitor<'de> for DurationVisitor {
        type Value = Duration;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a date string is expected to follow RFC3339 / ISO8601")
        }

        fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(Duration::milliseconds(v))
        }

        fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(Duration::milliseconds(v.try_into().unwrap()))
        }
    }

    pub fn serialize<S>(duration: &Duration, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_i64(duration.num_milliseconds())
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Duration, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(DurationVisitor)
    }
}
