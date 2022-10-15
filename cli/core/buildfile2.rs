use super::*;
use std::collections::BTreeMap;
use std::path::PathBuf;
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;

pub const BUILDFILE: &str = "Build.json";

#[derive(Debug)]
pub struct Buildfile2 {
    /// The Path to this build file.
    pub file: PathBuf,

    /// The Targets defined within this build file.
    pub targets: Vec<Target>,
}

#[derive(Error, Debug)]
pub enum Buildfile2Error {
    #[error("Could not parse file: {0:?}")]
    ParseError(serde_json::Error),

    #[error("Could not print file: {0:#?}")]
    PrintError(serde_json::Error),

    #[error("Could not read build file at {file:?} due to: {err:?}")]
    FileReadError { file: PathBuf, err: std::io::Error },

    #[error(transparent)]
    TomlError(toml::de::Error),

    #[error("Expected contents of a Build.toml file to be a TOML table, but instead found: {0:?}")]
    Buildfile2MustBeTable(toml::Value),

    #[error("Rule should be table")]
    RuleShouldBeTable,

    #[error(transparent)]
    TargetError(TargetError),

    #[error(transparent)]
    RuleConfigError(RuleConfigError),

    #[error("The following target does not have a name: {config:?}")]
    TargetNeedsName {
        config: RuleConfig,
        err: RuleConfigError,
    },
}

impl Buildfile2 {
    #[tracing::instrument(name = "Buildfile2::from_label")]
    pub async fn from_label(label: &Label) -> Result<Buildfile2, Buildfile2Error> {
        let full_path = label.workspace().join(label.path()).join(BUILDFILE);
        Self::from_file(&label.workspace(), &full_path, &label.path()).await
    }

    #[tracing::instrument(name = "Buildfile2::from_file")]
    pub async fn from_file(
        workspace_root: &PathBuf,
        buildfile: &PathBuf,
        path_relative_to_workspace: &PathBuf,
    ) -> Result<Buildfile2, Buildfile2Error> {
        let mut file =
            fs::File::open(&buildfile)
                .await
                .map_err(|err| Buildfile2Error::FileReadError {
                    err,
                    file: buildfile.clone(),
                })?;

        let mut bytes = vec![];
        file.read_to_end(&mut bytes)
            .await
            .map_err(|err| Buildfile2Error::FileReadError {
                err,
                file: buildfile.clone(),
            })?;

        let json: Vec<BTreeMap<String, serde_json::Value>> =
            serde_json::from_slice(&bytes).map_err(Buildfile2Error::ParseError)?;

        let mut targets = vec![];

        for json_cfg in json {
            let rule_name = json_cfg["rule"].as_str().unwrap().to_string();

            let mut config: RuleConfig = RuleConfig::new();

            for (k, v) in json_cfg {
                config.insert(
                    k.to_string(),
                    TryFrom::try_from(v).map_err(Buildfile2Error::RuleConfigError)?,
                );
            }

            let name =
                config
                    .get_string("name")
                    .map_err(|err| Buildfile2Error::TargetNeedsName {
                        err,
                        config: config.clone(),
                    })?;

            let label = Label::builder()
                .workspace(workspace_root.to_str().unwrap().to_string())
                .name(name)
                .from_path(path_relative_to_workspace.to_path_buf())
                .unwrap();

            let target = Target::new(label, &rule_name, config);

            targets.push(target);
        }

        Ok(Self {
            file: workspace_root.join(path_relative_to_workspace),
            targets,
        })
    }
}
