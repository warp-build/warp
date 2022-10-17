use super::*;
use std::collections::BTreeMap;
use std::io::BufReader;
use std::path::PathBuf;
use thiserror::*;
use tokio::fs;

pub const BUILDFILE: &str = "Build.json";

#[derive(Debug)]
pub struct Buildfile {
    /// The Path to this build file.
    pub file: PathBuf,

    /// The Targets defined within this build file.
    pub targets: Vec<Target>,
}

#[derive(Error, Debug)]
pub enum BuildfileError {
    #[error("Could not parse file: {0:?}")]
    ParseError(serde_json::Error),

    #[error("Could not print file: {0:#?}")]
    PrintError(serde_json::Error),

    #[error("Could not read build file at {file:?} due to: {err:?}")]
    FileReadError { file: PathBuf, err: std::io::Error },

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

impl Buildfile {
    #[tracing::instrument(name = "Buildfile::from_label")]
    pub async fn from_label(label: &Label) -> Result<Buildfile, BuildfileError> {
        let full_path = label.workspace().join(label.path()).join(BUILDFILE);
        Self::from_file(&label.workspace(), &full_path, &label.path()).await
    }

    #[tracing::instrument(name = "Buildfile::from_file")]
    pub async fn from_file(
        workspace_root: &PathBuf,
        buildfile: &PathBuf,
        path_relative_to_workspace: &PathBuf,
    ) -> Result<Buildfile, BuildfileError> {
        let file =
            fs::File::open(buildfile)
                .await
                .map_err(|err| BuildfileError::FileReadError {
                    err,
                    file: buildfile.clone(),
                })?;

        let reader = json_comments::StripComments::new(BufReader::new(file.into_std().await));

        let json: Vec<BTreeMap<String, serde_json::Value>> =
            serde_json::from_reader(reader).map_err(BuildfileError::ParseError)?;

        let mut targets = vec![];

        for json_cfg in json {
            let rule_name = json_cfg["rule"].as_str().unwrap().to_string();

            let mut config: RuleConfig = RuleConfig::new();

            for (k, v) in json_cfg {
                config.insert(
                    k.to_string(),
                    TryFrom::try_from(v).map_err(BuildfileError::RuleConfigError)?,
                );
            }

            let name =
                config
                    .get_string("name")
                    .map_err(|err| BuildfileError::TargetNeedsName {
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
