use super::*;
use serde_derive::{Deserialize, Serialize};
use std::path::collections::BTreeMap;
use std::path::{Path, PathBuf};
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;

pub const WSIG_EXT: &str = "wsig";

#[derive(Debug, Clone)]
pub struct SignatureFile {
    pub targets: Vec<Target>,
}

#[derive(Error, Debug)]
pub enum SignatureFileError {
    #[error("Could not parse Warp Signature file: {0:?}")]
    ParseError(serde_json::Error),

    #[error("Could not print Warp Signature file: {0:#?}")]
    PrintError(serde_json::Error),

    #[error(transparent)]
    IOError(std::io::Error),

    #[error("The following target does not have a name: {config:?}")]
    TargetNeedsName {
        config: RuleConfig,
        err: RuleConfigError,
    },
}

impl SignatureFile {
    #[tracing::instrument(name = "SignatureFile::from_label")]
    pub async fn from_label(label: &Label) -> Result<SignatureFile, SignatureFileError> {
        let wsig_path = label
            .workspace()
            .join(label.path())
            .with_extension(WSIG_EXT);

        let mut file = fs::File::open(&wsig_path)
            .await
            .map_err(OutputManifestError::IOError)?;

        let mut bytes = vec![];
        file.read_to_end(&mut bytes)
            .await
            .map_err(OutputManifestError::IOError)?;

        let file_json: BTreeMap<String, serde_json::Value> =
            serde_json::from_slice(&bytes).map_err(SignatureFileError::ParseError)?;

        let mut targets = vec![];
        for (rule_name, configs) in file_json {
            for config in configs {
                let config: RuleConfig =
                    TryFrom::try_from(config).map_err(SignatureFileError::RuleConfigError)?;

                let name = config.get_string("name").map_err(|err| {
                    SignatureFileError::TargetNeedsName {
                        config: config.clone(),
                        err,
                    }
                })?;

                let label = Label::builder()
                    .workspace(workspace_root.to_str().unwrap().to_string())
                    .name(name)
                    .from_path(path_relative_to_workspace.to_path_buf())
                    .unwrap();

                let target = Target::new(label, rule_name, config);

                targets.push(target);
            }
        }

        Ok(SignatureFile {
            file: wsig_path,
            targets,
        })
    }
}
