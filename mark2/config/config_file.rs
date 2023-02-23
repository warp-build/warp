use serde::{Deserialize, Serialize};
use std::io::BufReader;
use std::path::{Path, PathBuf};
use thiserror::*;
use tokio::fs;

/// A struct representing a `Warpfile` file in a Warp project. This struct is a 1:1 mapping to the
/// JSON file to be able to easily derive the ser/de.
///
#[derive(Clone, Default, Debug, Serialize, Deserialize)]
pub struct WarpConfigFile {
    pub workspace: WorkspaceConfigFile,
}

#[derive(Clone, Default, Debug, Serialize, Deserialize)]
pub struct WorkspaceConfigFile {
    pub name: String,
}

impl WarpConfigFile {
    pub async fn read(path: &Path) -> Result<Self, WarpConfigFileError> {
        let file =
            fs::File::open(&path)
                .await
                .map_err(|err| WarpConfigFileError::CouldNotReadFile {
                    path: path.into(),
                    err,
                })?;

        let reader = json_comments::StripComments::new(BufReader::new(file.into_std().await));

        serde_json::from_reader(reader).map_err(WarpConfigFileError::ParseError)
    }
}

#[derive(Error, Debug)]
pub enum WarpConfigFileError {
    #[error("Could not parse Warpfile: {0:?}")]
    ParseError(serde_json::Error),

    #[error("Could not write Warpfile at {path:?} due to {err:?}")]
    CouldNotReadFile { path: PathBuf, err: std::io::Error },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_json() {
        let warp_config_file = serde_json::from_str(
            r#"
        {
            "workspace": {
                "name": "test"
            }
        }
        "#,
        );

        assert!(warp_config_file.is_ok());
        assert_matches!(
            warp_config_file,
            Ok(WarpConfigFile {
                workspace: WorkspaceConfigFile { name }
            }) if name == "test"
        );
    }
}
