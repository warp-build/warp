use super::*;
use thiserror::*;
use tokio::fs;

pub const BUILDFILE: &str = "Build.toml";

#[derive(Debug)]
pub struct Buildfile {
    /// The Path to this build file.
    pub file: PathBuf,

    /// The Targets defined within this build file.
    pub targets: Vec<Target>,
}

#[derive(Error, Debug)]
pub enum BuildfileError {}

impl Buildfile {
    #[tracing::instrument(name = "Buildfile::from_file")]
    pub async fn from_file(file: &PathBuf) -> Result<Buildfile, BuildfileError> {
        let string = fs::read_to_string(&file).await?;
        let contents = contents.parse::<Value>()?;
        let targets = Self::from_toml(&contents)?;
        Ok(Self { file, targets })
    }

    pub async fn from_toml(toml: &toml::Value) -> Result<Vec<Target>, BuildfileError> {
        let contents = contents
            .as_table()
            .map_err(|_| BuildfileError::ExpectedTomlTable(contents.clone()))?;

        let mut targets: Vec<Target> = vec![];

        for (rule_name, configs) in contents.iter() {
            for cfg in configs
                .as_array()
                .map_err(BuildfileError::RuleShouldBeTable)?
            {
                let target =
                    Target::from_toml(rule_name, cfg).map_err(BuildfileError::TargetError)?;
                targets.push(target);
            }
        }

        Ok(targets)
    }
}
