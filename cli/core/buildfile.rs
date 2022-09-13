use super::*;
use std::path::{Path, PathBuf};
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
pub enum BuildfileError {
    #[error(transparent)]
    FileReadError(std::io::Error),

    #[error(transparent)]
    TomlError(toml::de::Error),

    #[error("Expected contents of a Build.toml file to be a TOML table, but instead found: {0:?}")]
    BuildfileMustBeTable(toml::Value),

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
    #[tracing::instrument(name = "Buildfile::from_file")]
    pub async fn from_file(file: &PathBuf) -> Result<Buildfile, BuildfileError> {
        let string = fs::read_to_string(&file)
            .await
            .map_err(BuildfileError::FileReadError)?;

        let contents = string
            .parse::<toml::Value>()
            .map_err(BuildfileError::TomlError)?;

        Self::from_toml(file, &contents).await
    }

    pub async fn from_toml(path: &Path, toml: &toml::Value) -> Result<Buildfile, BuildfileError> {
        let contents = toml
            .as_table()
            .ok_or_else(|| BuildfileError::BuildfileMustBeTable(toml.clone()))?;

        let mut targets: Vec<Target> = vec![];

        for (rule_name, configs) in contents.iter() {
            for cfg in configs
                .as_array()
                .ok_or(BuildfileError::RuleShouldBeTable)?
            {
                let config: RuleConfig =
                    TryFrom::try_from(cfg).map_err(BuildfileError::RuleConfigError)?;

                let name =
                    config
                        .get_string("name")
                        .map_err(|err| BuildfileError::TargetNeedsName {
                            config: config.clone(),
                            err,
                        })?;

                let label = Label::from_path_and_name(path, &name);

                let target = Target::new(label, rule_name, config);

                targets.push(target);
            }
        }

        Ok(Self {
            file: path.to_path_buf(),
            targets,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn parses_multiple_targets_with_different_rules() {
        let path = PathBuf::from("example/path/used/only/in/label");
        let toml = r#"

[[rule1]]
name = "a"

[[rule2]]
name = "b"

        "#
        .parse::<toml::Value>()
        .unwrap();

        let buildfile = Buildfile::from_toml(&path, &toml).await.unwrap();

        let expected_label_a = Label::new("//example/path/used/only/in/label:a");
        let expected_label_b = Label::new("//example/path/used/only/in/label:b");

        assert_eq!(buildfile.targets.len(), 2);

        let target = &buildfile.targets[0];
        assert_eq!(target.label, expected_label_a);
        assert_eq!(target.rule_name, "rule1");

        let target = &buildfile.targets[1];
        assert_eq!(target.label, expected_label_b);
        assert_eq!(target.rule_name, "rule2");
    }

    #[tokio::test]
    async fn parses_multiple_targets_with_same_rule() {
        let path = PathBuf::from("example/path/used/only/in/label");
        let toml = r#"

[[hello_world]]
name = "a"

[[hello_world]]
name = "b"

        "#
        .parse::<toml::Value>()
        .unwrap();

        let buildfile = Buildfile::from_toml(&path, &toml).await.unwrap();

        let expected_label_a = Label::new("//example/path/used/only/in/label:a");
        let expected_label_b = Label::new("//example/path/used/only/in/label:b");

        assert_eq!(buildfile.targets.len(), 2);

        let target = &buildfile.targets[0];
        assert_eq!(target.label, expected_label_a);
        assert_eq!(target.rule_name, "hello_world");

        let target = &buildfile.targets[1];
        assert_eq!(target.label, expected_label_b);
        assert_eq!(target.rule_name, "hello_world");
    }

    #[tokio::test]
    async fn buildfile_must_be_table() {
        let path = PathBuf::from("example/path/used/only/in/label");
        let toml = toml::Value::String("this is a string".to_string());
        let buildfile = Buildfile::from_toml(&path, &toml).await;

        assert!(matches!(
            buildfile,
            Err(BuildfileError::BuildfileMustBeTable(_))
        ));
    }

    #[tokio::test]
    async fn target_needs_name() {
        let path = PathBuf::from("example/path/used/only/in/label");
        let toml = r#"

[[hello_world]]
deps = ["a","b","c"]

        "#
        .parse::<toml::Value>()
        .unwrap();

        let buildfile = Buildfile::from_toml(&path, &toml).await;

        assert!(matches!(
            buildfile,
            Err(BuildfileError::TargetNeedsName { .. })
        ));
    }

    #[tokio::test]
    async fn target_must_be_table() {
        let path = PathBuf::from("example/path/used/only/in/label");
        let toml = r#"

[hello_world]
name = "hello"
deps = ["a","b","c"]

        "#
        .parse::<toml::Value>()
        .unwrap();

        let buildfile = Buildfile::from_toml(&path, &toml).await;

        assert!(matches!(buildfile, Err(BuildfileError::RuleShouldBeTable)));
    }
}
