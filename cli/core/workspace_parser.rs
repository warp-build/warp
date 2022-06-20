use super::*;
use anyhow::anyhow;
use anyhow::Context;
use tracing::*;

pub struct WorkspaceParser {}

impl WorkspaceParser {
    #[tracing::instrument(name = "WorkspaceParser::from_toml", skip(toml, paths))]
    pub fn from_toml(toml: toml::Value, paths: WorkspacePaths) -> Result<Workspace, anyhow::Error> {
        let workspace = toml
            .get("workspace")
            .context("Workspace file must have a workspace section")?;

        let name = workspace
            .get("name")
            .context("Workspace must have a name field")?
            .as_str()
            .context("Workspace name field must be a string")?
            .to_string();

        let ignore_patterns = if let Some(ignore_patterns) = workspace.get("ignore_patterns") {
            WorkspaceParser::parse_ignore_patterns(ignore_patterns)?
        } else {
            vec![]
        };

        let toolchain_archives = if let Some(toolchains) = toml.get("toolchains") {
            let table = toolchains.as_table().context(format!("Expected the [toolchains] section in your Workspace.toml to be a TOML table, but instead found a {}", toolchains.type_str()))?;
            WorkspaceParser::parse_archives(table)?
        } else {
            vec![]
        };

        debug!("Found {} Toolchains:", toolchain_archives.len());

        Ok(Workspace {
            name,
            paths,
            build_files: vec![],
            local_rules: vec![],
            local_toolchains: vec![],
            toolchain_archives,
            ignore_patterns,
        })
    }

    pub fn parse_ignore_patterns(
        ignore_patterns: &toml::Value,
    ) -> Result<Vec<String>, anyhow::Error> {
        if let Some(list) = ignore_patterns.as_array() {
            let mut patterns = vec![];
            for pat in list {
                if let Some(pat_str) = pat.as_str() {
                    patterns.push(pat_str.to_string())
                } else {
                    return Err(anyhow!(
                        "Expected `ignore_patterns` to be an array of strings, but found a {}: {:?}",
                        pat.type_str(),
                        pat
                    ));
                }
            }
            Ok(patterns)
        } else {
            Err(anyhow!(
                "Expected `ignore_patterns` to be an array. Instead we found a '{}'",
                ignore_patterns.type_str()
            ))
        }
    }

    pub fn parse_archives(archives: &toml::value::Table) -> Result<Vec<Archive>, anyhow::Error> {
        let mut t = vec![];
        for (name, config) in archives {
            t.push(WorkspaceParser::parse_archive(name.to_string(), config)?);
        }
        Ok(t)
    }

    pub fn parse_archive(name: String, cfg: &toml::Value) -> Result<Archive, anyhow::Error> {
        let archive = Archive::new().with_name(name);
        let archive = cfg
            .get("archive_url")
            .and_then(|x| x.as_str())
            .map(|url| archive.clone().with_url(url.to_string()).mark_as_source())
            .unwrap_or(archive);
        let archive = cfg
            .get("release_url")
            .and_then(|x| x.as_str())
            .map(|url| archive.clone().with_url(url.to_string()).mark_as_release())
            .unwrap_or(archive);
        let archive = cfg
            .get("release_name")
            .and_then(|x| x.as_str())
            .map(|name| {
                archive
                    .clone()
                    .with_name(name.to_string())
                    .mark_as_release()
            })
            .unwrap_or(archive);
        let archive = cfg
            .get("strip_prefix")
            .and_then(|x| x.as_str())
            .map(|prefix| archive.clone().with_prefix(prefix.to_string()))
            .unwrap_or(archive);
        let archive = cfg
            .get("checksum")
            .and_then(|x| x.as_str())
            .map(|sha1| archive.clone().with_sha1(sha1.to_string()))
            .unwrap_or(archive);
        Ok(archive)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn parse(toml: toml::Value, root: &PathBuf) -> Result<Workspace, anyhow::Error> {
        let root = std::fs::canonicalize(root).unwrap();
        let paths = WorkspacePaths::new(&root, None, None).unwrap();
        WorkspaceParser::from_toml(toml, paths)
    }

    #[test]
    fn demands_workspace_name() {
        let toml: toml::Value = r#"
[workspace]
[toolchains]
[dependencies]
        "#
        .parse::<toml::Value>()
        .unwrap();
        let workspace = parse(toml, &PathBuf::from("."));
        assert_eq!(true, workspace.is_err());
    }

    #[test]
    fn parses_toml_into_workspace_struct() {
        let toml: toml::Value = r#"
[workspace]
name = "tiny_lib"

[toolchains]

[dependencies]
        "#
        .parse::<toml::Value>()
        .unwrap();
        let workspace = parse(toml, &PathBuf::from(".")).unwrap();
        assert_eq!(workspace.name, "tiny_lib");
    }

    #[test]
    fn expects_ignore_patterns_to_be_an_array() {
        let toml: toml::Value = r#"
[workspace]
name = "tiny_lib"
ignore_patterns = {}

[toolchains]

[dependencies]
        "#
        .parse::<toml::Value>()
        .unwrap();

        assert!(parse(toml, &PathBuf::from(".")).is_err());
    }

    #[test]
    fn parses_ignore_patterns_into_workspace() {
        let toml: toml::Value = r#"
[workspace]
name = "tiny_lib"
ignore_patterns = ["node_modules"]

[toolchains]

[dependencies]
        "#
        .parse::<toml::Value>()
        .unwrap();
        let workspace = parse(toml, &PathBuf::from(".")).unwrap();
        assert_eq!(workspace.ignore_patterns, vec!["node_modules"]);
    }

    /*
    #[test]
    fn allows_for_custom_toolchains() {
        let toml: toml::Value = r#"
    [workspace]
    name = "tiny_lib"

    [toolchains]
    erlang = { archive_url = "official", prefix = "otp-prefix" }
    gleam = { archive_url = "https://github.com/forked/gleam", sha1 = "sha1-test" }

    [dependencies]
            "#
        .parse::<toml::Value>()
        .unwrap();
        let _workspace = parse(toml, &PathBuf::from(".")).unwrap();
        assert_eq!(
            "official",
            toolchain_manager.get_archive("erlang").unwrap().url()
        );
        assert_eq!(
            "otp-prefix",
            toolchain_manager.get_archive("erlang").unwrap().prefix()
        );
        assert_eq!(
            "https://github.com/forked/gleam",
            toolchain_manager.get_archive("gleam").unwrap().url()
        );
        assert_eq!(
            "sha1-test",
            toolchain_manager.get_archive("gleam").unwrap().sha1()
        );
    }
    */
}
