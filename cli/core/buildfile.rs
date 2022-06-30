use super::*;
use anyhow::*;
use dashmap::DashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use std::vec::Vec;
use toml::Value;
use tracing::*;

pub const ZAPFILE: &str = "Build.toml";

#[derive(Debug)]
pub struct Buildfile {
    /// The Path to this build file.
    pub path: PathBuf,

    /// The Targets defined within this build file.
    pub targets: Vec<Target>,
}

impl Default for Buildfile {
    fn default() -> Self {
        Buildfile {
            path: PathBuf::from(ZAPFILE),
            targets: vec![],
        }
    }
}

impl Buildfile {
    /// Read the input path and parse it as a Buildfile using the existing
    /// rules.
    ///
    /// Unknown rules will be rejected.
    ///
    #[tracing::instrument(name = "Buildfile::from_file", skip(rule_manager))]
    pub fn from_file(
        workspace_prefix: &PathBuf,
        warpfile_path: &PathBuf,
        rule_manager: Arc<DashMap<String, Rule>>,
    ) -> Result<Buildfile, Error> {
        debug!("Parsing Build.toml at {:?}", warpfile_path);

        let contents = fs::read_to_string(&warpfile_path)?.parse::<Value>()?;
        let contents = contents.as_table().context(format!(
            "Expected Buildfile contents to be a TOML table but instead found: {:?}",
            &contents
        ))?;

        let package_dir = warpfile_path.clone();
        let package_dir = &package_dir
            .parent()
            .context(format!("Could not get the parent of: {:?}", &warpfile_path))?
            .to_path_buf();

        let pkg_prefix = package_dir.strip_prefix(workspace_prefix)?.to_path_buf();

        let mut targets: Vec<Target> = vec![];

        let available_rules: Vec<String> =
            rule_manager.iter().map(|r| r.name().to_string()).collect();
        for (rule_name, configs) in contents.iter() {
            for cfg in configs.as_array().context("Rule should be marked as a table, so if you wrote [rule_name], try writing [[rule_name]] instead")? {
                let name = {
                    let name = cfg.get("name").context(format!(
                            "Rule {} in file {:?} is missing a name.",
                            &rule_name, &warpfile_path
                    ))?;
                    name.as_str().context(format!(
                            "Expected name in rule {} in file {:?} to be a String but instead found {}",
                            &rule_name, &warpfile_path, &name
                    ))?
                };

                let label = Label::from_path_and_name(&pkg_prefix, name);
                let rule = rule_manager.get(rule_name).context(format!("Could not find a rule named `{}`, are you sure its spelled correctly and installed in   {}/.warp/rules  ?  \n\nAvailable rules are: {:?}", rule_name, workspace_prefix.to_str().unwrap(), available_rules))?;

                let rule_config = {
                    let table = cfg.as_table().context(format!(
                            "Expected a rule configuration to be a TOML Table, but instead found {:?}",
                            cfg
                    ))?;

                    // NOTE(@ostera): DashMap deadlocks if you take a read and a write borrow on
                    // the same key!
                    let values: RuleConfig = rule.defaults().clone();

                    for (key, value_type) in rule.config().as_map().iter() {
                        let value = match table.get(key) {
                            Some(value) => Buildfile::parse_config_value(value, value_type)?,
                            None => values.get(key).context(format!("When building   {}  I did not find the attribute {:?} on the Build.toml, which is mandatory. You can add it like this:

{} = <value>

", label.to_string(), key, key))?
                        };

                        let expanded_value = Buildfile::expand_value(value, &pkg_prefix)?;

                        values.insert(key.to_string(), expanded_value);
                    }

                    values.insert("name".to_string(), CfgValue::String(name.to_string()));

                    values
                };

                let target = Target::new(label, &rule, rule_config);
                targets.push(target);
            }
        }

        Ok(Buildfile {
            path: warpfile_path.to_path_buf(),
            targets,
        })
    }

    pub fn parse_config_value(
        value: &toml::Value,
        cfg_type: &CfgValueType,
    ) -> Result<CfgValue, anyhow::Error> {
        match cfg_type {
            CfgValueType::String => {
                let s = value
                    .as_str()
                    .context(format!("Expected String but found: {:?}", value))?;
                Ok(CfgValue::String(s.to_string()))
            }
            CfgValueType::Label => {
                let s = value
                    .as_str()
                    .context(format!("Expected Label but found: {:?}", value))?;
                let l = Label::new(s);
                Ok(CfgValue::Label(l))
            }
            CfgValueType::File => {
                let s = value
                    .as_str()
                    .context(format!("Expected File path but found: {:?}", value))?;
                Ok(CfgValue::File(PathBuf::from(s)))
            }
            CfgValueType::List(inner) => {
                let arr = value
                    .as_array()
                    .context(format!("Expected List but found: {:?}", value))?;
                let mut elements = vec![];

                for e in arr {
                    let value = Buildfile::parse_config_value(e, &inner)?;
                    elements.extend(match value {
                        CfgValue::List(subparts) => subparts,
                        el => vec![el],
                    })
                }

                Ok(CfgValue::List(elements))
            }
        }
    }

    pub fn expand_value(
        value: CfgValue,
        warpfile_path: &PathBuf,
    ) -> Result<CfgValue, anyhow::Error> {
        match value {
            CfgValue::File(path) => {
                if path.to_str().unwrap().contains('*') {
                    let entries = glob::glob(warpfile_path.join(&path).to_str().unwrap())
                        .context("Could not read glob pattern")?;

                    let mut files = vec![];
                    for entry in entries {
                        files.push(CfgValue::File(entry?));
                    }
                    Ok(CfgValue::List(files))
                } else {
                    Ok(CfgValue::File(warpfile_path.join(&path)))
                }
            }
            CfgValue::List(parts) => {
                let mut elements = vec![];

                for p in parts {
                    let expanded_value = Buildfile::expand_value(p, &warpfile_path)?;
                    elements.extend(match expanded_value {
                        CfgValue::List(subparts) => subparts,
                        el => vec![el],
                    })
                }

                Ok(CfgValue::List(elements))
            }
            x => Ok(x),
        }
    }
}
