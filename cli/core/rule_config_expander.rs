use std::path::PathBuf;

use super::*;
use thiserror::*;

pub struct ConfigExpander;

#[derive(Error, Debug)]
pub enum ConfigExpanderError {
    #[error("Missing mandatory field {field_name} in target: {target:?}")]
    MissingMandatoryField { target: Target, field_name: String },

    #[error("Invalid glob pattern: {path:?}\n\nError: {err:?}")]
    InvalidGlobPattern {
        path: String,
        err: glob::PatternError,
    },

    #[error("Field {field:?} was expected to be of type {expected_type:?}")]
    InvalidTypeForField {
        field: CfgValue,
        expected_type: CfgValueType,
    },

    #[error(transparent)]
    GlobError(glob::GlobError),

    #[error(transparent)]
    LabelError(LabelError),
}

impl ConfigExpander {
    #[tracing::instrument(name = "ConfigExpander::expand", skip(self, rule, target))]
    pub async fn expand(
        &self,
        rule: &Rule,
        target: &Target,
    ) -> Result<RuleConfig, ConfigExpanderError> {
        let mut values: RuleConfig = rule.defaults.clone();

        for (key, value_type) in rule.config.as_map() {
            let value =
                match target.config.get(key) {
                    Some(value) => value,
                    None => values.get(key).ok_or_else(|| {
                        ConfigExpanderError::MissingMandatoryField {
                            target: target.clone(),
                            field_name: key.clone(),
                        }
                    })?,
                };

            let expanded_value = self.expand_value(&target.label, value, value_type)?;

            values.insert(key.to_string(), expanded_value);
        }

        values.insert(
            "name".to_string(),
            CfgValue::String(target.label.name().to_string()),
        );

        Ok(values)
    }

    #[tracing::instrument(name = "ConfigExpander::expand_value", skip(self))]
    pub fn expand_value(
        &self,
        label: &Label,
        value: CfgValue,
        value_type: &CfgValueType,
    ) -> Result<CfgValue, ConfigExpanderError> {
        match (value_type, &value) {
            (CfgValueType::File, CfgValue::File(path)) => {
                self.expand_glob(label, path.to_str().unwrap())
            }
            (CfgValueType::File, CfgValue::String(path)) => self.expand_glob(label, path),
            (CfgValueType::Label, CfgValue::String(name)) => {
                let label = name.parse().map_err(ConfigExpanderError::LabelError)?;
                Ok(CfgValue::Label(label))
            }
            (CfgValueType::Label, CfgValue::Label(label)) => Ok(CfgValue::Label(label.clone())),
            (CfgValueType::List(t), CfgValue::List(parts)) => {
                self.expand_list(label, parts.to_vec(), t)
            }
            (CfgValueType::String, CfgValue::String(_)) => Ok(value),
            _ => Err(ConfigExpanderError::InvalidTypeForField {
                field: value,
                expected_type: value_type.clone(),
            }),
        }
    }

    #[tracing::instrument(name = "ConfigExpander::expand_glob", skip(self))]
    pub fn expand_glob(
        &self,
        label: &Label,
        cfg_path: &str,
    ) -> Result<CfgValue, ConfigExpanderError> {
        let workspace = label.workspace().unwrap();
        let path = {
            let p = workspace.join(label.path());
            let p = if p.ends_with(&cfg_path) {
                p
            } else {
                p.join(cfg_path)
            };
            p.to_str().unwrap().to_string()
        };
        if path.contains('*') {
            let entries =
                glob::glob(&path).map_err(|err| ConfigExpanderError::InvalidGlobPattern {
                    path: path.to_string(),
                    err,
                })?;

            let mut files = vec![];
            for entry in entries {
                let entry = entry.map_err(ConfigExpanderError::GlobError)?;
                let entry = entry.strip_prefix(workspace).unwrap().to_path_buf();
                files.push(CfgValue::File(entry));
            }
            Ok(CfgValue::List(files))
        } else {
            let path = PathBuf::from(path)
                .strip_prefix(workspace)
                .unwrap()
                .to_path_buf();
            Ok(CfgValue::File(path))
        }
    }

    #[tracing::instrument(name = "ConfigExpander::expand_list", skip(self))]
    pub fn expand_list(
        &self,
        label: &Label,
        parts: Vec<CfgValue>,
        value_type: &CfgValueType,
    ) -> Result<CfgValue, ConfigExpanderError> {
        let mut elements = vec![];

        for p in parts {
            let expanded_value = self.expand_value(label, p, value_type)?;
            elements.extend(match expanded_value {
                CfgValue::List(subparts) => subparts,
                el => vec![el],
            })
        }

        Ok(CfgValue::List(elements))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
