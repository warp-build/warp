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
}

impl ConfigExpander {
    pub async fn expand(
        &self,
        rule: &Rule,
        target: &Target,
    ) -> Result<RuleConfig, ConfigExpanderError> {
        let mut values: RuleConfig = rule.defaults.clone();

        for (key, value_type) in rule.config.as_map().iter() {
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

            let expanded_value = self.expand_value(value, &value_type)?;

            values.insert(key.to_string(), expanded_value);
        }

        values.insert(
            "name".to_string(),
            CfgValue::String(target.label.to_string()),
        );

        Ok(values)
    }

    pub fn expand_value(
        &self,
        value: CfgValue,
        value_type: &CfgValueType,
    ) -> Result<CfgValue, ConfigExpanderError> {
        match (value_type, &value) {
            (CfgValueType::File, CfgValue::File(_)) => Ok(value),
            (CfgValueType::File, CfgValue::String(path)) => self.expand_glob(&path),
            (CfgValueType::String, CfgValue::String(_)) => Ok(value),
            (CfgValueType::List(t), CfgValue::List(parts)) => self.expand_list(parts.to_vec(), &t),
            _ => Err(ConfigExpanderError::InvalidTypeForField {
                field: value,
                expected_type: value_type.clone(),
            }),
        }
    }

    pub fn expand_glob(&self, path: &str) -> Result<CfgValue, ConfigExpanderError> {
        if path.contains('*') {
            let entries =
                glob::glob(path).map_err(|err| ConfigExpanderError::InvalidGlobPattern {
                    path: path.to_string(),
                    err,
                })?;

            let mut files = vec![];
            for entry in entries {
                let entry = entry.map_err(ConfigExpanderError::GlobError)?;
                files.push(CfgValue::File(entry));
            }
            Ok(CfgValue::List(files))
        } else {
            Ok(CfgValue::File(PathBuf::from(path)))
        }
    }

    pub fn expand_list(
        &self,
        parts: Vec<CfgValue>,
        value_type: &CfgValueType,
    ) -> Result<CfgValue, ConfigExpanderError> {
        let mut elements = vec![];

        for p in parts {
            let expanded_value = self.expand_value(p, &value_type)?;
            elements.extend(match expanded_value {
                CfgValue::List(subparts) => subparts,
                el => vec![el],
            })
        }

        Ok(CfgValue::List(elements))
    }
}
