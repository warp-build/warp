use std::path::{Path, PathBuf};

use super::{Config, Rule, Type, Value};
use crate::model::{Signature, TargetError};
use thiserror::Error;
use tracing::{error, instrument, trace};

/// Rule configuration expansion and type-checking.
///
/// This struct receives a [Signature] and a [Rule], and it produces a [Config] object that matches
/// the schema in the [Rule]. Side-effects will occur if any of the [Signature] values includes
/// globs and the expected type is a File or List of Files.
///
pub struct Expander;

impl Expander {
    #[instrument(name = "Expander::expand", skip(self, rule, sig))]
    pub async fn expand(&self, rule: &Rule, sig: &Signature) -> Result<Config, ExpanderError> {
        let mut values: Config = rule.defaults().clone();

        for (key, value_type) in rule.spec().as_map() {
            trace!("Expanding {:?} of type {:?}", key, value_type);
            let value = match sig.config().get(key) {
                Some(value) => value,
                None => values
                    .get(key)
                    .ok_or_else(|| ExpanderError::MissingMandatoryField {
                        field_name: key.clone(),
                        sig: sig.clone().into(),
                        rule: rule.clone().into(),
                    })?,
            }
            .clone();

            let expanded_value = self.expand_value(sig.target().dir(), value, value_type)?;

            values.insert(key.to_string(), expanded_value);
        }

        let name = Value::String(sig.target().to_string());
        trace!("Expanded config for {:?}", name);
        values.insert("name".to_string(), name);

        Ok(values)
    }

    #[instrument(name = "Expander::expand_value", skip(self), ret)]
    pub fn expand_value(
        &self,
        root: &Path,
        value: Value,
        value_type: &Type,
    ) -> Result<Value, ExpanderError> {
        trace!("Expanding value {:?} of type {:?}", value, value_type);
        match (value_type, &value) {
            (Type::File, Value::File(path)) => self.expand_glob(root, path),
            (Type::File, Value::String(path)) => self.expand_glob(root, Path::new(path)),
            (Type::Target, Value::String(name)) => {
                let target = name.parse().map_err(ExpanderError::TargetError)?;
                Ok(Value::Target(target))
            }
            (Type::Target, Value::Target(target)) => Ok(Value::Target(target.clone())),
            (Type::List(t), Value::List(parts)) => self.expand_list(root, parts.to_vec(), t),
            (Type::String, Value::String(_)) => Ok(value),
            _ => {
                error!(
                    "Could not expand value {:?} of type {:?}",
                    value, value_type
                );
                Err(ExpanderError::InvalidTypeForField {
                    field: value,
                    expected_type: value_type.clone(),
                })
            }
        }
    }

    #[instrument(name = "Expander::expand_glob", skip(self))]
    pub fn expand_glob(&self, root: &Path, cfg_path: &Path) -> Result<Value, ExpanderError> {
        let path = root.join(cfg_path).to_string_lossy().to_string();
        if path.contains('*') {
            let entries = glob::glob(&path).map_err(|err| ExpanderError::InvalidGlobPattern {
                path: path.to_string(),
                err,
            })?;

            let mut files = vec![];
            for entry in entries {
                let entry = entry.map_err(ExpanderError::GlobError)?;
                files.push(Value::File(entry));
            }
            Ok(Value::List(files))
        } else {
            Ok(Value::File(PathBuf::from(path)))
        }
    }

    #[instrument(name = "Expander::expand_list", skip(self))]
    pub fn expand_list(
        &self,
        root: &Path,
        parts: Vec<Value>,
        value_type: &Type,
    ) -> Result<Value, ExpanderError> {
        let mut elements = vec![];

        for p in parts {
            let expanded_value = self.expand_value(root, p, value_type)?;
            elements.extend(match expanded_value {
                Value::List(subparts) => subparts,
                el => vec![el],
            })
        }

        Ok(Value::List(elements))
    }
}

#[derive(Error, Debug)]
pub enum ExpanderError {
    #[error("Missing mandatory field \"{field_name}\" in sig: {sig:?}")]
    MissingMandatoryField {
        sig: Box<Signature>,
        field_name: String,
        rule: Box<Rule>,
    },

    #[error("Invalid glob pattern: {path:?}\n\nError: {err:?}")]
    InvalidGlobPattern {
        path: String,
        err: glob::PatternError,
    },

    #[error("Field {field:?} was expected to be of type {expected_type:?}")]
    InvalidTypeForField { field: Value, expected_type: Type },

    #[error(transparent)]
    GlobError(glob::GlobError),

    #[error(transparent)]
    TargetError(TargetError),
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_fs::prelude::*;

    #[test]
    fn expands_string_value_from_a_target_file() {
        let root = assert_fs::TempDir::new().unwrap();

        root.child("hello/1").touch().unwrap();
        root.child("hello/2").touch().unwrap();
        root.child("hello/3").touch().unwrap();

        let value = Expander
            .expand_value(root.path(), Value::String("hello/*".into()), &Type::File)
            .unwrap();
        assert_matches!(value, Value::List(values) if values.len() == 3);
    }

    #[test]
    fn expands_file_value_from_a_target_file() {
        let root = assert_fs::TempDir::new().unwrap();

        root.child("hello/1").touch().unwrap();
        root.child("hello/2").touch().unwrap();
        root.child("hello/3").touch().unwrap();

        let value = Expander
            .expand_value(root.path(), Value::File("hello/*".into()), &Type::File)
            .unwrap();
        assert_matches!(value, Value::List(values) if values.len() == 3);
    }

    #[test]
    fn expands_globs_from_a_dir_root() {
        let root = assert_fs::TempDir::new().unwrap();
        root.child("hello/1").touch().unwrap();
        root.child("hello/2").touch().unwrap();
        root.child("hello/3").touch().unwrap();

        let value = Expander
            .expand_glob(root.path(), Path::new("hello/*"))
            .unwrap();

        assert_matches!(value, Value::List(values) if values.len() == 3);
    }
}
