use super::{Config, Rule, Type, Value};
use crate::model::{ConcreteTarget, Signature, TargetError};
use thiserror::Error;
use tracing::{error, trace};

/// Rule configuration expansion and type-checking.
///
/// This struct receives a [Signature] and a [Rule], and it produces a [Config] object that matches
/// the schema in the [Rule]. Side-effects will occur if any of the [Signature] values includes
/// globs and the expected type is a File or List of Files.
///
pub struct Expander;

impl Expander {
    #[tracing::instrument(name = "Expander::expand", skip(self, rule, sig))]
    pub async fn expand(&self, rule: &Rule, sig: &Signature) -> Result<Config, ExpanderError> {
        let mut values: Config = rule.defaults().clone();

        for (key, value_type) in rule.spec().as_map() {
            trace!("Expanding {:?} of type {:?}", key, value_type);
            let value = match sig.config().get(key) {
                Some(value) => value,
                None => values
                    .get(key)
                    .ok_or_else(|| ExpanderError::MissingMandatoryField {
                        sig: sig.clone().into(),
                        field_name: key.clone(),
                        rule: rule.clone().into(),
                    })?,
            }
            .clone();

            let expanded_value = self.expand_value(&sig.target(), value, &value_type)?;

            values.insert(key.to_string(), expanded_value);
        }

        let name = Value::String(sig.target().to_string());
        trace!("Expanded config for {:?}", name);
        values.insert("name".to_string(), name);

        Ok(values)
    }

    #[tracing::instrument(name = "Expander::expand_value", skip(self))]
    pub fn expand_value(
        &self,
        target: &ConcreteTarget,
        value: Value,
        value_type: &Type,
    ) -> Result<Value, ExpanderError> {
        trace!("Expanding value {:?} of type {:?}", value, value_type);
        match (value_type, &value) {
            (Type::File, Value::File(path)) => self.expand_glob(target, path.to_str().unwrap()),
            (Type::File, Value::String(path)) => self.expand_glob(target, path),
            (Type::Target, Value::String(name)) => {
                let target = name.parse().map_err(ExpanderError::TargetError)?;
                Ok(Value::Target(target))
            }
            (Type::Target, Value::Target(target)) => Ok(Value::Target(target.clone())),
            (Type::List(t), Value::List(parts)) => self.expand_list(target, parts.to_vec(), t),
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

    #[tracing::instrument(name = "Expander::expand_glob", skip(self))]
    pub fn expand_glob(
        &self,
        target: &ConcreteTarget,
        cfg_path: &str,
    ) -> Result<Value, ExpanderError> {
        let path = {
            let p = target.path().to_owned();
            let p = if p.ends_with(cfg_path) {
                p
            } else {
                p.join(cfg_path)
            };
            p.to_str().unwrap().to_string()
        };
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
            Ok(Value::File(path.into()))
        }
    }

    #[tracing::instrument(name = "Expander::expand_list", skip(self))]
    pub fn expand_list(
        &self,
        target: &ConcreteTarget,
        parts: Vec<Value>,
        value_type: &Type,
    ) -> Result<Value, ExpanderError> {
        let mut elements = vec![];

        for p in parts {
            let expanded_value = self.expand_value(target, p, value_type)?;
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
    #[error("Missing mandatory field {field_name} in sig: {sig:?}")]
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
}
