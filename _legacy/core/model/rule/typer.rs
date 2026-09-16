use super::{ConfigError, Type, Value};
use std::path::PathBuf;

pub struct Typer;

impl Typer {
    pub fn typecheck(mut json: serde_json::Value, type_: Type) -> Result<Value, ConfigError> {
        match &mut json {
            serde_json::Value::String(string) => match type_ {
                Type::String => Ok(Value::String(std::mem::take(string))),
                Type::Target => Ok(Value::Target(
                    string.parse().map_err(ConfigError::TargetError)?,
                )),
                Type::File => Ok(Value::File(PathBuf::from(std::mem::take(string)))),
                Type::List(_) => Err(ConfigError::TypeMismatch {
                    expected: type_.clone(),
                    actual: json.clone(),
                }),
                Type::Map(_) => todo!(),
            },
            serde_json::Value::Array(parts) => match type_ {
                Type::List(element_type) => {
                    let mut ps = vec![];
                    for part in parts {
                        let p = Typer::typecheck(part.clone(), *element_type.clone())?;
                        ps.push(p);
                    }
                    Ok(Value::List(ps))
                }
                _ => Err(ConfigError::TypeMismatch {
                    expected: type_.clone(),
                    actual: json.clone(),
                }),
            },
            _ => Err(ConfigError::UnsupportedValueType {
                value: json.clone(),
            }),
        }
    }
}
