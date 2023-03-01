use crate::resolver::Target;
use fxhash::FxHashMap;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    String,
    Target,
    List(Box<Type>),
    Map(FxHashMap<String, Type>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    String(String),
    Target(Target),
    List(Vec<Value>),
    Map(FxHashMap<String, Value>),
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Spec(FxHashMap<String, Type>);

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RawConfig(FxHashMap<String, Value>);

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Config(FxHashMap<String, Value>);

/// A checker that verifies a configuration against a spec and returns a validated configuration.
pub struct Checker;

impl Checker {
    pub fn check(spec: Spec, config: RawConfig) -> Result<Config, CheckerError> {
        todo!()
    }
}

#[derive(Error, Debug)]
pub enum CheckerError {}
