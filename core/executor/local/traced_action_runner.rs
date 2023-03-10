use crate::model::ExecutableSpec;
use std::{collections::BTreeMap, path::PathBuf};
use thiserror::Error;

pub enum ActionRunnerFlow {
    Completed,
    MissingInputs { inputs: Vec<PathBuf> },
}

pub struct TracedActionRunner;

impl TracedActionRunner {
    pub async fn run(
        store_path: &PathBuf,
        env: &BTreeMap<String, String>,
        spec: &ExecutableSpec,
    ) -> Result<ActionRunnerFlow, ActionRunnerError> {
        for action in spec.actions() {
            action.run(spec.target(), store_path, env).await?;
        }
        Ok(ActionRunnerFlow::Completed)
    }
}

#[derive(Error, Debug)]
pub enum ActionRunnerError {
    #[error(transparent)]
    Unknown(anyhow::Error),
}

impl From<anyhow::Error> for ActionRunnerError {
    fn from(value: anyhow::Error) -> Self {
        ActionRunnerError::Unknown(value)
    }
}
