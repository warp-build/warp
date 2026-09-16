use crate::model::ExecutableSpec;
use std::collections::BTreeMap;
use std::path::PathBuf;
use thiserror::Error;
use tracing::debug;

pub enum ActionRunnerFlow {
    Completed,
    MissingInputs { inputs: Vec<PathBuf> },
}

pub struct ActionRunner;

impl ActionRunner {
    pub async fn run(
        store_path: &PathBuf,
        env: &BTreeMap<String, String>,
        spec: &ExecutableSpec,
    ) -> Result<ActionRunnerFlow, ActionRunnerError> {
        for action in spec.actions() {
            let res = action.run(spec.target(), store_path, env).await;
            debug!("executed action with result: {:?}", res);
            if let Err(err) = res {
                return Err(ActionRunnerError::ActionError(err));
            }
        }
        Ok(ActionRunnerFlow::Completed)
    }
}

#[derive(Error, Debug)]
pub enum ActionRunnerError {
    #[error(transparent)]
    Unknown(anyhow::Error),

    #[error(transparent)]
    ActionError(anyhow::Error),
}

impl From<anyhow::Error> for ActionRunnerError {
    fn from(value: anyhow::Error) -> Self {
        ActionRunnerError::Unknown(value)
    }
}
