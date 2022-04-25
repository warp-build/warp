pub use deno_core;

use log::*;
use std::path::PathBuf;
use std::rc::Rc;

mod error {
    use thiserror::Error;
    #[derive(Error, Debug)]
    pub enum LoadError {
        #[error("The module name `{module_name}` is invalid: {reason:?}")]
        BadModuleName { module_name: String, reason: String },

        #[error("The module name `{module_name}` could not be resolved: {reason:?}")]
        ModuleResolutionError { module_name: String, reason: String },

        #[error("The module name `{module_name}` could not be evaluated: {reason:?}")]
        ModuleEvaluationError { module_name: String, reason: String },

        #[error("Something went wrong.")]
        Unknown,
    }
}

pub struct BuildScript {
    pub runtime: deno_core::JsRuntime,
}

impl BuildScript {
    pub fn new() -> BuildScript {
        let rt_options = deno_core::RuntimeOptions {
            module_loader: Some(Rc::new(deno_core::FsModuleLoader)),
            ..Default::default()
        };

        let runtime = deno_core::JsRuntime::new(rt_options);

        BuildScript { runtime }
    }

    pub async fn load_from_str(
        &mut self,
        module_name: &str,
        module_code: &str,
    ) -> Result<deno_core::ModuleId, error::LoadError> {
        trace!("Loading module from string");
        self.load_from_parts(module_name, Some(module_code.to_string()))
            .await
    }

    pub async fn load_from_path(
        &mut self,
        module: PathBuf,
    ) -> Result<deno_core::ModuleId, error::LoadError> {
        trace!("Loading module at {:?}", &module);
        self.load_from_parts(module.to_str().unwrap(), None).await
    }

}
