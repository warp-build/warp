use super::Task;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Builder, Debug, Default, Clone, Serialize, Deserialize)]
#[builder(build_fn(error = "DependenciesError"))]
pub struct Dependencies {
    toolchains: Vec<Task>,
    compile_deps: Vec<Task>,
    runtime_deps: Vec<Task>,
    transitive_compile_deps: Vec<Task>,
    transitive_runtime_deps: Vec<Task>,
}

impl Dependencies {
    pub fn builder() -> DependenciesBuilder {
        Default::default()
    }

    pub fn all_deps(&self) -> impl Iterator<Item = &Task> {
        self.toolchains()
            .iter()
            .chain(self.transitive_compile_deps().iter())
            .chain(self.transitive_runtime_deps().iter())
            .chain(self.compile_deps().iter())
            .chain(self.runtime_deps().iter())
    }

    pub fn all_compile_deps(&self) -> impl Iterator<Item = &Task> {
        self.toolchains()
            .iter()
            .chain(self.transitive_compile_deps().iter())
            .chain(self.compile_deps().iter())
    }

    pub fn toolchains(&self) -> &[Task] {
        self.toolchains.as_ref()
    }

    pub fn compile_deps(&self) -> &[Task] {
        self.compile_deps.as_ref()
    }

    pub fn runtime_deps(&self) -> &[Task] {
        self.runtime_deps.as_ref()
    }

    pub fn set_toolchains(&mut self, toolchains: Vec<Task>) {
        self.toolchains = toolchains;
    }

    pub fn transitive_compile_deps(&self) -> &[Task] {
        self.transitive_compile_deps.as_ref()
    }

    pub fn transitive_runtime_deps(&self) -> &[Task] {
        self.transitive_runtime_deps.as_ref()
    }
}

#[derive(Error, Debug)]
pub enum DependenciesError {
    #[error(transparent)]
    BuilderError(derive_builder::UninitializedFieldError),
}

impl From<derive_builder::UninitializedFieldError> for DependenciesError {
    fn from(value: derive_builder::UninitializedFieldError) -> Self {
        DependenciesError::BuilderError(value)
    }
}
