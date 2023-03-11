use super::TargetId;
use thiserror::Error;

#[derive(Builder, Debug, Default, Clone)]
#[builder(build_fn(error = "DependenciesError"))]
pub struct Dependencies {
    toolchains: Vec<TargetId>,
    compile_deps: Vec<TargetId>,
    transitive_deps: Vec<TargetId>,
    runtime_deps: Vec<TargetId>,
}

impl Dependencies {
    pub fn builder() -> DependenciesBuilder {
        Default::default()
    }

    pub fn toolchains(&self) -> &[TargetId] {
        self.toolchains.as_ref()
    }

    pub fn compile_deps(&self) -> &[TargetId] {
        self.compile_deps.as_ref()
    }

    pub fn transitive_deps(&self) -> &[TargetId] {
        self.transitive_deps.as_ref()
    }

    pub fn runtime_deps(&self) -> &[TargetId] {
        self.runtime_deps.as_ref()
    }

    pub fn set_toolchains(&mut self, toolchains: Vec<TargetId>) {
        self.toolchains = toolchains;
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
