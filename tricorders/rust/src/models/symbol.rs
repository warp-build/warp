use thiserror::Error;

#[derive(Builder, Debug, Default, Clone)]
#[builder(build_fn(error = "SymbolError"))]
pub struct Symbol {
    scope: SymbolScope,

    #[builder(default)]
    name: String,
}

impl Symbol {
    pub fn builder() -> SymbolBuilder {
        SymbolBuilder::default()
    }

    pub fn scope(&self) -> SymbolScope {
        self.scope.clone()
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub enum SymbolScope {
    #[default]
    All,

    Named,
}

#[derive(Error, Debug)]
pub enum SymbolError {
    #[error(transparent)]
    BuilderError(derive_builder::UninitializedFieldError),
}

impl From<derive_builder::UninitializedFieldError> for SymbolError {
    fn from(value: derive_builder::UninitializedFieldError) -> Self {
        SymbolError::BuilderError(value)
    }
}
