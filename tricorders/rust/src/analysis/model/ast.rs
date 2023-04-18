use std::path::PathBuf;

use syn::parse_quote;
use thiserror::Error;

#[derive(Builder, Debug, Clone)]
#[builder(build_fn(error = "AstError"))]
pub struct Ast {
    ast: syn::File,

    #[builder(setter(into))]
    file: PathBuf,

    source: String,

    test_name: String,
}

impl Ast {
    pub fn builder() -> AstBuilder {
        AstBuilder::default()
    }

    pub fn ast(&self) -> syn::File {
        self.ast.clone()
    }

    pub fn source(&self) -> &str {
        self.source.as_ref()
    }

    pub fn test_name(&self) -> &str {
        self.test_name.as_ref()
    }

    pub fn file(&self) -> &PathBuf {
        &self.file
    }
}

impl Default for Ast {
    fn default() -> Self {
        Self {
            ast: parse_quote!(),
            source: "".to_string(),
            file: PathBuf::from(""),
            test_name: "".to_string(),
        }
    }
}

#[derive(Error, Debug)]
pub enum AstError {
    #[error(transparent)]
    BuilderError(derive_builder::UninitializedFieldError),
}

impl From<derive_builder::UninitializedFieldError> for AstError {
    fn from(value: derive_builder::UninitializedFieldError) -> Self {
        AstError::BuilderError(value)
    }
}
