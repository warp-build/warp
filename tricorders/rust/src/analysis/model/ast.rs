use super::Symbol;
use syn::parse_quote;
use thiserror::Error;

#[derive(Builder, Debug, Clone)]
#[builder(build_fn(error = "AstError"))]
pub struct Ast {
    ast: syn::File,

    file: String,

    source: String,

    symbol: Symbol,
}

impl Ast {
    pub fn builder() -> AstBuilder {
        AstBuilder::default()
    }

    pub fn ast(&self) -> syn::File {
        self.ast.clone()
    }

    pub fn file(&self) -> &str {
        self.file.as_ref()
    }

    pub fn source(&self) -> &str {
        self.source.as_ref()
    }

    pub fn symbol(&self) -> &Symbol {
        &self.symbol
    }
}

impl Default for Ast {
    fn default() -> Self {
        Self {
            ast: parse_quote!(),
            source: "".to_string(),
            file: "".to_string(),
            symbol: Symbol::default(),
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
