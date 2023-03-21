use crate::models::Symbol;
use syn::parse_quote;

#[derive(Builder, Debug, Clone)]
#[builder(build_fn(error = "crate::GetAstError"))]
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

impl From<derive_builder::UninitializedFieldError> for crate::GetAstError {
    fn from(value: derive_builder::UninitializedFieldError) -> Self {
        crate::GetAstError::BuilderError(value)
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
