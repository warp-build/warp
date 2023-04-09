use crate::analysis::model::{Ast, AstError, Symbol};
use crate::analysis::tree_splitter::TreeSplitter;
use std::path::Path;
use thiserror::*;
use tokio::fs;
use tracing::*;

#[derive(Default)]
pub struct GetAst {}

impl GetAst {
    pub async fn get_ast(file: String, symbol: Symbol) -> Result<Ast, GetAstError> {
        println!("Analyzing: {:?}", file);

        if !Self::is_supported_file(&file) {
            return Err(GetAstError::UnsupportedFile { file });
        }

        match &symbol {
            Symbol::All => Self::do_get_all_ast(file).await,
            Symbol::Named { name: _ } => Self::do_get_named_ast(file, symbol).await,
        }
    }

    async fn do_get_all_ast(file: String) -> Result<Ast, GetAstError> {
        let source = fs::read_to_string(&file)
            .await
            .map_err(|err| GetAstError::CouldNotReadFile {
                file: file.to_string(),
                err,
            });

        let src = source.unwrap();
        let ast = syn::parse_file(&src)
            .map_err(|err| GetAstError::CouldNotParseFile {
                file: file.to_string(),
                err,
            })
            .unwrap();

        Ok(Ast::builder().ast(ast).source(src).file(file).build()?)
    }

    async fn do_get_named_ast(file: String, symbol: Symbol) -> Result<Ast, GetAstError> {
        let source = fs::read_to_string(&file)
            .await
            .map_err(|err| GetAstError::CouldNotReadFile {
                file: file.to_string(),
                err,
            })
            .unwrap();

        let (ast, src) = TreeSplitter::tree_split(symbol.clone(), &source);

        Ok(Ast::builder()
            .ast(ast)
            .source(src)
            .file(file)
            .symbol(symbol)
            .build()?)
    }

    pub fn is_supported_file(file: &str) -> bool {
        match Path::new(&file).extension() {
            Some(ext) => ext == "rs",
            _ => false,
        }
    }
}

#[derive(Error, Debug)]
pub enum GetAstError {
    #[error("Could not load file at {file:?} due to {err:?}")]
    CouldNotReadFile { file: String, err: std::io::Error },

    #[error("Could not parse file {file:?} due to {err:?}")]
    CouldNotParseFile { file: String, err: syn::Error },

    #[error("Unsupported file {file:?}")]
    UnsupportedFile { file: String },

    #[error(transparent)]
    AstError(AstError),
}

impl From<AstError> for GetAstError {
    fn from(value: AstError) -> Self {
        GetAstError::AstError(value)
    }
}
