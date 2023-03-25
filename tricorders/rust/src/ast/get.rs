use crate::models::{Ast, Symbol};
use crate::tree_splitter::TreeSplitter;
use crate::AstError;
use std::path::Path;
pub(crate) use thiserror::*;
use tokio::fs;
use tracing::*;

#[derive(Default)]
pub struct GetAst {}

#[derive(Error, Debug)]
pub enum GetAstError {
    #[error("Could not load file at {file:?} due to {err:?}")]
    CouldNotReadFile { file: String, err: std::io::Error },

    #[error("Could not parse file {file:?} due to {err:?}")]
    CouldNotParseFile { file: String, err: syn::Error },
}

impl GetAst {
    pub async fn get_ast(file: String, symbol: Symbol) -> Result<Ast, AstError> {
        println!("Analyzing: {:?}", file);

        match Path::new(&file).extension() {
            Some(ext) if ext == "rs" => match &symbol {
                Symbol::All => Self::do_get_all_ast(file).await,
                Symbol::Named { name: _ } => Self::do_get_named_ast(file, symbol).await,
            },
            _ => Ok(Ast::default()),
        }
    }

    async fn do_get_all_ast(file: String) -> Result<Ast, AstError> {
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

    async fn do_get_named_ast(file: String, symbol: Symbol) -> Result<Ast, AstError> {
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
}
