use crate::analysis::model::{Ast, AstError};
use crate::analysis::tree_splitter::TreeSplitter;
use log::*;
use std::path::Path;
use thiserror::*;

#[derive(Default)]
pub struct GetAst {}

impl GetAst {
    pub async fn get_ast(file: &Path, test_matcher: Vec<String>) -> Result<Vec<Ast>, GetAstError> {
        info!("Analyzing: {:?}", &file);

        let sources = TreeSplitter::expand_file(file.to_path_buf());

        let matching_tests: Vec<String> = TreeSplitter::find_matching_tests(test_matcher, &sources);

        let asts: Vec<Ast> = matching_tests
            .iter()
            .map(|test_name| {
                let sources = sources.to_string();
                let (ast, src) = TreeSplitter::tree_split(test_name.clone(), sources);
                Ast::builder()
                    .ast(ast)
                    .source(src)
                    .file(file)
                    .test_name(test_name.to_string())
                    .build()
                    .unwrap()
            })
            .collect();

        Ok(asts)
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
