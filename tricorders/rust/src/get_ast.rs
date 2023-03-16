use crate::proto::build::warp::symbol::Sym::*;
use crate::proto::build::warp::tricorder::get_ast_response::Response as AstResponse;
use crate::proto::build::warp::{Dependency, Symbol};
use crate::tree_splitter::TreeSplitter;
use std::path::Path;
pub(crate) use thiserror::*;
use tokio::fs;
use tonic::{Request, Response, Status};
use tracing::*;
mod proto {
    pub use crate::proto::build::warp::tricorder::*;
}

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
    pub async fn get_ast(
        request: Request<proto::GetAstRequest>,
    ) -> Result<Response<proto::GetAstResponse>, Status> {
        let request_data = request.into_inner();
        let filename = request_data.clone().file;
        println!("Analyzing: {:?}", filename.clone());

        match Path::new(&filename).extension() {
            Some(ext) if ext == "rs" => match request_data.symbol.unwrap().sym.unwrap() {
                All(_) => Ok(Response::new(proto::GetAstResponse {
                    response: Some(
                        GetAst::do_get_all_rs_ast(&request_data.file, request_data.dependencies)
                            .await,
                    ),
                })),
                Named(name) => Ok(Response::new(proto::GetAstResponse {
                    response: Some(
                        GetAst::do_get_named_rs_ast(
                            &request_data.file,
                            &name,
                            request_data.dependencies,
                        )
                        .await,
                    ),
                })),
            },
            _ => Ok(Response::new(proto::GetAstResponse::default())),
        }
    }

    async fn do_get_all_rs_ast(
        file: &str,
        _deps: Vec<Dependency>,
    ) -> proto::get_ast_response::Response {
        let source = fs::read_to_string(&file)
            .await
            .map_err(|err| GetAstError::CouldNotReadFile {
                file: file.clone().to_string(),
                err,
            });

        let src = source.unwrap();
        let ast = syn::parse_file(&src)
            .map_err(|err| GetAstError::CouldNotParseFile {
                file: file.clone().to_string(),
                err,
            })
            .unwrap();

        AstResponse::Ok(proto::GetAstSuccessResponse {
            file: file.to_string(),
            symbol: Some(Symbol {
                sym: Some(All(true)),
            }),
            source: src.to_string(),
            ast: format!("{:#?}", ast.items),
        })
    }

    async fn do_get_named_rs_ast(
        file: &str,
        symbol_name: &str,
        _deps: Vec<Dependency>,
    ) -> proto::get_ast_response::Response {
        let source = fs::read_to_string(&file)
            .await
            .map_err(|err| GetAstError::CouldNotReadFile {
                file: file.clone().to_string(),
                err,
            })
            .unwrap();

        let (ast, src) = TreeSplitter::tree_split(symbol_name, &source);
        AstResponse::Ok(proto::GetAstSuccessResponse {
            file: file.to_string(),
            symbol: Some(Symbol {
                sym: Some(Named(symbol_name.to_string())),
            }),
            source: src.to_string(),
            ast: format!("{:#?}", ast),
        })
    }
}
