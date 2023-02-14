use crate::proto::build::warp::codedb::*;
use crate::proto::build::warp::{symbol, Symbol};
use std::path::Path;
use thiserror::*;
use tokio::fs;
use tonic::{Request, Response, Status};
use tracing::*;

#[derive(Default)]
pub struct GetAst {}

#[derive(Error, Debug)]
pub enum GetAstError {
    #[error("Could not load file at {file:?} due to {err:?}")]
    CouldNotReadFile { file: String, err: std::io::Error },
}

impl GetAst {
    pub async fn get_dependencies(
        request: Request<GetAstRequest>,
    ) -> Result<Response<GetAstResponse>, Status> {
        let request_data = request.into_inner();
        let filename = request_data.clone().file;
        println!("Analyzing: {:?}", filename.clone());

        match Path::new(&filename).extension() {
            Some(ext) if ext == "rs" => {
                Ok(Response::new(GetAst::do_get_rs_ast(request_data).await))
            }
            _ => Ok(Response::new(GetAstResponse::default())),
        }
    }

    async fn do_get_rs_ast(request: GetAstRequest) -> GetAstResponse {
        let response = GetAst::handle_result(request).await;

        GetAstResponse {
            response: Some(response.unwrap()),
        }
    }

    async fn handle_result(
        request: GetAstRequest,
    ) -> Result<crate::proto::build::warp::codedb::get_ast_response::Response, GetAstError> {
        let source = match request.symbol.unwrap().sym.unwrap() {
            symbol::Sym::All(true) => fs::read_to_string(&request.file).await.map_err(|err| {
                GetAstError::CouldNotReadFile {
                    file: request.file.clone(),
                    err,
                }
            })?,
            symbol::Sym::Named(_) => "".to_string(),
            _ => "".to_string(),
        };

        Ok(
            crate::proto::build::warp::codedb::get_ast_response::Response::Ok(
                GetAstSuccessResponse {
                    file: request.file,
                    symbol: Some(Symbol {
                        sym: Some(symbol::Sym::All(true)),
                    }),
                    source: source,
                    ast: "ast()".to_string(),
                },
            ),
        )
    }
}
