use crate::proto::build::warp::codedb::*;
use crate::proto::build::warp::Symbol;
use std::path::Path;
use thiserror::*;
use tokio::fs;
use tonic::{Request, Response, Status};
use tracing::*;
use tree_sitter::{Parser, Tree};

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
        let mut parser = Parser::new();
	parser.set_language(tree_sitter_rust::language()).expect("Error loading Rust grammar");

        let source =
            fs::read_to_string(&request.file)
                .await
                .map_err(|err| GetAstError::CouldNotReadFile {
                    file: request.file.clone(),
                    err,
                });

        let src = source.unwrap();
        let ast = parser.parse(src.clone(), None).unwrap();

        let response = GetAst::handle_result(request, ast, &src).await;

        GetAstResponse {
            response: Some(response.unwrap()),
        }
    }

    async fn handle_result(
        request: GetAstRequest,
        ast: Tree,
        source: &str,
    ) -> Result<crate::proto::build::warp::codedb::get_ast_response::Response, GetAstError> {
        let symbol = request.symbol.unwrap();

        Ok(
            crate::proto::build::warp::codedb::get_ast_response::Response::Ok(
                GetAstSuccessResponse {
                    file: request.file,
                    symbol: Some(Symbol { sym: symbol.sym }),
                    source: source.to_string(),
                    ast: format!("{:?}", ast),
                },
            ),
        )
    }
}
