use crate::proto::build::warp::{Symbol, symbol};
use tonic::{Request, Response, Status};
use crate::proto::build::warp::codedb::*;
use std::path::Path;

#[derive(Default)]
pub struct GetAst {}

impl GetAst {
    pub fn get_dependencies(
	request: Request<GetAstRequest>,
    ) -> Result<Response<GetAstResponse>, Status> {
	let request_data = request.into_inner();
	let filename = request_data.clone().file;
	println!("Analyzing: {:?}", filename.clone());
	
	match Path::new(&filename).extension() {
	    Some(ext) => if ext == "rs" {
		Ok(Response::new(GetAst::do_get_rs_ast(request_data)))
	    } else { 
		Ok(Response::new(GetAstResponse::default()))
	    },
	    _ => Ok(Response::new(GetAstResponse::default())),
	}
    }

    fn do_get_rs_ast(
	_request: GetAstRequest,
    ) -> GetAstResponse {
	GetAstResponse {
	    response: Some(crate::proto::build::warp::codedb::get_ast_response::Response::Ok(GetAstSuccessResponse{
		file: "test.rs".to_string(),
		symbol: Some(Symbol{sym: Some(symbol::Sym::All(true))}),
		source: "source.rs".to_string(),
		ast: "ast()".to_string()
	    }))}
    }
}

