use crate::proto::build::warp::tricorder::{tricorder_service_server::TricorderService, EnsureReadyRequest, EnsureReadyResponse, GetAstRequest, GetAstResponse, GenerateSignatureRequest, GenerateSignatureResponse};
use tonic::{Request, Response, Status};

#[derive(Default)]
pub struct TricorderServiceImpl {}

#[tonic::async_trait]
impl TricorderService for TricorderServiceImpl {
	async fn ensure_ready(
		&self,
		_request: Request<EnsureReadyRequest>
	) -> Result<Response<EnsureReadyResponse>, Status> {
		Ok(Response::new(EnsureReadyResponse::default()))	
	}
    async fn get_ast(
        &self,
        request: Request<GetAstRequest>,
    ) -> Result<Response<GetAstResponse>, Status> {
        crate::get_ast::GetAst::get_ast(request).await
    }

    async fn generate_signature(
        &self,
        request: Request<GenerateSignatureRequest>,
    ) -> Result<Response<GenerateSignatureResponse>, Status> {
        crate::generate_signature::GenerateSignature::generate_signature(request).await
    }
}
