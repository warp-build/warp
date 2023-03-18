mod mappers;
use tonic::{Request, Response, Status};
use crate::proto::build::warp::tricorder::{
    tricorder_service_server::TricorderService, EnsureReadyRequest, EnsureReadyResponse,
    GenerateSignatureRequest, GenerateSignatureResponse, GetAstRequest, GetAstResponse,
    PrepareDependencyRequest, PrepareDependencyResponse,
};
use crate::analysis::GenerateSignature;
use crate::ast::GetAst;
use crate::models::Symbol;


#[derive(Default)]
pub struct TricorderServiceImpl {}

#[tonic::async_trait]
impl TricorderService for TricorderServiceImpl {
    async fn ensure_ready(
        &self,
        _request: Request<EnsureReadyRequest>,
    ) -> Result<Response<EnsureReadyResponse>, Status> {
        Ok(Response::new(EnsureReadyResponse::default()))
    }
    async fn get_ast(
        &self,
        request: Request<GetAstRequest>,
    ) -> Result<Response<GetAstResponse>, Status> {
		let request_data = request.into_inner();
		let file = request_data.file;
		let symbol: Symbol = request_data.symbol.unwrap().into();
        GetAst::get_ast(request).await
    }

    async fn generate_signature(
        &self,
        request: Request<GenerateSignatureRequest>,
    ) -> Result<Response<GenerateSignatureResponse>, Status> {
        GenerateSignature::generate_signature(request).await
    }

    async fn prepare_dependency(
        &self,
        _request: Request<PrepareDependencyRequest>,
    ) -> Result<Response<PrepareDependencyResponse>, Status> {
        Ok(Response::new(PrepareDependencyResponse::default()))
    }
}
