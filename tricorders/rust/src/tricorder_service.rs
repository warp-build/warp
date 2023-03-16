use crate::generate_signature::GenerateSignature;
use crate::get_ast::GetAst;
use crate::proto::build::warp::tricorder::PrepareDependencyResponse;
use crate::proto::build::warp::tricorder::{
    tricorder_service_server::TricorderService, EnsureReadyRequest, EnsureReadyResponse,
    GenerateSignatureRequest, GenerateSignatureResponse, GetAstRequest, GetAstResponse,
    PrepareDependencyRequest,
};
use tonic::{Request, Response, Status};

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

    async fn generate_signature(
        &self,
        request: Request<GenerateSignatureRequest>,
    ) -> Result<Response<GenerateSignatureResponse>, Status> {
        GenerateSignature::generate_signature(request).await
    }

    async fn get_ast(
        &self,
        request: Request<GetAstRequest>,
    ) -> Result<Response<GetAstResponse>, Status> {
        GetAst::get_ast(request).await
    }

    async fn prepare_dependency(
        &self,
        _request: Request<PrepareDependencyRequest>,
    ) -> Result<Response<PrepareDependencyResponse>, Status> {
        unimplemented!()
    }
}
