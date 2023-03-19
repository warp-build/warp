mod mappers;
use crate::analysis::Analysis;
use crate::ast::GetAst;
use crate::models::{Ast, AstError, Signature, Symbol};
use crate::proto::build::warp::tricorder::GenerateSignatureSuccessResponse;
use crate::proto::build::warp::tricorder::{
    generate_signature_response::Response as SigResponse,
    tricorder_service_server::TricorderService, EnsureReadyRequest, EnsureReadyResponse,
    GenerateSignatureRequest, GenerateSignatureResponse, GetAstRequest, GetAstResponse,
    PrepareDependencyRequest, PrepareDependencyResponse,
};
use crate::AnalysisError;
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
        let request_data = request.into_inner();
        let file = request_data.clone().file;
        let symbol: Symbol = request_data.clone().symbol.unwrap().into();
        let workspace_root = request_data.clone().workspace_root;

        let response: Result<Vec<Signature>, AnalysisError> =
            Analysis::generate_signature(workspace_root.clone(), file.clone(), symbol.clone())
                .await;

        match response {
            Ok(signature) => Ok(Response::new(GenerateSignatureResponse {
                response: Some(SigResponse::Ok(GenerateSignatureSuccessResponse {
                    workspace_root,
                    file,
                    symbol: Some((&symbol).into()),
                    signatures: signature.iter().map(|e| e.clone().into()).collect(),
                })),
            })),
            // TODO(@calin): create a proper way of dealing with missing dependencies.
            Err(_) => Ok(Response::new(GenerateSignatureResponse {
                response: Some(SigResponse::Ok(GenerateSignatureSuccessResponse {
                    workspace_root,
                    file,
                    symbol: Some((&symbol).into()),
                    signatures: vec![],
                })),
            })),
        }
    }

    async fn get_ast(
        &self,
        request: Request<GetAstRequest>,
    ) -> Result<Response<GetAstResponse>, Status> {
        let request_data = request.into_inner();
        let file = request_data.clone().file;
        let symbol: Symbol = request_data.clone().symbol.unwrap().into();
        let response: Result<Ast, AstError> = GetAst::get_ast(file, symbol).await;

        match response {
            Ok(ast) => Ok(ast.into()),
            // TODO(@calin): create a proper Requirement internal model for missing deps
            // and return it properly here.
            Err(_) => Ok(Ast::default().into()),
        }
    }

    async fn prepare_dependency(
        &self,
        _request: Request<PrepareDependencyRequest>,
    ) -> Result<Response<PrepareDependencyResponse>, Status> {
        Ok(Response::new(PrepareDependencyResponse::default()))
    }
}
