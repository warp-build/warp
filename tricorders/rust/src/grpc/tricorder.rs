use super::mappers::ast_to_success_response;
use super::proto::build::warp::tricorder::tricorder_service_server::{
    TricorderService, TricorderServiceServer,
};
use super::proto::build::warp::tricorder::{
    generate_signature_response, EnsureReadyRequest, EnsureReadyResponse, GenerateSignatureRequest,
    GenerateSignatureResponse, GenerateSignatureSuccessResponse, GetAstRequest, GetAstResponse,
    PrepareDependencyRequest, PrepareDependencyResponse,
};
use super::GrpcTricorderError;
use crate::analysis::model::{Ast, Symbol};
use crate::analysis::Analyzer;
use crate::dependencies::DependencyManager;
use std::net::SocketAddr;
use std::path::Path;
use std::sync::Arc;
use tonic::transport::Server;
use tonic::{Request, Response, Status};

#[derive(Builder)]
#[builder(build_fn(error = "GrpcTricorderError"))]
pub struct GrpcTricorder {
    address: SocketAddr,
    dep_manager: Arc<DependencyManager>,
    analyzer: Arc<Analyzer>,
}

impl GrpcTricorder {
    pub fn builder() -> GrpcTricorderBuilder {
        Default::default()
    }

    pub async fn serve(self) -> Result<(), GrpcTricorderError> {
        let address = self.address;

        Server::builder()
            .add_service(TricorderServiceServer::new(self))
            .serve(address)
            .await?;

        Ok(())
    }
}

#[tonic::async_trait]
impl TricorderService for GrpcTricorder {
    async fn ensure_ready(
        &self,
        _request: Request<EnsureReadyRequest>,
    ) -> Result<Response<EnsureReadyResponse>, Status> {
        self.dep_manager.wait_until_ready().await;
        Ok(Response::new(EnsureReadyResponse::default()))
    }

    async fn generate_signature(
        &self,
        request: Request<GenerateSignatureRequest>,
    ) -> Result<Response<GenerateSignatureResponse>, Status> {
        let request_data = request.into_inner();
        let file = request_data.clone().file;
        let test_matcher: Vec<String> = match request_data.test_matcher {
            Some(matches) => matches.into(),
            None => Vec::new(),
        };
        let workspace_root = request_data.workspace_root;

        let response = self
            .analyzer
            .generate_signature(workspace_root.clone(), Path::new(&file), test_matcher)
            .await;

        match response {
            Ok(signature) => Ok(Response::new(GenerateSignatureResponse {
                response: Some(generate_signature_response::Response::Ok(
                    GenerateSignatureSuccessResponse {
                        workspace_root,
                        file,
                        signatures: signature.iter().map(|e| e.clone().into()).collect(),
                    },
                )),
            })),
            // TODO(@calin): create a proper way of dealing with missing dependencies.
            Err(_) => Ok(Response::new(GenerateSignatureResponse {
                response: Some(generate_signature_response::Response::Ok(
                    GenerateSignatureSuccessResponse {
                        workspace_root,
                        file,
                        signatures: vec![],
                    },
                )),
            })),
        }
    }

    async fn get_ast(
        &self,
        request: Request<GetAstRequest>,
    ) -> Result<Response<GetAstResponse>, Status> {
        let request = request.into_inner();

        let sym = match request.test_matcher {
            Some(test_matcher) if !test_matcher.raw.is_empty() => Symbol::Named {
                name: test_matcher.raw[0].to_string(),
            },
            _ => Symbol::All,
        };

        let response = self.analyzer.get_ast(request.file, sym).await;

        match response {
            Ok(ast) => Ok(Response::new(ast_to_success_response(ast))),
            // TODO(@calin): create a proper Requirement internal model for missing deps
            // and return it properly here.
            Err(_) => Ok(Response::new(ast_to_success_response(Ast::default()))),
        }
    }

    async fn prepare_dependency(
        &self,
        _request: Request<PrepareDependencyRequest>,
    ) -> Result<Response<PrepareDependencyResponse>, Status> {
        Ok(Response::new(PrepareDependencyResponse::default()))
    }
}
