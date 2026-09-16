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
use crate::analysis::model::Goal;
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
        let file = request_data.file.clone();
        let test_matcher: Vec<String> = match &request_data.test_matcher {
            Some(matches) => matches.clone().into(),
            None => Vec::new(),
        };
        let workspace_root = request_data.workspace_root.clone();
        let goal = super::proto::build::warp::Goal::from_i32(request_data.goal)
            .unwrap()
            .into();

        let response = self
            .analyzer
            .generate_signature(
                goal,
                Path::new(&workspace_root),
                Path::new(&file),
                test_matcher,
            )
            .await;

        match response {
            Ok(signatures) => Ok(Response::new(GenerateSignatureResponse {
                response: Some(generate_signature_response::Response::Ok(
                    GenerateSignatureSuccessResponse {
                        workspace_root,
                        file,
                        signatures: signatures.iter().map(|e| e.clone().into()).collect(),
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
        let workspace_root = request.workspace_root.clone();
        let file = request.file.clone();

        let test_matcher: Vec<String> = if let Some(test_matcher) = request.test_matcher {
            test_matcher.raw
        } else {
            vec![]
        };

        let response = self
            .analyzer
            .get_ast(Path::new(&workspace_root), Path::new(&file), test_matcher)
            .await;

        match response {
            Ok(asts) => Ok(Response::new(ast_to_success_response(workspace_root, asts))),
            // TODO(@calin): create a proper Requirement internal model for missing deps
            // and return it properly here.
            Err(_) => Ok(Response::new(ast_to_success_response(
                workspace_root,
                vec![],
            ))),
        }
    }

    async fn prepare_dependency(
        &self,
        request: Request<PrepareDependencyRequest>,
    ) -> Result<Response<PrepareDependencyResponse>, Status> {
        let request = request.into_inner();
        let workspace_root = Path::new(&request.package_root);

        let cargo_toml = workspace_root.join("Cargo.toml").canonicalize().unwrap();

        let signatures = self
            .analyzer
            .generate_signature(Goal::Build, workspace_root, &cargo_toml, vec![])
            .await
            .unwrap()
            .into_iter()
            .map(|sig| sig.into())
            .collect();

        let response = PrepareDependencyResponse { signatures };

        Ok(Response::new(response))
    }
}
