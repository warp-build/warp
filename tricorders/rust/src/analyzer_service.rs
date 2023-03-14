use crate::proto::build::warp::codedb::analyzer_service_server::AnalyzerService;
use crate::proto::build::warp::codedb::*;
use crate::proto::build::warp::Dependency;
use tonic::{Request, Response, Status};

#[derive(Default)]
pub struct AnalyzerServiceImpl {}

#[tonic::async_trait]
impl AnalyzerService for AnalyzerServiceImpl {
    async fn get_dependencies(
        &self,
        _request: Request<GetDependenciesRequest>,
    ) -> Result<Response<GetDependenciesResponse>, Status> {
        let response = GetDependenciesResponse {
            status: 1,
            dependencies: vec![Dependency {
                name: "test".to_string(),
                version: "test".to_string(),
                url: "test".to_string(),
                archive_resolver: "test".to_string(),
                signature_resolver: "test".to_string(),
                archive_subdir: "test".to_string(),
                store_path: "test".to_string(),
                tricorder_url: "https://warp.test/tricorder".to_string(),
            }],
        };
        Ok(Response::new(response))
    }
    async fn get_interested_paths(
        &self,
        _request: Request<GetInterestedPathsRequest>,
    ) -> Result<Response<GetInterestedPathsResponse>, Status> {
        let response = GetInterestedPathsResponse {
            build_files: vec!["file.rs".to_string()],
            test_files: vec!["test.rs".to_string()],
        };
        Ok(Response::new(response))
    }

    async fn get_provided_symbols(
        &self,
        _request: Request<GetProvidedSymbolsRequest>,
    ) -> Result<Response<GetProvidedSymbolsResponse>, Status> {
        let response = GetProvidedSymbolsResponse {
            skipped: true,
            file: "test.rs".to_string(),
            provides: vec![],
        };
        Ok(Response::new(response))
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
