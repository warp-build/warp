use crate::proto::build::warp::symbol::Sym::*;
use crate::proto::build::warp::tricorder::generate_signature_response::Response as SigResponse;
use crate::proto::build::warp::Signature;
use crate::proto::build::warp::{tricorder::*, Symbol};
use crate::rs_generate_signature::RsGenerateSignature;
use std::path::{Path, PathBuf};
use thiserror::*;
use tonic::{Request, Response, Status};

#[derive(Default)]
pub struct GenerateSignature {}

#[derive(Error, Debug)]
pub enum GenerateSignatureError {
    #[error("Could not load file at {file:?} due to {err:?}")]
    CouldNotReadFile { file: String, err: std::io::Error },

	#[error("Missing dep {dep:?}")]
    MissingDependency { dep: String },

}

impl GenerateSignature {
    pub async fn generate_signature(
        request: Request<GenerateSignatureRequest>,
    ) -> Result<Response<GenerateSignatureResponse>, Status> {
        let request_data = request.into_inner();
        let filename = request_data.clone().file;
        println!("Analyzing: {:?}", filename.clone());

        match Path::new(&filename).extension() {
            Some(ext) if ext == "rs" => match request_data.clone().symbol.unwrap().sym.unwrap() {
                All(_) => Ok(Response::new(
                    GenerateSignature::do_gen_sig_all_rs(request_data).await,
                )),
                Named(_name) => Ok(Response::new(GenerateSignatureResponse::default())),
            },
            _ => Ok(Response::new(GenerateSignatureResponse::default())),
        }
    }

    async fn do_gen_sig_all_rs(request: GenerateSignatureRequest) -> GenerateSignatureResponse {
        let mut code_paths: Vec<PathBuf> = request
            .dependencies
            .iter()
            .map(|x| PathBuf::from(&x.store_path))
            .collect();
        code_paths.dedup_by(|a, b| a == b);

        let signatures = RsGenerateSignature::generate_all(&request.file, code_paths.clone()).await;

        let response = GenerateSignature::handle_result(request, code_paths, signatures).await;

        GenerateSignatureResponse {
            response: Some(response.unwrap()),
        }
    }

    async fn handle_result(
        request: GenerateSignatureRequest,
        _code_paths: Vec<PathBuf>,
        signatures: Vec<Signature>,
    ) -> Result<SigResponse, GenerateSignatureError> {
        Ok(SigResponse::Ok(GenerateSignatureSuccessResponse {
            workspace_root: request.workspace_root,
            file: request.file,
            symbol: Some(Symbol {
                sym: Some(All(true)),
            }),
            signatures,
        }))
    }
}
