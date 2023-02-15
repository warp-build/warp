use crate::proto::build::warp::codedb::*;
use crate::proto::build::warp::Signature;
use crate::rs_generate_signature::*;
use std::path::{Path, PathBuf};
use thiserror::*;
use tonic::{Request, Response, Status};

#[derive(Default)]
pub struct GenerateSignature {}

#[derive(Error, Debug)]
pub enum GenerateSignatureError {}

impl GenerateSignature {
    pub async fn generate_signature(
        request: Request<GenerateSignatureRequest>,
    ) -> Result<Response<GenerateSignatureResponse>, Status> {
        let request_data = request.into_inner();
        let filename = request_data.clone().file;
        println!("Analyzing: {:?}", filename.clone());

        match Path::new(&filename).extension() {
            Some(ext) if ext == "rs" => Ok(Response::new(
                GenerateSignature::do_gen_sig_rs(request_data).await,
            )),
            _ => Ok(Response::new(GenerateSignatureResponse::default())),
        }
    }

    async fn do_gen_sig_rs(request: GenerateSignatureRequest) -> GenerateSignatureResponse {
        let mut code_paths: Vec<PathBuf> = request
            .dependencies
            .iter()
            .map(|x| PathBuf::from(&x.store_path))
            .collect();
        code_paths.dedup_by(|a, b| a == b);

        let result = RsGenerateSignature::generate(&request.file, code_paths.clone());

        let response = GenerateSignature::handle_result(request, code_paths, result).await;

        GenerateSignatureResponse {
            response: Some(response.unwrap()),
        }
    }

    async fn handle_result(
        request: GenerateSignatureRequest,
        _code_paths: Vec<PathBuf>,
        signatures: Vec<Signature>,
    ) -> Result<
        crate::proto::build::warp::codedb::generate_signature_response::Response,
        GenerateSignatureError,
    > {
        Ok(
            crate::proto::build::warp::codedb::generate_signature_response::Response::Ok(
                GenerateSignatureSuccessResponse {
                    status: 1,
                    file: request.file,
                    signatures: signatures,
                    json_signature: "".to_string(),
                },
            ),
        )
    }
}
