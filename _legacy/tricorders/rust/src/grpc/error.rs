use derive_builder::UninitializedFieldError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GrpcTricorderError {
    #[error(transparent)]
    GrpcServerError(tonic::transport::Error),

    #[error(transparent)]
    BuilderError(UninitializedFieldError),
}

impl From<tonic::transport::Error> for GrpcTricorderError {
    fn from(value: tonic::transport::Error) -> Self {
        GrpcTricorderError::GrpcServerError(value)
    }
}

impl From<UninitializedFieldError> for GrpcTricorderError {
    fn from(value: UninitializedFieldError) -> Self {
        GrpcTricorderError::BuilderError(value)
    }
}
