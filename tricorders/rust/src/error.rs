use crate::dependencies::DependencyManagerError;
use crate::grpc::GrpcTricorderError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TricorderError {
    #[error(transparent)]
    DependencyManagerError(DependencyManagerError),

    #[error(transparent)]
    GrpcTricorderError(GrpcTricorderError),
}

impl From<DependencyManagerError> for TricorderError {
    fn from(value: DependencyManagerError) -> Self {
        TricorderError::DependencyManagerError(value)
    }
}

impl From<GrpcTricorderError> for TricorderError {
    fn from(value: GrpcTricorderError) -> Self {
        TricorderError::GrpcTricorderError(value)
    }
}
