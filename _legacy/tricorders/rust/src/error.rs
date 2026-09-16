use crate::dependencies::DependencyManagerError;
use crate::grpc::GrpcTricorderError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TricorderError {
    #[error(transparent)]
    DependencyManagerError(DependencyManagerError),

    #[error(transparent)]
    GrpcTricorderError(GrpcTricorderError),

    #[error(transparent)]
    Io(std::io::Error),
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

impl From<std::io::Error> for TricorderError {
    fn from(value: std::io::Error) -> Self {
        TricorderError::Io(value)
    }
}
