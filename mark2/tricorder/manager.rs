use super::Tricorder;
use thiserror::*;

pub struct TricorderManager<T: Tricorder> {}

impl<T: Tricorder> TricorderManager<T> {
    pub fn new() -> Self {
        Self
    }

    pub async fn find_and_ready(&self) -> Result<T, TricorderManagerError> {}
}

#[derive(Error, Debug)]
pub enum TricorderManagerError {}
