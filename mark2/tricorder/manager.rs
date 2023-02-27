use crate::util::process_pool::ProcessPool;

use super::Tricorder;
use thiserror::*;

pub struct TricorderManager<T: Tricorder> {
  process_pool: ProcessPool<T>
}

impl<T: Tricorder> TricorderManager<T> {
    pub fn new() -> Self {
        Self {
            process_pool: ProcessPool::new()
        }
    }

    pub async fn find_and_ready(&self, concrete_target: ConcreteTarget) -> Result<T, TricorderManagerError> {

        // 1. 
    }
}

#[derive(Error, Debug)]
pub enum TricorderManagerError {}
