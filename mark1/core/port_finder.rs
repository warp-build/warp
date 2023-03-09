use std::net::TcpListener;

use thiserror::*;

pub struct PortFinder;

#[derive(Error, Debug)]
pub enum PortError {
    #[error("Could not find an open port :(")]
    CouldNotFindOpenPort,
}

impl PortFinder {
    pub fn next() -> Result<u16, PortError> {
        let mut port: u16 = 1024;
        loop {
            if TcpListener::bind(("0.0.0.0", port)).is_ok() {
                return Ok(port);
            }
            if port < 65534 {
                port += 1;
                continue;
            }
            break;
        }
        Err(PortError::CouldNotFindOpenPort)
    }
}
