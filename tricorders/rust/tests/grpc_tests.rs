use std::path::PathBuf;
use tokio::time::timeout;

#[tokio::test]
async fn gets_ready_on_boot() {
    let root = PathBuf::from(".");
    let port = utils::PortFinder::next().unwrap();
    let addr = format!("0.0.0.0:{}", port);
    let sock = addr.parse().unwrap();

    let _server_handle = tokio::spawn(tricorder::Tricorder::new(sock, &root).unwrap().run());

    let mut client = utils::connect(&addr, 1000).await;

    timeout(std::time::Duration::from_millis(5000), async {
        let request = proto::build::warp::tricorder::EnsureReadyRequest::default();
        client.ensure_ready(request).await.unwrap().into_inner()
    })
    .await
    .unwrap();
}

/// Protobuf generated code.
mod proto {
    include!("../src/grpc/protos/_include.rs");
}

mod utils {
    use super::proto;
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

    pub async fn connect(
        addr: &str,
        max_retries: usize,
    ) -> proto::build::warp::tricorder::tricorder_service_client::TricorderServiceClient<
        tonic::transport::Channel,
    > {
        let mut retries = max_retries;
        loop {
            if retries == 0 {
                panic!(
                    "Could not connect to {} after {} retries",
                    addr, max_retries
                );
            }
            let conn =
                proto::build::warp::tricorder::tricorder_service_client::TricorderServiceClient::connect(
                    format!("http://{}", addr)
                )
                .await;
            if let Ok(conn) = conn {
                break conn;
            }
            retries -= 1;
            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        }
    }
}
