use log::info;
use tricorder::Tricorder;

#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
    env_logger::Builder::new()
        .filter_level(log::LevelFilter::Off)
        .format_timestamp_micros()
        .format_module_path(false)
        .parse_env("WARP_TRICORDER_LOG")
        .try_init()
        .unwrap();

    let port: i32 = std::env::args().nth(2).unwrap().parse().unwrap();

    let addr = format!("0.0.0.0:{}", port).parse().unwrap();

    let root = std::env::args().nth(3).unwrap();

    info!("Started gRPC server on 0.0.0.0:{} on {}", port, root);

    Tricorder::new(addr, &root)?.run().await?;

    Ok(())
}
