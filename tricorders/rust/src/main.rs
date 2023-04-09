use tricorder::Tricorder;

#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
    let port: i32 = std::env::args().nth(2).unwrap().parse().unwrap();
    let addr = format!("0.0.0.0:{}", port).parse().unwrap();

    println!("Started gRPC server on 0.0.0.0:{}", port);

    Tricorder::new(addr)?.run().await?;

    Ok(())
}
