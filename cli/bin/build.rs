fn main() -> Result<(), anyhow::Error> {
    tonic_build::compile_protos("../../schemas/build/warp/codedb/analyzer.proto")?;
    Ok(())
}
