fn main() -> Result<(), anyhow::Error> {
    tonic_build::configure()
        .include_file("_include.rs")
        .compile_well_known_types(true)
        .compile(&["protos/build/warp/codedb/analyzer.proto"], &["protos"])?;
    Ok(())
}
