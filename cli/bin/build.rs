fn main() -> Result<(), anyhow::Error> {
    tonic_build::configure()
        .include_file("_include.rs")
        .compile_well_known_types(true)
        .compile(
            &[
                "../../schemas/build/warp/codedb/analyzer.proto",
                "../../schemas/build/warp/resolver.proto",
                "../../schemas/build/warp/common.proto",
            ],
            &["../../"],
        )?;
    Ok(())
}
