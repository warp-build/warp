fn main() {
    if std::env::var("WARP_BOOTSTRAP")
        .map(|val| val == "true")
        .unwrap_or_default()
    {
        return;
    }
    tonic_build::configure()
        .build_server(true)
        .include_file("_include.rs")
        .out_dir("src/grpc/protos/")
        .compile_well_known_types(true)
        .compile(
            &[
                "../../schemas/build/warp/common.proto",
                "../../schemas/build/warp/tricorder.proto",
            ],
            &["../../"],
        )
        .unwrap_or_else(|e| panic!("protobuf compile error: {}", e));
}
