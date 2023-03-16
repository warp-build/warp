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
        .out_dir("protos_gen/")
        .compile_well_known_types(true)
        .compile(
            &[
                "../protos/schemas/build/warp/common.proto",
                "../protos/schemas/build/warp/tricorder.proto",
            ],
            &["../protos/"],
        )
        .unwrap_or_else(|e| panic!("protobuf compile error: {}", e));
}
