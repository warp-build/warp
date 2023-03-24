fn main() {
    tonic_build::configure()
        .build_server(true)
        .include_file("_include.rs")
        .compile_well_known_types(true)
        .compile(
            &[
                "../../schemas/build/warp/tricorder.proto",
                "../../schemas/build/warp/common.proto",
            ],
            &["../.."],
        )
        .unwrap_or_else(|e| panic!("protobuf compile error: {}", e));
}
