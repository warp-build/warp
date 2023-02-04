fn main() {

    tonic_build::configure()
        .build_server(true)
        .out_dir("./src")
        .compile(&[
	    "../../schemas/build/warp/codedb/analyzer.proto",
            "../../schemas/build/warp/resolver.proto",
            "../../schemas/build/warp/common.proto",
	], &["../.."])
        .unwrap_or_else(|e| panic!("protobuf compile error: {}", e));

}
