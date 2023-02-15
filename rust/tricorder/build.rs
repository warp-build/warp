use std::path::PathBuf;

fn main() {
    tonic_build::configure()
        .build_server(true)
        .include_file("_include.rs")
        .compile_well_known_types(true)
        .compile(
            &[
                "../../schemas/build/warp/codedb/analyzer.proto",
                "../../schemas/build/warp/resolver.proto",
                "../../schemas/build/warp/common.proto",
            ],
            &["../.."],
        )
        .unwrap_or_else(|e| panic!("protobuf compile error: {}", e));

    let dir: PathBuf = ["vendor/tree-sitter-rust", "src"].iter().collect();

    cc::Build::new()
        .include(&dir)
        .file(dir.join("parser.c"))
        .file(dir.join("scanner.c"))
        .warnings(false)
        .compile("tree-sitter-rust");
}
