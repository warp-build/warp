use std::path::PathBuf;

use tree_sitter::Parser;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.is_empty() {
        println!(
            r#"

usage:

  parser FILE

        "#
        );
        return;
    }
    let path = PathBuf::from(args.get(1).unwrap());

    let source_code = std::fs::read_to_string(&path).unwrap();

    let mut parser = Parser::new();
    parser
        .set_language(match path.extension().unwrap().to_str().unwrap() {
            "hrl" | "erl" => tree_sitter_erlang::language(),
            "go" => tree_sitter_go::language(),
            "js" => tree_sitter_javascript::language(),
            "py" => tree_sitter_python::language(),
            "rs" => tree_sitter_rust::language(),
            "swift" => tree_sitter_swift::language(),
            "ts" => tree_sitter_typescript::language_typescript(),
            "jsx" | "tsx" => tree_sitter_typescript::language_tsx(),
            _ => panic!("unsupported language: {}", args.get(1).unwrap()),
        })
        .unwrap();
    let tree = parser.parse(source_code, None).unwrap();

    println!("{}", tree.root_node().to_sexp());
}
