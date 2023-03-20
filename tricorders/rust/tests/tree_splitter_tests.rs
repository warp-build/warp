use tricorder::{Symbol, TreeSplitter};

#[test]
fn t001_simple_fn_graph() {
    let src = include_str!("./sources/t001_simple_fn_graph.rs");
    let symbol = Symbol::builder()
        .name("main".to_string())
        .scope(tricorder::SymbolScope::Named)
        .build()
        .unwrap();

    let (ast, filtered_source) = TreeSplitter::tree_split(symbol, src);
    insta::assert_debug_snapshot!(ast);
    insta::assert_display_snapshot!(format!("{}", &filtered_source));
}
