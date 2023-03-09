use tricorder::TreeSplitter;

#[test]
fn t001_simple_fn_graph() {
    let src = include_str!("./sources/t001_simple_fn_graph.rs");
    let symbol = "main";
    let (ast, filtered_source) = TreeSplitter::tree_split(symbol, src);
    insta::assert_debug_snapshot!(ast);
    insta::assert_display_snapshot!(format!("{}", &filtered_source));
}
