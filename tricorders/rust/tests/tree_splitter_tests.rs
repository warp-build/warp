use tricorder::analysis::model::Symbol;
use tricorder::analysis::tree_splitter::TreeSplitter;

#[test]
fn t001_simple_fn_graph() {
    let src = include_str!("./sources/t001_simple_fn_graph.rs");
    let symbol = Symbol::Named {
        name: "main".to_string(),
    };

    let (ast, filtered_source) = TreeSplitter::tree_split(symbol, src);
    insta::assert_debug_snapshot!(ast);
    insta::assert_display_snapshot!(format!("{}", &filtered_source));
}

#[test]
fn t002_has_tests() {
    let src = include_str!("./sources/t001_simple_fn_graph.rs");
    let has_tests = TreeSplitter::has_tests(src);
    insta::assert_debug_snapshot!(has_tests);

    let src = include_str!("./sources/t003_lib_with_tests.rs");
    let has_tests = TreeSplitter::has_tests(src);
    insta::assert_debug_snapshot!(has_tests);
}
