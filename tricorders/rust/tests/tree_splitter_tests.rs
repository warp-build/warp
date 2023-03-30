use std::path::PathBuf;
use tricorder::analysis::tree_splitter::TreeSplitter;

#[test]
fn t001_simple_fn_graph() {
    let source =
        TreeSplitter::expand_file(PathBuf::from("./tests/sources/t001_simple_fn_graph.rs"));

    let test_name = "main".to_string();

    let (ast, filtered_source) = TreeSplitter::tree_split(test_name, source);
    insta::assert_debug_snapshot!(ast);
    insta::assert_display_snapshot!(format!("{}", &filtered_source));
}

#[test]
fn t002_has_tests() {
    let source =
        TreeSplitter::expand_file(PathBuf::from("./tests/sources/t001_simple_fn_graph.rs"));
    let has_tests = TreeSplitter::has_tests(&source);
    insta::assert_debug_snapshot!(has_tests);

    let source = TreeSplitter::expand_file(PathBuf::from("./tests/sources/t003_lib_with_tests.rs"));
    let has_tests = TreeSplitter::has_tests(&source);
    insta::assert_debug_snapshot!(has_tests);
}

#[test]
fn t003_get_ast_on_tests_inside_of_mods() {
    let source = TreeSplitter::expand_file(PathBuf::from("./tests/sources/t003_lib_with_tests.rs"));

    let test_matcher = vec!["one".to_string()];
    let matching_tests = TreeSplitter::find_matching_tests(test_matcher, &source);

    let ast = syn::parse_file(&source).unwrap();
    let symbol_ast =
        TreeSplitter::get_ast_named(&matching_tests.first().unwrap().to_string(), ast.clone());
    let deps = TreeSplitter::get_interested_symbols(symbol_ast, ast);
    let (ast, formatted_ast) =
        TreeSplitter::tree_split(matching_tests.first().unwrap().to_string(), source);

    insta::assert_debug_snapshot!(deps);
    insta::assert_debug_snapshot!(ast);
    insta::assert_display_snapshot!(formatted_ast);
}

#[test]
fn t004_get_ast_on_tests_inside_of_mods() {
    let source = TreeSplitter::expand_file(PathBuf::from("./tests/sources/t003_lib_with_tests.rs"));

    let ast = syn::parse_file(&source).unwrap();
    let test_matcher = vec!["add_three_and_two".to_string()];
    let matching_tests = TreeSplitter::find_matching_tests(test_matcher, &source);
    let symbol_ast =
        TreeSplitter::get_ast_named(&matching_tests.first().unwrap().to_string(), ast.clone());
    let deps = TreeSplitter::get_interested_symbols(symbol_ast, ast);
    let (ast, formatted_ast) =
        TreeSplitter::tree_split(matching_tests.first().unwrap().to_string(), source);

    insta::assert_debug_snapshot!(deps);
    insta::assert_debug_snapshot!(ast);
    insta::assert_display_snapshot!(formatted_ast);
}
