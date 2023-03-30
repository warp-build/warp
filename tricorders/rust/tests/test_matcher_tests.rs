use std::path::PathBuf;

use tricorder::analysis::tree_splitter::TreeSplitter;

#[test]
fn t001_find_matching_tests() {
    let source = TreeSplitter::expand_file(PathBuf::from("./tests/sources/t003_lib_with_tests.rs"));
    insta::assert_display_snapshot!(format!("{:#?}", &source));

    let test_name = vec!["add".to_string()];

    let matching_tests = TreeSplitter::find_matching_tests(test_name, &source);
    insta::assert_display_snapshot!(format!("{:#?}", &matching_tests));
}

#[test]
fn t002_find_all_tests() {
    let source = TreeSplitter::expand_file(PathBuf::from("./tests/sources/t003_lib_with_tests.rs"));

    let test_name = vec![];

    let matching_tests = TreeSplitter::find_matching_tests(test_name, &source);
    insta::assert_display_snapshot!(format!("{:#?}", &matching_tests));
}
