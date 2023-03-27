use assert_fs::prelude::*;
use tricorder::{GenerateSignature, TreeSplitter};

#[tokio::test]
async fn t002_get_mods() {
    let curr_workspace = assert_fs::TempDir::new().unwrap();

    let sources = curr_workspace.child("./tests/sources/");
    sources.copy_from("./tests/sources/", &["*.rs"]).unwrap();
    let test_file = sources.child("t002_get_mods.rs");

    let src = include_str!("./sources/t002_get_mods.rs");
    let (mods, crates) = TreeSplitter::get_deps_all(src);
    let ast = TreeSplitter::get_ast(src);
    insta::assert_display_snapshot!(format!("{:#?}", &mods));
    insta::assert_display_snapshot!(format!("{:#?}", &crates));
    insta::assert_display_snapshot!(format!("{:#?}", &ast));

    let signatures = GenerateSignature::all(
        curr_workspace.path().to_string_lossy().to_string(),
        test_file
            .path()
            .strip_prefix(curr_workspace.path())
            .unwrap()
            .to_string_lossy()
            .to_string(),
    )
    .await;
    insta::assert_display_snapshot!(format!("{:#?}", &signatures));
}
