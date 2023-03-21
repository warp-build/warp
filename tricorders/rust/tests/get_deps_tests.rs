use tricorder::{GenerateSignature, TreeSplitter};

#[tokio::test]
async fn t002_get_mods() {
    let src = include_str!("./sources/t002_get_mods.rs");
    let file_path = "./tests/sources/t002_get_mods.rs";
    let workspace_root = "/Users/capitanu/repos/github.com/warp-build/warp/tricorders/rust/";
    let (mods, crates) = TreeSplitter::get_deps_all(src);
    let ast = TreeSplitter::get_ast(src);
    insta::assert_display_snapshot!(format!("{:#?}", &mods));
    insta::assert_display_snapshot!(format!("{:#?}", &crates));
    insta::assert_display_snapshot!(format!("{:#?}", &ast));

    let signatures =
        GenerateSignature::all(workspace_root.to_string(), file_path.to_string()).await;
    insta::assert_display_snapshot!(format!("{:#?}", &signatures));
}
