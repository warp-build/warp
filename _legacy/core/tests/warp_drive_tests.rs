use assert_fs::prelude::*;
use url::Url;
use warp_core::{Config, Goal, Target, WarpDriveMarkII, WARPFILE};

mod common;

#[tokio::test]
async fn can_be_created_in_a_workspace() {
    let warp_root = assert_fs::TempDir::new().unwrap();
    let curr_workspace = assert_fs::TempDir::new().unwrap();
    let warpfile = curr_workspace.child(WARPFILE);
    warpfile
        .write_str(
            r#"
        {
            "workspace": {
                "name": "test_from_subdir"
            }
        }
        "#,
        )
        .unwrap();

    let config = Config::builder()
        .warp_root(warp_root.path().to_path_buf())
        .invocation_dir(curr_workspace.path().to_path_buf())
        .build()
        .unwrap();

    WarpDriveMarkII::new(config).await.unwrap();
}

#[tokio::test]
async fn can_be_called_with_no_targets() {
    let warp_root = assert_fs::TempDir::new().unwrap();
    let curr_workspace = assert_fs::TempDir::new().unwrap();
    let warpfile = curr_workspace.child(WARPFILE);
    warpfile
        .write_str(
            r#"
        {
            "workspace": {
                "name": "test_from_subdir"
            }
        }
        "#,
        )
        .unwrap();

    let config = Config::builder()
        .warp_root(warp_root.path().to_path_buf())
        .invocation_dir(curr_workspace.path().to_path_buf())
        .build()
        .unwrap();

    let mut drive = WarpDriveMarkII::new(config).await.unwrap();

    let targets: Vec<String> = vec![];
    let results = drive.execute(Goal::Build, &targets).await.unwrap();

    assert_eq!(results.len(), 0);
}

#[tokio::test]
async fn executes_target() {
    let warp_root = assert_fs::TempDir::new().unwrap();
    // NOTE(@ostera): this line is useful for debugging the output directory when something goes wrong.
    // let warp_root = warp_root.into_persistent();
    dbg!(&warp_root.path());

    let curr_workspace = assert_fs::TempDir::new().unwrap();
    // NOTE(@ostera): this line is useful for debugging the output directory when something goes wrong.
    // let curr_workspace = curr_workspace.into_persistent();
    dbg!(&curr_workspace.path());

    let warpfile = curr_workspace.child(WARPFILE);
    warpfile
        .write_str(
            r#"
        {
            "workspace": {
                "name": "test_from_subdir"
            }
        }
        "#,
        )
        .unwrap();

    let file_target = curr_workspace.child("good_file.warp_test");
    file_target.write_str("dummy data").unwrap();

    let mut server = mockito::Server::new_async().await;
    let mock_url = server.url().parse::<Url>().unwrap();
    let config = Config::builder()
        .warp_root(warp_root.path().to_path_buf())
        .invocation_dir(curr_workspace.path().to_path_buf())
        .public_store_cdn_url(mock_url.clone())
        .public_store_metadata_url(mock_url.clone())
        .public_rule_store_url(mock_url)
        .build()
        .unwrap();

    // NOTE(@ostera): this mock will be used to not fetch the real tricorder
    let public_store_mock = server
        .mock("GET", "/a-hash.tar.gz")
        .with_status(200)
        .with_body(include_bytes!("./test_tricorder/package.tar.gz"))
        .create_async()
        .await;

    // NOTE(@ostera): this mock will be used to download the manifest
    let package_manifest_mock = server
        .mock("GET", "/tricorders/test/manifest.json")
        .with_status(200)
        .with_body(
            r#"
{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {
        "aarch64-apple-darwin": [ "a-hash" ],
        "x86_64-apple-darwin": [ "a-hash" ],
        "aarch64-unknown-linux-gnu": [ "a-hash" ],
        "x86_64-unknown-linux-gnu": [ "a-hash" ]
    }
}
                "#,
        )
        .create_async()
        .await;

    let rule_store_mock = server
        .mock("GET", "/test_rule.js")
        .with_status(200)
        .with_body(
            include_str!("./fixtures/rules/test_rule.js")
                .replace("{URL}", &server.url())
                .as_bytes(),
        )
        .create_async()
        .await;

    let mut drive = WarpDriveMarkII::new(config).await.unwrap();

    let target: Target = curr_workspace.path().join("good_file.warp_test").into();

    let results = drive
        .execute(Goal::Build, &[target])
        .await
        .unwrap()
        .get_results();

    assert!(!results.is_empty());

    public_store_mock.assert_async().await;
    package_manifest_mock.assert_async().await;
    rule_store_mock.assert_async().await;

    let task_result = results.get(0).unwrap();
    let hash = task_result.artifact_manifest.hash();

    assert!(warp_root
        .child(format!("store/{hash}/Manifest.json"))
        .exists());

    assert!(warp_root
        .child(format!("store/{hash}/good_file.warp_test"))
        .exists());
}

#[tokio::test]
async fn packs_target() {
    let warp_root = assert_fs::TempDir::new().unwrap();
    // NOTE(@ostera): this line is useful for debugging the output directory when something goes wrong.
    // let warp_root = warp_root.into_persistent();
    dbg!(&warp_root.path());

    let curr_workspace = assert_fs::TempDir::new().unwrap();
    // NOTE(@ostera): this line is useful for debugging the output directory when something goes wrong.
    // let curr_workspace = curr_workspace.into_persistent();
    dbg!(&curr_workspace.path());

    curr_workspace.child("store").create_dir_all().unwrap();

    let warpfile = curr_workspace.child(WARPFILE);
    warpfile
        .write_str(
            r#"
        {
            "workspace": {
                "name": "test_from_subdir"
            }
        }
        "#,
        )
        .unwrap();

    let file_target = curr_workspace.child("good_file.warp_test");
    file_target.write_str("dummy data").unwrap();

    let mut server = mockito::Server::new_async().await;
    let mock_url = server.url().parse::<Url>().unwrap();

    let config = Config::builder()
        .warp_root(warp_root.path().to_path_buf())
        .invocation_dir(curr_workspace.path().to_path_buf())
        .public_store_cdn_url(mock_url.clone())
        .public_store_metadata_url(mock_url.clone())
        .public_rule_store_url(mock_url)
        .build()
        .unwrap();

    // NOTE(@ostera): this mock will be used to not fetch the real tricorder
    let public_store_mock = server
        .mock("GET", "/a-hash.tar.gz")
        .with_status(200)
        .with_body(include_bytes!("./test_tricorder/package.tar.gz"))
        .create_async()
        .await;

    // NOTE(@ostera): this mock will be used to download the manifest
    let package_manifest_mock = server
        .mock("GET", "/tricorders/test/manifest.json")
        .with_status(200)
        .with_body(
            r#"
{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {
        "aarch64-apple-darwin": [ "a-hash" ],
        "x86_64-apple-darwin": [ "a-hash" ],
        "aarch64-unknown-linux-gnu": [ "a-hash" ],
        "x86_64-unknown-linux-gnu": [ "a-hash" ]
    }
}
                "#,
        )
        .create_async()
        .await;

    let rule_store_mock = server
        .mock("GET", "/test_rule.js")
        .with_status(200)
        .with_body(
            include_str!("./fixtures/rules/test_rule.js")
                .replace("{URL}", &server.url())
                .as_bytes(),
        )
        .create_async()
        .await;

    let mut drive = WarpDriveMarkII::new(config.clone()).await.unwrap();

    let target_path = curr_workspace.path().join("good_file.warp_test");
    let target: Target = target_path.clone().into();

    let results = drive
        .execute(Goal::Build, &[target.clone()])
        .await
        .unwrap()
        .get_results();

    assert!(!results.is_empty());

    public_store_mock.assert_async().await;
    package_manifest_mock.assert_async().await;
    rule_store_mock.assert_async().await;

    let manifest = results.get(0).unwrap().artifact_manifest.clone();
    let pack_results = drive.pack(target, /* upload */ false).await.unwrap();

    let pkgs = pack_results
        .manifest()
        .keys()
        .get(config.host_env().host_triple())
        .unwrap();

    assert_eq!(pkgs, &vec![manifest.hash().to_string()]);
    assert!(target_path.exists());
}
