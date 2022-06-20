/*
use std::path::PathBuf;
use zap_core::*;

#[tokio::test]
async fn it_reads_the_project_from_an_absolute_path() {
    let root = std::fs::canonicalize(PathBuf::from(&"./tests/sample_project")).unwrap();

    let workspace: Workspace =
        WorkspaceBuilder::build(root.clone(), None, None).unwrap();
    let mut zap = LocalWorker::from_workspace(workspace);
    zap.prepare(&Label::all()).await.unwrap();

    let workspace = zap.workspace;
    assert_eq!("sample_project", workspace.name);
    assert_eq!(root.to_str(), workspace.paths.workspace_root.to_str());
    assert_eq!(10, zap.targets.len());
}

#[tokio::test]
async fn it_reads_the_project_from_a_relative_path() {
    let root = PathBuf::from(&"./tests/sample_project");

    let workspace: Workspace =
        WorkspaceBuilder::build(root.clone(), None, None).unwrap();
    let mut zap = LocalWorker::from_workspace(workspace);
    zap.prepare(&Label::all()).await.unwrap();

    let workspace = zap.workspace;
    assert_eq!("sample_project", workspace.name);
    assert_eq!(std::fs::canonicalize(root).unwrap().to_str(), workspace.paths.workspace_root.to_str());
    assert_eq!(10, zap.targets.len());
}

#[tokio::test]
async fn build_dependency_graph_from_workspace() {
    let root = PathBuf::from(&"./tests/sample_project");

    let workspace: Workspace =
        WorkspaceBuilder::build(root.clone(), None, None).unwrap();
    let mut zap = LocalWorker::from_workspace(workspace);
    zap.prepare(&Label::all()).await.unwrap();

    let mut dep_graph = zap.dep_graph;

    let mut target_names_in_order = dep_graph.target_names();
    // NOTE(@ostera): we alphabetically to be able to assert deterministically
    target_names_in_order.sort();

    let root = PathBuf::from("//");
    let mut expected = vec![
        "//zap.build/test/toolchains:caramel".to_string(),
        "//zap.build/toolchains:erlang".to_string(),
        format!("{}:{}", root.join("a").to_str().unwrap(), "a_app"),
        format!("{}:{}", root.join("a").to_str().unwrap(), "lib"),
        format!("{}:{}", root.join("b").join("c").to_str().unwrap(), "lib"),
        format!(
            "{}:{}",
            root.join("b").join("c").to_str().unwrap(),
            "my_archive"
        ),
        format!("{}:{}", root.join("b").to_str().unwrap(), "b_app"),
        format!("{}:{}", root.join("b").to_str().unwrap(), "lib"),
        format!("{}:{}", root.join("b").to_str().unwrap(), "my_release"),
        format!("{}:{}", root.join("b").to_str().unwrap(), "rel"),
    ];
    expected.sort();

    assert_eq!(10, target_names_in_order.len());
    assert_eq!(
        format!("{:?}", expected),
        format!("{:?}", target_names_in_order)
    );
}

#[tokio::test]
async fn build_dependency_graph_from_workspace_and_scope() {
    let root = PathBuf::from(&"./tests/sample_project");

    let workspace: Workspace =
        WorkspaceBuilder::build(root.clone(), None, None).unwrap();
    let mut zap = LocalWorker::from_workspace(workspace);
    zap.prepare(&Label::new("//b/c:lib")).await.unwrap();

    let mut dep_graph = zap.dep_graph;

    let mut target_names_in_order = dep_graph.target_names();
    target_names_in_order.sort();

    let mut expected = vec![
        format!(
            "{}:{}",
            PathBuf::from("//").join("a").to_str().unwrap(),
            "lib"
        ),
        format!(
            "{}:{}",
            PathBuf::from("//").join("b").join("c").to_str().unwrap(),
            "lib"
        ),
        format!(
            "{}:{}",
            PathBuf::from("//").join("b").to_str().unwrap(),
            "lib"
        ),
        "//zap.build/toolchains:erlang".to_string(),
    ];
    expected.sort();

    assert_eq!(4, target_names_in_order.len());
    assert_eq!(
        format!("{:?}", expected),
        format!("{:?}", target_names_in_order)
    );
}
*/
