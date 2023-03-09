use tokio::fs;
use tricorder::TreeSplitter;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = "./tests/sources/t001_simple_fn_graph.rs";
    let source = fs::read_to_string(&file).await?;
    let ast = TreeSplitter::tree_split("main", &source);
    let formatted_ast = prettyplease::unparse(&ast);
    print!("{}", formatted_ast);
    Ok(())
}
