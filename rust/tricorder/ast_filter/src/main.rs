use tokio::fs;
use tricorder::TreeSplitter;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = "./tests/simple_add.rs";
    let source = fs::read_to_string(&file).await?;
    println!("{:#?}", TreeSplitter::tree_split("internal", &source));
    Ok(())
}

/*
fn meat() { false }

fn salad() { true }

fn potato() {
    salad()
}

Items = [
    FnItem("meat", ...),
    FnItem("salad", ...),
    FnItem("potato", [
        Return(Call("salad", []))
    ]
]

MarkedItems = [
    NotMatch(FnItem("meat", ...)),
    NotMatch(FnItem("salad", ...)),
    Match(FnItem("potato", [Return(Call("salad", []))],
        deps: ["sallad"])
]

AllDeps = "potato" ++ ["sallad"]

// for symbol "potato" (limited to functions):
// 1. iterate over items and _mark_ the stuff that matches "potato"
// 2. filter all the items and only keep the things that are match, or that are required by a match
// 3. profit
//

struct MarkedItem {
    IsMatch { item: Item, requires: Vec<String> },
    IsNotMatch(Item)
}
*/
