use tokio::fs;

#[tokio::main]
async fn main() {

    let file = "./tests/simple_add.rs";
    
    let source = fs::read_to_string(&file).await;
    
    let src = source.unwrap();
    let ast = syn::parse_file(&src).unwrap();

    println!("{:#?}", get_ast_named("internal_adder", ast));
    
}

fn get_ast_named(
    symbol: &str,
    ast: syn::File,
) -> syn::File {
    let subtree = get_symbol(&symbol, ast);
    syn::File{
	shebang: None,
	items: vec![subtree.unwrap()],
	attrs: vec![],
    }
}

fn get_symbol(
    symbol: &str,
    ast: syn::File,
) -> Option<syn::Item> {
    for item in ast.items {
	match &item {
	    syn::Item::Fn(itemfn) => {
		if itemfn.sig.ident.to_string() == symbol {
		    return Some(item);
		}
	    },
	    _ => println!("Unsupported item type"),
	}
    }
    None
}
