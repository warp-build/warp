use tokio::fs;

#[tokio::main]
async fn main() {
    let file = "./tests/simple_add.rs";

    let source = fs::read_to_string(&file).await;

    let src = source.unwrap();
    let ast = syn::parse_file(&src).unwrap();

    println!("{:#?}", get_ast_named("internal_adder", ast));
}

fn get_ast_named(symbol: &str, file: syn::File) -> syn::File {
    let subtree = get_symbol(&symbol, file.clone());
    syn::File {
        shebang: file.shebang,
        items: vec![subtree.unwrap()],
        attrs: file.attrs,
    }
}

fn get_symbol(symbol: &str, file: syn::File) -> Option<syn::Item> {
    for item in file.items {
        match &item {
            syn::Item::Fn(item_fn) => {
                if item_fn.sig.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            syn::Item::Const(item_const) => {
                if item_const.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            syn::Item::Enum(item_enum) => {
                if item_enum.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            syn::Item::ExternCrate(item_extern_crate) => {
                if item_extern_crate.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            syn::Item::ForeignMod(item_foreign_mod) => {
                for foreign_item in &item_foreign_mod.items {
                    match &foreign_item {
                        syn::ForeignItem::Fn(fn_item) => {
                            if fn_item.sig.ident.to_string() == symbol {
                                return Some(item);
                            }
                        }
                        syn::ForeignItem::Static(static_item) => {
                            if static_item.ident.to_string() == symbol {
                                return Some(item);
                            }
                        }
                        syn::ForeignItem::Type(type_item) => {
                            if type_item.ident.to_string() == symbol {
                                return Some(item);
                            }
                        }
                        _ => println!("Unsupported foreign mod"),
                    }
                }
            }
            syn::Item::Impl(item_impl) => {
                for impl_item in &item_impl.items {
                    match &impl_item {
                        syn::ImplItem::Const(const_impl_item) => {
                            if const_impl_item.ident.to_string() == symbol {
                                return Some(item);
                            }
                        }
                        syn::ImplItem::Method(method_item) => {
                            if method_item.sig.ident.to_string() == symbol {
                                return Some(item);
                            }
                        }
                        syn::ImplItem::Type(type_item) => {
                            if type_item.ident.to_string() == symbol {
                                return Some(item);
                            }
                        }
                        _ => println!("Unsupported implementation item"),
                    }
                }
            }
            syn::Item::Macro2(item_macro2) => {
                if item_macro2.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            syn::Item::Mod(item_mod) => {
                if item_mod.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            syn::Item::Static(item_static) => {
                if item_static.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            syn::Item::Struct(item_struct) => {
                if item_struct.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            syn::Item::Trait(item_trait) => {
                if item_trait.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            syn::Item::TraitAlias(item_trait_alias) => {
                if item_trait_alias.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            syn::Item::Type(item_type) => {
                if item_type.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            syn::Item::Union(item_union) => {
                if item_union.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            _ => println!("Unsupported item"),
        }
    }
    None
}
