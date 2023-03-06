pub struct TreeSplitter;

impl TreeSplitter {
    pub fn tree_split(symbol: &str, sources: &str) -> syn::File {
        let ast = syn::parse_file(sources).unwrap();
        Self::get_ast_named(symbol, ast)
    }

    pub fn get_ast_named(symbol: &str, file: syn::File) -> syn::File {
        let subtree_option: Vec<syn::Item> = file
            .items
            .iter()
            .filter_map(|item| {
                let found_symbol = Self::get_symbol(symbol, item.clone());
                match &found_symbol {
                    Some(_) => found_symbol,
                    None => None,
                }
            })
            .collect();

        match subtree_option.first() {
            Some(x) => syn::File {
                shebang: file.shebang,
                items: vec![x.clone()],
                attrs: file.attrs,
            },
            None => syn::File {
                shebang: file.shebang,
                items: vec![],
                attrs: file.attrs,
            },
        }
    }

    pub fn get_symbol(symbol: &str, item: syn::Item) -> Option<syn::Item> {
        match item.clone() {
            syn::Item::Fn(item_fn) => {
                if item_fn.sig.ident.to_string() == symbol {
                    return Some(item);
                }
            }
            syn::Item::Const(item_const) => {
                if item_const.ident.to_string() == symbol {
                    return Some(syn::Item::Const(item_const));
                }
            }
            syn::Item::Enum(item_enum) => {
                if item_enum.ident.to_string() == symbol {
                    return Some(syn::Item::Enum(item_enum));
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
                for mod_item in item_mod.content.unwrap_or_default().1 {
                    let rec_symbol = Self::get_symbol(symbol, mod_item.clone());
                    match rec_symbol {
                        Some(x) => return Some(x),
                        None => {}
                    }
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
        None
    }
}
