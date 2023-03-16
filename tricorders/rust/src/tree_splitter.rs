use super::*;
pub struct TreeSplitter {}

impl TreeSplitter {
    pub fn tree_split(symbol: &str, sources: &str) -> (syn::File, String) {
        let ast = syn::parse_file(sources).unwrap();
        let symbol_ast = Self::get_ast_named(symbol, ast.clone());
        let mut deps = Self::get_interested_symbols(symbol_ast.clone(), ast.clone());
        deps.push(symbol.to_string());
        let stripped_ast = Self::strip_ast(ast, deps);
        let formatted_ast = prettyplease::unparse(&stripped_ast);
        (stripped_ast, formatted_ast)
    }

    pub fn strip_ast(ast: syn::File, deps: Vec<String>) -> syn::File {
        let mut file_ast = ast.clone();
        let mut ast_filter_visitor = AstFilter { deps: &deps };
        syn::visit_mut::visit_file_mut(&mut ast_filter_visitor, &mut file_ast);
        file_ast
    }

    pub fn get_interested_symbols(ast: syn::File, full_ast: syn::File) -> Vec<String> {
        let mut symbols = Vec::new();
        let mut symbols_str = Vec::new();
        let mut visitor = InternalDependencyAccumulator {
            symbols: &mut symbols,
            symbols_str: &mut symbols_str,
        };
        syn::visit::visit_file(&mut visitor, &ast);

        let mut symbols_temp = symbols_str.clone();
        let mut return_symbols = symbols_str.clone();
        while symbols_temp.len() != 0 {
            let mut symbols_rec = Vec::new();
            let mut symbols_str_rec = Vec::new();
            for dep in symbols_temp.iter() {
                let symbol_ast = Self::get_ast_named(dep, full_ast.clone());
                let mut visitor_temp = InternalDependencyAccumulator {
                    symbols: &mut symbols_rec,
                    symbols_str: &mut symbols_str_rec,
                };
                syn::visit::visit_file(&mut visitor_temp, &symbol_ast);
            }
            symbols_temp = symbols_str_rec.clone();
            return_symbols.append(&mut symbols_str_rec);
        }
        return_symbols
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
                    return Some(syn::Item::Fn(item_fn));
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
