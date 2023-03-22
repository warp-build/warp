use crate::ast::*;
use crate::dependency::*;
use crate::models::Symbol;

pub struct TreeSplitter {}

impl TreeSplitter {
    pub fn get_deps_all(sources: &str) -> (Vec<String>, Vec<String>) {
        let ast = syn::parse_file(sources).unwrap();
        let mut mods: Vec<String> = Vec::new();
        let mut crates: Vec<String> = Vec::new();
        let mut visitor = AllDependency {
            mods: &mut mods,
            crates: &mut crates,
        };
        syn::visit::visit_file(&mut visitor, &ast);
        (mods, crates)
    }

    pub fn tree_split(symbol: Symbol, sources: &str) -> (syn::File, String) {
        match symbol {
            Symbol::Named { name } => {
                let ast = syn::parse_file(sources).unwrap();
                let symbol_ast = Self::get_ast_named(&name, ast.clone());
                let mut deps = Self::get_interested_symbols(symbol_ast, ast.clone());
                deps.push(name);
                let stripped_ast = Self::strip_ast(ast, deps);
                let formatted_ast = prettyplease::unparse(&stripped_ast);
                (stripped_ast, formatted_ast)
            }
            _ => panic!("Expected Symbol::Named, but got something else."),
        }
    }

    pub fn strip_ast(mut file_ast: syn::File, deps: Vec<String>) -> syn::File {
        let mut ast_filter_visitor = AstFilter { deps: &deps };
        syn::visit_mut::visit_file_mut(&mut ast_filter_visitor, &mut file_ast);
        file_ast
    }

    pub fn get_interested_symbols(ast: syn::File, full_ast: syn::File) -> Vec<String> {
        let mut symbols = Vec::new();
        let mut symbols_str = Vec::new();
        let mut visitor = SymbolDependency {
            symbols: &mut symbols,
            symbols_str: &mut symbols_str,
        };
        syn::visit::visit_file(&mut visitor, &ast);

        let mut symbols_temp = symbols_str.clone();
        let mut return_symbols = symbols_str.clone();
        while !symbols_temp.is_empty() {
            let mut symbols_rec = Vec::new();
            let mut symbols_str_rec = Vec::new();
            for dep in symbols_temp.iter() {
                let symbol_ast = Self::get_ast_named(dep, full_ast.clone());
                let mut visitor_temp = SymbolDependency {
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
                if item_fn.sig.ident == symbol {
                    return Some(syn::Item::Fn(item_fn));
                }
            }
            syn::Item::Const(item_const) => {
                if item_const.ident == symbol {
                    return Some(syn::Item::Const(item_const));
                }
            }
            syn::Item::Enum(item_enum) => {
                if item_enum.ident == symbol {
                    return Some(syn::Item::Enum(item_enum));
                }
            }
            syn::Item::Impl(item_impl) => {
                for impl_item in &item_impl.items {
                    match &impl_item {
                        syn::ImplItem::Const(const_impl_item) => {
                            if const_impl_item.ident == symbol {
                                return Some(item);
                            }
                        }
                        syn::ImplItem::Method(method_item) => {
                            if method_item.sig.ident == symbol {
                                return Some(item);
                            }
                        }
                        syn::ImplItem::Type(type_item) => {
                            if type_item.ident == symbol {
                                return Some(item);
                            }
                        }
                        _ => println!("Unsupported implementation item"),
                    }
                }
            }
            syn::Item::Macro2(item_macro2) => {
                if item_macro2.ident == symbol {
                    return Some(item);
                }
            }
            syn::Item::Mod(item_mod) => {
                if item_mod.ident == symbol {
                    return Some(item);
                }
                for mod_item in item_mod.content.unwrap_or_default().1 {
                    let rec_symbol = Self::get_symbol(symbol, mod_item.clone());
                    if let Some(x) = rec_symbol {
                        return Some(x);
                    }
                }
            }
            syn::Item::Static(item_static) => {
                if item_static.ident == symbol {
                    return Some(item);
                }
            }
            syn::Item::Struct(item_struct) => {
                if item_struct.ident == symbol {
                    return Some(item);
                }
            }
            syn::Item::Trait(item_trait) => {
                if item_trait.ident == symbol {
                    return Some(item);
                }
            }
            syn::Item::TraitAlias(item_trait_alias) => {
                if item_trait_alias.ident == symbol {
                    return Some(item);
                }
            }
            syn::Item::Type(item_type) => {
                if item_type.ident == symbol {
                    return Some(item);
                }
            }
            syn::Item::Union(item_union) => {
                if item_union.ident == symbol {
                    return Some(item);
                }
            }
            _ => println!("Unsupported item"),
        }
        None
    }

    // Mainly used for testing purposes
    pub fn get_ast(sources: &str) -> syn::File {
        syn::parse_file(sources).unwrap()
    }
}
