use std::collections::HashSet;
use std::path::PathBuf;
use std::process::Command;

use crate::analysis::ast::*;
use crate::analysis::dependency::*;
pub struct TreeSplitter {}

impl TreeSplitter {
    pub fn expand_file(file_path: PathBuf) -> String {
        let cmd_out = Command::new("rustc")
            .arg("-Z")
            .arg("unpretty=expanded")
            .arg("--test")
            .arg(file_path)
            .output()
            .unwrap();

        String::from_utf8(cmd_out.stdout).unwrap()
    }

    pub fn parse_file(file_path: PathBuf) -> String {
        let cmd_out = Command::new("rustc")
            .arg("-Z")
            .arg("unpretty=normal")
            .arg("--test")
            .arg(file_path)
            .output()
            .unwrap();

        String::from_utf8(cmd_out.stdout).unwrap()
    }

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

    pub fn tree_split(test_name: String, sources: String) -> (syn::File, String) {
        let ast = syn::parse_file(&sources).unwrap();
        let symbol_ast = Self::get_ast_named(&test_name, ast.clone());
        let mut deps = Self::get_interested_symbols(symbol_ast, ast.clone());
        deps.push(test_name);
        let stripped_ast = Self::strip_ast(ast, deps);
        let formatted_ast = prettyplease::unparse(&stripped_ast);
        (stripped_ast, formatted_ast)
    }

    pub fn strip_ast(mut file_ast: syn::File, deps: Vec<String>) -> syn::File {
        let mut ast_filter_visitor = AstFilter { deps: &deps };
        syn::visit_mut::visit_file_mut(&mut ast_filter_visitor, &mut file_ast);
        file_ast
    }

    pub fn has_tests(src: &str) -> bool {
        let ast = syn::parse_file(src).unwrap();
        let mut visitor = TestVisitor { has_test: false };
        syn::visit::visit_file(&mut visitor, &ast);
        visitor.has_test
    }

    pub fn find_matching_tests(test_matcher: Vec<String>, sources: &str) -> Vec<String> {
        let ast = syn::parse_file(sources).unwrap();
        let mut matching_tests: Vec<String> = Vec::new();
        if test_matcher.is_empty() {
            let mut visitor = TestFinder {
                matcher: "".to_string(),
                tests: vec![],
            };
            syn::visit::visit_file(&mut visitor, &ast);
            matching_tests.extend(visitor.tests);
        } else {
            for matcher in test_matcher.into_iter() {
                let mut visitor = TestFinder {
                    matcher,
                    tests: vec![],
                };
                syn::visit::visit_file(&mut visitor, &ast);
                matching_tests.extend(visitor.tests);
            }
        }
        matching_tests
    }

    pub fn get_interested_symbols(ast: syn::File, full_ast: syn::File) -> Vec<String> {
        let mut symbols = HashSet::new();
        let mut visitor = SymbolDependency::new(&mut symbols);
        syn::visit::visit_file(&mut visitor, &ast);

        let mut symbols_temp = symbols.clone();
        while !symbols_temp.is_empty() {
            let mut symbols_rec = HashSet::new();
            for dep in symbols_temp.iter() {
                let symbol_ast = Self::get_ast_named(dep.to_string().as_ref(), full_ast.clone());
                let mut visitor_temp = SymbolDependency::new(&mut symbols_rec);
                syn::visit::visit_file(&mut visitor_temp, &symbol_ast);
            }
            symbols_temp = symbols_rec.clone();
            symbols.extend(symbols_rec);
        }
        let mut res: Vec<String> = symbols.iter().map(|sym| sym.to_string()).collect();
        res.sort();
        res
    }

    // Returns an AST with only the needed function as the only element in the items
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
                        _ => {}
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
            _ => {}
        }
        None
    }

    // Mainly used for testing purposes
    pub fn get_ast(sources: &str) -> syn::File {
        syn::parse_file(sources).unwrap()
    }
}
