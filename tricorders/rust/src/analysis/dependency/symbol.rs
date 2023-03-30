use std::collections::HashSet;

use syn::visit::{self, Visit};
use syn::*;

pub struct SymbolDependency<'a> {
    symbols: &'a mut HashSet<Ident>,
    local_symbols: HashSet<Ident>,
}

impl<'a> SymbolDependency<'a> {
    pub fn new(symbols: &'a mut HashSet<Ident>) -> Self {
        Self {
            symbols,
            local_symbols: HashSet::new(),
        }
    }

    fn visit_block(&mut self, block: &'a Block) {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn add_to_local_vars(&mut self, pat: &'a Pat) {
        match &pat {
            Pat::Ident(pat_ident) => {
                self.local_symbols.insert(pat_ident.ident.clone());
            }
            _ => {}
        }
    }
}

impl<'a> Visit<'a> for SymbolDependency<'a> {
    fn visit_item_fn(&mut self, item_fn: &'a ItemFn) {
        let local_symbols: HashSet<_> = item_fn
            .sig
            .inputs
            .iter()
            .flat_map(|arg| match arg {
                FnArg::Receiver(_) => None,
                FnArg::Typed(PatType { pat, .. }) => match &**pat {
                    Pat::Ident(ident) => Some(ident.ident.clone()),
                    _ => None,
                },
            })
            .collect();

        self.local_symbols.extend(local_symbols);
        self.visit_block(&item_fn.block);
    }

    fn visit_expr_path(&mut self, expr: &'a ExprPath) {
        if let Some(ident) = expr.path.get_ident() {
            if !self.local_symbols.contains(ident) {
                self.symbols.insert(ident.clone());
            }
        }
        visit::visit_expr_path(self, expr);
    }

    fn visit_expr_call(&mut self, expr: &'a ExprCall) {
        if let Expr::Path(ref path) = *expr.func {
            match path.path.get_ident() {
                Some(ident) => {
                    if !self.local_symbols.contains(ident) {
                        self.symbols.insert(ident.clone());
                    }
                }
                None => {
                    let fun_name = &path.path.segments.last().unwrap().ident;
                    if !self.local_symbols.contains(fun_name) {
                        self.symbols.insert(fun_name.clone());
                    }
                }
            }
        }
        visit::visit_expr_call(self, expr);
    }

    fn visit_local(&mut self, expr: &'a Local) {
        if let Pat::Ident(ref ident) = expr.pat {
            self.local_symbols.insert(ident.ident.clone());
            if let Some((_, ref subpat)) = ident.subpat {
                self.visit_pat(subpat);
            }
        }
        if let Some(init_expr) = &expr.init {
            dbg!(&init_expr.1);
            self.visit_expr(&init_expr.1)
        }
        visit::visit_local(self, expr);
    }

    fn visit_expr_match(&mut self, expr: &'a ExprMatch) {
        for arm in expr.arms.iter() {
            match &arm.pat {
                Pat::Tuple(pat_tuple) => {
                    for elem in &pat_tuple.elems {
                        self.add_to_local_vars(elem);
                    }
                }
                _ => {}
            }
            self.visit_expr(&arm.body);
        }
        visit::visit_expr_match(self, expr);
    }

    fn visit_expr_let(&mut self, expr: &'a ExprLet) {
        if let Pat::Ident(ref ident) = expr.pat {
            if ident.by_ref.is_some() || ident.mutability.is_some() {
                return;
            }
            if !self.local_symbols.contains(&ident.ident) {
                self.symbols.insert(ident.ident.clone());
            }
            if let Some((_, ref subpat)) = ident.subpat {
                self.visit_pat(subpat);
            }
        }
        self.visit_expr(&expr.expr);
    }

    fn visit_expr_closure(&mut self, expr: &'a ExprClosure) {
        for input in &expr.inputs {
            if let Pat::Ident(ref pat_ident) = input {
                if !self.local_symbols.contains(&pat_ident.ident) {
                    self.symbols.insert(pat_ident.ident.clone());
                }
            }
        }
        self.visit_expr(&expr.body);
    }

    fn visit_item_const(&mut self, item: &'a ItemConst) {
        self.visit_expr(&item.expr);
    }
}
