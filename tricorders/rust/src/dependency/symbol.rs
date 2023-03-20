use syn::visit::{self, Visit};
use syn::{spanned::Spanned, Ident, Stmt};

pub struct SymbolDependency<'a> {
    pub symbols: &'a mut Vec<Ident>,
    pub symbols_str: &'a mut Vec<String>,
}

impl<'a> Visit<'a> for SymbolDependency<'a> {
    fn visit_ident(&mut self, ident: &'a Ident) {
        if self.symbols.last().map_or(false, |i| i == "call") {
            self.symbols_str.push(ident.to_string());
        }
        visit::visit_ident(self, ident);
    }

    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                if let syn::Expr::Call(_) = expr {
                    self.symbols.push(Ident::new("call", expr.span()));
                }
            }
            Stmt::Semi(expr, _) => {
                if let syn::Expr::Call(_) = expr {
                    self.symbols.push(Ident::new("call", expr.span()));
                }
            }
            _ => {}
        }
        visit::visit_stmt(self, stmt);
    }
    fn visit_item_fn(&mut self, itemfn: &'a syn::ItemFn) {
        if self.symbols.last().map_or(false, |i| i == "call") {
            self.symbols.pop();
        }
        visit::visit_item_fn(self, itemfn)
    }
}
