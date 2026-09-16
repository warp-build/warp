use syn::visit::{self, Visit};
use syn::{ItemConst, ItemFn, ItemMod};

pub struct TestVisitor {
    pub has_test: bool,
}

impl<'ast> Visit<'ast> for TestVisitor {
    fn visit_item_fn(&mut self, item: &'ast ItemFn) {
        if item.attrs.iter().any(|attr| {
            attr.path.is_ident("test") || attr.path.is_ident("rustc_test_marker")
        }) {
            self.has_test = true;
        } else if item
            .attrs
            .iter()
            .any(|attr| attr.path.is_ident("cfg") && attr.tokens.to_string().contains("test"))
        {
            self.has_test = true;
        }
        visit::visit_item_fn(self, item);
    }

    fn visit_item_const(&mut self, item: &'ast ItemConst) {
        if item
            .attrs
            .iter()
            .any(|attr| attr.path.is_ident("rustc_test_marker"))
        {
            self.has_test = true;
        }
        visit::visit_item_const(self, item);
    }

    fn visit_item_mod(&mut self, item: &'ast ItemMod) {
        if item
            .attrs
            .iter()
            .any(|attr| attr.path.is_ident("cfg") && attr.tokens.to_string().contains("test"))
        {
            self.has_test = true;
        } else {
            visit::visit_item_mod(self, item);
        }
    }
}

pub struct TestFinder {
    pub matcher: String,
    pub tests: Vec<String>,
}

impl<'a> Visit<'a> for TestFinder {
    fn visit_item_fn(&mut self, item: &'a syn::ItemFn) {
        if item.attrs.iter().any(|attr| {
            attr.path.is_ident("test") || attr.path.is_ident("rustc_test_marker")
        }) {
            if item.sig.ident.to_string().starts_with(&self.matcher) {
                self.tests.push(item.sig.ident.to_string());
            }
        } else if item
            .attrs
            .iter()
            .any(|attr| attr.path.is_ident("cfg") && attr.tokens.to_string().contains("test"))
            && item.sig.ident.to_string().starts_with(&self.matcher)
        {
            self.tests.push(item.sig.ident.to_string());
        }
        visit::visit_item_fn(self, item);
    }

    fn visit_item_const(&mut self, item: &'a ItemConst) {
        if item
            .attrs
            .iter()
            .any(|attr| attr.path.is_ident("rustc_test_marker"))
            && item.ident.to_string().starts_with(&self.matcher)
        {
            self.tests.push(item.ident.to_string());
        }
        visit::visit_item_const(self, item);
    }

    fn visit_item_mod(&mut self, item: &ItemMod) {
        // Expanded test modules no longer retain their `#[cfg(test)]` attribute;
        // traverse every inline module so marker constants remain discoverable.
        visit::visit_item_mod(self, item);
    }
}
