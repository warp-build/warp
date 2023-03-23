use syn::{
    visit::{self, Visit},
    File, ItemFn, ItemMod,
};

pub struct TestVisitor {
    pub has_test: bool,
}

impl<'ast> Visit<'ast> for TestVisitor {
    fn visit_item_fn(&mut self, item: &'ast ItemFn) {
        if item.attrs.iter().any(|attr| attr.path.is_ident("test")) {
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
