use syn::visit_mut::VisitMut;
use syn::Item;

pub struct AstFilter<'a> {
    pub deps: &'a Vec<String>,
}

impl<'a> VisitMut for AstFilter<'a> {
    fn visit_item_mut(&mut self, item: &mut Item) {
        if let Item::Fn(item_fn) = item {
            if !self.deps.contains(&item_fn.sig.ident.to_string()) {
                *item = Item::Verbatim(Default::default());
            }
        }
    }
}
