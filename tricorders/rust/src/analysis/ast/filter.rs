use syn::visit_mut::VisitMut;
use syn::Item;

pub struct AstFilter<'a> {
    pub deps: &'a Vec<String>,
}

impl<'a> VisitMut for AstFilter<'a> {
    fn visit_item_mut(&mut self, item: &mut Item) {
        match item {
            Item::Fn(item_fn) => {
                if !self.deps.contains(&item_fn.sig.ident.to_string()) {
                    *item = Item::Verbatim(Default::default());
                }
            }
            Item::Const(item_const) => {
                if !self.deps.contains(&item_const.ident.to_string()) {
                    *item = Item::Verbatim(Default::default());
                }
            }
            Item::ExternCrate(item_extern_crate) => {
                if !self.deps.contains(&item_extern_crate.ident.to_string()) {
                    *item = Item::Verbatim(Default::default());
                }
            }
            Item::Mod(item_mod) => {
                if let Some(content) = item_mod.content.as_mut() {
                    for cont_item in &mut content.1 {
                        self.visit_item_mut(cont_item);
                    }
                }
            }
            _ => {}
        }
    }
}
