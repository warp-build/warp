use syn::visit::Visit;
use syn::{ItemExternCrate, ItemMod};

pub struct AllDependency<'a> {
    pub mods: &'a mut Vec<String>,
    pub crates: &'a mut Vec<String>,
}

impl<'a> Visit<'a> for AllDependency<'a> {
    fn visit_item_extern_crate(&mut self, item: &ItemExternCrate) {
        self.crates.push(item.ident.to_string());
    }

    fn visit_item_mod(&mut self, item: &ItemMod) {
        if item.content.is_none() {
            self.mods.push(item.ident.to_string());
        }
    }
}
