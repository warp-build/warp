use syn::{visit::Visit, ItemExternCrate, ItemMod};

pub struct ModAndCrateVisitor<'a> {
    pub mods: &'a mut Vec<String>,
    pub crates: &'a mut Vec<String>,
}

impl<'a> Visit<'a> for ModAndCrateVisitor<'a> {
    fn visit_item_extern_crate(&mut self, item: &ItemExternCrate) {
        self.crates.push(item.ident.to_string());
    }

    fn visit_item_mod(&mut self, item: &ItemMod) {
        if item.content.is_none() {
            self.mods.push(item.ident.to_string());
        }
    }
}
