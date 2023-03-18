#[derive(Builder, Debug, Clone)]
pub struct Symbol {
	scope: SymbolScope,
	name: String,
}

impl Default for Symbol {
    fn default() -> Self {
        Self::builder().build().unwrap()
    }
}

impl Symbol {
	pub fn builder() -> ConfigBuilder {
        ConfigBuilder::default()
    }

	pub fn scope(&self) -> SymbolType {
		self.scope
	}
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SymbolScope {
	
	#[default]
	All,
	
	Named(String)
}
