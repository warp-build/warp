use crate::proto::build::warp::{Dependency, Symbol as ProtoSymbol, symbol::Sym::{All, Named}};
use crate::models::{Symbol, SymbolScope};

impl From<ProtoSymbol> for Symbol {
	fn from(sym: ProtoSymbol) -> Self {
		match sym.sym.unwrap() {
			All(_) =>
				Symbol::builder()
				.scope(SymbolScope::All),
			Named(name) =>
				Symbol::builder()
				.name(name)
				.scope(SymbolScope::All)
		}
	}
}
