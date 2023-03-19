use std::collections::HashMap;

use crate::models::{Ast, Config, Requirement, Signature, Symbol, SymbolScope, Value};
use crate::proto::build::warp::{
    requirement::Requirement as ProtoSpecificRequirement,
    symbol::Sym::{All, Named},
    tricorder::{get_ast_response::Response as AstResponse, GetAstResponse, GetAstSuccessResponse},
    DependencyRequirement, FileRequirement, Requirement as ProtoRequirement, Signature as ProtoSig,
    Symbol as ProtoSymbol, SymbolRequirement, UrlRequirement,
};
use crate::proto::google::protobuf::{
    value::Kind, ListValue, Struct as ProtoStruct, Value as ProtoValue,
};
use tonic::Response;

impl From<ProtoSymbol> for Symbol {
    fn from(sym: ProtoSymbol) -> Self {
        match sym.sym.unwrap() {
            All(_) => Symbol::builder().build().unwrap(),
            Named(name) => Symbol::builder()
                .name(name)
                .scope(SymbolScope::All)
                .build()
                .unwrap(),
        }
    }
}

impl From<&Symbol> for ProtoSymbol {
    fn from(sym: &Symbol) -> Self {
        match sym.scope() {
            SymbolScope::All => ProtoSymbol {
                sym: Some(All(true)),
            },
            SymbolScope::Named => ProtoSymbol {
                sym: Some(Named(sym.name())),
            },
        }
    }
}

impl From<Ast> for Response<GetAstResponse> {
    fn from(ast: Ast) -> Self {
        Response::new(GetAstResponse {
            response: Some(AstResponse::Ok(GetAstSuccessResponse {
                file: ast.file().to_string(),
                symbol: Some(ast.symbol().into()),
                source: ast.source().to_string(),
                ast: format!("{:#?}", ast.ast()),
            })),
        })
    }
}

impl From<Signature> for ProtoSig {
    fn from(sig: Signature) -> Self {
        ProtoSig {
            name: sig.target().to_string(),
            rule: sig.rule().to_string(),
            deps: sig.deps().iter().map(|e| e.clone().into()).collect(),
            runtime_deps: sig
                .runtime_deps()
                .iter()
                .map(|e| e.clone().into())
                .collect(),
            config: Some(sig.config().into()),
        }
    }
}

impl From<Requirement> for ProtoRequirement {
    fn from(req: Requirement) -> Self {
        match req {
            Requirement::File { path } => ProtoRequirement {
                requirement: Some(ProtoSpecificRequirement::File(FileRequirement {
                    path: path.to_str().unwrap().to_string(),
                })),
            },
            Requirement::Symbol { raw, kind } => ProtoRequirement {
                requirement: Some(ProtoSpecificRequirement::Symbol(SymbolRequirement {
                    raw,
                    kind,
                })),
            },
            Requirement::Url {
                url,
                tricorder_url,
                subpath,
            } => ProtoRequirement {
                requirement: Some(ProtoSpecificRequirement::Url(UrlRequirement {
                    url: url.to_string(),
                    subpath: subpath.unwrap().to_str().unwrap().to_string(),
                })),
            },
            Requirement::Dependency {
                name,
                version,
                url,
                tricorder,
            } => ProtoRequirement {
                requirement: Some(ProtoSpecificRequirement::Dependency(
                    DependencyRequirement {
                        name: name,
                        version: version,
                        url: url.to_string(),
                        tricorder_url: tricorder.to_string(),
                        archive_resolver: "".to_string(),
                        signature_resolver: "".to_string(),
                    },
                )),
            },
        }
    }
}

impl From<&Config> for ProtoStruct {
    fn from(config: &Config) -> Self {
        let mut configs = HashMap::new();
        for (key, value) in config.values() {
            configs.insert(key.to_string(), value.clone().into());
        }

        ProtoStruct { fields: configs }
    }
}

impl From<Value> for ProtoValue {
    fn from(val: Value) -> Self {
        match val {
            Value::String(string) => ProtoValue {
                kind: Some(Kind::StringValue(string)),
            },
            Value::Target(target) => ProtoValue {
                kind: Some(Kind::StringValue(target.to_string())),
            },
            Value::File(file) => ProtoValue {
                kind: Some(Kind::StringValue(file.to_str().unwrap().to_string())),
            },
            Value::List(parts) => ProtoValue {
                kind: Some(Kind::ListValue(ListValue {
                    values: parts.iter().map(|e| e.clone().into()).collect(),
                })),
            },
        }
    }
}

impl From<String> for Value {
    fn from(val: String) -> Self {
        Value::String(val)
    }
}
