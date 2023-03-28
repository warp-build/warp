use crate::models::{Ast, Config, Requirement, Signature, Symbol, Value};
use crate::proto::build::warp::symbol::Sym::{All, Named};
use crate::proto::build::warp::tricorder::{
    get_ast_response, GetAstResponse, GetAstSuccessResponse,
};
use crate::proto::build::warp::{
    DependencyRequirement, FileRequirement, SymbolRequirement, UrlRequirement,
};
use crate::proto::google::protobuf::value::Kind;
use crate::proto::google::protobuf::ListValue;
use std::collections::HashMap;

impl From<crate::proto::build::warp::TestMatcher> for Vec<String> {
    fn from(matcher: crate::proto::build::warp::TestMatcher) -> Self {
        matcher.raw
    }
}

impl From<crate::proto::build::warp::Symbol> for Symbol {
    fn from(sym: crate::proto::build::warp::Symbol) -> Self {
        match sym.sym.unwrap() {
            All(_) => Symbol::All,
            Named(name) => Symbol::Named { name },
        }
    }
}

impl From<&Symbol> for crate::proto::build::warp::Symbol {
    fn from(sym: &Symbol) -> Self {
        match sym {
            Symbol::All => crate::proto::build::warp::Symbol {
                sym: Some(All(true)),
            },
            Symbol::Named { name } => crate::proto::build::warp::Symbol {
                sym: Some(Named(name.to_string())),
            },
        }
    }
}

pub fn ast_to_success_response(ast: Ast) -> GetAstResponse {
    GetAstResponse {
        response: Some(get_ast_response::Response::Ok(GetAstSuccessResponse {
            file: ast.file().to_string(),
            source: ast.source().to_string(),
            ast: format!("{:#?}", ast.ast()),
        })),
    }
}

impl From<Signature> for crate::proto::build::warp::Signature {
    fn from(sig: Signature) -> Self {
        crate::proto::build::warp::Signature {
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

impl From<Requirement> for crate::proto::build::warp::Requirement {
    fn from(req: Requirement) -> Self {
        match req {
            Requirement::File { path } => crate::proto::build::warp::Requirement {
                requirement: Some(crate::proto::build::warp::requirement::Requirement::File(
                    FileRequirement {
                        path: path.to_str().unwrap().to_string(),
                    },
                )),
            },
            Requirement::Symbol { raw, kind } => crate::proto::build::warp::Requirement {
                requirement: Some(crate::proto::build::warp::requirement::Requirement::Symbol(
                    SymbolRequirement { raw, kind },
                )),
            },
            Requirement::Url {
                url,
                tricorder_url: _,
                subpath,
            } => crate::proto::build::warp::Requirement {
                requirement: Some(crate::proto::build::warp::requirement::Requirement::Url(
                    UrlRequirement {
                        url: url.to_string(),
                        subpath: subpath.unwrap().to_str().unwrap().to_string(),
                    },
                )),
            },
            Requirement::Dependency {
                name,
                version,
                url,
                tricorder,
            } => crate::proto::build::warp::Requirement {
                requirement: Some(
                    crate::proto::build::warp::requirement::Requirement::Dependency(
                        DependencyRequirement {
                            name: name,
                            version: version,
                            url: url.to_string(),
                            tricorder_url: tricorder.to_string(),
                            archive_resolver: "".to_string(),
                            signature_resolver: "".to_string(),
                        },
                    ),
                ),
            },
        }
    }
}

impl From<&Config> for crate::proto::google::protobuf::Struct {
    fn from(config: &Config) -> Self {
        let mut configs = HashMap::new();
        for (key, value) in config.values() {
            configs.insert(key.to_string(), value.clone().into());
        }

        crate::proto::google::protobuf::Struct { fields: configs }
    }
}

impl From<Value> for crate::proto::google::protobuf::Value {
    fn from(val: Value) -> Self {
        match val {
            Value::String(string) => crate::proto::google::protobuf::Value {
                kind: Some(Kind::StringValue(string)),
            },
            Value::Target(target) => crate::proto::google::protobuf::Value {
                kind: Some(Kind::StringValue(target.to_string())),
            },
            Value::File(file) => crate::proto::google::protobuf::Value {
                kind: Some(Kind::StringValue(file.to_str().unwrap().to_string())),
            },
            Value::List(parts) => crate::proto::google::protobuf::Value {
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
