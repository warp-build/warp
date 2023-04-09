use super::proto;
use super::proto::build::warp::symbol::Sym::{All, Named};
use super::proto::build::warp::tricorder::{
    get_ast_response, GetAstResponse, GetAstSuccessResponse,
};
use super::proto::build::warp::{
    DependencyRequirement, FileRequirement, SymbolRequirement, UrlRequirement,
};
use super::proto::google::protobuf::value::Kind;
use super::proto::google::protobuf::ListValue;
use crate::analysis::model::{Ast, Config, Requirement, Signature, Symbol, Value};
use std::collections::HashMap;

impl From<proto::build::warp::TestMatcher> for Vec<String> {
    fn from(matcher: proto::build::warp::TestMatcher) -> Self {
        matcher.raw
    }
}

impl From<proto::build::warp::Symbol> for Symbol {
    fn from(sym: proto::build::warp::Symbol) -> Self {
        match sym.sym.unwrap() {
            All(_) => Symbol::All,
            Named(name) => Symbol::Named { name },
        }
    }
}

impl From<&Symbol> for proto::build::warp::Symbol {
    fn from(sym: &Symbol) -> Self {
        match sym {
            Symbol::All => proto::build::warp::Symbol {
                sym: Some(All(true)),
            },
            Symbol::Named { name } => proto::build::warp::Symbol {
                sym: Some(Named(name.to_string())),
            },
        }
    }
}

pub fn ast_to_success_response(ast: Ast) -> GetAstResponse {
    GetAstResponse {
        response: Some(get_ast_response::Response::Ok(GetAstSuccessResponse {
            subtrees: vec![proto::build::warp::tricorder::AstSubtree {
                workspace_root: "".to_string(),
                file: ast.file().to_string(),
                source_chunk: ast.source().to_string(),
                ast: format!("{:#?}", ast.ast()),
                signature_name: "".to_string(),
            }],
        })),
    }
}

impl From<Signature> for proto::build::warp::Signature {
    fn from(sig: Signature) -> Self {
        proto::build::warp::Signature {
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

impl From<Requirement> for proto::build::warp::Requirement {
    fn from(req: Requirement) -> Self {
        match req {
            Requirement::File { path } => proto::build::warp::Requirement {
                requirement: Some(proto::build::warp::requirement::Requirement::File(
                    FileRequirement {
                        path: path.to_str().unwrap().to_string(),
                    },
                )),
            },
            Requirement::Symbol { raw, kind } => proto::build::warp::Requirement {
                requirement: Some(proto::build::warp::requirement::Requirement::Symbol(
                    SymbolRequirement { raw, kind },
                )),
            },
            Requirement::Url {
                url,
                tricorder_url: _,
                subpath,
            } => proto::build::warp::Requirement {
                requirement: Some(proto::build::warp::requirement::Requirement::Url(
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
            } => proto::build::warp::Requirement {
                requirement: Some(proto::build::warp::requirement::Requirement::Dependency(
                    DependencyRequirement {
                        name,
                        version,
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

impl From<&Config> for proto::google::protobuf::Struct {
    fn from(config: &Config) -> Self {
        let mut configs = HashMap::new();
        for (key, value) in config.values() {
            configs.insert(key.to_string(), value.clone().into());
        }

        proto::google::protobuf::Struct { fields: configs }
    }
}

impl From<Value> for proto::google::protobuf::Value {
    fn from(val: Value) -> Self {
        match val {
            Value::String(string) => proto::google::protobuf::Value {
                kind: Some(Kind::StringValue(string)),
            },
            Value::Target(target) => proto::google::protobuf::Value {
                kind: Some(Kind::StringValue(target)),
            },
            Value::File(file) => proto::google::protobuf::Value {
                kind: Some(Kind::StringValue(file.to_str().unwrap().to_string())),
            },
            Value::List(parts) => proto::google::protobuf::Value {
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
