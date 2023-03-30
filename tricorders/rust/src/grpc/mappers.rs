use super::proto;
use super::proto::build::warp::tricorder::{
    get_ast_response, GetAstResponse, GetAstSuccessResponse,
};
use super::proto::build::warp::{FileRequirement, RemoteRequirement, SymbolRequirement};
use super::proto::google::protobuf::value::Kind;
use super::proto::google::protobuf::ListValue;
use crate::analysis::model::{Ast, Config, Goal, Requirement, Signature, Value};
use std::collections::HashMap;

impl From<proto::build::warp::TestMatcher> for Vec<String> {
    fn from(matcher: proto::build::warp::TestMatcher) -> Self {
        matcher.raw
    }
}

impl From<proto::build::warp::Goal> for Goal {
    fn from(goal: proto::build::warp::Goal) -> Self {
        match goal {
            proto::build::warp::Goal::Build => Goal::Build,
            proto::build::warp::Goal::Test => Goal::Test,
            proto::build::warp::Goal::Run => Goal::Run,
            proto::build::warp::Goal::Unknown => unimplemented!(),
        }
    }
}

impl From<Goal> for proto::build::warp::Goal {
    fn from(goal: Goal) -> Self {
        match goal {
            Goal::Build => proto::build::warp::Goal::Build,
            Goal::Test => proto::build::warp::Goal::Test,
            Goal::Run => proto::build::warp::Goal::Run,
        }
    }
}

pub fn ast_to_success_response(workspace_root: String, asts: Vec<Ast>) -> GetAstResponse {
    GetAstResponse {
        response: Some(get_ast_response::Response::Ok(GetAstSuccessResponse {
            subtrees: asts
                .iter()
                .map(|ast| proto::build::warp::tricorder::AstSubtree {
                    workspace_root: workspace_root.clone(),
                    file: ast.file().to_string_lossy().to_string(),
                    source_chunk: ast.source().to_string(),
                    ast: format!("{:#?}", ast.ast()),
                    signature_name: ast.test_name().to_string(),
                })
                .collect(),
        })),
    }
}

impl From<Signature> for proto::build::warp::Signature {
    fn from(sig: Signature) -> Self {
        proto::build::warp::Signature {
            name: sig.name().to_string(),
            rule: sig.rule().to_string(),
            deps: sig.deps().iter().cloned().map(|e| e.into()).collect(),
            runtime_deps: sig
                .runtime_deps()
                .iter()
                .cloned()
                .map(|e| e.into())
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
            Requirement::Remote {
                name,
                url,
                tricorder,
                subpath,
            } => proto::build::warp::Requirement {
                requirement: Some(proto::build::warp::requirement::Requirement::Remote(
                    RemoteRequirement {
                        name,
                        url: url.to_string(),
                        tricorder_url: tricorder.to_string(),
                        subpath: subpath.to_str().unwrap().to_string(),
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
