use crate::proto::build::warp::Signature;
use std::path::PathBuf;
use crate::proto::google::protobuf::{Struct, ListValue, Value};
use crate::proto::google::protobuf::value::Kind;
use std::collections::HashMap;

#[derive(Default)]
pub struct RsGenerateSignature {}

impl RsGenerateSignature {
    pub fn generate(file: &str, _code_paths: Vec<PathBuf>) -> Vec<Signature> {

	let sources = vec![Value{
	     kind: Some(Kind::StringValue("main.rs".to_string())),
	}];
	
	let signature = Signature {
	    name: file.to_string(),
	    deps: vec![],
	    rule: "rust_binary".to_string(),
	    runtime_deps: vec![],
	    config: Some(Struct{
		fields: HashMap::from_iter(vec![(
		    "srcs".to_string(),
		    Value {
			kind: Some(Kind::ListValue(ListValue {
			    values: sources,
			})),
		    },
		)]),
	    }),
	};
	vec![signature]
    }
}
