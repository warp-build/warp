//! Protocol primitives for the next-generation Warp execution model.
//!
//! The protocol is intentionally independent of executors, storage backends,
//! schedulers, and language-specific tricorders. Those components exchange
//! these values across local and remote boundaries.

use blake3::Hasher;
use serde::{Deserialize, Serialize};

pub const PROTOCOL_VERSION: &str = "warp.protocol/v1";

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct TaskRequest {
    pub workspace: String,
    pub command: Vec<String>,
    pub working_directory: String,
    #[serde(default)]
    pub environment: Vec<EnvironmentValue>,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct ScannedBlueprint {
    pub protocol: String,
    pub task: TaskRequest,
    pub expected_inputs: Vec<InputRef>,
    pub expected_outputs: Vec<OutputSpec>,
    pub dependencies: Vec<CasRef>,
    pub toolchain: Toolchain,
    pub capabilities: CapabilitySet,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct VerifiedBlueprint {
    pub protocol: String,
    pub task: TaskRequest,
    pub inputs: Vec<InputRef>,
    pub outputs: Vec<OutputRef>,
    pub dependencies: Vec<CasRef>,
    pub toolchain: Toolchain,
    pub platform: Platform,
    pub capabilities: CapabilitySet,
    pub environment: Vec<EnvironmentValue>,
    pub trace: TraceRefs,
    pub exit_status: i32,
}

impl VerifiedBlueprint {
    pub fn digest(&self) -> CasRef {
        let bytes = serde_json::to_vec(self).expect("verified blueprint must serialize");
        let mut digest = Hasher::new();
        digest.update(&bytes);
        CasRef(format!("blake3:{}", digest.finalize().to_hex()))
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct InputRef {
    pub path: String,
    pub content: CasRef,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct OutputSpec {
    pub path: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct OutputRef {
    pub path: String,
    pub content: CasRef,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct EnvironmentValue {
    pub name: String,
    pub value_digest: Option<CasRef>,
    pub present: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, PartialEq, Eq)]
pub struct Toolchain {
    pub runtime: Vec<String>,
    pub package_manager: Option<String>,
    pub manifest: Option<CasRef>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, PartialEq, Eq)]
pub struct Platform {
    pub operating_system: String,
    pub architecture: String,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, PartialEq, Eq)]
pub struct CapabilitySet {
    pub allowed: Vec<String>,
    pub observed: Vec<String>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, PartialEq, Eq)]
pub struct TraceRefs {
    pub process: Option<CasRef>,
    pub filesystem: Option<CasRef>,
    pub network: Option<CasRef>,
    pub stdout: Option<CasRef>,
    pub stderr: Option<CasRef>,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct CasRef(pub String);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verified_blueprint_digest_is_stable() {
        let blueprint = VerifiedBlueprint {
            protocol: PROTOCOL_VERSION.to_string(),
            task: TaskRequest {
                workspace: "example".to_string(),
                command: vec!["yarn".to_string(), "test".to_string()],
                working_directory: "/workspace".to_string(),
                environment: vec![],
            },
            inputs: vec![],
            outputs: vec![],
            dependencies: vec![],
            toolchain: Toolchain::default(),
            platform: Platform {
                operating_system: "linux".to_string(),
                architecture: "amd64".to_string(),
            },
            capabilities: CapabilitySet::default(),
            environment: vec![],
            trace: TraceRefs::default(),
            exit_status: 0,
        };

        assert_eq!(blueprint.digest(), blueprint.digest());
        assert!(blueprint.digest().0.starts_with("blake3:"));
    }
}
