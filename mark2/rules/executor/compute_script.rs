use crate::model::rule::{Config, Value};
use crate::model::{Dependencies, ExecutionEnvironment, Rule, Signature};
use crate::sync::*;
use crate::worker::TaskResults;

pub struct ComputeScript;

impl ComputeScript {
    pub fn as_js_source(
        task_results: Arc<TaskResults>,
        env: &ExecutionEnvironment,
        sig: &Signature,
        deps: &Dependencies,
        rule: &Rule,
        config: &Config,
    ) -> String {
        let config: serde_json::Value = config.clone().into();

        let compile_deps: serde_json::Value = serde_json::Value::Array(
            deps.compile_deps()
                .iter()
                .flat_map(|dep| task_results.get_task_result(*dep))
                .map(|tr| tr.artifact_manifest)
                .map(|dep| {
                    let mut map = serde_json::Map::new();
                    map.insert(
                        "name".to_string(),
                        serde_json::Value::String(dep.target().to_string()),
                    );
                    map.insert(
                        "target".to_string(),
                        serde_json::to_value(dep.target().to_string()).unwrap(),
                    );
                    map.insert(
                        "srcs".to_string(),
                        serde_json::Value::Array(
                            dep.srcs()
                                .iter()
                                .map(|p| serde_json::Value::String(p.to_str().unwrap().to_string()))
                                .collect(),
                        ),
                    );
                    map.insert(
                        "outs".to_string(),
                        serde_json::Value::Array(
                            dep.outs()
                                .iter()
                                .map(|p| serde_json::Value::String(p.to_str().unwrap().to_string()))
                                .collect(),
                        ),
                    );
                    serde_json::Value::Object(map)
                })
                .collect(),
        );

        let transitive_deps: serde_json::Value = serde_json::Value::Array(
            deps.transitive_deps()
                .iter()
                .flat_map(|dep| task_results.get_task_result(*dep))
                .map(|tr| tr.artifact_manifest)
                .map(|dep| {
                    let mut map = serde_json::Map::new();
                    map.insert(
                        "name".to_string(),
                        serde_json::Value::String(dep.target().into()),
                    );
                    map.insert(
                        "ruleName".to_string(),
                        serde_json::Value::String(dep.rule_name().to_string()),
                    );
                    map.insert(
                        "target".to_string(),
                        serde_json::to_value(dep.target().to_string()).unwrap(),
                    );
                    map.insert(
                        "srcs".to_string(),
                        serde_json::Value::Array(
                            dep.srcs()
                                .iter()
                                .map(|p| serde_json::Value::String(p.to_str().unwrap().to_string()))
                                .collect(),
                        ),
                    );
                    map.insert(
                        "outs".to_string(),
                        serde_json::Value::Array(
                            dep.outs()
                                .iter()
                                .map(|p| serde_json::Value::String(p.to_str().unwrap().to_string()))
                                .collect(),
                        ),
                    );
                    serde_json::Value::Object(map)
                })
                .collect(),
        );

        let runtime_deps: serde_json::Value = serde_json::Value::Array(
            deps.runtime_deps()
                .iter()
                .flat_map(|dep| task_results.get_task_result(*dep))
                .map(|tr| tr.artifact_manifest)
                .map(|dep| {
                    let mut map = serde_json::Map::new();
                    map.insert(
                        "name".to_string(),
                        serde_json::Value::String(dep.target().to_string()),
                    );
                    map.insert(
                        "ruleName".to_string(),
                        serde_json::Value::String(dep.rule_name().to_string()),
                    );
                    map.insert(
                        "target".to_string(),
                        serde_json::to_value(dep.target().to_string()).unwrap(),
                    );
                    map.insert(
                        "srcs".to_string(),
                        serde_json::Value::Array(
                            dep.srcs()
                                .iter()
                                .map(|p| serde_json::Value::String(p.to_str().unwrap().to_string()))
                                .collect(),
                        ),
                    );
                    map.insert(
                        "outs".to_string(),
                        serde_json::Value::Array(
                            dep.outs()
                                .iter()
                                .map(|p| serde_json::Value::String(p.to_str().unwrap().to_string()))
                                .collect(),
                        ),
                    );
                    serde_json::Value::Object(map)
                })
                .collect(),
        );

        let target: serde_json::Value = serde_json::to_value(sig.target().target_id()).unwrap();

        let env: serde_json::Value = env.clone().into();

        let program_template = r#"

(() => {
    let input = {
      target: {TARGET},
      rule: "{RULE_NAME}",
      cfg: {CONFIG},
      deps: {DEPS},
      transitiveDeps: {TRANSITIVE_DEPS},
      runtimeDeps: {RUNTIME_DEPS},
      env: {ENVIRONMENT},
    };

    trace(JSON.stringify(input, null, 2));

    Warp.Signatures.compute(input);
})();

        "#;

        program_template
            .replace("{TARGET}", &target.to_string())
            .replace("{RULE_NAME}", &rule.name())
            .replace("{CONFIG}", &config.to_string())
            .replace("{DEPS}", &compile_deps.to_string())
            .replace("{TRANSITIVE_DEPS}", &transitive_deps.to_string())
            .replace("{RUNTIME_DEPS}", &runtime_deps.to_string())
            .replace("{ENVIRONMENT}", &env.to_string())
    }
}

impl From<Config> for serde_json::Value {
    fn from(cfg: Config) -> Self {
        let mut map: serde_json::map::Map<String, serde_json::Value> = serde_json::map::Map::new();

        for (key, value) in cfg.values().iter() {
            map.insert(key.to_string(), value.clone().into());
        }

        serde_json::Value::Object(map)
    }
}

impl From<Value> for serde_json::Value {
    fn from(val: Value) -> Self {
        match val {
            Value::String(string) => serde_json::Value::String(string),
            Value::Target(target) => serde_json::Value::String(target.to_string()),
            Value::File(path) => serde_json::Value::String(path.to_str().unwrap().to_string()),
            Value::List(parts) => {
                serde_json::Value::Array(parts.iter().map(|e| e.clone().into()).collect())
            }
        }
    }
}
