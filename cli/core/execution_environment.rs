use thiserror::*;

#[derive(Error, Debug)]
pub enum ExecutionEnvironmentError {}

#[derive(Debug, Clone)]
pub struct ExecutionEnvironment {
    pub host_triple: String,
}

impl ExecutionEnvironment {
    pub fn new() -> Self {
        Self {
            host_triple: guess_host_triple::guess_host_triple().unwrap().to_string(),
        }
    }
}

impl Default for ExecutionEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

impl From<ExecutionEnvironment> for serde_json::Value {
    fn from(val: ExecutionEnvironment) -> Self {
        let mut map: serde_json::map::Map<String, serde_json::Value> = serde_json::map::Map::new();

        map.insert(
            "platform".to_string(),
            serde_json::Value::String(val.host_triple),
        );

        serde_json::Value::Object(map)
    }
}
