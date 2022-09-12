use std::hash::Hash;

pub struct ExecutionEnvironment {
    host_triple: String,
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

impl Hash for ExecutionEnvironment {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.host_triple.hash(state);
    }
}
