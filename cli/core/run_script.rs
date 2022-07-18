#[derive(Debug, Clone, PartialEq)]
pub struct RunScript {
    pub env: std::collections::HashMap<String, String>,
    pub run_script: std::path::PathBuf,
}
