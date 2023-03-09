#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RunScript {
    pub env: std::collections::HashMap<String, String>,
    pub run_script: std::path::PathBuf,
}
