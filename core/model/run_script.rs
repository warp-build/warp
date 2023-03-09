use std::collections::HashMap;
use std::path::PathBuf;

/// A RunScript is a runnable script with a saved environment, used to execute the output of a rule
/// in a uniform way.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RunScript {
    pub env: HashMap<String, String>,
    pub run_script: PathBuf,
}
