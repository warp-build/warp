use std::collections::HashMap;
use std::path::PathBuf;

/// A TestRunner is a runnable script with a saved environment, used to execute a test in a uniform
/// way.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestRunner {
    pub env: HashMap<String, String>,
    pub test_runner: PathBuf,
}
