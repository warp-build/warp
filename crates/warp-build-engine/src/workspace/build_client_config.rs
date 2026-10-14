#[derive(Clone, Debug, Default)]
pub struct BuildClientConfig {
    pub default_binary: Option<String>,
    pub excluded_directories: Vec<String>,
}
