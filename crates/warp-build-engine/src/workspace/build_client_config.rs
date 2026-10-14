use super::Label;

#[derive(Clone, Debug, Default)]
pub struct BuildClientConfig {
    pub default_binary: Option<Label>,
    pub excluded_directories: Vec<String>,
}
