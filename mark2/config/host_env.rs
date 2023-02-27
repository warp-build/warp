use sha2::Sha256;

/// The host environment.
///
pub struct HostEnv {
    host_triple: String,
}

impl HostEnv {
    pub fn host_triple(&self) -> &str {
        self.host_triple.as_ref()
    }

    pub fn hash(&self) -> String {
        let mut s = Sha256::new();
        s.update(&self.host_triple);
        format!("{:x}", s.finalize())
    }
}

impl Default for HostEnv {
    fn default() -> Self {
        Self {
            host_triple: guess_host_triple::guess_host_triple().unwrap().to_string(),
        }
    }
}
