use std::path::Path;

#[derive(Clone, Debug)]
pub struct ArtifactId(String);

impl ArtifactId {
    pub fn new<I: Into<String>>(id: I) -> Self {
        Self(id.into())
    }

    pub fn inner(&self) -> &str {
        &self.0
    }
}

impl AsRef<Path> for ArtifactId {
    fn as_ref(&self) -> &Path {
        Path::new(&self.0)
    }
}
