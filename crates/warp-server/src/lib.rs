use std::path::PathBuf;
use warp_protocol::CasRef;
use warp_store::{BlobDigest, Cas};

#[derive(Debug, PartialEq, Eq)]
pub enum ArtifactError {
    InvalidReference(String),
    Store(String),
}

impl std::fmt::Display for ArtifactError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidReference(message) => {
                write!(formatter, "invalid CAS reference: {message}")
            }
            Self::Store(message) => write!(formatter, "artifact store error: {message}"),
        }
    }
}

impl std::error::Error for ArtifactError {}

/// Server-side artifact operations. Network transport and authentication are separate layers.
pub struct ArtifactService {
    store: Cas,
}

impl ArtifactService {
    pub fn new(root: impl Into<PathBuf>) -> Result<Self, ArtifactError> {
        let store = Cas::new(root).map_err(|error| ArtifactError::Store(error.to_string()))?;
        Ok(Self { store })
    }

    pub fn put(&self, bytes: &[u8]) -> Result<CasRef, ArtifactError> {
        let digest = self
            .store
            .put_bytes(bytes)
            .map_err(|error| ArtifactError::Store(error.to_string()))?;
        Ok(CasRef(format!("blake3:{}", digest.as_str())))
    }

    pub fn get(&self, reference: &CasRef) -> Result<Option<Vec<u8>>, ArtifactError> {
        let digest = reference
            .0
            .strip_prefix("blake3:")
            .ok_or_else(|| ArtifactError::InvalidReference("expected blake3:<hex>".to_string()))?;
        let digest = BlobDigest::parse(digest.to_string())
            .map_err(|error| ArtifactError::InvalidReference(error.to_string()))?;
        self.store
            .get_bytes(&digest)
            .map_err(|error| ArtifactError::Store(error.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_TEST: AtomicU64 = AtomicU64::new(0);

    struct TestDirectory(PathBuf);

    impl TestDirectory {
        fn new() -> Self {
            let id = NEXT_TEST.fetch_add(1, Ordering::Relaxed);
            let path =
                std::env::temp_dir().join(format!("warp-server-test-{}-{id}", std::process::id()));
            std::fs::create_dir_all(&path).unwrap();
            Self(path)
        }
    }

    impl Drop for TestDirectory {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn artifact_service_round_trips_content_addressed_artifacts() {
        let directory = TestDirectory::new();
        let service = ArtifactService::new(&directory.0).unwrap();
        let reference = service.put(b"team artifact").unwrap();
        assert!(reference.0.starts_with("blake3:"));
        assert_eq!(
            service.get(&reference).unwrap(),
            Some(b"team artifact".to_vec())
        );
        assert!(service
            .get(&CasRef("blake3:0".to_string()))
            .unwrap_err()
            .to_string()
            .contains("invalid"));
    }
}
