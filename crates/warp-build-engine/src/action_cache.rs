use crate::error::{Result, WarpError};
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use warp_store::BlobDigest;

static NEXT_TEMP: AtomicU64 = AtomicU64::new(0);

pub struct ActionCache {
    root: PathBuf,
}

impl ActionCache {
    pub fn new(root: impl Into<PathBuf>) -> Result<Self> {
        let root = root.into();
        fs::create_dir_all(&root).map_err(|error| WarpError::io("create action cache", error))?;
        Ok(Self { root })
    }

    pub fn lookup(&self, key: &str) -> Result<Option<BlobDigest>> {
        let path = self.path_for(key)?;
        match fs::read_to_string(&path) {
            Ok(value) => BlobDigest::parse(value.trim().to_string())
                .map(Some)
                .map_err(|error| WarpError::new(format!("invalid action-cache entry: {error}"))),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(error) => Err(WarpError::io("read action-cache entry", error)),
        }
    }

    pub fn insert(&self, key: &str, output: &BlobDigest) -> Result<()> {
        let path = self.path_for(key)?;
        let id = NEXT_TEMP.fetch_add(1, Ordering::Relaxed);
        let temporary = self
            .root
            .join(format!(".warp-action-{}-{id}", std::process::id()));
        let mut file = OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temporary)
            .map_err(|error| WarpError::io("create temporary action-cache entry", error))?;
        file.write_all(output.as_str().as_bytes())
            .and_then(|()| file.write_all(b"\n"))
            .map_err(|error| WarpError::io("write action-cache entry", error))?;
        drop(file);
        fs::rename(&temporary, &path).map_err(|error| {
            let _ = fs::remove_file(&temporary);
            WarpError::io("publish action-cache entry", error)
        })
    }

    fn path_for(&self, key: &str) -> Result<PathBuf> {
        let digest = key
            .strip_prefix("blake3:")
            .ok_or_else(|| WarpError::new("action key must use the BLAKE3 algorithm"))?;
        let digest = BlobDigest::parse(digest.to_string())
            .map_err(|error| WarpError::new(format!("invalid action key: {error}")))?;
        Ok(self.root.join(digest.as_str()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicU64;

    static NEXT_TEST: AtomicU64 = AtomicU64::new(0);

    struct TestDirectory(PathBuf);

    impl TestDirectory {
        fn new() -> Self {
            let id = NEXT_TEST.fetch_add(1, Ordering::Relaxed);
            let path = std::env::temp_dir().join(format!(
                "warp-action-cache-test-{}-{id}",
                std::process::id()
            ));
            fs::create_dir_all(&path).unwrap();
            Self(path)
        }
    }

    impl Drop for TestDirectory {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn maps_action_keys_to_content_addresses() {
        let directory = TestDirectory::new();
        let cache = ActionCache::new(directory.0.join("actions")).unwrap();
        let key = format!("blake3:{}", "a".repeat(64));
        let output = BlobDigest::parse("b".repeat(64)).unwrap();

        assert_eq!(cache.lookup(&key).unwrap(), None);
        cache.insert(&key, &output).unwrap();
        assert_eq!(cache.lookup(&key).unwrap(), Some(output));
        assert!(cache.lookup("sha256:0000").is_err());
    }
}
