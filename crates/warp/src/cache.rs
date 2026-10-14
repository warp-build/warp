use crate::digest::sha256_reader;
use crate::error::{Result, WarpError};
use std::fs::{self, File, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_TEMP: AtomicU64 = AtomicU64::new(0);

pub fn restore(cache: &Path, key: &str, output: &Path) -> Result<bool> {
    let artifact = artifact_path(cache, key);
    let manifest = manifest_path(cache, key);
    if !artifact.is_file() || !manifest.is_file() {
        return Ok(false);
    }

    let expected = fs::read_to_string(&manifest)
        .map_err(|error| WarpError::io("read cache manifest", error))?;
    let actual = sha256_reader(
        File::open(&artifact).map_err(|error| WarpError::io("open cached artifact", error))?,
    )?;
    if expected.trim() != actual {
        return Ok(false);
    }

    copy_atomically(&artifact, output)?;
    Ok(true)
}

pub fn store(cache: &Path, key: &str, artifact: &Path) -> Result<()> {
    fs::create_dir_all(cache).map_err(|error| WarpError::io("create build cache", error))?;
    let artifact_path = artifact_path(cache, key);
    let manifest_path = manifest_path(cache, key);
    let artifact_digest = sha256_reader(
        File::open(artifact).map_err(|error| WarpError::io("open compiled artifact", error))?,
    )?;

    copy_atomically(artifact, &artifact_path)?;
    write_atomically(&manifest_path, artifact_digest.as_bytes())?;
    Ok(())
}

pub fn copy_atomically(source: &Path, destination: &Path) -> Result<()> {
    let parent = destination
        .parent()
        .ok_or_else(|| WarpError::new("output path has no parent directory"))?;
    fs::create_dir_all(parent).map_err(|error| WarpError::io("create output directory", error))?;
    let temporary = temporary_path(parent, "warp-output");
    fs::copy(source, &temporary).map_err(|error| WarpError::io("stage output artifact", error))?;
    replace_file(&temporary, destination, "publish output artifact")
}

fn write_atomically(path: &Path, contents: &[u8]) -> Result<()> {
    let parent = path
        .parent()
        .ok_or_else(|| WarpError::new("cache manifest path has no parent directory"))?;
    fs::create_dir_all(parent).map_err(|error| WarpError::io("create cache directory", error))?;
    let temporary = temporary_path(parent, "warp-manifest");
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&temporary)
        .map_err(|error| WarpError::io("create temporary cache manifest", error))?;
    file.write_all(contents)
        .map_err(|error| WarpError::io("write cache manifest", error))?;
    file.write_all(b"\n")
        .map_err(|error| WarpError::io("finish cache manifest", error))?;
    drop(file);
    replace_file(&temporary, path, "publish cache manifest")
}

fn replace_file(temporary: &Path, destination: &Path, operation: &str) -> Result<()> {
    if let Err(error) = fs::rename(temporary, destination) {
        let _ = fs::remove_file(temporary);
        return Err(WarpError::io(operation, error));
    }
    Ok(())
}

fn artifact_path(cache: &Path, key: &str) -> PathBuf {
    cache.join(format!("{key}.artifact"))
}

fn manifest_path(cache: &Path, key: &str) -> PathBuf {
    cache.join(format!("{key}.sha256"))
}

fn temporary_path(parent: &Path, prefix: &str) -> PathBuf {
    let id = NEXT_TEMP.fetch_add(1, Ordering::Relaxed);
    parent.join(format!(".{prefix}-{}-{id}", std::process::id()))
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
                std::env::temp_dir().join(format!("warp-cache-test-{}-{id}", std::process::id()));
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
    fn restores_only_intact_cached_artifacts() {
        let directory = TestDirectory::new();
        let cache = directory.0.join("cache");
        let compiled = directory.0.join("compiled");
        let restored = directory.0.join("out/warp");
        fs::write(&compiled, b"executable").unwrap();

        store(&cache, "abc123", &compiled).unwrap();
        assert!(restore(&cache, "abc123", &restored).unwrap());
        assert_eq!(fs::read(&restored).unwrap(), b"executable");

        fs::write(artifact_path(&cache, "abc123"), b"tampered").unwrap();
        assert!(!restore(&cache, "abc123", &restored).unwrap());
    }
}
