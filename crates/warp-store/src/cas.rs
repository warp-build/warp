use crate::error::{Result, StoreError};
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_TEMP: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BlobDigest(String);

impl BlobDigest {
    pub fn parse(value: impl Into<String>) -> Result<Self> {
        let value = value.into();
        if value.len() != 64 || !value.bytes().all(|byte| byte.is_ascii_hexdigit()) {
            return Err(StoreError::new(format!(
                "invalid BLAKE3 blob digest {value:?}"
            )));
        }
        Ok(Self(value.to_ascii_lowercase()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    fn cache_key(&self) -> String {
        format!("blake3:{}", self.0)
    }
}

/// Content-addressed immutable blobs backed by cacache's on-disk store.
pub struct Cas {
    root: PathBuf,
}

impl Cas {
    pub fn new(root: impl Into<PathBuf>) -> Result<Self> {
        let root = root.into();
        fs::create_dir_all(&root).map_err(|error| StoreError::io("create CAS root", error))?;
        Ok(Self { root })
    }

    pub fn put_bytes(&self, bytes: &[u8]) -> Result<BlobDigest> {
        let digest = BlobDigest::parse(blake3::hash(bytes).to_hex().to_string())?;
        if self.verify_existing(&digest)? {
            return Ok(digest);
        }

        let mut writer = cacache::SyncWriter::create(&self.root, digest.cache_key())
            .map_err(|error| StoreError::new(format!("open cacache writer: {error}")))?;
        writer
            .write_all(bytes)
            .map_err(|error| StoreError::io("write CAS blob", error))?;
        writer
            .commit()
            .map_err(|error| StoreError::new(format!("commit cacache blob: {error}")))?;
        Ok(digest)
    }

    pub fn put_file(&self, source: &Path) -> Result<BlobDigest> {
        let digest = hash_file(source)?;
        if self.verify_existing(&digest)? {
            return Ok(digest);
        }

        let mut input =
            File::open(source).map_err(|error| StoreError::io("open CAS input", error))?;
        let mut writer = cacache::SyncWriter::create(&self.root, digest.cache_key())
            .map_err(|error| StoreError::new(format!("open cacache writer: {error}")))?;
        let copied_digest = copy_and_hash(&mut input, &mut writer)?;
        if copied_digest != digest {
            return Err(StoreError::new(format!(
                "CAS input changed while being stored: expected {}, found {}",
                digest.as_str(),
                copied_digest.as_str()
            )));
        }
        writer
            .commit()
            .map_err(|error| StoreError::new(format!("commit cacache blob: {error}")))?;
        Ok(digest)
    }

    pub fn get_bytes(&self, digest: &BlobDigest) -> Result<Option<Vec<u8>>> {
        let bytes = match cacache::read_sync(&self.root, digest.cache_key()) {
            Ok(bytes) => bytes,
            Err(cacache::Error::EntryNotFound(_, _)) => return Ok(None),
            Err(error) => return Err(StoreError::new(format!("read cacache blob: {error}"))),
        };
        let actual = BlobDigest::parse(blake3::hash(&bytes).to_hex().to_string())?;
        if &actual != digest {
            return Err(StoreError::new(format!(
                "BLAKE3 integrity failure: expected {}, found {}",
                digest.as_str(),
                actual.as_str()
            )));
        }
        Ok(Some(bytes))
    }

    pub fn get_file(&self, digest: &BlobDigest, destination: &Path) -> Result<bool> {
        let mut reader = match cacache::SyncReader::open(&self.root, digest.cache_key()) {
            Ok(reader) => reader,
            Err(cacache::Error::EntryNotFound(_, _)) => return Ok(false),
            Err(error) => return Err(StoreError::new(format!("open cacache blob: {error}"))),
        };

        let parent = destination
            .parent()
            .ok_or_else(|| StoreError::new("materialization path has no parent"))?;
        fs::create_dir_all(parent)
            .map_err(|error| StoreError::io("create materialization directory", error))?;
        let temporary = temporary_path(parent, "warp-materialized");
        let mut output = OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temporary)
            .map_err(|error| StoreError::io("create materialization file", error))?;
        let actual_digest = copy_and_hash(&mut reader, &mut output)?;
        reader
            .check()
            .map_err(|error| StoreError::new(format!("verify cacache blob: {error}")))?;
        if &actual_digest != digest {
            let _ = fs::remove_file(&temporary);
            return Err(StoreError::new(format!(
                "BLAKE3 integrity failure: expected {}, found {}",
                digest.as_str(),
                actual_digest.as_str()
            )));
        }
        output
            .flush()
            .map_err(|error| StoreError::io("flush materialized CAS blob", error))?;
        drop(output);
        fs::rename(&temporary, destination).map_err(|error| {
            let _ = fs::remove_file(&temporary);
            StoreError::io("publish materialized CAS blob", error)
        })?;
        Ok(true)
    }

    fn verify_existing(&self, digest: &BlobDigest) -> Result<bool> {
        let mut reader = match cacache::SyncReader::open(&self.root, digest.cache_key()) {
            Ok(reader) => reader,
            Err(cacache::Error::EntryNotFound(_, _)) => return Ok(false),
            Err(error) => return Err(StoreError::new(format!("open cacache blob: {error}"))),
        };
        let actual = hash_reader(&mut reader)?;
        reader
            .check()
            .map_err(|error| StoreError::new(format!("verify cacache blob: {error}")))?;
        if &actual != digest {
            return Err(StoreError::new(format!(
                "BLAKE3 integrity failure: expected {}, found {}",
                digest.as_str(),
                actual.as_str()
            )));
        }
        Ok(true)
    }
}

fn hash_file(path: &Path) -> Result<BlobDigest> {
    hash_reader(&mut File::open(path).map_err(|error| StoreError::io("open CAS input", error))?)
}

fn hash_reader(reader: &mut impl Read) -> Result<BlobDigest> {
    let mut hasher = blake3::Hasher::new();
    let mut buffer = [0; 32 * 1024];
    loop {
        let count = reader
            .read(&mut buffer)
            .map_err(|error| StoreError::io("read CAS blob", error))?;
        if count == 0 {
            break;
        }
        hasher.update(&buffer[..count]);
    }
    BlobDigest::parse(hasher.finalize().to_hex().to_string())
}

fn copy_and_hash(reader: &mut impl Read, writer: &mut impl Write) -> Result<BlobDigest> {
    let mut hasher = blake3::Hasher::new();
    let mut buffer = [0; 32 * 1024];
    loop {
        let count = reader
            .read(&mut buffer)
            .map_err(|error| StoreError::io("read CAS blob", error))?;
        if count == 0 {
            break;
        }
        writer
            .write_all(&buffer[..count])
            .map_err(|error| StoreError::io("write CAS blob", error))?;
        hasher.update(&buffer[..count]);
    }
    BlobDigest::parse(hasher.finalize().to_hex().to_string())
}

fn temporary_path(parent: &Path, prefix: &str) -> PathBuf {
    let id = NEXT_TEMP.fetch_add(1, Ordering::Relaxed);
    parent.join(format!(".{prefix}-{}-{id}", std::process::id()))
}

#[cfg(test)]
mod tests {
    use super::*;

    static NEXT_TEST: AtomicU64 = AtomicU64::new(0);

    struct TestDirectory(PathBuf);

    impl TestDirectory {
        fn new() -> Self {
            let id = NEXT_TEST.fetch_add(1, Ordering::Relaxed);
            let path =
                std::env::temp_dir().join(format!("warp-cas-test-{}-{id}", std::process::id()));
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
    fn stores_and_restores_content_by_blake3_digest() {
        let directory = TestDirectory::new();
        let store = Cas::new(directory.0.join("cas")).unwrap();
        let digest = store.put_bytes(b"immutable artifact").unwrap();
        let destination = directory.0.join("out/artifact");

        assert_eq!(
            digest.as_str(),
            blake3::hash(b"immutable artifact").to_hex().as_str()
        );
        assert!(store.get_file(&digest, &destination).unwrap());
        assert_eq!(fs::read(destination).unwrap(), b"immutable artifact");
        assert!(!store
            .get_file(
                &BlobDigest::parse("0".repeat(64)).unwrap(),
                &directory.0.join("missing")
            )
            .unwrap());
    }

    #[test]
    fn rejects_malformed_digests() {
        assert!(BlobDigest::parse("not-a-digest").is_err());
        assert!(BlobDigest::parse("a".repeat(64)).is_ok());
    }

    #[test]
    fn streams_files_without_loading_them_into_memory() {
        let directory = TestDirectory::new();
        let source = directory.0.join("large.bin");
        fs::write(&source, vec![42; 2 * 1024 * 1024]).unwrap();
        let store = Cas::new(directory.0.join("cas")).unwrap();
        let digest = store.put_file(&source).unwrap();
        let restored = directory.0.join("restored.bin");
        assert!(store.get_file(&digest, &restored).unwrap());
        assert_eq!(hash_file(&source).unwrap(), hash_file(&restored).unwrap());
    }
}
