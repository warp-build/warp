use crate::error::{Result, WarpError};
use rayon_core::{ThreadPool, ThreadPoolBuilder};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::time::{Duration, Instant};

/// Dedicated workers that run only BLAKE3 file-hashing jobs.
pub(super) struct HashPool {
    workers: ThreadPool,
    thread_count: usize,
    files_hashed: AtomicU64,
    bytes_hashed: AtomicU64,
    elapsed_nanos: AtomicU64,
    entries: Mutex<HashMap<PathBuf, Arc<HashEntry>>>,
}

#[derive(Clone, Copy)]
pub(super) struct HashStats {
    pub(super) thread_count: usize,
    pub(super) files_hashed: u64,
    pub(super) bytes_hashed: u64,
    pub(super) elapsed: Duration,
}

struct HashEntry {
    state: Mutex<HashState>,
    ready: Condvar,
}

enum HashState {
    Empty,
    Running,
    Complete(String),
    Failed(String),
}

impl HashPool {
    pub(super) fn new() -> Result<Self> {
        let thread_count = std::thread::available_parallelism()
            .map(usize::from)
            .unwrap_or(1);
        let workers = ThreadPoolBuilder::new()
            .num_threads(thread_count)
            .thread_name(|index| format!("warp-hash-{index}"))
            .build()
            .map_err(|error| WarpError::new(format!("create hashing thread pool: {error}")))?;
        Ok(Self {
            workers,
            thread_count,
            files_hashed: AtomicU64::new(0),
            bytes_hashed: AtomicU64::new(0),
            elapsed_nanos: AtomicU64::new(0),
            entries: Mutex::new(HashMap::new()),
        })
    }

    /// Hashes all previously unseen paths concurrently, sharing real digests for repeated paths.
    pub(super) fn hash_files(&self, paths: &[PathBuf]) -> Result<Vec<String>> {
        let entries = self.entries_for(paths)?;
        let mut pending = Vec::new();
        for (path, entry) in paths.iter().zip(&entries) {
            let mut state = entry
                .state
                .lock()
                .map_err(|_| WarpError::new("hash entry poisoned"))?;
            match &*state {
                HashState::Empty => {
                    *state = HashState::Running;
                    pending.push((path.clone(), entry.clone()));
                }
                HashState::Running | HashState::Complete(_) | HashState::Failed(_) => {}
            }
        }

        if !pending.is_empty() {
            let hash_each_file_in_parallel = pending.len() == 1;
            self.workers.scope(|scope| {
                for (path, entry) in pending {
                    scope.spawn(move |_| self.hash_one(path, entry, hash_each_file_in_parallel));
                }
            });
        }

        entries
            .iter()
            .map(|entry| {
                let mut state = entry
                    .state
                    .lock()
                    .map_err(|_| WarpError::new("hash entry poisoned"))?;
                loop {
                    match &*state {
                        HashState::Complete(digest) => return Ok(digest.clone()),
                        HashState::Failed(error) => return Err(WarpError::new(error.clone())),
                        HashState::Empty => {
                            return Err(WarpError::new("hash job was not scheduled"));
                        }
                        HashState::Running => {
                            state = entry
                                .ready
                                .wait(state)
                                .map_err(|_| WarpError::new("hash entry poisoned"))?;
                        }
                    }
                }
            })
            .collect()
    }

    pub(super) fn hash_file(&self, path: &Path) -> Result<String> {
        self.hash_files(&[path.to_path_buf()])?
            .into_iter()
            .next()
            .ok_or_else(|| WarpError::new("hash pool returned no digest"))
    }

    pub(super) fn remember(&self, path: &Path, digest: &str) -> Result<()> {
        let entry = self
            .entries_for(&[path.to_path_buf()])?
            .into_iter()
            .next()
            .ok_or_else(|| WarpError::new("hash pool returned no entry"))?;
        let mut state = entry
            .state
            .lock()
            .map_err(|_| WarpError::new("hash entry poisoned"))?;
        while matches!(*state, HashState::Running) {
            state = entry
                .ready
                .wait(state)
                .map_err(|_| WarpError::new("hash entry poisoned"))?;
        }
        *state = HashState::Complete(digest.to_owned());
        entry.ready.notify_all();
        Ok(())
    }

    pub(super) fn stats(&self) -> HashStats {
        HashStats {
            thread_count: self.thread_count,
            files_hashed: self.files_hashed.load(Ordering::Relaxed),
            bytes_hashed: self.bytes_hashed.load(Ordering::Relaxed),
            elapsed: Duration::from_nanos(self.elapsed_nanos.load(Ordering::Relaxed)),
        }
    }

    fn entries_for(&self, paths: &[PathBuf]) -> Result<Vec<Arc<HashEntry>>> {
        let mut entries = self
            .entries
            .lock()
            .map_err(|_| WarpError::new("hash cache poisoned"))?;
        Ok(paths
            .iter()
            .map(|path| {
                entries
                    .entry(path.clone())
                    .or_insert_with(|| {
                        Arc::new(HashEntry {
                            state: Mutex::new(HashState::Empty),
                            ready: Condvar::new(),
                        })
                    })
                    .clone()
            })
            .collect())
    }

    fn hash_one(&self, path: PathBuf, entry: Arc<HashEntry>, parallel_file: bool) {
        let started = Instant::now();
        let result: Result<(String, u64)> = (|| {
            let bytes = std::fs::metadata(&path)
                .map_err(|error| WarpError::io("inspect build input", error))?
                .len();
            let mut hasher = blake3::Hasher::new();
            // Hash small files on separate workers; split large files inside BLAKE3 itself.
            if parallel_file || bytes >= 1024 * 1024 {
                hasher
                    .update_mmap_rayon(&path)
                    .map_err(|error| WarpError::io("hash build input", error))?;
            } else {
                hasher
                    .update_mmap(&path)
                    .map_err(|error| WarpError::io("hash build input", error))?;
            }
            let digest = hasher.finalize().to_hex().to_string();
            Ok((digest, bytes))
        })();

        if let Ok((_, bytes)) = &result {
            self.files_hashed.fetch_add(1, Ordering::Relaxed);
            self.bytes_hashed.fetch_add(*bytes, Ordering::Relaxed);
            self.elapsed_nanos.fetch_add(
                started.elapsed().as_nanos().min(u64::MAX as u128) as u64,
                Ordering::Relaxed,
            );
        }
        if let Ok(mut state) = entry.state.lock() {
            *state = match result {
                Ok((digest, _)) => HashState::Complete(digest),
                Err(error) => HashState::Failed(error.to_string()),
            };
        }
        entry.ready.notify_all();
    }
}
