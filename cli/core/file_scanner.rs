use globset::{Glob, GlobSet, GlobSetBuilder};
use regex::Regex;
use std::path::PathBuf;
use thiserror::*;
use tokio::fs;

#[derive(Error, Debug)]
pub enum FileScannerError {
    #[error("Invalid regex error: {0:?}")]
    InvalidRegex(regex::Error),

    #[error("Invalid pattern error: {0:?}")]
    InvalidPattern(globset::Error),

    #[error("File I/O Error: {0:?}")]
    IOError(std::io::Error),
}

#[derive(Debug, Clone)]
pub struct FileScanner {
    root: PathBuf,
    match_pattern: Regex,
    skip_patterns: globset::GlobSet,
    max_concurrency: usize,
}

impl Default for FileScanner {
    fn default() -> Self {
        Self::new()
    }
}

impl FileScanner {
    pub fn new() -> FileScanner {
        FileScanner {
            root: PathBuf::from("."),
            match_pattern: Regex::new(".*").unwrap(),
            skip_patterns: GlobSet::default(),
            max_concurrency: 10,
        }
    }

    pub fn max_concurrency(&mut self, max_concurrency: usize) -> &mut FileScanner {
        self.max_concurrency = max_concurrency;
        self
    }

    pub async fn starting_from(
        &mut self,
        root: &PathBuf,
    ) -> Result<&mut FileScanner, FileScannerError> {
        self.root = fs::canonicalize(root)
            .await
            .map_err(FileScannerError::IOError)?;

        Ok(self)
    }

    pub fn matching_path(&mut self, pattern: &str) -> Result<&mut FileScanner, FileScannerError> {
        self.match_pattern = Regex::new(pattern).map_err(FileScannerError::InvalidRegex)?;
        Ok(self)
    }

    pub fn skipping_paths(
        &mut self,
        patterns: &[String],
    ) -> Result<&mut FileScanner, FileScannerError> {
        let mut builder = GlobSetBuilder::new();

        for pattern in patterns {
            let glob = Glob::new(pattern).map_err(FileScannerError::InvalidPattern)?;
            builder.add(glob);
        }

        self.skip_patterns = builder.build().map_err(FileScannerError::InvalidPattern)?;

        Ok(self)
    }

    #[tracing::instrument(name = "FileScanner::stream_files")]
    pub async fn stream_files(
        &self,
    ) -> impl futures::Stream<Item = Result<PathBuf, FileScannerError>> {
        let mut dirs = vec![self.root.clone()];
        let root = self.root.clone();
        let match_pattern = self.match_pattern.clone();
        let skip_patterns = self.skip_patterns.clone();
        async_stream::try_stream! {
            'dir_loop: while let Some(dir) = dirs.pop() {
                if dir != root {
                    let relative_dir = dir.strip_prefix(&root).unwrap().to_str().unwrap();
                    if skip_patterns.is_match(&relative_dir) {
                        continue 'dir_loop
                    }
                }

                let mut read_dir = fs::read_dir(&dir).await.map_err(FileScannerError::IOError)?;

                while let Ok(Some(entry)) = read_dir.next_entry().await {
                    if entry.path().is_dir() {
                        dirs.push(entry.path());
                        continue;
                    }
                    if match_pattern.is_match(entry.path().to_str().unwrap()) {
                        yield entry.path().clone();
                    }
                }
            }
        }
    }
}
