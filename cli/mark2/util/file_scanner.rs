use globset::{Glob, GlobSet, GlobSetBuilder};
use regex::Regex;
use std::path::PathBuf;
use thiserror::*;
use tokio::fs;
use tracing::*;

/// A generic concurrent, streaming file scanner.
///
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
        self.root =
            fs::canonicalize(root)
                .await
                .map_err(|err| FileScannerError::CouldNotCanonicalize {
                    path: root.clone(),
                    err,
                })?;

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
            let pattern = format!("*{}*", pattern);
            let glob = Glob::new(&pattern).map_err(FileScannerError::InvalidPattern)?;
            builder.add(glob);
        }

        self.skip_patterns = builder.build().map_err(FileScannerError::InvalidPattern)?;

        Ok(self)
    }

    #[tracing::instrument(name = "FileScanner::stream_files", skip(self))]
    pub async fn stream_files(
        &self,
    ) -> impl futures::Stream<Item = Result<PathBuf, FileScannerError>> {
        let mut dirs = vec![self.root.clone()];
        let root = self.root.clone();
        let match_pattern = self.match_pattern.clone();
        let skip_patterns = self.skip_patterns.clone();
        async_stream::try_stream! {
            while let Some(dir) = dirs.pop() {
                trace!("Reading {:?}", &dir);
                let mut read_dir = fs::read_dir(&dir).await.map_err(|err| FileScannerError::CouldNotReadDir { dir: dir.clone(), err })?;

                while let Ok(Some(entry)) = read_dir.next_entry().await {
                    let path = entry.path().clone();
                    trace!("Reading {:?}", &path);

                    if entry.metadata().await.unwrap().is_dir() {
                        let relative_dir = &path.strip_prefix(&root).unwrap().to_str().unwrap();
                        let should_skip = skip_patterns.is_match(&relative_dir);
                        if should_skip {
                            debug!("Skipped {:?}", &path);
                            continue;
                        }
                        dirs.push(path.clone());
                        continue;
                    }

                    if match_pattern.is_match(path.to_str().unwrap()) {
                        trace!("Yielding {:?}", &path);
                        yield path;
                        continue;
                    }


                }
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum FileScannerError {
    #[error("Invalid regex error: {0:?}")]
    InvalidRegex(regex::Error),

    #[error("Invalid pattern error: {0:?}")]
    InvalidPattern(globset::Error),

    #[error("Expected path {path:?} to exist but could not canonicalize it due to: {err:?}")]
    CouldNotCanonicalize { path: PathBuf, err: std::io::Error },

    #[error("Could not read {dir:?} while scanning due to: {err:?}")]
    CouldNotReadDir { dir: PathBuf, err: std::io::Error },
}
