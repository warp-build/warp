use anyhow::Context;
use futures::StreamExt;
use regex::Regex;
use std::path::PathBuf;
use tokio::fs;
use tracing::*;

#[derive(Debug, Clone)]
pub struct FileScanner {
    root: PathBuf,
    match_pattern: Regex,
    skip_patterns: Vec<Regex>,
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
            skip_patterns: vec![],
            max_concurrency: 10,
        }
    }

    pub fn max_concurrency(&mut self, max_concurrency: usize) -> &mut FileScanner {
        self.max_concurrency = max_concurrency;
        self
    }

    pub fn starting_from(&mut self, root: &PathBuf) -> &mut FileScanner {
        self.root = root.clone();
        self
    }

    pub fn matching_path(&mut self, pattern: &str) -> Result<&mut FileScanner, anyhow::Error> {
        self.match_pattern = Regex::new(pattern)?;
        Ok(self)
    }

    pub fn skipping_paths(&mut self, patterns: &[&str]) -> Result<&mut FileScanner, anyhow::Error> {
        for pattern in patterns {
            self.skip_patterns.push(Regex::new(pattern)?);
        }
        Ok(self)
    }

    #[tracing::instrument(name = "FileScanner::find_files")]
    pub async fn find_files(&self) -> Result<Vec<PathBuf>, anyhow::Error> {
        let root = fs::canonicalize(&self.root).await?;
        self.scan_files(&root).await
    }

    async fn scan_files(&self, root: &PathBuf) -> Result<Vec<PathBuf>, anyhow::Error> {
        let mut dirs = vec![root.clone()];
        let match_pattern = self.match_pattern.clone();
        let skip_patterns = self.skip_patterns.clone();
        let file_stream = async_stream::stream! {
            'dir_loop: while let Some(dir) = dirs.pop() {
                for skip_pattern in &skip_patterns {
                    if skip_pattern.is_match(dir.to_str().unwrap()) {
                        continue 'dir_loop;
                    }
                }

                let mut read_dir = fs::read_dir(&dir).await.unwrap();
                debug!("Scanning dir: {:?}", &dir);
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
        };

        use std::sync::*;
        let files = Arc::new(RwLock::new(vec![]));
        file_stream
            .for_each_concurrent(self.max_concurrency, |file| async {
                files.write().unwrap().push(file)
            })
            .await;

        let files = files.read().unwrap().clone();
        Ok(files)
    }
}
