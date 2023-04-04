use super::{Archive, ArchiveBuilderError};
use crate::events::event::ArchiveEvent;
use crate::events::EventChannel;
use crate::store::{ArtifactManifest, MANIFEST_FILE};
use crate::sync::Arc;
use crate::Config;
use async_compression::futures::bufread::GzipDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;
use futures::{StreamExt, TryStreamExt};
use sha2::{Digest, Sha256};
use std::io::Read;
use std::path::{Path, PathBuf};
use thiserror::Error;
use tokio::fs;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio_util::compat::TokioAsyncReadCompatExt;
use tracing::instrument;
use url::Url;

#[derive(Debug, Builder)]
pub struct ArchiveManager {
    client: reqwest::Client,
    archive_root: PathBuf,
    force_redownload: bool,
    public_store_cdn_url: Url,
    event_channel: Arc<EventChannel>,
}

impl ArchiveManager {
    pub fn new(config: &Config) -> Self {
        Self {
            client: config.http_client().clone(),
            event_channel: config.event_channel(),
            archive_root: config.archive_root().to_path_buf(),
            force_redownload: config.force_redownload(),
            public_store_cdn_url: config.public_store_cdn_url().clone(),
        }
    }

    #[instrument(name = "ArchiveManager::find", skip(self))]
    pub async fn find(&self, url: &Url) -> Result<Option<Archive>, ArchiveManagerError> {
        let path = self.archive_path(url);
        let sha256 = path.with_extension("sha256");

        if fs::File::open(&path).await.is_err() {
            return Ok(None);
        };

        if fs::File::open(&sha256).await.is_err() {
            return Ok(None);
        };

        let mut hash_file = fs::File::open(&sha256).await.unwrap();
        let mut hash = String::new();
        hash_file.read_to_string(&mut hash).await.unwrap();

        Ok(Some(
            Archive::builder()
                .final_path(path)
                .hash(hash)
                .url(url.clone())
                .build()
                .unwrap(),
        ))
    }

    #[instrument(name = "ArchiveManager::extract", skip(self))]
    pub async fn extract(
        &self,
        archive: &Archive,
        root: &Path,
    ) -> Result<Archive, ArchiveManagerError> {
        let extracted_archive = Archive::builder()
            .final_path(root.to_path_buf())
            .hash(archive.hash())
            .url(archive.url().clone())
            .build()?;

        if fs::metadata(root).await.is_ok() {
            return Ok(extracted_archive);
        }

        self.event_channel.send(ArchiveEvent::ExtractionStarted {
            source: archive.final_path().to_path_buf(),
            destination: root.to_path_buf(),
            url: archive.url().clone(),
        });

        let file = fs::File::open(archive.final_path()).await?;
        {
            let root = root.to_path_buf();
            let mut data = vec![];
            let mut unzip_stream = GzipDecoder::new(futures::io::BufReader::new(file.compat()));

            if futures::AsyncReadExt::read_to_end(&mut unzip_stream, &mut data)
                .await
                .is_err()
            {
                data = vec![];
                let mut file = std::fs::File::open(archive.final_path()).unwrap();
                file.read_to_end(&mut data)?;
            };

            tokio::task::spawn_blocking(move || {
                let mut tar = tar::Archive::new(std::io::BufReader::new(&*data));
                tar.unpack(root)
            })
            .await
            .unwrap()
        }?;

        self.event_channel.send(ArchiveEvent::ExtractionCompleted {
            source: extracted_archive.final_path().to_path_buf(),
            destination: root.to_path_buf(),
            url: extracted_archive.url().clone(),
        });

        Ok(extracted_archive)
    }

    #[instrument(name = "ArchiveManager::compress", skip(self))]
    pub async fn compress(
        &self,
        manifest: Arc<ArtifactManifest>,
    ) -> Result<Archive, ArchiveManagerError> {
        let archive_url = self
            .public_store_cdn_url
            .join(&format!("{}.tar.gz", manifest.hash()))
            .unwrap();

        if let Some(archive) = self.find(&archive_url).await? {
            self.event_channel.send(ArchiveEvent::CompressionCached {
                target: manifest.target().to_string(),
                sha256: manifest.hash().to_string(),
                total_files: manifest.outs().len(),
            });
            return Ok(archive);
        }

        self.event_channel.send(ArchiveEvent::CompressionStarted {
            target: manifest.target().to_string(),
            sha256: manifest.hash().to_string(),
            total_files: manifest.outs().len(),
        });

        let tarball_path = self.archive_path(&archive_url);

        {
            let ec = self.event_channel.clone();
            let tarball_path = tarball_path.clone();
            let manifest = manifest.clone();
            let manifest_path = manifest.store_path().join(MANIFEST_FILE);
            tokio::task::spawn_blocking(move || {
                let _ = std::fs::create_dir_all(tarball_path.parent().unwrap());
                let tar_file = std::fs::File::create(tarball_path).unwrap();
                let enc = GzEncoder::new(tar_file, Compression::default());
                let mut tar = tar::Builder::new(enc);
                let total_files = manifest.outs().len();
                let target = manifest.target().to_string();

                let mut f = std::fs::File::open(manifest_path)?;
                tar.append_file(MANIFEST_FILE, &mut f)?;

                let mut files = manifest.outs().to_vec();
                let mut current = 1;
                while let Some(out) = files.pop() {
                    let path = manifest.store_path().join(&out);
                    let mut f = std::fs::File::open(&path)?;
                    if f.metadata().unwrap().is_dir() {
                        let mut read_dir = std::fs::read_dir(&path)?;
                        while let Some(Ok(entry)) = read_dir.next() {
                            let path = entry.path().clone();
                            files.push(
                                path.strip_prefix(manifest.store_path())
                                    .unwrap()
                                    .to_path_buf(),
                            );
                        }
                        continue;
                    }

                    tar.append_file(out, &mut f)?;
                    ec.send(ArchiveEvent::CompressionProgress {
                        target: target.clone(),
                        current,
                        total_files,
                    });
                    current += 1;
                }

                tar.finish()
            })
            .await
            .unwrap()?
        };

        fs::write(tarball_path.with_extension("sha256"), manifest.hash()).await?;

        self.event_channel.send(ArchiveEvent::CompressionCompleted {
            target: manifest.target().to_string(),
            sha256: manifest.hash().to_string(),
            total_files: manifest.outs().len(),
        });

        Ok(Archive::builder()
            .final_path(tarball_path)
            .url(archive_url)
            .hash(manifest.hash().to_string())
            .build()
            .unwrap())
    }

    #[instrument(name = "ArchiveManager::download", skip(self))]
    pub async fn download(&self, url: &Url) -> Result<Archive, ArchiveManagerError> {
        if !self.force_redownload {
            if let Some(archive) = self.find(url).await? {
                return Ok(archive);
            }
        }

        self.event_channel
            .send(ArchiveEvent::DownloadStarted { url: url.clone() });

        let response = self.client.get(url.clone()).send().await?;

        if response.status().is_success() {
            let (final_path, hash, total_size) = self.stream_response(response, url).await?;

            self.event_channel.send(ArchiveEvent::DownloadCompleted {
                url: url.clone(),
                sha256: hash.clone(),
                total_size,
            });

            let archive = Archive::builder()
                .final_path(final_path)
                .hash(hash)
                .url(url.clone())
                .build()?;

            return Ok(archive);
        }

        Err(ArchiveManagerError::DownloadFailed {
            url: url.clone(),
            err: response.status(),
        })
    }

    async fn stream_response(
        &self,
        response: reqwest::Response,
        url: &Url,
    ) -> Result<(PathBuf, String, u64), std::io::Error> {
        let total_size = response.content_length().unwrap_or_default();
        let mut progress = 0;

        let mut byte_stream = response
            .bytes_stream()
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));

        let tmp_root = self.archive_root.join("_tmp");
        fs::create_dir_all(&tmp_root).await?;

        let tempfile = tempfile::NamedTempFile::new_in(&tmp_root)?;
        let mut outfile = fs::File::from_std(tempfile.reopen()?);

        let mut s = Sha256::new();
        while let Some(chunk) = byte_stream.next().await {
            let mut chunk = chunk?;
            progress += chunk.len();
            self.event_channel.send(ArchiveEvent::DownloadProgress {
                url: url.clone(),
                progress,
                total_size,
            });
            s.update(&chunk);
            outfile.write_all_buf(&mut chunk).await?;
        }

        let path = self.archive_path(url);
        fs::create_dir_all(&path.parent().unwrap()).await?;
        tempfile.persist(&path)?;

        let hash = format!("{:x}", s.finalize());
        fs::write(path.with_extension("sha256"), &hash).await?;

        Ok((path, hash, total_size))
    }

    fn archive_path(&self, url: &Url) -> PathBuf {
        let mut s = Sha256::new();
        s.update(&url.to_string());
        let hash = format!("{:x}", s.finalize());

        let scheme_and_host = PathBuf::from(url.scheme()).join(url.host_str().unwrap());
        self.archive_root.join(scheme_and_host).join(hash)
    }
}

#[derive(Error, Debug)]
pub enum ArchiveManagerError {
    #[error("Could not download URL {url:?} due to: {err:?}")]
    DownloadFailed { url: Url, err: hyper::StatusCode },

    #[error(transparent)]
    RequestError(reqwest::Error),

    #[error(transparent)]
    IoError(std::io::Error),

    #[error(transparent)]
    ArchiveBuilderError(ArchiveBuilderError),
}

impl From<std::io::Error> for ArchiveManagerError {
    fn from(value: std::io::Error) -> Self {
        ArchiveManagerError::IoError(value)
    }
}

impl From<reqwest::Error> for ArchiveManagerError {
    fn from(value: reqwest::Error) -> Self {
        ArchiveManagerError::RequestError(value)
    }
}

impl From<ArchiveBuilderError> for ArchiveManagerError {
    fn from(value: ArchiveBuilderError) -> Self {
        ArchiveManagerError::ArchiveBuilderError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_fs::prelude::*;

    #[tokio::test]
    async fn downloads_file_to_archives_folder() {
        let archive_root = assert_fs::TempDir::new().unwrap();

        let config = Config::builder()
            .archive_root(archive_root.path().to_path_buf())
            .build()
            .unwrap();
        let am = ArchiveManager::new(&config);

        let _m = mockito::mock("GET", "/sample_artifact.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create();

        let url: Url = format!("{}/sample_artifact.tar.gz", mockito::server_url())
            .parse()
            .unwrap();
        let archive = am.download(&url).await.unwrap();
        let final_path = archive
            .final_path()
            .strip_prefix(archive_root.path())
            .unwrap();

        assert_eq!(
            final_path.to_string_lossy().to_string(),
            "http/127.0.0.1/53c7c792c4667c37741310914aa53416fe9750c8f787641e7aff55ecfdbe97d0"
        );
    }

    #[tokio::test]
    async fn fails_if_url_is_not_downloadable() {
        let config = Config::default();
        let am = ArchiveManager::new(&config);

        let _m = mockito::mock("GET", "/sample_artifact.tar.gz")
            .with_status(400)
            .create();

        let url: Url = format!("{}/sample_artifact.tar.gz", mockito::server_url())
            .parse()
            .unwrap();
        assert!(am.download(&url).await.is_err());
    }

    #[tokio::test]
    async fn compress_files_from_artifact_manifest() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        // let warp_root = warp_root.into_persistent();
        dbg!(&warp_root);

        let invocation_dir = assert_fs::TempDir::new().unwrap();
        // let invocation_dir = invocation_dir.into_persistent();
        dbg!(&invocation_dir);

        let config = Config::builder()
            .invocation_dir(invocation_dir.path().to_path_buf())
            .warp_root(warp_root.path().to_path_buf())
            .build()
            .unwrap();

        let store_entry = warp_root.child("store/a-hash");
        store_entry.child("output_file").touch().unwrap();

        let manifest = store_entry.child("Manifest.json");
        let expected = include_str!("./fixtures/Manifest.json").replace(
            "{STORE_PATH}",
            config.artifact_store_root().to_str().unwrap(),
        );
        manifest.write_str(&expected).unwrap();

        let artifact_manifest: Arc<ArtifactManifest> = ArtifactManifest::from_file(manifest.path())
            .await
            .unwrap()
            .into();

        dbg!(&artifact_manifest);

        let am = ArchiveManager::new(&config);

        let archive = am.compress(artifact_manifest.clone()).await.unwrap();

        let final_path = archive.final_path().strip_prefix(&am.archive_root).unwrap();

        let archive_url = am
            .public_store_cdn_url
            .join(&format!("{}.tar.gz", artifact_manifest.hash()))
            .unwrap();

        let tarball_path = am.archive_path(&archive_url);

        let expected = tarball_path.strip_prefix(&am.archive_root).unwrap();

        assert_eq!(final_path, expected);
    }
}
