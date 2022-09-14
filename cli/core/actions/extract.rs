use crate::event::*;
use crate::event_channel::*;
use crate::Label;
use async_compression::futures::bufread::GzipDecoder;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::fs;
use tokio_util::compat::TokioAsyncReadCompatExt;

#[derive(Debug, Clone, PartialEq)]
pub struct ExtractAction {
    pub src: PathBuf,
    pub dst: PathBuf,
}

impl ExtractAction {
    #[tracing::instrument(name = "action::ExtractAction::run")]
    pub async fn run(
        &self,
        label: Label,
        sandbox_root: &PathBuf,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let mut file = fs::File::open(&sandbox_root.join(&self.src)).await?;

        let dst = fs::canonicalize(sandbox_root.join(&self.dst)).await?;

        event_channel.send(Event::ArchiveUnpacking(label));

        match async_zip::read::seek::ZipFileReader::new(&mut file).await {
            Ok(mut zip) => {
                for i in 0..zip.entries().len() {
                    let reader = zip.entry_reader(i).await?;

                    if reader.entry().dir() {
                        continue;
                    }

                    let file_path = PathBuf::from(reader.entry().name());
                    let path = dst.join(&file_path);
                    fs::create_dir_all(path.parent().unwrap()).await?;

                    let mut output = fs::File::create(path).await?;
                    reader.copy_to_end_crc(&mut output, 65536).await?;
                }
            }
            Err(_) => {
                let file = fs::File::open(&sandbox_root.join(&self.src)).await?;
                let decompress_stream =
                    GzipDecoder::new(futures::io::BufReader::new(file.compat()));
                let tar = async_tar::Archive::new(decompress_stream);
                tar.unpack(dst).await?;
            }
        }

        fs::remove_file(&sandbox_root.join(&self.src)).await?;

        Ok(())
    }
}
