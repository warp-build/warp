use std::path::PathBuf;
use tokio::fs;

#[derive(Debug, Clone, PartialEq)]
pub struct ExtractAction {
    pub src: PathBuf,
    pub dst: PathBuf,
}

impl ExtractAction {
    #[tracing::instrument(name = "action::ExtractAction::run")]
    pub async fn run(self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        let mut file = fs::File::open(&sandbox_root.join(&self.src)).await?;

        match async_zip::read::seek::ZipFileReader::new(&mut file).await {
            Ok(mut zip) => {
                for i in 0..zip.entries().len() {
                    let reader = zip.entry_reader(i).await?;

                    if reader.entry().dir() {
                        continue;
                    }

                    let file_path = PathBuf::from(reader.entry().name());
                    let path = sandbox_root.join(&self.dst).join(&file_path);
                    fs::create_dir_all(path.parent().unwrap()).await?;

                    let mut output = fs::File::create(path).await?;
                    reader.copy_to_end_crc(&mut output, 65536).await?;
                }
            }
            Err(_) => {
                let mut tar = tokio_tar::Archive::new(file);
                dbg!(&tar);
                tar.unpack(self.dst).await?;
            }
        }
        Ok(())
    }
}
