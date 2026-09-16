use std::path::{Path, PathBuf};

pub trait PathExt {
    fn clean(&self) -> PathBuf;
}

#[cfg(target_os = "macos")]
impl PathExt for PathBuf {
    fn clean(&self) -> PathBuf {
        if let Ok(path) = self.strip_prefix("/private") {
            Path::new("/").join(path)
        } else {
            self.to_path_buf()
        }
    }
}

#[cfg(not(target_os = "macos"))]
impl PathExt for PathBuf {
    fn clean(&self) -> PathBuf {
        self.to_path_buf()
    }
}
