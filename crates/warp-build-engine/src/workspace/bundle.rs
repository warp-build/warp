use crate::error::{Result, WarpError};
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Component, Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

const BUNDLE_MAGIC: &[u8] = b"WARP-TREE\x01";
const MAX_ENTRIES: u64 = 1_000_000;
const MAX_PATH_LENGTH: u64 = 16 * 1024;
static NEXT_RESTORE: AtomicU64 = AtomicU64::new(0);

pub(super) fn pack_directory(source: &Path, bundle: &Path) -> Result<()> {
    let mut files = Vec::new();
    collect_files(source, source, &mut files)?;
    files.sort();
    if files.is_empty() {
        return Err(WarpError::new(format!(
            "build output directory {} contains no files",
            source.display()
        )));
    }

    let mut output =
        File::create(bundle).map_err(|error| WarpError::io("create build output bundle", error))?;
    output
        .write_all(BUNDLE_MAGIC)
        .map_err(|error| WarpError::io("write build output bundle", error))?;
    write_u64(&mut output, files.len() as u64)?;

    for path in files {
        let relative = path
            .strip_prefix(source)
            .map_err(|_| WarpError::new("build output escaped its declared directory"))?;
        let relative = relative.to_str().ok_or_else(|| {
            WarpError::new(format!(
                "build output path is not valid UTF-8: {}",
                relative.display()
            ))
        })?;
        let relative = relative.replace('\\', "/");
        let relative_bytes = relative.as_bytes();
        if relative_bytes.is_empty() || relative_bytes.len() as u64 > MAX_PATH_LENGTH {
            return Err(WarpError::new(
                "build output path has an unsupported length",
            ));
        }

        let mut input =
            File::open(&path).map_err(|error| WarpError::io("open build output file", error))?;
        let size = input
            .metadata()
            .map_err(|error| WarpError::io("inspect build output file", error))?
            .len();
        write_u64(&mut output, relative_bytes.len() as u64)?;
        write_u64(&mut output, size)?;
        output
            .write_all(relative_bytes)
            .map_err(|error| WarpError::io("write build output path", error))?;
        let copied = std::io::copy(&mut input, &mut output)
            .map_err(|error| WarpError::io("bundle build output file", error))?;
        if copied != size {
            return Err(WarpError::new(format!(
                "build output changed while bundling {}",
                path.display()
            )));
        }
    }
    output
        .flush()
        .map_err(|error| WarpError::io("flush build output bundle", error))?;
    Ok(())
}

pub(super) fn restore_directory(bundle: &Path, destination: &Path) -> Result<()> {
    let staging = create_staging_directory(destination)?;
    if let Err(error) = unpack_directory(bundle, &staging) {
        let _ = fs::remove_dir_all(&staging);
        return Err(error);
    }

    match fs::symlink_metadata(destination) {
        Ok(metadata) if metadata.file_type().is_symlink() => {
            let _ = fs::remove_dir_all(&staging);
            return Err(WarpError::new(format!(
                "refusing to replace symbolic-link build output {}",
                destination.display()
            )));
        }
        Ok(metadata) if metadata.is_dir() => {
            fs::remove_dir_all(destination)
                .map_err(|error| WarpError::io("replace build output directory", error))?;
        }
        Ok(_) => {
            let _ = fs::remove_dir_all(&staging);
            return Err(WarpError::new(format!(
                "build output {} exists but is not a directory",
                destination.display()
            )));
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => return Err(WarpError::io("inspect build output directory", error)),
    }

    fs::rename(&staging, destination)
        .map_err(|error| WarpError::io("materialize build output directory", error))
}

fn collect_files(root: &Path, directory: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
    let mut entries = fs::read_dir(directory)
        .map_err(|error| WarpError::io("read build output directory", error))?
        .map(|entry| {
            entry
                .map(|entry| entry.path())
                .map_err(|error| WarpError::io("read build output entry", error))
        })
        .collect::<Result<Vec<_>>>()?;
    entries.sort();

    for path in entries {
        let kind = fs::symlink_metadata(&path)
            .map_err(|error| WarpError::io("inspect build output entry", error))?
            .file_type();
        if kind.is_symlink() {
            return Err(WarpError::new(format!(
                "build output contains an unsupported symbolic link: {}",
                path.display()
            )));
        }
        if kind.is_dir() {
            collect_files(root, &path, files)?;
        } else if kind.is_file() {
            path.strip_prefix(root)
                .map_err(|_| WarpError::new("build output escaped its declared directory"))?;
            files.push(path);
        }
    }
    Ok(())
}

fn unpack_directory(bundle: &Path, destination: &Path) -> Result<()> {
    let mut input = File::open(bundle)
        .map_err(|error| WarpError::io("open cached build output bundle", error))?;
    let mut magic = vec![0; BUNDLE_MAGIC.len()];
    input
        .read_exact(&mut magic)
        .map_err(|error| WarpError::io("read cached build output bundle", error))?;
    if magic != BUNDLE_MAGIC {
        return Err(WarpError::new(
            "cached build output has an invalid bundle header",
        ));
    }
    let count = read_u64(&mut input)?;
    if count == 0 || count > MAX_ENTRIES {
        return Err(WarpError::new(
            "cached build output has an invalid file count",
        ));
    }

    for _ in 0..count {
        let path_length = read_u64(&mut input)?;
        let size = read_u64(&mut input)?;
        if path_length == 0 || path_length > MAX_PATH_LENGTH {
            return Err(WarpError::new(
                "cached build output contains an invalid path length",
            ));
        }
        let mut path_bytes = vec![0; path_length as usize];
        input
            .read_exact(&mut path_bytes)
            .map_err(|error| WarpError::io("read cached build output path", error))?;
        let path = String::from_utf8(path_bytes).map_err(|error| {
            WarpError::new(format!("cached build output path is not UTF-8: {error}"))
        })?;
        let relative = Path::new(&path);
        if relative.is_absolute()
            || relative
                .components()
                .any(|component| !matches!(component, Component::Normal(_)))
        {
            return Err(WarpError::new(format!(
                "cached build output contains an unsafe path: {path:?}"
            )));
        }

        let output_path = destination.join(relative);
        let parent = output_path
            .parent()
            .ok_or_else(|| WarpError::new("cached build output path has no parent"))?;
        fs::create_dir_all(parent)
            .map_err(|error| WarpError::io("create cached build output directory", error))?;
        let mut output = OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&output_path)
            .map_err(|error| WarpError::io("create cached build output file", error))?;
        let mut limited_input = Read::take(&mut input, size);
        let copied = std::io::copy(&mut limited_input, &mut output)
            .map_err(|error| WarpError::io("restore cached build output file", error))?;
        if copied != size {
            return Err(WarpError::new("cached build output ended unexpectedly"));
        }
    }

    let mut trailing = [0];
    if input
        .read(&mut trailing)
        .map_err(|error| WarpError::io("validate cached build output bundle", error))?
        != 0
    {
        return Err(WarpError::new("cached build output contains trailing data"));
    }
    Ok(())
}

fn create_staging_directory(destination: &Path) -> Result<PathBuf> {
    let parent = destination
        .parent()
        .ok_or_else(|| WarpError::new("build output directory has no parent"))?;
    fs::create_dir_all(parent)
        .map_err(|error| WarpError::io("create build output parent directory", error))?;
    let name = destination
        .file_name()
        .ok_or_else(|| WarpError::new("build output directory has no name"))?
        .to_string_lossy();
    for _ in 0..16 {
        let sequence = NEXT_RESTORE.fetch_add(1, Ordering::Relaxed);
        let path = parent.join(format!(
            ".{name}-warp-restore-{}-{sequence}",
            std::process::id()
        ));
        match fs::create_dir(&path) {
            Ok(()) => return Ok(path),
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(error) => {
                return Err(WarpError::io(
                    "create build output staging directory",
                    error,
                ))
            }
        }
    }
    Err(WarpError::new(
        "could not allocate a build output staging directory",
    ))
}

fn write_u64(writer: &mut impl Write, value: u64) -> Result<()> {
    writer
        .write_all(&value.to_le_bytes())
        .map_err(|error| WarpError::io("write build output bundle", error))
}

fn read_u64(reader: &mut impl Read) -> Result<u64> {
    let mut bytes = [0; 8];
    reader
        .read_exact(&mut bytes)
        .map_err(|error| WarpError::io("read build output bundle", error))?;
    Ok(u64::from_le_bytes(bytes))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_TEST: AtomicU64 = AtomicU64::new(0);

    #[test]
    fn bundles_and_restores_nested_files_without_following_symlinks() {
        let root = std::env::temp_dir().join(format!(
            "warp-bundle-test-{}-{}",
            std::process::id(),
            NEXT_TEST.fetch_add(1, Ordering::Relaxed)
        ));
        let source = root.join("source");
        let bundle = root.join("output.bundle");
        let destination = root.join("restored");
        fs::create_dir_all(source.join("assets")).unwrap();
        fs::write(source.join("index.html"), "<h1>Warp</h1>").unwrap();
        fs::write(source.join("assets/site.js"), "console.log('warp')").unwrap();

        pack_directory(&source, &bundle).unwrap();
        restore_directory(&bundle, &destination).unwrap();
        assert_eq!(
            fs::read(destination.join("index.html")).unwrap(),
            b"<h1>Warp</h1>"
        );
        assert_eq!(
            fs::read(destination.join("assets/site.js")).unwrap(),
            b"console.log('warp')"
        );

        fs::remove_dir_all(root).unwrap();
    }
}
