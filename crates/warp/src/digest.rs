use crate::error::{Result, WarpError};
use std::io::{Read, Write};
use std::process::{Command, Stdio};

#[cfg(test)]
pub fn sha256(input: &[u8]) -> Result<String> {
    sha256_with(|writer| {
        writer
            .write_all(input)
            .map_err(|error| WarpError::io("send data to SHA-256 utility", error))
    })
}

pub fn sha256_reader(mut input: impl Read) -> Result<String> {
    sha256_with(|writer| {
        std::io::copy(&mut input, writer)
            .map(|_| ())
            .map_err(|error| WarpError::io("send data to SHA-256 utility", error))
    })
}

pub fn sha256_with(write_input: impl FnOnce(&mut dyn Write) -> Result<()>) -> Result<String> {
    let (program, arguments) = digest_program()?;
    let mut child = Command::new(&program)
        .args(arguments)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|error| WarpError::io("start SHA-256 utility", error))?;

    let mut stdin = child.stdin.take().expect("piped stdin is configured");
    let write_result = write_input(&mut stdin);
    drop(stdin);
    write_result?;

    let output = child
        .wait_with_output()
        .map_err(|error| WarpError::io("wait for SHA-256 utility", error))?;
    if !output.status.success() {
        return Err(WarpError::new(format!(
            "SHA-256 utility failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        )));
    }

    let stdout = String::from_utf8(output.stdout).map_err(|error| {
        WarpError::new(format!("SHA-256 utility returned invalid UTF-8: {error}"))
    })?;
    let digest = stdout
        .split_whitespace()
        .next()
        .ok_or_else(|| WarpError::new("SHA-256 utility returned no digest"))?;
    if digest.len() != 64 || !digest.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        return Err(WarpError::new(format!(
            "SHA-256 utility returned malformed digest: {digest:?}"
        )));
    }

    Ok(digest.to_ascii_lowercase())
}

fn digest_program() -> Result<(String, Vec<&'static str>)> {
    for (program, arguments) in [("shasum", vec!["-a", "256"]), ("sha256sum", vec![])] {
        match Command::new(program).arg("--version").output() {
            Ok(_) => return Ok((program.to_string(), arguments)),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => continue,
            Err(error) => return Err(WarpError::io("find SHA-256 utility", error)),
        }
    }

    Err(WarpError::new(
        "no supported SHA-256 utility found (install shasum or sha256sum)",
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sha256_matches_standard_vectors() {
        assert_eq!(
            sha256(b"").unwrap(),
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        );
        assert_eq!(
            sha256(b"abc").unwrap(),
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        );
    }
}
