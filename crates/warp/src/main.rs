mod build;
mod cache;
mod cli;
mod digest;
mod error;
mod fingerprint;

use std::process::ExitCode;

fn main() -> ExitCode {
    match cli::run(std::env::args_os().skip(1)) {
        Ok(message) => {
            println!("{message}");
            ExitCode::SUCCESS
        }
        Err(error) => {
            eprintln!("warp: {error}");
            ExitCode::FAILURE
        }
    }
}
