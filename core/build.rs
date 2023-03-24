use deno_core::{JsRuntime, RuntimeOptions};

use std::env;
use std::path::PathBuf;

fn main() -> Result<(), anyhow::Error> {
    let o = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let snapshot_path = o.join("JS_SNAPSHOT.bin");
    let options = RuntimeOptions {
        will_snapshot: true,
        ..Default::default()
    };
    let isolate = JsRuntime::new(options);

    let snapshot = isolate.snapshot();
    let snapshot_slice: &[u8] = &snapshot;
    println!("Snapshot size: {}", snapshot_slice.len());
    std::fs::write(&snapshot_path, snapshot_slice).unwrap();
    println!("Snapshot written to: {} ", snapshot_path.display());

    tonic_build::configure()
        .include_file("_include.rs")
        .compile_well_known_types(true)
        .compile(
            &[
                "../schemas/build/warp/tricorder.proto",
                "../schemas/build/warp/common.proto",
            ],
            &["../"],
        )?;

    Ok(())
}
