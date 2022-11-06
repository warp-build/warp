use deno_core::JsRuntime;
use deno_core::RuntimeOptions;

use std::env;
use std::path::PathBuf;

fn main() -> Result<(), anyhow::Error> {
    let o = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let snapshot_path = o.join("JS_SNAPSHOT.bin");
    let options = RuntimeOptions {
        will_snapshot: true,
        ..Default::default()
    };
    let mut isolate = JsRuntime::new(options);

    let snapshot = isolate.snapshot();
    let snapshot_slice: &[u8] = &*snapshot;
    println!("Snapshot size: {}", snapshot_slice.len());
    std::fs::write(&snapshot_path, snapshot_slice).unwrap();
    println!("Snapshot written to: {} ", snapshot_path.display());

    tonic_build::compile_protos("../../schemas/build/warp/codedb.proto")?;

    Ok(())
}
