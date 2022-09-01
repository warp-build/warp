use deno_core::JsRuntime;
use deno_core::RuntimeOptions;

use std::env;
use std::path::PathBuf;

fn main() {
    let o = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let snapshot_path = o.join("JS_SNAPSHOT.bin");
    let options = RuntimeOptions {
        will_snapshot: true,
        extensions: vec![deno_console::init()],
        ..Default::default()
    };
    let mut isolate = JsRuntime::new(options);

    let snapshot = isolate.snapshot();
    let snapshot_slice: &[u8] = &*snapshot;
    println!("Snapshot size: {}", snapshot_slice.len());
    std::fs::write(&snapshot_path, snapshot_slice).unwrap();
    println!("Snapshot written to: {} ", snapshot_path.display());
}
