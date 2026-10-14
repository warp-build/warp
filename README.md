# Warp

Warp is a build system that discovers Rust workspaces through Cargo metadata,
plans package targets as a dependency graph, and invokes `rustc` directly for
workspace code. Cargo remains the source of truth for Rust package metadata and
third-party dependencies.

## Build Warp

```bash
cargo build --bin warp
./target/debug/warp build
```

Warp places workspace outputs under `_build/<target-triple>/<profile>/`,
mirroring package paths. A selected executable is promoted to that build
directory's `bin/` folder. Client-specific choices live in the workspace's
`warp.toml`; this repository selects its `warp` binary by default, and
`--bin` overrides that choice. `exclude-directories` in that file controls workspace discovery and source
snapshots. The default action/CAS cache is per-user and uses the operating
system's cache directory (on macOS,
`~/Library/Caches/warp`). `--cache` and `--output` override the cache and
promoted executable paths. Builds print a `Building` line, per-target `OK` or
`ERR` events (including elapsed time for cache hits), and a final elapsed-time
summary. Status labels are green/red in an interactive terminal and respect
`NO_COLOR`.

Select a single Cargo package by directory or manifest path; Warp builds that
package and its workspace dependencies rather than every workspace target:

```bash
./target/debug/warp build ./crates/warp-store
```

Select a Rust target triple and profile explicitly when needed:

```bash
./target/debug/warp build --target aarch64-apple-darwin --profile release
```

Before building a workspace that has third-party dependencies, build those
dependencies with Cargo for the same target and profile, for example:

```bash
cargo build --target aarch64-apple-darwin --release
./target/debug/warp build --target aarch64-apple-darwin --profile release
```

Warp supports target-specific linker and Rust flags through Cargo-style
`CARGO_TARGET_<TRIPLE>_LINKER` and `CARGO_TARGET_<TRIPLE>_RUSTFLAGS` environment
variables. Rust workspace build scripts (`build.rs`) are not yet supported.

A standalone Rust source can also be built by passing its path:

```bash
./target/debug/warp build src/main.rs --target aarch64-apple-darwin
```

## Architecture

The workspace separates the CLI, server, protocol, build engine, JavaScript
build language, and content-addressed store into independent crates. The
previous implementation is preserved locally under [`_legacy`](_legacy) for
architectural reference.
