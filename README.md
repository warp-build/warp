# Warp

Warp is a build system that discovers Rust workspaces through Cargo metadata,
plans package targets as a dependency graph, and invokes `rustc` directly for
workspace code. Cargo remains the source of truth for Rust package metadata and
third-party dependencies.

## Build Warp

```bash
RUSTC="$(rustup which rustc --toolchain stable)" \
  CARGO_TARGET_DIR=_build/cargo/stable \
  rustup run stable cargo build --workspace
cargo build --bin warp
./target/debug/warp build
```

Warp places workspace outputs under `_build/<compilation-target-triple>/<profile>/`,
mirroring package paths. A selected executable is promoted to that build
directory's `bin/` folder. Client-specific choices live in the workspace's
`warp.toml`; this repository selects its `warp` binary by default, and
`--bin` overrides that choice. Workspace discovery and source snapshots honor
nested `.gitignore` patterns; `_build` and `.git` are always excluded, and
`exclude-directories` adds client-specific directory exclusions. The default
action/CAS cache is per-user and uses the operating system's cache directory
(on macOS, `~/Library/Caches/warp`). `--cache` and `--output` override the cache and
promoted executable paths. Builds print a `Building` line, per-target `OK` or
`ERR` events (including elapsed time for cache hits), and a final elapsed-time
summary. Status labels are green/red in an interactive terminal and respect
`NO_COLOR`.

Positional arguments are build labels, not compilation target triples. With no
labels, `warp build` uses `BuildScope::All`: it recursively discovers and builds
every supported Cargo/Rust target and every locked Bun package target in the
workspace. Discovery honors nested `.gitignore` rules and configured directory
exclusions. A `Label` resolves graph roots; a `BuildTarget` is a labeled
build-graph node; a `CompilationTargetTriple` is passed to `rustc` and selects
platform artifacts. A label can name a Cargo package directory or package name,
a Bun package directory, or a Cargo `package::target`; multiple labels select
multiple graph roots. Cargo roots include their workspace dependencies, while
Bun installs the dependencies recorded by its lockfile:

```bash
./target/debug/warp build ./crates/warp-rules ./crates/warp-cli -x macos
```

`-x` selects the compilation target triple (or an alias configured by
`warp-toolchains.toml`), while `--profile` selects the build profile. The
repository's toolchain file lists supported triples and selects the Rust
`stable` channel. Cargo dependencies for that channel must be built into
`_build/cargo/stable` with that channel's `rustc`; Warp uses the channel-specific
directory to avoid mixing artifacts from different compiler distributions.
Other toolchains can be declared there as they are implemented.

Before building a workspace that has third-party dependencies, build those
dependencies with Cargo for the same target and profile, for example:

```bash
RUSTC="$(rustup which rustc --toolchain stable)" \
  CARGO_TARGET_DIR=_build/cargo/stable \
  rustup run stable cargo build --target aarch64-apple-darwin --release
./target/debug/warp build ./crates/warp-cli -x aarch64-apple-darwin --profile release
```

Warp supports target-specific linker and Rust flags through Cargo-style
`CARGO_TARGET_<TRIPLE>_LINKER` and `CARGO_TARGET_<TRIPLE>_RUSTFLAGS` environment
variables. Rust workspace build scripts (`build.rs`) are not yet supported.

A standalone Rust source can also be built by passing its path:

```bash
./target/debug/warp build src/main.rs -x aarch64-apple-darwin
```

## Build the Warp website

The React/Vite site in `sites/warp.leostera.dev` uses Bun 1.4.2, pinned in its
`package.json`. Install that Bun version with the installer at [bun.sh](https://bun.sh):

```bash
curl -fsSL https://bun.sh/install | bash -s "bun-v1.4.2"
```

Then install the site dependencies and start its development server:

```bash
cd sites/warp.leostera.dev
bun install --frozen-lockfile
bun run dev
```

Warp can also build a locked Bun package directly from the repository root:

```bash
./target/debug/warp build ./sites/warp.leostera.dev
```

A Bun target needs `package.json` with a non-empty `scripts.build` entry and a
`bun.lock` (or `bun.lockb`). Warp honors an exact `packageManager: "bun@..."`
pin, installs dependencies with `bun install --frozen-lockfile`, and runs
`bun run build` in an isolated source snapshot. It caches the declared output
(`dist` for a regular package, `.cloudflare/output` for a package with
`cloudflare.config.ts`) under the normal `_build/<target>/<profile>/` tree. Warp
builds but does not deploy the site; use `bun run deploy` from the site directory
to deploy with Cloudflare's `cf` CLI.

## Architecture

The workspace separates the CLI, server, protocol, build engine, JavaScript
build language, and content-addressed store into independent crates. The
previous implementation is preserved locally under [`_legacy`](_legacy) for
architectural reference.
