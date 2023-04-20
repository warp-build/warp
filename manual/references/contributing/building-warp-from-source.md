# Building \`warp\` from source

Below are instructions on how to build warp from source. If you just want to use warp you can download a prebuilt executable (more information in the [Getting started](../../fundamentals/getting-started/) chapter).

## Cloning the Repository

Clone on Linux or Mac:

```shell
git clone https://github.com/warp-build/warp.git
```

## Prerequisites

warp requires a few toolchains:

* the latest stable release of Rust
* `deno` version `v1.32` or newer
* `elixir` version `1.14` with OTP 25
* a `protoc` compiler version `3.20` or newer

## Building warp

The easiest way to build `warp` is to call:

```
make build
```

## Testing warp

You can run tests on the entirety of `warp` by calling:

```
make test
```

## Installing warp locally

You can install `warp` locally by running

```
make install
```

## Cross-compilation

The `make release` target will help you cross-compile releases for macOS M1, macOS Intel, Linux ARM, and Linux Intel.

To use it you need to configure `.cargo`. For example, on macOS, you need the GCC cross-compilation toolchains. Your `$HOME/.cargo/config` would then look like this:

```toml
[target.x86_64-unknown-linux-gnu]
ar = "x86_64-unknown-linux-gnu-ar"
linker = "x86_64-unknown-linux-gnu-gcc"

[target.aarch64-unknown-linux-gnu]
ar = "aarch64-unknown-linux-gnu-ar"
linker = "aarch64-unknown-linux-gnu-gcc"
```

To install these toolchains, you can run:

```sh
brew tap messense/macos-cross-toolchains
brew install aarch64-unknown-linux-gnu
brew install x86_64-unknown-linux-gnu
```

If you have issues with the `python@3.11` version, you can edit the Brew bottle here, and set it to `python@3`:

```sh
# this will create a .bak backup file
sed -i'.bak' 's/python@3.11/python@3/' /opt/homebrew/Library/Taps/messense/homebrew-macos-cross-toolchains/aarch64-unknown-linux-gnu.rb
```

### &#x20;<a href="#cross-compilation" id="cross-compilation"></a>
