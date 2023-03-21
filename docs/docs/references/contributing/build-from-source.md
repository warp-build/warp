---
sidebar_position: 1
---

# Building `warp` from Source

Below are instructions on how to build warp from source. If you just want to use
warp you can download a prebuilt executable (more information in the
[`Getting Started`](../../getting-started/installing.md)
chapter).

## Cloning the Repository

Clone on Linux or Mac:

```shell
git clone https://github.com/warp-build/warp.git
```

## Prerequisites

warp requires the latest stable release of Rust.

1. [Install Rust](https://www.rust-lang.org/tools/install). Check that
Rust installed/updated correctly:

```
rustc -V
cargo -V
```

## Building warp

The easiest way to build warp is to call:

```
make build
```

## Testing warp

You can run tests on the entirety of warp by calling:

```
make test
```
