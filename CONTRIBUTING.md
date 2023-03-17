# Contribution Guide

## Project Setup

Before Warp reaches the bootstrapping stage, we have a few prerequisites:

* the latest Rust stable toolchain – install it with [rustup](https://rustup.rs/)

* a working Elixir 1.13 installation

* a working Protobuf compiler – install with `brew install protobuf` or `apt
  install protobuf`

Then start by running `make` to get an initial build of the project.

> NOTE: if `make` fails because we're missing some toolchain, this is a bug.


## Bootstrapping a Tricorder

Sometimes we need to build a Tricorder without having the last version of
itself around. For those cases, we have the `warp bootstrap` command.

Before running `warp bootstrap`, make sure that the tricorder is self-contained
and requires no additional system dependencies.

For example, in the BEAM tricorder we have a `bootstrap.sh` script that will
call `protoc` to generate the Elixir code. If you have changed the
`schemas/**/*.proto` files, you will need to call this script again before
calling `warp bootstrap`, because `warp` will not have access to your globally
installed `protoc` compiler.

## Submitting Contributions

We expect everyone to go through a learning curve when they start contributing,
and during this time we want to see Pull Requests being opened that are well
scoped, and small enough to be quick to review and merge.

In an ideal scenario, your first few pull requests will build on top of each
other instead of having a single PR that implements an entire new feature.
