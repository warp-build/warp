---
sidebar_position: 2
---

# Supported Platforms

This is a living document that reflects the current state of support for
specific platforms (like the Erlang programming language) and tools (like git).

### Erlang

#### Toolchains

Toolchain versions 24, 25 and HEAD have been tested.

You can find recommended versions and hashes in the
[registry](https://github.com/warp-build/rules.warp.build/blob/main/toolchains/registry.json).

#### Rules

We can currently build several rules as seen in the
[Rulebook](/category/rulebook), including:

- [`erlang_library`](/rulebook/erlang/erlang_library) -- for low-level Erlang
  libraries that require no rebar3/make
- [`erlang_proto_library`](/rulebook/erlang/erlang_proto_library) -- for
  generating Erlang source code from a .proto file
- [`erlang_script`](/rulebook/erlang/erlang_script) -- for bundling Erlang
  binaries
- [`erlang_test`](/rulebook/erlang/erlang_test) -- for running Erlang CommonTest
  suites
- [`erlang_proper_test`](/rulebook/erlang/erlang_proper_test) -- for running
  PropEr tests
- [`erlangmk_library`](/rulebook/erlang/erlangmk_library) -- for building
  [erlang.mk](https://erlang.mk/) libraries
- [`rebar3_library`](/rulebook/erlang/rebar3_library) -- for building
  [rebar3](https://rebar3.org/) libraries
- [`otp_application`](/rulebook/erlang/otp_application) -- 🚧 for bundling OTP
  applications
- [`otp_release`](/rulebook/erlang/otp_release) -- 🚧 for bundling OTP releases

### Elixir

#### Toolchains

Toolchain versions 1.13, and 1.14 tested.

You can find recommended versions and hashes in the
[registry](https://github.com/warp-build/rules.warp.build/blob/main/toolchains/registry.json).

#### Rules

We can currently build several rules as seen in the
[Rulebook](/category/rulebook), including:

- [`elixir_library`](/rulebook/elixir/elixir_library) -- for low-level Elixir
  libraries that don't require `mix`
- [`elixir_proto_library`](/rulebook/elixir/elixir_proto_library) -- for
  generating Elixir source code from a .proto file
- [`eex_library`](/rulebook/elixir/eex_library) -- for bundling Elixir templated
  files
- [`elixir_script`](/rulebook/elixir/elixir_script) -- for bundling Elixir
  binaries
- [`mix_library`](/rulebook/elixir/mix_library) -- for building
  [mix](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
  libraries

---

## Experimental Support

### Cmake

### Deno

### Fly.io

### Git

### Gleam

### Make

### Minio

### Ngrok

### Node

### OpenSSL

### Protobuf

### Python

### Ruby

### Rust

### Terraform
