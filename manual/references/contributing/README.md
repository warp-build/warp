# 🧱 Contributing

We welcome and appreciate all contributions to Warp. This page serves as a helper to get you started on contributing.

### Projects

There are several projects that make up the Warp ecosystem. All of them live within the same monorepo at [warp-build/warp](https://github.com/warp-build/warp).

These projects have different scopes, use different programming languages, and have varying difficulty levels when it comes to contributions.

Here's a brief description of the projects to help you get started.

### [**warp-core**](https://github.com/warp-build/warp/tree/main/core)

This is the core project that implements all of the subsystems: resolution, archive management, caching, incremental execution, signature planning, etc.

If you want to fix a bug or add a new feature to `warp` this is the project you want to contribute to.

Languages: **Rust, JavaScript**

### [**warp-cli**](https://github.com/warp-build/warp/tree/main/cli)

This is the main project that provides the `warp` CLI.

If you want to fix a bug or improve the DX of running `warp` this is most likely the project you want to contribute to.

Languages: **Rust**, **JavaScript**

### [**tricorders/beam**](https://github.com/warp-build/warp/tree/main/tricorders/beam)

The BEAM Tricorder implements support for understanding and working with BEAM languages.

If you want to improve support for Elixir, Erlang, or Gleam, this is the project you want to contribute to.

Languages: **Elixir, Erlang, JavaScript.**

### [**tricorders/rust**](https://github.com/warp-build/warp/tree/main/tricorders/rust)

The Rust Tricorder implements support for understanding and working with Rust code.

If you want to improve support for Rust or Cargo, this is the project you want to contribute to.

Languages: **Rust, JavaScript.**

### [rules and toolchains](https://github.com/warp-build/warp/tree/main/rules)

Warp's collection of Rules and Toolchains implements support for several tools (like `git`) and teaches Warp how to do new things.&#x20;

In this project, you can find rules for building Rust binaries, Erlang libraries, archive tarballs, and more.

If you want to improve any language support, this is the project you want to contribute to.

Languages: **Rust, JavaScript.**
