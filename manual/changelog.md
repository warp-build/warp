---
description: Warp is improving every day!
---

# Changelog

### v0.0.68 (2023/01/12)

This is our biggest release yet and we're super excited to share it with you 🤩

#### ⚡️ **Zero Configuration Builds**

When you ask Warp to build something, Warp needs to understand _how_ to build it. But writing Build files (in whatever format) is a drag: Makefiles get messy, BUILD.bazel files go out of date. In this release of Warp we introduce fully zero-config builds through a new concept we're calling **Warp Signatures** (yes, we like Star Trek 🖖, sue us).&#x20;

A Warp Signature is a unique recipe for building a piece of source code at a point in time, that can be derived statically from analyzing the repository, and is updated automatically as the file changes.

Make a code change? No problem.

Warp understands how to update the build graph based on the semantics of your change. New import? New dependency. Removed call to library? Removed dependency.

To bootstrap a repository, we've built a new command: warp lift. This takes care of flattening all dependencies, and analyzing all the code you're relying on, to make signature generation possible and fast.

#### 🏁 **Semantic Test Caching**

Picture a test suite. When it starts out, it usually has just a handful of tests, maybe a few of them are big but its no big deal to rerun them all. If it was cached by file changes, then you will have to rerun this entire test suite any time a single test in it changes.

As your test suite grows, you could split it up into several files, but as with any other code this isn't free.

Starting with support for the Erlang ecosystem, Warp can now cache results going as deeply as to AST-diff test cases within single test suites/files. This allows existing large test suites to be cached with excellent granularity, and takes the burden away from refactors that only are required with coarser-granularity caching.

#### 🏃 **Runtime Dependencies**

Usually building A requires B and C (its dependencies) to exist and be built beforehand. However, some other times we can build A without having built B and C, as long as we have B and C _at runtime_. Dynamically linked libraries, and all language supporting late-binding (such as Erlang and Elixir), support this.

This feature dramatically speeds up compilation in large projects since we can parallelize a lot more work, while still being able guarantee that the build is correct.

#### 🔧 **Warp Analyzer Protocol**

to make interfacing with new ecosystems easier in the long run, we're introducing an alpha version of a protocol that will allow us to plug in different LSPs directly into Warp for analysis of build targets. This is currently implemented as a gRPC/Protobuf service interface, and it powers Warp's dependency resolution and signature generation processes.

More about this soon on our to-be-released docs page!

#### 📜 **Hinting**

Ssometimes the analyzers and Warp's heuristics can't figure out how to build something. Turns out, builds are hard 🙃. For example, Warp can detect Erlang tests and dependencies across all your source files, but it can't figure out what fixtures you are going to read from disk. To figure this out, we'll need to run your tests and instrument them (spoiler alert: check back on this later).

So for now we allow a small .warp file to be dropped near whichever file you want to add more dependencies (or runtime dependencies) to. Like this:

./src/verl.erl ./src/verl.erl.warp <- this is the hints file

In it you can add some JSON to describe what targets this file provides, and which overrides you want to supply it:

```json
{
  "*": {// this means 'add to all targets from this file'
    "deps": [ "./src/fixtures/my_fixture.txt" ]
  }
}
```

Looking forward to sharing more in the next couple of months! So if you've got any feedback feel free to drop it in the box below, and in our [Public Roadmap](https://warp.productlane.io/roadmap).

### v0.0.44  (2022/10/21)

⚡️ **Instant Setup**: new and existing projects can now be set up incredibly quickly with warp init. This flow will ask you to choose whichever languages and tools you are going to use (\`ruby\`, erlang, make, etc), and will prepare your workspace for it.

If your project already has sources in it, we will also run a _lifter_.&#x20;

A _lifter_ is a tool that analyzes an existing project and knows how to:

1\. build a dependency graph for Warp

2\. detects all the necessary dependencies for your project

By the end of this process you should be able to run warp build and warp test to incrementally build and test your project.

{% hint style="info" %}
**NOTE**: static analysis is _hard_ and existing projects come in all shapes and sizes, so while we can guarantee that the lifters will make the initial setup much nicer, sometimes they just make silly mistakes. Like treating all your test fixtures as actual code. Keep this in mind!
{% endhint %}

Check out this video where we set up warp on the verl repository in 20 seconds and get incremental builds of \~70ms: [https://www.youtube.com/watch?v=f6ngarxsdUE](https://www.youtube.com/watch?v=f6ngarxsdUE)

📦 **Dependency Management**: now warp helps you keep your dependencies tight and fast! We have a clever tiny architecture that helps us use any ecosystem's best tools to figure out where a dependency is, and how to build it. When your build targets now depend on URLs like [https://hex.pm/packages/phoenix](https://hex.pm/packages/phoenix), you will see a new line in the command line that reads:

```bash
Resolving "https://hex.pm/packages/phoenix"
```

This will also recursively get all the dependencies that are necessary.  Remote-caching is currently enabled for these dependencies, so if someone else has already built Phoenix, noone else will needs to do it again!

Lastly, you will find that the lifters write into a file called .warp/Dependencies.json where all of the dependency versions are specified.

📜 **Fewer Files, and all JSON**

**W**while not a feature, we have moved away from TOML and onto JSON. All future configuration will happen with a variant of JSON that supports comments in it.

We are also working towards moving away from Build files to provide an even cleaner experience. More news soon!

🐛 **Bugfixes**:

\* Fixed concurrency bugs and improved store locks

\* Workspaces can now depend on themselves (useful if you're providing tools that you also want to run in the same repo)

\* Fixed stack overflows when importing JS modules due to a large number of Rust Futures

### v0.0.33 (2022/10/03)

🖇️ **Remote Workspaces**: now your workspace can reference things that live in _other repos_. This is super useful for 2 things: polyrepo setups, and to execute stuff that someone else has built but that you don't want to set up locally by yourself.

We are using Remote Workspaces to execute _lifters_ to set up projects automatically, without you ever having to install anything on your machine. That's right, if we detect an Elixir file, we'll run the Elixir lifter tool automagically. Zero setup needed.

You can run it too! Try warp run [https://tools.warp.build/erlang/lifter2](https://tools.warp.build/erlang/lifter2) -- help after adding the remote workspace to your Workspace.toml:

```toml
[remote_workspaces]
"tools.warp.build" = {
    github = "warp-build/tools.warp.build",
    git_ref = "29cc8f513d6648ecba1d67ec84e337642c303289"
}
```

PS: if you need a workspace, you can always run warp init in an empty directory, but that command is still not ready for prime time 🙈&#x20;

✏️  **Simpler Labels**: labels had always acted a little weird, but primarily they were hard to get autocompletion on in the shell, and tiring to update when moving code around.

Now you can use actual paths for things! So warp build ./path/to/my/file.ex is a valid label to a file. Same in your targets, when specifying dependencies you can drill down by relative paths:

```toml
[[elixir_library]]
name = "user_api"
deps = ["./lib/users:lib"]
```

🐞 **Bugfixes**:

\* WARP-73 - warp will now always clean up old outputs from the warp-outputs folder when replacing them with a newer set of outputs

\* warp will now always create a /warp/home dir for all the sandboxed tools to rely on

### v0.0.22 (2022/09/22)

Small change that caches the rules based on the URLs (treating the URLs as uinque enough) rather than hashing the URL paths. This means that inspecting and tinkering with them becomes a lot easier :star\_struck:

For example, before you had to go to `/warp/rules/6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e-pkgs.warp.build/node.js` to find the `node.js` toolchain.

Now you can see the path structure much more clearly:

```bash

[  96]  /warp/rules
└── [  96]  https
    └── [ 128]  rules.warp.build
        ├── [  58]  rules
        │   ├── [ 595]  archive.js
        │   ├── [1.2K]  bsb_library.js
        │   ├── [ 725]  cargo_binary.js
        │   ├── [ 642]  cargo_library.js
        │   ├── [ 763]  deno_executable.js
        │   ├── [ 475]  deno_library.js
        │   ├── [ 223]  dummy.js
        │   ├── [ 480]  eex_library.js
        │   ├── [1.5K]  elixir_application.js
        │   ├── [ 540]  elixir_image.js
        │   ├── [3.1K]  elixir_library.js
        │   ├── [1.0K]  elixir_proto_library.js
        │   ├── [ 615]  elixir_release.js
        │   ├── [1.9K]  elixir_script.js
        │   ├── [1.1K]  erlang_application.js
        │   ├── [3.0K]  erlang_library.js
        │   ├── [2.2K]  erlang_proper_test.js
        │   ├── [ 882]  erlang_proto_library.js
        │   ├── [5.5K]  erlang_script.js
        │   ├── [2.7K]  erlang_test.js
        │   ├── [1.1K]  erlangmk_library.js
        │   ├── [ 257]  files.js
        │   ├── [ 650]  firefly_binary.js
        │   ├── [ 693]  fly_deploy.js
        │   ├── [ 665]  fly_launch.js
        │   ├── [   0]  gleam_library.js
        │   ├── [ 671]  gleam_package.js
        │   ├── [2.0K]  iex_shell.js
        │   ├── [1.2K]  make_library.js
        │   ├── [1.0K]  mix_escript.js
        │   ├── [3.1K]  mix_library.js
        │   ├── [1.3K]  mix_release.js
        │   ├── [ 552]  ngrok_tunnel.js
        │   ├── [ 589]  npm_library.js
        │   ├── [ 503]  otp_release.js
        │   ├── [ 644]  protobuf_library.js
        │   ├── [1.7K]  rebar3_library.js
        │   ├── [1022]  rescript_library.js
        │   ├── [ 786]  rust_binary.js
        │   ├── [ 589]  rust_test.js
        │   ├── [ 727]  terraform_apply.js
        │   └── [ 623]  webpack_bundle.js
        └── [  63]  toolchains
            ├── [1.7K]  cmake.js
            ├── [ 888]  deno.js
            ├── [2.3K]  elixir.js
            ├── [ 933]  elixir_protobuf.js
            ├── [2.2K]  erlang.js
            ├── [ 988]  erlang_protobuf.js
            ├── [1.2K]  firefly.js
            ├── [1.1K]  flyctl.js
            ├── [ 987]  git.js
            ├── [ 667]  gleam.js
            ├── [ 633]  minio.js
            ├── [ 383]  ngrok.js
            ├── [1.0K]  node.js
            ├── [1.2K]  openssl.js
            ├── [1.0K]  protobuf.js
            ├── [1.0K]  python.js
            ├── [ 974]  rebar3.js
            ├── [2.2K]  registry.json
            ├── [1.2K]  rescript.js
            ├── [1.0K]  ruby.js
            ├── [2.3K]  rust.js
            ├── [1015]  terraform.js
            └── [ 583]  webpack.js

```

The rule writing experience has a lot of room for improvement, since it's been only us writing them so far, but we'll put some docs on how to get up and running soon for those of you that want to customize your builds :relieved:

### v0.0.18 (2022/09/20)

In this release we got:

* Remote rules -- now Rules can be downloaded from http/https targets like [https://pkgs.warp.build/toolchains/erlang.js](https://pkgs.warp.build/toolchains/erlang.js) -- which makes it possible for us to iterate over them without having to re-release new versions of warp.
* Rule environment variables and PATH are automatically cascading across direct, transitive, and toolchain dependencies -- this means you can now write rules that _assume_ things like `mix` will be available, and warp will make sure to make it available
* Fix a bug where warp would hang on empty workspaces
* Much faster Label comparison and other performance optimizations
