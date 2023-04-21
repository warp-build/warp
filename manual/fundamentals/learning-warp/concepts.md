# Concepts

### Warp Phases

There are 3 main phases to any Warp execution: resolve, plan, execute.

1. In the **resolve** or **resolution** phase, Warp goes from a Goal and some Targets into Signatures.
2. In the **planning** phase, we use the Rule Engine to plan our Signatures.
3. In the **execution** phase, we execute our planned Signatures only if there are new things to be done.

### Workspace

A Warp workspace is any folder that is also a Git repository and includes a Warpfile.

Warpfiles serve two purposes: 1) indicates to Warp that this is a warp-ready workspace, 2) provides workspace-specific configurations. If the default configurations are sufficient, the Warpfile is as simple as: 
```json
{
  "workspace": {
    "name": "workspace-name"
  }
}

```
Note that the Warpfile should exist at the root of the workspace.

### Goal

A **Goal** in Warp represents a high-level user intent. It answers the question "What does the user want to do in this execution?" – typically the answer is _build_, _test_, or _publish_.

### Targets

A **Target** in Warp is an object we operate on. Targets can be paths to a file, URLs, or aliases.&#x20;

The target `@all` is used to represent _every single target in the workspace_.

When calling the `warp` tool, a Target is usually specified after a goal:

```bash
$ warp build <target>
```

Here are some example targets:

* File targets
  * `warp build ./Cargo.toml`
  * `warp build ./packages/design-system/package.json`
* URL targets
  * `warp build https://rules.warp.build/toolchains/erlang`
  * `warp build https://github.com/warp-build/warp`

### Signatures
When you ask Warp to build something, Warp needs to understand how to build it. But writing Build files (in whatever format) is a drag: Makefiles get messy, BUILD.bazel files go out of date. This is why the team at warp believes strongly in **fully zero-config builds**, which we make possible through a concept we're calling Warp Signatures (yes, we like Star Trek 🖖, sue us).

A Warp Signature is a unique recipe for building a piece of source code at a point in time, that can be derived statically from analyzing the repository, and is updated automatically as the file changes.

Make a code change? No problem.

Warp understands how to update the build graph based on the semantics of your change. New import? New dependency. Removed call to library? Removed dependency.

### Rules

### Tricorders
