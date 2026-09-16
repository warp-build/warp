# Warp feasibility audit for an agent-oriented build system

Status: local working document. Nothing in this audit has been published.

## Scope

This first pass inventories the existing Warp repository, validates its current buildability, and assesses which architectural pieces are candidates for an in-place evolution. The target use cases are both:

- `cloudflare/fe/stratus`
- `cloudflare/ai-agents/cloudflare-agent`

The future system may introduce a new manifest/protocol. The assumed implementation shape is a Rust core with TypeScript/JavaScript adapters and agent-facing tooling, backed initially by local storage, GitLab CI, and an R2/S3-compatible remote cache.

## Repository snapshot

Checkout inspected:

```text
/Users/leostera/Developer/github.com/leostera/warp
```

Observed revision:

```text
a932470b feat(core): move to sha256 for integrity checks (#67)
```

The repository is a Rust workspace with these main areas:

- `core/`: build graph, planning, execution, stores, rules, workspace discovery, and tricorders.
- `cli/`: the `warp` command and human-oriented reporting.
- `rules/`: JavaScript rules and toolchains.
- `tricorders/`: language-specific analyzers, currently including Rust and BEAM-oriented tooling.
- `store/`: packaged tricorder/rule assets.
- `schemas/`: protocol/schema material.
- `docs/`: Docusaurus documentation.

Approximate implementation size from the checkout:

- Rust: 28,643 lines across `core`, `cli`, and `tricorders`.
- JavaScript rules: 3,499 lines.
- Repository disk usage: approximately 62 MB including the Git checkout and build metadata.

This is substantial enough to contain a real build engine, but large enough that a dependency upgrade or wholesale rewrite would be a project rather than a first refactor.

## Current validation

### Build and test baseline

The dependency graph and Deno integration were modernized locally to work with the available Homebrew Rust `1.98.1` compiler. The repository still declares `nightly-2023-04-11` in `rust-toolchain.toml`, but that toolchain is not installed in this environment.

```bash
cargo check --workspace
cargo test --workspace
```

Both commands now complete successfully. The workspace test run reports:

- `tricorder`: 11 unit tests plus integration and snapshot tests passing.
- `warp-core`: 86 unit tests passing.
- `warp-cli`: unit tests pass; the privileged `/warp`-volume E2E test is explicitly ignored unless run in its required environment.
- No test failures.

The local compatibility work included updating the lockfile, moving the Deno runtime to the compatible `deno_core 0.243`/`deno_console 0.130` API surface, adapting op and module-loader APIs, making analyzer test fixtures independent of the process working directory, and removing the analyzer's dependency on nightly-only `rustc -Z unpretty` expansion. Snapshot expectations were regenerated for direct stable-source analysis.

Warnings remain (deprecated Deno APIs, unused/dead code, and the legacy `cfg(shuttle)` configuration), so this is a runnable baseline rather than a clean-warning baseline.

### Important implication

The next modernization task should make the toolchain policy explicit and reproducible:

1. Choose and pin a supported Rust toolchain rather than relying on the unavailable 2023 nightly.
2. Replace remaining deprecated Deno APIs and remove legacy warnings incrementally.
3. Preserve the hash and artifact golden tests while changing scheduling/execution behavior.

## Architecture inventory

### `core/model`: strongest reusable foundation

Important types include:

- `Target`: alias, local filesystem, or remote target.
- `Signature`: named rule plus target and compile/runtime dependencies.
- `Task`: goal/target/signature identity used by the scheduler.
- `ExecutableSpec`: planned actions, sources, outputs, environment, dependencies, and artifact hash.
- `ArtifactManifest`: materialized outputs and metadata.
- `ExecutionEnvironment`: host/toolchain context.

This is the clearest conceptual seam for an agent build protocol. The existing types already distinguish declaration, planning, and execution sufficiently to support explainable plans.

**Assessment: salvage directly in principle; redesign the serialization and identity contract before exposing it as a stable protocol.**

### `core/resolver` and `core/planner`: reusable phase split, too coupled internally

The resolver turns targets into signatures. The planner expands dependencies, invokes rule execution, discovers toolchains, resolves source chunks, and creates an `ExecutableSpec`.

This phase split is valuable for:

- affected-task planning;
- dry-run explanations;
- approval before execution;
- agent-readable dependency closure;
- resuming interrupted plans.

The implementation is coupled to the current rule/tricorder model and uses in-memory registries extensively. A future protocol should make the plan a serializable value rather than an implicit state distributed among registries.

**Assessment: salvage the phases and graph concepts; refactor behind stable task/plan interfaces.**

### `core/worker`: useful local graph execution, insufficient policy model

The worker pool provides dependency-aware execution and concurrent workers. It has a clean executor/worker boundary and emits workflow events.

Missing first-class concepts for the intended use case include:

- cancellation and deadlines;
- retries and failure policy;
- resource classes and budgets;
- workspace isolation;
- explicit filesystem/network/secret capabilities;
- approval gates;
- resumability and event replay;
- side-effect classification.

**Assessment: salvage scheduling mechanics after adding a task execution policy model.**

### `core/executor`: useful boundary, local execution is not agent-safe yet

`Executor` exposes completion, validation, and missing-dependency flows. This is a good interface for pluggable local/remote executors.

The future default executor must not inherit ambient command execution assumptions. It should accept a fully resolved execution envelope containing:

- working directory;
- argv, not only shell text;
- declared inputs/outputs;
- environment allowlist;
- capabilities;
- resource limits;
- timeout/cancellation token;
- provenance context.

**Assessment: high-value interface; local executor requires a security redesign.**

### `core/store`: useful local artifact model, replace backend assumptions

The local store and manifests are directly relevant to content-addressed caching. The public store and package-manifest flow are specialized around Warp's historical distribution model and host triples.

The future store should abstract over:

- local filesystem CAS;
- R2/S3 object storage;
- GitLab CI cache/artifacts;
- optional peer or shared cache.

A cache hit should verify a manifest and output digest, not merely find a directory by hash. Remote access should be capability-controlled and separate from task planning.

**Assessment: salvage manifest/CAS concepts; replace the store interface and remote distribution path.**

### `core/rules` and Deno/V8 integration: largest liability

Rules are JavaScript files loaded from local rule directories or downloaded from a configured public rule store. The core depends directly on Deno/V8-related crates.

This conflicts with the desired safety model:

- executable rules are supply-chain inputs;
- network retrieval can occur as part of planning;
- adapter identity and provenance are underspecified;
- permissions are not represented as task capabilities;
- the dependency footprint is disproportionately large.

**Assessment: do not preserve as the primary extension mechanism. Replace with versioned adapters and an explicit protocol. Retain a compatibility adapter only if migration requires it.**

### `core/tricorder`: strong plugin idea, wrong packaging coupling

Tricorders provide language-specific analysis through an out-of-process/service-oriented boundary, including readiness, AST/signature generation, and dependency preparation.

That maps well to TypeScript/workspace and agent-evaluation adapters. However, the current manager installs artifacts from the Warp store, starts a binary, finds a port, and connects over gRPC. This is too much policy in one abstraction.

A modern adapter should declare:

- protocol version;
- adapter digest/version;
- supported workspace signals;
- required capabilities;
- discovered tasks and their inputs;
- structured explanations.

**Assessment: salvage the process isolation and adapter concept; replace installation/discovery/transport coupling.**

### `core/events`: important for agent usability

The event channel and structured workflow events are one of the most underappreciated reusable pieces. Agents need machine-readable progress, cache decisions, subprocess lifecycle, evidence locations, and approval requests.

The current events should evolve toward a durable event schema with execution IDs and replay support.

**Assessment: salvage and expand.**

### `cli`: useful command boundary, not yet machine-first

The CLI supports build/test/run/bootstrap/setup/pack flows and human-oriented reporters. An agent-oriented CLI needs stable JSON output for:

- discovery;
- planning;
- explanations;
- execution events;
- evidence;
- approvals;
- cache diagnostics.

Human output can remain, but it should be a rendering of the same structured event/plan model.

**Assessment: salvage command concepts; add a stable machine protocol before adding agent features.**

## Content identity assessment

`ExecutableSpecBuilder::hash_and_build` currently combines action data, shell environment entries, dependency artifact hashes, output paths, source paths, source contents, and host triple for architecture-dependent tasks.

This is directionally correct, but not sufficient as a long-term reproducibility contract because identity also needs to account for:

- canonical command/argv representation;
- executable and toolchain versions;
- normalized working directory;
- adapter/protocol/policy versions;
- declared network and external-resource inputs;
- lockfiles and package-manager metadata;
- file mode/symlink semantics;
- model/provider/dataset/evaluator versions for agent evaluations.

The implementation also relies on Rust `Hash` behavior for action hashing. That is appropriate for internal code but not a stable cross-language wire identity. A future task digest should derive from a canonical serialized envelope and explicit input digests.

## Preliminary salvage matrix

| Area | Salvage | Modernization needed | Recommendation |
|---|---:|---:|---|
| Target/signature/task concepts | High | Medium | Preserve concepts; define stable protocol types |
| Executable specs | High | High | Keep model; redesign canonical identity |
| Dependency planning | High | Medium | Extract graph/planning core from rule runtime |
| Local worker scheduling | Medium-high | High | Add policy, cancellation, isolation, resumability |
| Executor interface | High | High | Make execution envelopes capability-based |
| Local CAS/manifests | High | Medium-high | Generalize to local/R2/S3/GitLab backends |
| Public store/package distribution | Low-medium | High | Replace with generic CAS and signed manifests |
| JavaScript/Deno rules | Low | High | Compatibility mode only; not future core |
| Tricorders | Medium-high | High | Keep adapter/process idea; replace packaging protocol |
| Events/reporter | High | Medium | Make event schema durable and machine-first |
| CLI | Medium-high | Medium | Add JSON protocol and approval/evidence commands |
| Current dependency/toolchain setup | Low | High | Modernize before feature expansion |

## Current feasibility judgment

Warp is salvageable as an **evolutionary foundation**, but not as a drop-in modern build system. The highest-value path is to preserve the graph/planning/artifact ideas while introducing a new protocol boundary around them.

The most important early risk is not algorithmic. It is architecture coupling: rules, remote distribution, language analysis, execution, and caching currently cross boundaries that an agent-safe system needs to keep separate.

The next investigation should characterize the two target repositories and define the smallest common task protocol they can both express, before modifying Warp's core types.
