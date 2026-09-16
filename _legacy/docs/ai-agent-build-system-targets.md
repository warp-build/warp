# Target repository characterization

## `cloudflare/ai-agents/cloudflare-agent`

This is a pnpm workspace with `apps/*` and `packages/*`, currently orchestrated by Turborepo. The root defines 17 workspace package manifests and 5 Wrangler configurations.

The important existing graph policy is in `turbo.json`:

- `topo` establishes dependency order through `^topo`.
- `build` depends on upstream `build` and local `topo`.
- `deploy*` depends on `build` and `topo`, is explicitly uncached, and receives Cloudflare/Sentry credentials.
- `check:types` and `check:lint` depend on `topo` and restrict their inputs.
- `test` and `test:ci` are currently uncached.
- `eval` and `eval:tools` are uncached and pass model/provider credentials and model selection through the environment.

### What this tells us

1. **The task graph already has semantic categories.** Build, checks, tests, deploy, and eval are not interchangeable.
2. **Turborepo's environment model is useful but incomplete for agent safety.** `env` and `passThroughEnv` identify sensitive inputs, but they do not express capabilities, approval requirements, or secret non-disclosure.
3. **Current cache policy is conservative for tests/evals.** A future system should not blindly enable caching. It should define identity for datasets, models, provider settings, evaluator code, and external state first.
4. **`topo` is a compatibility concern.** A new protocol needs to express dependency ordering without requiring a synthetic command task.
5. **Wrangler configurations are natural adapter inputs.** Worker bindings, compatibility dates, generated types, and deployment targets should be discovered and included in task identity.

### Good initial pilot tasks

- Workspace/package graph discovery.
- Type checking and lint planning.
- Wrangler build/dry-run validation.
- SDK drift and generated-artifact checks.
- Explainable selection of affected tasks.
- Approval-aware deploy planning without executing deploys.

### Tasks requiring special treatment

- `dev`: persistent, non-cacheable, interactive.
- `deploy*`: side-effecting, credential-bearing, approval-required, never replayed from cache.
- `eval*`: structured evidence and score outputs; cache policy depends on model/provider/dataset identity.
- Worker tests: runtime-specific sandbox and fixture setup.

## `cloudflare/fe/stratus`

Stratus is substantially larger: 49 package manifests and a broad pnpm workspace with package, app, library, worker, test, and tooling subtrees. It has a root script surface spanning compilation, type definitions, linting, Jest/Vitest, Playwright, i18n generation, endpoint scanning, Medic analysis, publishing, previews, and deployment-related workflows.

The workspace configuration includes:

- many workspace glob families;
- catalogs and catalogs for React versions;
- TypeScript overrides and package patches;
- Node/pnpm-specific linker and hoisting policy;
- explicit package-manager security controls for install scripts;
- current-architecture-only package installation constraints.

### What this tells us

1. **Package discovery cannot rely on a single conventional workspace glob.** The adapter must honor `pnpm-workspace.yaml` exactly.
2. **The package graph is necessary but insufficient.** Many meaningful tasks are root scripts or scripts nested in application-specific tooling.
3. **Input declarations must include package-manager policy.** Lockfiles, workspace config, catalogs, overrides, patches, Node/pnpm versions, and relevant `.npmrc` settings affect outputs.
4. **The first Stratus integration should be selective.** Replacing all orchestration would duplicate a large amount of CI policy and application knowledge.
5. **Affected-task planning is likely the highest-value feature.** The repository has enough packages and generated tooling that an agent needs a compact explanation of why a command is selected.

### Good initial pilot tasks

- Parse workspace/package dependency graph.
- Map changed files to affected packages.
- Plan package compilation and type checks.
- Explain root-vs-package script selection.
- Capture deterministic scanner/report artifacts.
- Model publishing/deployment as approval-gated side effects.

### Tasks requiring special treatment

- Browser/e2e tests: runtime and service dependencies need explicit environment declarations.
- i18n generation and endpoint catalog generation: generated outputs and source catalogs must be modeled precisely.
- `publish:*`, preview, and deployment scripts: side-effecting and credential-bearing.
- Root scripts that invoke shell wrappers: must be represented as structured command tasks or explicit adapters, not opaque cacheable commands by default.

## Common protocol requirements

The two repositories share enough structure to justify a common protocol:

```text
Workspace
  root, package manager, runtime/toolchain versions, policy

Package/Project
  id, directory, dependencies, metadata

Task
  project, name, command or adapter, dependencies
  inputs, outputs, environment, capabilities, cache policy

Plan
  selected tasks, closure, cache decisions, explanations

Execution
  task digest, status, evidence, output manifests, provenance
```

The protocol must permit a task to be:

- deterministic and cacheable;
- non-cacheable but reproducible in its plan;
- persistent/interactive;
- side-effecting and approval-required;
- evaluation-producing with structured scores;
- delegated to an adapter rather than a shell command.
