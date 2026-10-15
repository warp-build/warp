interface RustTargetConfig {
  compiler: string;
  compilation_target: string;
  host_target: string;
  profile: string;
  crate_name: string;
  package_manifest_directory: string;
  package_name: string;
  package_version: string;
  crate_type: string;
  dependencies: string[];
  dependency_directories: string[];
  native_directories: string[];
  edition: string;
  features: string[];
  environment: string;
  output_directory: string;
  output_name: string;
  source_root: string;
  source: string;
}

interface Toolchain {
  name: string;
  program: string;
  versionArgs: string[];
  config: Record<string, unknown>;
}

interface RuleContext<Config> {
  cfg(): Config;
  action(): {
    exec(action: {
      program: string;
      args: string[];
      env?: Record<string, string>;
      cwd?: string | null;
    }): void;
    declareOutputs(outputs: string[]): void;
  };
}

declare const configuration: RustTargetConfig;
declare const Rustc: Toolchain;
declare const Warp: {
  Toolchain(spec: { name: string; program: string; versionArgs?: string[] }): Toolchain;
  Rule<Config>(spec: {
    name: string;
    mnemonic: string;
    cfg: Record<string, unknown>;
    defaults?: Record<string, unknown>;
    toolchains?: Toolchain[];
    impl(ctx: RuleContext<Config>): void;
  }): { plan(configuration: Config): unknown };
  emit(plan: unknown): void;
};
declare function string(): string;
declare function file(): string;

function targetEnvironmentName(target: string, suffix: string): string {
  const normalized = target.replace(/[-.]/g, "_").toUpperCase();
  return `CARGO_TARGET_${normalized}_${suffix}`;
}

function rustcFlags(target: string, environment: Record<string, string>): string[] {
  const flags: string[] = [];
  const linker = environment[targetEnvironmentName(target, "LINKER")];
  if (linker !== undefined) flags.push("-C", `linker=${linker}`);

  if (environment.CARGO_ENCODED_RUSTFLAGS !== undefined) {
    flags.push(...environment.CARGO_ENCODED_RUSTFLAGS.split("\u001f").filter(Boolean));
  } else if (environment.RUSTFLAGS !== undefined) {
    flags.push(...environment.RUSTFLAGS.split(/\s+/).filter(Boolean));
  }
  const targetFlags = environment[targetEnvironmentName(target, "RUSTFLAGS")];
  if (targetFlags !== undefined) flags.push(...targetFlags.split(/\s+/).filter(Boolean));
  return flags;
}

function profileArguments(profile: string): string[] {
  if (profile === "debug") return ["-C", "debuginfo=2"];
  if (profile === "release") {
    return [
      "-C", "opt-level=3",
      "-C", "debug-assertions=off",
      "-C", "overflow-checks=off",
    ];
  }
  throw new Error(`unsupported Rust build profile ${profile}`);
}

const RustTarget = Warp.Rule<RustTargetConfig>({
  name: "rust_target",
  mnemonic: "RustTarget",
  cfg: {
    compiler: string(),
    compilation_target: string(),
    host_target: string(),
    profile: string(),
    crate_name: string(),
    package_manifest_directory: string(),
    package_name: string(),
    package_version: string(),
    crate_type: string(),
    dependencies: [string()],
    dependency_directories: [string()],
    native_directories: [string()],
    edition: string(),
    features: [string()],
    environment: string(),
    output_directory: string(),
    output_name: string(),
    source_root: string(),
    source: file(),
  },
  toolchains: [Rustc],
  impl(ctx) {
    const config = ctx.cfg();
    const effectiveTarget = config.crate_type === "proc-macro"
      ? config.host_target
      : config.compilation_target;
    const outputDirectory = config.output_directory;
    const environment = JSON.parse(config.environment) as Record<string, string>;
    const arguments = [
      "--target", effectiveTarget,
      "--crate-name", config.crate_name,
      "--crate-type", config.crate_type,
      "--edition", config.edition,
      "--emit=link",
      "--out-dir", outputDirectory,
      "--remap-path-prefix", `${config.source_root}=/warp-source`,
      "--remap-path-prefix", `${outputDirectory}=/warp-output`,
      ...profileArguments(config.profile),
      ...rustcFlags(effectiveTarget, environment),
      ...config.dependency_directories.flatMap((directory) => ["-L", `dependency=${directory}`]),
      ...config.native_directories.flatMap((directory) => ["-L", `native=${directory}`]),
      ...config.features.flatMap((feature) => ["--cfg", `feature=${JSON.stringify(feature)}`]),
      ...config.dependencies.flatMap((dependency) => ["--extern", dependency]),
      config.source,
    ];

    ctx.action().exec({
      program: config.compiler,
      args: arguments,
      cwd: config.source_root,
      env: {
        CARGO_MANIFEST_DIR: config.package_manifest_directory,
        CARGO_PKG_NAME: config.package_name,
        CARGO_PKG_VERSION: config.package_version,
      },
    });
    ctx.action().declareOutputs([config.output_name]);
  },
});

Warp.emit(RustTarget.plan(configuration));
