interface RustTargetConfig {
  compiler: string;
  crate_name: string;
  crate_type: string;
  dependencies: string[];
  edition: string;
  features: string[];
  compiler_arguments: string[];
  output_name: string;
  source: string;
}

interface RuleContext<Config> {
  cfg(): Config;
  action(): {
    exec(action: { program: string; args: string[] }): void;
    declareOutputs(outputs: string[]): void;
  };
}

declare const configuration: RustTargetConfig;
declare const Warp: {
  Rule<Config>(spec: {
    name: string;
    mnemonic: string;
    cfg: Record<string, unknown>;
    defaults?: Record<string, unknown>;
    toolchains?: string[];
    impl(ctx: RuleContext<Config>): void;
  }): { plan(configuration: Config): unknown };
  emit(plan: unknown): void;
};
declare function string(): string;
declare function file(): string;

const RustTarget = Warp.Rule<RustTargetConfig>({
  name: "rust_target",
  mnemonic: "RustTarget",
  cfg: {
    compiler: string(),
    crate_name: string(),
    crate_type: string(),
    dependencies: [string()],
    edition: string(),
    features: [string()],
    compiler_arguments: [string()],
    output_name: string(),
    source: file(),
  },
  toolchains: ["rustc"],
  impl(ctx) {
    const config = ctx.cfg();
    const featureArguments = config.features.flatMap((feature) => [
      "--cfg",
      `feature=${JSON.stringify(feature)}`,
    ]);
    const externArguments = config.dependencies.flatMap((dependency) => [
      "--extern",
      dependency,
    ]);

    ctx.action().exec({
      program: config.compiler,
      args: [
        "--crate-name",
        config.crate_name,
        "--crate-type",
        config.crate_type,
        "--edition",
        config.edition,
        "--emit=link",
        ...config.compiler_arguments,
        ...featureArguments,
        ...externArguments,
        config.source,
      ],
    });
    ctx.action().declareOutputs([config.output_name]);
  },
});

Warp.emit(RustTarget.plan(configuration));
