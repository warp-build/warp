interface BunTargetConfig {
  bun: string;
  bun_version: string;
  package_json: string;
  lockfile: string;
  cloudflare_config: string;
  source_directory: string;
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

declare const configuration: BunTargetConfig;
declare const Bun: Toolchain;
declare const Warp: {
  Toolchain(spec: { name: string; program: string; versionArgs?: string[] }): Toolchain;
  Rule<Config>(spec: {
    name: string;
    mnemonic: string;
    cfg: Record<string, unknown>;
    toolchains?: Toolchain[];
    impl(ctx: RuleContext<Config>): void;
  }): { plan(configuration: Config): unknown };
  emit(plan: unknown): void;
};
declare function string(): string;

interface PackageManifest {
  packageManager?: string;
  scripts?: Record<string, string>;
}

const BunTarget = Warp.Rule<BunTargetConfig>({
  name: "bun_target",
  mnemonic: "BunTarget",
  cfg: {
    bun: string(),
    bun_version: string(),
    package_json: string(),
    lockfile: string(),
    cloudflare_config: string(),
    source_directory: string(),
  },
  toolchains: [Bun],
  impl(ctx) {
    const config = ctx.cfg();
    const manifest = JSON.parse(config.package_json) as PackageManifest;
    if (config.lockfile.trim() === "") {
      throw new Error("Bun package requires a lockfile declared by its toolchain");
    }
    if (manifest.packageManager !== undefined &&
        manifest.packageManager !== `bun@${config.bun_version}`) {
      throw new Error(
        `packageManager ${manifest.packageManager} does not match Bun ${config.bun_version}`,
      );
    }
    if (typeof manifest.scripts?.build !== "string" || manifest.scripts.build.trim() === "") {
      throw new Error("Bun package must declare a non-empty scripts.build command");
    }

    const output = config.cloudflare_config !== ""
      ? ".cloudflare/output"
      : "dist";
    ctx.action().exec({
      program: config.bun,
      args: ["install", "--frozen-lockfile"],
      cwd: config.source_directory,
    });
    ctx.action().exec({
      program: config.bun,
      args: ["run", "build"],
      cwd: config.source_directory,
    });
    ctx.action().declareOutputs([output]);
  },
});

Warp.emit(BunTarget.plan(configuration));
