interface BunConfiguration {
  bun: string;
}

interface BunToolchain {
  name: string;
  program: string;
  versionArgs: string[];
  config: Record<string, unknown>;
}

declare const Warp: {
  Toolchain(spec: {
    name: string;
    program: string;
    versionArgs?: string[];
    config?: Record<string, unknown>;
  }): BunToolchain;
};

function bunToolchain(configuration: BunConfiguration): BunToolchain {
  return Warp.Toolchain({
    name: "bun",
    program: configuration.bun,
    versionArgs: ["--version"],
    config: {
      package_manifest: "package.json",
      lockfiles: ["bun.lock", "bun.lockb"],
      cloudflare_config: "cloudflare.config.ts",
    },
  });
}
