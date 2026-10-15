interface CargoConfiguration {
  compilation_target: string;
  rust_toolchain: string;
  cargo_target_directory: string;
}

interface CargoToolchain {
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
  }): CargoToolchain;
};

function cargoToolchain(configuration: CargoConfiguration): CargoToolchain {
  const metadataEnvironment = configuration.rust_toolchain === ""
    ? {}
    : {
        RUSTUP_TOOLCHAIN: configuration.rust_toolchain,
        CARGO_TARGET_DIR: configuration.cargo_target_directory,
      };

  return Warp.Toolchain({
    name: "cargo",
    program: "cargo",
    versionArgs: ["--version"],
    config: {
      manifest_file: "Cargo.toml",
      workspace_inputs: [
        "Cargo.toml",
        "Cargo.lock",
        "rust-toolchain.toml",
        ".cargo/config.toml",
        ".cargo/config",
      ],
      discovery_command: {
        program: "cargo",
        arguments: [
          "metadata",
          "--locked",
          "--format-version",
          "1",
          "--filter-platform",
          configuration.compilation_target,
        ],
        env: metadataEnvironment,
      },
    },
  });
}
