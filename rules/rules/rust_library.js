import RustToolchain, {
  RLIB_EXT,
  RS_EXT,
} from "https://rules.warp.build/toolchains/rust.js";

const impl = (ctx) => {
  const {
    crate_type,
    crate_name,
    cwd,
    deps,
    edition,
    main,
    name,
    srcs,
    features,
    target,
  } = ctx.cfg();

  const externCrates = ctx
    .transitiveDeps()
    .flatMap((dep) => {
      let rlib = dep.outs.find((out) => out.endsWith(RLIB_EXT));
      if (rlib) {
        let name = dep.config.crate_name;
        return [`${name}=${rlib}`];
      }
      return [];
    })
    .unique();

  if (crate_type == "proc-macro") {
    externCrates.push("proc-macro");
  }
  const externs = externCrates.map(crate => `--extern ${crate}`);

  const libs = ctx
    .transitiveDeps()
    .flatMap((dep) => dep.outs)
    .map((out) => File.parent(out))
    .unique()
    .map((out) => `-L ${out}`);

  if (features.length === 0) {
    features.push("default");
  }
  const cfg_features = features.map((feat) => `--cfg 'feature="${feat}"'`);

  const OUT_DIR = `target`;

  ctx.action().runShell({
    script: `

mkdir -p ${OUT_DIR}

rustc \\
  --edition ${edition} \\
  --crate-name ${crate_name} \\
  --crate-type ${crate_type} \\
  --emit asm,llvm-bc,llvm-ir,obj,metadata,link,dep-info,mir \\
  --out-dir ${OUT_DIR} \\
  ${cfg_features.join(" \\\n  ")} \\
  ${libs.join(" \\\n  ")} \\
  ${externs.join(" \\\n  ")} \\
  ${main}

`,
  });

  let ext = crate_type;
  if (crate_type == "lib") {
    ext = "rlib";
  }

  ctx.action().copy({
    src: `${cwd()}/Cargo.toml`,
    dst: `${OUT_DIR}/Cargo.toml`,
  });

  ctx.action().declareOutputs([
    `${OUT_DIR}/lib${crate_name}.${ext}`,
    `${OUT_DIR}/Cargo.toml`,
  ]);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/rust_library",
  mnemonic: "RustLib",
  impl,
  cfg: {
    name: target(),
    cargo_name: string(),
    crate_name: string(),
    crate_type: string(),
    features: [string()],
    edition: string(),
    main: file(),
    srcs: [file()],
    deps: [target()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [RustToolchain],
});
