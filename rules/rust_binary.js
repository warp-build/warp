import RustToolchain, {
  RS_EXT,
} from "https://rules.warp.build/toolchains/rust.js";

const impl = (ctx) => {
  const { name, deps, label, srcs } = ctx.cfg();
  const cwd = Label.path(label);

  ctx.action().runShell({
    script: `rustc src/main.rs`,
  });

  const run = `main`;

  ctx
    .action()
    .declareOutputs([run]);
  ctx.action().setPermissions({ file: run, executable: true });
  ctx.action().declareRunScript(run);
  ctx.provides({ [name]: run });
};

export default Warp.Rule({
  runnable: true,
  name: "https://rules.warp.build/rules/rust_binary",
  mnemonic: "RustBin",
  impl,
  cfg: {
    name: label(),
    deps: [label()],
    srcs: [file()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [RustToolchain],
});
