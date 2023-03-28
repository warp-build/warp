import RustToolchain, {
  RS_EXT,
} from "https://rules.warp.build/toolchains/rust.js";

const impl = (ctx) => {
  const { name, deps, target, srcs } = ctx.cfg();
  const cwd = Target.path(target);

  ctx.action().runShell({
    script: `rustc ${name}`,
  });

  const run = `main`;

  ctx.action().declareOutputs([run]);

  ctx.action().setPermissions({ file: run, executable: true });
  // For now, mark II does not support running
  // ctx.action().declareRunScript(run);
  ctx.provides({ [name]: run });
};

export default Warp.Rule({
  runnable: true,
  name: "https://rules.warp.build/rules/rust_binary",
  mnemonic: "RustBin",
  impl,
  cfg: {
    name: target(),
    deps: [target()],
    srcs: [file()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [RustToolchain],
});
