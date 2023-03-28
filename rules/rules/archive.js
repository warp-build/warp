export const TAR_EXT = ".tar";
export const TAR_GZ_EXT = ".tar.gz";

const impl = (ctx) => {
  const { name, target, srcs } = ctx.cfg();

  const transitiveDeps = ctx.transitiveDeps().flatMap((dep) => dep.outs);

  const out = Target.path(target).join(`${name}.tar.gz`);
  ctx.action().declareOutputs([out]);

  ctx.action().exec({
    cmd: "tar",
    args: ["czfh", out, ...srcs, ...transitiveDeps],
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/archive",
  mnemonic: "Tar",
  impl,
  cfg: {
    name: target(),
    deps: [target()],
    srcs: [file()],
  },
});
