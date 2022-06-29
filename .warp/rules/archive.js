export const TAR_EXT = ".tar"
export const TAR_GZ_EXT = ".tar.gz"

const impl = ctx => {
  const { name, label, srcs } = ctx.cfg();

  const transitiveDeps = ctx.transitiveDeps().flatMap(dep => dep.outs);

  const out = Label.path(label).join(`${name}.tar.gz`);
  ctx.action().declareOutputs([ out ]);

  ctx.action().exec({
    cmd: "tar",
    args: ["czfh", out, ...srcs, ...transitiveDeps]
  });
};

export default Warp.Rule({
  name: "archive",
  mnemonic: "Tar",
  impl,
  cfg: {
    name: label(),
    deps: [label()],
    srcs: [file()],
  },
});
