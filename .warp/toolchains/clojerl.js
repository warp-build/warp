const impl = ctx => {
  const root = ctx.archive().unpackedRoot();
  const binRoot = root.join("bin");
  const CLOJERL = binRoot.join("clojerl");
  ctx.provides({ CLOJERL });
};

export default Warp.Toolchain({
  name: "clojerl",
  mnemonic: "ClojErl",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
});
