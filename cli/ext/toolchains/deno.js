export const JS_EXT = ".js";
export const TS_EXT = ".ts";

const impl = ctx => {
  const { unarchivedRoot } = ctx.cfg();
  const DENO = File.join(unarchivedRoot, "deno");
  ctx.provides({ DENO });
  ctx.action().declareOutputs([]);
};

export default Warp.Toolchain({
  name: "//warp.build/toolchains:deno",
  mnemonic: "Deno",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
});
