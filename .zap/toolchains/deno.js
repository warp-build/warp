export const JS_EXT = ".js";
export const TS_EXT = ".ts";

const impl = ctx => {
  const { host } = ctx.env();
  const { version, sha1 } = ctx.cfg();

  const url = `https://github.com/denoland/deno/releases/download/v${version}/deno-${host.triplet}.zip`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  const DENO = "deno";
  ctx.provides({ DENO });

  ctx.action().declareOutputs([]);
};

export default Zap.Toolchain({
  name: "//zap.build/toolchains:deno",
  mnemonic: "Deno",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  }
});
