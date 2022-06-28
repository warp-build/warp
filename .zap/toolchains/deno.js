export const JS_EXT = ".js";
export const TS_EXT = ".ts";

const impl = ctx => {
  const { host } = ctx.env();
  const { version, sha1 } = ctx.cfg();

  const url = `https://github.com/denoland/deno/releases/download/v${version}/deno-${host.triple}.zip`

  const output = "deno.zip"

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  const DENO = "deno";
  ctx.action().setPermissions({ file: DENO, executable: true })
  ctx.action().declareOutputs([DENO]);
  ctx.action().declareRunScript(DENO);

  ctx.provides({ DENO });

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
