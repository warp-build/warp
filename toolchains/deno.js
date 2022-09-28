export const JS_EXT = ".js";
export const TS_EXT = ".ts";

const impl = ctx => {
  const { host } = ctx.env();
  const { version, sha1_aarch64, sha1_x86_64 } = ctx.cfg();

  let arch = host.arch
  let sha1 = sha1_aarch64
  if (arch === "x86_64") {
    sha1 = sha1_x86_64
  }

  const url = `https://github.com/denoland/deno/releases/download/v${version}/deno-${host.triple}.zip`

  const output = "deno.zip"

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  const DENO = "deno";
  ctx.action().setPermissions({ file: DENO, executable: true })

  ctx.action().declareOutputs([DENO]);

  ctx.provides({ DENO });

};

export default Warp.Toolchain({
  name: "https://pkgs.warp.build/toolchains/deno",
  mnemonic: "Deno",
  impl,
  cfg: {
    version: string(),
    sha1_aarch64: string(),
    sha1_x86_64: string()
  }
});
