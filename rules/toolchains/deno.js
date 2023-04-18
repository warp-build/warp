export const JS_EXT = ".js";
export const TS_EXT = ".ts";

const impl = (ctx) => {
  const { host } = ctx.env();
  const { version, sha256_aarch64, sha256_x86_64 } = ctx.cfg();

  let arch = host.arch;
  let sha256 = sha256_aarch64;
  if (arch === "x86_64") {
    sha256 = sha256_x86_64;
  }

  const url =
    `https://github.com/denoland/deno/releases/download/v${version}/deno-${host.triple}.zip`;

  const output = "deno.zip";

  ctx.action().download({ url, sha256, output });

  ctx.action().extract({ src: output, dst: "." });

  const DENO = "deno";
  ctx.action().setPermissions({ file: DENO, executable: true });

  ctx.action().declareOutputs([DENO]);

  ctx.provides({ DENO });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/deno",
  mnemonic: "Deno",
  impl,
  cfg: {
    version: string(),
    sha256_aarch64: string(),
    sha256_x86_64: string(),
  },
});
