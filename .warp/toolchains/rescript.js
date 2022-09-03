export const CMI_EXT = ".cmi";
export const CMT_EXT = ".cmt";
export const JS_EXT = ".js";
export const MERLIN = ".merlin";
export const RESI_EXT = ".resi";
export const RES_EXT = ".res";

const impl = ctx => {
  const { version, sha1 } = ctx.cfg();
  const { host } = ctx.env();

  const url = `https://github.com/rescript-lang/rescript-compiler/archive/refs/tags/${version}.tar.gz`

  const output = "rescript.tar.gz"

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  const binRoot = `rescript-compiler-${version}/${host.os}`;
  const BSC = File.join(binRoot, "bsc.exe");
  const BSREFMT = File.join(binRoot, "refmt.exe");
  const RESCRIPT = File.join(binRoot, "rescript.exe");

  ctx.action().setPermissions({ file: BSC, executable: true })
  ctx.action().setPermissions({ file: RESCRIPT, executable: true })
  ctx.action().setPermissions({ file: BSREFMT, executable: true })

  ctx.action().declareOutputs([]);

  ctx.provides({ BSC, BSREFMT, RESCRIPT });
};

export default Warp.Toolchain({
  name: "https://pkgs.warp.build/toolchains/rescript",
  mnemonic: "ReScript",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
});


