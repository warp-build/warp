export const CMI_EXT = ".cmi";
export const CMT_EXT = ".cmt";
export const JS_EXT = ".js";
export const MERLIN = ".merlin";
export const RESI_EXT = ".resi";
export const RES_EXT = ".res";

const impl = ctx => {
  const { unarchivedRoot } = ctx.cfg();

  const binRoot = `${unarchivedRoot}/${ctx.platform.os}`

  const BSC = File.join(binRoot, "bsc.exe");
  const BSREFMT = File.join(binRoot, "bsrefmt.exe");
  const RESCRIPT = File.join(binRoot, "rescript.exe");
  ctx.provides({ BSC, BSREFMT, RESCRIPT });
  ctx.action().declareOutputs([]);
};

export default Zap.Toolchain({
  name: "//zap.build/toolchains:rescript",
  mnemonic: "ReScript",
  impl,
});


