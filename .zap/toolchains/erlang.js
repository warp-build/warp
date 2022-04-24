export const HEADER_EXT = ".hrl";
export const BEAM_EXT = ".beam";
export const ERL_EXT = ".erl";

const impl = ctx => {
  const {
    archiveKind,
    archiveSha1,
    unarchivedRoot,
  } = ctx.cfg();

  if (archiveKind === "source") {
    ctx.action().exec({
      cmd: File.join(unarchivedRoot, "otp_build"),
      args: ["all"],
      cwd: unarchivedRoot
    });
  }

  const binRoot = File.join(unarchivedRoot, "bin");
  const ERLC = File.join(binRoot, "erlc");
  const ERL = File.join(binRoot, "erl");
  ctx.provides({ ERLC, ERL });
  ctx.action().declareOutputs([]);
};

export default Zap.Toolchain({
  name: "//zap.build/toolchains:erlang",
  mnemonic: "Erlang/OTP",
  impl,
});
