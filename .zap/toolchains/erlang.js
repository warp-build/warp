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
    ctx.action().runShell({
      script: `#!/bin/bash -xe

      cd ${unarchivedRoot}
      ./otp_build all

    `});
  }

  const binRoot = File.join(unarchivedRoot, "bin");
  const ERLC = File.join(binRoot, "erlc");
  const ERL = File.join(binRoot, "erl");
  const REBAR = "rebar3";
  ctx.provides({ ERLC, ERL, REBAR });
  ctx.action().declareOutputs([]);
};

export default Zap.Toolchain({
  name: "//zap.build/toolchains:erlang",
  mnemonic: "Erlang/OTP",
  impl,
});
