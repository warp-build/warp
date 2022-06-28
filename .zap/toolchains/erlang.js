export const HEADER_EXT = ".hrl";
export const BEAM_EXT = ".beam";
export const ERL_EXT = ".erl";

const impl = ctx => {
  const { version, sha1 } = ctx.cfg();

  const output = "erlang.tar.gz"

  const url = `https://github.com/erlang/otp/releases/download/OTP-${version}/otp_src_${version}.tar.gz`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  ctx.action().runShell({
    script: `#!/bin/bash -xe

cd otp_src_*
./otp_build all

`});

  const binRoot = "bin";
  const ERLC = File.join(binRoot, "erlc");
  const ERL = File.join(binRoot, "erl");

  ctx.action().setPermissions({ file: ERLC, executable: true })
  ctx.action().setPermissions({ file: ERL, executable: true })

  ctx.action().declareOutputs([]);

  ctx.provides({ ERLC, ERL });
};

export default Zap.Toolchain({
  name: "//zap.build/toolchains:erlang",
  mnemonic: "Erlang/OTP",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  }
});
