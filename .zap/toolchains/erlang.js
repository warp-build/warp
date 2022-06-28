export const HEADER_EXT = ".hrl";
export const BEAM_EXT = ".beam";
export const ERL_EXT = ".erl";

const impl = ctx => {
  const { version, sha1 } = ctx.cfg();

  const output = "erlang.tar.gz"

  const prefix = `otp_src_${version}`

  const url = `https://github.com/erlang/otp/releases/download/OTP-${version}/${prefix}.tar.gz`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  ctx.action().runShell({
    script: `#!/bin/bash -xe

cd ${prefix}
./otp_build all

`});

  const binRoot = `${prefix}/bin`;
  const ERLC = File.join(binRoot, "erlc");
  const ERL = File.join(binRoot, "erl");

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
