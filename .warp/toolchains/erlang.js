export const HEADER_EXT = ".hrl";
export const BEAM_EXT = ".beam";
export const ERL_EXT = ".erl";

const impl = ctx => {
  const { version, sha1, build_flags } = ctx.cfg();
  const { host } = ctx.env();

  const output = "erlang.tar.gz"

  const prefix = `otp_src_${version}`

  const url = `https://github.com/erlang/otp/releases/download/OTP-${version}/${prefix}.tar.gz`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  ctx.action().runShell({
    script: `#!/bin/bash -xe

cd ${prefix}
./otp_build all ${build_flags}

CANONICAL_SYSTEM_NAME=$(sh ./erts/autoconf/config.guess)

pushd bin;
  ln -s $CANONICAL_SYSTEM_NAME ${host.triple}
popd;

pushd release;
  ln -s $CANONICAL_SYSTEM_NAME ${host.triple}
popd;

`});

  ctx.action().declareOutputs([]);

  const binRoot = `${prefix}/bin/${host.triple}`;
  ctx.provides({
    ERLC: File.join(binRoot, "erlc"),
    ERL: File.join(binRoot, "erl"),
    ESCRIPT: File.join(binRoot, "escript"),
    CT_RUN: File.join(binRoot, "ct_run"),
    DIALYZER: File.join(binRoot, "dialyzer"),
    ERL_ROOT: `${prefix}/bin`,
    INCLUDE_PATH: `${prefix}/release/${host.triple}/usr/include`,
    LIB_PATH: `${prefix}/release/${host.triple}/lib`,
  });
};

export default Warp.Toolchain({
  name: "//warp.build/toolchains:erlang",
  mnemonic: "Erlang/OTP",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
    build_flags: [string()],
  },
  defaults: {
    build_flags: ["-t"]
  },
});
