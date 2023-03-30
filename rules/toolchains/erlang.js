import OpenSSLToolchain from "https://rules.warp.build/toolchains/openssl.js";

export const HEADER_EXT = ".hrl";
export const BEAM_EXT = ".beam";
export const ERL_EXT = ".erl";

const impl = (ctx) => {
  const { version, sha1, configure_flags, make_flags, build_size } = ctx.cfg();
  const { host } = ctx.env();

  const output = "erlang.tar.gz";

  const prefix = `otp_src_${version}`;

  const url =
    `https://github.com/erlang/otp/releases/download/OTP-${version}/${prefix}.tar.gz`;

  ctx.action().download({ url, sha1, output });

  ctx.action().extract({ src: output, dst: "." });

  ctx.action().runShell({
    script: `

set -e

unset ERL_LIBS
unset ERL_FLAGS
unset ERL_AFLAGS
unset ERL_ZFLAGS

cd ${prefix}

export ERLC_USE_SERVER=true
export ERL_TOP=$(pwd)
export LANG=C

./otp_build configure \
  ${configure_flags.join(" ")} \
  --with-ssl=$OpenSSL_HOME \
  --prefix=$(pwd)/dist

make

make install

`,
  });

  const root = `${prefix}/dist`;
  const lib = `${prefix}/lib`;
  ctx.action().declareOutputs([root, lib]);

  const binRoot = `${root}/bin`;
  ctx.provides({
    erlc: File.join(binRoot, "erlc"),
    erl: File.join(binRoot, "erl"),
    escript: File.join(binRoot, "escript"),
    ct_run: File.join(binRoot, "ct_run"),
    dialyzer: File.join(binRoot, "dialyzer"),
  });

  const usrRoot = `${root}/lib/erlang/usr`;
  ctx.setEnv({
    ERL_ROOT: ctx.path(binRoot),
    ERL_INCLUDE_PATH: ctx.path(`${usrRoot}/include`),
    ERL_LIB_PATH: ctx.path(`${usrRoot}/lib`),
  });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/erlang",
  mnemonic: "Erlang/OTP",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
    build_size: string(),
    configure_flags: [string()],
  },
  defaults: {
    sha1: "13f541022443d0e34021b2dca51b2541954d3e7b",
    version: "25.3",
    build_size: "full",
    configure_flags: [
      "--disable-debug",
      "--disable-silent-rules",
      "--enable-dynamic-ssl-lib",
      "--enable-hipe",
      "--enable-shared-zlib",
      "--enable-smp-support",
      "--enable-threads",
      "--without-javac",
      "--enable-darwin-64bit",
      "--enable-kernel-poll",
    ],
  },
  toolchains: [OpenSSLToolchain],
});
