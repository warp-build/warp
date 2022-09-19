import OpenSSLToolchain from "https://pkgs.warp.build/toolchains/openssl.js";

export const HEADER_EXT = ".hrl";
export const BEAM_EXT = ".beam";
export const ERL_EXT = ".erl";

const impl = ctx => {
  const { version, sha1, configure_flags, make_flags, build_size } = ctx.cfg();
  const { host } = ctx.env();

  const output = "erlang.tar.gz"

  const prefix = `otp_src_${version}`

  const url = `https://github.com/erlang/otp/releases/download/OTP-${version}/${prefix}.tar.gz`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  ctx.action().runShell({
    script: `

cd ${prefix}

${
  build_size === "tiny"
  ? "OTP_TINY_BUILD=true"
  : build_size === "small"
  ? "OTP_SMALL_BUILD=true"
  : ""
}

export OTP_SMALL_BUILD OTP_TINY_BUILD

./configure ${configure_flags.join(" ")} \
  --with-ssl=$OpenSSL_HOME \
  --prefix=$(pwd)/dist || exit 1

make all install ${make_flags.join(" ")} || exit 1

`});

  const root = `${prefix}/dist`;
  ctx.action().declareOutputs([root]);

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
  name: "https://pkgs.warp.build/toolchains/erlang",
  mnemonic: "Erlang/OTP",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
    build_size: string(),
    configure_flags: [string()],
    make_flags: [string()],
  },
  defaults: {
    build_size: "full",
    configure_flags: [],
    make_flags: [],
  },
  toolchains: [OpenSSLToolchain]
});
