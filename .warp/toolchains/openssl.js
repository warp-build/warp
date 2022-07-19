const impl = ctx => {
  const { version, sha1, build_flags } = ctx.cfg();
  const { host } = ctx.env();

  const output = "openssl.tar.gz"

  const prefix = `openssl-${version}`

  const url = `https://github.com/openssl/openssl/archive/${version}.tar.gz`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  ctx.action().runShell({
    script: `#!/bin/bash -xe

cd ${prefix}

mkdir dist

./config -no-tests --prefix=$(pwd)/dist || exit 1
make -j || exit 1
make install -j10 || exit 1

`});

  ctx.action().declareOutputs([]);

  ctx.provides({
    OPENSSL_HOME: `${prefix}/dist`
  });
};

export default Warp.Toolchain({
  name: "//warp.build/toolchains:openssl",
  mnemonic: "OpenSSL",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
});

