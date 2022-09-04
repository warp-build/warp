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

./config -no-tests --prefix=$(pwd)/dist
make -j
make install -j10

`});

  ctx.action().declareOutputs([]);

  ctx.provides({
    OPENSSL_HOME: `${prefix}/dist`
  });
};

export default Warp.Toolchain({
  name: "https://pkgs.warp.build/toolchains/openssl",
  mnemonic: "OpenSSL",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
});
