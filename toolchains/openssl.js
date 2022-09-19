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

make install -j

`});

  ctx.action().declareOutputs([
    `${prefix}/dist/bin`,
    `${prefix}/dist/lib`,
    `${prefix}/dist/include`,
    `${prefix}/dist/ssl`,
  ]);

  ctx.provides({
    openssl: `${prefix}/dist/bin/openssl`
  });

  ctx.setEnv({
    OpenSSL_HOME: ctx.path(`${prefix}/dist`),
    OpenSSL_INCLUDE: ctx.path(`${prefix}/dist/include`),
    OpenSSL_LIB: ctx.path(`${prefix}/dist/lib`),
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
