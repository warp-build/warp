const impl = (ctx) => {
  const { version, sha256, build_flags } = ctx.cfg();
  const { host } = ctx.env();

  const output = "openssl.tar.gz";

  const prefix = `openssl-${version}`;

  const url = `https://github.com/openssl/openssl/archive/${version}.tar.gz`;

  ctx.action().download({ url, sha256, output });

  ctx.action().extract({ src: output, dst: "." });

  ctx.action().runShell({
    script: `#!/bin/bash -xe

cd ${prefix}

./config -no-tests --prefix=$(pwd)/dist

make -j

make install -j

`,
  });

  ctx
    .action()
    .declareOutputs([
      `${prefix}/dist/bin`,
      `${prefix}/dist/lib`,
      `${prefix}/dist/include`,
      `${prefix}/dist/ssl`,
    ]);

  ctx.provides({
    openssl: `${prefix}/dist/bin/openssl`,
  });

  ctx.setEnv({
    OpenSSL_HOME: ctx.path(`${prefix}/dist`),
    OpenSSL_INCLUDE: ctx.path(`${prefix}/dist/include`),
    OpenSSL_LIB: ctx.path(`${prefix}/dist/lib`),
  });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/openssl",
  mnemonic: "OpenSSL",
  impl,
  cfg: {
    version: string(),
    sha256: string(),
  },
  defaults: {
    version: "OpenSSL_1_1_1q",
    sha256: "73118336c58ece2e3b87f1f933f8ba446e2bdc26",
  },
});
