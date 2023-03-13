import OpenSSLToolchain from "https://rules.warp.build/toolchains/openssl.js";

const impl = (ctx) => {
  const { version, sha1 } = ctx.cfg();

  const { host } = ctx.env();

  const output = "python.zip";
  const prefix = `Python-${version}`;

  const url =
    `https://www.python.org/ftp/python/${version}/Python-${version}.tgz`;

  ctx.action().download({ url, sha1, output });

  ctx.action().extract({ src: output, dst: "." });

  ctx.action().runShell({
    script: `

    cd ${prefix}

    ./configure --prefix=$(pwd)/dist --with-openssl=$OpenSSL_HOME

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
    ]);

  ctx.provides({
    python: `${prefix}/dist/bin/python3`,
  });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/python",
  mnemonic: "Python",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
  toolchains: [OpenSSLToolchain],
});
