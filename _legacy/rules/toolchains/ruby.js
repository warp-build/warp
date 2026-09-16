import OpenSSLToolchain from "https://rules.warp.build/toolchains/openssl.js";

const impl = (ctx) => {
  const { version, sha256 } = ctx.cfg();

  const output = "ruby.tar.gz";
  const prefix = `ruby-${version}`;

  const url = `https://cache.ruby-lang.org/pub/ruby/${
    version.slice(
      0,
      3,
    )
  }/ruby-${version}.tar.gz`;

  ctx.action().download({ url, sha256, output });

  ctx.action().extract({ src: output, dst: "." });

  ctx.action().runShell({
    script: `

    cd ${prefix}

    ./configure --prefix=$(pwd)/dist --with-openssl-dir=$OpenSSL_HOME

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
    ruby: `${prefix}/dist/bin/ruby`,
    irb: `${prefix}/dist/bin/irb`,
  });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/ruby",
  mnemonic: "Ruby",
  impl,
  cfg: {
    version: string(),
    sha256: string(),
  },
  toolchains: [OpenSSLToolchain],
});
