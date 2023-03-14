const impl = (ctx) => {
  const { host } = ctx.env();
  const { version, sha1 } = ctx.cfg();

  const url =
    `https://mirrors.edge.kernel.org/pub/software/scm/git/git-${version}.tar.gz`;

  const prefix = `git-${version}`;
  const output = "git.tar.gz";

  ctx.action().download({ url, sha1, output });

  ctx.action().extract({ src: output, dst: "." });

  ctx.action().runShell({
    script: `

    cd ${prefix}

    make configure

    ./configure --prefix=$(pwd)/dist

    make -j

    make install -j

    `,
  });

  ctx
    .action()
    .declareOutputs([
      `${prefix}/dist/bin`,
      `${prefix}/dist/libexec`,
      `${prefix}/dist/share`,
    ]);

  ctx.provides({ git: `${prefix}/dist/bin/git` });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/git",
  mnemonic: "Git",
  impl,
  cfg: {
    sha1: string(),
    version: string(),
  },
  defaults: {
    sha1: "fc5107c0cc41fc7ee870c360d67517cd0ee26d17",
    version: "2.38.1",
  },
});
