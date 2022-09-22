const impl = (ctx) => {
  const { sha1, version } = ctx.cfg();
  const { host } = ctx.env();

  let arch = host.arch;

  if (host.arch === "aarch64") {
    arch = "arm64";
  }

  let platform = `${host.os}-${arch}`;
  const node_path = `node-v${version}-${platform}`;
  const url = `https://nodejs.org/dist/v${version}/${node_path}.tar.gz`;

  const tempFile = "node.tar.gz";
  ctx.action().download({ url, sha1, output: tempFile });
  ctx.action().extract({ src: tempFile, dst: "." });

  ctx
    .action()
    .declareOutputs([
      `${node_path}/bin`,
      `${node_path}/lib`,
      `${node_path}/include`,
    ]);

  ctx.provides({
    npm: `${node_path}/bin/npm`,
    npx: `${node_path}/bin/npx`,
    node: `${node_path}/bin/node`,
    corepack: `${node_path}/bin/corepack`,
  });

  ctx.action().writeFile({ dst: "version", data: "1" });
};

export default Warp.Toolchain({
  name: "https://pkgs.warp.build/toolchains/node",
  mnemonic: "Node",
  impl,
  cfg: {
    sha1: string(),
    version: string(),
  },
  toolchains: [],
});
