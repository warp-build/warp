import NodeToolchain from "https://pkgs.warp.build/toolchains/node.js";

const impl = (ctx) => {
  const { version } = ctx.cfg();

  ctx.action().runShell({
    script: `
      npm i webpack-cli@${version}  
    `,
  });

  ctx.action().declareOutputs(["node_modules"]);

  ctx.provides({ webpack: "node_modules/.bin/webpack" });

  ctx.action().writeFile({ dst: "version", data: "1" });
};

export default Warp.Toolchain({
  name: "https://pkgs.warp.build/toolchains/webpack",
  mnemonic: "Webpack",
  impl,
  cfg: {
    version: string(),
  },
  toolchains: [NodeToolchain],
});
