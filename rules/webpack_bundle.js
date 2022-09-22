import NodeToolchain from "https://pkgs.warp.build/toolchains/node.js";
import WebpackToolchain from "https://pkgs.warp.build/toolchains/webpack.js";

const impl = (ctx) => {
  const { cfg, srcs, output } = ctx.cfg();

  ctx.action().runShell({
    script: `
      webpack  
    `,
  });

  ctx.action().declareOutputs([output]);
};

export default Warp.Rule({
  name: "https://pkgs.warp.build/rules/webpack_bundle",
  mnemonic: "Webpack Bundle",
  impl,
  cfg: {
    cfg: file(),
    srcs: [file()],
    output: string(),
  },
  defaults: {
    output: "dist",
  },
  toolchains: [NodeToolchain, WebpackToolchain],
});
