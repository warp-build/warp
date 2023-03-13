import NgrokToolchain from "https://rules.warp.build/toolchains/ngrok.js";

const impl = (ctx) => {
  const { target, args } = ctx.cfg();
  ctx.action().declareOutputs([]);
  ctx.action().runShell({
    script: `#!/bin/bash -xe

ngrok ${args.join(" ")}

    `,
  });
};

export default Warp.Rule({
  runnable: true,
  name: "https://rules.warp.build/rules/ngrok_tunnel",
  mnemonic: "NgrokTunel",
  impl,
  cfg: {
    name: target(),
    args: [string()],
    deps: [target()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [NgrokToolchain],
});
