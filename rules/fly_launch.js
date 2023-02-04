import FlyCtlToolchain from "https://rules.warp.build/toolchains/flyctl.js";

const impl = (ctx) => {
  const { label, name } = ctx.cfg();

  const run = `${Label.path(label)}/${name}.fly_launch`;

  ctx.action().writeFile({
    dst: run,
    data: `#!/bin/bash -e

cd ${Label.path(label)}
flyctl launch

`,
  });
  ctx.action().setPermissions({ file: run, executable: true });
  ctx.action().declareOutputs([run]);
  ctx.action().declareRunScript(run);
};

export default Warp.Rule({
  runnable: true,
  name: "https://rules.warp.build/rules/fly_launch",
  mnemonic: "FlyLaunch",
  impl,
  cfg: {
    name: label(),
  },
  toolchains: [FlyCtlToolchain],
});
