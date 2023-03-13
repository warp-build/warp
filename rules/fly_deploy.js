import FlyCtlToolchain from "https://rules.warp.build/toolchains/flyctl.js";

const impl = (ctx) => {
  const { target, name } = ctx.cfg();

  const run = `${Target.path(target)}/${name}.fly_deploy`;

  ctx.action().writeFile({
    dst: run,
    data: `#!/bin/bash -e

cd ${Target.path(target)}
flyctl deploy

`,
  });
  ctx.action().setPermissions({ file: run, executable: true });
  ctx.action().declareOutputs([run]);
  ctx.action().declareRunScript(run);
};

export default Warp.Rule({
  runnable: true,
  name: "https://rules.warp.build/rules/fly_deploy",
  mnemonic: "FlyDeploy",
  impl,
  cfg: {
    name: target(),
    toolchains: [target()],
  },
  toolchains: [FlyCtlToolchain],
});
