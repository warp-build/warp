import FlyCtlToolchain from "../toolchains/flyctl.js";

const impl = ctx => {
  const { label, name } = ctx.cfg();

  const { FLYCTL } = FlyCtlToolchain.provides()

  const run = `${Label.path(label)}/${name}.fly_deploy`

  ctx.action().writeFile({
    dst: run,
    data: `#!/bin/bash -e

cd ${Label.path(label)}
${FLYCTL} deploy

`
  })
  ctx.action().setPermissions({file: run, executable: true})
  ctx.action().declareOutputs([run])
  ctx.action().declareRunScript(run)
};

export default Warp.Rule({
  runnable: true,
  name: "fly_deploy",
  mnemonic: "FlyDeploy",
  impl,
  cfg: {
    name: label(),
    toolchains: [label()],
  },
  toolchains: [FlyCtlToolchain]
});

