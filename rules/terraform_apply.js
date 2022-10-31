import TerraformToolchain from "https://rules.warp.build/toolchains/terraform.js";

const impl = ctx => {
  const { label, name } = ctx.cfg();

  const { TERRAFORM } = TerraformToolchain.provides()

  const run = `${Label.path(label)}/${name}.terraform_apply`

  ctx.action().writeFile({
    dst: run,
    data: `#!/bin/bash -e

${TERRAFORM} apply

`
  })
  ctx.action().setPermissions({file: run, executable: true})
  ctx.action().declareOutputs([run])
  ctx.action().declareRunScript(run)
};

export default Warp.Rule({
  runnable: true,
  name: "https://rules.warp.build/rules/terraform_apply",
  mnemonic: "TerraformApply",
  impl,
  cfg: {
    name: label(),
    srcs: [file()],
  },
  defaults: {
    srcs: ["**/*.tf"],
  },
  toolchains: [TerraformToolchain]
});

