import TerraformToolchain from "https://rules.warp.build/toolchains/terraform.js";

const impl = (ctx) => {
  const { target, name } = ctx.cfg();

  const run = `${Target.path(target)}/${name}.terraform_apply`;

  ctx.action().writeFile({
    dst: run,
    data: `#!/bin/bash -e

terraform apply

`,
  });
  ctx.action().setPermissions({ file: run, executable: true });
  ctx.action().declareOutputs([run]);
  ctx.action().declareRunScript(run);
};

export default Warp.Rule({
  runnable: true,
  name: "https://rules.warp.build/rules/terraform_apply",
  mnemonic: "TerraformApply",
  impl,
  cfg: {
    name: target(),
    srcs: [file()],
  },
  defaults: {
    srcs: ["**/*.tf"],
  },
  toolchains: [TerraformToolchain],
});
