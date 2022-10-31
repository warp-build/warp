import DenoToolchain, {JS_EXT} from "https://rules.warp.build/toolchains/deno.js";

const impl = ctx => {
  const { label, name, deps, src, permissions } = ctx.cfg();
  const cwd = Label.path(label)

  const bin = `${cwd}/${name}`
  ctx.action().declareOutputs([bin]);
  ctx.action().declareRunScript(bin);

  const { DENO } = DenoToolchain.provides()
  ctx.action().runShell({
    script: `#!/bin/bash

cd ${cwd}
${DENO} compile ${permissions.join(" ")} ${File.filename(src)}

`,
  })
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/deno_executable",
  mnemonic: "DenoExe",
  impl,
  cfg: {
    name: label(),
    src: file(),
    deps: [label()],
    permissions: [string()],
  },
  defaults: {
    deps: [],
    permissions: [],
  },
  toolchains: [DenoToolchain]
});
