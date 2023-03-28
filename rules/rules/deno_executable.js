import DenoToolchain, {
  JS_EXT,
} from "https://rules.warp.build/toolchains/deno.js";

const impl = (ctx) => {
  const { target, name, deps, src, permissions } = ctx.cfg();
  const cwd = Target.path(target);

  const bin = `${cwd}/${name}`;
  ctx.action().declareOutputs([bin]);
  ctx.action().declareRunScript(bin);

  ctx.action().runShell({
    script: `#!/bin/bash

cd ${cwd}
deno compile ${permissions.join(" ")} ${File.filename(src)}

`,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/deno_executable",
  mnemonic: "DenoExe",
  impl,
  cfg: {
    name: target(),
    src: file(),
    deps: [target()],
    permissions: [string()],
  },
  defaults: {
    deps: [],
    permissions: [],
  },
  toolchains: [DenoToolchain],
});
