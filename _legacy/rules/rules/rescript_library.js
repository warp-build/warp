import ReScriptToolchain, {
  AST_EXT,
  CMI_EXT,
  CMT_EXT,
  JS_EXT,
  MERLIN,
  RES_EXT,
  RESI_EXT,
} from "https://rules.warp.build/toolchains/rescript.js";

const impl = (ctx) => {
  const { target, name, deps, srcs, cwd, suffix, store_path } = ctx.cfg();

  const res = srcs.find((src) => src.endsWith(RES_EXT));

  const resi = srcs.find((src) => src.endsWith(RESI_EXT));

  const cmi = File.withExtension(resi, CMI_EXT);

  const cmt = File.withExtension(res, CMT_EXT);

  const js = File.withExtension(res, suffix);

  const outputs = [cmi, cmt, js];

  ctx.action().declareOutputs(outputs);

  ctx.action().runShell({
    script: `#!/bin/bash

cd ${cwd()}
bsc ${store_path()}/${res} > ${js}


`,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/rescript_library",
  mnemonic: "RescriptLib",
  impl,
  cfg: {
    name: target(),
    srcs: [file()],
    deps: [target()],
    suffix: string(),
  },
  defaults: {
    srcs: [],
    deps: [],
  },
  toolchains: [ReScriptToolchain],
});
