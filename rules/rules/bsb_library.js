import ReScriptToolchain, {
  CMI_EXT,
  CMT_EXT,
  JS_EXT,
  MERLIN,
  RES_EXT,
  RESI_EXT,
} from "https://rules.warp.build/toolchains/rescript.js";

const impl = (ctx) => {
  const { target, name, deps, srcs } = ctx.cfg();
  const cwd = Target.path(target);

  const res = srcs.filter((src) => src.endsWith(RES_EXT));
  const resi = srcs.filter((src) => src.endsWith(RESI_EXT));

  const cmi = resi
    .map((src) => File.withExtension(src, CMI_EXT))
    .map((src) => `${cwd}/lib/bs/${src}`);

  const cmt = res
    .map((src) => File.withExtension(src, CMT_EXT))
    .map((src) => `${cwd}/lib/bs/${src}`);

  const js = res
    .map((src) => File.withExtension(src, JS_EXT))
    .map((src) => `${cwd}/lib/js/${src}`);

  const merlin = `${cwd}/${MERLIN}`;

  const outputs = [merlin, ...cmi, ...cmt, ...js];
  ctx.action().declareOutputs(outputs);

  ctx.action().runShell({
    script: `#!/bin/bash

cd ${cwd}
rescript build

`,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/bsb_library",
  mnemonic: "BsbLib",
  impl,
  cfg: {
    name: target(),
    srcs: [file()],
    deps: [target()],
  },
  defaults: {
    srcs: ["src/**/*.res", "src/**/*.resi", "bsconfig.json"],
    deps: [],
  },
  toolchains: [ReScriptToolchain],
});
