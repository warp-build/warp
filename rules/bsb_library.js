import ReScriptToolchain, {MERLIN, JS_EXT, RES_EXT, RESI_EXT, CMT_EXT, CMI_EXT} from "https://pkgs.warp.build/toolchains/rescript.js";

const impl = ctx => {
  const { label, name, deps, srcs, } = ctx.cfg();
  const cwd = Label.path(label)

  const res = srcs.filter(src => src.endsWith(RES_EXT))
  const resi = srcs.filter(src => src.endsWith(RESI_EXT))

  const cmi =
    resi.map(src => File.withExtension(src, CMI_EXT))
        .map(src => File.changeRoot(src, `${cwd}/lib/bs`))

  const cmt =
    res.map(src => File.withExtension(src, CMT_EXT))
        .map(src => File.changeRoot(src, `${cwd}/lib/bs`))

  const js =
    res.map(src => File.withExtension(src, JS_EXT))
        .map(src => File.changeRoot(src, `${cwd}/lib/js`))

  const merlin = `${cwd}/${MERLIN}`

  const outputs = [
    merlin,
    ...cmi,
    ...cmt,
    ...js,
  ]
  ctx.action().declareOutputs(outputs);

  const { rescript } = ReScriptToolchain.provides()
  ctx.action().runShell({
    script: `#!/bin/bash

cd ${cwd}
${rescript} build

`,
  })
};

export default Warp.Rule({
  name: "https://pkgs.warp.build/rules/bsb_library",
  mnemonic: "BsbLib",
  impl,
  cfg: {
    name: label(),
    srcs: [file()],
    deps: [label()],
  },
  defaults: {
    srcs: [
      "src/**/*.res",
      "src/**/*.resi",
      "bsconfig.json",
    ],
    deps: [],
  },
  toolchains: [ReScriptToolchain]
});

