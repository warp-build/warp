import FireflyToolchain from "https://pkgs.warp.build/toolchains/firefly.js";

const impl = ctx => {
  const { cwd, label, name, src, deps, opt_level } = ctx.cfg();

  ctx.action().declareOutputs([ name ]);

  ctx.action().runShell({
    script: `

firefly compile ${File.parent(src)}

`,
  })

  ctx.provides({
    [name]: name
  })
};

export default Warp.Rule({
  name: "https://pkgs.warp.build/rules/firefly_binary",
  mnemonic: "FireflyExe",
  impl,
  cfg: {
    name: label(),
    src: file(),
    deps: [label()],
    opt_level: string(),
  },
  defaults: {
    opt_level: "3",
    deps: [],
  },
  toolchains: [FireflyToolchain]
});

