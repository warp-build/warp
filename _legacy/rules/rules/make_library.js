const impl = (ctx) => {
  const {
    target,
    name,
    needs_configure,
    configure_file,
    configure_flags,
    make_opts,
    make_flags,
    targets,
    outs,
  } = ctx.cfg();

  const root = Target.path(target);
  ctx.action().declareOutputs(outs.map((out) => File.join(root, out)));

  ctx.action().runShell({
    script: `#!/bin/bash -xe

cd ${Target.path(target)}

${
      needs_configure === "true"
        ? `${configure_file} ${configure_flags.join(" ")}`
        : ""
    }

make ${make_opts.join(" ")} ${targets.join(" ")}

`,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/make_library",
  mnemonic: "MakeLib",
  impl,
  cfg: {
    name: target(),
    srcs: [file()],
    outs: [string()],
    deps: [target()],
    needs_configure: string(),
    configure_file: string(),
    configure_flags: [string()],
    make_opts: [string()],
    targets: [string()],
  },
  defaults: {
    srcs: ["./**/*"],
    outs: [],
    deps: [],
    needs_configure: "false",
    configure_file: "./configure",
    configure_flags: [],
    make_opts: [],
    targets: ["clean", "all"],
  },
  sandbox: {
    mode: "copy",
  },
  toolchains: [],
});
