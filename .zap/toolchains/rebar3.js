const impl = (ctx) => {
  const { unarchivedRoot, archiveKind } = ctx.cfg();

  if (archiveKind === "source") {
    ctx.action().runShell({
      script: `#!/bin/bash -xe

      cd ${unarchivedRoot}
      ./boostrap
    `,
    });
  }

  const REBAR3 = File.join(unarchivedRoot, "rebar3");
  ctx.provides({ REBAR3 });
  ctx.action().declareOutputs([]);
};

export default Zap.Toolchain({
  name: "//zap.build/toolchains:rebar3",
  mnemonic: "Rebar3",
  impl,
});
