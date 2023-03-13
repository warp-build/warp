import ElixirToolchain from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain from "https://rules.warp.build/toolchains/erlang.js";

const RULE_NAME = "https://rules.warp.build/rules/mix_release";

const impl = (ctx) => {
  const { cwd, bin } = ctx.cfg();

  const binPath = `${cwd()}/${bin}`;
  const outputs = [
    `${cwd()}/_build/default/rel`,
    binPath,
  ];

  ctx.action().declareOutputs(outputs);

  ctx.provides({
    [bin]: binPath,
  });

  ctx.action().writeFile({
    dst: binPath,
    data: `#!/bin/bash
cd ${cwd()}
exec _build/default/rel/${bin}/bin/${bin} start
    `,
  });

  ctx.action().setPermissions({
    file: binPath,
    executable: true,
  });

  ctx.action().runShell({
    env: { MIX_ENV: "default" },
    script: `
    export LANG=C.UTF-8

    cd ${cwd()}
    
    mix deps.get --force
    mix release

    `,
  });
};

export default Warp.Rule({
  name: RULE_NAME,
  mnemonic: "MixRelease",
  impl,
  cfg: {
    name: target(),
    bin: string(),
    srcs: [file()],
  },
  defaults: {
    srcs: [
      "lib/**/*.eex",
      "lib/**/*.ex",
      "test/**/*.exs",
      "mix.exs",
      "src/**/*.app.src",
      "src/**/*.erl",
      "src/**/*.hrl",
      "include/**/*.hrl",
      "priv/**/*",
    ],
  },
  toolchains: [ElixirToolchain, ErlangToolchain],
});
