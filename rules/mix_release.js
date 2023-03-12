import ElixirToolchain from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain from "https://rules.warp.build/toolchains/erlang.js";

const RULE_NAME = "https://rules.warp.build/rules/mix_release";

const impl = (ctx) => {
  const { cwd } = ctx.cfg();

  const outputs = [`${cwd()}/_build/default/lib`];

  ctx.action().declareOutputs(outputs);

  ctx.action().runShell({
    env: { MIX_ENV: "default" },
    script: `
    export LANG=C.UTF-8

    cd ${cwd()}
    
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
