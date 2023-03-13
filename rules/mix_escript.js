import ElixirToolchain from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain from "https://rules.warp.build/toolchains/erlang.js";

const RULE_NAME = "https://rules.warp.build/rules/mix_escript";

const impl = (ctx) => {
  const { cwd, bin } = ctx.cfg();

  const binPath = `${cwd()}/${bin}`;
  const outputs = [binPath];

  ctx.action().declareOutputs(outputs);
  ctx.provides({ [bin]: binPath });

  ctx.action().runShell({
    env: { MIX_ENV: "default" },
    script: `
    cd ${cwd()}
    mix deps.get --force
    mix escript.build
    `,
  });
};

export default Warp.Rule({
  name: RULE_NAME,
  mnemonic: "MixScript",
  impl,
  cfg: {
    name: target(),
    bin: string(),
    srcs: [file()],
  },
  defaults: {
    srcs: [
      "config/**/*.ex",
      "config/**/*.exs",
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
