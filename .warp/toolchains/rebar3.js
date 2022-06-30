import ErlangToolchain from "./erlang.js";

const impl = (ctx) => {
  const { url, sha1 } = ctx.cfg();

  const REBAR3 = "rebar3.exe"

  ctx.action().download({ url, sha1, output: REBAR3 })
  ctx.action().setPermissions({ file: REBAR3, executable: true })
  ctx.action().declareOutputs([REBAR3]);
  ctx.action().declareRunScript(REBAR3);

  ctx.provides({ REBAR3 });
};

export default Warp.Toolchain({
  name: "//warp.build/toolchains:rebar3",
  mnemonic: "Rebar3",
  impl,
  cfg: {
    url: string(),
    sha1: string(),
  },
  defaults: {
    url: "https://s3.amazonaws.com/rebar3/rebar3",
  },
  toolchains: [ErlangToolchain]
});
