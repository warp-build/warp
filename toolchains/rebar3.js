import ErlangToolchain from "https://pkgs.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { url, sha1 } = ctx.cfg();

  const rebar3 = "rebar3.exe"

  ctx.action().download({ url, sha1, output: rebar3 })
  ctx.action().setPermissions({ file: rebar3, executable: true })
  ctx.action().declareOutputs([rebar3]);
  ctx.action().declareRunScript(rebar3);

  ctx.provides({ rebar3 });
};

export default Warp.Toolchain({
  name: "https://pkgs.warp.build/toolchains/rebar3",
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
