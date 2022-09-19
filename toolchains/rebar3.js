import ErlangToolchain from "https://pkgs.warp.build/toolchains/erlang.js";
import CMakeToolchain from "https://pkgs.warp.build/toolchains/cmake.js";

const impl = (ctx) => {
  const { url, sha1 } = ctx.cfg();

  const rebar3 = "rebar3"

  ctx.action().download({ url, sha1, output: rebar3 })
  ctx.action().setPermissions({ file: rebar3, executable: true })
  ctx.action().declareOutputs([rebar3]);
  ctx.action().writeFile({dst: "version",  data: "1" });

  ctx.provides({ rebar3 });

  ctx.setEnv({
    REBAR3_VERSION: sha1
  });

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
  toolchains: [ErlangToolchain, CMakeToolchain]
});
