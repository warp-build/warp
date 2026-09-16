import ErlangToolchain from "https://rules.warp.build/toolchains/erlang.js";
import CMakeToolchain from "https://rules.warp.build/toolchains/cmake.js";
import GitToolchain from "https://rules.warp.build/toolchains/git.js";

const impl = (ctx) => {
  const { url, sha256 } = ctx.cfg();

  const rebar3 = "rebar3";

  ctx.action().download({ url, sha256, output: rebar3 });
  ctx.action().setPermissions({ file: rebar3, executable: true });
  ctx.action().declareOutputs([rebar3]);
  ctx.action().writeFile({ dst: "version", data: "1" });

  ctx.provides({ rebar3 });

  ctx.setEnv({
    REBAR3_VERSION: sha256,
  });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/rebar3",
  mnemonic: "Rebar3",
  impl,
  cfg: {
    url: string(),
    sha256: string(),
  },
  defaults: {
    url: "https://s3.amazonaws.com/rebar3/rebar3",
    sha256: "7e281994851487caf5daa4a2c32b5a0cabdf7bdb",
  },
  toolchains: [ErlangToolchain, CMakeToolchain, GitToolchain],
});
