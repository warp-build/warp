import ErlangToolchain, {
  BEAM_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";
import GleamToolchain, {
  GLEAM_EXT,
} from "https://rules.warp.build/toolchains/gleam.js";

const impl = (ctx) => {
  const { target, name, deps } = ctx.cfg();

  ctx.action().declareOutputs([]);

  ctx.action().runShell({
    script: `#!/bin/bash -xe
    ${GleamToolchain.provides().GLEAM} build
    `,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/gleam_package",
  mnemonic: "GleamPackage",
  impl,
  cfg: {
    name: target(),
    deps: [target()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [GleamToolchain, ErlangToolchain],
});
