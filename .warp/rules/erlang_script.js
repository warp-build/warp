import ErlangToolchain, { BEAM_EXT } from "../toolchains/erlang.js";

const impl = (ctx) => {
  const { label, name, srcs, headers, deps, main } = ctx.cfg();

  const { ESCRIPT } = ErlangToolchain.provides();

  const runtime_srcs = srcs
    .concat([main])
    .concat(headers);

  const run = `${Label.path(label)}/run_escript`;

  ctx.action().declareOutputs([run, ...runtime_srcs]);
  ctx.action().declareRunScript(run);
  ctx.action().writeFile({
    dst: run,
    data: `#!/bin/bash -e

${ESCRIPT} \
  ${main} \
  $(dirname "\${BASH_SOURCE[0]}")/${File.filename(main)} $*

`,
  });
  ctx.action().setPermissions({file: run, executable: true});
};

export default Warp.Rule({
  runnable: true,
  name: "erlang_script",
  mnemonic: "ErlScript",
  impl,
  cfg: {
    name: label(),
    main: file(),
    srcs: [file()],
    headers: [file()],
    deps: [label()],
  },
  defaults: {
    srcs: ["*.erl"],
    headers: ["*.hrl"],
    deps: []
  },
  toolchains: [ErlangToolchain],
});
