import ErlangToolchain, {
  HEADER_EXT,
  BEAM_EXT,
  ERL_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { label, name, app_src, mod, apps } = ctx.cfg();

  const ebin = File.join(Label.path(label), "ebin");

  const beams = ctx
    .deps()
    .flatMap((dep) => dep.outs)
    .filter((out) => out.endsWith(BEAM_EXT));

  const ebinBeams = beams.map((out) => File.join(ebin, File.filename(out)));

  ctx
    .action()
    .declareOutputs([
      ...ebinBeams,
      app_src,
      `${Label.path(label)}/${name}.ebin.tar`,
    ]);

  ctx.action().runShell({
    script: `#/bin/bash -xe

mkdir -p ${ebin}

mv ${beams.join(" ")} ${ebin}

`,
  });

  ctx.action().runShell({
    script: `#/bin/bash -xe

tar cf ${Label.path(label)}/${name}.ebin.tar ${Label.path(label)}/ebin

`,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/erlang_application",
  mnemonic: "ErlApp",
  impl,
  cfg: {
    name: label(),
    app_src: file(),
    deps: [label()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [ErlangToolchain],
});
