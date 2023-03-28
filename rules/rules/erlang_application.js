import ErlangToolchain, {
  BEAM_EXT,
  ERL_EXT,
  HEADER_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { target, name, app_src, mod, apps } = ctx.cfg();

  const ebin = File.join(Target.path(target), "ebin");

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
      `${Target.path(target)}/${name}.ebin.tar`,
    ]);

  ctx.action().runShell({
    script: `#/bin/bash -xe

mkdir -p ${ebin}

mv ${beams.join(" ")} ${ebin}

`,
  });

  ctx.action().runShell({
    script: `#/bin/bash -xe

tar cf ${Target.path(target)}/${name}.ebin.tar ${Target.path(target)}/ebin

`,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/erlang_application",
  mnemonic: "ErlApp",
  impl,
  cfg: {
    name: target(),
    app_src: file(),
    deps: [target()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [ErlangToolchain],
});
