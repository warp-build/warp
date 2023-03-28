import ElixirToolchain, {
  EX_EXT,
} from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain, {
  BEAM_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { target, name, app_name, mod, apps } = ctx.cfg();

  const ebin = File.join(Target.path(target), "ebin");

  const beams = ctx
    .deps()
    .flatMap((dep) => dep.outs)
    .filter((out) => out.endsWith(BEAM_EXT));

  const ebinBeams = beams.map((out) => File.join(ebin, File.filename(out)));

  const appFile = `${ebin}/${app_name}.app`;

  ctx
    .action()
    .declareOutputs([
      ...ebinBeams,
      appFile,
      `${Target.path(target)}/${name}.ebin.tar`,
    ]);

  ctx.action().runShell({
    script: `#/bin/bash -xe

mkdir -p ${ebin}

mv ${beams.join(" ")} ${ebin}

`,
  });

  ctx.action().writeFile({
    dst: appFile,
    data: `
{application, '${app_name}', [
  {description, ""},
  {vsn, "0.0.0"},
  {mod, {'Elixir.${mod}', []}},
  {applications, [${apps.join(",")}]}
]}.
`,
  });

  ctx.action().runShell({
    script: `#/bin/bash -xe

tar cf ${Target.path(target)}/${name}.ebin.tar ${Target.path(target)}/ebin

`,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/elixir_application",
  mnemonic: "ExApp",
  impl,
  cfg: {
    name: target(),
    app_name: string(),
    config: file(),
    deps: [target()],
    mod: string(),
    apps: [string()],
  },
  defaults: {
    apps: ["kernel", "stdlib"],
    deps: [],
    config: "config.exs",
  },
  toolchains: [ElixirToolchain, ErlangToolchain],
});
