import ElixirToolchain, {EX_EXT} from "../toolchains/elixir.js";
import ErlangToolchain, {BEAM_EXT} from "../toolchains/erlang.js";

const impl = ctx => {
  const { label, name, app_name, mod, apps} = ctx.cfg();

  const ebin = File.join(Label.path(label), "ebin")

  const beams =
    ctx.deps()
    .flatMap( dep => dep.outs )
    .filter( out => out.endsWith(BEAM_EXT) )

  const ebinBeams =
    beams
    .map( out => File.join(ebin, File.filename(out)) )

  const appFile = `${ebin}/${app_name}.app`

  ctx.action().declareOutputs([
    ...ebinBeams,
    appFile,
    `${Label.path(label)}/${name}.ebin.tar`
  ]);


  ctx.action().runShell({
    script: `#/bin/bash -xe

mkdir ${ebin}

mv ${beams.join(" ")} ${ebin}

` })

  ctx.action().writeFile({
    dst: appFile,
    data: `
{application, '${app_name}', [
  {description, ""},
  {vsn, "0.0.0"},
  {mod, {'Elixir.${mod}', []}},
  {applications, [${apps.join(",")}]}
]}.
`
  })

  ctx.action().runShell({
    script: `#/bin/bash -xe

tar cf ${Label.path(label)}/${name}.ebin.tar ${Label.path(label)}/ebin

` })

};

export default Zap.Rule({
  name: "elixir_application",
  mnemonic: "ExApp",
  impl,
  cfg: {
    name: label(),
    app_name: string(),
    deps: [label()],
    mod: string(),
    apps: [string()],
  },
	defaults: {
    apps: ["kernel", "stdlib"],
		deps: [],
	},
  toolchains: [ElixirToolchain, ErlangToolchain]
});

