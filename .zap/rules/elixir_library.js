import {TAR_EXT} from "../rules/archive.js";
import ElixirToolchain, {BEAM_EXT, EX_EXT} from "../toolchains/elixir.js";

const impl = ctx => {
  const { label, name, deps, srcs, elixirc_opts} = ctx.cfg();


  const prefix = Label.path(label)
  const relativeRoot = prefix.split('/').map(_ => `..`).join("/")

  // NOTE(@ostera): since most elixir libraries are built around their root
  // mix.exs, we gotta make sure our paths are relative to that too. Here we do
  // this by dropping the absolute prefix of the label form every source path.
  // This means that for the library `//my/app:lib` with one source file
  // `hello.ex`, the full source path will be `//my/app/hello.ex` and the the
  // prefix will be `//my/lib/`. We want the source path to be just
  // `./hello.ex` then.

  const outputs = srcs
    .map(label => {
      let modName = label
        .replace(prefix+"/lib/", "")
        .split("/")
        .map(part => part[0].toUpperCase() + part.slice(1))
        .join(".")
      return File.join(prefix, File.withExtension(`Elixir.${modName}`, BEAM_EXT))
    });
  ctx.action().declareOutputs(outputs);

  const transitiveDeps = ctx.transitiveDeps()
  transitiveDeps.forEach(dep => {
    dep.outs.forEach(out => {
      if (out.endsWith(TAR_EXT)) {
        ctx.action().exec({
          cmd: "tar",
          args: ["xf", File.filename(out)],
          cwd: Label.path(dep.label)
        })
      }
    })
  });

  const require = transitiveDeps
    .flatMap(dep => dep.srcs)
    .filter(src => src.endsWith(EX_EXT))

  const { ELIXIR, ELIXIRC } = ElixirToolchain.provides()
  ctx.action().runShell({
    script: `#!/bin/bash -xe

${ELIXIRC} \
  ${elixirc_opts.join(" ")} \
  ${srcs.join(" ")} \
  $(${ELIXIRC.replace("elixirc","elixir")} /Users/ostera/repos/github.com/AbstractMachinesLab/zap-cloud/tools/elixirdep/elixirdep.ex -- ${require.join(" ")} | xargs)
`
  });
};

export default Zap.Rule({
  name: "elixir_library",
  mnemonic: "ExLibrary",
  impl,
  cfg: {
    name: label(),
    deps: [label()],
    srcs: [file()],
    elixirc_opts: [string()],
  },
	defaults: {
    srcs: [ "*.ex", "lib/**/*.ex" ],
		deps: [],
    elixirc_opts: [ "--warnings-as-errors" ],
	},
  toolchains: [ElixirToolchain]
});
