import {TAR_EXT} from "../rules/archive.js";
import ElixirToolchain, {EX_EXT} from "../toolchains/elixir.js";
import ErlangToolchain, {BEAM_EXT} from "../toolchains/erlang.js";

const impl = ctx => {
  const { label, name, deps, srcs, elixirc_opts} = ctx.cfg();


  const prefix = Label.path(label)
  const relativeRoot = prefix.split('/').map(_ => `..`).join("/")

  // NOTE(@ostera): we are enforcing some naming conventions here, and 1 module
  // per .ex file. We could also just request a list of module names that are
  // being defined.
  const outputs = srcs
    .map(label => {
      let modName = label
        .replace(prefix+"/lib/", "")
        .split("/")
        .map(part => part[0].toUpperCase() + part.slice(1))
        .join(".")
      return File.join(prefix, File.withExtension(`Elixir.${modName}`, BEAM_EXT))
    });
  ctx.action().declareOutputs([...outputs, ...srcs]);

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

  const extraPaths = transitiveDeps
    .flatMap(dep => [
      `${Label.path(dep.label)}/_build/dev/lib/${Label.name(dep.label)}/ebin`,
      ...dep.outs.filter(out => out.endsWith(BEAM_EXT)) .map(path => File.parent(path)),
    ])
    .flatMap(path => ["-pa", path])
    .join(" ")

  const { ELIXIR, ELIXIRC } = ElixirToolchain.provides()
  ctx.action().runShell({
    script: `#!/bin/bash -xe

${ELIXIRC} \
  ${extraPaths} \
  ${elixirc_opts.join(" ")} \
  -o ${Label.path(label)} \
  ${srcs.join(" ")}
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
  toolchains: [ElixirToolchain, ErlangToolchain]
});
