import { TAR_EXT } from "https://rules.warp.build/rules/archive.js";
import ElixirToolchain, {
  EX_EXT,
} from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain, {
  BEAM_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { target, name, deps, srcs, modules, elixirc_opts } = ctx.cfg();

  const prefix = Target.path(target);
  const relativeRoot = prefix
    .split("/")
    .map((_) => `..`)
    .join("/");

  // NOTE(@ostera): we are enforcing some naming conventions here, and 1 module
  // per .ex file. We could also just request a list of module names that are
  // being defined.
  let outputs = modules.map((name) => `${prefix}/Elixir.${name}.beam`);
  if (outputs.length == 0) {
    outputs = srcs.map((target) => {
      let [path, ext] = target.replace(prefix + "/lib/", "").split(".");

      let modPath = path
        .split("/")
        .map((part) => part[0].toUpperCase() + part.slice(1))
        .map((part) =>
          part
            .split("_")
            .map((word) => word[0].toUpperCase() + word.slice(1))
            .join("")
        )
        .reduce((acc, part) => {
          if (acc.length == 0) {
            return [part];
          }
          if (acc[acc.length - 1] == part) {
            return acc;
          }
          return acc.concat([part]);
        }, []);

      let modName = modPath.join(".") + `.${ext}`;

      return File.join(
        prefix,
        File.withExtension(`Elixir.${modName}`, BEAM_EXT),
      );
    });
  }
  ctx.action().declareOutputs(outputs);

  const transitiveDeps = ctx.transitiveDeps();
  const elixirLibraries = transitiveDeps.filter(
    (dep) => dep.ruleName == "https://rules.warp.build/rules/elixir_library",
  );
  const mixLibraries = transitiveDeps.filter(
    (dep) => dep.ruleName == "https://rules.warp.build/rules/mix_library",
  );

  const extraPaths = [
    ...mixLibraries
      .map(
        (dep) =>
          `${Target.path(dep.target)}/_build/prod/lib/${
            Target.name(
              dep.target,
            )
          }/ebin`,
      )
      .unique(),
    ...elixirLibraries.flatMap((dep) =>
      dep.outs
        .filter((out) => out.endsWith(BEAM_EXT))
        .map((path) => File.parent(path))
        .unique()
    ),
  ]
    .flatMap((path) => ["-pa", path])
    .join(" ");

  mixLibraries.forEach((dep) => {
    dep.outs.forEach((out) => {
      if (out.endsWith(TAR_EXT)) {
        ctx.action().exec({
          cmd: "tar",
          args: ["xf", File.filename(out)],
          cwd: Target.path(dep.target),
        });
      }
    });
  });

  ctx.action().runShell({
    script: `

elixirc \
  ${extraPaths} \
  ${elixirc_opts.join(" ")} \
  -o ${prefix} \
  ${srcs.join(" ")}
`,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/elixir_library",
  mnemonic: "ExLibrary",
  impl,
  cfg: {
    name: target(),
    deps: [target()],
    srcs: [file()],
    elixirc_opts: [string()],
    modules: [string()],
  },
  defaults: {
    srcs: ["*.ex", "lib/**/*.ex"],
    deps: [],
    modules: [],
    elixirc_opts: ["--warnings-as-errors"],
  },
  toolchains: [ElixirToolchain, ErlangToolchain],
});
