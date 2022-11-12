import { TAR_EXT } from "https://rules.warp.build/rules/archive.js";
import ElixirToolchain from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain, { BEAM_EXT } from "https://rules.warp.build/toolchains/erlang.js";

const RULE_NAME = "https://rules.warp.build/rules/mix_library"

const impl = (ctx) => {
  const { cwd, label, name, deps, srcs, skip_deps, deps_args, compile_args } = ctx.cfg();

  const outputs = [
    `${cwd()}/_build/default/lib/${name}`,
    `${cwd()}/deps`
  ]
  if (skip_deps === "false") {
    outputs.push(`${cwd()}/mix.lock`);
    outputs.push(`${cwd()}/_build/default/lib`);
  }
  ctx.action().declareOutputs(outputs);

  let depsGet = "";
  if (skip_deps === "false") {
    depsGet = `mix deps.get --only \$MIX_ENV ${deps_args.join(" ")}`;
  }

  if (skip_deps === "true") {
    compile_args.push("--no-deps-check");
  }

  const transitiveDeps = ctx.transitiveDeps();

  const beamLibraries = transitiveDeps.filter(dep =>
    (dep.ruleName == "https://rules.warp.build/rules/elixir_library")
    || (dep.ruleName == "https://rules.warp.build/rules/erlang_library")
  );

  const mixLibraries =
    transitiveDeps
    .filter(dep => 
      (dep.ruleName == "https://rules.warp.build/rules/mix_library")
      || (dep.ruleName == "https://rules.warp.build/rules/rebar3_library")
      || (dep.ruleName == "https://rules.warp.build/rules/erlangmklibrary")
    );

  const protobufLibraries =
    transitiveDeps
    .filter(dep => dep.ruleName == "https://rules.warp.build/rules/elixir_proto_library");

  ctx.action().runShell({
    env: { MIX_ENV: "default" },
    script: `

# NOTE(@ostera): handle protobuf libraries
mkdir -p ${cwd()}/lib/generated/
${
  protobufLibraries.flatMap((dep) => {
    return dep.outs.flatMap((out) => {
      return `cp ${out} ${cwd()}/lib/generated/`
    })
  }).join("\n")
}

# NOTE(@ostera): handle other mix libraries
${
  mixLibraries.flatMap((dep) => {
    return `cp -R ${Label.path(dep.label)}/_build ${cwd()}/`
  }).join("\n")
}

# NOTE(@ostera): handle raw beam libraries
mkdir -p ${cwd()}/src
${
  beamLibraries.flatMap((dep) => {
    return dep.outs.map((out) => {
      return `cp -R ${out} ${cwd()}/src/`
    });
  }).join("\n")
}

cd ${cwd()}
${depsGet}

mkdir -p deps/${name}
cp mix.exs deps/${name}/mix.exs

mix compile ${compile_args.join(" ")}

`,
  });
};

export default Warp.Rule({
  name: RULE_NAME,
  mnemonic: "MixLib",
  impl,
  cfg: {
    name: label(),
    srcs: [file()],
    extra_srcs: [file()],
    deps: [label()],
    deps_args: [string()],
    skip_deps: string(),
    compile_args: [string()],
  },
  defaults: {
    srcs: [
      "lib/**/*.eex",
      "lib/**/*.ex",
      "mix.exs",
      "src/**/*.app.src",
      "src/**/*.erl",
      "src/**/*.hrl",
      "include/**/*.hrl",
      "priv/**/*",
    ],
    extra_srcs: [],
    deps: [],
    deps_args: [],
    skip_deps: "true",
    compile_args: [],
  },
  toolchains: [ElixirToolchain, ErlangToolchain],
});
