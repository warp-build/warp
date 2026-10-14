const rule = (configuration) => {
  const dependencyArguments = configuration.dependencies.flatMap((dependency) => [
    "--extern",
    `${dependency.name}=${dependency.path}`,
  ]);
  const featureArguments = configuration.features.flatMap((feature) => [
    "--cfg",
    `feature=${JSON.stringify(feature)}`,
  ]);

  return {
    program: "rustc",
    arguments: [
      "--crate-name",
      configuration.crate_name,
      "--crate-type",
      configuration.crate_type,
      "--edition",
      configuration.edition,
      "--emit=link",
      ...configuration.compiler_arguments,
      ...featureArguments,
      ...dependencyArguments,
      configuration.source,
    ],
  };
};

Warp.emit(rule(configuration));
