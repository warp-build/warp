(() => {
  Warp.Targets.compute({
    label: "{LABEL_NAME}",
    rule: "{RULE_NAME}",
    cfg: {CONFIG},
    deps: {DEPS},
    transitiveDeps: {TRANSITIVE_DEPS},
    platform: {PLATFORM},
  });
})();
