// NOTE(@ostera): HTTP vs HTTPS is relevant here -- our mock server can't do HTTPS
import * as Dep from "{URL}/dep_rule.js";

const impl = (ctx) => {
  const { srcs } = ctx.cfg();
  ctx.action().declareOutputs(srcs);
};

export default Warp.Rule({
  name: "{URL}/rule_with_dep",
  mnemonic: "RuleWithDep",
  impl,
  cfg: {
    name: target(),
    srcs: [file()],
  },
});
