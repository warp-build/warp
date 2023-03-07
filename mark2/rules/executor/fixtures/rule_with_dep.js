// NOTE(@ostera): HTTP vs HTTPS is relevant here -- our mock server can't do HTTPS
import * as Dep from "http://127.0.0.1:{PORT}/dep_rule.js";

const impl = (ctx) => {
  const { srcs } = ctx.cfg();
  ctx.action().declareOutputs(srcs);
};

export default Warp.Rule({
  name: "rule_with_dep",
  mnemonic: "RuleWithDep",
  impl,
  cfg: {
    name: target(),
    srcs: [file()],
  },
});
