import ErlangToolchain, {HEADER_EXT, BEAM_EXT, ERL_EXT} from "../toolchains/erlang.js";

const impl = ctx => {
  const { name, deps, srcs, headers, behaviors } = ctx.cfg();

  ctx.action().declareOutputs([]);
};

export default Warp.Rule({
  name: "otp_release",
  mnemonic: "OtpRel",
  impl,
  cfg: {
    name: label(),
    config: file(),
    deps: [label()],
  },
	defaults: {
		deps: [],
	},
  toolchains: [ErlangToolchain]
});
