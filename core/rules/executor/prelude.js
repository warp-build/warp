Array.prototype.unique = function () {
  return [...new Set(this)];
};

let trace = () => {};

let _FFI_CALL_COUNT = 0;
const ffi = (name, args) => {
  trace(
    `FFI Call #${_FFI_CALL_COUNT++} ${name} with arguments: ${JSON.stringify(
      args,
      null,
      2
    )}\n`
  );
  return Deno.core.ops[name].call({}, args);
};

const panic = (x) => {
  if (typeof x === "object") {
    x = JSON.stringify(x, null, 2);
  }
  throw new Error(x);
};

if (ffi("op_env_var", "WARP_LOG") === "DEBUG") {
  trace = (...args) => {
    Deno.core.print(args.join(" ") + "\n");
  };
}

const target = () => "target";
const string = () => "string";
const file = () => "file";

/*******************************************************************************
 * Global stores
 ******************************************************************************/
const __RULES = {};

/*******************************************************************************
 *
 * Warp's Rule and Toolchain API
 *
 ******************************************************************************/

const Warp = {};

Warp.Rules = {};

Warp.Rules.exists = (name) =>
  __RULES[name] !== null && __RULES[name] !== undefined;

Warp.Rules.register = (name, spec) => {
  __RULES[name] = spec;
};

Warp.Rules.getByName = (name) => {
  if (Warp.Rules.exists(name)) return __RULES[name];
  panic(`Expected rule ${name} but could not find it in store!`);
};

Warp.Signatures = {};

Warp.Signatures.compute = (inputSignature) => {
  trace(`computing signature: ${JSON.stringify(inputSignature.target)}`);
  const signature = inputSignature;

  const target = signature.target;

  trace(`getting rule: ${signature.rule}`);
  const rule = Warp.Rules.getByName(signature.rule);

  const ruleConfig = Object.fromEntries(
    Object.entries(rule.cfg).map(([k, type]) => {
      const value =
        signature.cfg[k] === undefined ? rule.defaults[k] : signature.cfg[k];
      if (value === undefined) {
        panic(
          `Expected signature  ${signature.target}  to have config key '${k}' (of type ${type}) but it was not present in your Build.toml, and it doesn't have a default value.`
        );
      }
      return [k, value];
    })
  );
  const config = JSON.parse(JSON.stringify(ruleConfig));
  trace(`with config: ${JSON.stringify(config, null, 2)}`);

  config.target = target;
  config.cwd = () => Target.dir(target);
  config.store_path = () => "{{NODE_STORE_PATH}}";

  let _provides = null;
  let _env = null;
  let _actions = [];

  const ctx = {
    cfg: () => config,
    deps: () => signature.deps,
    transitiveDeps: () => signature.transitiveDeps,
    runtimeDeps: () => signature.runtimeDeps,

    // TODO(@ostera): build this entire object on the Rust side
    env: () => ({
      host: {
        triple: signature.env.platform,
        arch: signature.env.platform.split("-")[0],
        os: signature.env.platform.endsWith("darwin")
          ? "darwin"
          : signature.env.platform.endsWith("linux-gnu")
          ? "linux"
          : signature.env.platform.endsWith("win32")
          ? "win32"
          : "unknown",
      },
    }),

    path: (path) => `{{NODE_STORE_PATH}}/${path}`,

    provides: (provides) => {
      _provides = provides;
    },

    setEnv: (env = {}) => {
      _env = env;
    },

    action: () => ({
      copy: ({ src, dst }) =>
        _actions.push({
          ffi: "op_ctx_actions_copy",
          data: { target, src, dst },
        }),

      declareOutputs: (outs) =>
        _actions.push({
          ffi: "op_ctx_actions_declare_outputs",
          data: { target, outs },
        }),

      declareRunScript: (runScript, opts = { env: {} }) =>
        _actions.push({
          ffi: "op_ctx_actions_declare_run_script",
          data: { target, runScript, env: opts.env },
        }),

      download: ({ url, sha1, output }) =>
        _actions.push({
          ffi: "op_ctx_download",
          data: { target, url, sha1, output },
        }),

      exec: ({ env = {}, cmd, args, cwd, needsTty = false }) =>
        _actions.push({
          ffi: "op_ctx_actions_exec",
          data: { target, cmd, args, cwd, env, needsTty },
        }),

      extract: ({ src, dst }) =>
        _actions.push({ ffi: "op_ctx_extract", data: { target, src, dst } }),

      runShell: ({ script, env = {}, needsTty = false }) =>
        _actions.push({
          ffi: "op_ctx_actions_run_shell",
          data: { target, script, env, needsTty },
        }),

      setPermissions: ({ file, executable }) =>
        _actions.push({
          ffi: "op_ctx_set_permissions",
          data: { target, file, executable },
        }),

      writeFile: ({ data, dst }) =>
        _actions.push({
          ffi: "op_ctx_actions_write_file",
          data: { target, data, dst },
        }),

      verifyChecksum: ({ file, sha1 }) =>
        _actions.push({
          ffi: "op_ctx_verify_checksum",
          data: { target, file, sha1 },
        }),
    }),
  };

  trace(`calling rule implementation`);
  rule.impl(ctx);
  trace(`actions: ${JSON.stringify(_actions, null, 2)}`);
  trace(`env: ${JSON.stringify(_env, null, 2)}`);
  trace(`provides: ${JSON.stringify(_provides, null, 2)}`);

  if (_provides !== null) {
    ffi("op_ctx_declare_provides", { target, provides: _provides });
  }
  if (_env !== null) ffi("op_ctx_declare_env", { target, env: _env });
  _actions.forEach((action) => ffi(action.ffi, action.data));
};

const check_config = (cfg) => {
  return Object.fromEntries(
    Object.entries(cfg).map(([k, t]) => {
      if (Array.isArray(t)) {
        if (t.length == 0) {
          panic(
            `Cfg map for rule ${name} found an empty list. Did you mean to use   [target()]   ?`
          );
        }
        return [k, `list_of_${t[0]}`];
      } else {
        return [k, t];
      }
    })
  );
};

Warp.Rule = (spec) => {
  const name = spec.name;
  if (!name) panic(`Rule must have a string name`);
  if (typeof name !== "string") {
    panic(`Rule name must be a string, instead found: ${name}`);
  }
  if (name.length < 5) {
    panic(`Rule name "${name}" should be at least 5 characters long`);
  }

  const mnemonic = spec.mnemonic;
  if (!mnemonic) panic(`Rule must have a string mnemonic`);
  if (typeof mnemonic !== "string") {
    panic(`Rule mnemonic must be a string, instead found: ${mnemonic}`);
  }

  const impl = spec.impl;
  if (!impl) panic(`Rule ${name} must have an implementation.`);
  if (typeof impl !== "function") {
    panic(
      `Rule ${name} implementation should be a function, instead found: ${typeof impl}`
    );
  }

  const cfg = spec.cfg;
  if (!cfg) panic(`Rule ${name} must define a config map with   cfg   `);
  if (Object.entries(cfg).length == 0) {
    panic(
      `Config map for rule ${name} is empty! Try adding a   name: target()   key?`
    );
  }
  spec.cfg = check_config(spec.cfg);

  spec.toolchains = (spec.toolchains || []).map((toolchain) => toolchain.name);
  spec.defaults = spec.defaults || {};
  spec.pinned = spec.pinned || false;
  spec.portable = spec.portable || false;
  spec.sandbox = spec.sandbox || { mode: "link" };

  spec.kind = "build";
  if (spec.name.endsWith("_test")) {
    spec.kind = "test";
  }
  if (spec.runnable) {
    spec.kind = "run";
  }

  Warp.Rules.register(name, spec);

  ffi("op_rule_new", spec);

  return spec;
};

// NOTE(@ostera): Toolchains are actually just Rules on this side, since we'll need
// to invoke them like any other Rule later on in `Warp.Signature.compute`
//
Warp.Toolchain = (spec) => {
  const name = spec.name;
  if (!name) panic(`Toolchain must have a string name.`);
  if (typeof name !== "string") {
    panic(`Toolchain name must be a string, instead found: ${name}.`);
  }
  if (name === "") {
    panic(`Toolchain name was empty! Here's some inspiration:   super_lang  `);
  }

  const impl = spec.impl;
  if (!impl) panic(`Rule ${name} must have an implementation.`);
  if (typeof impl !== "function") {
    panic(
      `Rule ${name} implementation should be a function, instead found: ${typeof impl}`
    );
  }

  const cfg = spec.cfg;
  if (!cfg) panic(`Rule ${name} must define a config map with   cfg   `);
  if (Object.entries(cfg).length == 0) {
    panic(
      `Config map for rule ${name} is empty! Try adding a   name: target()   key?`
    );
  }
  spec.cfg = check_config(spec.cfg);

  spec.defaults = spec.defaults || {};
  spec.toolchains = spec.toolchains || [];
  spec.toolchains = spec.toolchains.map((toolchain) => toolchain.name);

  spec.kind = "build";

  spec.pinned = true;
  spec.portable = false;

  spec.sandbox = spec.sandbox || { mode: "link" };

  Warp.Rules.register(name, spec);

  ffi("op_rule_new", spec);

  return spec;
};

/*******************************************************************************
 *
 * File APIs
 *
 ******************************************************************************/

const File = {};

File.parent = (path) => {
  trace(`File.parent(${path})`);
  return ffi("op_file_parent", path);
};

File.filename = (path) => {
  return ffi("op_file_filename", path);
};

File.withExtension = (path, ext) => {
  return ffi("op_file_with_extension", { path, ext });
};

File.join = (a, b) => `${a}/${b}`;

File.changeRoot = (path, root) => {
  let parts = path.split("/");
  parts.shift(1);
  return `${root}/${parts.join("/")}`;
};

/*******************************************************************************
 *
 * Target APIs
 *
 ******************************************************************************/

const Target = {};

Target.path = (target) => ffi("op_target_path", target);

Target.dir = (target) => ffi("op_target_dir", target);

Target.name = (target) => ffi("op_target_name", target);
