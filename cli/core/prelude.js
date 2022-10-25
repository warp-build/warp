Array.prototype.unique = function() {
  var arr = [];
  for (var i = 0; i < this.length; i++) {
    if (arr.indexOf(this[i]) === -1) {
      arr.push(this[i]);
    }
  }
  return arr;
}

const ffi = (name, args) => Deno.core.opSync(name, args);
const panic = x => {
  if (typeof x === "object") {
    x = JSON.stringify(x, null, 2)
  }
  throw new Error(x);
};

const label = () => "label";
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

Warp.Rules.exists = name => __RULES[name] !== null && __RULES[name] !== undefined;

Warp.Rules.register = (name, spec) => {
  __RULES[name] = spec;
};

Warp.Rules.getByName = name => {
    if (Warp.Rules.exists(name)) return __RULES[name];
    panic(`Expected rule ${name} but could not find it in store!`);
};


Warp.Targets = {};

Warp.Targets.compute = target => {
  const label = target.label;
  const rule = Warp.Rules.getByName(target.rule);

  const config = Object.fromEntries(Object.entries(rule.cfg)
    .map( ([k, type]) => {
      const value = target.cfg[k] === undefined ? rule.defaults[k] : target.cfg[k];
      if (value === undefined) panic(`Expected target  ${target.label}  to have config key '${k}' (of type ${type}) but it was not present in your Build.toml, and it doesn't have a default value.`);
      return [k, value];
    }));

  config.label = label;
  config.cwd = () => Label.path(label);

  const ctx = {
    cfg: () => config,
    deps: () => target.deps,
    transitiveDeps: () => target.transitiveDeps,

    // TODO(@ostera): build this entire object on the Rust side
    env: () => ({
      host: {
        triple: target.env.platform,
        arch: target.env.platform.split("-")[0],
        os: target.env.platform.endsWith("darwin") ? "darwin" :
            target.env.platform.endsWith("linux-gnu") ? "linux" :
            target.env.platform.endsWith("win32") ? "win32" : "unknown",
      }
    }),

    path: path => `{{NODE_STORE_PATH}}/${path}`,

    provides: provides => ffi("op_ctx_declare_provides", {label, provides}),

    setEnv: (env = {}) => ffi("op_ctx_declare_env", {label, env}),

    action: () => ({
      copy: ({src, dst}) => ffi("op_ctx_actions_copy", {label, src, dst}),

      declareOutputs: outs => ffi("op_ctx_actions_declare_outputs", {label, outs}),

      declareRunScript: (runScript, opts = { env: {} }) =>
        ffi("op_ctx_actions_declare_run_script", {label, runScript, env: opts.env}),

      download: ({url, sha1, output}) => ffi("op_ctx_download", {label, url, sha1, output}),

      exec: ({env = {}, cmd, args, cwd, needsTty = false}) => ffi("op_ctx_actions_exec", {label, cmd, args, cwd, env, needsTty}),

      extract: ({src, dst}) => ffi("op_ctx_extract", {label, src, dst}),

      runShell: ({script, env = {}, needsTty = false}) => ffi("op_ctx_actions_run_shell", {label, script, env, needsTty}),

      setPermissions: ({file, executable}) => ffi("op_ctx_set_permissions", {label, file, executable}),

      writeFile: ({data, dst}) => ffi("op_ctx_actions_write_file", {label, data, dst}),
    }),
  };

  rule.impl(ctx)
};


const check_config = cfg => {
  return Object.fromEntries(Object.entries(cfg).map(([k, t]) => {
    if (Array.isArray(t)) {
      if (t.length == 0) panic(`Cfg map for rule ${name} found an empty list. Did you mean to use   [label()]   ?`);
      return [k, `list_of_${t[0]}`];
    } else {
      return [k, t];
    }
  }));
};

Warp.Rule = spec => {
  const name = spec.name;
  if (!name) panic(`Rule must have a string name`);
  if (typeof name !== "string") panic(`Rule name must be a string, instead found: ${name}`);
  if (name.length < 5) panic(`Rule name "${name}" should be at least 5 characters long`);

  const mnemonic = spec.mnemonic;
  if (!mnemonic) panic(`Rule must have a string mnemonic`);
  if (typeof mnemonic !== "string") panic(`Rule mnemonic must be a string, instead found: ${mnemonic}`);

  const impl = spec.impl;
  if (!impl) panic(`Rule ${name} must have an implementation.`);
  if (typeof impl !== "function") panic(`Rule ${name} implementation should be a function, instead found: ${typeof impl}`);

  const cfg = spec.cfg;
  if (!cfg) panic(`Rule ${name} must define a config map with   cfg   `);
  if (Object.entries(cfg).length == 0) panic(`Config map for rule ${name} is empty! Try adding a   name: label()   key?`);
  spec.cfg = check_config(spec.cfg);

  spec.toolchains = (spec.toolchains || []).map( toolchain => toolchain.name );
  spec.defaults = spec.defaults || {};
  spec.kind = spec.name.endsWith("_test") ? "test" : spec.runnable ? "run" : "build";
  spec.pinned = spec.pinned || false;
  spec.portable = spec.portable || false;
  spec.sandbox = spec.sandbox || { mode: "link" };

  Warp.Rules.register(name, spec);

  ffi("op_rule_new", spec);

  return spec;
};


// NOTE(@ostera): Toolchains are actually just Rules on this side, since we'll need
// to invoke them like any other Rule later on in `Warp.Target.compute`
//
Warp.Toolchain = spec => {
  const name = spec.name;
  if (!name) panic(`Toolchain must have a string name.`);
  if (typeof name !== "string") panic(`Toolchain name must be a string, instead found: ${name}.`);
  if (name === "") panic(`Toolchain name was empty! Here's some inspiration:   super_lang  `);

  const impl = spec.impl;
  if (!impl) panic(`Rule ${name} must have an implementation.`);
  if (typeof impl !== "function") panic(`Rule ${name} implementation should be a function, instead found: ${typeof impl}`);

  const cfg = spec.cfg;
  if (!cfg) panic(`Rule ${name} must define a config map with   cfg   `);
  if (Object.entries(cfg).length == 0) panic(`Config map for rule ${name} is empty! Try adding a   name: label()   key?`);
  spec.cfg = check_config(spec.cfg);

  spec.defaults = spec.defaults || {};
  spec.toolchains = spec.toolchains || []
  spec.toolchains = spec.toolchains.map( toolchain => toolchain.name );

  spec.provides = () => ffi("op_ctx_fetch_provides", {label: name});
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

File.parent = (path) => ffi("op_file_parent", path);
File.filename = (path) => ffi("op_file_filename", path);
File.withExtension = (path, ext) => ffi("op_file_with_extension", {path, ext});
File.join = (a, b) => `${a}/${b}`
File.changeRoot = (path, root) => {
  let parts = path.split("/");
  parts.shift(1)
  return `${root}/${parts.join("/")}`
}

const Label = {};

Label.path = (label) => ffi("op_label_path", label);
Label.name = (label) => ffi("op_label_name", label);
