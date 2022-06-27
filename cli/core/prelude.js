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
const err = x => {
  throw new Error(x);
};

const label = () => "label";
const string = () => "string";
const file = () => "file";

const console = {
  log: (...args) => ffi("op_log", args)
};

/*******************************************************************************
 * Global stores
 ******************************************************************************/
const __RULES = {};

/*******************************************************************************
 *
 * Zap's Rule and Toolchain API
 *
 ******************************************************************************/

const Zap = {};


Zap.Rules = {};

Zap.Rules.exists = name => __RULES[name] !== null && __RULES[name] !== undefined;

Zap.Rules.register = (name, spec) => {
  __RULES[name] = spec;
};

Zap.Rules.getByName = name => {
    if (Zap.Rules.exists(name)) return __RULES[name];
    err(`Expected rule ${name} but could not find it in store!`);
};


Zap.Targets = {};

Zap.Targets.compute = target => {
  const label = target.label;
  const rule = Zap.Rules.getByName(target.rule);

  const config = Object.fromEntries(Object.entries(rule.cfg)
    .map( ([k, type]) => {
      const value = target.cfg[k] === undefined ? rule.defaults[k] : target.cfg[k];
      if (value === undefined) err(`Expected target  ${target.label}  to have config key '${k}' (of type ${type}) but it was not present in your Build.toml, and it doesn't have a default value.`);
      return [k, value];
    }));

  config.label = label;

  const ctx = {
    cfg: () => config,
    deps: () => target.deps,
    transitiveDeps: () => target.transitiveDeps,

    platform: {
      os: target.platform.endsWith("darwin") ? "darwin" :
          target.platform.endsWith("linux-gnu") ? "linux" :
          target.platform.endsWith("win32") ? "win32" : "unknown",
    },

    provides: provides => ffi("op_ctx_declare_provides", {label, provides}),

    action: () => ({
      runShell: ({script, env = {}, needsTty = false}) => ffi("op_ctx_actions_run_shell", {label, script, env, needsTty}),
      declareOutputs: outs => ffi("op_ctx_actions_declare_outputs", {label, outs}),
      declareRunScript: runScript => ffi("op_ctx_actions_declare_run_script", {label, runScript}),
      exec: ({env = {}, cmd, args, cwd, needsTty = false}) => ffi("op_ctx_actions_exec", {label, cmd, args, cwd, env, needsTty}),
      copy: ({src, dst}) => ffi("op_ctx_actions_copy", {label, src, dst}),
      writeFile: ({data, dst}) => ffi("op_ctx_actions_write_file", {label, data, dst}),
    }),
  };

  rule.impl(ctx)
};


const check_config = cfg => {
  return Object.fromEntries(Object.entries(cfg).map(([k, t]) => {
    if (Array.isArray(t)) {
      if (t.length == 0) err(`Cfg map for rule ${name} found an empty list. Did you mean to use   [label()]   ?`);
      return [k, `list_of_${t[0]}`];
    } else {
      return [k, t];
    }
  }));
};

Zap.Rule = spec => {
  const name = spec.name;
  if (!name) err(`Rule must have a string name`);
  if (typeof name !== "string") err(`Rule name must be a string, instead found: ${name}`);
  if (name.length < 5) err(`Rule name "${name}" should be at least 5 characters long`);

  const mnemonic = spec.mnemonic;
  if (!mnemonic) err(`Rule must have a string mnemonic`);
  if (typeof mnemonic !== "string") err(`Rule mnemonic must be a string, instead found: ${mnemonic}`);

  const impl = spec.impl;
  if (!impl) err(`Rule ${name} must have an implementation.`);
  if (typeof impl !== "function") err(`Rule ${name} implementation should be a function, instead found: ${typeof impl}`);

  const cfg = spec.cfg;
  if (!cfg) err(`Rule ${name} must define a config map with   cfg   `);
  if (Object.entries(cfg).length == 0) err(`Config map for rule ${name} is empty! Try adding a   name: label()   key?`);
  spec.cfg = check_config(spec.cfg);

  spec.toolchains = (spec.toolchains || []).map( toolchain => toolchain.name );
  spec.defaults = spec.defaults || {};
  spec.runnable = spec.runnable || false;

  // if (Zap.Rules.exists(name)) err(`There already exists rule toolchain called ${name}, consider renaming yours`);
  Zap.Rules.register(name, spec);


  ffi("op_rule_new", spec);

  return spec;
};


// NOTE(@ostera): Toolchains are actually just Rules on this side, since we'll need
// to invoke them like any other Rule later on in `Zap.Target.compute`
//
Zap.Toolchain = spec => {
  const name = spec.name;
  if (!name) err(`Toolchain must have a string name.`);
  if (typeof name !== "string") err(`Toolchain name must be a string, instead found: ${name}.`);
  if (name === "") err(`Toolchain name was empty! Here's some inspiration:   super_lang  `);

  const impl = spec.impl;
  if (!impl) err(`Rule ${name} must have an implementation.`);
  if (typeof impl !== "function") err(`Rule ${name} implementation should be a function, instead found: ${typeof impl}`);

  const cfg = spec.cfg;
  if (!cfg) err(`Rule ${name} must define a config map with   cfg   `);
  if (Object.entries(cfg).length == 0) err(`Config map for rule ${name} is empty! Try adding a   name: label()   key?`);
  spec.cfg = check_config(spec.cfg);

  spec.defaults = spec.defaults || {};
  spec.toolchains = spec.toolchains || []
  spec.toolchains = spec.toolchains.map( toolchain => toolchain.name );

  spec.provides = () => ffi("op_ctx_fetch_provides", {label: name}),
  spec.runnable = false;

  // if (Zap.Rules.exists(name)) err(`There already exists a toolchain called ${name}, consider renaming yours`);
  Zap.Rules.register(name, spec);

  ffi("op_toolchain_new", spec);

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
