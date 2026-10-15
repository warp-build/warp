use deno_ast::{MediaType, ParseParams, TranspileModuleOptions, TranspileOptions};
use deno_core::{op2, JsRuntime, OpState, RuntimeOptions};
use std::cell::RefCell;
#[derive(Debug, PartialEq, Eq)]
pub enum RuleError {
    Evaluation(String),
    MissingPlan,
    InvalidPlan(String),
}

impl std::fmt::Display for RuleError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Evaluation(message) => write!(formatter, "JavaScript rule failed: {message}"),
            Self::MissingPlan => formatter.write_str("JavaScript rule did not emit a plan"),
            Self::InvalidPlan(message) => write!(
                formatter,
                "JavaScript rule emitted an invalid plan: {message}"
            ),
        }
    }
}

impl std::error::Error for RuleError {}

#[derive(Default)]
struct EmittedPlan(Option<String>);

#[op2(fast)]
fn op_warp_emit_plan(
    state: &mut OpState,
    #[string] plan: String,
) -> Result<(), deno_error::JsErrorBox> {
    let emitted = state.borrow_mut::<EmittedPlan>();
    if emitted.0.is_some() {
        return Err(deno_error::JsErrorBox::type_error(
            "a Warp rule may emit exactly one plan",
        ));
    }
    emitted.0 = Some(plan);
    Ok(())
}

deno_core::extension!(
    warp_rules,
    ops = [op_warp_emit_plan],
    state = |state| state.put(EmittedPlan::default())
);

/// Transpile the supported TypeScript subset and evaluate a synchronous build rule without host I/O.
/// The rule must call `Warp.emit(plan)` exactly once. This transpiles syntax but does not type-check it.
pub fn evaluate(source: &str) -> Result<serde_json::Value, RuleError> {
    let source = transpile_typescript(source)?;
    RULE_RUNTIME.with(|runtime| {
        let mut runtime = runtime.try_borrow_mut().map_err(|_| {
            RuleError::Evaluation("rule runtime is already evaluating a rule".to_string())
        })?;
        if runtime.is_none() {
            *runtime = Some(RuleRuntime::new()?);
        }
        runtime
            .as_mut()
            .expect("rule runtime was initialized")
            .evaluate(&source)
    })
}

thread_local! {
    static RULE_RUNTIME: RefCell<Option<RuleRuntime>> = const { RefCell::new(None) };
}

struct RuleRuntime {
    tokio_runtime: tokio::runtime::Runtime,
    javascript: Option<JsRuntime>,
}

impl RuleRuntime {
    fn new() -> Result<Self, RuleError> {
        let tokio_runtime = tokio::runtime::Builder::new_current_thread()
            .enable_time()
            .build()
            .map_err(|error| RuleError::Evaluation(format!("create rule runtime: {error}")))?;
        let _tokio_guard = tokio_runtime.enter();
        let mut javascript = JsRuntime::new(RuntimeOptions {
            extensions: vec![warp_rules::init()],
            ..Default::default()
        });
        javascript
            .execute_script("<warp-rule-prelude>", PRELUDE)
            .map_err(|error| RuleError::Evaluation(error.to_string()))?;
        Ok(Self {
            tokio_runtime,
            javascript: Some(javascript),
        })
    }

    fn evaluate(&mut self, source: &str) -> Result<serde_json::Value, RuleError> {
        let _tokio_guard = self.tokio_runtime.enter();
        let javascript = self
            .javascript
            .as_mut()
            .expect("JavaScript runtime is present until RuleRuntime is dropped");
        let state = javascript.op_state();
        state.borrow_mut().borrow_mut::<EmittedPlan>().0 = None;
        let source = format!("(() => {{\n{source}\n}})();");
        javascript
            .execute_script("<warp-rule>", source)
            .map_err(|error| RuleError::Evaluation(error.to_string()))?;

        let state = javascript.op_state();
        let plan = state
            .borrow_mut()
            .borrow_mut::<EmittedPlan>()
            .0
            .take()
            .ok_or(RuleError::MissingPlan)?;
        let plan: serde_json::Value = serde_json::from_str(&plan)
            .map_err(|error| RuleError::InvalidPlan(error.to_string()))?;
        if !plan.is_object() {
            return Err(RuleError::InvalidPlan(
                "the top-level value must be an object".to_string(),
            ));
        }
        Ok(plan)
    }
}

impl Drop for RuleRuntime {
    fn drop(&mut self) {
        self.javascript.take();
    }
}

fn transpile_typescript(source: &str) -> Result<String, RuleError> {
    let specifier = deno_core::ModuleSpecifier::parse("file:///warp-rule.ts")
        .expect("static TypeScript rule specifier is valid");
    let parsed = deno_ast::parse_module(ParseParams {
        specifier,
        text: source.to_string().into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        scope_analysis: false,
        maybe_syntax: None,
    })
    .map_err(|error| RuleError::Evaluation(error.to_string()))?;
    let transpiled = parsed
        .transpile(
            &TranspileOptions::default(),
            &TranspileModuleOptions { module_kind: None },
            &deno_ast::EmitOptions::default(),
        )
        .map_err(|error| RuleError::Evaluation(error.to_string()))?;
    Ok(transpiled.into_source().text)
}

const PRELUDE: &str = r#"
const fail = (message) => { throw new TypeError(message); };
const string = () => "string";
const file = () => "file";
const target = () => "target";

const matches = (value, type) => {
  if (Array.isArray(type)) {
    return type.length === 1 && Array.isArray(value) &&
      value.every((element) => matches(element, type[0]));
  }
  if (type === "string" || type === "file" || type === "target") {
    return typeof value === "string";
  }
  return false;
};

const describe = (type) => Array.isArray(type)
  ? `list of ${describe(type[0])}`
  : type;

const resolveConfig = (schema, defaults, input) => {
  if (input === null || typeof input !== "object" || Array.isArray(input)) {
    fail("Warp.Rule configuration must be an object");
  }
  const config = {};
  for (const [key, type] of Object.entries(schema)) {
    const value = Object.hasOwn(input, key) ? input[key] : defaults[key];
    if (value === undefined) fail(`missing required rule config '${key}'`);
    if (!matches(value, type)) {
      fail(`rule config '${key}' must be ${describe(type)}`);
    }
    config[key] = value;
  }
  return config;
};

const Warp = Object.freeze({
  Toolchain(spec) {
    if (spec === null || typeof spec !== "object" || Array.isArray(spec)) {
      fail("Warp.Toolchain requires a specification object");
    }
    if (typeof spec.name !== "string" || spec.name.length === 0) {
      fail("Warp.Toolchain requires a non-empty name");
    }
    if (typeof spec.program !== "string" || spec.program.length === 0) {
      fail(`toolchain '${spec.name}' requires a program`);
    }
    const versionArgs = spec.versionArgs ?? ["--version"];
    if (!Array.isArray(versionArgs) || !versionArgs.every((arg) => typeof arg === "string")) {
      fail(`toolchain '${spec.name}' versionArgs must be string[]`);
    }
    const config = spec.config ?? {};
    if (config === null || typeof config !== "object" || Array.isArray(config)) {
      fail(`toolchain '${spec.name}' config must be an object`);
    }
    return Object.freeze({
      __warpToolchain: true,
      name: spec.name,
      program: spec.program,
      versionArgs: Object.freeze([...versionArgs]),
      config,
    });
  },
  emit(plan) {
    const json = JSON.stringify(plan);
    if (typeof json !== "string") {
      throw new TypeError("Warp.emit requires a JSON-serializable plan");
    }
    Deno.core.ops.op_warp_emit_plan(json);
  },
  Rule(spec) {
    if (typeof spec.name !== "string" || spec.name.length === 0) {
      fail("Warp.Rule requires a non-empty name");
    }
    if (typeof spec.mnemonic !== "string" || spec.mnemonic.length === 0) {
      fail(`rule '${spec.name}' requires a mnemonic`);
    }
    if (typeof spec.impl !== "function") fail(`rule '${spec.name}' requires impl(ctx)`);
    if (spec.cfg === null || typeof spec.cfg !== "object" || Array.isArray(spec.cfg)) {
      fail(`rule '${spec.name}' requires a cfg schema`);
    }
    const defaults = spec.defaults ?? {};
    const toolchains = (spec.toolchains ?? []).map((toolchain) => {
      if (toolchain === null || typeof toolchain !== "object" ||
          toolchain.__warpToolchain !== true) {
        fail(`rule '${spec.name}' toolchains must be Warp.Toolchain values`);
      }
      return {
        name: toolchain.name,
        program: toolchain.program,
        version_args: toolchain.versionArgs,
        config: toolchain.config,
      };
    });

    return Object.freeze({
      name: spec.name,
      mnemonic: spec.mnemonic,
      toolchains,
      plan(input) {
        const config = resolveConfig(spec.cfg, defaults, input);
        const executions = [];
        let outputs;
        const action = Object.freeze({
          exec({ program, args, env = {}, cwd = null }) {
            if (typeof program !== "string" || !Array.isArray(args) ||
                !args.every((argument) => typeof argument === "string")) {
              fail("exec requires a program and a string[] args");
            }
            if (env === null || typeof env !== "object" || Array.isArray(env) ||
                !Object.values(env).every((value) => typeof value === "string")) {
              fail("exec env must be a string-to-string object");
            }
            if (cwd !== null && typeof cwd !== "string") {
              fail("exec cwd must be a string or null");
            }
            executions.push({ program, arguments: args, env, cwd });
          },
          declareOutputs(value) {
            if (!Array.isArray(value) || !value.every((path) => typeof path === "string")) {
              fail("declareOutputs requires a string[]");
            }
            outputs = value;
          },
        });
        spec.impl(Object.freeze({ cfg: () => config, action: () => action }));
        if (executions.length === 0) fail(`rule '${spec.name}' did not declare an exec action`);
        if (outputs === undefined) fail(`rule '${spec.name}' did not declare outputs`);
        return { executions, outputs, toolchains };
      },
    });
  },
});

const NativeDate = Date;
globalThis.Date = class WarpDeterministicDate extends NativeDate {
  constructor(...args) {
    if (args.length === 0) {
      throw new Error("new Date() is unavailable in deterministic Warp rules");
    }
    super(...args);
  }
  static now() {
    throw new Error("Date.now is unavailable in deterministic Warp rules");
  }
};

Object.defineProperty(Math, "random", {
  configurable: false,
  enumerable: false,
  writable: false,
  value() {
    throw new Error("Math.random is unavailable in deterministic Warp rules");
  },
});

"#;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn evaluates_javascript_rule_and_returns_structured_plan() {
        let plan = evaluate(
            r#"
              const names = ["warp", "engine"];
              Warp.emit({ targets: names.map((name) => ({ name })) });
            "#,
        )
        .unwrap();
        assert_eq!(plan["targets"][0]["name"], "warp");
        assert_eq!(plan["targets"][1]["name"], "engine");
    }

    #[test]
    fn independent_rules_evaluate_concurrently_with_tokio_contexts() {
        let workers = (0..4)
            .map(|worker| {
                std::thread::spawn(move || {
                    for index in 0..4 {
                        let plan = evaluate(&format!(
                            "Warp.emit({{ worker: {worker}, index: {index} }});"
                        ))
                        .unwrap();
                        assert_eq!(plan["worker"], worker);
                        assert_eq!(plan["index"], index);
                    }
                })
            })
            .collect::<Vec<_>>();
        for worker in workers {
            worker.join().unwrap();
        }
    }

    #[test]
    fn transpiles_typescript_rule_annotations_before_evaluation() {
        let plan = evaluate(
            r#"
              type Inputs = { name: string };
              const label = (inputs: Inputs): string => inputs.name;
              Warp.emit({ label: label({ name: "rust" }) });
            "#,
        )
        .unwrap();
        assert_eq!(plan["label"], "rust");
    }

    #[test]
    fn legacy_shaped_rule_schema_builds_an_action_plan() {
        let plan = evaluate(
            r#"
              interface Config { name: string; sources: string[] }
              const Rustc = Warp.Toolchain({ name: "rustc", program: "rustc" });
              const rule = Warp.Rule<Config>({
                name: "example",
                mnemonic: "Example",
                cfg: { name: string(), sources: [file()] },
                defaults: { sources: ["main.rs"] },
                toolchains: [Rustc],
                impl(ctx) {
                  const config = ctx.cfg();
                  ctx.action().exec({ program: "rustc", args: config.sources });
                  ctx.action().declareOutputs([config.name]);
                },
              });
              Warp.emit(rule.plan({ name: "example.out" }));
            "#,
        )
        .unwrap();
        assert_eq!(plan["executions"][0]["program"], "rustc");
        assert_eq!(plan["executions"][0]["arguments"][0], "main.rs");
        assert_eq!(plan["outputs"][0], "example.out");
        assert_eq!(plan["toolchains"][0]["name"], "rustc");
        assert_eq!(plan["toolchains"][0]["program"], "rustc");
        assert_eq!(plan["toolchains"][0]["version_args"][0], "--version");
    }

    #[test]
    fn rules_can_declare_ordered_commands_for_multi_step_toolchains() {
        let plan = evaluate(
            r#"
              const Bun = Warp.Toolchain({ name: "bun", program: "bun" });
              const rule = Warp.Rule({
                name: "multi-step",
                mnemonic: "MultiStep",
                cfg: { root: string() },
                toolchains: [Bun],
                impl(ctx) {
                  const root = ctx.cfg().root;
                  ctx.action().exec({ program: "bun", args: ["install"], cwd: root });
                  ctx.action().exec({ program: "bun", args: ["run", "build"], cwd: root });
                  ctx.action().declareOutputs(["dist"]);
                },
              });
              Warp.emit(rule.plan({ root: "/workspace/site" }));
            "#,
        )
        .unwrap();
        assert_eq!(plan["executions"].as_array().unwrap().len(), 2);
        assert_eq!(plan["executions"][0]["arguments"][0], "install");
        assert_eq!(plan["executions"][1]["arguments"][1], "build");
        assert_eq!(plan["executions"][1]["cwd"], "/workspace/site");
        assert_eq!(plan["toolchains"][0]["name"], "bun");
    }

    #[test]
    fn rejects_string_toolchains_without_a_toolchain_declaration() {
        let error = evaluate(
            r#"
              const rule = Warp.Rule({
                name: "bad-toolchain",
                mnemonic: "BadToolchain",
                cfg: { root: string() },
                toolchains: ["bun"],
                impl(ctx) {
                  ctx.action().exec({ program: "bun", args: ["run", "build"] });
                  ctx.action().declareOutputs(["dist"]);
                },
              });
              Warp.emit(rule.plan({ root: "/workspace/site" }));
            "#,
        )
        .unwrap_err();
        assert!(matches!(error, RuleError::Evaluation(_)));
    }

    #[test]
    fn rejects_rules_that_emit_no_plan_or_emit_multiple_plans() {
        assert_eq!(evaluate("const answer = 42;"), Err(RuleError::MissingPlan));
        let error = evaluate("Warp.emit({}); Warp.emit({});").unwrap_err();
        assert!(matches!(error, RuleError::Evaluation(_)));
    }

    #[test]
    fn rejects_non_object_and_non_json_plans() {
        assert!(matches!(
            evaluate("Warp.emit([1, 2, 3]);"),
            Err(RuleError::InvalidPlan(_))
        ));
        assert!(matches!(
            evaluate("Warp.emit(undefined);"),
            Err(RuleError::Evaluation(_))
        ));
    }

    #[test]
    fn blocks_ambient_io_and_nondeterministic_sources() {
        assert!(matches!(
            evaluate("Warp.emit({ value: Deno.env.get('SECRET') });"),
            Err(RuleError::Evaluation(_))
        ));
        assert!(matches!(
            evaluate("Warp.emit({ value: Math.random() });"),
            Err(RuleError::Evaluation(_))
        ));
        assert!(matches!(
            evaluate("Warp.emit({ value: Date.now() });"),
            Err(RuleError::Evaluation(_))
        ));
    }
}
