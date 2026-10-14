use deno_ast::{MediaType, ParseParams, TranspileModuleOptions, TranspileOptions};
use deno_core::{op2, JsRuntime, OpState, RuntimeOptions};
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
    let mut runtime = JsRuntime::new(RuntimeOptions {
        extensions: vec![warp_rules::init()],
        ..Default::default()
    });
    runtime
        .execute_script("<warp-rule-prelude>", PRELUDE)
        .map_err(|error| RuleError::Evaluation(error.to_string()))?;
    runtime
        .execute_script("<warp-rule>", source)
        .map_err(|error| RuleError::Evaluation(error.to_string()))?;

    let state = runtime.op_state();
    let plan = state
        .borrow_mut()
        .borrow_mut::<EmittedPlan>()
        .0
        .take()
        .ok_or(RuleError::MissingPlan)?;
    let plan: serde_json::Value =
        serde_json::from_str(&plan).map_err(|error| RuleError::InvalidPlan(error.to_string()))?;
    if !plan.is_object() {
        return Err(RuleError::InvalidPlan(
            "the top-level value must be an object".to_string(),
        ));
    }
    Ok(plan)
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
    const toolchains = (spec.toolchains ?? []).map((toolchain) =>
      typeof toolchain === "string" ? toolchain : toolchain.name
    );

    return Object.freeze({
      name: spec.name,
      mnemonic: spec.mnemonic,
      toolchains,
      plan(input) {
        const config = resolveConfig(spec.cfg, defaults, input);
        let execution;
        let outputs;
        const action = Object.freeze({
          exec({ program, args, env = {}, cwd = null }) {
            if (execution !== undefined) fail("a Warp rule may declare only one exec action");
            if (typeof program !== "string" || !Array.isArray(args) ||
                !args.every((argument) => typeof argument === "string")) {
              fail("exec requires a program and a string[] args");
            }
            execution = { program, arguments: args, env, cwd };
          },
          declareOutputs(value) {
            if (!Array.isArray(value) || !value.every((path) => typeof path === "string")) {
              fail("declareOutputs requires a string[]");
            }
            outputs = value;
          },
        });
        spec.impl(Object.freeze({ cfg: () => config, action: () => action }));
        if (execution === undefined) fail(`rule '${spec.name}' did not declare an exec action`);
        if (outputs === undefined) fail(`rule '${spec.name}' did not declare outputs`);
        return { ...execution, outputs, toolchains };
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
              const rule = Warp.Rule<Config>({
                name: "example",
                mnemonic: "Example",
                cfg: { name: string(), sources: [file()] },
                defaults: { sources: ["main.rs"] },
                toolchains: ["rustc"],
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
        assert_eq!(plan["program"], "rustc");
        assert_eq!(plan["arguments"][0], "main.rs");
        assert_eq!(plan["outputs"][0], "example.out");
        assert_eq!(plan["toolchains"][0], "rustc");
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
