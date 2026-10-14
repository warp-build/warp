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

/// Evaluate a synchronous JavaScript build rule in a runtime without host I/O.
/// The rule must call `Warp.emit(plan)` exactly once.
pub fn evaluate(source: &str) -> Result<serde_json::Value, RuleError> {
    let mut runtime = JsRuntime::new(RuntimeOptions {
        extensions: vec![warp_rules::init()],
        ..Default::default()
    });
    runtime
        .execute_script("<warp-rule-prelude>", PRELUDE)
        .map_err(|error| RuleError::Evaluation(error.to_string()))?;
    runtime
        .execute_script("<warp-rule>", source.to_string())
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

const PRELUDE: &str = r#"
Object.defineProperty(globalThis, "Warp", {
  configurable: false,
  enumerable: true,
  writable: false,
  value: Object.freeze({
    emit(plan) {
      const json = JSON.stringify(plan);
      if (typeof json !== "string") {
        throw new TypeError("Warp.emit requires a JSON-serializable plan");
      }
      Deno.core.ops.op_warp_emit_plan(json);
    },
  }),
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
