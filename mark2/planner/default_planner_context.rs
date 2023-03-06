use crate::rules::SharedJsContext;

#[derive(Debug, Clone, Default)]
pub struct DefaultPlannerContext;

impl DefaultPlannerContext {}

impl From<DefaultPlannerContext> for SharedJsContext {
    fn from(value: DefaultPlannerContext) -> Self {
        SharedJsContext::default()
    }
}
