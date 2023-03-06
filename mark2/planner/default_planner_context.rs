use crate::rules::SharedJsContext;

#[derive(Debug, Clone)]
pub struct DefaultPlannerContext;

impl DefaultPlannerContext {
    pub fn into_js_ctx(&self) -> SharedJsContext {
        todo!()
    }
}
