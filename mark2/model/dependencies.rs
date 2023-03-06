use super::TargetId;

#[derive(Debug, Default, Clone)]
pub struct Dependencies {
    toolchains: Vec<TargetId>,
    compile_deps: Vec<TargetId>,
    transitive_deps: Vec<TargetId>,
    runtime_deps: Vec<TargetId>,
}

impl Dependencies {
    pub fn toolchains(&self) -> &[TargetId] {
        self.toolchains.as_ref()
    }

    pub fn compile_deps(&self) -> &[TargetId] {
        self.compile_deps.as_ref()
    }

    pub fn transitive_deps(&self) -> &[TargetId] {
        self.transitive_deps.as_ref()
    }

    pub fn runtime_deps(&self) -> &[TargetId] {
        self.runtime_deps.as_ref()
    }
}
