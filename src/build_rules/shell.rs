use crate::build_plan::BuildContext;
use crate::build_rules::build_rule::BuildRule;
use crate::model::target::Label;
use log::{debug, info};
use std::collections::HashSet;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct Shell {
    name: Label,
    dependencies: Vec<Label>,
}

impl Shell {
    pub fn new(name: Label) -> Shell {
        Shell {
            name,
            dependencies: vec![],
        }
    }

    pub fn set_name(&self, name: Label) -> Shell {
        Shell {
            name,
            ..self.clone()
        }
    }

    pub fn set_dependencies(&self, dependencies: Vec<Label>) -> Shell {
        Shell {
            dependencies,
            ..self.clone()
        }
    }

    pub fn name(&self) -> Label {
        self.name.clone()
    }

    pub fn dependencies(&self) -> Vec<Label> {
        self.dependencies.clone()
    }

    pub fn outputs(&self, ctx: &BuildContext) -> Vec<PathBuf> {
        vec![]
    }

    pub fn execute(&self, ctx: &mut BuildContext) -> Result<(), anyhow::Error> {
        let wrapped = BuildRule::Shell(self.clone());
        let code_paths: HashSet<PathBuf> = ctx
            .transitive_dependencies(&wrapped)
            .iter()
            .flat_map(|dep| dep.outputs(&ctx))
            .map(|path| path.parent().unwrap().to_path_buf())
            .collect();

        ctx.toolchain().shell(&code_paths.into_iter().collect())
    }
}
