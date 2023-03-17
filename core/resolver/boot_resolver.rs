use super::{ResolverError, TargetRegistry};
use crate::model::{rule, ConcreteTarget, FsTarget, Goal, Signature, TargetId};
use crate::sync::*;
use crate::tricorder::SignatureGenerationFlow;
use crate::workspace::WorkspaceManager;
use tracing::instrument;

#[cfg_attr(doc, aquamarine::aquamarine)]
/// Bootstrap Resolver used to create signatures during the bootstrapping phase of the build
/// system.
///
#[derive(Clone)]
pub struct BootstrapResolver {
    workspace_manager: Arc<WorkspaceManager>,
    target_registry: Arc<TargetRegistry>,
}

impl BootstrapResolver {
    pub fn new(
        workspace_manager: Arc<WorkspaceManager>,
        target_registry: Arc<TargetRegistry>,
    ) -> Self {
        Self {
            workspace_manager,
            target_registry,
        }
    }

    #[instrument(name = "BootstrapResolver::resolve", skip(self))]
    pub async fn resolve(
        &self,
        goal: Goal,
        target_id: TargetId,
        target: &FsTarget,
    ) -> Result<SignatureGenerationFlow, ResolverError> {
        if tokio::fs::metadata(target.path()).await.is_err() {
            return Err(ResolverError::CouldNotFindFile {
                path: target.path().clone(),
            });
        }

        let workspace = self.workspace_manager.current_workspace();
        let final_path = if target.path().starts_with(workspace.root()) {
            target
                .path()
                .strip_prefix(workspace.root())
                .unwrap()
                .to_path_buf()
        } else {
            target.path().to_path_buf()
        };

        let ct = ConcreteTarget::builder()
            .goal(goal)
            .target_id(target_id)
            .target(self.target_registry.get_target(target_id))
            .path(final_path)
            .workspace_root(workspace.root())
            .build()
            .unwrap();

        let ct = self
            .target_registry
            .associate_concrete_target(target_id, ct);

        let sig = match ct.name() {
            "mix.exs" => self.mix_escript((*ct).clone()),
            "Cargo.toml" => self.cargo_binary((*ct).clone()),
            _ => return Ok(SignatureGenerationFlow::IgnoredTarget(target_id)),
        };

        Ok(SignatureGenerationFlow::GeneratedSignatures {
            signatures: vec![sig],
        })
    }

    fn cargo_binary(&self, concrete_target: ConcreteTarget) -> Signature {
        Signature::builder()
            .rule("cargo_binary".to_string())
            .config({
                let mut config = rule::Config::default();
                config.insert("name".to_string(), concrete_target.name().into());
                config.insert("bin".to_string(), "tricorder".into());
                config.insert(
                    "srcs".to_string(),
                    rule::Value::List(vec![
                        rule::Value::File("protos/**/*".into()),
						rule::Value::File("core/**/*".into()),
                        rule::Value::File("service/**/*".into()),
                        rule::Value::File("tests/**/*".into()),
                        rule::Value::File("Cargo.toml".into()),
                    ]),
                );
                config
            })
            .target(concrete_target)
            .build()
            .unwrap()
    }

    fn mix_escript(&self, concrete_target: ConcreteTarget) -> Signature {
        Signature::builder()
            .rule("mix_escript".to_string())
            .config({
                let mut config = rule::Config::default();
                config.insert("name".to_string(), concrete_target.name().into());
                config.insert("bin".to_string(), "tricorder".into());
                config
            })
            .target(concrete_target)
            .build()
            .unwrap()
    }
}
