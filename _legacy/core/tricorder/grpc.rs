use self::proto::build::warp::tricorder::generate_signature_response::Response;
use self::proto::build::warp::tricorder::EnsureReadyRequest;
use super::{
    Connection, SignatureGenerationFlow, Subtree, Tricorder, TricorderContext, TricorderError,
};
use crate::archive::Archive;
use crate::code::SourceHasher;
use crate::model::{
    rule, ConcreteTarget, ExecutableSpec, RemoteTarget, Requirement, Signature, SignatureError,
    Task, TestMatcher, UnregisteredTask,
};
use crate::store::ArtifactManifest;
use crate::sync::Arc;
use crate::{Goal, Target};
use async_trait::async_trait;
use std::path::PathBuf;
use tracing::instrument;
use url::Url;

/// Protobuf generated code.
mod proto {
    include!(concat!(env!("OUT_DIR"), "/_include.rs"));
}

/// An implementation of the Tricorder framework that works over gRPC.
///
#[derive(Debug, Clone)]
pub struct GrpcTricorder {
    ctx: TricorderContext,
    conn: Connection,
    client: proto::build::warp::tricorder::tricorder_service_client::TricorderServiceClient<
        tonic::transport::Channel,
    >,
}

impl GrpcTricorder {
    fn requirements_to_tasks(&self, deps: Vec<proto::build::warp::Requirement>) -> Vec<Task> {
        let mut tasks = vec![];

        for dep in deps {
            let target: Target = match dep.requirement.unwrap() {
                proto::build::warp::requirement::Requirement::File(file) => file.path.into(),
                proto::build::warp::requirement::Requirement::Remote(remote) => {
                    let remote_target = RemoteTarget::builder()
                        .url(remote.url)
                        .tricorder_url(remote.tricorder_url)
                        .subpath(remote.subpath)
                        .build()
                        .unwrap();
                    Target::Remote(remote_target)
                }
                _ => continue,
            };

            let target_id = self.ctx.target_registry.register_target(target);

            let unreg_task = UnregisteredTask::builder()
                .goal(Goal::Build)
                .target_id(target_id)
                .build()
                .unwrap();

            let task = self.ctx.task_registry.register(unreg_task);

            tasks.push(task);
        }

        tasks
    }
}

#[async_trait]
impl Tricorder for GrpcTricorder {
    #[instrument(name = "GrpcTricorder::connect", skip(ctx))]
    async fn connect(conn: Connection, ctx: TricorderContext) -> Result<Self, TricorderError> {
        let conn_str = format!("http://0.0.0.0:{}", conn.port);
        let client = loop {
            let conn =
                proto::build::warp::tricorder::tricorder_service_client::TricorderServiceClient::connect(
                    conn_str.clone(),
                )
                .await;
            if let Ok(conn) = conn {
                break conn;
            }
            tokio::time::sleep(std::time::Duration::from_millis(1)).await;
        };
        Ok(Self { conn, client, ctx })
    }

    #[instrument(name = "GrpcTricorder::ensure_ready", skip(self))]
    async fn ensure_ready(&mut self) -> Result<(), TricorderError> {
        let req = EnsureReadyRequest::default();
        let _ = self.client.ensure_ready(req).await?;
        Ok(())
    }

    #[instrument(name = "GrpcTricorder::get_ast", skip(self, concrete_target))]
    async fn get_ast(
        &mut self,
        concrete_target: &ConcreteTarget,
        dependencies: &[(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
        test_matcher: &TestMatcher,
    ) -> Result<SignatureGenerationFlow, TricorderError> {
        let goal: proto::build::warp::Goal = concrete_target.goal().into();
        let request = proto::build::warp::tricorder::GetAstRequest {
            goal: goal.into(),
            workspace_root: concrete_target
                .workspace_root()
                .to_string_lossy()
                .to_string(),
            file: concrete_target.path().to_string_lossy().to_string(),
            dependencies: dependencies
                .iter()
                .map(|(_task, spec, manifest)| proto::build::warp::Dependency {
                    store_path: manifest.store_path().to_string_lossy().to_string(),
                    name: spec.signature().target().name().to_string(),
                    outputs: manifest
                        .outs()
                        .iter()
                        .map(|path| path.to_string_lossy().to_string())
                        .collect(),
                    ..Default::default()
                })
                .collect(),
            test_matcher: Some(proto::build::warp::TestMatcher {
                raw: test_matcher.raw().to_vec(),
            }),
        };

        let res = self
            .client
            .get_ast(request)
            .await?
            .into_inner()
            .response
            .unwrap();

        match res {
            proto::build::warp::tricorder::get_ast_response::Response::Ok(res) => {
                let subtrees = res
                    .subtrees
                    .into_iter()
                    .map(|msg| Subtree {
                        file: PathBuf::from(msg.file),
                        source_chunk: msg.source_chunk,
                        ast_hash: SourceHasher::hash_str(msg.ast),
                        signature_name: msg.signature_name,
                    })
                    .collect();
                Ok(SignatureGenerationFlow::ExtractedAst { subtrees })
            }
            proto::build::warp::tricorder::get_ast_response::Response::MissingDeps(res) => {
                let mut requirements = vec![];

                for req in res.requirements {
                    let url = self.conn.tricorder_url.clone();
                    requirements.push((url, req.requirement.unwrap()).into())
                }

                Ok(SignatureGenerationFlow::MissingRequirements { requirements })
            }
        }
    }

    #[instrument(
        name = "GrpcTricorder::generate_signature",
        skip(self, concrete_target)
    )]
    async fn generate_signature(
        &mut self,
        concrete_target: &ConcreteTarget,
        dependencies: &[(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
        test_matcher: Option<Arc<TestMatcher>>,
    ) -> Result<SignatureGenerationFlow, TricorderError> {
        let goal: proto::build::warp::Goal = concrete_target.goal().into();
        let request = proto::build::warp::tricorder::GenerateSignatureRequest {
            workspace_root: concrete_target
                .workspace_root()
                .to_string_lossy()
                .to_string(),
            file: concrete_target.path().to_string_lossy().to_string(),
            dependencies: dependencies
                .iter()
                .map(|(_task, spec, manifest)| proto::build::warp::Dependency {
                    store_path: manifest.store_path().to_string_lossy().to_string(),
                    name: spec.signature().target().name().to_string(),
                    outputs: manifest
                        .outs()
                        .iter()
                        .map(|path| path.to_string_lossy().to_string())
                        .collect(),
                    ..Default::default()
                })
                .collect(),
            test_matcher: test_matcher.map(|matcher| proto::build::warp::TestMatcher {
                raw: matcher.raw().to_vec(),
            }),
            goal: goal.into(),
        };

        let res = self
            .client
            .generate_signature(request)
            .await?
            .into_inner()
            .response
            .unwrap();

        match res {
            Response::Ok(res) => {
                let shared_deps: Vec<Task> = dependencies
                    .iter()
                    .map(|(task, _spec, _manifest)| *task)
                    .collect();

                let mut signatures = vec![];
                for mut proto_sig in res.signatures.into_iter() {
                    let mut config: rule::Config = proto_sig.config.take().unwrap().into();
                    config.insert(
                        "name".to_string(),
                        rule::Value::String(proto_sig.name.clone()),
                    );

                    let mut deps = self.requirements_to_tasks(proto_sig.deps);
                    deps.extend(shared_deps.iter());

                    let runtime_deps = self.requirements_to_tasks(proto_sig.runtime_deps);

                    let sig = Signature::builder()
                        .name(proto_sig.name)
                        .rule(proto_sig.rule)
                        .target(concrete_target.clone())
                        .config(config)
                        .deps(deps)
                        .runtime_deps(runtime_deps)
                        .build()?;

                    signatures.push(sig);
                }
                Ok(SignatureGenerationFlow::GeneratedSignatures { signatures })
            }
            Response::MissingDeps(res) => {
                let mut requirements = vec![];

                for req in res.requirements {
                    let url = self.conn.tricorder_url.clone();
                    requirements.push((url, req.requirement.unwrap()).into())
                }

                Ok(SignatureGenerationFlow::MissingRequirements { requirements })
            }
        }
    }

    async fn ready_dependency(
        &mut self,
        ct: &ConcreteTarget,
        archive: &Archive,
    ) -> Result<SignatureGenerationFlow, TricorderError> {
        let request = proto::build::warp::tricorder::PrepareDependencyRequest {
            package_name: ct.name().to_string(),
            package_root: ct.abs_path().to_string_lossy().to_string(),
            url: archive.url().to_string(),
        };

        let res = self.client.prepare_dependency(request).await?.into_inner();

        let mut signatures = vec![];
        for mut proto_sig in res.signatures.into_iter() {
            let mut config: rule::Config = proto_sig.config.take().unwrap().into();
            config.insert(
                "name".to_string(),
                rule::Value::String(proto_sig.name.clone()),
            );

            let deps = self.requirements_to_tasks(proto_sig.deps);
            let runtime_deps = self.requirements_to_tasks(proto_sig.runtime_deps);

            let sig = Signature::builder()
                .name(proto_sig.name)
                .rule(proto_sig.rule)
                .target(ct.clone())
                .config(config)
                .deps(deps)
                .runtime_deps(runtime_deps)
                .build()?;

            signatures.push(sig);
        }

        Ok(SignatureGenerationFlow::GeneratedSignatures { signatures })
    }
}

impl From<tonic::Status> for TricorderError {
    fn from(value: tonic::Status) -> Self {
        TricorderError::GrpcError(value)
    }
}

impl From<SignatureError> for TricorderError {
    fn from(value: SignatureError) -> Self {
        TricorderError::SignatureError(value)
    }
}

impl From<proto::google::protobuf::Struct> for rule::Config {
    fn from(values: proto::google::protobuf::Struct) -> Self {
        let mut cfg = Self::default();
        for (k, v) in values.fields {
            cfg.insert(k, v.into())
        }
        cfg
    }
}

impl From<proto::google::protobuf::Value> for rule::Value {
    fn from(val: proto::google::protobuf::Value) -> Self {
        match val.kind.unwrap() {
            proto::google::protobuf::value::Kind::StringValue(k) => Self::String(k),
            proto::google::protobuf::value::Kind::ListValue(l) => l.into(),
            proto::google::protobuf::value::Kind::NullValue(_) => unimplemented!(),
            proto::google::protobuf::value::Kind::NumberValue(_) => unimplemented!(),
            proto::google::protobuf::value::Kind::BoolValue(_) => unimplemented!(),
            proto::google::protobuf::value::Kind::StructValue(_) => unimplemented!(),
        }
    }
}

impl From<proto::google::protobuf::ListValue> for rule::Value {
    fn from(list: proto::google::protobuf::ListValue) -> Self {
        let mut items = vec![];
        for item in list.values {
            items.push(item.into());
        }
        Self::List(items)
    }
}

impl From<Goal> for proto::build::warp::Goal {
    fn from(value: Goal) -> Self {
        match value {
            Goal::Bootstrap => Self::Unknown,
            Goal::Build => Self::Build,
            Goal::Fetch => Self::Unknown,
            Goal::Run => Self::Run,
            Goal::Test { .. } => Self::Test,
        }
    }
}

impl From<(url::Url, proto::build::warp::requirement::Requirement)> for Requirement {
    fn from(
        (_tricorder_url, value): (url::Url, proto::build::warp::requirement::Requirement),
    ) -> Self {
        match value {
            proto::build::warp::requirement::Requirement::File(file) => Requirement::File {
                path: file.path.into(),
            },
            proto::build::warp::requirement::Requirement::Symbol(sym) => Requirement::Symbol {
                raw: sym.raw,
                kind: sym.kind,
            },
            proto::build::warp::requirement::Requirement::Remote(remote) => Requirement::Remote {
                name: remote.name,
                url: remote.url.parse::<Url>().unwrap(),
                tricorder_url: remote.tricorder_url.parse::<Url>().unwrap(),
                subpath: if remote.subpath.is_empty() {
                    None
                } else {
                    Some(PathBuf::from(remote.subpath))
                },
            },
        }
    }
}
