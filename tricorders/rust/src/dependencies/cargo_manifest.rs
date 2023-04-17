use cargo::core::SourceId;
use cargo::util::errors::ManifestError;
use cargo::util::OptVersionReq;
use derive_builder::UninitializedFieldError;
use semver::Version;
use std::path::{Path, PathBuf};
use thiserror::*;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum CargoManifest {
    Workspace(Workspace),
    Package(Package),
}

impl CargoManifest {
    pub fn from_path(path: &Path) -> Result<CargoManifest, CargoManifestError> {
        let path = path.canonicalize().unwrap();
        let config = cargo::Config::default().unwrap();
        let source_id = SourceId::for_path(&path).unwrap();
        match cargo::util::toml::read_manifest(&path, source_id, &config)? {
            (cargo::core::EitherManifest::Real(real_manifest), _paths) => {
                Ok(Self::Package(Package::from_real_manifest(real_manifest)?))
            }
            (cargo::core::EitherManifest::Virtual(virtual_manifest), _paths) => Ok(
                Self::Workspace(Workspace::from_virtual_manifest(virtual_manifest)?),
            ),
        }
    }

    pub fn is_workspace(&self) -> bool {
        matches!(&self, Self::Workspace(_))
    }

    pub fn is_package(&self) -> bool {
        matches!(&self, Self::Package(_))
    }

    pub fn workspace(&self) -> Option<&Workspace> {
        match &self {
            Self::Workspace(ref w) => Some(w),
            Self::Package(_) => None,
        }
    }

    pub fn package(&self) -> Option<&Package> {
        match &self {
            Self::Workspace(_) => None,
            Self::Package(ref p) => Some(p),
        }
    }
}

#[derive(Builder, Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
#[builder(build_fn(error = "CargoManifestError"))]
pub struct Package {
    #[builder(setter(into))]
    name: String,

    #[builder(default = "cargo::core::Edition::Edition2021", setter(into))]
    edition: cargo::core::Edition,

    #[builder(default = "semver::Version::parse(\"0\").unwrap()", setter(into))]
    version: Version,

    #[builder(default, setter(into))]
    dependencies: Vec<Dependency>,

    #[builder(default, setter(into))]
    dev_dependencies: Vec<Dependency>,

    #[builder(default, setter(into))]
    build_dependencies: Vec<Dependency>,

    #[builder(default, setter(into))]
    bin: Vec<Binary>,

    #[builder(default, setter(into))]
    lib: Vec<Library>,
}

impl Package {
    pub fn builder() -> PackageBuilder {
        Default::default()
    }

    pub fn from_real_manifest(
        real_manifest: cargo::core::Manifest,
    ) -> Result<Self, CargoManifestError> {
        let mut bins = vec![];
        let mut libs = vec![];
        for target in real_manifest.targets() {
            let src_path = target.src_path().path().unwrap();
            let src_path = if src_path.starts_with("/private") {
                Path::new("/").join(src_path.strip_prefix("/private").unwrap())
            } else {
                src_path.to_path_buf()
            };

            match target.kind() {
                cargo::core::TargetKind::Lib(crate_types) => {
                    for crate_type in crate_types {
                        let lib = Library::builder()
                            .name(target.name())
                            .crate_type(crate_type.clone())
                            .src_path(src_path.clone())
                            .required_features(
                                target
                                    .required_features()
                                    .map(|f| f.to_vec())
                                    .unwrap_or_default(),
                            )
                            .build()?;

                        libs.push(lib)
                    }
                }
                cargo::core::TargetKind::Bin => {
                    let bin = Binary::builder()
                        .name(target.name())
                        .src_path(src_path)
                        .required_features(
                            target
                                .required_features()
                                .map(|f| f.to_vec())
                                .unwrap_or_default(),
                        )
                        .build()?;

                    bins.push(bin)
                }
                _ => (),
            }
        }

        let mut deps = vec![];
        let mut dev_deps = vec![];
        let mut build_deps = vec![];
        for dep in real_manifest.dependencies() {
            let source = if dep.source_id().is_crates_io() {
                DependencySource::CratesIo
            } else if dep.source_id().is_git() {
                DependencySource::Git {
                    rev: dep
                        .source_id()
                        .git_reference()
                        .map(|r| r.clone().into())
                        .unwrap_or(GitReference::HEAD),
                }
            } else {
                DependencySource::Path {
                    path: dep.source_id().local_path().unwrap(),
                }
            };

            let url = dep.source_id().url().clone();

            let dep = Dependency::builder()
                .kind(dep.kind().into())
                .source(source)
                .url(url)
                .name(dep.name_in_toml().to_string())
                .version_req(dep.version_req().clone())
                .build()?;

            match dep.kind() {
                DependencyKind::Normal => deps.push(dep),
                DependencyKind::Development => dev_deps.push(dep),
                DependencyKind::Build => build_deps.push(dep),
            }
        }

        Self::builder()
            .name(real_manifest.name().to_string())
            .version(real_manifest.version().clone())
            .edition(real_manifest.edition())
            .bin(bins)
            .lib(libs)
            .dependencies(deps)
            .dev_dependencies(dev_deps)
            .build_dependencies(build_deps)
            .build()
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn version(&self) -> &Version {
        &self.version
    }

    pub fn dependencies(&self) -> &[Dependency] {
        self.dependencies.as_ref()
    }

    pub fn bin(&self) -> &[Binary] {
        self.bin.as_ref()
    }

    pub fn lib(&self) -> &[Library] {
        self.lib.as_ref()
    }

    pub fn dev_dependencies(&self) -> &[Dependency] {
        self.dev_dependencies.as_ref()
    }

    pub fn build_dependencies(&self) -> &[Dependency] {
        self.build_dependencies.as_ref()
    }

    pub fn edition(&self) -> cargo::core::Edition {
        self.edition
    }
}

impl Default for Package {
    fn default() -> Self {
        Self {
            name: Default::default(),
            version: semver::Version::parse("0").unwrap(),
            edition: cargo::core::Edition::Edition2021,
            dependencies: Default::default(),
            dev_dependencies: Default::default(),
            build_dependencies: Default::default(),
            bin: Default::default(),
            lib: Default::default(),
        }
    }
}

#[derive(Builder, Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
#[builder(build_fn(error = "CargoManifestError"))]
pub struct Workspace {
    #[builder(default, setter(into))]
    members: Vec<String>,
}

impl Workspace {
    pub fn builder() -> WorkspaceBuilder {
        Default::default()
    }

    pub fn from_virtual_manifest(
        _virtual_manifest: cargo::core::VirtualManifest,
    ) -> Result<Self, CargoManifestError> {
        Self::builder().build()
    }
}

#[derive(Builder, Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
#[builder(build_fn(error = "CargoManifestError"))]
pub struct Binary {
    #[builder(setter(into))]
    name: String,

    #[builder(setter(into))]
    src_path: PathBuf,

    #[builder(default, setter(into, strip_option))]
    required_features: Vec<String>,
}

impl Binary {
    pub fn builder() -> BinaryBuilder {
        Default::default()
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn src_path(&self) -> &PathBuf {
        &self.src_path
    }

    pub fn required_features(&self) -> &[String] {
        self.required_features.as_ref()
    }
}

#[derive(Builder, Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
#[builder(build_fn(error = "CargoManifestError"))]
pub struct Library {
    #[builder(setter(into))]
    name: String,

    #[builder(setter(into))]
    crate_type: cargo::core::compiler::CrateType,

    #[builder(setter(into))]
    src_path: PathBuf,

    #[builder(default, setter(into, strip_option))]
    required_features: Vec<String>,
}

impl Library {
    pub fn builder() -> LibraryBuilder {
        Default::default()
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn crate_type(&self) -> &cargo::core::compiler::CrateType {
        &self.crate_type
    }

    pub fn src_path(&self) -> &PathBuf {
        &self.src_path
    }

    pub fn required_features(&self) -> &[String] {
        self.required_features.as_ref()
    }
}

#[derive(Builder, Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
#[builder(build_fn(error = "CargoManifestError"))]
pub struct Dependency {
    #[builder(setter(into))]
    name: String,

    #[builder(setter(into))]
    version_req: VersionReq,

    kind: DependencyKind,

    url: url::Url,

    source: DependencySource,
}

impl Dependency {
    pub fn builder() -> DependencyBuilder {
        Default::default()
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn version_req(&self) -> &VersionReq {
        &self.version_req
    }

    pub fn kind(&self) -> &DependencyKind {
        &self.kind
    }

    pub fn url(&self) -> &str {
        self.url.as_ref()
    }

    pub fn source(&self) -> &DependencySource {
        &self.source
    }

    pub fn is_crates_io(&self) -> bool {
        matches!(&self.source, DependencySource::CratesIo)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VersionReq {
    Any,
    Req(semver::VersionReq),
    Locked(semver::Version, semver::VersionReq),
}

impl VersionReq {
    pub fn matches(&self, v: &Version) -> bool {
        match self {
            VersionReq::Any => true,
            VersionReq::Req(req) => req.matches(v),
            VersionReq::Locked(v2, _) => v == v2,
        }
    }
}

impl ToString for VersionReq {
    fn to_string(&self) -> String {
        match self {
            VersionReq::Any => "*".to_string(),
            VersionReq::Req(r) => r.to_string(),
            VersionReq::Locked(v, _) => v.to_string(),
        }
    }
}

impl PartialOrd for VersionReq {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        todo!()
    }
}

impl Ord for VersionReq {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        todo!()
    }
}

impl From<cargo::util::OptVersionReq> for VersionReq {
    fn from(value: cargo::util::OptVersionReq) -> Self {
        match value {
            OptVersionReq::Any => Self::Any,
            OptVersionReq::Req(r) => Self::Req(r),
            OptVersionReq::Locked(v, r) => Self::Locked(v, r),
        }
    }
}

#[derive(Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum DependencyKind {
    #[default]
    Normal,
    Development,
    Build,
}

impl From<cargo::core::dependency::DepKind> for DependencyKind {
    fn from(value: cargo::core::dependency::DepKind) -> Self {
        match value {
            cargo::core::dependency::DepKind::Normal => Self::Normal,
            cargo::core::dependency::DepKind::Development => Self::Development,
            cargo::core::dependency::DepKind::Build => Self::Build,
        }
    }
}

#[derive(Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum DependencySource {
    Path {
        path: PathBuf,
    },
    Git {
        rev: GitReference,
    },
    #[default]
    CratesIo,
}

#[derive(Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum GitReference {
    Tag(String),
    Branch(String),
    Rev(String),
    #[default]
    HEAD,
}

impl From<cargo::core::GitReference> for GitReference {
    fn from(value: cargo::core::GitReference) -> Self {
        match value {
            cargo::core::GitReference::Tag(v) => Self::Tag(v),
            cargo::core::GitReference::Branch(v) => Self::Branch(v),
            cargo::core::GitReference::Rev(v) => Self::Rev(v),
            cargo::core::GitReference::DefaultBranch => Self::HEAD,
        }
    }
}

#[derive(Error, Debug)]
pub enum CargoManifestError {
    #[error(transparent)]
    BuilderError(UninitializedFieldError),

    #[error("Virtual manifests are not supported yet")]
    UnsupportedVirtualManifest,

    #[error(transparent)]
    ManifestError(ManifestError),
}

impl From<UninitializedFieldError> for CargoManifestError {
    fn from(value: UninitializedFieldError) -> Self {
        CargoManifestError::BuilderError(value)
    }
}

impl From<ManifestError> for CargoManifestError {
    fn from(value: ManifestError) -> Self {
        CargoManifestError::ManifestError(value)
    }
}
