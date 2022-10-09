use super::*;
use serde::de::Visitor;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::path::PathBuf;
use thiserror::*;
use url::Url;

static COLON: char = ':';
static DOT: char = '.';
static WILDCARD: &str = "//...";

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum LocalLabelKind {
    Relative,
    Absolute,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum InnerLabel {
    Wildcard,
    Local {
        kind: LocalLabelKind,
        path: PathBuf,
    },
    Remote {
        url: String,
        host: String,
        prefix_hash: String,
        path: String,
    },
}

impl Default for InnerLabel {
    fn default() -> Self {
        InnerLabel::Wildcard
    }
}

#[derive(Default, Builder, Clone, Debug, PartialOrd, Ord, Serialize, Deserialize)]
#[builder(build_fn(error = "LabelBuilderError"))]
pub struct Label {
    #[builder(default)]
    workspace: String,

    #[builder(default)]
    inner_label: InnerLabel,

    #[builder(default)]
    name: String,
}

#[derive(Error, Debug)]
pub enum LabelBuilderError {
    #[error(transparent)]
    WorkerError(build_worker::BuildWorkerError),

    #[error(transparent)]
    QueueError(build_queue::QueueError),
}

impl LabelBuilder {
    #[tracing::instrument(name = "LabelBuilder::with_workspace")]
    pub fn with_workspace(&mut self, workspace: &Workspace) -> &mut Self {
        self.workspace(workspace.paths.workspace_root.to_str().unwrap().to_string());
        self
    }

    /// Turn a URL into a Label.
    ///
    /// When doing so, the path of the URL will become the Path that we will use to find things
    /// within the remote workspace.
    ///
    #[tracing::instrument(name = "LabelBuilder::from_url")]
    pub fn from_url(&mut self, url: &Url) -> Result<Label, LabelBuilderError> {
        let raw_url = url.to_string();

        // TODO(@ostera): actually validate that we have a path with at least one segment
        let mut s = Sha256::new();
        s.update(url[..url::Position::BeforePath].as_bytes());
        for path in url.path_segments().unwrap().rev().skip(1) {
            s.update(path.as_bytes());
        }
        let prefix_hash = format!("{:x}", s.finalize());

        let name = url.path_segments().unwrap().last().unwrap().to_string();
        let host = url.host_str().unwrap().to_string();
        let path = format!(".{}", url.path());

        self.name(name);
        self.inner_label(InnerLabel::Remote {
            url: raw_url,
            host,
            prefix_hash,
            path,
        });

        self.build()
    }

    /// Turn a Path into a label.
    ///
    /// When doing this, the final stem of a path will become the name of the target
    /// if no name is set yet.
    ///
    #[tracing::instrument(name = "LabelBuilder::from_path")]
    pub fn from_path(&mut self, path: PathBuf) -> Result<Label, LabelBuilderError> {
        let (kind, path) = self._clean_path(path);

        if self.name.is_none() {
            let name = path.file_name().unwrap().to_str().unwrap().to_string();
            self.name(name);
        }

        self.inner_label(InnerLabel::Local { path, kind });

        self.build()
    }

    /// Parse a String into a Label. This will attempt to:
    ///
    /// 1. parse the String as a URL first
    /// 2. handle wildcard labels
    /// 3. deal with override target names (`a:b` notation)
    ///
    #[tracing::instrument(name = "LabelBuilder::from_string")]
    pub fn from_string(&mut self, str: &str) -> Result<Label, LabelBuilderError> {
        if let Ok(url) = Url::parse(str) {
            return self.from_url(&url);
        }

        if str.eq(WILDCARD) {
            self.name("".to_string());
            self.inner_label(InnerLabel::Wildcard);
            return self.build();
        }

        // NOTE(@ostera): we are specifying a label within this bulid target,
        // that we should override.
        //
        // else we will just use the same label as the filename.
        //
        let (path, name) = if str.contains(COLON) {
            let parts: Vec<&str> = str.split(COLON).collect();
            (parts[0].to_string(), parts[1].to_string())
        } else {
            let name = PathBuf::from(str)
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string();

            (str.to_string(), name)
        };
        self.name(name);

        let path = path.replace("//", "/");

        self.from_path(PathBuf::from(path))
    }

    fn _clean_path(&self, path: PathBuf) -> (LocalLabelKind, PathBuf) {
        let path = if path.ends_with(BUILDFILE) {
            path.parent().unwrap().to_path_buf()
        } else {
            path
        };

        let path = PathBuf::from(path.to_str().unwrap().replace("/./", "/"));

        let mut stripped = false;
        let path = if path.is_absolute() {
            stripped = true;
            path.strip_prefix(&self.workspace.clone().unwrap_or_default())
                .unwrap()
                .to_path_buf()
        } else {
            path
        };

        if path.starts_with("/") {
            (
                LocalLabelKind::Absolute,
                PathBuf::from(path.to_str().unwrap().replacen('/', "", 1)),
            )
        } else if stripped {
            (LocalLabelKind::Absolute, path)
        } else {
            (LocalLabelKind::Relative, path)
        }
    }
}

impl std::fmt::Debug for LabelBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LabelBuilder")
            .field("workspace", &self.workspace)
            .field("inner_label", &self.inner_label)
            .field("name", &self.name)
            .finish()
    }
}

impl Label {
    #[tracing::instrument(name = "Label::builder")]
    pub fn builder() -> LabelBuilder {
        LabelBuilder::default()
    }

    pub fn new(str: &str) -> Label {
        Self::builder().from_string(str).unwrap()
    }

    pub fn name(&self) -> String {
        self.name.to_string()
    }

    pub fn hash(&self) -> usize {
        match &self.inner_label {
            InnerLabel::Wildcard => 0,
            InnerLabel::Local { path, .. } => {
                fxhash::hash(&format!("{}:{}", &path.to_str().unwrap(), &self.name))
            }
            InnerLabel::Remote { url, .. } => fxhash::hash(&url),
        }
    }

    pub fn url(&self) -> Url {
        match &self.inner_label {
            InnerLabel::Remote { url, .. } => url.parse().unwrap(),
            _ => panic!("Tried to get a URl out of a local label: {:?}", self),
        }
    }

    pub fn workspace(&self) -> PathBuf {
        PathBuf::from(&self.workspace)
    }

    pub fn path(&self) -> PathBuf {
        match &self.inner_label {
            InnerLabel::Wildcard => PathBuf::from(DOT.to_string()),
            InnerLabel::Local { path, .. } => path.to_path_buf(),
            InnerLabel::Remote { path, .. } => PathBuf::from(path.to_string()),
        }
    }

    pub fn is_all(&self) -> bool {
        matches!(self.inner_label, InnerLabel::Wildcard)
    }

    pub fn is_local(&self) -> bool {
        matches!(self.inner_label, InnerLabel::Local { .. })
    }

    pub fn is_remote(&self) -> bool {
        matches!(self.inner_label, InnerLabel::Remote { .. })
    }

    pub fn is_relative(&self) -> bool {
        matches!(
            &self.inner_label,
            InnerLabel::Local {
                kind: LocalLabelKind::Relative,
                ..
            }
        )
    }

    pub fn change_workspace(&self, workspace: &Workspace) -> Self {
        let mut new_label = self.clone();
        new_label.workspace = workspace.paths.workspace_root.to_str().unwrap().to_string();
        new_label
    }

    pub fn as_store_prefix(&self) -> String {
        match &self.inner_label {
            InnerLabel::Remote {
                host, prefix_hash, ..
            } => format!("{}-{}", prefix_hash, host),
            _ => panic!("We can't turn a non-remote label into a cache prefix!"),
        }
    }
}

impl Eq for Label {}

impl PartialEq for Label {
    fn eq(&self, other: &Self) -> bool {
        self.hash() == other.hash()
    }
}

impl std::hash::Hash for Label {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.hash().hash(state);
    }
}

impl ToString for Label {
    fn to_string(&self) -> String {
        match &self.inner_label {
            InnerLabel::Wildcard => WILDCARD.to_string(),
            InnerLabel::Local { path, .. } => {
                let path = path.to_str().unwrap();
                let path = if path == "." { "" } else { path };
                format!("//{}:{}", path.replace("./", ""), self.name)
            }
            InnerLabel::Remote { url, .. } => url.to_string(),
        }
    }
}

pub mod stringy_serde {
    use super::*;

    struct LabelVisitor;
    impl<'de> Visitor<'de> for LabelVisitor {
        type Value = Label;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter
                .write_str("a string following the label syntax: :a, ./a, //a, https://hello/a")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(Self::Value::builder().from_string(v).unwrap())
        }

        fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(Self::Value::builder().from_string(&v).unwrap())
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Label, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_string(LabelVisitor)
    }

    pub fn serialize<S>(label: &Label, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&label.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_workspace(root: &str) -> Workspace {
        Workspace {
            paths: WorkspacePaths {
                workspace_root: PathBuf::from(root),
                ..Default::default()
            },
            ..Default::default()
        }
    }

    #[test]
    fn parses_wildcard_path() {
        let path = "//...";
        let l1 = Label::builder()
            .with_workspace(&Workspace::default())
            .from_string(path)
            .unwrap();
        assert_eq!("//...", l1.to_string());
        assert!(!l1.is_local());
        assert!(!l1.is_remote());
        assert!(l1.is_all());
    }

    #[test]
    fn parses_local_paths() {
        let path = ":hello";
        let l1 = Label::builder()
            .with_workspace(&Workspace::default())
            .from_string(path)
            .unwrap();
        assert_eq!("//:hello", l1.to_string());
        assert!(l1.is_local());
        assert!(!l1.is_remote());
        assert!(!l1.is_all());
    }

    #[test]
    fn parses_relative_paths_with_implicit_target() {
        let path = "./hello";
        let l1 = Label::builder()
            .with_workspace(&Workspace::default())
            .from_string(path)
            .unwrap();
        assert_eq!("//hello:hello", l1.to_string());
        assert!(l1.is_local());
        assert!(!l1.is_remote());
        assert!(!l1.is_all());

        let path = "./hello";
        let l1 = Label::builder()
            .with_workspace(&make_workspace("/test/workspace"))
            .from_string(path)
            .unwrap();
        assert_eq!("//test/workspace/hello:hello", l1.to_string());
        assert!(l1.is_local());
        assert!(!l1.is_remote());
        assert!(!l1.is_all());
    }

    #[test]
    fn parses_relative_paths_with_explicit_target() {
        let path = "./hello:world";
        let l1 = Label::builder()
            .with_workspace(&Workspace::default())
            .from_string(path)
            .unwrap();
        assert_eq!("//hello:world", l1.to_string());
        assert!(l1.is_local());
        assert!(!l1.is_remote());
        assert!(!l1.is_all());
    }

    #[test]
    fn parses_absolute_paths() {
        let path = "//hello/world:bin";
        let l1 = Label::builder()
            .with_workspace(&Workspace::default())
            .from_string(path)
            .unwrap();
        assert_eq!("//hello/world:bin", l1.to_string());
        assert!(l1.is_local());
        assert!(!l1.is_remote());
        assert!(!l1.is_all());

        let path = "//hello/world:bin";
        let l1 = Label::builder()
            .with_workspace(&make_workspace("/test/workspace"))
            .from_string(path)
            .unwrap();
        assert_eq!("//test/workspace/hello/world:hello", l1.to_string());
        assert!(l1.is_local());
        assert!(!l1.is_remote());
        assert!(!l1.is_all());
    }

    #[test]
    fn parses_remote_label() {
        let path = "https://pkgs.warp.build/toolchains/openssl";
        let l1 = Label::builder()
            .with_workspace(&Workspace::default())
            .from_string(path)
            .unwrap();
        assert!(l1.is_remote());
        assert_eq!(l1.name(), "openssl");
        assert!(
            matches!(l1.inner_label, InnerLabel::Remote { prefix_hash, .. } if prefix_hash == "6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e")
        );
    }

    #[test]
    fn can_turn_into_store_prefix() {
        let path = "https://pkgs.warp.build/toolchains/openssl";
        let l1 = Label::builder()
            .with_workspace(&Workspace::default())
            .from_string(path)
            .unwrap();
        assert_eq!(
            l1.as_store_prefix(),
            "6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e-pkgs.warp.build"
        );
    }
}
