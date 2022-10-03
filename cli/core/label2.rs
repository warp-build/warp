use super::*;
use serde::{de::Visitor, Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};
use thiserror::*;
use url::Url;

static COLON: char = ':';
static DOT: char = '.';
static WILDCARD: &str = "//...";

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum InnerLabel {
    Wildcard,
    Local {
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

#[derive(Default, Builder, Clone, Debug, PartialOrd, Ord)]
#[builder(build_fn(error = "Label2BuilderError"))]
pub struct Label2 {
    #[builder(default)]
    workspace: String,

    #[builder(default)]
    inner_label: InnerLabel,

    #[builder(default)]
    name: String,

    #[builder(default)]
    hash: usize,
}

#[derive(Error, Debug)]
pub enum Label2BuilderError {
    #[error(transparent)]
    WorkerError(build_worker::BuildWorkerError),

    #[error(transparent)]
    QueueError(build_queue::QueueError),
}

impl Label2Builder {
    pub fn with_workspace(&mut self, workspace: &Workspace) -> &mut Self {
        self.workspace(workspace.paths.workspace_root.to_str().unwrap().to_string());
        self
    }

    /// Turn a URL into a Label.
    ///
    /// When doing so, the path of the URL will become the Path that we will use to find things
    /// within the remote workspace.
    ///
    pub fn from_url(&mut self, url: &Url) -> Result<Label2, Label2BuilderError> {
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
        let path = url.path().to_string();

        self.name(name);
        self.hash(fxhash::hash(&raw_url));
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
    pub fn from_path(&mut self, path: PathBuf) -> Result<Label2, Label2BuilderError> {
        let path = PathBuf::from(self.workspace.as_ref().unwrap()).join(path);

        if self.name.is_none() {
            let name = path.file_name().unwrap().to_str().unwrap().to_string();
            let hash = fxhash::hash(&format!("{}:{}", &path.to_str().unwrap(), &name));
            self.name(name);
            self.hash(hash);
        }

        self.inner_label(InnerLabel::Local { path });

        self.build()
    }

    /// Parse a String into a Label. This will attempt to:
    ///
    /// 1. parse the String as a URL first
    /// 2. handle wildcard labels
    /// 3. deal with override target names (`a:b` notation)
    ///
    pub fn from_string(&mut self, str: &str) -> Result<Label2, Label2BuilderError> {
        if let Ok(url) = Url::parse(str) {
            return self.from_url(&url);
        }

        if str.eq(WILDCARD) {
            self.name("".to_string());
            self.hash(0);
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

        let path = path.replace("./", "");
        self.from_path(PathBuf::from(path))
    }
}

impl Label2 {
    pub fn builder() -> Label2Builder {
        Label2Builder::default()
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn hash(&self) -> usize {
        self.hash
    }

    pub fn url(&self) -> Url {
        match &self.inner_label {
            InnerLabel::Remote { url, .. } => url.parse().unwrap(),
            _ => panic!("Tried to get a URl out of a local label: {:?}", self),
        }
    }

    pub fn path(&self) -> PathBuf {
        match &self.inner_label {
            InnerLabel::Wildcard => PathBuf::from(DOT.to_string()),
            InnerLabel::Local { path, .. } => path.to_path_buf(),
            InnerLabel::Remote { path, .. } => PathBuf::from(format!(".{}", path)),
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

    pub fn reparent(&self, workspace: &Workspace) -> Self {
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

impl Eq for Label2 {}

impl PartialEq for Label2 {
    fn eq(&self, other: &Self) -> bool {
        self.hash() == other.hash()
    }
}

impl std::hash::Hash for Label2 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.hash().hash(state);
    }
}

impl ToString for Label2 {
    fn to_string(&self) -> String {
        match &self.inner_label {
            InnerLabel::Wildcard => WILDCARD.to_string(),
            InnerLabel::Local { path } => {
                let path = path.to_str().unwrap();
                let path = if path == "." { "" } else { path };
                if path.starts_with("//") {
                    format!("{}:{}", path, self.name)
                } else {
                    format!("//{}:{}", path, self.name)
                }
            }
            InnerLabel::Remote { url, .. } => url.to_string(),
        }
    }
}

struct Label2Visitor;
impl<'de> Visitor<'de> for Label2Visitor {
    type Value = Label2;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a string following the label syntax: :a, ./a, //a, https://hello/a")
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

impl<'de> Deserialize<'de> for Label2 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_string(Label2Visitor)
    }
}

impl Serialize for Label2 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
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
        let l1 = Label2::builder()
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
        let l1 = Label2::builder()
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
        let l1 = Label2::builder()
            .with_workspace(&Workspace::default())
            .from_string(path)
            .unwrap();
        assert_eq!("//hello:hello", l1.to_string());
        assert!(l1.is_local());
        assert!(!l1.is_remote());
        assert!(!l1.is_all());

        let path = "./hello";
        let l1 = Label2::builder()
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
        let l1 = Label2::builder()
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
        let l1 = Label2::builder()
            .with_workspace(&Workspace::default())
            .from_string(path)
            .unwrap();
        assert_eq!("//hello/world:bin", l1.to_string());
        assert!(l1.is_local());
        assert!(!l1.is_remote());
        assert!(!l1.is_all());

        let path = "//hello/world:bin";
        let l1 = Label2::builder()
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
        let l1 = Label2::builder()
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
        let l1 = Label2::builder()
            .with_workspace(&Workspace::default())
            .from_string(path)
            .unwrap();
        assert_eq!(
            l1.as_store_prefix(),
            "6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e-pkgs.warp.build"
        );
    }
}
