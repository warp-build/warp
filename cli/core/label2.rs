use super::*;
use serde::{de::Visitor, Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};
use url::Url;

static COLON: char = ':';
static DOT: &str = ".";

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
pub struct Label {
    #[builder(default)]
    workspace: String,

    #[builder(default)]
    inner_label: InnerLabel,

    #[builder(default)]
    name: String,

    #[builder(default)]
    hash: usize,
}

impl Label {
    pub fn all() -> Label {
        Label::default()
    }
}

impl LabelBuilder {
    pub fn with_workspace(&mut self, workspace: &Workspace) -> Result<&mut Self, anyhow::Error> {
        self.workspace(workspace.paths.workspace_name);
        Ok(self)
    }

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
        let path = url.path().to_string();

        self.name(name);
        self.hash(fxhash::hash(&raw_url));
        self.inner_label(InnerLabel::Remote {
            url: raw_url.to_string(),
            host,
            prefix_hash,
            path,
        });

        self.build()
    }

    pub fn from_path(&mut self, path: &PathBuf) -> Result<Label, LabelBuilderError> {
        let name = path.file_name().unwrap().to_str().unwrap().to_string();
        let hash = fxhash::hash(&format!("{}:{}", &path.to_str().unwrap(), &name));
        self.name(name);
        self.hash(hash);
        self.inner_label(InnerLabel::Local { path });

        self.build()
    }

    pub fn from_string(&mut self, str: &str) -> Result<Label, LabelBuilderError> {
        if let Ok(url) = Url::parse(str) {
            return self.from_url(&url);
        }
    }
}

impl Label {
    #[tracing::instrument(name = "Label::from_path")]
    pub fn from_path(workspace_root: &Path, path: &str) -> Label {
        let full_path = workspace_root.join(path);
        if let Ok(abs_path) = std::fs::canonicalize(PathBuf::from(&full_path)) {
            if let Ok(rel_path) = abs_path.strip_prefix(workspace_root) {
                let stripped_path = rel_path.to_str().unwrap();
                return Label::new(stripped_path);
            }
        }
        Label::new(path)
    }

    #[tracing::instrument(name = "Label::from_path_and_name")]
    pub fn from_path_and_name(path: &Path, name: &str) -> Label {
        Label::new(&format!("//{}:{}", path.to_str().unwrap(), name))
    }

    // TODO(@ostera): turn this into Result<Label, LabelError>
    #[tracing::instrument(name = "Label::new")]
    pub fn new(name: &str, workspace: &Workspace) -> Label {
        let name = name.replace('\"', "");

        let is_wildcard = name.starts_with("//") && name.ends_with("...");
        let is_abs_name = name.starts_with("//");

        if is_wildcard {
            return Label::Wildcard;
        }

        let parts: Vec<&str> = name.split(COLON).collect();
        if is_abs_name {
            if parts.len() == 1 {
                let path = PathBuf::from(name.strip_prefix("//").unwrap());
                let name = path.file_name().unwrap().to_str().unwrap().to_string();
                let hash = fxhash::hash(&format!("{}:{}", &path.to_str().unwrap(), &name));
                return Label::Absolute { name, path, hash };
            }

            let path = PathBuf::from(parts[0].strip_prefix("//").unwrap());
            let name = parts[1].to_string();
            let hash = fxhash::hash(&format!("{}:{}", &path.to_str().unwrap(), &name));
            return Label::Absolute { path, name, hash };
        }

        // local target = relative to this build file
        if name.starts_with(COLON) {
            let path = PathBuf::from(".");
            let name = name.strip_prefix(COLON).unwrap().to_string();
            let hash = fxhash::hash(&format!("{}:{}", &path.to_str().unwrap(), &name));
            Label::Relative { path, name, hash }
        } else if parts.len() == 1 {
            let path = PathBuf::from(name);
            let name = path.file_name().unwrap().to_str().unwrap().to_string();
            let hash = fxhash::hash(&format!("{}:{}", &path.to_str().unwrap(), &name));
            Label::Relative { name, path, hash }
        } else {
            let name = parts[1].to_string();
            let path = PathBuf::from(parts[0]);
            let hash = fxhash::hash(&format!("{}:{}", &path.to_str().unwrap(), &name));
            Label::Relative { name, path, hash }
        }
    }

    pub fn name(&self) -> String {
        match self {
            Label::Wildcard => WILDCARD.to_string(),
            Label::Relative { name, .. } => name.clone(),
            Label::Absolute { name, .. } => name.clone(),
            Label::Remote { name, .. } => name.clone(),
        }
    }

    pub fn url(&self) -> Url {
        match self {
            Label::Remote { url, .. } => url.parse().unwrap(),
            _ => panic!("Tried to get a URl out of a local label: {:?}", self),
        }
    }

    pub fn path(&self) -> PathBuf {
        match self {
            Label::Wildcard => PathBuf::from(DOT),
            Label::Relative { path, .. } => path.to_path_buf(),
            Label::Absolute { path, .. } => {
                let path = path.to_path_buf();
                if path.to_str().unwrap() == "" {
                    PathBuf::from(DOT)
                } else {
                    path
                }
            }
            Label::Remote { path, .. } => PathBuf::from(format!(".{}", path)),
        }
    }

    fn hash(&self) -> usize {
        match self {
            Label::Wildcard => 0,
            Label::Relative { hash, .. } => *hash,
            Label::Absolute { hash, .. } => *hash,
            Label::Remote { hash, .. } => *hash,
        }
    }

    pub fn is_all(&self) -> bool {
        matches!(self, Label::Wildcard)
    }

    pub fn is_relative(&self) -> bool {
        matches!(self, Label::Relative { .. })
    }

    pub fn is_absolute(&self) -> bool {
        matches!(self, Label::Absolute { .. } | Label::Wildcard)
    }

    pub fn is_remote(&self) -> bool {
        matches!(self, Label::Remote { .. })
    }

    pub fn reparent(&self, root: &Path) -> Label {
        let new_path = root.join(self.path());
        Self::new(new_path.to_str().unwrap())
    }

    pub fn canonicalize(&self, root: &Path) -> Label {
        match self {
            Label::Relative { name, path, .. } => {
                let name = name.clone();
                let path = root.join(path.strip_prefix(".").unwrap());
                let hash = fxhash::hash(&format!("{}:{}", &path.to_str().unwrap(), &name));
                Label::Absolute { name, path, hash }
            }
            _ => self.clone(),
        }
    }

    pub fn as_store_prefix(&self) -> String {
        match self {
            Label::Remote {
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
        match self.inner_label {
            InnerLabel::Wildcard => WILDCARD.to_string(),
            InnerLabel::Local { path } => {
                let path = path.to_str().unwrap();
                let path = if path == "." { "" } else { path };
                format!("//{}:{}", path, self.name)
            }
            InnerLabel::Remote { url, .. } => url.to_string(),
        }
    }
}

struct LabelVisitor;
impl<'de> Visitor<'de> for LabelVisitor {
    type Value = Label;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a string following the label syntax: :a, ./a, //a, https://hello/a")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Label::new(v))
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Label::new(&v))
    }
}

impl<'de> Deserialize<'de> for Label {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_string(LabelVisitor)
    }
}

impl Serialize for Label {
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

    #[test]
    fn parses_local_paths() {
        let path = ":hello";
        let l1 = Label::new(path);
        let l2: Label = path.into();
        let l3: Label = path.to_string().into();
        assert_eq!(path, l1.to_string());
        assert!(!l1.is_absolute());
        assert!(!l1.is_all());
        assert_eq!(path, l2.to_string());
        assert!(!l2.is_absolute());
        assert!(!l2.is_all());
        assert_eq!(path, l3.to_string());
        assert!(!l3.is_absolute());
        assert!(!l3.is_all());
    }

    #[test]
    fn parses_relative_paths_with_explicit_target() {
        let path = "./hello:world";
        let l1 = Label::new(path);
        assert_eq!(path, l1.to_string());
        assert_eq!("./hello", l1.path().to_str().unwrap());
        assert_eq!("world", l1.name());
    }

    #[test]
    fn parses_relative_paths_with_implicit_target() {
        let path = "./hello";
        let l1 = Label::new(path);
        assert_eq!("./hello:hello", l1.to_string());
        assert_eq!("./hello", l1.path().to_str().unwrap());
        assert_eq!("hello", l1.name());
    }

    #[test]
    fn canonicalizes_relative_path_from_a_root() {
        let root = PathBuf::from("my/path");
        let path = "./hello/relative:world";
        let l1 = Label::new(path);
        assert_eq!(path, l1.to_string());
        assert!(l1.is_relative());
        assert_eq!(
            l1.canonicalize(&root).to_string(),
            "//my/path/hello/relative:world"
        );
    }

    #[test]
    fn parses_absolute_paths() {
        let path = "//my/path:hello";
        let l1 = Label::new(path);
        let l2: Label = path.into();
        let l3: Label = path.to_string().into();
        assert_eq!(path, l1.to_string());
        assert!(l1.is_absolute());
        assert!(!l1.is_all());
        assert_eq!(path, l2.to_string());
        assert!(l2.is_absolute());
        assert!(!l2.is_all());
        assert_eq!(path, l3.to_string());
        assert!(l3.is_absolute());
        assert!(!l3.is_all());
    }

    #[test]
    fn parses_wildcard_path() {
        let path = "//...";
        let l1 = Label::new(path);
        let l2: Label = path.into();
        let l3: Label = path.to_string().into();
        assert_eq!(path, l1.to_string());
        assert!(l1.is_absolute());
        assert!(l1.is_all());
        assert_eq!(path, l2.to_string());
        assert!(l2.is_absolute());
        assert!(l2.is_all());
        assert_eq!(path, l3.to_string());
        assert!(l3.is_absolute());
        assert!(l3.is_all());
    }

    #[test]
    fn parses_remote_label() {
        let path = "https://pkgs.warp.build/toolchains/openssl";
        let l1 = Label::new(path);
        assert!(l1.is_remote());
        assert_eq!(l1.name(), "openssl");
        assert!(
            matches!(l1, Label::Remote { prefix_hash, .. } if prefix_hash == "6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e")
        );
    }

    #[test]
    fn can_turn_into_store_prefix() {
        let path = "https://pkgs.warp.build/toolchains/openssl";
        let l1 = Label::new(path);
        assert_eq!(
            l1.as_store_prefix(),
            "6d79d7a9670467d52e84da7cd1011fe958572011d5872be4fc62d05a1a40081e-pkgs.warp.build"
        );
    }
}
