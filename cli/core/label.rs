use super::*;
use serde::de::Visitor;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use thiserror::*;
use tracing::*;
use url::Url;

fn split_path(path: &str) -> (PathBuf, Option<String>) {
    if let Some((path, name)) = path.split_once(':') {
        (path.into(), Some(name.to_string()))
    } else {
        (path.into(), None)
    }
}

#[derive(Error, Debug)]
pub enum LabelError {
    #[error(transparent)]
    WorkerError(build_worker::BuildWorkerError),

    #[error(transparent)]
    QueueError(build_queue::QueueError),

    #[error(transparent)]
    BuilderError(derive_builder::UninitializedFieldError),
}

impl From<derive_builder::UninitializedFieldError> for LabelError {
    fn from(err: derive_builder::UninitializedFieldError) -> Self {
        Self::BuilderError(err)
    }
}

/// A wildcard Label is a placeholder for all the labels (concrete and abstract), living
/// under a
#[derive(
    Builder, Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[builder(build_fn(name = "build", error = "LabelError"))]
pub struct WildcardLabel {
    /// The absolute path to the workspace where this label is pointing.
    ///
    #[builder(default)]
    workspace: PathBuf,

    /// The prefix of this workspace to be used as a filter when queueing the entire workspace.
    ///
    #[builder(default)]
    prefix: PathBuf,
}

impl WildcardLabel {
    pub fn builder() -> WildcardLabelBuilder {
        WildcardLabelBuilder::default()
    }
}

impl WildcardLabelBuilder {
    pub fn build_label(&self) -> Result<Label, LabelError> {
        Ok(Label::Wildcard(self.build().unwrap()))
    }
}

impl ToString for WildcardLabel {
    fn to_string(&self) -> String {
        if self.prefix.to_string_lossy() == "/" {
            "//...".to_string()
        } else {
            format!("//{}/...", self.prefix.to_string_lossy())
        }
    }
}

impl AsRef<Path> for WildcardLabel {
    fn as_ref(&self) -> &Path {
        &self.prefix
    }
}

/// Local Labels are Labels that point to a specific file on disk.
/// These may belong to the current or to another workspace.
///
#[derive(
    Builder, Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[builder(build_fn(name = "build", error = "LabelError"))]
pub struct LocalLabel {
    /// The absolute path to the workspace where this file lives.
    ///
    /// We expected `workspace` to be a prefix of `file`.
    ///
    #[builder(default = r#"PathBuf::from(".")"#)]
    workspace: PathBuf,

    /// The relative path to the file from `workspace`.
    ///
    /// We expected `workspace` to be a prefix of `file`.
    ///
    file: PathBuf,

    /// The name of a specific Target within the signature for this file.
    ///
    /// At some point, this LocalLabel will be analyzed and turned into a Signature that may have
    /// multiple Targets.
    ///
    #[builder(default)]
    #[serde(default)]
    name: Option<String>,

    /// An optional Label from which the current label was promoted. Label promotion usually
    /// happens when we grab an Remote Label and turn it into a Local label.
    ///
    #[builder(default)]
    #[serde(default)]
    promoted_from: Option<Box<RemoteLabel>>,

    /// An associated URL, primarily used for printing concrete labels that have been
    /// reparented to a new Workspace.
    ///
    /// In that case, the `file` path will not point to a path within the current workspace,
    /// and printing it prefixed by `workspace` will yield long, scary paths like:
    ///
    /// ```
    ///   "/warp/store/d688e48f34895dd351f3cb722b5531558eb47aaa22fd3f81cba3a614fc461306-e6a4dc99c0c9f17a6d4d865e40aefc409986f849/69487aa53056d26251dce8f828d4e986c7a765537bde51d296309aab0a95b5af/src/app_config.erl"
    /// ```
    ///
    /// whereas a much cleaner path could be:
    ///
    /// ```
    ///   "https://gitlab.com/leapsight/key_value/src/app_config.erl"
    /// ```
    ///
    /// We put this together by using this `associated_url` field and the `file` field.
    ///
    #[builder(default)]
    #[serde(default)]
    associated_url: Option<Url>,
}

impl LocalLabel {
    pub fn builder() -> LocalLabelBuilder {
        LocalLabelBuilder::default()
    }

    pub fn set_workspace<W: Into<PathBuf>>(&mut self, w: W) {
        self.workspace = w.into();
    }

    pub fn set_associated_url(&mut self, url: Url) {
        self.associated_url = Some(url);
    }

    pub fn hash(&self) -> usize {
        fxhash::hash(&self.file.to_string_lossy())
    }

    pub fn workspace(&self) -> &Path {
        &self.workspace
    }

    pub fn name(&self) -> Option<Cow<'_, str>> {
        self.name.as_ref().map(Cow::from)
    }

    pub fn promoted_from(&self) -> Option<RemoteLabel> {
        self.promoted_from.as_ref().map(|r| r.as_ref().to_owned())
    }
}

impl LocalLabelBuilder {
    pub fn build_label(&self) -> Result<Label, LabelError> {
        Ok(Label::Local(self.build().unwrap()))
    }
}

impl ToString for LocalLabel {
    fn to_string(&self) -> String {
        let prefix = {
            if let Some(url) = &self.associated_url {
                url.to_string()
            } else if let Some(remote) = &self.promoted_from {
                remote.to_string()
            } else {
                format!("./{}", self.file.to_string_lossy())
            }
        };

        if let Some(name) = &self.name {
            if !prefix.ends_with(name) {
                return format!("{}:{}", prefix, name);
            }
        }
        prefix
    }
}

impl AsRef<Path> for LocalLabel {
    fn as_ref(&self) -> &Path {
        &self.file
    }
}

impl From<AbstractLabel> for LocalLabel {
    fn from(l: AbstractLabel) -> Self {
        LocalLabel::builder()
            .workspace(l.workspace().into())
            .file(l.path)
            .name(Some(l.name))
            .build()
            .unwrap()
    }
}

/// Remote Labels are Labels built from a URL. They may be
///
#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RemoteLabel {
    /// The URL used to create this label.
    ///
    url: String,

    pub(crate) host: String,

    scheme: String,

    pub(crate) prefix_hash: String,

    pub(crate) path: PathBuf,

    name: Option<String>,
}

impl RemoteLabel {
    pub fn url_str(&self) -> Cow<'_, str> {
        Cow::from(&self.url)
    }

    pub fn url(&self) -> url::Url {
        self.url.parse().unwrap()
    }

    pub fn hash(&self) -> usize {
        fxhash::hash(&self.url)
    }

    pub fn name(&self) -> Option<Cow<'_, str>> {
        self.name.as_ref().map(Cow::from)
    }

    /// Turn a RemoteLabel into a LocalLabel by using the URL to create a path to a remote
    /// workspace.
    ///
    /// For Remote Rules and Toolchains, this is a synthetic path that is never used.
    ///
    /// For Remote Workspaces, this is the path where we will end up downloading and extracting
    /// contents.
    ///
    fn to_local<P>(&self, workspace_root: P) -> LocalLabel
    where
        P: AsRef<Path> + Clone,
    {
        let workspace_root = workspace_root.as_ref();
        let workspace = workspace_root.join(&self.scheme).join(&self.host);

        let name = self
            .name
            .clone()
            .unwrap_or_else(|| self.path.file_name().unwrap().to_string_lossy().to_string());

        let path = self.path.to_string_lossy().to_string();
        let path = path.strip_prefix('/').unwrap();

        LocalLabel::builder()
            .workspace(workspace)
            .file(path.into())
            .name(Some(name))
            .promoted_from(Some(Box::new(self.to_owned())))
            .build()
            .unwrap()
    }
}

impl ToString for RemoteLabel {
    fn to_string(&self) -> String {
        self.url.to_string()
    }
}

impl From<&url::Url> for RemoteLabel {
    fn from(val: &url::Url) -> Self {
        val.to_owned().into()
    }
}

impl From<url::Url> for RemoteLabel {
    fn from(val: url::Url) -> Self {
        let mut s = Sha256::new();
        s.update(val[..url::Position::BeforePath].as_bytes());
        for path in val.path_segments().unwrap().rev().skip(1) {
            s.update(path.as_bytes());
        }

        let path = val.path();

        let (path, name) = split_path(path);

        let name = name.or_else(|| Some(val.path_segments().unwrap().last().unwrap().to_string()));

        RemoteLabel {
            url: val.to_string(),
            host: val.host_str().unwrap().to_string(),
            scheme: val.scheme().to_string(),
            prefix_hash: format!("{:x}", s.finalize()),
            path,
            name,
        }
    }
}

impl AsRef<Path> for RemoteLabel {
    fn as_ref(&self) -> &Path {
        &self.path
    }
}

/// Abstract Labels are labels generated on the fly from reading a Signature file or from
/// generating a Signature out of a Local label.
///
#[derive(Builder, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[builder(build_fn(name = "build", error = "LabelError"))]
pub struct AbstractLabel {
    workspace: PathBuf,
    path: PathBuf,
    name: String,
}

impl AbstractLabel {
    pub fn builder() -> AbstractLabelBuilder {
        AbstractLabelBuilder::default()
    }

    pub fn set_workspace<W: Into<PathBuf>>(&mut self, w: W) {
        self.workspace = w.into();
    }

    pub fn set_path<W: Into<PathBuf>>(&mut self, w: W) {
        self.path = w.into();
    }

    pub fn workspace(&self) -> &Path {
        &self.workspace
    }

    pub fn name(&self) -> Cow<'_, str> {
        Cow::from(&self.name)
    }

    pub fn hash(&self) -> usize {
        fxhash::hash(&self.path)
    }
}

impl AbstractLabelBuilder {
    pub fn build_label(&self) -> Result<Label, LabelError> {
        Ok(Label::Abstract(self.build().unwrap()))
    }
}

impl ToString for AbstractLabel {
    fn to_string(&self) -> String {
        format!("{}:{}", self.path.to_string_lossy(), self.name)
    }
}

impl AsRef<Path> for AbstractLabel {
    fn as_ref(&self) -> &Path {
        &self.path
    }
}

impl From<LocalLabel> for AbstractLabel {
    fn from(l: LocalLabel) -> Self {
        let name = l.name.as_ref().map(|n| n.to_string()).unwrap_or_else(|| {
            let file_name = l.file.file_name().unwrap();
            file_name.to_string_lossy().to_string()
        });

        AbstractLabel::builder()
            .workspace(l.workspace().into())
            .path(l.file)
            .name(name)
            .build()
            .unwrap()
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize)]
pub enum Label {
    Wildcard(WildcardLabel),
    Local(LocalLabel),
    Remote(RemoteLabel),
    Abstract(AbstractLabel),
}

/// Constructors and Setters
impl Label {
    #[tracing::instrument(name = "Label::local_builder")]
    pub fn local_builder() -> LocalLabelBuilder {
        LocalLabel::builder()
    }

    #[tracing::instrument(name = "Label::wildcard_builder")]
    pub fn wildcard_builder() -> WildcardLabelBuilder {
        WildcardLabel::builder()
    }

    #[tracing::instrument(name = "Label::wildcard_builder")]
    pub fn abstract_builder() -> AbstractLabelBuilder {
        AbstractLabel::builder()
    }

    #[tracing::instrument(name = "Label::all")]
    pub fn all() -> Self {
        Self::default()
    }

    pub fn set_associated_url(&mut self, url: Url) {
        match self {
            Label::Local(l) => l.set_associated_url(url),
            _ => (),
        }
    }

    pub fn set_workspace<W>(&mut self, w: W)
    where
        W: Into<PathBuf>,
    {
        match self {
            Label::Abstract(l) => l.set_workspace(w),
            Label::Local(l) => l.set_workspace(w),
            _ => (),
        }
    }

    pub fn set_path<W>(&mut self, w: W)
    where
        W: Into<PathBuf>,
    {
        match self {
            Label::Abstract(l) => l.set_path(w),
            _ => (),
        }
    }

    pub fn to_local<P>(&self, workspace_root: P) -> Option<Label>
    where
        P: AsRef<Path> + Clone,
    {
        match &self {
            Label::Remote(r) => Some(r.to_local(workspace_root).into()),
            Label::Abstract(a) => Some(a.to_owned().into()),
            Label::Local(_) => {
                let mut l = self.to_owned();
                l.set_workspace(workspace_root.as_ref());
                Some(l)
            }
            _ => None,
        }
    }

    pub fn to_abstract(&self) -> Option<Label> {
        match &self {
            Label::Local(l) => {
                let abstract_label: AbstractLabel = l.to_owned().into();
                Some(abstract_label.into())
            }
            Label::Abstract(_) => Some(self.to_owned()),
            _ => None,
        }
    }
}

/// Accessors for Labels
impl Label {
    pub fn hash(&self) -> usize {
        match &self {
            Label::Wildcard(_) => 0,
            Label::Local(l) => l.hash(),
            Label::Remote(r) => r.hash(),
            Label::Abstract(a) => a.hash(),
        }
    }

    pub fn name(&self) -> Cow<'_, str> {
        match &self {
            Label::Wildcard(_) => Cow::from(""),
            Label::Local(l) => l.name().unwrap_or_default(),
            Label::Remote(r) => r.name().unwrap_or_default(),
            Label::Abstract(a) => a.name(),
        }
    }

    pub fn workspace(&self) -> Option<&Path> {
        match &self {
            Label::Local(l) => Some(l.workspace()),
            Label::Abstract(l) => Some(l.workspace()),
            _ => None,
        }
    }

    pub fn path(&self) -> &Path {
        match &self {
            Label::Wildcard(w) => w.as_ref(),
            Label::Local(l) => l.as_ref(),
            Label::Remote(r) => r.as_ref(),
            Label::Abstract(a) => a.as_ref(),
        }
    }

    pub fn extension(&self) -> Option<String> {
        self.path()
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| ext.to_string())
    }

    pub fn is_file(&self) -> bool {
        matches!(&self, Self::Local(_))
    }

    pub fn is_all(&self) -> bool {
        matches!(&self, Self::Wildcard(_))
    }

    pub fn is_abstract(&self) -> bool {
        matches!(&self, Self::Abstract(_))
    }

    pub fn is_remote(&self) -> bool {
        matches!(&self, Self::Remote(_))
    }

    pub fn get_abstract(&self) -> Option<&AbstractLabel> {
        match &self {
            Label::Abstract(a) => Some(a),
            _ => None,
        }
    }

    pub fn get_remote(&self) -> Option<&RemoteLabel> {
        match &self {
            Label::Remote(r) => Some(r),
            _ => None,
        }
    }

    pub fn get_local(&self) -> Option<&LocalLabel> {
        match &self {
            Label::Local(l) => Some(l),
            _ => None,
        }
    }
}

impl Default for Label {
    fn default() -> Self {
        Self::Wildcard(WildcardLabel::default())
    }
}

impl AsRef<Label> for Label {
    fn as_ref(&self) -> &Label {
        self
    }
}

impl ToString for Label {
    fn to_string(&self) -> String {
        match &self {
            Label::Wildcard(l) => l.to_string(),
            Label::Local(l) => l.to_string(),
            Label::Remote(l) => l.to_string(),
            Label::Abstract(l) => l.to_string(),
        }
    }
}

impl From<AbstractLabel> for Label {
    fn from(val: AbstractLabel) -> Self {
        Label::Abstract(val)
    }
}

impl From<LocalLabel> for Label {
    fn from(val: LocalLabel) -> Self {
        Label::Local(val)
    }
}

impl From<RemoteLabel> for Label {
    fn from(val: RemoteLabel) -> Self {
        Label::Remote(val)
    }
}

impl From<&url::Url> for Label {
    fn from(val: &url::Url) -> Self {
        Label::Remote(val.into())
    }
}

impl From<url::Url> for Label {
    fn from(val: url::Url) -> Self {
        Label::Remote(val.into())
    }
}

impl FromStr for Label {
    type Err = LabelError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(url) = s.parse::<url::Url>() {
            return Ok(url.into());
        }

        if s.starts_with("//") && s.ends_with("...") {
            let prefix = s.replace("//", "/").replace("...", "");
            return Self::wildcard_builder().prefix(prefix.into()).build_label();
        }

        let s = s.replace("//", "").replace("./", "");

        let (file, name) = split_path(&s);

        let name = name.or_else(|| Some(file.file_name().unwrap().to_string_lossy().to_string()));

        Self::local_builder().file(file).name(name).build_label()
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
        Ok(FromStr::from_str(v).unwrap())
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(FromStr::from_str(&v).unwrap())
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let key: String = map
            .next_key()?
            .ok_or_else(|| serde::de::Error::custom("Found an empty map"))?;

        let label = match key.as_str() {
            "Wildcard" => Label::Wildcard(map.next_value()?),
            "Local" => Label::Local(map.next_value()?),
            "Abstract" => Label::Abstract(map.next_value()?),
            "Remote" => Label::Remote(map.next_value()?),
            _ => {
                return Err(serde::de::Error::custom(format!(
                    "Do not know how to deserialize label of kind '{}'",
                    key
                )))
            }
        };

        Ok(label)
    }
}

impl<'de> Deserialize<'de> for Label {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(LabelVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_from_string_url() {
        let label: Label = serde_json::from_str("https://hello.world").unwrap();
        dbg!(label);
    }
}
