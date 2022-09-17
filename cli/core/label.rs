use std::path::{Path, PathBuf};
use url::Url;

static COLON: char = ':';
static DOT: &str = ".";

static WILDCARD: &str = "//...";

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Label {
    Wildcard,
    Relative {
        name: String,
        path: PathBuf,
    },
    Absolute {
        name: String,
        path: PathBuf,
    },
    Remote {
        url: String,
        host: String,
        prefix_hash: String,
        name: String,
    },
}

impl From<&str> for Label {
    fn from(name: &str) -> Label {
        Label::new(name)
    }
}

impl From<String> for Label {
    fn from(name: String) -> Label {
        Label::new(&name)
    }
}

impl ToString for Label {
    fn to_string(&self) -> String {
        match self {
            Label::Wildcard => WILDCARD.to_string(),
            Label::Relative { name, path } => {
                let path = path.to_str().unwrap();
                let path = if path == "." { "" } else { path };
                format!("{}:{}", path, name)
            }
            Label::Absolute { name, path } => format!("//{}:{}", path.to_str().unwrap(), name),
            Label::Remote { url, .. } => url.clone(),
        }
    }
}

impl Default for Label {
    fn default() -> Label {
        WILDCARD.into()
    }
}

impl Label {
    pub fn all() -> Label {
        Label::default()
    }

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
    pub fn new(name: &str) -> Label {
        if let Ok(url) = Url::parse(name) {
            let raw_url = name;
            // TODO(@ostera): actually validate that we have a path with at least one segment
            let mut s = blake3::Hasher::new();
            s.update(url[..url::Position::BeforePath].as_bytes());
            for path in url.path_segments().unwrap().rev().skip(1) {
                s.update(path.as_bytes());
            }
            let prefix_hash = s.finalize().to_string();

            let name = url.path_segments().unwrap().last().unwrap().to_string();
            let host = url.host_str().unwrap().to_string();
            return Label::Remote {
                url: raw_url.to_string(),
                host,
                name,
                prefix_hash,
            };
        }

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
                return Label::Absolute {
                    name: path.file_name().unwrap().to_str().unwrap().to_string(),
                    path,
                };
            }
            return Label::Absolute {
                path: PathBuf::from(parts[0].strip_prefix("//").unwrap()),
                name: parts[1].to_string(),
            };
        }

        // local target = relative to this build file
        if name.starts_with(COLON) {
            Label::Relative {
                path: PathBuf::from("."),
                name: name.strip_prefix(COLON).unwrap().to_string(),
            }
        } else if parts.len() == 1 {
            let path = PathBuf::from(name);
            Label::Relative {
                name: path.file_name().unwrap().to_str().unwrap().to_string(),
                path,
            }
        } else {
            Label::Relative {
                name: parts[1].to_string(),
                path: PathBuf::from(parts[0]),
            }
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
            Label::Absolute { path, .. } => path.to_path_buf(),
            Label::Remote { .. } => panic!(
                "Tried to get a local path out of a remote label: {:?}",
                self
            ),
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

    pub fn canonicalize(&self, root: &Path) -> Label {
        match self {
            Label::Relative { name, path } => Label::Absolute {
                name: name.clone(),
                path: root.join(path.strip_prefix(".").unwrap()),
            },
            _ => self.clone(),
        }
    }

    pub fn as_store_prefix(&self) -> String {
        match self {
            Label::Remote {
                host,
                prefix_hash,
                name,
                ..
            } => format!("{}-{}/{}", host, prefix_hash, name),
            _ => panic!("We can't turn a non-remote label into a cache prefix!"),
        }
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
            matches!(l1, Label::Remote { prefix_hash, .. } if prefix_hash == "12420716278727001446")
        );
    }

    #[test]
    fn can_turn_into_store_prefix() {
        let path = "https://pkgs.warp.build/toolchains/openssl";
        let l1 = Label::new(path);
        assert_eq!(
            l1.as_store_prefix(),
            "pkgs.warp.build-12420716278727001446/openssl"
        );
    }
}
