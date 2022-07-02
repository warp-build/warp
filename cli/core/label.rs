use serde::*;
use std::path::{Path, PathBuf};

static COLON: char = ':';
static DOT: &str = ".";

static WILDCARD: &str = "//...";

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize)]
pub enum Label {
    Wildcard,
    Relative { name: String, path: PathBuf },
    Absolute { name: String, path: PathBuf },
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

    pub fn from_path_and_name(path: &Path, name: &str) -> Label {
        Label::new(&format!("//{}:{}", path.to_str().unwrap(), name))
    }

    pub fn new(name: &str) -> Label {
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
        }
    }

    pub fn path(&self) -> PathBuf {
        match self {
            Label::Wildcard => PathBuf::from(DOT),
            Label::Relative { path, .. } => path.to_path_buf(),
            Label::Absolute { path, .. } => path.to_path_buf(),
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

    pub fn canonicalize(&self, root: &Path) -> Label {
        match self {
            Label::Relative { name, path } => Label::Absolute {
                name: name.clone(),
                path: root.join(path.strip_prefix(".").unwrap()),
            },
            _ => self.clone(),
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
}
