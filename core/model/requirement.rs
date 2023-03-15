use std::path::PathBuf;
use url::Url;

/// A Target Requirement. This represents an unmet dependency from a Target, typically used during
/// the resolution phase. When a Tricorder tries to generate a signature, but realizes it needs
/// more files, or a specific URL, or even another dependency handled first, it will emit a series
/// of Requirements for the resolver to process first.
///
#[derive(Debug, Clone)]
pub enum Requirement {
    File {
        path: PathBuf,
    },

    Symbol {
        raw: String,
        kind: String,
    },

    Url {
        url: Url,
        tricorder_url: Url,
    },

    Dependency {
        name: String,
        version: String,
        url: Url,
        tricorder: Url,
    },
}
