#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Archive {
    #[prost(string, tag = "1")]
    pub url: ::prost::alloc::string::String,
    #[prost(string, tag = "2")]
    pub sha256: ::prost::alloc::string::String,
    #[prost(string, tag = "3")]
    pub strip_prefix: ::prost::alloc::string::String,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Signature {
    #[prost(string, tag = "1")]
    pub name: ::prost::alloc::string::String,
    #[prost(string, tag = "2")]
    pub rule: ::prost::alloc::string::String,
    #[prost(message, repeated, tag = "3")]
    pub deps: ::prost::alloc::vec::Vec<Requirement>,
    #[prost(message, repeated, tag = "4")]
    pub runtime_deps: ::prost::alloc::vec::Vec<Requirement>,
    #[prost(message, optional, tag = "5")]
    pub config: ::core::option::Option<super::super::google::protobuf::Struct>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Symbol {
    #[prost(oneof = "symbol::Sym", tags = "1, 2")]
    pub sym: ::core::option::Option<symbol::Sym>,
}
/// Nested message and enum types in `Symbol`.
pub mod symbol {
    #[allow(clippy::derive_partial_eq_without_eq)]
    #[derive(Clone, PartialEq, ::prost::Oneof)]
    pub enum Sym {
        #[prost(bool, tag = "1")]
        All(bool),
        #[prost(string, tag = "2")]
        Named(::prost::alloc::string::String),
    }
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct FileRequirement {
    #[prost(string, tag = "1")]
    pub path: ::prost::alloc::string::String,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct SymbolRequirement {
    #[prost(string, tag = "1")]
    pub raw: ::prost::alloc::string::String,
    #[prost(string, tag = "2")]
    pub kind: ::prost::alloc::string::String,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct UrlRequirement {
    #[prost(string, tag = "1")]
    pub url: ::prost::alloc::string::String,
    #[prost(string, tag = "2")]
    pub subpath: ::prost::alloc::string::String,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct DependencyRequirement {
    #[prost(string, tag = "1")]
    pub name: ::prost::alloc::string::String,
    #[prost(string, tag = "2")]
    pub version: ::prost::alloc::string::String,
    #[prost(string, tag = "3")]
    pub url: ::prost::alloc::string::String,
    /// This resolver label will be used to find out where to fetch the archive
    /// that corresponds to this dependency.
    #[prost(string, tag = "4")]
    pub archive_resolver: ::prost::alloc::string::String,
    /// this resolver label will be used to generate a signature for the contents
    /// of the archive.
    #[prost(string, tag = "5")]
    pub signature_resolver: ::prost::alloc::string::String,
    /// The URL of the Tricorder that should be used to handle this dependency.
    #[prost(string, tag = "6")]
    pub tricorder_url: ::prost::alloc::string::String,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Requirement {
    #[prost(oneof = "requirement::Requirement", tags = "1, 2, 3, 4")]
    pub requirement: ::core::option::Option<requirement::Requirement>,
}
/// Nested message and enum types in `Requirement`.
pub mod requirement {
    #[allow(clippy::derive_partial_eq_without_eq)]
    #[derive(Clone, PartialEq, ::prost::Oneof)]
    pub enum Requirement {
        #[prost(message, tag = "1")]
        File(super::FileRequirement),
        #[prost(message, tag = "2")]
        Symbol(super::SymbolRequirement),
        #[prost(message, tag = "3")]
        Url(super::UrlRequirement),
        #[prost(message, tag = "4")]
        Dependency(super::DependencyRequirement),
    }
}
/// The Dependency message is used to communicate dependencies that have already
/// been resolved.
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Dependency {
    #[prost(string, tag = "1")]
    pub name: ::prost::alloc::string::String,
    #[prost(string, tag = "2")]
    pub version: ::prost::alloc::string::String,
    #[prost(string, tag = "3")]
    pub url: ::prost::alloc::string::String,
    /// This resolver label will be used to find out where to fetch the archive
    /// that corresponds to this dependency.
    #[prost(string, tag = "4")]
    pub archive_resolver: ::prost::alloc::string::String,
    /// this resolver label will be used to generate a signature for the contents
    /// of the archive.
    #[prost(string, tag = "5")]
    pub signature_resolver: ::prost::alloc::string::String,
    /// NOTE(@ostera): allow specifying a subdir within a larger archive
    #[prost(string, tag = "6")]
    pub archive_subdir: ::prost::alloc::string::String,
    /// The exact location of this dependency in the machine it is being executed.
    #[prost(string, tag = "7")]
    pub store_path: ::prost::alloc::string::String,
    /// The URL of the Tricorder that should be used to handle this dependency.
    #[prost(string, tag = "8")]
    pub tricorder_url: ::prost::alloc::string::String,
    /// A collection of strings pointing to the relative paths of the outputs this
    /// dependency created. All these paths are relative to the \[store_path=7\].
    #[prost(string, repeated, tag = "9")]
    pub outputs: ::prost::alloc::vec::Vec<::prost::alloc::string::String>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct TestMatcher {
    #[prost(string, repeated, tag = "1")]
    pub raw: ::prost::alloc::vec::Vec<::prost::alloc::string::String>,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, ::prost::Enumeration)]
#[repr(i32)]
pub enum Status {
    Unknown = 0,
    Ok = 1,
    Err = 2,
}
impl Status {
    /// String value of the enum field names used in the ProtoBuf definition.
    ///
    /// The values are not transformed in any way and thus are considered stable
    /// (if the ProtoBuf definition does not change) and safe for programmatic use.
    pub fn as_str_name(&self) -> &'static str {
        match self {
            Status::Unknown => "STATUS_UNKNOWN",
            Status::Ok => "STATUS_OK",
            Status::Err => "STATUS_ERR",
        }
    }
    /// Creates an enum from field names used in the ProtoBuf definition.
    pub fn from_str_name(value: &str) -> ::core::option::Option<Self> {
        match value {
            "STATUS_UNKNOWN" => Some(Self::Unknown),
            "STATUS_OK" => Some(Self::Ok),
            "STATUS_ERR" => Some(Self::Err),
            _ => None,
        }
    }
}
