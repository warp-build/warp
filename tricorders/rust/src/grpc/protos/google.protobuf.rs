/// Represents a JSON object.
///
/// An unordered key-value map, intending to perfectly capture the semantics of a
/// JSON object. This enables parsing any arbitrary JSON payload as a message
/// field in ProtoJSON format.
///
/// This follows RFC 8259 guidelines for interoperable JSON: notably this type
/// cannot represent large Int64 values or `NaN`/`Infinity` numbers,
/// since the JSON format generally does not support those values in its number
/// type.
///
/// If you do not intend to parse arbitrary JSON into your message, a custom
/// typed message should be preferred instead of using this type.
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Struct {
    /// Unordered map of dynamically typed values.
    #[prost(map = "string, message", tag = "1")]
    pub fields: ::std::collections::HashMap<::prost::alloc::string::String, Value>,
}
/// Represents a JSON value.
///
/// `Value` represents a dynamically typed value which can be either
/// null, a number, a string, a boolean, a recursive struct value, or a
/// list of values. A producer of value is expected to set one of these
/// variants. Absence of any variant is an invalid state.
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Value {
    /// The kind of value.
    #[prost(oneof = "value::Kind", tags = "1, 2, 3, 4, 5, 6")]
    pub kind: ::core::option::Option<value::Kind>,
}
/// Nested message and enum types in `Value`.
pub mod value {
    /// The kind of value.
    #[allow(clippy::derive_partial_eq_without_eq)]
    #[derive(Clone, PartialEq, ::prost::Oneof)]
    pub enum Kind {
        /// Represents a JSON `null`.
        #[prost(enumeration = "super::NullValue", tag = "1")]
        NullValue(i32),
        /// Represents a JSON number. Must not be `NaN`, `Infinity` or
        /// `-Infinity`, since those are not supported in JSON. This also cannot
        /// represent large Int64 values, since JSON format generally does not
        /// support them in its number type.
        #[prost(double, tag = "2")]
        NumberValue(f64),
        /// Represents a JSON string.
        #[prost(string, tag = "3")]
        StringValue(::prost::alloc::string::String),
        /// Represents a JSON boolean (`true` or `false` literal in JSON).
        #[prost(bool, tag = "4")]
        BoolValue(bool),
        /// Represents a JSON object.
        #[prost(message, tag = "5")]
        StructValue(super::Struct),
        /// Represents a JSON array.
        #[prost(message, tag = "6")]
        ListValue(super::ListValue),
    }
}
/// Represents a JSON array.
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct ListValue {
    /// Repeated field of dynamically typed values.
    #[prost(message, repeated, tag = "1")]
    pub values: ::prost::alloc::vec::Vec<Value>,
}
/// Represents a JSON `null`.
///
/// `NullValue` is a sentinel, using an enum with only one value to represent
/// the null value for the `Value` type union.
///
/// A field of type `NullValue` with any value other than `0` is considered
/// invalid. Most ProtoJSON serializers will emit a `Value` with a `null_value`
/// set as a JSON `null` regardless of the integer value, and so will round trip
/// to a `0` value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, ::prost::Enumeration)]
#[repr(i32)]
pub enum NullValue {
    /// Null value.
    NullValue = 0,
}
impl NullValue {
    /// String value of the enum field names used in the ProtoBuf definition.
    ///
    /// The values are not transformed in any way and thus are considered stable
    /// (if the ProtoBuf definition does not change) and safe for programmatic use.
    pub fn as_str_name(&self) -> &'static str {
        match self {
            NullValue::NullValue => "NULL_VALUE",
        }
    }
    /// Creates an enum from field names used in the ProtoBuf definition.
    pub fn from_str_name(value: &str) -> ::core::option::Option<Self> {
        match value {
            "NULL_VALUE" => Some(Self::NullValue),
            _ => None,
        }
    }
}
