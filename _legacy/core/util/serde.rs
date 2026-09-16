/// Serializing datetimes with Serde as ISO8601 strings.
pub mod iso8601 {
    use chrono::{DateTime, Utc};
    use serde::de::Visitor;
    use serde::{Deserializer, Serializer};

    struct DateTimeVisitor;
    impl<'de> Visitor<'de> for DateTimeVisitor {
        type Value = DateTime<Utc>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a date string is expected to follow RFC3339 / ISO8601")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            let date = DateTime::parse_from_rfc3339(v)
                .map_err(|err| E::custom(format!("{:?}", err)))?
                .with_timezone(&Utc);
            Ok(date)
        }
    }

    pub fn serialize<S>(date: &DateTime<Utc>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&date.to_rfc3339())
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<DateTime<Utc>, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_string(DateTimeVisitor)
    }
}

/// Serializing durations with Serde as milliseconds.
pub mod duration {
    use chrono::Duration;
    use serde::de::Visitor;
    use serde::{Deserializer, Serializer};

    struct DurationVisitor;
    impl<'de> Visitor<'de> for DurationVisitor {
        type Value = Duration;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a date string is expected to follow RFC3339 / ISO8601")
        }

        fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(Duration::milliseconds(v))
        }

        fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(Duration::milliseconds(v.try_into().unwrap()))
        }
    }

    pub fn serialize<S>(duration: &Duration, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_i64(duration.num_milliseconds())
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Duration, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(DurationVisitor)
    }
}
