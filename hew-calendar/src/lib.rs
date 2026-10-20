//! Pure UTC calendar operations shared by native and sandbox bindings.

use chrono::format::{parse as parse_fields, Item, Parsed, StrftimeItems};
use chrono::{DateTime, Datelike, NaiveDate, Timelike, Utc};
use std::fmt::Write as _;

/// Checked calendar failures, independent of the host's string representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// The timestamp or civil year is outside Chrono's supported calendar.
    Range,
    /// Invalid date/time text or civil components.
    Parse(String),
    /// An unsupported or invalid formatting directive.
    Format(String),
}

impl Error {
    /// Stable checked-boundary discriminant: success is zero.
    #[must_use]
    pub const fn code(&self) -> i32 {
        match self {
            Self::Range => 1,
            Self::Parse(_) => 2,
            Self::Format(_) => 3,
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Range => f.write_str("timestamp is outside the calendar range -262143..=262142"),
            Self::Parse(message) | Self::Format(message) => f.write_str(message),
        }
    }
}

impl std::error::Error for Error {}

/// UTC calendar components, including the length of their month.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Components {
    pub year: i64,
    pub month: u32,
    pub day: u32,
    pub hour: u32,
    pub minute: u32,
    pub second: u32,
    pub nanosecond: u32,
    /// Monday is zero and Sunday is six.
    pub weekday: u32,
    pub days_in_month: u32,
}

fn datetime(seconds: i64, nanoseconds: u32) -> Result<DateTime<Utc>, Error> {
    if nanoseconds >= 1_000_000_000 {
        return Err(Error::Parse(
            "timestamp nanoseconds must be normalized".into(),
        ));
    }
    DateTime::from_timestamp(seconds, nanoseconds).ok_or(Error::Range)
}

fn parts(value: DateTime<Utc>) -> Result<(i64, u32), Error> {
    let nanos = value.timestamp_subsec_nanos();
    if nanos >= 1_000_000_000 {
        return Err(Error::Parse(
            "leap seconds cannot be represented by a timestamp".into(),
        ));
    }
    Ok((value.timestamp(), nanos))
}

/// Format normalized Unix parts using Chrono's strftime dialect.
///
/// # Errors
/// Returns a range error outside the calendar, or a format error for an
/// unsupported directive.
pub fn format(seconds: i64, nanoseconds: u32, pattern: &str) -> Result<String, Error> {
    let value = datetime(seconds, nanoseconds)?;
    let items = StrftimeItems::new(pattern);
    if items.clone().any(|item| matches!(item, Item::Error)) {
        return Err(Error::Format("invalid datetime format directive".into()));
    }
    let mut text = String::new();
    write!(text, "{}", value.format_with_items(items))
        .map_err(|_| Error::Format("datetime directive cannot be formatted".into()))?;
    Ok(text)
}

/// Parse a datetime with Chrono's strftime dialect. Absent offsets mean UTC;
/// explicit offsets are preserved while resolving the absolute timestamp.
///
/// # Errors
/// Returns a parse error for invalid or inconsistent fields, including leap
/// seconds, and a range error for dates beyond the supported calendar.
pub fn parse(text: &str, pattern: &str) -> Result<(i64, u32), Error> {
    let mut parsed = Parsed::new();
    parse_fields(&mut parsed, text, StrftimeItems::new(pattern)).map_err(parse_error)?;
    if parsed
        .year()
        .is_some_and(|year| year < NaiveDate::MIN.year() || year > NaiveDate::MAX.year())
    {
        return Err(Error::Range);
    }
    if parsed.offset().is_none() {
        parsed.set_offset(0).map_err(parse_error)?;
    }
    let value = parsed
        .to_datetime()
        .map_err(parse_error)?
        .with_timezone(&Utc);
    parts(value)
}

fn parse_error(error: chrono::ParseError) -> Error {
    Error::Parse(error.to_string())
}

/// Extract UTC components from normalized Unix parts.
///
/// # Errors
/// Returns a range error for moments outside the supported calendar.
pub fn components(seconds: i64, nanoseconds: u32) -> Result<Components, Error> {
    let value = datetime(seconds, nanoseconds)?;
    let days_in_month = u32::from(value.num_days_in_month());
    Ok(Components {
        year: i64::from(value.year()),
        month: value.month(),
        day: value.day(),
        hour: value.hour(),
        minute: value.minute(),
        second: value.second(),
        nanosecond: value.nanosecond(),
        weekday: value.weekday().num_days_from_monday(),
        days_in_month,
    })
}

/// Construct a UTC timestamp from civil components.
///
/// # Errors
/// Returns a range error for an unsupported year, or a parse error for invalid
/// components and leap seconds.
pub fn from_components(
    year: i64,
    month: u32,
    day: u32,
    hour: u32,
    minute: u32,
    second: u32,
    nanosecond: u32,
) -> Result<(i64, u32), Error> {
    let year = i32::try_from(year).map_err(|_| Error::Range)?;
    if year < NaiveDate::MIN.year() || year > NaiveDate::MAX.year() {
        return Err(Error::Range);
    }
    let date = NaiveDate::from_ymd_opt(year, month, day)
        .ok_or_else(|| Error::Parse("invalid calendar date".into()))?;
    if nanosecond >= 1_000_000_000 {
        return Err(Error::Parse("nanoseconds must be in 0..1000000000".into()));
    }
    let value = date
        .and_hms_nano_opt(hour, minute, second, nanosecond)
        .ok_or_else(|| Error::Parse("invalid time of day".into()))?;
    parts(value.and_utc())
}

/// Construct UTC Unix parts from signed civil fields.
///
/// # Errors
/// Returns the calendar or component error without truncating negative fields.
pub fn from_parts(
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    minute: i64,
    second: i64,
    nanosecond: u32,
) -> Result<(i64, u32), Error> {
    let field =
        |value| u32::try_from(value).map_err(|_| Error::Parse("invalid calendar component".into()));
    from_components(
        year,
        field(month)?,
        field(day)?,
        field(hour)?,
        field(minute)?,
        field(second)?,
        nanosecond,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fractional_offsets_and_negative_instants_round_trip() {
        let (seconds, nanos) = parse(
            "1970-01-01T01:29:59.999999999+01:30",
            "%Y-%m-%dT%H:%M:%S%.f%:z",
        )
        .unwrap();
        assert_eq!((seconds, nanos), (-1, 999_999_999));
        assert_eq!(
            format(seconds, nanos, "%Y-%m-%dT%H:%M:%S%.fZ").unwrap(),
            "1969-12-31T23:59:59.999999999Z",
        );
        assert_eq!(
            parse("1969-12-31 23:59:59.999999999", "%Y-%m-%d %H:%M:%S%.f").unwrap(),
            (seconds, nanos),
        );
    }

    #[test]
    fn leap_seconds_and_invalid_components_are_rejected() {
        assert!(matches!(
            parse("2016-12-31T23:59:60Z", "%Y-%m-%dT%H:%M:%SZ"),
            Err(Error::Parse(message)) if message.contains("leap seconds")
        ));
        assert!(from_components(2025, 2, 29, 0, 0, 0, 0).is_err());
        assert!(from_components(2024, 2, 29, 0, 0, 60, 0).is_err());
        let (seconds, nanos) = from_components(2024, 2, 29, 23, 59, 59, 123).unwrap();
        let fields = components(seconds, nanos).unwrap();
        assert_eq!(
            (fields.day, fields.days_in_month, fields.weekday),
            (29, 29, 3)
        );
        assert_eq!(fields.nanosecond, 123);
    }

    #[test]
    fn calendar_boundaries_are_distinct_from_timestamp_boundaries() {
        for date in [NaiveDate::MIN, NaiveDate::MAX] {
            let (seconds, nanos) =
                from_components(i64::from(date.year()), date.month(), date.day(), 0, 0, 0, 0)
                    .unwrap();
            assert_eq!(
                components(seconds, nanos).unwrap().year,
                i64::from(date.year())
            );
        }
        assert_eq!(components(i64::MIN, 0), Err(Error::Range));
        assert_eq!(components(i64::MAX, 0), Err(Error::Range));
        assert_eq!(
            from_components(262_143, 1, 1, 0, 0, 0, 0),
            Err(Error::Range)
        );
        assert!(matches!(format(0, 0, "%Q"), Err(Error::Format(_))));
        assert!(matches!(format(0, 0, "%#z"), Err(Error::Format(_))));
    }
}
