//! Pure UTC calendar operations shared by native and sandbox bindings.

use time::error::TryFromParsed;
use time::format_description::{parse_strftime_borrowed, BorrowedFormatItem};
use time::macros::format_description;
use time::parsing::Parsed;
use time::{Date, Month, OffsetDateTime, Time, UtcDateTime};

/// Checked calendar failures, independent of the host's string representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// The timestamp or civil year is outside the supported UTC calendar.
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
            Self::Range => write!(
                f,
                "timestamp is outside the calendar range {}..={}",
                Date::MIN.year(),
                Date::MAX.year()
            ),
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

fn datetime(seconds: i64, nanoseconds: u32) -> Result<UtcDateTime, Error> {
    if nanoseconds >= 1_000_000_000 {
        return Err(Error::Parse(
            "timestamp nanoseconds must be normalized".into(),
        ));
    }
    UtcDateTime::from_unix_timestamp_nanos(
        i128::from(seconds) * 1_000_000_000 + i128::from(nanoseconds),
    )
    .map_err(|_| Error::Range)
}

fn parts(value: UtcDateTime) -> (i64, u32) {
    (value.unix_timestamp(), value.nanosecond())
}

/// Format normalized Unix parts using the time crate's strftime dialect.
///
/// # Errors
/// Returns a range error outside the calendar, or a format error for an
/// unsupported directive.
pub fn format(seconds: i64, nanoseconds: u32, pattern: &str) -> Result<String, Error> {
    let value = datetime(seconds, nanoseconds)?;
    let items = parse_strftime_borrowed(pattern).map_err(|err| Error::Format(err.to_string()))?;
    value
        .format(&items)
        .map_err(|err| Error::Format(err.to_string()))
}

/// Parse strftime fields into an absolute timestamp. A missing offset means UTC.
///
/// # Errors
/// Returns a parse error for invalid fields or directives, and a range error
/// if the resolved instant is outside the calendar.
pub fn parse(text: &str, pattern: &str) -> Result<(i64, u32), Error> {
    let items = parse_strftime_borrowed(pattern).map_err(|err| Error::Parse(err.to_string()))?;
    parse_fields(text, &items)
}

fn parse_fields(text: &str, items: &[BorrowedFormatItem<'_>]) -> Result<(i64, u32), Error> {
    let mut parsed = Parsed::new();
    let remaining = parsed
        .parse_items(text.as_bytes(), items)
        .map_err(|err| Error::Parse(err.to_string()))?;
    if !remaining.is_empty() {
        return Err(Error::Parse("unexpected trailing date or time text".into()));
    }
    if parsed.second() == Some(60) {
        return Err(Error::Parse(
            "leap seconds cannot be represented by a timestamp".into(),
        ));
    }
    // A civil UTC offset must be strictly less than one day.
    if parsed
        .offset_hour()
        .is_some_and(|hour| hour.unsigned_abs() >= 24)
    {
        return Err(Error::Parse("UTC offset must be less than 24 hours".into()));
    }
    if parsed.offset_hour().is_none() {
        parsed.set_offset_hour(0);
    }
    let value = OffsetDateTime::try_from(parsed).map_err(|err| match err {
        TryFromParsed::ComponentRange(range) if matches!(range.name(), "year" | "timestamp") => {
            Error::Range
        }
        _ => Error::Parse(err.to_string()),
    })?;
    Ok(parts(value.checked_to_utc().ok_or(Error::Range)?))
}

const ISO_DATE_TIME: &[BorrowedFormatItem<'_>] = format_description!(
    version = 2,
    "[year range:extended]-[month]-[day]T[hour]:[minute]:[second]"
);
const ISO_INPUT: &[BorrowedFormatItem<'_>] = format_description!(
    version = 2,
    "[year range:extended]-[month]-[day]T[hour]:[minute]:[second]\
     [optional [.[subsecond]]]\
     [first [Z][z][[offset_hour sign:mandatory]:[offset_minute]]\
     [[offset_hour sign:mandatory][offset_minute]][[offset_hour sign:mandatory]]]"
);

/// Format UTC ISO 8601 with zero, three, six or nine fractional digits.
///
/// # Errors
/// Returns a range error outside the calendar or a parse error for unnormalized parts.
pub fn format_iso8601(seconds: i64, nanoseconds: u32) -> Result<String, Error> {
    use std::fmt::Write as _;
    let date = datetime(seconds, nanoseconds)?;
    let mut text = date
        .format(ISO_DATE_TIME)
        .map_err(|err| Error::Format(err.to_string()))?;
    if nanoseconds != 0 {
        let (fraction, width) = if nanoseconds.is_multiple_of(1_000_000) {
            (nanoseconds / 1_000_000, 3)
        } else if nanoseconds.is_multiple_of(1_000) {
            (nanoseconds / 1_000, 6)
        } else {
            (nanoseconds, 9)
        };
        write!(text, ".{fraction:0width$}").expect("writing to a String is infallible");
    }
    text.push('Z');
    Ok(text)
}

/// Parse an ISO date, time, optional fraction and required UTC designator or numeric offset.
///
/// # Errors
/// Returns a parse error for malformed input, or a range error outside the UTC calendar.
pub fn parse_iso8601(text: &str) -> Result<(i64, u32), Error> {
    parse_fields(text, ISO_INPUT)
}

/// Extract UTC components from normalized Unix parts.
///
/// # Errors
/// Returns a range error for moments outside the supported calendar.
pub fn components(seconds: i64, nanoseconds: u32) -> Result<Components, Error> {
    let value = datetime(seconds, nanoseconds)?;
    Ok(Components {
        year: i64::from(value.year()),
        month: u32::from(u8::from(value.month())),
        day: u32::from(value.day()),
        hour: u32::from(value.hour()),
        minute: u32::from(value.minute()),
        second: u32::from(value.second()),
        nanosecond: value.nanosecond(),
        weekday: u32::from(value.weekday().number_days_from_monday()),
        days_in_month: u32::from(value.month().length(value.year())),
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
    if !(Date::MIN.year()..=Date::MAX.year()).contains(&year) {
        return Err(Error::Range);
    }
    let field =
        |value| u8::try_from(value).map_err(|_| Error::Parse("invalid calendar component".into()));
    let invalid = |err: time::error::ComponentRange| Error::Parse(err.to_string());
    let month = Month::try_from(field(month)?).map_err(invalid)?;
    let date = Date::from_calendar_date(year, month, field(day)?).map_err(invalid)?;
    let clock = Time::from_hms_nano(field(hour)?, field(minute)?, field(second)?, nanosecond)
        .map_err(invalid)?;
    Ok(parts(UtcDateTime::new(date, clock)))
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
        for text in [
            "1970-01-01T01:29:59.999999999+01:30",
            "1969-12-31T23:29:59.999999999-0030",
        ] {
            assert_eq!(parse_iso8601(text), Ok((-1, 999_999_999)));
        }
        for (nanos, fraction) in [
            (0, ""),
            (100_000_000, ".100"),
            (100_000, ".000100"),
            (1, ".000000001"),
        ] {
            let text = format_iso8601(-1, nanos).unwrap();
            assert_eq!(text, format!("1969-12-31T23:59:59{fraction}Z"));
            assert_eq!(parse_iso8601(&text), Ok((-1, nanos)));
        }
        assert_eq!(parse("1970-01-01 01:29:59 +0130", "%F %T %z"), Ok((-1, 0)));
        assert_eq!(parse("1969-12-31 23:59:59", "%F %T"), Ok((-1, 0)));
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
    fn iso_offsets_and_fractional_seconds_are_checked_at_the_input() {
        for text in [
            "2016-12-31T18:59:60-05:00",
            "2026-01-01T12:00:00+24:00",
            "2026-01-01T12:00:00",
            "2026-01-01T12:00:00Zextra",
        ] {
            assert!(
                matches!(parse_iso8601(text), Err(Error::Parse(_))),
                "{text}"
            );
        }
        assert_eq!(
            parse_iso8601("+999999-12-31T23:59:59-01:00"),
            Err(Error::Range)
        );
        assert_eq!(
            parse_iso8601("-999999-01-01T00:00:00+01:00"),
            Err(Error::Range)
        );
        let parts = from_components(12000, 1, 1, 12, 0, 0, 1).unwrap();
        assert_eq!(
            format_iso8601(parts.0, parts.1).unwrap(),
            "+12000-01-01T12:00:00.000000001Z"
        );
        assert_eq!(parse_iso8601("+12000-01-01T12:00:00.000000001Z"), Ok(parts));
        assert_eq!(
            format(-1, 9, "%F %T %z %s").unwrap(),
            "1969-12-31 23:59:59 +0000 -1"
        );
        assert_eq!(parse("-1", "%s"), Ok((-1, 0)));
    }

    #[test]
    fn calendar_boundaries_are_distinct_from_timestamp_boundaries() {
        for date in [Date::MIN, Date::MAX] {
            let (seconds, nanos) = from_components(
                i64::from(date.year()),
                u32::from(u8::from(date.month())),
                u32::from(date.day()),
                0,
                0,
                0,
                0,
            )
            .unwrap();
            assert_eq!(
                components(seconds, nanos).unwrap().year,
                i64::from(date.year())
            );
        }
        assert_eq!(components(i64::MIN, 0), Err(Error::Range));
        assert_eq!(components(i64::MAX, 0), Err(Error::Range));
        assert_eq!(
            from_components(1_000_000, 1, 1, 0, 0, 0, 0),
            Err(Error::Range)
        );
        assert!(matches!(format(0, 0, "%Q"), Err(Error::Format(_))));
        assert!(matches!(format(0, 0, "%#z"), Err(Error::Format(_))));
    }
}
