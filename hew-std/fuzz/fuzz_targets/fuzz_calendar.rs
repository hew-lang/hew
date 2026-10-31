//! `std.time` calendar parsing and formatting with untrusted patterns.
//!
//! Input: 12 bytes of timestamp, then `text NUL pattern`. Properties: parse,
//! format, components and from-parts never panic on any text, pattern or
//! instant; components rebuild the same instant; and a lossless pattern
//! formats any in-range instant to text that parses back to it.
#![no_main]

use libfuzzer_sys::fuzz_target;

mod support;

const LOSSLESS: &str = "%Y-%m-%dT%H:%M:%S%.9f%:z";

fuzz_target!(|data: &[u8]| {
    let Some((stamp, rest)) = data.split_first_chunk::<12>() else {
        return;
    };
    let seconds = i64::from_le_bytes(stamp[..8].try_into().expect("8 bytes"));
    let nanos = u32::from_le_bytes(stamp[8..].try_into().expect("4 bytes"));
    let Ok(text) = std::str::from_utf8(rest) else {
        return;
    };
    let (text, pattern) = support::two_fields(text);

    let _ = hew_calendar::parse(text, pattern);
    let _ = hew_calendar::format(seconds, nanos, pattern);
    let _ = hew_calendar::parse(text, LOSSLESS);

    let Ok(parts) = hew_calendar::components(seconds, nanos) else {
        return;
    };
    let rebuilt = hew_calendar::from_parts(
        parts.year,
        i64::from(parts.month),
        i64::from(parts.day),
        i64::from(parts.hour),
        i64::from(parts.minute),
        i64::from(parts.second),
        parts.nanosecond,
    );
    assert_eq!(
        rebuilt,
        Ok((seconds, nanos)),
        "components do not rebuild {parts:?}"
    );
    let printed = hew_calendar::format(seconds, nanos, LOSSLESS)
        .expect("an in-range instant formats losslessly");
    assert_eq!(
        hew_calendar::parse(&printed, LOSSLESS),
        Ok((seconds, nanos)),
        "{printed:?} does not parse back"
    );
});
