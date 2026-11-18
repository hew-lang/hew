# std.time.datetime

Checked UTC parsing, formatting and calendar components for `timestamp`.
Native and sandbox execution use the same UTC calendar engine. Unix seconds and the
nanosecond fraction retain their precision; explicit offsets normalize to UTC
and leap seconds return a parse error.

Calendar conversion supports proleptic Gregorian years −999999 through +999999.
A timestamp outside this range remains valid but returns `DateTimeError.Range`
when converted to a calendar view.

Use `format` and `parse` with strftime directives such as `%F %T %z`.
Use `to_iso8601` and `parse_iso8601` to round-trip nanoseconds.

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.
