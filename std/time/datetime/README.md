# std.time.datetime

Checked UTC parsing, formatting and calendar components for `timestamp`.
Native and sandbox execution use one Chrono engine. Unix seconds and the
nanosecond fraction retain their precision; explicit offsets normalize to UTC
and leap seconds return a parse error.

Calendar conversion supports proleptic Gregorian years -262143 through +262142.
A timestamp outside this range remains valid but returns `DateTimeError.Range`
when converted to a calendar view.

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.
