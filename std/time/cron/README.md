# std.time.cron

Cron expressions are ordinary Hew values with automatic value cleanup.
`parse` returns a checked error for malformed input.
`expr.next(after)` returns a checked result and uses typed timestamps to find a
whole-second occurrence strictly after `after`.

```hew
import std.time.cron;
import std.time.datetime;

fn main() {
    let schedule = cron.parse("0 9 * * Mon-Fri").expect("valid schedule");
    let next = schedule.next(timestamp.now()).expect("future occurrence");
    println(datetime.to_iso8601(next).expect("calendar range"));
}
```

Accepted forms:

- Five fields: minute, hour, day of month, month, weekday.
- Six fields: second followed by the five fields above.
- Seven fields: the six fields followed by a year.
- `@yearly`, `@monthly`, `@weekly`, `@daily` and `@hourly`.
- Lists, ascending ranges, steps, month names and weekday names.
- `?` as a wildcard in the day-of-month and weekday fields.

Day-of-month and weekday both have to match. Weekdays retain Sunday=1 through
Saturday=7. Named months and weekdays are case-insensitive. A numeric point
with a step continues through its field's range; wildcard year steps retain
their established 1970 phase. Explicit years are non-negative integers;
wildcard years also cover the calendar before year zero.

The search stores year ranges and progressions without expanding them, advances
calendar fields, and uses Gregorian repetition to bound impossible schedules.
It has no 2100 cutoff and does not scan elapsed seconds.
`CronError.NoNextOccurrence` reports an exhausted or impossible schedule.
`CronError.CalendarRange` reports an input or future occurrence beyond the
shared calendar engine's range.

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.
