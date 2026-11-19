# Hew calendar engine

Native and sandbox runtimes use the Rust `time` crate for UTC calendar
conversion. Its extended proleptic Gregorian calendar covers years −999999
through +999999. Hew timestamps keep their full signed Unix seconds and
nanosecond fraction outside this calendar. Converting them to civil fields
returns a range error.

Custom formats use `time`’s strftime dialect. The ISO helpers preserve
nanoseconds and accept numeric offsets. Parsing without an offset in a custom
format uses UTC. Unix seconds exclude leap seconds, and parsing rejects a
seconds field of 60.
