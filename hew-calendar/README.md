# Hew calendar engine

Pure UTC calendar conversion, parsing and formatting shared by the native and
sandbox runtimes. Uses Chrono's proleptic Gregorian calendar, from year -262143
through +262142. Timestamp representation is wider; conversion outside this
calendar returns a range error.

Unix seconds exclude leap seconds. Parsing rejects Chrono's leap-second
representation instead of silently changing its instant. Explicit offsets are
normalized to UTC; formats without an offset denote UTC.
