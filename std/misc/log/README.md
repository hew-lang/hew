# std.misc.log

Levelled text and structured JSON logging to stderr.

Import `std.misc.log`. Use `log.setup()`, then `log.info(message)` or
`log.info_with(message, fields)`. Fields are a `Vec<string>` built with
`log.field`, `log.field_int`, `log.field_float` and `log.field_bool`.
`log.set_level(log.DEBUG)` sets the global threshold;
`log.set_format(log.JSON)` selects JSON output.

For separate settings, create `log.new_logger(level, format)` and pass it to
`log.logger_info(logger, message, fields)` or another `logger_*` function.
These functions return unit. This module does not provide automatic task or
actor tracing, telemetry propagation, or an error-discard API.

See [the module declarations](log.hew) for signatures and examples, and the
[standard library overview](../../README.md) for other modules.
