# Local Make configuration

The Makefile optionally loads the ignored `.env` before resolving build
configuration. Defined temporary-directory and Cargo settings are exported to
child processes, while command-line assignments retain precedence. Undefined
settings remain absent: exporting an empty `CARGO_TARGET_DIR` causes Cargo's
output-directory resolution to fail.

Validated missing-file handling, propagation of local settings to a child
process, and command-line precedence with Make. `make test-build-harness` passed
on Linux. Windows and macOS checks were not run for this change.
