# Stdlib surface conventions

Use these conventions when writing or reviewing a standard-library module.
The [language guide](hew-language-guide.md) describes the current surface;
module declarations remain the reference for individual APIs.

## Types and declarations

Use explicit-width integers such as `i64` and `u32`; choose a width that matches
the operation and any external ABI. Use `isize` or `usize` where target-sized
arithmetic is intended. Preserve the declared index and length types of an API.

Declare Hew records with `type`, comma-separated fields and ordinary `impl`
methods. Use `self`, `var self` or `consume self` according to the operation.
Module paths are dotted, such as `std.fs` and `std.encoding.json`.

## Results and absence

A fallible operation returns `Result<T, E>` with a readable error. Its ordinary
name is the primary operation; do not create a panicking twin and a `try_`
alias just to distinguish error handling. A nonblocking probe may use `try_`
when it denotes a different operation.

Use `Option<T>` for expected absence, not sentinel integers or empty strings.
Callers propagate with `?`, recover with expression-local `handle error { ... }`,
or supply an absence default with `??`. `expect(reason)` is for an invariant.

## Values and resources

Ordinary values clean up automatically. Do not require manual free/close calls
for strings, collections or JSON values. Collection mutations need a `var`
binding; adapters consume the iterators they retain.

External resources declare `close(consume self)` returning unit. Scope cleanup
invokes it automatically; explicit close consumes the value. Keep fallible
finish, flush or commit separate so the caller can handle its outcome.
Borrowed parameters remain borrowed; transfer parameters spell `consume`.

## Calls and ABI boundaries

Ordinary calls may suspend without an await prefix. Fork creates a task and
await joins a task or vector of tasks. A scope is a value whose structured
cleanup finishes before its result or recovery is returned.

Extern declarations must match the actual ABI, including width, ownership,
layout and release pairing. Do not infer an ABI from a similar symbol's name.
For wire types, use explicit field tags and widths appropriate to the schema;
never reuse a retired field tag for a different meaning.
