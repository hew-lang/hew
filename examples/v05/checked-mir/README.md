# Checked-MIR execution corpus

Each top-level `.hew` fixture exercises a compiler/runtime seam. A runnable
fixture has a committed `<fixture>.expected` transcript containing its exit
status and verbatim stdout.

`make checked-mir-run` compiles every fixture, reads raw MIR only to determine
whether it declares `main`, and executes every runnable fixture under a
wall-clock cap. It compares the resulting transcript with the committed
expectation and enables actor-balance checking. The gate therefore catches a
wrong answer, crash, timeout, or actor leak without pinning incidental MIR
rendering.

`make checked-mir-expect` recaptures transcripts. It refuses to write an
expectation for a compile error, signal/timeout, or actor leak, so breakage
cannot be blessed as a capture side effect.

Runnability is compiler-derived rather than maintained as a name list:

- a fixture whose raw MIR declares `main` must have a `.expected` sibling;
- a library-only fixture must not have one;
- an expectation without its `.hew` fixture is an error.

Typed source-to-MIR ABI mapping is covered by focused Rust tests, including
the concrete scalar Vec family and arity matrix in
`hew-mir/tests/lowering_calls/vec_runtime_family_abi.rs`.

## Reject fixtures (`reject/`)

Move/init checker rejection fixtures — see `reject/README.md`.
