# Parser Fuzz Testing

Fuzz targets for hew-lexer and hew-parser using cargo-fuzz / libFuzzer.

## Prerequisites

```bash
rustup install nightly
cargo install cargo-fuzz
```

## Running

```bash
cd hew-parser
cargo +nightly fuzz run fuzz_parse       # UTF-8 source -> parser
cargo +nightly fuzz run fuzz_lex         # UTF-8 source -> lexer span invariants
cargo +nightly fuzz run fuzz_check       # parse-clean source -> checker
cargo +nightly fuzz run fuzz_mir         # typecheck-clean source -> HIR/MIR lowering
```

## Corpus

Corpus directories under `fuzz/corpus/` are generated and ignored by git.
Hydrate them from current v0.5 fixtures and parseable examples with:

```bash
make fuzz-corpus
```

`make fuzz-oracle` runs the fuzz-to-run completeness oracle over the tracked
regression set: each candidate `.hew` file goes through parse → check, and if it
is checker-valid it is compiled to a native binary and executed. Anything not
provably clean — an NYI/codegen refusal, a build ICE, a signal, an abort, a
timeout — fails unless it is registered in `tests/fuzz-oracle/expected-failures.txt`,
and a registered entry that starts passing fails too. Add `FUZZ_ORACLE_FULL=1` to
also sweep the raw cargo-fuzz corpus directories hydrated above; that sweep is
nondeterministic, so it is not part of CI.
