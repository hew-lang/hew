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

`make fuzz-smoke FUZZ_SMOKE_SECONDS=45` rebuilds the corpus, builds every
target, and runs a short local libFuzzer smoke window for each target.
