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

`make fuzz-oracle` explicitly replays the raw corpus through checking, native
compilation and execution. Invalid source is expected; compiler crashes,
runtime signals, timeouts and output floods are reported for investigation.
Set `FUZZ_CORPUS_DIR=/path/to/inputs` to replay another corpus. Each execution
has a timeout and output cap, and timeout cleanup terminates its process group.

Stable regression programs belong to `tests/core-acceptance/cases/`, where
observable output, exit status and cleanup are checked at O0 and O2. Raw replay
has no expected-failure ledger and is not a default CI gate.
