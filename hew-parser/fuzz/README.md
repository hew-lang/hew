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
cargo +nightly fuzz run fuzz_parse   # Fuzz the parser
cargo +nightly fuzz run fuzz_lex     # Fuzz the lexer
```

## Corpus

Initial corpus seeds are in `fuzz/corpus/`. New interesting inputs are saved automatically.
