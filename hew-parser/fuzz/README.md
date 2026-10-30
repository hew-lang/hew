# Compiler Fuzz Testing

Fuzz targets for the Hew frontend using cargo-fuzz / libFuzzer. The targets
here build on a small host (the continuous fuzzing service rotates through all
of them); the diagnostic renderer target links the whole driver and lives in
`hew-cli/fuzz`.

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
cargo +nightly fuzz run fuzz_fmt         # hew fmt: faithful reprint, fixed point
cargo +nightly fuzz run fuzz_check_determinism  # two checks agree; const order
cargo +nightly fuzz run fuzz_project     # hew.toml + modules in a temporary tree

cd ../hew-cli
HEW_STD=$PWD/../std cargo +nightly fuzz run fuzz_diagnostics  # text and JSON diagnostics
```

`hew-cli/fuzz` links the whole driver, including `hew-codegen-rs` (inkwell and
`llvm-sys`), so building it needs LLVM 22 and about 4 GB of target directory.
It stays out of the Cargo workspace and the continuous service, which builds
only `hew-parser/fuzz`. Where LLVM is installed, `cargo fuzz list --fuzz-dir
hew-cli/fuzz` from the repository root lists its targets. It runs three full
`hew check`s per input, about one input a second, so its seed set is a sample.

The continuous service runs each target with `-s none -rss_limit_mb=1024
-timeout=10`; use the same limits locally.

| Target                   | Properties                                                                                                                                                                                                                                                                                                                                                                                                                           |
| ------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `fuzz_fmt`               | For source without parse errors, `hew fmt` output passes the fidelity oracle (same program, every comment in place, line endings kept) and formatting it again changes nothing.                                                                                                                                                                                                                                                      |
| `fuzz_check_determinism` | Two fresh checkers report the same errors and warnings in the same order (unused-variable and unused-`var` warnings compared as a set until their emission order is fixed; raw expression types, whose inference variables number from a process-wide counter, are not compared) and publish the same expression, binding and annotation types. Rotating the module's `const` items among their slots keeps the verdict (spec §3.2). |
| `fuzz_project`           | Manifest parsing, package resolution, manifest validation and the `hew check` frontend never panic; a failed check reports something; a package entry lies inside the package; the working directory is never searched; without a path dependency, quoted import or link, every source read lies in the project or the standard library.                                                                                             |
| `fuzz_diagnostics`       | `hew check` text and JSON never panic, exit alike and agree on each diagnostic's file, line and column; JSON spans lie within their file on character boundaries; two runs print identical JSON; output stays within a constant factor of the input.                                                                                                                                                                                 |

`fuzz_project` reads a text archive: a line `=== <relative path>` starts a
file of the project and `=== @link <relative path>` links that path to a
directory outside the project. Paths that would leave the project are dropped
by the harness, as are paths through a link. Each input gets a fresh tree
under `$TMPDIR/hew-fuzz-project-<pid>/`, removed once the input has run.

## Corpus

Corpus directories under `fuzz/corpus/` are generated and ignored by git.
Hydrate them, and `hew-cli/fuzz/corpus/`, from current fixtures, examples and
example projects with:

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
