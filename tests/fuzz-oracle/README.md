# Fuzz-to-run completeness oracle

## What this proves

Every checker-valid Hew program (one that passes `hew check`) must also
compile and run cleanly.  This oracle verifies that invariant continuously:

- For each `.hew` file in the candidate set, run `hew check`.
- If checker-valid, compile to a native binary (`hew build -o`) and execute it
  **directly** (not `hew run`, which masks signals — see below).
- Classify the outcome fail-closed.  Anything not provably clean is a failure
  unless explicitly registered in `expected-failures.txt`.

The oracle is the machine that makes "admit only what you lower" continuously
verifiable rather than a point-in-time audit.

## Why the binary is executed directly (not `hew run`)

`hew run` on Unix maps a child signal (SIGSEGV/SIGABRT) to exit code 1 via
`ExitStatus::code().unwrap_or(1)`.  A crash and a clean non-zero exit are
indistinguishable.  The oracle compiles the binary separately then runs it
directly so the real signal (negative returncode, e.g. -11 = SIGSEGV) is
visible for fail-closed classification.

## Candidate sources

| Source | Mode | Notes |
|---|---|---|
| `tests/vertical-slice/accept/*.hew` | CI (regressions) | Known-good well-typed programs — must all pass. |
| `tests/fuzz-oracle/regressions/*.hew` | CI (regressions) | Minimized failures promoted from fuzz campaigns. |
| `hew-parser/fuzz/corpus/**` | Manual (`FUZZ_ORACLE_FULL=1`) | Raw cargo-fuzz bytes; many fail the frontend gate (expected). Not in CI due to nondeterminism. |

## Ratchet: expected-failures.txt

`expected-failures.txt` lists regression filenames that are **known to fail**,
each annotated with a tracking issue.

**Gate behaviour:**
- A listed regression that **passes** → `unexpected-pass` → gate failure.
  This forces the entry to be removed and a positive guard to be added.
- An unlisted regression that **fails** → `unexpected-fail` → gate failure.
  This forces the failure to be registered (or fixed).

The ratchet fires in both directions so a confirmed gap that silently reverts
is caught the day it re-emerges.

## How to add a regression

1. Minimise the failing program (manually, or with a future `a3-fuzz-minimize`
   tool) until it is the smallest program that reproduces the failure.
2. Add it to `regressions/` with a descriptive filename.
3. Add an entry to `expected-failures.txt` with a tracking issue.
4. Run `make fuzz-oracle` to confirm the oracle classifies it as expected-fail.
5. Commit both files together.

When the underlying fix lands:
1. Remove the entry from `expected-failures.txt`.
2. Add (or update) a vertical-slice fixture that guards the fixed behaviour.
3. Run `make fuzz-oracle` to confirm the oracle now shows unexpected-pass
   (which requires the entry to be gone), then run it again after clearing
   expected-failures to confirm clean passage.

## Running the oracle

```
# CI mode (regressions only, fast, deterministic):
make fuzz-oracle

# Full mode (adds raw cargo-fuzz corpus, nondeterministic):
make fuzz-oracle FUZZ_ORACLE_FULL=1

# Direct invocation with options:
python3 scripts/fuzz/run-oracle.py --verbose
python3 scripts/fuzz/run-oracle.py --full --report /tmp/oracle-report.json
python3 scripts/fuzz/run-oracle.py --hew /path/to/hew --timeout 30
```

## Connecting fuzz campaigns to this oracle

The generative campaign (`scripts/fuzz/compiler.py`) and the raw cargo-fuzz
corpus find failures; this oracle permanently records and re-verifies them.
The seam is **hand-promotion**: take a minimized failure from a campaign,
add it to `regressions/`, register it in `expected-failures.txt`.  The
generative campaign remains a manual tool; CI stays corpus-deterministic.
