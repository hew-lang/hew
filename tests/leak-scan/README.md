# tests/leak-scan/

Fixtures for `make leak-scan` (`scripts/lint-orchestration-leak.sh`).

## Files

| File | Purpose |
|------|---------|
| `leaky-sample.rs` | Deliberately-leaky sample — every orchestration-token pattern that `lint-orchestration-leak.sh` is designed to catch. The rule **must** exit 1 when this file is fed to it. |

## The leak-scan gate

`make leak-scan` runs `scripts/lint-orchestration-leak.sh` against all
tracked source and exits 1 on any hit.  It is wired into:

- `make lint` (the `lint` dependency chain)
- `scripts/ci-preflight-dispatcher.sh` fallback lane

## Why this fixture exists

Orchestration lanes have leaked internal identifiers into committed source,
each caught only at review and requiring a revision cycle:

| Pattern class | Example tokens | Caught by |
|---------------|----------------|-----------|
| PCA tracking IDs | `PCA-8`, `PCA-12` | T1 |
| Fleet terms | `wire-fleet` | T2 |
| Lane names | `wire-l1`, `wireL2` | T3 |
| Lane IDs | `L7`, `L8`, `L9` | T4 |
| Q-tags (lowercase) | `q185`, `q023` | T5 |
| Q-tags (short) | `Qa`, `Qb` | T6 |
| Orch paths in strings | `.tmp/orchestration/…` | T7 |
| Orch vocab in source | `cross-eco`, `this lane`, `GPT` | T8 |
| Dist-work lane codes | `DIST-4`, `DIST-8` | T9 |
| Follow-up tags in docs | `F1`, `F3` in `*.md` | T10 |

T9 and T10 were added after `DIST-N` lane codes and `F<n>` follow-up tags
reached the PR gate as BLOCKs (~58 occurrences in source comments and a
`LESSONS.md` ledger-tag leak).

## Running the gate manually

```sh
# Should exit 0 (clean tree):
make leak-scan

# Proves the fixture is caught (the gate exits 1):
git add tests/leak-scan/leaky-sample.rs
scripts/lint-orchestration-leak.sh  # exits 1 — sees leaky-sample.rs
git restore --staged tests/leak-scan/leaky-sample.rs
```

The `lint-orchestration-leak.sh --self-test` mode runs a fully isolated
in-memory probe of every pattern without touching the real tree.
