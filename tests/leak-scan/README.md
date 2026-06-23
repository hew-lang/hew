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

Four consecutive orchestration lanes leaked internal identifiers into
committed source:

| Lane | Token | Location |
|------|-------|----------|
| PCA-8 | `PCA-8` | comment/string |
| wire-l1 | `L7` | reason/string |
| g8 | `G8` | comment/string |
| wire-l2 | `q185`, `Qa` | user-facing diagnostic strings |

Each was caught only at review, requiring a revision cycle. This fixture
proves the pre-push gate fires on every such pattern before code reaches
a reviewer.

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
