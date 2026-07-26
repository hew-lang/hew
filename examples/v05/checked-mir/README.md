# MIR dump corpus

Two fixture sets live here.

## Positive golden corpus (`*.hew` + `golden/`)

Each top-level `.hew` fixture exercises one runtime-call family cluster
(collections, handle closes, bytes, actor link/monitor/spawn/ask,
channels + select, streams, tasks, supervisors, lambda actors, string
helpers, Option/Result helpers, regex match arms, math intrinsics).
`golden/<fixture>.{raw,elab}.mir` pins the byte-exact textual
`--dump-mir` output for both stages.

The corpus is the behavioural oracle for internal retyping work on the
compiler's runtime-call seams: a refactor claiming "zero behaviour
change" must leave every golden byte-identical.

- `make checked-mir-verify` — re-dump every fixture, diff against the
  goldens, fail on any drift (including missing or stale goldens) and
  check `golden/MANIFEST.sha256` against the goldens on disk.
- `make checked-mir-golden` — recapture the goldens. Only run this in a
  commit whose body justifies the dump change (e.g. a MIR carrier gained
  a typed field that the Debug rendering prints).

### Golden regeneration is reported, not silent

A regenerated golden encodes new behaviour as expected, so the text diff
cannot fail for the change regenerating it. Two things keep that visible:

- `make checked-mir-golden` diffs each dump against the committed golden
  before overwriting and prints a `N changed, M new, K unchanged` report
  with a `CHANGED <file> (+added -removed)` line per moved golden. Quote
  it in the commit body.
- `golden/MANIFEST.sha256` records one `sha256  name` line per golden and
  is rewritten by the same command. Regenerating goldens therefore always
  shows up as changed lines in one small central file — the count of
  changed lines *is* the count of regenerated goldens, readable at a
  glance even when the `.mir` diffs are collapsed. `checked-mir-verify`
  recomputes the manifest and fails on any mismatch, in both directions:
  a golden edited without recapture, and a manifest edited without a
  golden moving.

Family-coverage accounting lives in
`hew-types/tests/checked_mir_corpus_coverage.rs`: every
`RuntimeCallFamily` is either exercised by at least one golden dump or
pinned in `EXPECTED_UNCOVERED` with a probed reason. The pin is
fail-closed in both directions — newly covered families must leave the
list, silently lost coverage fails the test.

## Reject fixtures (`reject/`)

Move/init checker rejection fixtures — see `reject/README.md`.
