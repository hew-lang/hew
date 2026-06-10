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
  goldens, fail on any drift (including missing or stale goldens).
- `make checked-mir-golden` — recapture the goldens. Only run this in a
  commit whose body justifies the dump change (e.g. a MIR carrier gained
  a typed field that the Debug rendering prints).

Family-coverage accounting lives in
`hew-types/tests/checked_mir_corpus_coverage.rs`: every
`RuntimeCallFamily` is either exercised by at least one golden dump or
pinned in `EXPECTED_UNCOVERED` with a probed reason. The pin is
fail-closed in both directions — newly covered families must leave the
list, silently lost coverage fails the test.

## Reject fixtures (`reject/`)

Move/init checker rejection fixtures — see `reject/README.md`.
