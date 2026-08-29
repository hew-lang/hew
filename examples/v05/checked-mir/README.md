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

- `make checked-mir-verify` diffs each dump against the committed golden
  before overwriting and prints a `N changed, M new, K unchanged` report
  with a `CHANGED <file> (+added -removed)` line per moved golden. Quote
  it in the commit body.
- `golden/MANIFEST.sha256` records one `sha256  name` line per golden and
  is rewritten by the same command. Regenerating goldens therefore always
  shows up as changed lines in one small central file — the count of
  changed lines _is_ the count of regenerated goldens, readable at a
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

## Execution gate (`*.expected`)

Dumping is not running. A fixture can segfault on every execution while
every golden stays byte-identical, because the golden diff only sees text
the compiler printed — it never loads the program. That is not
hypothetical: a drop-elaboration change once made
`channel_auto_close_scope` segfault unconditionally while
`checked-mir-verify` stayed green, and the same change regenerated both
channel goldens, so the text diff could not have failed for it.

`make checked-mir-run` builds and executes every fixture and diffs a
transcript against the fixture's committed `<fixture>.expected` sibling:

```
exit: 0
stdout:
ready
```

The transcript is the exit status and the verbatim stdout. stderr is
deliberately not pinned: the runtime emits timing-dependent shutdown
diagnostics (see `actor_ask_race`, whose losing worker is still parked
when `main` returns) that would make the gate flaky on a loaded machine.
Exit status plus stdout already fail on a crash, a wrong answer, or
silence where output was expected. A fixture with no output still gets an
expectation — the `stdout:` section is simply empty — so "prints nothing"
is asserted rather than assumed.

- `make checked-mir-run` — the gate. Runs in CI and in the compiler
  pipeline and types preflight tiers alongside `checked-mir-verify`.
- `make checked-mir-expect` — recapture the transcripts.

### Runnability is read back from the compiler, not listed

A fixture is runnable exactly when its raw MIR declares a `main` entry
point, which `--dump-mir raw` reports. There is no list of names to
maintain and none to go stale:

- a fixture whose MIR declares `main` **must** have a `.expected`;
  missing one fails the gate;
- a fixture whose MIR declares no `main` (a library-only fixture such as
  `pattern_owned_project_predicate_consume`) **must not** have one; a
  leftover `.expected` fails the gate, so a fixture that loses its `main`
  cannot silently keep a passing artefact;
- a `.expected` with no fixture fails the gate.

The one fixture the compiler refuses to build,
`actor_link_monitor`, pins that refusal as its transcript:

```
compile-error: E_NOT_YET_IMPLEMENTED
```

`hew_actor_unlink` is an admitted-but-not-wired runtime family, and
codegen fails closed on it. That is asserted, not waived: when the
lowering arm lands the fixture will build, the transcript will no longer
be a `compile-error:` line, and the gate will fail until the expectation
is replaced with a real run transcript.

`checked-mir-expect` refuses to write a transcript for a fixture that
fails to build, is killed by the wall-clock cap, or dies on a fault
signal. Blessing breakage therefore cannot be a side effect of running
the capture command — it takes a hand-written file, which is authored
content a reviewer sees.

## Reject fixtures (`reject/`)

Move/init checker rejection fixtures — see `reject/README.md`.
