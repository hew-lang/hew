# CI tiers, routing, and cache

How a change decides what must be proven before it merges, and where each
proof lives. One page, because the answer used to be spread across two path
filters, three hand-written base refs, and a comment.

## Routing: what runs, and why

`scripts/ci-preflight-route.sh` derives the route from the CI event. There is
one derivation and every job reads it.

| Event | Question | Route |
|---|---|---|
| `pull_request` | is THIS DIFF sound? | `--base <the PR's own base SHA>` |
| `push` to main | is the INTEGRATED TREE sound? | `--comprehensive --fail-fast` |
| `workflow_dispatch` | same as push | `--comprehensive` |
| anything else | — | error |

`merge_group` is deliberately unhandled. A queue candidate is not a diff
against a known base, so it needs its own derivation and a real merge-group run
to validate; inferring one here would ship an unexercised path.

`scripts/ci-preflight-dispatcher.sh` then selects gates by intersecting the
changed paths with each gate's `# inputs:` declaration in the Makefile.

| Diff | Selection |
|---|---|
| non-empty, every path classified | narrow: every gate whose declared inputs intersect |
| non-empty, any path unclassified | comprehensive — fail closed |
| **empty, under CI or with an explicit `--base`** | **error** |
| empty, local, no `--base` | informational exit 0 |

An empty change set on CI means the base derivation is wrong. Narrowing is
never the right answer to an empty diff, and neither is passing: a push to main
whose base collapsed to a self-diff once went green having run nothing.
`--allow-empty` is the only intentional spelling and is refused under CI.

## Platform tier: does this need Windows and macOS?

`scripts/lib/gate_inputs.py platform-tier` is the single authority. It emits
`none | smoke | full`, taken as the maximum over the changed paths, so it is
monotone: one native path escalates the whole run.

| Tier | Trigger | Windows and macOS run |
|---|---|---|
| `none` | every changed path is classified prose | nothing; `Platform gates` reports green saying so |
| `smoke` | any other classified change | build, the E9 probes, `make test-cabi`, and the platform behaviour suite |
| `full` | native link / LLVM / C-ABI / process / fs / net / codegen / toolchain / `Cargo.lock` / `.github/actions/**` | smoke plus the whole workspace nextest |
| — | any path matching no rule | `full` — fail closed |

**The platform behaviour suite** is `[profile.platform]` in
`.config/nextest.toml`. It selects the surfaces only a non-Linux runner can
observe: linker driver, cross-target selection, process spawn and exit codes,
trap-to-crash abort paths, MSVC-vs-SysV calling convention and dispatch,
CodeView/DWARF debug info, and the concurrent link + shared-artifact contract.
Ordinary platform-neutral correctness runs on Linux, once. The step asserts the
selection is non-empty and prints the count; the count is never asserted equal.

## Cache: what is saved, by whom

Three mechanisms, three decisions.

| Layer | On a pull request | On `refs/heads/main` |
|---|---|---|
| `Swatinem/rust-cache` (registry/deps) | restores | restores **and saves** |
| `sccache` (workspace crates) | `READ_ONLY` | `READ_WRITE` |

Both layers restore everywhere and write only from the default branch, because
GitHub Actions cache entries saved on the default branch are readable from
every branch while a branch's own entries are readable by nobody else. One open
pull request had taken 10.47 GB of the 10 GB repository budget with entries no
other branch could use, evicting main's 0.01 GB — so every pull request
restored from a layer it had just displaced.

sccache's half of that was previously fixed by not installing sccache off main
at all, which stopped the eviction by giving pull requests nothing: the
`SCCACHE_GHA_ENABLED` boolean had no read-only companion at the v0.10.0 the
action installed. sccache **0.16.0** added `SCCACHE_GHA_RW_MODE`
(`READ_ONLY` | `READ_WRITE`, default `READ_WRITE`), so the mode now carries the
policy and the tool runs everywhere. The pin is 0.16.0 rather than the current
0.17.0 on purpose: 0.16.0 is the first release with the mode, and 0.17.0 ships
a new client-side execution architecture.

Reading the per-job `sccache` summary block on a pull request: `writes` is `0`
and `write errors` equals the miss count, because `READ_ONLY` refuses each
store and counts a refused store as a write error while still returning the
compiler's own output. That is the mode working. `SCCACHE_IGNORE_SERVER_IO_ERROR`
keeps an unreachable cache a cold compile rather than a hard rustc failure — a
DNS blip took out `Install cargo-deny` on 2026-08-21 before a single check had
run.

Cache keys name the **rustc fingerprint**, never the job:

| Key | Fingerprint |
|---|---|
| `linux-mold` | x86_64-linux **with** `-C link-arg=-fuse-ld=mold` |
| `linux` | x86_64-linux without it |
| `windows` | windows-msvc |
| `macos` | darwin-arm64 |

The mold rustflag stays declared in exactly the jobs that install mold. It is
not hoisted to workflow level or `.cargo/config.toml`: `lint`, `license-check`,
`docs-and-scripts` and `playground-wasm-build` do not install mold, and the
flag applies to host build-script and proc-macro links even in the wasm job.
`scripts/tests/test_ci_workflow_contract.py` enforces both directions.

## Scheduled tier

`.github/nightly-owners.yml` names an owner and a freshness window per
scheduled workflow. It is validated against the workflows, both directions: a
scheduled workflow with no entry is an error, and an entry for a workflow that
no longer schedules is an error.

- **Reporter** — `.github/workflows/scheduled-failure-report.yml`, called as a
  job by each scheduled workflow, on green as well as red. Red opens or updates
  one `ci-nightly` issue with a *verified* assignee; the next green closes it.
  The caller grants `issues: write`, because a called workflow cannot elevate
  its caller's token scope.
- **Freshness** — `scripts/check-nightly-freshness.py`, run by the standalone
  `nightly-freshness` job. It queries `event=schedule` only, so a manual
  dispatch cannot launder a rotting nightly green. Auth failures (401/403/404)
  are red naming the missing permission and are never retried or reported as
  staleness; transport and 5xx failures get a bounded retry and then fail
  closed.

  **Advisory today, required after one green nightly.** The check runs on every
  pull request and reports its own red, but is not aggregated into
  `Build & test (Linux)` yet. The last successful *scheduled* coverage-nightly
  run predates the gdb provisioning fix, so requiring it now would turn the
  required Linux context red on every pull request for a reason no author can
  fix — a repository-wide deadlock, not a gate. Nothing about the check is
  softened to make it land: no flag, no bypass, no grace window, no permissive
  fallback. One `needs:` edge is deferred.

  To activate, once a scheduled coverage-nightly run has succeeded:

  ```
  gh api "repos/:owner/:repo/actions/workflows/coverage-nightly.yml/runs?event=schedule&status=success&per_page=1" \
    --jq '.workflow_runs[0].run_started_at'
  python3 scripts/check-nightly-freshness.py   # must exit 0
  ```

  then, in one commit:

  1. add `nightly-freshness` to `linux-required`'s `needs:`;
  2. add `NIGHTLY_FRESHNESS_RESULT: ${{ needs.nightly-freshness.result }}` to
     that job's `env:`;
  3. add `test "$NIGHTLY_FRESHNESS_RESULT" = success` to its assertion.

  `scripts/tests/test_ci_workflow_contract.py` holds the job to the advisory
  shape until then and to the required shape afterwards, and rejects a `needs:`
  entry that arrives without its assertion.

There is no bypass label and no skip environment variable. A stale nightly is
fixed, or the nightly and its owner entry are deleted in one commit.

## One gate, one home

`lint` runs its authority gates with no `if:` guard, so selecting them into a
Linux shard as well runs each of them twice for one change — `structural-lint`
cost 242s in a shard and 246s in `lint`. Jobs that dispatch the router declare
`LINT_GATE_OWNER=lint`, and the router skips the gates `lint` owns, exactly as
`COMPILED_HEW_GATE_OWNER=aggregate` already lets the compiled-Hew aggregate own
its two suites.

A local `make preflight` sets no owner and still runs everything, which is what
keeps it a rehearsal of CI rather than a subset of it. The owned list is held
equal to the parsed `lint` job by `scripts/tests/test_ci_workflow_contract.py`,
in both directions: a gate added to `lint` but missing from the list keeps
running twice, and a gate removed from `lint` but left in the list would stop
running on the pull-request path altogether.

## Dogfood IR: telemetry, not a gate

`make dogfood-compile-measure` compiles the dogfood fixture and REPORTS what it
produced. It used to compare exact LLVM define-block bytes, define count and
basic-block count against a committed baseline, on the required lint job; every
benign codegen change broke it and the only available response was to
regenerate the baseline, so it measured nothing anybody decided on while
costing time on the merge path.

It still fails if the fixture does not compile, or emits IR with no functions —
that is a defect, not a shape opinion. The numbers go to the nightly run
summary. Regressions in what this fixture compiles to are caught by the
ll-byte-identity oracle, where byte identity IS the contract, and by the
compiled-Hew behaviour suites, which run the programs.

## Structural authority: membership, not magnitude

`scripts/structural-authority-inventory.tsv` records which MODULES are reviewed
and permitted to carry each parsed authority form. It no longer records how many
sites each module holds.

- an authority site in a module with no row is red — new authority landed
  somewhere nobody looked;
- a row whose module carries no site is red — the row is stale, or the parse
  stopped matching and the audit has quietly become vacuous;
- sites moving around inside a reviewed module are that module's business.

The retired per-(form, path) counts fired on every legal refactor inside an
owner module, and the file's own prologue conceded they could not see a
net-zero relocation between two listed modules. What is deliberately no longer
detected: an extra call inside an already-reviewed function, a third identity
field on an already-reviewed struct, a new variant on an already-reviewed
carrier. For the last of those, rustc rejects an unhandled variant on a closed
enum before this audit runs.

The canonical-keyspace allowances carry the same semantics and the same
review requirements — owner, follow-on work item, reason — without a per-file
occurrence count.

## Shard balance

`scripts/preflight-command-weights.tsv` holds measured elapsed seconds per
command, and it is the partitioner's only weight authority — the dispatcher
loads it at startup. Regenerate it from a real run:

```
scripts/ci-preflight-dispatcher.sh --comprehensive --profile-json run.json
make preflight-weights-regen PROFILE_JSON=run.json
```

A stale weight costs makespan, never coverage: the partition is exhaustive and
disjoint whatever the weights say, and an unmeasured command falls back to its
timeout floor or the default. A missing or unreadable corpus is likewise not
fatal — every command falls back and the run proceeds. Nothing regenerates it
automatically.

Timeout **floors** deliberately stay in the dispatcher. They do not degrade
safely: an unreadable file that zeroed them would drop `make test` to the 600 s
comprehensive tier against a measured 1104 s runtime and kill a healthy gate.
Floors and weights cannot share one degrade contract, so they do not share one
home.

The `CI budget` job writes this run's wall time, job-minutes, Linux shard
makespan and spread, and the selected route into the run summary. It asserts
nothing — a budget is a signal, not a gate.

## Build once, run four times

The four Linux shards each derived their own warm-up — a measured
376/406/338/188 s, **1302 s of duplicated build per run** — compiling the same
test binaries and the same shared Cargo outputs four times over one tree.

`linux-nextest-archive` builds that surface once and hands it to the shards.

**What crosses.** A `cargo nextest archive` tarball: the selected test
binaries, `binaries-metadata.json` and `cargo-metadata.json`, build-script
output, linked paths, and the shared Hew outputs listed explicitly in
`.config/nextest.toml` under `[profile.ci] archive.include` — the debug
compiler, `libhew.a` and its freshness certificate, the native runtime archive,
the two `wasm32-wasip1` archives, and the release-lib launcher. Every host
entry is `on-missing = "error"`.

**What does not.** No `deps/`, no `.fingerprint/`, no incremental state, no
recursive `target`. Those are Cargo's build state; shipping them would make the
archive a second, staler authority on what is built, and would recreate the
whole-target-directory transfer this replaces.

**Identity comes from the workflow, not a manifest.** The consumers are
DAG-gated on the producer in the same run, `download-artifact` reads the
current run of the current repository, the artifact is immutable and its digest
is validated (`digest-mismatch: error`), and producer and consumers pin the
same runner image, toolchain, LLVM, mold RUSTFLAGS and checkout SHA. A manifest
binding rustc version, ref, `Cargo.lock` hash, feature set and nextest version
would restate those guarantees as a second identity system with its own drift —
nine fields, each a silent wrong-artefact bug when it slips. That was the
kill criterion the first pass of this work applied, and it still holds; what
changed is that the same-run DAG makes the manifest unnecessary rather than
merely expensive. `scripts/compiled-hew-artifact.py` keeps its manifest for the
different problem it solves: a compact binary pair whose consumer must know
which revision produced it.

**How a consumer uses it.** `cargo nextest list --extract-to` (nextest's own
extractor; it runs no test) unpacks under `RUNNER_TEMP`, never the checkout —
an archive in `$PWD` is untracked paths no gate declares, which makes the
router fail closed to comprehensive on every run. The extracted `target` is
exported as `CARGO_TARGET_DIR`, so `scripts/cargo-output-dir.py` and everything
derived from it — the Makefile's `DEBUG_DIR`/`RELEASE_LIB_DIR`/`WASM_DEBUG_DIR`,
`check-libhew-fresh.sh`, and `hew-testutil`'s own resolver — already point into
it. No second path authority is introduced. JUnit is unaffected: nextest writes
its store under the remapped *workspace* root, so
`target/nextest/<profile>/junit.xml` stays where the upload step reads it.

**Two spellings of one selection.** `--binaries-metadata` and every Cargo
package/target flag are one clap group in cargo-nextest; passing both is
rejected before a test runs. Reuse mode therefore says `-E 'package(x)'` where
local mode says `-p x`. The Makefile carries both spellings side by side and
`scripts/tests/test_ci_prebuilt_artifacts.py` holds them to denoting the same
set.

**What still builds.** `make test-cabi` compiles hew-cabi: every workspace-wide
nextest invocation in this repository excludes it, and
`cargo nextest archive --workspace` is one of those.
`test-runtime-no-default-features` compiles its own feature set, which is a
different build and not shareable. `sandbox-parity` runs plain Cargo. These are
bounded correctness fallbacks, not artefact-recovery paths.

**Failure is closed.** A producer failure skips the shards, `linux-required`
sees a non-success matrix result, and the required context is red. A digest
mismatch, a failed extraction, a missing metadata file, a missing declared
archive entry, or a half-supplied reuse environment fails before any gate runs.
There is no per-shard rebuild fallback: it would turn a producer defect into
four slow green runs.

**Acceptance is measured, not asserted.** The producer reports its shared
artefact build time, archive creation time and archive size; each consumer
reports download and extract time and the elapsed time of its first use of the
prebuilt tree. Accept only when

```
producer build + archive + max(download + extract) < the four-shard warm-up it replaced
```

and aggregate runner-minutes fall. Nothing in CI asserts those numbers — a
threshold on a timing is a flake generator, and the question they answer is
whether to keep the arrangement, not whether this run is correct.

## Hosted proof steps

Four properties in this document cannot be proved on a developer machine: they
are properties of GitHub's artifact transport, its cache scoping, its issue
tracker, and its branch-protection contexts. Each is proved on a pull-request
branch, from the run summaries and the API, without breaking protected `main`
and without manufacturing a green.

Read the run summary blocks these produce as evidence, not as gates: none of
them asserts a number, and none of them can make a run red for being slow.

### 1. Build once — the archive actually removes the duplicated build

On any pull request that selects the compile path:

1. Open the `Build Linux test archive` job summary. Record `shared artefact
   build`, `archive creation`, and `archive size`.
2. Open each of the four `Linux gates (shard N/4)` summaries. Record `download`
   and `extract`, and `shared artefact verify`.
3. Confirm each shard's warm-up section in the `Run change-scoped tests` log
   contains no `cargo nextest run … --no-run` and no `make stdlib` build — the
   dispatcher prints its derived warm-up before executing it.
4. Apply the acceptance inequality above against the 376/406/338/188 s warm-up
   this replaced, and read `CI budget`'s `Linux shard makespan` and
   `Job-minutes` lines for the aggregate.

Counterfactual, on a disposable branch only: delete one entry from
`[profile.ci] archive.include` and push. The producer must fail in
`Archive the Linux test binaries` with `extra path … not found`, the four
shards must be skipped, and `Build & test (Linux)` must be red. Revert before
merging.

### 2. Cache — pull requests read and do not write

1. On a pull-request branch, open any Rust job's `sccache — <key>` summary
   block. `mode` must read `READ_ONLY`, `writes` must be `0`, and `hits` must
   be non-zero once main has populated the layer.
2. Push a second commit that changes only a comment and re-read the same
   block: `hits` must rise. A cold second run means the default-branch layer is
   not being restored, which is a cache-key question, not a mode question.
3. Compare against a `main` run's block, where `mode` reads `READ_WRITE` and
   `writes` is non-zero.
4. `gh api repos/:owner/:repo/actions/cache/usage` before and after a
   pull-request run: `active_caches_size_in_bytes` must not grow from the
   pull request.

The local analogue, which proves the mechanism but not the GHA backend:
`SCCACHE_LOCAL_RW_MODE=READ_ONLY sccache rustc …` against an empty
`SCCACHE_DIR` compiles successfully, reports `cache_writes: 0` and one
`cache_write_error`, and leaves the cache directory empty; the same compile in
`READ_WRITE` reports one write and leaves one file.

### 3. Scheduled tier — one observed issue open, assign, and close

Issues are repository-scoped, not branch-scoped: a reporter triggered from a
branch opens a **real** issue. Treat every test-triggered issue as real, verify
it through the API, and close it by making the run green rather than by hand.

1. On a disposable branch, add a `run: exit 1` step to one nightly workflow's
   job, gated `if: github.event_name == 'workflow_dispatch'` so the real
   schedule never sees it. Leave the `report:` job's `needs:`/`if: always()`
   wiring untouched.
2. `gh workflow run <file>.yml --ref <branch>`, then poll
   `gh run list --workflow=<file>.yml --branch <branch> --limit 1
   --json databaseId,status,conclusion` until `completed`.
3. `gh issue list --label ci-nightly --state open --json number,title,assignees,url`
   — exactly one issue, its title naming the workflow, its body naming the
   failing job, and `assignees[].login` containing the owner-table login. Read
   the API response, not the web UI: `POST /issues/{n}/assignees` returns 201
   and assigns nobody for a non-assignable login, which is the failure this
   assertion exists for.
4. Remove the forced-red step, dispatch again, and confirm the run is green.
5. `gh issue view <n> --json state,closedAt` → `CLOSED`, and
   `gh issue view <n> --json comments -q '.comments[-1].body'` names the green
   run URL.
6. Leave the issue closed. Do not merge the forced-red scaffolding.

The `ci-nightly` label does not exist in this repository yet; step 3 also
proves the reporter's create-on-404 path, since an issue created with an
unknown label is a 422.

### 4. Platform tier — `full` really runs the full workspace

1. Push a change touching only a Linux-only path (for example
   `hew-analysis/src/…`) and confirm `Build & test (Windows)` and
   `Build & test (macOS arm64)` report a deliberate skip at tier `none`, with
   `Platform gates` green because the tier is `none` and not because two jobs
   reported nothing.
2. Push a change touching `.github/actions/setup-llvm/action.yml` — the file
   whose two non-nested `dorny` filters produced the original wrong-green — and
   confirm the tier is `full` and both platform jobs run the whole workspace
   suite rather than the narrowed smoke set.
3. Read the platform behaviour suite's printed selected count on both
   platforms. It must be non-empty. It is printed, never asserted equal.

### 5. Required contexts — before any ruleset edit

`nightly-freshness` moves into `linux-required` only after a **scheduled**
coverage-nightly run has succeeded on `main` and after proof 3 above has been
observed. Both halves are required: a green dispatch does not prove the
schedule works, and a working freshness check over an unowned tier is half a
tier.

Before editing the ruleset for any context change:

1. `gh pr list --state open --json number,headRefName` and, for each,
   `gh pr checks <n>` — every context that is about to be required must
   already be emitting on every open pull request. A head that predates the
   workflow commit will not emit it and must be rebased first.
2. Land the workflow change on `main` while the old contexts still report.
3. Only then edit `required_status_checks`.
4. Re-run step 1 immediately afterwards; every open pull request must show the
   new contexts green.
5. Only after one pull request has merged under the new set may the superseded
   context names be removed from the workflow definitions. Never reorder 4 and
   5.

Rollback order is the reverse: restore the ruleset's previous context list
first, then revert the workflow commits.
