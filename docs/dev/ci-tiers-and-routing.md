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
  pull request, and the script itself still exits nonzero for a stale
  nightly, an auth/scope defect, or a malformed response — none of its
  fail-closed semantics are softened. What is tolerated, while advisory, is
  the job's conclusion: the one assertion step carries `continue-on-error:
  true` so this check cannot itself turn a PR's check suite red for a nightly
  no author can fix — that would be a repository-wide deadlock, not a gate.
  The step still posts its own red annotation and keeps its failure outcome
  and logs, while the job and check conclusion are success, so the signal
  stays visible; it is not aggregated into `Build & test (Linux)` yet, and
  the tolerance is scoped to that one step, never the whole job. No flag, no
  bypass, no grace window,
  no permissive fallback was added to the *script* to make it land early —
  only the job's conclusion is tolerated, and only until activation.

  To activate, once a scheduled coverage-nightly run has succeeded:

  ```
  gh api "repos/:owner/:repo/actions/workflows/coverage-nightly.yml/runs?event=schedule&status=success&per_page=1" \
    --jq '.workflow_runs[0].run_started_at'
  python3 scripts/check-nightly-freshness.py   # must exit 0
  ```

  then, in one commit:

  1. remove `continue-on-error: true` from the assertion step — required
     checks are never tolerated;
  2. add `nightly-freshness` to `linux-required`'s `needs:`;
  3. add `NIGHTLY_FRESHNESS_RESULT: ${{ needs.nightly-freshness.result }}` to
     that job's `env:`;
  4. add `test "$NIGHTLY_FRESHNESS_RESULT" = success` to its assertion.

  `scripts/tests/test_ci_workflow_contract.py` holds the job to the advisory
  (tolerated) shape until then and to the required (untolerated) shape
  afterwards, and rejects a `needs:` entry that arrives without its assertion
  or a `continue-on-error` that survives activation.

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
417/732/470/383 s on run `32966803389` — and each additionally ran
`make stdlib`, so one tree's test binaries and shared Cargo outputs were
compiled four times. `linux-nextest-archive` builds that surface once and
hands it to them.

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

**How a consumer uses it — and where the tree has to land.** A compiled binary
carries absolute paths nothing remaps after the fact: `env!("CARGO_BIN_EXE_hew")`
is a literal rustc baked in at the producer, and `current_exe()`-relative stdlib
discovery walks `<target>/<profile>/deps/` upwards at run time. Run `33028214259`
materialized the tree under `RUNNER_TEMP` and priced that: about **1069 failures
on the archived nextest surface**, plus the three gates that compile a fresh test
binary in the shard (`observe-functional-test`, `libhew-link-race-test`,
`sandbox-parity`). `mqtt-broker-e2e` failed too and is **not** one of them — it
runs the archive's compiler by absolute path and compiles no test binary, so its
failure is behavioural and stays red on its own merits.

So the tree is materialized at **`$GITHUB_WORKSPACE/target`**, the producer's
own absolute path, which GitHub gives every job in a run. What the `RUNNER_TEMP`
rule protected still holds: the router reads `git ls-files --others
--exclude-standard`, and `target/` is `.gitignore`d, so an ignored path is as
invisible as an out-of-tree one. Extraction still stages under `RUNNER_TEMP`
(nextest wants an empty root) and only the archive's `target/` moves in. Cargo
is sent to `$RUNNER_TEMP/ci-cargo-target` in the job's **first step**, because
`Swatinem/rust-cache` resolves what it caches as `<workspace>/target` and never
reads `CARGO_TARGET_DIR` — hence `cache-targets: 'false'` on the shards. The
tree is then made **read-only** and is deliberately **not** Cargo's output.

Those are the same decision from two directions. The archive's contents are
certified — `libhew.a` carries a freshness certificate binding it to the
sources that produced it, `hew` is the compiler the producer built — so a tree
Cargo writes into is a tree those certificates stop describing. Pointing
`CARGO_TARGET_DIR` at it did exactly that:
`forced-cancel-composite-check-build` runs
`cargo build -p hew-cli -p hew-lib --features hew-runtime/forced-cancel-test`
in warm-up, before any gate, and overwrote both. Every later gate in that shard
would have linked a runtime nobody selected.

So the Makefile carries two authorities instead of one path: `CARGO_*` is where
Cargo WRITES, `ARTIFACT_*` is where shared artefacts are READ from. Locally
they are the same directory and nothing changes. In prebuilt mode
`ARTIFACT_ROOT` is the archive — `DEBUG_DIR`, `RELEASE_LIB_DIR`,
`WASM_DEBUG_DIR`, `LIBHEW` and `check-libhew-fresh.sh` follow it — while every
`cargo build`, `cargo run`, `cargo test` and feature-specific build writes to
Cargo's own directory. `chmod -R a-w` on the extracted tree makes that
separation enforced rather than trusted: a stray write fails at the writer with
a permission error instead of surfacing as a wrong link an hour later. There is
no repair path and no self-heal message, because there is nothing to repair.

**The projection.** `hew-testutil` resolves a shared artefact from its own test
executable's `<target>/<profile>/deps/` position, so an *archived* binary finds
the archive. A gate that COMPILES a fresh test binary produces one under
*Cargo's* root instead, where nothing had put those artefacts.
`scripts/ci-project-shared-artifacts.sh` closes it by symlinking, never copying,
the set read from `.config/nextest.toml`'s `archive.include` — the producer's own
pack list, `on-missing` policies included. Its `gate` verb projects, runs and
re-verifies in one process, so a failing gate cannot skip the check. Prebuilt
mode only.

JUnit is unaffected: nextest writes its store under the remapped *workspace*
root, so `target/nextest/<profile>/junit.xml` stays where the upload step
reads it.

**Two spellings of one selection.** `--binaries-metadata` and every Cargo
package/target flag are one clap group in cargo-nextest; passing both is
rejected before a test runs. Reuse mode therefore says `-E 'package(x)'` where
local mode says `-p x`. The Makefile carries both spellings side by side and
`scripts/tests/test_ci_prebuilt_artifacts.py` holds them to denoting the same
set.

**What still builds.** Everything the archive cannot carry, because it carries
runnable outputs and no rlibs or fingerprints: `make test-cabi`,
`make test-runtime-unit`, `make sandbox-parity`,
`make forced-cancel-composite-check`, `make stdlib-user-build-clean`, and any
`-build` form of those. They are bounded correctness fallbacks, not
artefact-recovery paths, and they are why the runner's disk guard stays.

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
4. Apply the acceptance inequality above against the 417/732/470/383 s warm-up
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

## Where the runner minutes went

The measured baseline is a green comprehensive run on `main`
(`32966803389`): **413 job-minutes, 69.1 minutes wall, critical path the
longest Linux shard at 68.2 minutes.** Every number below is per-command
elapsed time scraped from that run's four shard logs, not an estimate.

Four duplicates carried most of the waste. Each is removed by naming an owner
or a subsumption, never by dropping a check:

| Duplicate | Measured | Where it went |
|---|---:|---|
| `make test-compiler-pipeline` | 1190 s | Deleted from every tier. Its eight packages are a strict subset of `make test`'s workspace run under the same profile, and it was `comprehensive-only` — a tier that always selects `make test`. The lifecycle matrix it chained is selected in its own right. |
| Workspace Clippy in a shard | 92 s + a cold check-build | `lint` runs the identical invocation. Owned by `lint`, like its other gates. Clippy artefacts carry their own fingerprint, so no shared artefact could have supplied that build. |
| `make playground-check`, `make sandbox-fixtures-check` | 191 s | `playground-wasm-build` runs both with the browser tooling already provisioned. Owned by that job whenever the playground filter fires, and by the shards when it does not. |
| Debug compiler, `libhew.a`, release-lib compiler in `compiled-hew-linux` | 8.7 min | The archive producer builds them once. That job now downloads, certifies and packages; it compiles nothing and installs no LLVM. |

The four Linux shards additionally each ran `make stdlib` — 2.2, 3.2, 3.2 and
2.5 minutes, 11.1 job-minutes across the four — and derived their own warm-up.

The archive removes the compiled artefacts it carries, and nothing else. It
carries **runnable outputs only**: test binaries, non-test binaries integration
tests use, build-script output, linked paths, nextest's metadata, and the
shared Hew archives. It deliberately carries no `.fingerprint/`, no `deps/`
rlibs and no incremental state, because those are Cargo's private build ledger
— they are large, they go stale against any toolchain or flag difference, and a
consumer that trusted them would be trusting a second, weaker answer to "is
this current" than the freshness certificate already gives.

The consequence is exact and worth stating plainly: **anything that must
COMPILE still compiles**, into Cargo's own directory, from a Swatinem-restored
dependency layer. In the comprehensive profile that is `make test-cabi`
(hew-cabi is excluded from every workspace nextest invocation, and
`cargo nextest archive --workspace` is one of those), `make test-runtime-unit`
(`--no-default-features`, a different feature set and therefore a different
artefact), `make sandbox-parity` (plain Cargo plus the Node runner),
`make forced-cancel-composite-check` (a `hew-runtime/forced-cancel-test` build,
in its own target directory), `make stdlib-user-build-clean`, and
`make playground-check` where the playground job does not own it. The shards
still compile, still write to disk, and still need the runner's disk guard.

### Projected effect

Gate work in the comprehensive profile, weighted by the same measured seconds
and partitioned by the same LPT packer the dispatcher uses:

| | gate work | shard makespan |
|---|---:|---:|
| before this pass | 114.8 job-min | 28.7 min |
| after, with the corpus corrected from the same run | 94.2 job-min | 24.0 min |

The corpus correction matters to that second row and is why it is not lower.
Several commands had no measured row and were taking the 60-second default —
`make test-build-harness` among them, at a real 558 seconds. Feeding the
partitioner its true weights moved the makespan from an apparent 22.9 minutes
to an honest 24.0.

Whole-run, against the 413 job-minute baseline: **about 342 job-minutes, a 17%
reduction.** Roughly 20 of that is the subsumed compiler-pipeline gate, roughly
44 is warm-up and `make stdlib` no longer paid four times, and roughly 10 is
the compiled-Hew packager no longer rebuilding a compiler; the producer costs
about 22 back.

The critical path is the honest part. Removing gate work shortens the longest
shard, but the archive producer is a serial prefix the shards wait on, so the
run lands near **59 minutes against a measured 69.1** — and, measured instead
against this branch *without* the producer, the producer costs roughly six
minutes of wall time to save roughly forty runner-minutes. That trade is taken
deliberately, and it is reversible on its own: the producer is one job, one
`needs:` entry, and one Makefile mode.

None of these figures assume the cache behaviour below improves. If it does,
they get better; the projection does not spend it in advance.

### Measured: hosted run `33028214259`

| | job-minutes | wall |
|---|---:|---:|
| baseline (`32966803389`) | 413 | 69.1 min |
| projected | ~342 | ~59 min |
| **measured (`33028214259`)** | **353.7** | **65.3 min** |

The archive works: 2.03 GiB transferred, and the **archived nextest surface
issued zero rebuilds** in all four shards — not "zero compile requests per
shard", since the shards still compile what the archive cannot carry.

The 11.7 job-minute gap to projection is one shard: shard 2 ran 13.7 minutes
longer than its weighted plan. Its failures — ownership/MIR, SIGSEGV fuzz and
stream fixtures, compiled-Hew O0, MIR/LLVM drift, Windows and macOS full-suite —
have signatures that also appear on `main`, and several were reproduced against
base sources during discovery; the overrun itself was **not** reproduced on base
`be7c624d6`. So 353.7 / 65.3 is the number, and a ~340 / ~61 counterfactual is
conditional on an unproven attribution. Those failures are real and stay red.

The cache was cold and correctly so (first pull request run, and caches now
save only from `main`), so these are the pessimistic setup costs.

Two defects this run priced, both fixed in the revision that follows it: the
archive's materialization path (above), and ast-grep provisioned five times on
the compiled route, ~17.9 runner-minutes of which four installs are duplicates.
That is now one producer job uploading a tarball every consumer unpacks through
the same fail-closed bootstrap, plus a cache split into pinned `restore`/`save`
so only `main` writes and no two jobs race a key.

### Retained expensive gates, and why

* **`make test` (1122 s)** — the workspace suite. Nothing subsumes it; it is
  what subsumes the others.
* **`make hew-check-all` (694 s)** — compiles the whole `.hew` corpus through
  the DEBUG compiler. The release-lib compiler in the archive would run it
  several times faster, and that is not taken: debug assertions inside the
  compiler are part of what this gate proves, so the faster binary would be a
  silent coverage change.
* **`make test-build-harness` (558 s)** — the router's own counterfactuals,
  already halved by deleting its byte-identical twin. It is the price of a
  router that decides what CI runs.
* **`make fuzz-oracle` (531 s)** and **`make test-vertical-slice` (343 s)** —
  both compile `tests/vertical-slice/accept`, so the corpus is built twice.
  They are different oracles over it (expected output versus crash/abort under
  a bounded deadline), and merging them is a redesign, not a deduplication.
  Recorded here as the largest remaining overlap.
* **Windows and macOS full runs (46.8 and 41.1 min)** — linking, path
  separators, process spawning, calling convention, CodeView. Not reachable
  from a Linux runner at any price.

### Candidates left on the table, with the measurement each needs

* **`Free runner disk` (11 job-min over four shards)** — **not** a candidate
  for removal on the strength of this work. The shards still compile (see the
  list above), still install LLVM, and now additionally hold an extracted
  archive; the step exists because the runner died of ENOSPC twice. It already
  prints `df -h /`, so the headroom under the new arrangement is readable from
  the next run — and only a measurement showing real headroom, not the
  existence of the archive, would justify touching it.
* **More shards** — after the compiler-pipeline gate goes, the makespan is
  bounded by `make test` itself, so a fifth shard buys about a minute of wall
  for roughly twelve runner-minutes of setup. Not worth it.
