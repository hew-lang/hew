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
| `sccache` (workspace crates) | not installed, not enabled | enabled |

`SCCACHE_GHA_ENABLED` is a boolean and the GHA backend has no read-only mode,
so "restore but do not write" is not expressible for sccache; the choice is
write or nothing, and nothing is what stops PR-scoped entries evicting main's.

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
  `nightly-freshness` job and aggregated into `Build & test (Linux)`. It queries
  `event=schedule` only, so a manual dispatch cannot launder a rotting nightly
  green. Auth failures (401/403/404) are red naming the missing permission and
  are never retried or reported as staleness; transport and 5xx failures get a
  bounded retry and then fail closed.

There is no bypass label and no skip environment variable. A stale nightly is
fixed, or the nightly and its owner entry are deleted in one commit.

## Shard balance

`scripts/preflight-command-weights.tsv` holds measured elapsed seconds per
command. Regenerate it from a real run:

```
scripts/ci-preflight-dispatcher.sh --comprehensive --profile-json run.json
make preflight-weights-regen PROFILE_JSON=run.json
```

A stale weight costs makespan, never coverage: the partition is exhaustive and
disjoint whatever the weights say, and an unmeasured command falls back to its
timeout floor. Nothing regenerates it automatically.

The `CI budget` job writes this run's wall time, job-minutes, Linux shard
makespan and spread, and the selected route into the run summary. It asserts
nothing — a budget is a signal, not a gate.

## Build-once artefact sharing: measured, and cut

The four Linux shards each derive their own warm-up (376/406/338/188 s =
**1302 s of duplicated build per run**), so the prize is real. A generic
Rust/nextest archive to remove it is **not** taken, and this is the reasoning
rather than an omission.

The existing bundle (`scripts/compiled-hew-artifact.py`) binds `format`,
`source_revision`, and a per-file `sha256` — *which source produced these
bytes*. That is sufficient for two known binaries consumed by a known step, and
it stays exactly as it is.

It is nowhere near sufficient for a generic share. Such a manifest would have
to bind rustc version **and** commit hash, toolchain channel, target triple,
`Cargo.lock` hash, the effective RUSTFLAGS — including the mold split above,
which puts two Linux jobs on genuinely different fingerprints — cargo profile,
resolved feature set, and the nextest version. Nine fields, each of which is a
silent wrong-artefact bug when it drifts, guarding a saving bounded by
transfer time on an archive that would carry the whole workspace `target/`.

The kill criterion was written before the work: cut if the manifest plumbing,
the size, or the transfer time means the net saving is small or the complexity
exceeds what one reviewer can hold. It fails on the first and third. A generic
artefact-sharing framework that pays for itself in minutes and costs a
permanent correctness surface is a loss for a program whose thesis is
simplification.

Two cheaper things address the same 1302 s and are not this: main-only cache
saving (above), which makes each shard's warm-up start warm instead of cold,
and the routing fix, which stops most pull requests selecting the gates that
need the expensive warm-up at all.
