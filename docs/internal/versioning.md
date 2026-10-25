# Hew Versioning Policy

Guidance, not strict rules. The goal is that a contributor reading a tag, a
PR, or `Cargo.toml` can answer "what changed, and what do I need to do about
it?" without reading the commit log.

## Scheme

Hew uses **SemVer-ish** versioning: `vMAJOR.MINOR.PATCH`, with optional
pre-release suffix `-alpha.N`, `-beta.N`, `-rc.N`, or `-pre`.

While Hew is pre-1.0, MAJOR stays at `0`. The 0.6 series develops edition
2026 through feature and maintenance releases, including intentional surface
changes with migration notes. The [specification](../specs/HEW-SPEC-2026.md)
owns the language's version and edition contract.

## What each axis signals

| Bump  | Signals                                                                 | Examples                                                   |
| ----- | ----------------------------------------------------------------------- | ---------------------------------------------------------- |
| MAJOR | Stability commitment. Multi-year deprecation cycle. Not yet reached.    | `v1.0.0` — see "When v1.0" below.                          |
| MINOR | New development series with a coherent scope.                           | `v0.5 → v0.6` (native compiler and language development). |
| PATCH | Feature or maintenance release within the current development series. | `v0.6.0 → v0.6.1` (language additions and fixes).         |

### MINOR bump (`0.Y`)

Bump MINOR when opening a new development series with a coherent scope.
An individual feature, refactor or migration does not by itself require
leaving the 0.6 series.

### PATCH bump (`Y.Z`, `Z > 0`)

Bump PATCH to deliver a useful, qualified set of changes within the current
series. In 0.6.x this includes language features, standard-library additions,
correctness and performance fixes, and approved incompatible changes.
Packages using an addition declare its minimum compiler version through
`hew` in `hew.toml`. Incompatible changes require exact migration instructions
and coherent updates to all in-tree consumers.

### Development builds

Immediately after publishing a release, open the next planned development
version on main with `scripts/bump-version.sh`. Store the plain base version in
`Cargo.toml`, without a `-dev` suffix: the build appends `-dev.X+<commit>` to
untagged builds, where `X` counts commits since the nearest reachable Hew
release tag. Unrelated tags do not affect that count. A dirty checkout also
appends `.dirty`; a clean build at the matching tag reports the plain version.

After publishing v0.9.3, main uses base `0.9.4` for subsequent development.
Keep edition 2026 unchanged. The
post-release bump is separate from the qualified release commit and its tag. The
[post-release checklist](../release-runbook.md#phase-6--post-release-verification)
owns the concrete steps.

### Pre-release suffix

- `-pre` — an untagged preview version while a release is being assembled.
  Development builds also identify their commit through `hew --version`.
- `-alpha.N` — early preview of a MINOR. Surface is in flux. Tag only when
  external users are asked to exercise a specific capability.
- `-beta.N` — feature-complete but unstable. Use when the surface is frozen
  and we want bug reports before the `.0`.
- `-rc.N` — release candidate. No changes expected before the `.0` except
  fixes for issues found in RC. Tag if a release has external-stakeholder
  validation gates.

We do not require any pre-release stages. Most MINORs ship straight to
`.0`. Use `-rc` when a release is large enough that a 24–72h soak with
external pinning is genuinely useful.

## Pre-1.0 discipline

SemVer says anything goes pre-1.0. We're stricter than that:

1. **Document every breaking change** in `CHANGELOG.md` and the affected
   release's notes, with old shape, new shape and migration. See `docs/release-runbook.md`.
2. **Don't break casually.** A breaking change should land in a coherent
   PR with rationale, not as a drive-by during unrelated work.
3. **Keep the specification and consumers aligned.** Edition 2026 is still
   evolving; a 0.6.x release number does not imply that an intentional surface
   change can omit migration instructions.

## Release cadence

Target: **roughly one MINOR every 4–6 weeks**, slip when scope demands.
Historical cadence supports this — see "History" below.

We are **scope-driven, not date-driven**. The 4–6 week figure is for
planning, not a deadline. Ship when:

- The CHANGELOG narrative is coherent.
- All release PRs are merged.
- The release-gate CI is green.
- The release runbook (`docs/release-runbook.md`) checklist passes.

If a MINOR slips past ~8 weeks, ask why. Usually one of: the scope is
actually two MINORs, a foundational refactor is blocking, or release
hygiene has regressed.

## When v1.0 happens

`v1.0.0` is the point at which **breaking the language surface requires a
multi-year deprecation cycle**, not a CHANGELOG entry. Concrete triggers:

- The language reference (syntax + core semantics) has not had a breaking
  change in two consecutive MINORs.
- The stdlib public surface is judged complete enough that further additions
  are extensions, not gap-filling.
- Tooling (formatter, LSP, package manager, build) is at the quality bar
  documented in the v0.4.0 ship-criteria (see release notes).
- Edition machinery has been exercised at least once (an edition rollover
  has shipped).
- At least one significant external project has built and shipped on Hew.

Until those hold, we stay pre-1.0.

## Edition vs. version

Hew has a language **edition** (e.g. `edition = "2026"` in `hew.toml`) that
is **orthogonal** to the compiler version. Edition 2026 remains under
development; the compatibility rules below apply once an edition stabilizes:

- A **version** (`v0.6.1`) identifies a compiler/runtime/stdlib build.
- An **edition** (`2026`) identifies a stable surface contract that
  Hew source code targets and that the compiler must continue to accept.

Rules of thumb:

- An edition spans many compiler versions. `edition = "2026"` is expected
  to be accepted by every Hew compiler from its ratification onward.
- A compiler version can introduce a new edition (e.g. `edition = "2028"`)
  while still compiling code written for older editions.
- Removing support for an old edition is a **MAJOR** event, not a MINOR.
  We do not do this pre-1.0 either, except by explicit RFC.
- Adding a new edition is a MINOR event, with prominent release notes and
  a migration tool path.

The Rust-side `edition = "2021"` in our `Cargo.toml` files is unrelated to
the Hew language edition; it governs how the compiler's own source compiles.

## Where the version lives

- **Workspace `Cargo.toml`** (`[workspace.package] version = "..."`) — the
  source of truth. All crates inherit via `version.workspace = true`.
- **`docs/syntax-data.json`** — its `version` field is hand-maintained and
  checked against `CARGO_PKG_VERSION` by a `hew-lexer` test. The bump script
  updates it together with lockfiles, npm identities and generated fixtures.
- **`CHANGELOG.md`** — `[Unreleased]` becomes `[X.Y.Z]` at tag time.
- **`docs/releases/vX.Y.Z.md`** — curated narrative release notes
  (when the release warrants them; PATCH releases may skip this).
- **Git tag** `vX.Y.Z` — the trigger for the release workflow.
- **GitHub release** — created from the tag, body sourced from
  `docs/releases/vX.Y.Z.md`.

## Tagging

The release runbook is canonical. Tag its qualified release-branch commit
after the release PR merges into main:

```sh
git merge-base --is-ancestor origin/release/vX.Y.Z origin/main
git tag -s vX.Y.Z origin/release/vX.Y.Z -m "Hew vX.Y.Z"
git push origin vX.Y.Z
```

The tag push triggers `.github/workflows/release.yml`. Pre-tag validation
runs through `.github/workflows/release-gate.yml` on a `release/vX.Y.Z`
branch — see `docs/release-runbook.md`.

After tagging, follow the release runbook when opening the next planned
development version. A patch release does not automatically open a new MINOR.

## Closeout

When you ship a release, ask:

1. Does the CHANGELOG entry tell a coherent story, or is it a list of
   unrelated bullets? If the latter, the next release should be smaller.
2. Did this release introduce a durable invariant worth adding to [`engineering-invariants.md`](engineering-invariants.md)?
3. Does `Cargo.toml` identify the next planned development version?
4. Is the release notes file (`docs/releases/vX.Y.Z.md`) in place if the
   release warranted one?
