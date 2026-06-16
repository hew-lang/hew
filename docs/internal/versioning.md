# Hew Versioning Policy

Guidance, not strict rules. The goal is that a contributor reading a tag, a
PR, or `Cargo.toml` can answer "what changed, and what do I need to do about
it?" without reading the commit log.

## Scheme

Hew uses **SemVer-ish** versioning: `vMAJOR.MINOR.PATCH`, with optional
pre-release suffix `-alpha.N`, `-beta.N`, `-rc.N`, or `-pre`.

While Hew is pre-1.0, MAJOR stays at `0` and the **MINOR axis carries the
weight of "real release"**. PATCH exists and is welcome, but it has a
narrow purpose (see below).

## What each axis signals

| Bump  | Signals                                                                 | Examples                                                   |
| ----- | ----------------------------------------------------------------------- | ---------------------------------------------------------- |
| MAJOR | Stability commitment. Multi-year deprecation cycle. Not yet reached.    | `v1.0.0` — see "When v1.0" below.                          |
| MINOR | Meaningful surface change. Breaking changes allowed. Migration notes.   | `v0.4 → v0.5` (HIR/MIR cutover, concurrency surface).      |
| PATCH | Bugfix or non-breaking addition on top of an already-shipped MINOR.     | `v0.1.0 → v0.1.9` (the early-language cadence).            |

### MINOR bump (`0.Y`)

Bump MINOR when **any** of the following is true:

- A public language, stdlib, or CLI surface changes shape (signature, return
  type, default behaviour, error contract).
- A user-visible capability lands that warrants release notes more than one
  paragraph long.
- The compiler/runtime crosses a foundational seam (IR refactor, ABI change,
  new codegen pipeline) — even if the user-facing surface is unchanged,
  because tooling and downstream consumers will care.
- The CHANGELOG `[Unreleased]` section has accumulated enough material that
  the next release notes file would be a genuine narrative, not a bullet
  list of three fixes.

A MINOR bump does **not** require breaking changes. "We shipped a coherent
chunk of work" is sufficient reason.

### PATCH bump (`Y.Z`, `Z > 0`)

Bump PATCH when **all** of the following are true:

- The change is strictly additive or corrective relative to the most recent
  MINOR tag (no surface shapes change incompatibly).
- The change is worth giving downstream users a pinnable tag for — typically
  a bugfix, a CI/release-asset fix, a security-relevant patch, or a small
  capability addition users are actively waiting on.
- The change is small enough that a one-paragraph note in
  `docs/releases/vY.Z.md` (or appended to the existing MINOR notes) is
  sufficient.

If a fix is important but a MINOR is imminent (days away), prefer rolling it
into the MINOR. If the MINOR is weeks away, ship a PATCH.

PATCH is **not** reserved for emergencies. Multiple PATCH releases per
MINOR is healthy — `v0.1.x` shipped nine of them. The dearth of PATCH
releases in the `v0.3` and `v0.4` cycles reflects rapid MINOR cadence, not a
policy against PATCHes.

### Pre-release suffix

- `-pre` — the workspace `Cargo.toml` version between tags. Always reflects
  "the next MINOR is being assembled." Example today: `0.5.0-pre`.
  Never tagged; only present in `Cargo.toml` on `main` between releases.
- `-alpha.N` — early preview of a MINOR. Surface is in flux. Tag only when
  external users are asked to exercise a specific capability.
- `-beta.N` — feature-complete but unstable. Use when the surface is frozen
  and we want bug reports before the `.0`.
- `-rc.N` — release candidate. No changes expected before the `.0` except
  fixes for issues found in RC. Tag if a release has external-stakeholder
  validation gates (the only historical use: `v0.2.0-rc2`).

We do not require any pre-release stages. Most MINORs ship straight to
`.0`. Use `-rc` when a release is large enough that a 24–72h soak with
external pinning is genuinely useful.

## Pre-1.0 discipline

SemVer says anything goes pre-1.0. We're stricter than that:

1. **Document every breaking change** in `CHANGELOG.md` under the relevant
   MINOR, with old shape, new shape, and migration. See `docs/release-runbook.md`.
2. **Don't break casually.** A breaking change should land in a coherent
   PR with rationale, not as a drive-by during unrelated work.
3. **PATCH releases never break.** If a PATCH would change a shape, it's a
   MINOR instead.

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
is **orthogonal** to the compiler version:

- A **version** (`v0.5.0`) identifies a compiler/runtime/stdlib build.
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
- **`CHANGELOG.md`** — `[Unreleased]` becomes `[X.Y.Z]` at tag time.
- **`docs/releases/vX.Y.Z.md`** — curated narrative release notes
  (when the release warrants them; PATCH releases may skip this).
- **Git tag** `vX.Y.Z` — the trigger for the release workflow.
- **GitHub release** — created from the tag, body sourced from
  `docs/releases/vX.Y.Z.md`.

## Tagging

The release runbook is canonical. In short:

```sh
# After CHANGELOG + Cargo.toml bump commit is on main and CI is green:
git tag -a vX.Y.Z -m "vX.Y.Z"
git push origin vX.Y.Z
```

The tag push triggers `.github/workflows/release.yml`. Pre-tag validation
runs through `.github/workflows/release-gate.yml` on a `release/vX.Y`
branch — see `docs/release-runbook.md`.

After tagging, bump the workspace version to the next `-pre`:

```toml
# Cargo.toml
version = "X.Y+1.0-pre"
```

Commit that as `chore: open vX.Y+1 development`.

## History (calibration)

Real Hew release history, for grounding:

| Tag        | Date       | Notes                                                          |
| ---------- | ---------- | -------------------------------------------------------------- |
| v0.1.0     | 2026-02    | First public tag.                                              |
| v0.1.1–.9  | 2026-02/03 | Nine PATCHes — early-language bugfix/iteration cadence.        |
| v0.2.0-rc2 | 2026-03-16 | Only RC in history. Pre-tag soak before a substantial MINOR.   |
| v0.2.0     | 2026-03-16 |                                                                |
| v0.2.1–.2  | 2026-03    | Two PATCHes.                                                   |
| v0.3.0     | 2026-04-06 | No PATCHes — cadence shifted to MINOR-only.                    |
| v0.4.0     | 2026-05-06 | No PATCHes — substantial release (LSP formatting, stdlib `int` |
|            |            | unification, explicit teardown for HTTP/regex/JSON).           |
| v0.5.0     | (planned)  | Compiler-foundation cutover; HIR/MIR + concurrency surface.    |

Observation: the PATCH gap from v0.3 onward correlates with faster MINOR
cadence, not a policy. If a `v0.5.1` is warranted (stdlib bugfix, release-
asset issue, regression), tag it.

## Examples

- **Stdlib function gains a new optional parameter:** MINOR. Surface
  changed shape (even backwards-compatibly).
- **Bugfix where `std/encoding/json` mishandles surrogate pairs:** PATCH.
- **LSP server gains a new request handler:** MINOR. New capability.
- **CI release-asset workflow fix that doesn't change binaries:** Not a
  release at all; the bug is in CI, fix it in CI.
- **Codegen miscompile that produces wrong output for a specific shape:**
  PATCH if a MINOR isn't imminent; otherwise roll into the MINOR.
- **Compiler internal refactor with zero user-facing change:** MINOR if it
  crosses a foundational seam (v0.5's IR ladder qualifies). Otherwise no
  release — it lands on `main` and ships with the next MINOR.
- **Removing a deprecated alias (`fs.read_line` → `io.read_line()`):**
  MINOR. Breaking change, documented in release notes with migration.

## Closeout

When you ship a release, ask:

1. Does the CHANGELOG entry tell a coherent story, or is it a list of
   unrelated bullets? If the latter, the next release should be smaller.
2. Did this release introduce a new invariant worth adding to `LESSONS.md`?
3. Is `Cargo.toml` now on the next `-pre`?
4. Is the release notes file (`docs/releases/vX.Y.Z.md`) in place if the
   release warranted one?
