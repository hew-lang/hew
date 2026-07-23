# Hew Release Runbook

Pre-tag validation checklist for Hew releases.
This is the concrete expansion of the `ci-full-run-pre-tag` todo.

## Prerequisites

- [ ] All release PRs merged to `main`
- [ ] `main` CI is green (check [Actions → CI](../../actions/workflows/ci.yml))
- [ ] The release branch `gate-sanitizers` job is green: ASan passed on the release commit, and TSan/Miri are either green in their recurring lanes or covered by commit-pinned waivers in `release-sanitizer-waiver.toml` (see Known gaps)
- [ ] FreeBSD nightly is green or has a known-issue note (check [Actions → FreeBSD CI](../../actions/workflows/freebsd.yml))
- [ ] CHANGELOG.md `[Unreleased]` section is populated
- [ ] Curated GitHub release notes are drafted at `docs/releases/vX.Y.Z.md`
- [ ] Version in workspace `Cargo.toml` is still the *previous* release (bump happens below)

## Phase 1 — Assemble the candidate

```bash
# Ensure you're on a clean, up-to-date main
git checkout main && git pull --ff-only

# Verify the candidate commit
git log --oneline -5  # confirm expected HEAD
```

## Breaking change discipline (pre-1.0)

**Recognizing breaking public-API changes:**

- Adding a variant to a `pub enum` without `#[non_exhaustive]` breaks exhaustive-match callers.
- Changing the signature of a `pub fn` (parameter count, order, type, or return type).
- Removing or renaming an exported `pub` item.
- Narrowing the visibility of a previously public item.

**Handling breaks (pre-1.0):**

1. **In the PR:** add `#[non_exhaustive]` to the enum before adding variants, or mark the whole surface `#[deprecated]` if a complete replacement is preferred. If adding enum variants or changing signatures is unavoidable without `#[non_exhaustive]`, document the migration in a comment.
2. **In-tree callers:** update all tests, examples, probes, and docs in the same PR — never carry forward compatibility aliases. The break is immediate and clean.
3. **CHANGELOG entry:** in the `[Unreleased]` section, add a `### Changed` entry listing the affected module paths, the old shape, and the new shape with a one-line migration note. Example:

   ```
   ### Changed
   
   - **stdlib vec: `index_of` now returns `i64`:** changed from `fn index_of(elem: T) -> Option<i64>` 
     to `fn index_of(elem: T) -> i64` (returning `-1` if not found). Update call sites to 
     check `result < 0` instead of matching `Option`.
   ```

4. **Version bump:** breaking changes trigger a **minor version bump** (e.g., 0.3.0 → 0.4.0).

**Rationale:** pre-1.0, breaking changes allow rapid stdlib refinement without long deprecation cycles. All in-tree code must be updated in the same PR so the break is visible at a glance. `#[non_exhaustive]` protects downstream code from silent miscompilation.

## Phase 2 — Version bump

> **Prerequisite:** The version bump must be a single commit that updates
> `Cargo.toml`'s workspace version AND the matching `[Unreleased]` →
> `[X.Y.Z]` rename in `CHANGELOG.md`. Tagging a commit where `Cargo.toml`
> still reports the prior version produces binaries that self-report the
> wrong version.

```bash
# Bump workspace version in Cargo.toml
# (currently: edit `version = "X.Y.Z"` in the root [workspace.package])
$EDITOR Cargo.toml

# Stamp CHANGELOG.md — move [Unreleased] contents under new version header
$EDITOR CHANGELOG.md

# Update lockfile
cargo check --workspace

# Commit the version bump
# Note: all crates use version.workspace = true, so only root Cargo.toml needs editing.
git add Cargo.toml Cargo.lock CHANGELOG.md
git commit -m "chore: bump version to v0.4.0"
```

## Phase 3 — Push release branch (triggers release-gate CI)

```bash
git checkout -b release/v0.4
git push origin release/v0.4
```

This triggers `.github/workflows/release-gate.yml`, which runs:

| Platform       | Build scope                              | Test scope                          |
|----------------|------------------------------------------|-------------------------------------|
| Linux x86_64   | hew-cli, adze-cli, hew-lsp, hew-lib, WASM runtime | Rust workspace, codegen E2E (native + WASM) |
| Linux aarch64  | hew-cli, adze-cli, hew-lsp, hew-lib, WASM runtime | Rust workspace, codegen E2E (native + WASM) |
| macOS arm64    | hew-cli, adze-cli, hew-lsp, hew-lib     | Rust workspace, codegen E2E (native) |
| Windows x86_64 | adze-cli, hew-lsp (hew-cli: check only) | Rust workspace (no codegen)         |

**Wait for all release gate jobs to go green, including `gate-sanitizers`.**
The sanitizer job runs ASan on the release branch commit and rejects missing,
ambiguous, expired, or non-commit-scoped TSan/Miri waivers.

## Phase 4 — Local cross-platform validation (optional but recommended)

For full cross-platform hardware validation beyond CI runners:

```bash
# Linux only (fast, local)
make pre-release PLATFORMS="linux"

# Linux x86_64 + optional Linux aarch64 remote validation
make pre-release PLATFORMS="linux linux-aarch64"

# All platforms (requires .env.pre-release with SSH host config)
make pre-release
```

If your only local arm64 hardware is Debian bookworm (for example pirea51),
do not treat LLVM 22 apt failures there as a repo regression:
`apt.llvm.org/bookworm` arm64 may not publish the LLVM 22 development packages
that `llvm-sys` needs (`llvm-22-dev`, `clang-22`, or `lld-22`). The
authoritative local/CI-compatible path is Ubuntu 24.04 arm64
(`ubuntu-24.04-arm` in CI, or an Ubuntu 24.04 arm VM/container locally).

Requires `.env.pre-release` in the repo root (gitignored):

```bash
MACOS_HOST=my-mac.local
LINUX_AARCH64_HOST=user@ubuntu-24-arm-host
LINUX_AARCH64_PROJECT_DIR=/path/to/hew
FREEBSD_HOST=user@freebsd-host
FREEBSD_PROJECT_DIR=/path/to/hew
WINDOWS_HOST=user@windows-host
WINDOWS_PROJECT_DIR=P:/path/to/hew
```

Windows hosts also need a one-time LLVM 22 install that matches the tag
release workflow: install into `C:\llvm-22` and verify
`C:\llvm-22\bin\clang.exe` exists before running `make pre-release`. See
[`docs/cross-platform-build-guide.md`](cross-platform-build-guide.md#windows)
for the exact bootstrap command sequence. The validator defaults to
`LLVM_PREFIX=C:\llvm-22` and prepends `C:\llvm-22\bin` to `PATH`; override
with `HEW_WINDOWS_LLVM_PREFIX`, `HEW_WINDOWS_CC`, and `HEW_WINDOWS_CXX` if
that host uses a different compiler driver.

What `make pre-release` does:
1. `make release` — release build of all binaries
2. `scripts/pre-release-validate.sh` — per-platform:
     - Build all release artifacts
     - Verify binaries exist and run (`--version`)
     - Smoke test: compile and execute a .hew program
     - Linux: verify no dynamic LLVM deps (`ldd` check)
      - Linux aarch64 (optional): rsync + SSH build on Ubuntu 24.04 arm64, with
        LLVM 22 provisioned from `apt.llvm.org/noble`
     - Remote platforms (macOS/FreeBSD/Windows): rsync + SSH build
     - Windows: require `LLVM_PREFIX`, then compile+run a smoke program so
       validation cannot silently pass a frontend-only `hew.exe`

For a local macOS clean-room check of the Homebrew/release binary shape:

```bash
cargo build -p hew-cli --release
scripts/verify-macos-binary.sh target/release/hew
cat > hew-smoke.hew <<'EOF'
fn main() { println("Hello from Hew!"); }
EOF
./target/release/hew version
./target/release/hew check hew-smoke.hew
rm -f hew-smoke.hew
```

Expected `otool -L` output is limited to system paths under `/usr/lib/` and
`/System/Library/`. Any `/opt/homebrew/` or `/usr/local/opt/` entry is a
release blocker.

## Phase 5 — Candidate tag and publication order

The publication sequence is a fail-closed dependency graph. Do not advance
past a failed or missing result; items grouped in braces may run independently,
but every arm must succeed before the graph rejoins:

1. Confirm every release bar and the final-candidate checklist are green on
   the exact candidate commit, including sanitizer evidence, required secrets,
   and branch protection.
2. Create the signed tag only after the preceding evidence is recorded.
3. Let the release workflow build and publish the signed platform assets and
   checksums. Its curated body must be the exact
   `docs/releases/<tag>.md` file for that tag.
4. After the assets exist, complete both independent publication arms:
   - Manually dispatch `.github/workflows/publish-npm-packages.yml` for
     `@hew-lang/{wasm,sandbox-wasm,sandbox-vm}@0.6.0-rc1` through its actual
     workflow, and wait for each result. A tag does not publish these packages.
   - Wait for the release workflow's automated playground dispatch, then
     verify the published image, API, and `hew run` smoke path against the
     candidate version.
5. Only after both arms are green, pin the candidate and cut over the banner in
   `hew.sh` and `hew.run`.
6. Rebuild Android from the tagged candidate and verify its artifact.

Homebrew intentionally skips prerelease tags; its optional tap update is
separate from the required playground dispatch. Do not run obsolete downstream
vendoring commands for npm consumers until their vendoring assumptions are
repaired or the commands are removed.

Do not tag until `.github/workflows/release-gate.yml` is green on the release
branch. In particular, `gate-sanitizers` must have validated ASan on the exact
release commit and accepted any TSan/Miri waivers for that same commit.

```bash
git tag v0.4.0
git push origin v0.4.0
```

This triggers `.github/workflows/release.yml`, which:
- Builds release tarballs for linux-x86_64, linux-aarch64, darwin-x86_64, darwin-aarch64, windows-x86_64
- Extracts staged release archives and runs `hew run` from the packaged layout on Unix targets, with `HEW_STD` pointed at the extracted `std/`
- Runs `scripts/verify-macos-binary.sh` on macOS artifacts before signing
- Runs package-layout smoke inside the FreeBSD VM after the tarball is assembled
- Runs Ubuntu clean-room tarball smoke for linux-x86_64 and linux-aarch64
- Builds Linux distro packages and smoke-tests the installable `.deb` / `.rpm` / `.pkg.tar.zst` outputs in Docker (Arch remains x86_64-only)
- Signs and notarizes macOS binaries on tag releases
- Creates a GitHub Release with checksums and the curated notes from
  `docs/releases/<tag>.md`
- Updates the Homebrew tap (if HOMEBREW_TAP_TOKEN is configured)
- Publishes the VS Code extension (if VSCE_PAT is configured)

macOS release notes:

- arm64 release builds run on `macos-15`; Intel release builds stay on `macos-13`
- `MACOSX_DEPLOYMENT_TARGET=13.0` is exported in the release workflow so the
  shipped binaries remain compatible with macOS 13+
- Tag releases require all of:
  - `APPLE_CERTIFICATE_P12`
  - `APPLE_CERTIFICATE_PASSWORD`
  - `APPLE_API_KEY_P8`
  - `APPLE_API_KEY_ID`
  - `APPLE_API_ISSUER_ID`
- If any required Apple secret is missing on a tag release, the macOS job must fail

## Phase 6 — Docs publish (after release tag)

- [ ] Confirm `secrets.CLOUDFLARE_API_TOKEN` is set in repository settings
      (one-time setup — then remove the `if: false` guard in
      `.github/workflows/deploy-docs.yml` and this checkbox).
- [ ] On tag push: the `Deploy docs` workflow fires automatically. Verify it
      succeeded in [Actions → Deploy docs](../../actions/workflows/deploy-docs.yml).
- [ ] If the workflow is disabled or fails, run locally:
      ```bash
      make publish-docs
      wrangler pages deploy target/doc/ --project-name hew-docs
      ```
- [ ] Spot-check `hew-docs.pages.dev` shows the new release's stdlib content
      (verify module count matches `hew doc` output: currently 55 modules).

## Phase 7 — Post-release verification

- [ ] GitHub Release page has all platform tarballs
- [ ] Download and smoke-test at least one tarball
- [ ] Homebrew formula updated (if applicable): `brew install hew-lang/hew/hew`
- [ ] VS Code extension published (if applicable)
- [ ] Author blog post at `hew-lang/hew.sh/src/content/blog/<YYYY>/<MM>/release-v<XYZ>.md` — required for any release with breaking changes; recommended for all minor releases.
- [ ] Verify `release.yml` downstream jobs completed:
  - Homebrew formula update (`hew-lang/homebrew-hew`)
  - Playground compiler bump (`hew-lang/playground`)
  - VS Code extension version sync (`hew-lang/vscode-hew`)
- [ ] If any downstream job failed (e.g. missing secret), re-trigger manually after fixing.
- [ ] Verify the live `hew --version` on a freshly-installed binary matches the tagged version.

## Downstream grammar sync

Any PR that modifies `docs/syntax-data.json` must also run the downstream sync
before the PR merges, or immediately after on a follow-up branch:

```bash
scripts/sync-downstream.sh --check   # dry-run: confirm drift matches expectations
scripts/sync-downstream.sh --commit  # apply and commit in each sibling repo
```

Verify the resulting commits in `tree-sitter-hew`, `vscode-hew`, `vim-hew`,
`hew.sh`, and `hew.run` are merged before tagging the release.

If the sibling-repo commits cannot land synchronously with the hew PR, open
PRs in those repos as a follow-up immediately. Unsynced downstream grammars
cause keyword-highlighting gaps that are invisible from this repo's CI.

## Coverage matrix summary

| Check                        | Where it runs                | Blocking? |
|------------------------------|------------------------------|-----------|
| Clippy + rustfmt             | ci.yml (every PR)            | Yes       |
| Rust workspace tests         | ci.yml + release-gate.yml    | Yes       |
| Codegen E2E (native)         | ci.yml + release-gate.yml    | Yes       |
| Codegen E2E (WASM)           | ci.yml + release-gate.yml    | Yes       |
| Native↔sandbox-VM parity     | ci.yml (Linux, `make sandbox-parity`) | Yes for PRs |
| Smoke test (compile+run)     | release-gate.yml             | Yes       |
| Packaged archive smoke (Linux/macOS) | release.yml (Unix matrix) | Yes    |
| Packaged archive smoke (Windows zip) | release.yml (Windows job) | Best-effort |
| FreeBSD packaged archive smoke | release.yml (FreeBSD VM)   | Advisory  |
| Linux package install smoke  | release.yml (`linux-packages`) | Yes    |
| Linux Docker clean-room tarball smoke | release.yml (`docker-clean-room-test`) | Yes |
| macOS build + tests          | ci.yml + release-gate.yml    | Yes       |
| Windows build + tests        | ci.yml + release-gate.yml    | Yes       |
| FreeBSD build + tests        | freebsd.yml (nightly)        | Advisory  |
| ASan                         | release-gate.yml (`gate-sanitizers`) + nightly-sanitizers.yml | Yes for release branches |
| TSan (Rust runtime)          | nightly-sanitizers.yml + `release-sanitizer-waiver.toml` | Waiver-carried for releases |
| Miri                         | `release-sanitizer-waiver.toml` | Waiver-carried for releases |
| Codegen silent-failure lint  | codegen-lint.yml (PR)        | Advisory  |
| Local cross-platform build   | `make pre-release`           | Recommended |

## Known gaps (tracked)

### Sanitizer trust contract

The release branch gate is the release-time authority for sanitizer evidence:

- **ASan is a hard pre-tag gate on the exact release commit.** The
  `gate-sanitizers` job in `.github/workflows/release-gate.yml` runs
  `make asan`, records the result, and then invokes
  `scripts/check-sanitizer-gate.sh "${GITHUB_SHA}" ...`. The validator fails
  closed when the ASan result is absent, red, skipped, or ambiguous.
- **TSan is waiver-carried for releases while the upstream nightly
  `build-std`/TSan link failure remains unresolved.** The nightly sanitizer
  job still provides signal, but a release commit needs an explicit
  `axis = "tsan"` waiver row in `release-sanitizer-waiver.toml` unless
  future work promotes TSan to a recurring green release input.
- **Miri is not yet a recurring gate.** It is reserved in the same waiver
  contract because Miri has caught real Stacked-Borrows issues, but standing up
  the recurring subset and skip policy is tracked separately.
- **ASan coverage is only as broad as `make asan`.** Today that command runs
  the `hew-runtime --lib` ASan suite. It does not prove integration-only free
  sites, thread-reachable handle leaks, or every packaged binary path are
  covered. Expanding ASan to integration binaries is a tracked follow-on; once
  `make asan` grows, the release gate inherits that coverage automatically.

To add a waiver, edit `release-sanitizer-waiver.toml` on the release branch and
add one `[[waiver]]` row per non-green axis:

```toml
[[waiver]]
axis = "tsan"
reason = "upstream nightly build-std + TSan link failure"
tracking = "https://github.com/hew-lang/hew/issues/<issue>"
commit = "<40-hex release commit SHA>"
expires = "YYYY-MM-DD"
```

The `commit` must match the release branch HEAD exactly. Keep `expires` short
enough to force re-evaluation on the next release candidate, and remove expired
rows instead of extending them without new evidence. Blanket waivers
(`axis = "*"`, `axis = "all"`), missing fields, duplicate rows, and expired
rows fail the gate.

- **Windows codegen**: hew-cli is `cargo check` only on Windows CI (no LLVM provisioned).
  The release.yml Windows job builds from source (~90 min). If the Windows build
  breaks at tag time, it uses `continue-on-error: true`. The packaged zip smoke
  is best-effort for the same reason.
- **Release-gate sandbox parity**: native↔sandbox-VM parity is enforced per PR
  on Linux by `ci.yml` with `make sandbox-parity`. The release gate does not
  rerun that Node/npm-backed harness across its platform matrix; rerun
  `make sandbox-parity` locally before tagging when cutting a release candidate.
- **linux-aarch64**: No pre-tag CI gate before tagging; the tag workflow now
  runs both packaged-archive smoke and an Ubuntu clean-room smoke on arm64.
- **FreeBSD**: Nightly plus tag-time packaged-archive smoke, but the tag job is
  still `continue-on-error: true`. Check the last nightly before tagging.
- **Local Debian bookworm arm64 hosts**: `apt.llvm.org/bookworm` arm64 does not
  publish the LLVM 22 packages the release build uses. Validate linux-aarch64
  on Ubuntu 24.04 arm64 instead (CI `ubuntu-24.04-arm`, or an Ubuntu 24.04
  arm VM/container / remote host).
- **TSan (Rust runtime)**: upstream Rust/Cargo build-std + TSan link failures
  (duplicate lang items, panic-strategy mismatch) have no clean repo-side fix
  as of 2026-04. Keep the nightly signal and carry release exceptions only via
  `release-sanitizer-waiver.toml`; re-evaluate when upstream resolves.
- **WASM capability gaps**: Channels and I/O streams are rejected at compile
  time when targeting wasm32-wasi.  Timers (`sleep`/`sleep_until`) now have
  cooperative semantics on WASM (actor parks at message boundary) and emit a
  warning rather than an error.  See
  [`docs/wasm-capability-matrix.md`](wasm-capability-matrix.md) for the full
  Tier 1 / Tier 2 disposition table and the WASM-TODO backlog.
