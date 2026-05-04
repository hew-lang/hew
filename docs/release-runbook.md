# Hew Release Runbook

Pre-tag validation checklist for Hew releases.
This is the concrete expansion of the `ci-full-run-pre-tag` todo.

## Prerequisites

- [ ] All release-lane PRs merged to `main`
- [ ] `main` CI is green (check [Actions → CI](../../actions/workflows/ci.yml))
- [ ] Nightly sanitizers are clean (check [Actions → Nightly Sanitizers](../../actions/workflows/nightly-sanitizers.yml)) — TSan is advisory, see Known gaps
- [ ] FreeBSD nightly is green or has a known-issue note (check [Actions → FreeBSD CI](../../actions/workflows/freebsd.yml))
- [ ] CHANGELOG.md `[Unreleased]` section is populated
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
   
   - **stdlib vec: `index_of` now returns `int`:** changed from `fn index_of(elem: T) -> Option<int>` 
     to `fn index_of(elem: T) -> int` (returning `-1` if not found). Update call sites to 
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
| macOS arm64    | hew-cli, adze-cli, hew-lsp, hew-lib     | Rust workspace, codegen E2E (native) |
| Windows x86_64 | adze-cli, hew-lsp (hew-cli: check only) | Rust workspace (no codegen)         |

**Wait for all three gate jobs to go green.**

## Phase 4 — Local cross-platform validation (optional but recommended)

For full cross-platform hardware validation beyond CI runners:

```bash
# Linux only (fast, local)
make pre-release PLATFORMS="linux"

# All platforms (requires .env.pre-release with SSH host config)
make pre-release
```

Requires `.env.pre-release` in the repo root (gitignored):

```bash
MACOS_HOST=my-mac.local
FREEBSD_HOST=user@freebsd-host
FREEBSD_PROJECT_DIR=/path/to/hew
WINDOWS_HOST=user@windows-host
WINDOWS_PROJECT_DIR=P:/path/to/hew
```

Windows hosts also need a one-time LLVM/MLIR 22 bootstrap that matches the tag
release workflow: install into `C:\llvm-22` and verify
`C:\llvm-22\lib\cmake\mlir\MLIRConfig.cmake` exists before running
`make pre-release`. See
[`docs/cross-platform-build-guide.md`](cross-platform-build-guide.md#windows)
for the exact bootstrap command sequence. The validator defaults to
`LLVM_PREFIX=C:\llvm-22`, `HEW_EMBED_STATIC=1`, and `CC/CXX=cl`; override with
`HEW_WINDOWS_LLVM_PREFIX`, `HEW_WINDOWS_CC`, and `HEW_WINDOWS_CXX` if that host
uses a different compiler driver.

What `make pre-release` does:
1. `make release` — static-link release build of all binaries
2. `scripts/pre-release-validate.sh` — per-platform:
     - Build all release artifacts
     - Verify binaries exist and run (`--version`)
     - Smoke test: compile and execute a .hew program
     - Linux: verify no dynamic LLVM/MLIR deps (`ldd` check)
     - Remote platforms (macOS/FreeBSD/Windows): rsync + SSH build
     - Windows: require `C:\llvm-22\lib\cmake\mlir\MLIRConfig.cmake`, force
       `LLVM_PREFIX` + `HEW_EMBED_STATIC=1`, then compile+run a smoke program so
       validation cannot silently pass a frontend-only `hew.exe`

For a local macOS clean-room check of the Homebrew/release binary shape:

```bash
HEW_EMBED_STATIC=1 cargo build -p hew-cli --release
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

## Phase 5 — Tag and release

```bash
git tag v0.4.0
git push origin v0.4.0
```

This triggers `.github/workflows/release.yml`, which:
- Builds release tarballs for linux-x86_64, linux-aarch64, darwin-x86_64, darwin-aarch64, windows-x86_64
- Runs `scripts/verify-macos-binary.sh` on macOS artifacts before signing
- Signs and notarizes macOS binaries on tag releases
- Creates a GitHub Release with checksums
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
| Smoke test (compile+run)     | release-gate.yml             | Yes       |
| macOS build + tests          | ci.yml + release-gate.yml    | Yes       |
| Windows build + tests        | ci.yml + release-gate.yml    | Yes       |
| FreeBSD build + tests        | freebsd.yml (nightly)        | Advisory  |
| ASan + UBSan                 | nightly-sanitizers.yml       | Advisory  |
| TSan (Rust runtime)          | nightly-sanitizers.yml       | Advisory (waived — see Known gaps) |
| Codegen silent-failure lint  | codegen-lint.yml (PR)        | Advisory  |
| Local cross-platform build   | `make pre-release`           | Recommended |

## Known gaps (tracked)

- **Windows codegen**: hew-cli is `cargo check` only on Windows CI (no LLVM provisioned).
  The release.yml Windows job builds from source (~90 min). If the Windows build
  breaks at tag time, it uses `continue-on-error: true`.
- **linux-aarch64**: No CI gate before tagging; first exercised by release.yml.
  Mitigation: linux-aarch64 shares the same codegen as linux-x86_64.
- **FreeBSD**: Nightly only. Check the last run before tagging.
- **TSan (Rust runtime)**: `continue-on-error: true` — upstream Rust/Cargo build-std +
  TSan link failures (duplicate lang items, panic-strategy mismatch) have no clean
  repo-side fix as of 2026-04.  Kept for signal; re-evaluate when upstream resolves.
- **WASM capability gaps**: Channels and I/O streams are rejected at compile
  time when targeting wasm32-wasi.  Timers (`sleep_ms`/`sleep`) now have
  cooperative semantics on WASM (actor parks at message boundary) and emit a
  warning rather than an error.  See
  [`docs/wasm-capability-matrix.md`](wasm-capability-matrix.md) for the full
  Tier 1 / Tier 2 disposition table and the WASM-TODO backlog.
