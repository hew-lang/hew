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

## Phase 2 — Version bump

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
git commit -m "chore: bump version to v0.3.0"
```

## Phase 3 — Push release branch (triggers release-gate CI)

```bash
git checkout -b release/v0.3
git push origin release/v0.3
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

What `make pre-release` does:
1. `make release` — static-link release build of all binaries
2. `scripts/pre-release-validate.sh` — per-platform:
   - Build all release artifacts
   - Verify binaries exist and run (`--version`)
   - Smoke test: compile and execute a .hew program
   - Linux: verify no dynamic LLVM/MLIR deps (`ldd` check)
   - Remote platforms (macOS/FreeBSD/Windows): rsync + SSH build

## Phase 5 — Tag and release

```bash
git tag v0.3.0
git push origin v0.3.0
```

This triggers `.github/workflows/release.yml`, which:
- Builds release tarballs for linux-x86_64, linux-aarch64, darwin-aarch64, windows-x86_64
- Signs and notarizes macOS binaries (if Apple secrets are configured)
- Creates a GitHub Release with checksums
- Updates the Homebrew tap (if HOMEBREW_TAP_TOKEN is configured)
- Publishes the VS Code extension (if VSCE_PAT is configured)

## Phase 6 — Post-release verification

- [ ] GitHub Release page has all platform tarballs
- [ ] Download and smoke-test at least one tarball
- [ ] Homebrew formula updated (if applicable): `brew install hew-lang/hew/hew`
- [ ] VS Code extension published (if applicable)

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
