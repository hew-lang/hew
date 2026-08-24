# Hew Release Runbook

Pre-tag validation checklist for Hew releases.
This is the concrete expansion of the `ci-full-run-pre-tag` todo.

## Prerequisites

- [ ] All release PRs merged to `main`
- [ ] `main` CI is green (check [Actions → CI](../../actions/workflows/ci.yml))
- [ ] The release branch `gate-sanitizers` job is green: ASan executed and passed, and TSan/Miri have bounded behavioral ledger entries for this release version in `release-sanitizer-waiver.toml` (see Known gaps)
- [ ] FreeBSD nightly is green or has a known-issue note (check [Actions → FreeBSD CI](../../actions/workflows/freebsd.yml))
- [ ] CHANGELOG.md has either a populated `[Unreleased]` section or the dated
      `[X.Y.Z]` section for the intended release
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
| Linux x86_64   | hew-cli, hew-lsp, hew-lib, WASM runtime | Rust workspace, codegen E2E (native + WASM) |
| Linux aarch64  | hew-cli, hew-lsp, hew-lib, WASM runtime | Rust workspace, codegen E2E (native + WASM) |
| macOS arm64    | hew-cli, hew-lsp, hew-lib     | Rust workspace, codegen E2E (native) |
| Windows x86_64 | hew-cli, hew-lsp, hew-observe, hew-lib | Rust workspace + C-ABI + executable release-library consumer |

**Wait for all release gate jobs to go green, including `gate-sanitizers`.**
The sanitizer job executes ASan and rejects missing, ambiguous, expired, vague,
or wrong-release TSan/Miri behavioral evidence.

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
# Optional when LLVM 22 is outside the standard Homebrew locations:
HEW_MACOS_LLVM_PREFIX=/opt/homebrew/opt/llvm@22
LINUX_AARCH64_HOST=user@ubuntu-24-arm-host
FREEBSD_HOST=user@freebsd-host
WINDOWS_HOST=user@windows-host
# Required for Windows validation; build it with `make windows-release-candidate`
# (archives the committed HEAD tree — Cargo.toml/Cargo.lock/crate sources —
# since scripts/windows-release-build.ps1 runs `cargo build` on the Windows
# host itself; it does not consume precompiled artifacts).
HEW_WINDOWS_CANDIDATE_ARCHIVE=/absolute/path/to/target/hew-windows-candidate.tar.gz
```

Build the candidate archive from the repo root before running Windows
validation:

```bash
make windows-release-candidate
export HEW_WINDOWS_CANDIDATE_ARCHIVE="$(pwd)/target/hew-windows-candidate.tar.gz"
```

The Windows host must also have a populated Cargo cache for the workspace's
locked dependencies: `windows-release-build.ps1` runs
`cargo fetch --locked --offline` before building and fails validation outright
if that cache is missing, so run `cargo fetch --locked` (online) on the
Windows host at least once beforehand.

Windows hosts also need Visual Studio C++ Build Tools with a Windows SDK and a
one-time LLVM 22 install that matches the tag release workflow: install into
`C:\llvm-22`, verify `vswhere.exe` can find the x64 C++ tools, and verify
`C:\llvm-22\bin\clang.exe` exists before running `make pre-release`. See
[`docs/cross-platform-build-guide.md`](cross-platform-build-guide.md#windows)
for the exact bootstrap command sequence. The validator defaults to
`LLVM_PREFIX=C:\llvm-22`; it imports `VsDevCmd.bat` for the SDK/CRT environment
before prepending `C:\llvm-22\bin` to `PATH`. Override with
`HEW_WINDOWS_LLVM_PREFIX`, `HEW_WINDOWS_CC`, and `HEW_WINDOWS_CXX` if that host
uses a different compiler driver. Windows validation consumes the configured
candidate archive without rebuilding it locally. It also runs Cargo offline, so
the Windows host must have a populated Cargo cache for the locked dependencies.

The macOS validator requires an LLVM 22 root. It first honors
`HEW_MACOS_LLVM_PREFIX` (or `MACOS_LLVM_PREFIX` in `.env.pre-release`), then
uses `brew --prefix llvm@22` when `brew` is available, and finally probes
`/opt/homebrew/opt/llvm@22` and `/usr/local/opt/llvm@22` directly. Each
candidate is accepted only when its `bin/llvm-config --version` reports major
version 22; an absent or different version fails validation before building.

What `make pre-release` does:
1. `make release` — release build of all binaries
2. `scripts/pre-release-validate.sh` — per-platform:
     - Build all release artifacts
     - Verify binaries exist and run (`--version`)
     - Smoke test: compile and execute a .hew program
     - Linux: verify no dynamic LLVM deps (`ldd` check)
     - Linux aarch64 (optional): stage the local candidate in a fresh remote
       temporary directory, then build on Ubuntu 24.04 arm64 with LLVM 22
       provisioned from `apt.llvm.org/noble`
     - macOS and FreeBSD: stage the local working-tree candidate in a fresh
       remote temporary directory. Windows stages the configured candidate
       archive in its own fresh remote directory. Existing host checkouts are
       never reset, updated, or used as a fallback.
     - Every requested remote platform fails closed when its host is absent or
       unreachable. Narrow `PLATFORMS` explicitly when omitting a host.
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
2. Before creating the signed tag, publish the candidate playground image from
   the exact reviewed playground commit that introduced the candidate contract:

   ```bash
   PLAYGROUND_CONTRACT_REF=21be84bb97436436b640f2acd09fb6dd2e0fbf94
   PLAYGROUND_REF=<exact-reviewed-40-character-playground-sha>
   HEW_RELEASE_SHA=<exact-40-character-hew-sha>
   VERSION=<version-without-v-prefix>
   PLAYGROUND_CHECKOUT="$(mktemp -d)"
   git clone https://github.com/hew-lang/playground.git "${PLAYGROUND_CHECKOUT}"
   git -C "${PLAYGROUND_CHECKOUT}" checkout --detach "${PLAYGROUND_REF}"
   test "$(git -C "${PLAYGROUND_CHECKOUT}" rev-parse HEAD)" = "${PLAYGROUND_REF}"
   git -C "${PLAYGROUND_CHECKOUT}" merge-base --is-ancestor \
     "${PLAYGROUND_CONTRACT_REF}" HEAD
   test -z "$(git -C "${PLAYGROUND_CHECKOUT}" status --porcelain)"
   (
     cd "${PLAYGROUND_CHECKOUT}"
     . ./toolchains.env
     test "${HEW_DEFAULT_VERSION}" = "${VERSION}"
     test "${HEW_CANDIDATE_SHA}" = "${HEW_RELEASE_SHA}"
     env -u MAKEFLAGS -u MFLAGS -u MAKEOVERRIDES -u MAKEFILES -u GNUMAKEFLAGS \
       HEW_EXAMPLES_REF="${HEW_RELEASE_SHA}" \
       HEW_VERSION="${VERSION}" \
       PLAYGROUND_PLATFORM=linux/amd64 \
       PLAYGROUND_RELEASE_IMAGE=ghcr.io/hew-lang/playground \
       scripts/publish-release-image.sh candidate
   )
   PLAYGROUND_IMAGE=ghcr.io/hew-lang/playground:v${VERSION}
   PLAYGROUND_IMAGE_DIGEST="$(docker buildx imagetools inspect \
     "${PLAYGROUND_IMAGE}" --format '{{json .Manifest}}' | jq -er '.digest')"
   if ! [[ "${PLAYGROUND_IMAGE_DIGEST}" =~ ^sha256:[0-9a-f]{64}$ ]]; then
     echo "invalid playground image digest" >&2
     exit 1
   fi
   gh variable set PLAYGROUND_RELEASE_IMAGE_LOCK \
     --body "v${VERSION}@${PLAYGROUND_IMAGE_DIGEST}" --repo hew-lang/hew
   ```

   After the Hew candidate SHA is fixed, first merge a separately reviewed,
   minimal playground `toolchains.env` bump that sets `HEW_DEFAULT_VERSION` to
   the candidate version and `HEW_CANDIDATE_SHA` to that exact Hew commit.
   `PLAYGROUND_REF` is the resulting exact clean playground commit; the
   `merge-base` check proves it contains the candidate publisher merged in #34.
   `HEW_EXAMPLES_REF` is the exact candidate commit. The playground publisher
   accepts the untagged repository path and itself publishes the exact
   `ghcr.io/hew-lang/playground:v${VERSION}` tag. It stages the authorized Hew
   checkout, scrubs inherited GNU Make parser controls, uses candidate authority,
   and stamps the SHA as `org.opencontainers.image.revision`. Record the clean
   playground SHA, candidate SHA, platform, image digest, and smoke result before
   continuing. The version-scoped `PLAYGROUND_RELEASE_IMAGE_LOCK` is the
   pre-tag handoff: it records the immutable raw manifest/index digest without
   requiring another Hew or playground commit. Do not use a Make target or the
   `publish` mode before tagging: publish authority requires the remote signed tag.
3. Create the signed tag only after the preceding evidence is recorded.
4. Let the release workflow build and publish the signed platform assets and
   checksums. Its curated body must be the exact
   `docs/releases/<tag>.md` file for that tag.
5. After the assets exist, complete the npm publication arm:
   - Manually dispatch `.github/workflows/publish-npm-packages.yml` with
     `release_tag=v0.6.0-rc1` for
     `@hew-lang/{wasm,sandbox-wasm,sandbox-vm}@0.6.0-rc1`, and wait for each
     result. The workflow checks out that immutable tag and rejects a workspace
     or sandbox package version mismatch. A tag does not publish these packages.
6. The tag-push release workflow only observes the pre-tag candidate image; it
   never dispatches mutable downstream state. The tag must resolve to the exact
   digest recorded in `PLAYGROUND_RELEASE_IMAGE_LOCK`, expose exactly the
   `linux/amd64` release platform, and its
   `org.opencontainers.image.revision` label must bind the release commit. The
   workflow validates the registry's digest header and the raw manifest/index
   bytes before inspecting the platform and revision. Then verify the published
   image, API, and `hew run` smoke path against the candidate version. Running
   `scripts/assert-playground-release-image.sh` outside Actions requires
   `GHCR_USERNAME` and `GHCR_TOKEN`; the token must be a classic GitHub PAT with
   the `read:packages` scope (and organization SSO authorization when the
   organization requires it).
   Any intentional post-tag rebuild must use
   `scripts/publish-release-image.sh publish` manually from that same exact clean
   playground checkout. Reconfirm the new digest and update the version-scoped
   lock before rerunning the assertion; never dispatch a mutable remote branch.
7. Only after both independent publication arms are green, pin the candidate and cut over the banner in
   `hew.sh` and `hew.run`.
8. Rebuild Android from the tagged candidate and verify its artifact.

Homebrew intentionally skips prerelease tags; its optional tap update is
separate from the required playground release image. Do not run obsolete downstream
vendoring commands for npm consumers until their vendoring assumptions are
repaired or the commands are removed.

Do not tag until `.github/workflows/release-gate.yml` is green on the release
branch. In particular, `gate-sanitizers` must have executed ASan successfully
and accepted the bounded TSan/Miri behavioral ledger for the release version.

```bash
git tag -s v0.6.0-rc1 -m "Hew v0.6.0-rc1"
git push origin v0.6.0-rc1
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
- [ ] Confirm the token can edit Pages projects in the target account.
      The production project is
      `hew-docs`, and its custom domain is `docs.hew.sh`.
- [ ] On tag push: the `Deploy docs` workflow fires automatically. Verify it
      succeeded in [Actions → Deploy docs](../../actions/workflows/deploy-docs.yml).
- [ ] If the workflow is disabled or fails, run locally:
      ```bash
      make publish-docs
      CLOUDFLARE_ACCOUNT_ID="$CLOUDFLARE_ACCOUNT_ID" \
        wrangler pages deploy target/doc/ --project-name hew-docs
      ```
- [ ] Spot-check `docs.hew.sh` shows the new release's stdlib content and verify
      its module count matches the `hew doc` output.

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
| Release-library consumer link+run | release-gate.yml + release.yml (every platform/architecture lane) | Yes |
| Packaged archive smoke (Linux/macOS) | release.yml (Unix matrix) | Yes    |
| Packaged archive smoke (Windows zip) | release.yml (Windows job) | Yes |
| FreeBSD packaged archive smoke | release.yml (FreeBSD VM, x86_64 + aarch64) | Yes |
| Linux package install smoke  | release.yml (`linux-packages`) | Yes    |
| Linux Docker clean-room tarball smoke | release.yml (`docker-clean-room-test`) | Yes |
| macOS build + tests          | ci.yml + release-gate.yml    | Yes       |
| Windows build + tests        | ci.yml + release-gate.yml    | Yes       |
| FreeBSD build + tests        | release-gate.yml (x86_64 + aarch64), freebsd.yml (nightly) | Yes for release branches |
| ASan                         | release-gate.yml (`gate-sanitizers`) + nightly-sanitizers.yml | Yes for release branches |
| TSan (Rust runtime)          | nightly-sanitizers.yml + `release-sanitizer-waiver.toml` | Bounded behavioral ledger for releases |
| Miri                         | `release-sanitizer-waiver.toml` | Bounded behavioral ledger for releases |
| Codegen silent-failure lint  | codegen-lint.yml (PR)        | Advisory  |
| Local cross-platform build   | `make pre-release`           | Recommended |

## Known gaps (tracked)

### Sanitizer trust contract

The release branch gate is the release-time authority for sanitizer evidence:

- **ASan is an executed hard pre-tag gate and cannot be waived.** The
  `gate-sanitizers` job in `.github/workflows/release-gate.yml` runs
  `make asan`, records the result, and invokes
  `scripts/check-sanitizer-gate.sh "${RELEASE_VERSION}" ...`. The validator
  fails closed when ASan is absent, red, skipped, or ambiguous.
- **TSan has a bounded release-version behavioral ledger while the upstream
  `build-std`/TSan link failure remains unresolved.** The nightly lane still
  provides signal, but a release needs an explicit `axis = "tsan"` row that
  records observed behavior, rationale, owner, tracking issue, and expiry.
- **Miri is not yet a recurring gate.** It uses the same bounded behavioral
  ledger until a representative recurring FFI subset exists.
- **ASan coverage is only as broad as `make asan`.** Today that command runs
  the `hew-runtime --lib` ASan suite. It does not prove integration-only free
  sites, thread-reachable handle leaks, or every packaged binary path are
  covered. Expanding ASan to integration binaries is a tracked follow-on; once
  `make asan` grows, the release gate inherits that coverage automatically.

To record a limitation, edit `release-sanitizer-waiver.toml` and add exactly
one `[[waiver]]` row per non-executed axis:

```toml
[[waiver]]
axis = "tsan"
release = "0.6.0-rc1"
behavior = "the concrete observed lane or toolchain behavior"
reason = "why the behavior is not an authoritative release failure"
tracking = "https://github.com/hew-lang/hew/issues/<issue>"
owner = "Hew runtime maintainers"
expires = "YYYY-MM-DD"
```

The `release` must match the version being validated. Keep `expires` short
enough to force re-evaluation on a later release candidate, and remove expired
rows instead of extending them without new evidence. Blanket or vague entries,
missing or duplicate fields, duplicate axis rows, and expired
rows fail the gate.

- **Windows codegen**: release-gate and tag workflows provision LLVM 22, build
  the release compiler and `hew.lib`, and execute a Rust-staticlib consumer
  through `hew build --link-lib`. Ordinary PR CI remains the faster workspace
  path; the release workflows are the platform artifact authority.
- **Release-gate sandbox parity**: native↔sandbox-VM parity is enforced per PR
  on Linux by `ci.yml` with `make sandbox-parity`. The release gate does not
  rerun that Node/npm-backed harness across its platform matrix; rerun
  `make sandbox-parity` locally before tagging when cutting a release candidate.
- **linux-aarch64**: The pre-tag release gate builds and tests on a native
  Ubuntu 24.04 arm runner; the tag workflow additionally runs packaged-archive
  and clean-room smoke checks.
- **FreeBSD**: Both x86_64 and aarch64 are blocking pre-tag build/test lanes and
  blocking tag-time packaged-archive lanes. The nightly remains additional
  early warning rather than the release authority.
- **Local Debian bookworm arm64 hosts**: `apt.llvm.org/bookworm` arm64 does not
  publish the LLVM 22 packages the release build uses. Validate linux-aarch64
  on Ubuntu 24.04 arm64 instead (CI `ubuntu-24.04-arm`, or an Ubuntu 24.04
  arm VM/container / remote host).
- **TSan (Rust runtime)**: upstream Rust/Cargo build-std + TSan link failures
  (duplicate lang items, panic-strategy mismatch) have no clean repo-side fix
  as of 2026-04. Keep the nightly signal and record bounded release-version
  behavior only via `release-sanitizer-waiver.toml`; re-evaluate when upstream
  resolves.
- **WASM capability gaps**: Channels and I/O streams are rejected at compile
  time when targeting wasm32-wasi.  Timers (`sleep`/`sleep_until`) now have
  cooperative semantics on WASM (actor parks at message boundary) and emit a
  warning rather than an error.  See
  [`docs/wasm-capability-matrix.md`](wasm-capability-matrix.md) for the full
  Tier 1 / Tier 2 disposition table and the WASM-TODO backlog.
