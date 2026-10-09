# Release and ecosystem readiness

Snapshot: 2026-10-09. Deliver a usable compiler/client candidate and compatible
signed ecosystem archives through the [release runbook](release-runbook.md).
The next compiler tag/version remains a release-owner decision under
[versioning](versioning.md); committed `0.6.0-rc7` identifies an already
published candidate, not permission to reuse its tag or replace its assets.

## Starting point

- Inspected Hew [main at e6bdf09](https://github.com/hew-lang/hew/tree/e6bdf09abaa50aba134ab1a0da2ba9739989d6d8).
  [Main CI](https://github.com/hew-lang/hew/actions/runs/37901139708),
  [nightly sanitizers](https://github.com/hew-lang/hew/actions/runs/37933275996)
  and [FreeBSD x86_64 nightly](https://github.com/hew-lang/hew/actions/runs/37940742948)
  passed for that revision and their recorded scopes.
- [Registry PR3666](https://github.com/hew-lang/hew/pull/3666) is merged.
  Official slash wire identities, custom dotted defaults/explicit slash modes,
  signing checks and fallback handling are implemented. Its controlled
  [signed-install fixtures](https://github.com/hew-lang/hew/blob/4de1c83407a9dd3b4c2a9bc6ae0c67766613cf7c/hew-pkg/tests/test_registry_wire_install.rs)
  passed with the actual CLI and native consumers; official package lookup
  also passed. These prove the source fix, not live archive compatibility.
  Published [rc7](https://github.com/hew-lang/hew/releases/tag/v0.6.0-rc7)
  predates this fix.
- [Editor launch guidance](https://github.com/hew-lang/hew/pull/3660),
  [sanitizer guidance](https://github.com/hew-lang/hew/pull/3662) and the
  [VS Code formatter fix](https://github.com/hew-lang/vscode-hew/pull/21)
  are merged. Their completed source work does not publish a new editor build.
- Resolve candidate inclusion or deferral of open [PR3659](https://github.com/hew-lang/hew/pull/3659)
  (module membership), [PR3664](https://github.com/hew-lang/hew/pull/3664)
  (keyed construction) and [PR3661](https://github.com/hew-lang/hew/pull/3661)
  (platform paths/native symbol collisions). Reuse those efforts.
  [Typed failure edges](https://github.com/hew-lang/hew/pull/3636) have already
  merged since the registry fixture qualification; recheck downstream source
  and native ABI against the selected candidate.

## Ecosystem inventory

[Ecosystem PR9](https://github.com/hew-lang/ecosystem/pull/9) is already merged
at [d7bb375](https://github.com/hew-lang/ecosystem/tree/d7bb375c8330b1d838bea6c71f49aa8def2240bf).
Its rc7 source audit and pure-Hew tests passed; compiler minimums now agree
with that audit. It did not qualify installed native packages against a new
compiler candidate or publish archives.

Read-only registry observation at 15:42 UTC and archive hashing at 15:46 UTC
on 2026-10-09 still find every intended `0.3.0` version below absent. Package
links identify the canonical version-detail endpoints. The fifteen `0.2.0`
archive hashes match registry metadata; PR9's source-only audit found removed
`::` syntax under both rc4 and rc7. Checksums are not signature verification
or execution evidence. Older `0.1.0` versions are outside that compatibility audit.

| Intended package (`0.3.0` absent) | Existing `0.2.0` archive SHA-256 |
| --- | --- |
| [hew.auth.oauth](https://registry.hewpkg.com/api/v1/packages/hew/auth/oauth/0.3.0) | [`1b97176b181d19faf0158f6578e6ed8630f2c80978593d4fd17ca7a01b7bc9bd`](https://cdn.hewpkg.com/tarballs/hew/auth/oauth/0.2.0.tar.zst) |
| [hew.dag](https://registry.hewpkg.com/api/v1/packages/hew/dag/0.3.0) | [`0c5cb70cfbe8855fef98b2b11edcf0db9dd6af0d8bc322216ecda6e9c8cb93af`](https://cdn.hewpkg.com/tarballs/hew/dag/0.2.0.tar.zst) |
| [hew.db.mongodb](https://registry.hewpkg.com/api/v1/packages/hew/db/mongodb/0.3.0) | [`5165fcc67494bb34340d66487ebb6d75cb638dfd352396951db38f99096a9658`](https://cdn.hewpkg.com/tarballs/hew/db/mongodb/0.2.0.tar.zst) |
| [hew.db.mysql](https://registry.hewpkg.com/api/v1/packages/hew/db/mysql/0.3.0) | [`139b72a5b8d98fcfbcf0ed2d3c823eae6a728735fe936008a255cf8845f65a56`](https://cdn.hewpkg.com/tarballs/hew/db/mysql/0.2.0.tar.zst) |
| [hew.db.postgres](https://registry.hewpkg.com/api/v1/packages/hew/db/postgres/0.3.0) | [`83fc9049fec09d83c0373d740cf4b6ff8667150b54b547859cc4b55ce1084dec`](https://cdn.hewpkg.com/tarballs/hew/db/postgres/0.2.0.tar.zst) |
| [hew.db.redis](https://registry.hewpkg.com/api/v1/packages/hew/db/redis/0.3.0) | [`ac9c720e5151f1ac8286e8f96b62086886e2839e62a738d295cbe37e271b98cb`](https://cdn.hewpkg.com/tarballs/hew/db/redis/0.2.0.tar.zst) |
| [hew.db.sql](https://registry.hewpkg.com/api/v1/packages/hew/db/sql/0.3.0) | No published package |
| [hew.db.sqlite](https://registry.hewpkg.com/api/v1/packages/hew/db/sqlite/0.3.0) | [`e9b1cc95599698e7c23d54a5b405cdf0707bb5701ca5be6c9582e73dff8f4ae3`](https://cdn.hewpkg.com/tarballs/hew/db/sqlite/0.2.0.tar.zst) |
| [hew.image.magick](https://registry.hewpkg.com/api/v1/packages/hew/image/magick/0.3.0) | [`7f432acc8dcf20c6d61cca0dc93c378868eb521a67dd743b1754bae9a7d1f8de`](https://cdn.hewpkg.com/tarballs/hew/image/magick/0.2.0.tar.zst) |
| [hew.math.stats](https://registry.hewpkg.com/api/v1/packages/hew/math/stats/0.3.0) | [`89e1aee908b19f9bd57e08258bba885aa58fb964657a9bc534216ef322dcb14a`](https://cdn.hewpkg.com/tarballs/hew/math/stats/0.2.0.tar.zst) |
| [hew.metrics](https://registry.hewpkg.com/api/v1/packages/hew/metrics/0.3.0) | [`56f3b4383cbc570dcb2a210f0daf702428bc6a14a8bdf0412ef550440f445702`](https://cdn.hewpkg.com/tarballs/hew/metrics/0.2.0.tar.zst) |
| [hew.net.http](https://registry.hewpkg.com/api/v1/packages/hew/net/http/0.3.0) | [`9b7fb6ce788185dc5c7080216b04a9ce0d2bfe5cd943651e33a180f91af8e2a6`](https://cdn.hewpkg.com/tarballs/hew/net/http/0.2.0.tar.zst) |
| [hew.queue.mqtt](https://registry.hewpkg.com/api/v1/packages/hew/queue/mqtt/0.3.0) | [`fe49f74eab3a998c8240158f097e39f7aec9d7361fd1dcf7b4226f0d80a6c320`](https://cdn.hewpkg.com/tarballs/hew/queue/mqtt/0.2.0.tar.zst) |
| [hew.queue.nats](https://registry.hewpkg.com/api/v1/packages/hew/queue/nats/0.3.0) | [`febd274b5f9d6a33d26ba413f8f65e62db75215e387b1950fa3e46493f0bc792`](https://cdn.hewpkg.com/tarballs/hew/queue/nats/0.2.0.tar.zst) |
| [hew.storage.s3](https://registry.hewpkg.com/api/v1/packages/hew/storage/s3/0.3.0) | [`91bb93406cf2281b5737852e07668f186cd3a429c42baa44176f8cd80eac659d`](https://cdn.hewpkg.com/tarballs/hew/storage/s3/0.2.0.tar.zst) |
| [hew.template](https://registry.hewpkg.com/api/v1/packages/hew/template/0.3.0) | [`26b75c610c639c62073dd5ef51ebfd35d36b9abb21aa6713a42f72bea8f65f89`](https://cdn.hewpkg.com/tarballs/hew/template/0.2.0.tar.zst) |

Keep authored source/archive bytes, publisher checksum signatures and registry
countersignatures distinct. Accepting historical descriptive metadata in
PR3666 does not migrate incompatible source inside an old archive.

## Work and acceptance

1. **Freeze the candidate.** Choose its scope, new version and commit; align
   Cargo/lockfiles, sandbox npm identity, syntax data, changelog and curated
   release notes using `scripts/release-identity.sh` and `make release-checks`.
   Revalidate/migrate the pinned ecosystem sources against that candidate,
   then align compiler ranges, native `hew-cabi` revision and lockfile via
   the ecosystem [toolchain-pin procedure](https://github.com/hew-lang/ecosystem/blob/d7bb375c8330b1d838bea6c71f49aa8def2240bf/docs/toolchain.md).
2. **Qualify the compiler and shipped artefacts.** Complete candidate CI,
   [release-gate.yml](../.github/workflows/release-gate.yml) and the
   [release.yml](../.github/workflows/release.yml) dry run (`dry_run=true`).
   Run the declared Linux x86_64/aarch64, macOS arm64/x86_64, Windows x86_64
   and FreeBSD x86_64 lanes. The current FreeBSD aarch64 pre-tag lane proves
   compiler build and native smoke only. The release workflow currently ships
   six toolchain archives and has no FreeBSD aarch64 archive job or required
   asset. Restore and qualify that lane, or record an owner-approved deferral
   and align supported-platform claims; cross-built libraries alone do not
   qualify a shipped archive.
   Require executed ASan, strict workspace/native/C-ABI/compiled-Hew gates,
   Linux native-to-sandbox parity and the supported WASI/browser build and
   execution paths. Respect the [WASM capability matrix](wasm-capability-matrix.md):
   matching refusals or skips are not successful parity. Review nightly
   safety scopes and record the runbook's TSan/Miri acceptance decision.
   Verify each final archive's checksum/provenance, relocation, tools,
   native library consumers and first use (Hello World, init/edit/rebuild,
   diagnostic recovery and actor execution). Qualify the editor package,
   formatter and LSP against matching candidate binaries; synchronize downstream
   grammar consumers when included syntax changes require it. Measure Linux GLIBC/GLIBCXX/
   CXXABI requirements and execute on the declared minimum OS; development
   binaries do not establish the release archive's runtime baseline.
3. **Qualify exact package bytes before public upload.** Update the ecosystem
   publisher's [rc7 source pin](https://github.com/hew-lang/ecosystem/blob/d7bb375c8330b1d838bea6c71f49aa8def2240bf/toolchain.env)
   to the chosen corrected client. Stage the intended archives in an isolated
   registry/profile with an isolated `HOME`: the publisher uses `$HOME/.hew`,
   so `HEW_HOME` alone does not isolate its credential paths. Use the packaged
   candidate, empty cache, verified
   publisher signatures and exact registry countersignatures. Exercise
   transitive dependencies/features, lock/offline reuse and tampering refusal.
   Rerun signed client fixtures with `HEW_BIN` pointing at the packaged
   candidate and `HEW_WIRE_TEST_NATIVE=1` through `make test-strict`.
   For every intended package, install and compile/run a consumer with
   expected output; native packages also need their library builds, matched
   ABI and the [ecosystem corpus/service tests](https://github.com/hew-lang/ecosystem/blob/d7bb375c8330b1d838bea6c71f49aa8def2240bf/docs/toolchain.md#the-corpus-gate).
   Check supported pure-Hew WASM consumers separately. Source checks alone,
   a metadata response or an archive hash are insufficient.
4. **Resolve recorded failures and retain evidence.** The latest
   [cross-platform gate](https://github.com/hew-lang/hew/actions/runs/37875862773)
   was cancelled on an earlier PR3661 head, not the selected candidate.
   [Coverage at e6bdf09](https://github.com/hew-lang/hew/actions/runs/37942830488)
   failed: instrumented WASI parity could not find its runtime archive, and
   runtime coverage reported `examples/supervisor_crash_budget.hew` failing
   or timing out. Classify/fix or explicitly disposition those causes with
   equivalent candidate checks. Record source SHA, run/job scope, outcomes,
   archive hashes, signature identities and consumer outputs; keep missing,
   failed, skipped and cancelled work visible.

## Publication order and recovery

After qualification and explicit publication authorization:

1. Publish the new compiler/toolchain and verify its distributed bytes,
   checksums/provenance and version before advertising registry installation.
   Follow the runbook's tag/environment approvals and immutable-asset recovery.
2. Publish the qualified package set with the corrected pinned client.
   The existing [ecosystem main-push workflow](https://github.com/hew-lang/ecosystem/blob/d7bb375c8330b1d838bea6c71f49aa8def2240bf/.github/workflows/publish.yml)
   uploads automatically: keep preparation off `main` until publication is
   authorized. Its [publisher](https://github.com/hew-lang/ecosystem/blob/d7bb375c8330b1d838bea6c71f49aa8def2240bf/scripts/publish-packages.sh)
   currently sorts manifest paths; establish dependency-first publication
   (`hew.db.sql` before SQL drivers), not alphabetical order.
3. Read back every uploaded version, strictly verify identities/signatures
   and bytes, then repeat clean **public** installs and compiled consumers.
   Only afterward update installation links/examples and publish a separately
   versioned editor build with matching LSP pins and archive hashes.
4. On partial failure, stop further publication and retain the per-version
   results; a Git revert cannot undo registry uploads. Independently read back
   and verify already-present versions before retrying: a publisher skip is
   not verification. Resume only missing, verified versions using the existing
   recovery mechanisms. Never overwrite signed archives, published release assets or
   tags. If a new package is unusable, approve any yank/corrective version
   explicitly; return affected guidance to the pinned, tested clone route
   rather than recommend incompatible old archives. Restore downstream pins
   to a known working version when needed.

Completion means the intended public compiler/client and package bytes are
available, trusted clean installs and consumers pass, and the chosen platform
and safety scope is recorded. Update or retire this snapshot when that holds.
