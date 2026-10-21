# Release and ecosystem readiness

Snapshot: 2026-10-11. Deliver a usable compiler/client candidate and compatible
signed ecosystem archives through the [release runbook](release-runbook.md).
The source version is `0.6.0-rc8`; release qualification and publication are
separate steps under [versioning](versioning.md). Never replace published
candidate tags or assets.

## Starting point

- Module membership, keyed construction, platform paths/native symbol fixes
  and typed failure edges have merged through
  [PR3659](https://github.com/hew-lang/hew/pull/3659),
  [PR3664](https://github.com/hew-lang/hew/pull/3664),
  [PR3661](https://github.com/hew-lang/hew/pull/3661) and
  [PR3636](https://github.com/hew-lang/hew/pull/3636).
- [Registry PR3666](https://github.com/hew-lang/hew/pull/3666) is merged.
  Official slash wire identities, custom dotted defaults/explicit slash modes,
  signing checks and fallback handling are implemented. Controlled
  [signed-install fixtures](https://github.com/hew-lang/hew/blob/4de1c83407a9dd3b4c2a9bc6ae0c67766613cf7c/hew-pkg/tests/test_registry_wire_install.rs)
  exercise the CLI and native consumers. Published
  [rc7](https://github.com/hew-lang/hew/releases/tag/v0.6.0-rc7)
  predates the client fix.
- The earlier rc8 candidate qualified by
  [PR3673](https://github.com/hew-lang/hew/pull/3673) passed its
  [cross-platform gate](https://github.com/hew-lang/hew/actions/runs/38027378763)
  and [release dry run](https://github.com/hew-lang/hew/actions/runs/38027378726),
  with publication skipped. Those results cover that revision and scope.
  Later compiler/parser/formatter changes require fresh combined qualification.
- [Windows child-pipe readiness](https://github.com/hew-lang/hew/pull/3675)
  and [current capability descriptions](https://github.com/hew-lang/hew/pull/3674)
  have merged, with PR, merge-queue and merged-main checks passing.
  [Nightly sanitizers](https://github.com/hew-lang/hew/actions/runs/38139361238)
  and [FreeBSD nightly](https://github.com/hew-lang/hew/actions/runs/38143935846)
  passed on that main revision. Later compiler changes need their own evidence.
- [Editor launch guidance](https://github.com/hew-lang/hew/pull/3660),
  [sanitizer guidance](https://github.com/hew-lang/hew/pull/3662) and the
  [VS Code formatter fix](https://github.com/hew-lang/vscode-hew/pull/21)
  are merged. Source completion does not publish a new editor build.

## Ecosystem inventory

[Ecosystem PR9](https://github.com/hew-lang/ecosystem/pull/9) is already merged
at [d7bb375](https://github.com/hew-lang/ecosystem/tree/d7bb375c8330b1d838bea6c71f49aa8def2240bf).
Its rc7 source audit and pure-Hew tests passed; compiler minimums now agree
with that audit. It did not qualify installed native packages against a new
compiler candidate or publish archives.

The historical registry observation at 15:42 UTC and archive hashing at 15:46 UTC
on 2026-10-09 found every intended `0.3.0` version below absent. Recheck the
version endpoints before qualification or publication; this dated inventory
does not assert current registry state. Package
links identify the canonical version-detail endpoints. The fifteen `0.2.0`
archive hashes match registry metadata; PR9's source-only audit found removed
`::` syntax under both rc4 and rc7. Checksums are not signature verification
or execution evidence. Older `0.1.0` versions are outside that compatibility audit.

| Intended package (`0.3.0` absent on 2026-10-09)                                        | Observed `0.2.0` archive SHA-256                                                                                                     |
| -------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| [hew.auth.oauth](https://registry.hewpkg.com/api/v1/packages/hew/auth/oauth/0.3.0)     | [`1b97176b181d19faf0158f6578e6ed8630f2c80978593d4fd17ca7a01b7bc9bd`](https://cdn.hewpkg.com/tarballs/hew/auth/oauth/0.2.0.tar.zst)   |
| [hew.dag](https://registry.hewpkg.com/api/v1/packages/hew/dag/0.3.0)                   | [`0c5cb70cfbe8855fef98b2b11edcf0db9dd6af0d8bc322216ecda6e9c8cb93af`](https://cdn.hewpkg.com/tarballs/hew/dag/0.2.0.tar.zst)          |
| [hew.db.mongodb](https://registry.hewpkg.com/api/v1/packages/hew/db/mongodb/0.3.0)     | [`5165fcc67494bb34340d66487ebb6d75cb638dfd352396951db38f99096a9658`](https://cdn.hewpkg.com/tarballs/hew/db/mongodb/0.2.0.tar.zst)   |
| [hew.db.mysql](https://registry.hewpkg.com/api/v1/packages/hew/db/mysql/0.3.0)         | [`139b72a5b8d98fcfbcf0ed2d3c823eae6a728735fe936008a255cf8845f65a56`](https://cdn.hewpkg.com/tarballs/hew/db/mysql/0.2.0.tar.zst)     |
| [hew.db.postgres](https://registry.hewpkg.com/api/v1/packages/hew/db/postgres/0.3.0)   | [`83fc9049fec09d83c0373d740cf4b6ff8667150b54b547859cc4b55ce1084dec`](https://cdn.hewpkg.com/tarballs/hew/db/postgres/0.2.0.tar.zst)  |
| [hew.db.redis](https://registry.hewpkg.com/api/v1/packages/hew/db/redis/0.3.0)         | [`ac9c720e5151f1ac8286e8f96b62086886e2839e62a738d295cbe37e271b98cb`](https://cdn.hewpkg.com/tarballs/hew/db/redis/0.2.0.tar.zst)     |
| [hew.db.sql](https://registry.hewpkg.com/api/v1/packages/hew/db/sql/0.3.0)             | No published package                                                                                                                 |
| [hew.db.sqlite](https://registry.hewpkg.com/api/v1/packages/hew/db/sqlite/0.3.0)       | [`e9b1cc95599698e7c23d54a5b405cdf0707bb5701ca5be6c9582e73dff8f4ae3`](https://cdn.hewpkg.com/tarballs/hew/db/sqlite/0.2.0.tar.zst)    |
| [hew.image.magick](https://registry.hewpkg.com/api/v1/packages/hew/image/magick/0.3.0) | [`7f432acc8dcf20c6d61cca0dc93c378868eb521a67dd743b1754bae9a7d1f8de`](https://cdn.hewpkg.com/tarballs/hew/image/magick/0.2.0.tar.zst) |
| [hew.math.stats](https://registry.hewpkg.com/api/v1/packages/hew/math/stats/0.3.0)     | [`89e1aee908b19f9bd57e08258bba885aa58fb964657a9bc534216ef322dcb14a`](https://cdn.hewpkg.com/tarballs/hew/math/stats/0.2.0.tar.zst)   |
| [hew.metrics](https://registry.hewpkg.com/api/v1/packages/hew/metrics/0.3.0)           | [`56f3b4383cbc570dcb2a210f0daf702428bc6a14a8bdf0412ef550440f445702`](https://cdn.hewpkg.com/tarballs/hew/metrics/0.2.0.tar.zst)      |
| [hew.net.http](https://registry.hewpkg.com/api/v1/packages/hew/net/http/0.3.0)         | [`9b7fb6ce788185dc5c7080216b04a9ce0d2bfe5cd943651e33a180f91af8e2a6`](https://cdn.hewpkg.com/tarballs/hew/net/http/0.2.0.tar.zst)     |
| [hew.queue.mqtt](https://registry.hewpkg.com/api/v1/packages/hew/queue/mqtt/0.3.0)     | [`fe49f74eab3a998c8240158f097e39f7aec9d7361fd1dcf7b4226f0d80a6c320`](https://cdn.hewpkg.com/tarballs/hew/queue/mqtt/0.2.0.tar.zst)   |
| [hew.queue.nats](https://registry.hewpkg.com/api/v1/packages/hew/queue/nats/0.3.0)     | [`febd274b5f9d6a33d26ba413f8f65e62db75215e387b1950fa3e46493f0bc792`](https://cdn.hewpkg.com/tarballs/hew/queue/nats/0.2.0.tar.zst)   |
| [hew.storage.s3](https://registry.hewpkg.com/api/v1/packages/hew/storage/s3/0.3.0)     | [`91bb93406cf2281b5737852e07668f186cd3a429c42baa44176f8cd80eac659d`](https://cdn.hewpkg.com/tarballs/hew/storage/s3/0.2.0.tar.zst)   |
| [hew.template](https://registry.hewpkg.com/api/v1/packages/hew/template/0.3.0)         | [`26b75c610c639c62073dd5ef51ebfd35d36b9abb21aa6713a42f72bea8f65f89`](https://cdn.hewpkg.com/tarballs/hew/template/0.2.0.tar.zst)     |

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
   Apply the runbook platform policy: Linux x86_64/aarch64 are primary, macOS
   ARM secondary, and Windows/FreeBSD nonblocking on patches except for major
   build/run regressions. Keep failures explicit and assign deferred fixes
   before the next minor or in another patch. The current FreeBSD aarch64
   pre-tag lane proves compiler build and native smoke only. The release workflow currently ships
   five toolchain archives and has no FreeBSD aarch64 archive job or required
   asset. Track that distribution gap under the platform policy; cross-built
   libraries alone do not qualify a shipped archive.
   Require executed ASan, strict workspace/native/C-ABI/compiled-Hew gates,
   Linux native-to-sandbox parity and browser compiler/VM build and execution
   paths. Standalone WASI and non-browser Wasm are non-gating in the 0.6 series.
   Respect the [WASM capability matrix](wasm-capability-matrix.md):
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
4. **Retain revision-bound evidence and unresolved scope.** The earlier
   [cross-platform gate](https://github.com/hew-lang/hew/actions/runs/38027378763)
   passed after portable fixture and deadline corrections. Its FreeBSD aarch64
   result is build/smoke evidence, not a full suite or shipped archive.
   The historical [coverage run](https://github.com/hew-lang/hew/actions/runs/37942830488)
   failed on runtime archive lookup and supervisor execution; the latest
   applicable nightly/coverage outcomes must be assessed separately from PR CI.
   Record source SHA, run/job scope, outcomes, archive hashes, signature
   identities and consumer outputs. Keep failed, skipped, cancelled and
   unexecuted work visible. Reuse earlier evidence only when the relevant
   source, configuration and artefact identities are established unchanged.

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
