# Compile-time knobs report

## Final state

- Branch: `perf/compile-time-knobs`
- HEAD: `001fd8df4918f50cdc2e44f01e32d95412d358c5`
- Push status: not pushed, as instructed
- Tracked worktree status: clean
- Dogfood source revision: `0dcbdba9868938caad74a851b7a9e74d90f2b9ce`

## Commits

1. `4c0451e4d9e37de4c434483bf14633bd8338c815` — `feat(codegen): emit LLVM IR only on request`
2. `c2d711eb2c376b59eb380097918153312d0bb917` — `feat(codegen): honour the requested LLVM optimization level`
3. `fca6d016c238bcbe87547c29f7e3f6e24dd64d2f` — `build(make): select the release-lib compiler by default`
4. `001fd8df4918f50cdc2e44f01e32d95412d358c5` — `perf(compiler): gate dogfood LLVM IR size`

## Probe results

### P0.8 — PASS

Production-source search found no read of emitted textual LLVM IR before object emission or linking. A behavioral FIFO trace then pre-created the expected `.ll` path as a named pipe with one draining reader. The build completed inside a 10-second watchdog and produced 31,830 bytes of IR, a 5,424-byte object, and a 381,696-byte binary. Reopening the FIFO would have blocked, so this proves the compiler only wrote the IR and constructed the object from an in-memory module.

### P0.5 — PASS

Homebrew LLVM's `llc` was available at `/opt/homebrew/opt/llvm/bin/llc`. Three runs on captured dogfood IR measured:

| Backend | Runs (s) | Median (s) | Object bytes |
|---|---:|---:|---:|
| `llc -O0` | 2.44, 2.40, 2.72 | 2.44 | 6,795,704 |
| `llc -O2` | 12.07, 12.25, 13.59 | 12.25 | 3,034,216 |

The existing CLI contract says optimization level 0 is unoptimized, and the checked-in O2 differential gate separately proves runtime parity. Nothing required the target machine to retain O2 code generation when the CLI selected O0.

### P0.4 — PASS

Three paired runs reproduced a large compiler-profile effect while preserving the 88,289,778-byte IR size, exceeding the required 20% threshold:

| Command | Debug runs (s) | Debug median | Release-lib runs (s) | Release-lib median | Reduction |
|---|---:|---:|---:|---:|---:|
| build | 207.02, 113.12, 95.55 | 113.12 | 58.61, 45.88, 48.63 | 48.63 | 57.0% |
| check | 61.05, 48.88, 52.61 | 52.61 | 7.90, 13.25, 8.82 | 8.82 | 83.2% |

Both profiles emitted 1,222 definitions and 245,315 basic blocks. Raw IR hashes varied because declaration ordering is nondeterministic, but byte size and structural counts were exact.

### P0.1 — FAIL; corresponding work intentionally not implemented

A fresh dogfood-shaped fixture covered an actor, an exhaustive machine, ownership-heavy early returns, and a transitive standard-library import. It compiled and checked, and its IR was stable at 664,853 bytes. The first warm repeat moved from a 0.31-second median to 0.32 seconds (+3.2%), but the small control was too close to timer resolution. Replacing it with the unchanged MQTT broker control exposed severe host-load instability across independent five-run alternating batches:

| Batch | Fixture median (s) | Control median (s) |
|---|---:|---:|
| 1 | 0.51 | 1.32 |
| 2 | 1.28 | 3.15 |

The fixture remained exactly 664,853 bytes and the control remained exactly 3,370,102 bytes, but the wall clock was nowhere near the required ±5% stability. Per the falsification rule, no timing flag, checked-in measure fixture, median ceiling, or baseline was implemented. The hypothesis must be replaced or the timing environment stabilized before that work can proceed.

## Implemented behavior

### Explicit LLVM IR retention

- Object emission constructs one LLVM module per target and feeds that same module to textual printing, optimization, and object writing.
- `hew compile` and `hew build` retain textual IR only with `--emit-llvm`.
- Ordinary linked builds remove their intermediate native or wasm object after successful linking.
- `--emit-obj` still retains the requested object.
- Every checked-in `.ll` consumer that needs textual IR now requests it explicitly.
- A focused codegen test proves in-process object emission succeeds without creating `.ll`.

### Optimization contract

- Hew O0 maps to Inkwell `OptimizationLevel::None`.
- Hew O2 retains Inkwell `OptimizationLevel::Default`.
- The O0 middle-end branch remains empty.
- `hew build` defaults to O0; `hew build --release` defaults to O2; an explicit `--opt-level` overrides the convenience flag.
- CLI help documents these defaults.

### Developer launcher profile

- `make hew` builds the compiler and linkable archive with `release-lib` and stages the compiler at `build/bin/hew`.
- `make hew-debug` keeps a separate debug compiler at `build/bin/hew-debug`.
- `make hew-profile-check` prints and asserts the release-lib path.
- CI invokes that assertion, and eligible diff-routed preflight profiles include the same command.
- Make targets that depended on `make hew` now consume the stable `build/bin/hew` launcher instead of bypassing it through the old debug path.

## Dogfood before/after measurement

All measurements used the pinned copy and `ops/native-loop.hew`. Wall times are medians of three runs. IR was requested explicitly for the size/count capture; ordinary post-change builds were separately asserted to leave no `.ll` or `.o`.

| Compiler selection | Build wall (s) | Check wall (s) | `.ll` bytes | Defines | Basic blocks | `.o` bytes | Binary bytes |
|---|---:|---:|---:|---:|---:|---:|---:|
| Before: default debug launcher | 113.12 | 52.61 | 88,289,778 | 1,222 | 245,315 | 3,033,280 | 11,660,864 |
| Before: release-lib control | 48.63 | 8.82 | 88,289,778 | 1,222 | 245,315 | 3,034,216 | 4,773,456 |
| After: default release-lib launcher, O0 backend | 12.10 | 7.34 | 88,289,778 | 1,222 | 245,315 | 6,795,704 | 7,594,240 |

The supported default improved build wall time by 89.3% and check wall time by 86.0% relative to the former debug default. Against the pre-change release-lib control, the O0 backend change improved build time by 75.1% and check time by 16.8%. The larger O0 object and binary are the expected size-for-compile-speed tradeoff documented by the optimization probe.

Post-change raw times were build 12.08/12.34/12.10 seconds and check 7.34/7.39/7.34 seconds. All four post-change binaries (three ordinary builds and one explicit-IR build) were exactly 7,594,240 bytes; their hashes varied with the same nondeterministic ordering seen before.

## Verification

- Pre-commit hooks passed on all three commits. The optimization commit's first attempt was rejected by `clippy::struct_excessive_bools`; the CLI argument container received the repository's established narrow allowance, and the retry passed.
- Focused codegen no-IR object test: 1 passed, 0 failed.
- Focused target-machine mapping test: 1 passed, 0 failed.
- Focused CLI release parsing run: 4 passed, 0 failed, including release-default and explicit-override coverage.
- End-to-end object mapping: default O0 and `--release --opt-level 0` were byte-identical 6,176-byte objects; `--release` produced a distinct 1,096-byte O2 object.
- Artifact matrix: ordinary linked build produced only the binary; `--emit-llvm` produced binary plus IR; `--emit-obj --emit-llvm` retained object plus IR; compile retained IR only when requested.
- `make hew-profile-check`: passed and printed `compiler profile: release-lib`.
- `make hew-debug`: passed and printed `compiler profile: debug`.
- `make check-gate-reachability`: all 55 self-tests passed; the full authority scan reached all 79 CI gate targets, all 24 workspace crates, all five CI-profile exclusions, and all 418 documented Make references.
- `make test-hew-ratchet`: passed with 0 expected failures and 0 actual failures; final lines were `Ratchet: PASSED` and `GATE_EXIT=0`.
- `make ll-diff`: passed; 15 fixtures × 2 targets were byte-identical.
- Diff-routed `make preflight`: not started. The required host conflict check found other active `make preflight` / `ci-preflight-dispatcher.sh --fail-fast` processes (including root make PIDs 1605 and 64230). The brief requires reporting and stopping rather than starting a second preflight.

## PR-ready summary

### Summary

- Build a single LLVM module per target and retain textual IR only when explicitly requested.
- Make target-machine optimization follow the selected O0/O2 level, with O0 as the normal build default and O2 behind `--release`.
- Select the release-lib compiler for the supported developer launcher while preserving an explicit debug launcher.
- Print and assert the selected compiler profile in CI and diff-routed verification.

### Measurements

| Compiler selection | Build wall (s) | Check wall (s) | IR bytes | Object bytes | Binary bytes |
|---|---:|---:|---:|---:|---:|
| Former default debug compiler | 113.12 | 52.61 | 88,289,778 | 3,033,280 | 11,660,864 |
| New default release-lib compiler with O0 backend | 12.10 | 7.34 | 88,289,778 | 6,795,704 | 7,594,240 |

Ordinary builds no longer retain intermediate IR or object files. Explicit IR capture preserved 1,222 definitions and 245,315 basic blocks.

### Verification

- Focused codegen and CLI tests passed.
- Hew suite ratchet passed with zero failures.
- LLVM corpus verification passed for 15 fixtures on native and wasm32.
- Gate reachability and profile assertions passed.
- The timing-regression gate was intentionally omitted because its required stability probe failed.
- Diff-routed preflight was not started because another host preflight was active, as required by the concurrency rule.
