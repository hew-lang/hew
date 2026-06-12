# Sandbox VM divergences

The sandbox VM is deterministic by design. It admits programs whose observable behavior can be reproduced in a browser-hosted runtime and rejects native-only APIs that require host operating-system authority. This document is the public catalog for accepted runtime divergences and out-of-scope native surfaces.

## Contents

- [Scheduler determinism vs native preemption](#scheduler-determinism-vs-native-preemption)
- [Virtual clock vs wall-clock](#virtual-clock-vs-wall-clock)
- [Seeded PRNG vs host entropy](#seeded-prng-vs-host-entropy)
- [Logical heap accounting vs allocator](#logical-heap-accounting-vs-allocator)
- [Deterministic actor/message IDs](#deterministic-actormessage-ids)
- [Page stdout/stderr/stdin vs OS streams](#page-stdoutstderrstdin-vs-os-streams)
- [In-memory streams only (no file/network backing)](#in-memory-streams-only-no-filenetwork-backing)
- [Web Worker single-threaded execution](#web-worker-single-threaded-execution)
- [Regex engine differences](#regex-engine-differences)
- [Windows parity enforcement](#windows-parity-enforcement)
- [Profile admission diagnostics](#profile-admission-diagnostics)
- [v0.5 substrate surface admission](#v05-substrate-surface-admission)
- [Out-of-scope native surfaces](#out-of-scope-native-surfaces)

## Scheduler determinism vs native preemption

Native execution may be preempted by the host scheduler. The sandbox VM instead uses deterministic scheduling points so a run can be replayed from the same bytecode package, seed, and input stream. Programs must not rely on native thread interleavings, timing races, or host scheduling fairness as observable behavior.

## Virtual clock vs wall-clock

Sandbox time is virtual. Sleep and deadline behavior advances through the VM clock rather than the browser or operating-system wall clock. This keeps replay stable and prevents host load from changing program results.

## Seeded PRNG vs host entropy

Sandbox random APIs are seeded by the VM run configuration. They do not read host entropy sources. The same seed and input stream produce the same random sequence.

## Logical heap accounting vs allocator

The sandbox VM accounts heap usage logically at VM allocation boundaries. Native allocator layout, fragmentation, and platform-specific allocation overhead are not part of the sandbox contract. Heap-limit diagnostics therefore refer to the VM accounting model, not the host allocator's byte-for-byte behavior.

## Deterministic actor/message IDs

Actor and message identifiers are allocated deterministically by the sandbox runtime. Native runtimes may derive identifiers from process-local runtime state or concurrent allocation order; sandbox IDs are stable for replay and comparison.

## Page stdout/stderr/stdin vs OS streams

Sandbox standard input, output, and error are page-owned streams. They are provided by the embedding page or test harness, not by operating-system file descriptors. Output ordering is defined by VM execution order.

## In-memory streams only (no file/network backing)

Sandbox streams are in-memory values. They do not open host files, sockets, pipes, terminals, or device handles. Programs that require file-backed or network-backed streams must run on a native target.

## Web Worker single-threaded execution

The browser sandbox runs inside a Web Worker and executes VM work on a single JavaScript worker thread. This isolates execution from the page UI thread and avoids shared-memory data races, but it does not expose native threads or host parallelism.

## Regex engine differences

Sandbox regex support is limited to the admitted sandbox profile and may use the browser-compatible implementation selected by the VM. Patterns that depend on native-engine extensions, locale-specific behavior, or implementation-defined backtracking limits are outside the sandbox compatibility contract.

## Windows parity enforcement

Native↔sandbox parity is enforced on Linux CI through the provisioned `make sandbox-parity` gate. Windows currently skips that parity harness because the Windows runner does not provision the `hew-sandbox-vm` Node/npm toolchain for it. This tracked gap is issue #1823.

## Profile admission diagnostics

The bytecode exporter rejects source that cannot be represented by the current sandbox profile. These diagnostics are accepted profile divergences until the corresponding VM surface is admitted:

| Anchor | Diagnostic kind | Meaning |
| --- | --- | --- |
| <a id="reserved-control-flow-lowering"></a>Reserved control-flow lowering | `reserved_control_flow` | Loop and branch forms that need a later bytecode lowering pass are not exported yet. |
| <a id="reserved-runtime-feature-lowering"></a>Reserved runtime feature lowering | `reserved_runtime_feature` | Actor, supervisor, machine, async, generator, and structured-concurrency features that need additional VM runtime support are not exported yet. |
| <a id="unknown-actor-method-symbol"></a>Unknown actor method symbol | `unknown_method_symbol` | Method calls that cannot be resolved through the current sandbox profile allowlist are rejected. |
| <a id="unknown-profile-symbol"></a>Unknown profile symbol | `unknown_symbol` | Helpers or builtins outside the current sandbox profile allowlist are rejected. |
| <a id="unsafe-rejected"></a>Unsafe rejected | `unsafe_rejected` | Unsafe blocks are rejected by the browser sandbox profile. |

## v0.5 substrate surface admission

The current sandbox profile is explicit allowlist first. Native v0.5 surfaces that are not yet represented by sandbox bytecode or the TypeScript VM must reject before bytecode emission with a typed diagnostic.

| Surface | Sandbox disposition | Diagnostic / opcode evidence |
| --- | --- | --- |
| `extern` / native FFI | Rejected as native-only. | `Unsupported::NATIVE_ONLY`; covered by sandbox profile tests. |
| `unsafe` blocks | Rejected. | `unsafe_rejected`; covered by sandbox profile tests. |
| `while let` | Rejected until loop/control-flow lowering is admitted. | `reserved_control_flow`; covered by sandbox profile tests. |
| `string` methods | `len` and `slice` are admitted; broader native string methods remain rejected until the VM has matching shims. | `string.len` / `string.slice` bytecode tests for admitted methods; `unknown_method_symbol` tests for unsupported methods. |
| Machine generics | Rejected with the rest of machine runtime declarations. | `reserved_runtime_feature`; covered by sandbox profile tests. |
| Record construction / auto-derived record layout | Admitted for deterministic value records. | `record.new` and `record.get` bytecode tests. |
| `is` identity operator | Rejected until sandbox heap identity semantics are admitted. | `reserved_runtime_feature`; covered by sandbox profile tests. |

## Out-of-scope native surfaces

The following native surfaces are rejected by the sandbox profile. The stable typed diagnostic kind is `Unsupported::NATIVE_ONLY`.

| Surface | Examples | Diagnostic kind |
| --- | --- | --- |
| File I/O beyond stdin | `std::fs`, `std::io::File*` | `Unsupported::NATIVE_ONLY` |
| Network sockets | `std::net::tcp`, `std::net::udp` | `Unsupported::NATIVE_ONLY` |
| Native FFI | `extern` blocks, raw pointer FFI surfaces | `Unsupported::NATIVE_ONLY` |
| OS signals, processes, env vars | `std::process`, `std::os`, `std::env` | `Unsupported::NATIVE_ONLY` |
| Real-time / wall-clock APIs | `std::time` | `Unsupported::NATIVE_ONLY` |
