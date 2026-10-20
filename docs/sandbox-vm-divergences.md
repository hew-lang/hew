# Browser runtime behaviour and limits

The browser compiler shares Hew's frontend and verified ownership SIR.
`@hew-lang/wasm` exports editor analysis and `compileToSandboxBytecode`;
`@hew-lang/sandbox-vm` executes the resulting v1 package.

The package carries the compiler's ownership transfers, cleanup edges,
checked operations, selected callables, actor protocols and suspension
points. The VM loader validates every declared operation before execution and
returns `sandbox_rejected` for capabilities outside the browser runtime.

## Executed language surfaces

The VM executes ordinary functions, closures, records, enums, machines,
collections and checked arithmetic. Resumable frames support actors,
supervisors, tasks, scopes, deadlines, select, race, generators and bounded
in-memory pipes. Structural formatting follows the checked nested `Display`
selections and can suspend or fail through those callbacks.

Typed JSON, CBOR, YAML, TOML and MessagePack use the native Rust codec
engine through `@hew-lang/wasm`'s `executeCodec` export. The host supplies
that initialized export as the VM's `codecBridge` option. Codec plans, exact
integers, bytes and structured decode errors cross the bridge without a
second format implementation.

Authored resource close runs during normal cleanup and fault recovery.
Task results, generator captures and buffered pipe items retain their owned
cleanup obligations. Supervised roles follow replacement incarnations;
observing a failed call alone does not settle the run's crash debt.

`make sandbox-parity` compiles and executes the same source programs with the
native compiler and VM. Additional browser tests exercise concurrency,
recovery, cleanup and deterministic replay. The capability categories below
define the boundary for programs outside that corpus.

## Capability refusal

The loader returns `sandbox_rejected` with entries containing `category`,
`code`, `capability`, `message` and `span`. The message is intended for
developers; `capability` retains the operation identity for tooling.

| Category | Meaning | Examples |
| --- | --- | --- |
| `native_only` | The operation requires a host capability. | Filesystem access, network sockets, remote actor calls. |
| `not_implemented` | The browser executor has no implementation for the operation. | Periodic handlers, links and monitors, per-actor heap limits, opaque resource close, missing library shims. |
| `invalid_package` | The package does not satisfy the supported bytecode contract. | Missing entry point, unknown schema or opcode. |

Unimplemented features are refused before the program prints output.
Compiler diagnostics, language panics, checked traps, VM failures and step
budget exhaustion have separate result statuses. A consumer may offer
explicit remote execution; it should not silently reroute failures.

The shipped standard library is embedded in the Wasm compiler from the same
sources as native Hew, including intrinsic provenance. Resolving an import
does not grant the browser its host capabilities.

## Deterministic execution

The VM schedules resumable frames on one JavaScript thread. The bytecode,
seed and recorded inputs reproduce the same scheduler choices. Native
thread interleavings and host scheduling fairness are outside that promise.

Sleep and deadline operations advance a virtual clock. Random APIs use the
run's seeded PRNG. Actor identifiers and scheduler resume tickets are
deterministic. A step budget bounds execution; it does not represent native
CPU time or allocator usage.

Browser `isize` and `usize` use 64-bit semantics to match the native parity
reference, rather than wasm32 pointer width. Strings and owned byte arrays
retain their Hew value semantics.

Structural rendering of an `#[opaque]` handle (`f"{v:?}"` with no `Display`
override) prints `<Name@identity>` on both engines. Native uses the handle's
address; the VM assigns an identity to its own handle representation. Compare
the shape of this output across targets, not the identity value.

## Host differences

Standard input and output belong to the embedding application. Input is
consumed one line per `read_line` call and recorded for replay; end of input
returns `None`. Pipes connect in-memory producers and consumers,
without opening operating-system handles.

Regex matching uses the JavaScript engine. Engine-specific pattern features
and matching behaviour require separate parity checks before being relied
on across targets.

The native-to-VM parity harness runs on Linux. Windows currently skips that
Node-based harness; native and WASI checks are separate evidence.

`timestamp.now()` reads the replay clock's `current_ms` as Unix milliseconds.
That coordinate already includes `epoch_ms`; it is not added again. The default
clock starts at the Unix epoch, and hosts can supply the existing replay clock
to choose another coordinate. Clock acquisition has millisecond precision; stored
timestamps and timestamp arithmetic retain nanosecond precision. Native programs
read the operating system wall clock. Neither clock is a monotonic `instant`.
