# Ownership and Unwind Hard Cutover

Status: adopted. This is the compiler/runtime ownership model; there is no
advisory or legacy native mode.

## Contract

Hew MIR is the ownership authority. Every heap-owning definition, including a
synthetic call result or projection temporary, enters the owned-local ledger.
A move transfers one obligation, a borrow creates none, and a shared COW value
requires an explicit retain. Code generation consumes elaborated drop plans; it
must not rediscover ownership from LLVM types or symbol spellings.

The obligation verifier rejects a function unless every reachable exit balances
each owner exactly once. The checked exits are ordinary call continuation,
return, panic/trap, cancellation, yield/suspension cleanup, and call unwind.
Under-release and over-release are both compilation errors.

Successful calls initialize their result and continue with the normal edge.
Failed calls do not initialize the destination and follow the unwind edge. The
unwind plan is computed from the pre-call ownership state, so values transferred
later to `ReturnSlot` or an aggregate remain protected during earlier calls.

## Native LLVM lowering

On native Itanium-EH targets, potentially unwinding Hew and external calls lower
to LLVM `invoke`. The unwind destination has a cleanup `landingpad`, runs the
MIR-provided destructors in reverse ownership order, and finishes with `resume`.
The module uses `rust_eh_personality`, matching the Rust runtime boundary that
catches typed Hew panic payloads in the scheduler.

Runtime exports that can raise a logical Hew trap use Rust's `extern
"C-unwind"` ABI. This makes the unwind permission explicit at the FFI boundary
and lets the LLVM `invoke` cleanup edge run. Non-trapping exports remain plain
`extern "C"`; allowing a Rust panic to cross one would be an ABI violation and
abort instead of executing Hew destructors.

Suspended computations continue to use LLVM coroutine ownership: suspended
frames are destroyed through `llvm.coro.destroy`; frames abandoned while
running are reclaimed by the scheduler's active-frame quarantine. The two
authorities are reconciled so one allocation has one free authority.

WASM and Windows MSVC use the target-specific typed cleanup registry because
the current Inkwell backend does not expose a uniform usable EH representation
for those targets. This is a per-target backend choice, not a mixed or staged
native mode. The same MIR drop plans and obligation verifier drive both paths.

Synchronous hardware faults are process-fatal. Signal handlers run on an
alternate stack, use only async-signal-safe operations, and `_exit`; Hew never
attempts to resume arbitrary interrupted ownership code. Logical Hew traps and
arena exhaustion use a typed Rust unwind payload and are recovered at the actor
scheduler boundary after cleanup landing pads have run.

## Strings and C ABI provenance

Managed Hew strings are header-bearing reference-counted allocations. The C ABI
registers the exact returned data pointer and allocation base before publishing
the pointer. Retain, release, and header validation first prove membership in
that registry, then perform header pointer arithmetic. A pointer absent from the
registry is borrowed (for example, a compiler-emitted literal); it is never
probed or freed as a managed string. Retired or corrupt managed allocations fail
closed.

This replaces executable-image address scans and prevents undefined behavior
from subtracting a header offset from an arbitrary C string pointer.

## Actor state

Lexical owners use MIR/LLVM stack unwinding. Actor-state replacement remains a
separate transaction: materialize the replacement, register the state mutation,
release the old value, publish the new value, then commit. Crash recovery can
therefore distinguish stable actor state from a partially prepared replacement
without treating state fields as lexical stack owners.

## Verification requirements

- MIR validates one normal and one unwind plan for every call and rejects orphan
  or duplicate unwind drops.
- Obligation balance is a hard compiler gate for every exit kind.
- LLVM tests verify `invoke`, `landingpad`, typed destructor calls, `resume`, and
  module validity.
- Native and WASM tests cover the structured and fallback target backends.
- C ABI tests cover provenance, corruption, retain/release, unmanaged pointers,
  and refcount overflow.
- Runtime tests cover typed panic recovery, active coroutine reclamation,
  cleanup reentrancy, hardware-fault death, and string/container lifetimes.

## Design basis

The ownership model follows Swift SIL's explicit owned/guaranteed values and
forwarding consumes, Rust MIR's path-sensitive move paths and drop elaboration,
and LLVM's native exception/coroutine cleanup primitives:

- [Swift SIL ownership](https://github.com/swiftlang/swift/blob/main/docs/SIL/Ownership.md)
- [Rust move paths](https://rustc-dev-guide.rust-lang.org/borrow-check/moves-and-initialization/move-paths.html)
- [Rust drop elaboration](https://rustc-dev-guide.rust-lang.org/mir/drop-elaboration.html)
- [LLVM exception handling](https://llvm.org/docs/ExceptionHandling.html)
- [LLVM coroutines](https://llvm.org/docs/Coroutines.html)
- [Rust FFI unwinding](https://doc.rust-lang.org/nomicon/ffi.html)
