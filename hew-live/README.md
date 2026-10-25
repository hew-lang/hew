# Experimental native live execution

`hew-live` prepares and loads native Hew host exports in a persistent process.
It imports one shared `libhew` runtime and retains code generations through
explicit executable roots and dependency edges. The native boundary is trusted
and tied to an exact build; it is not a public cross-version ABI or an isolation
boundary.

Build and exercise the native execution path with:

```sh
make test-live-native
make test-live-host
```

The test compiles ordinary Hew into a native module, retains a resource owning
an actor and mutable closure across calls, verifies consuming ownership slots,
and closes the actor before retiring its generation. It also rejects a mismatched
compiler identity and reclaims an unreachable generation dependency cycle.

An embedding process must not statically link another Hew runtime. Open the
shared runtime once with `NativeHost::open`, clone that host for additional
owners, and prepare modules with `native::prepare`. That function links a
verified `hew build --emit-obj --export-c` object against the selected runtime.
The returned manifest records the exact module and runtime hashes alongside
compiler and target identities. Load only trusted, immutable prepared artefacts.
The loader checks their identities and runtime symbol resolution before
exposing exports.

Retain a `Generation` with every owning value, suspended call, actor, callback,
borrowed foreign view and destructor descriptor that can enter that code.
Register dependencies with `Generation::depends_on`. Call `NativeHost::collect`
after closing registrations and completing callbacks; dependency cycles without
roots are reclaimed together. The unsafe symbol API requires the generated
header's exact signature and ownership contract. A native runtime lifetime token
still follows the existing `hew_runtime_new` / `hew_runtime_release` contract.

`NativeSession` executes compiler-generated submissions over persistent typed
places. Each result reports actual initialization and write/move revisions,
including work completed before a fault. Clearing a session runs ordinary Hew
cleanup in reverse declaration order and collects cleanup failures. The typed
places retain their defining code until their values and cleanup are released.

The `hew-live-host` worker runs these sessions separately from the compiler,
which already statically links a runtime. One local control connection carries
ordered prepare, execute, inspect, retire and cleanup commands; ordinary Hew
stdio remains available to the program. A lost reply leaves the caller with an
uncertain outcome. The worker never retries or replays a submission.

Preparation supports ELF and Mach-O linker drivers; execution has been tested
on Linux. The persistent source frontend, portable components, deployment
migration and authoring integration remain in progress.
