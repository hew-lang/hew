# Experimental native live execution

`hew-live` prepares and loads native Hew host exports in a persistent process.
It imports one shared `libhew` runtime and retains code generations through
explicit executable roots and dependency edges. The native boundary is trusted
and tied to an exact build; it is not a public cross-version ABI or an isolation
boundary.

Build and exercise the native execution path with:

```sh
make test-live-native
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

Native preparation supports ELF and Mach-O linker drivers; the execution oracle
has been run on Linux. Windows module linking remains unimplemented.

The optional `component` feature provides typed bindings for the portable
reference router in `wit/router.wit`. Its Hew implementation uses ordinary
typed pipes to await imported services and retains its accepted-event count in
a guest actor. Each component instance contains its own Hew runtime. The same
WIT contract also has an independent Rust implementation.

With an installed WASI SDK, run both implementations through the shared oracle:

```sh
HEW_WASI_SDK=/path/to/wasi-sdk make test-live-component
```

The oracle checks concurrent calls, service errors, Unicode and embedded NULs,
admission refusal while paused, retained-state resume, disposal and snapshot
activation with different instance grants. Guest C bindings can be generated
with `make live-component-bindings COMPONENT_BINDINGS_DIR=output`.

The reference router has one counter actor and no autonomous background work.
Its quiescence contract drains admitted calls and retains the old actor so an
unsuccessful replacement can resume it. General subtree quiescence with timers
and child tasks, fenced network deployment and durable artefact caching remain
separate implementation work; this is not yet the complete live-execution host.
