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
make test-live-authoring
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
on Linux. `hew_compile::live::frontend::LiveFrontend` prepares source inputs
against immutable declaration snapshots and the retained bindings' exact types.
Publish a prepared input using the worker's actual occupancy, even after a
fault, and release the superseded slots it returns. Earlier values and closures
keep their original declarations when a later input replaces a same-name type
or function. Expanded syntax retains macro hygiene and source spans.

An experimental source prompt is available with the matching compiler, worker
and shared runtime:

```sh
make hew-native live-host
target/debug/hew tool live --runtime target/debug/libhew.so
```

On macOS the library is `libhew.dylib`. Enter declarations, statements or
expressions using ordinary Hew syntax. Values persist between inputs;
initializers execute once. `:type expression` checks without execution,
`:session` lists retained bindings, and `:clear` completes cleanup before
forgetting them. Logical failures preserve completed mutations and actual field
ownership. If the worker disconnects or an operation times out, the prompt ends
with an uncertain-outcome diagnostic and never replays the input. Retired code
artefacts are removed when the worker reports them unloaded; active artefacts
live in the session's scoped temporary directory.

Source snippets and `:load` resolve imports relative to `--project` (the current
directory by default). Ordinary Hew imports retain each imported file's own
resolution context. The worker owns program stdout and stderr; the prompt owns
stdin. Source diagnostics and exact fixes use the original input's offsets;
compiler-generated scaffolding retains its own diagnostic context.
Use `--show-wrapper` to inspect each checked submission's generated source on
stderr before execution.

Deployment migration and editor integration remain in progress.

## Portable components

The optional `component` feature provides typed bindings for the portable
reference router in `wit/router.wit`. Its Hew implementation uses ordinary
typed pipes to await imported services and retains its accepted-event count in
a guest actor. WIT generation supplies ordinary Hew service functions returning
`Result` and `Option`; the SDK owns their carriers and lifecycle adapter. The
router source uses those typed functions without authoring foreign-call plumbing.
Each component instance contains its own Hew runtime. The same
WIT contract also has an independent Rust implementation.

With an installed WASI SDK, run both implementations through the shared oracle:

```sh
HEW_WASI_SDK=/path/to/wasi-sdk make test-live-component
```

The oracle checks concurrent calls, service errors, Unicode and embedded NULs,
admission refusal while paused, retained-state resume, disposal and snapshot
activation with different instance grants. Guest C bindings can be generated
with `make live-component-bindings COMPONENT_BINDINGS_DIR=output`.

The Hew reference also runs a sleeping periodic actor. The adapter fences host
calls before pausing periodic admission, then drains existing actor turns, child
tasks and one-shot actor requests through the guest's ordinary driver. Suspended
frames block that drain. Periodic registrations and actor state remain owned by
the old instance, so an unsuccessful replacement can resume them. The oracle
checks periodic progress after resume and unchanged state during a sleeping
snapshot. A host must drain every exported call and its release before using the
runtime drain API; an empty ready queue alone does not establish quiescence.
Resuming an in-progress attempt withdraws its snapshot authority and requests
cooperative cancellation. Its borrowed state stays retained until release; a
new snapshot or disposal cannot bypass that cleanup. Ordinary calls may resume
without waiting for the abandoned snapshot's result.

`component::host` runs each Store in a persistent task and keeps deployment
admission outside that task. `Host::prepare` compiles immutable bytes before the
fence. `Host::prepare_cached` also publishes validated bytes to an `ArtifactCache`.
The cache survives host restarts; reload verifies the content digest and compiles
with the current engine. `Host::restore` never loads unchecked native machine code.
`Deployment::status` reads the active epoch and component digest together.
`Deployment::replace` drains admitted calls, snapshots the retained
instance and initializes an ungranted candidate within one host deadline. Fuel
yields let the deadline expire even when candidate initialization loops. Expiry
withdraws the attempt and resumes the old instance; commit enables the candidate
and returns a separate retirement handle for old-instance cleanup. A caller
cancelled after admission does not release its running guest call's lease.
`Deployment::dispose` consumes the deployment and retains its cleanup task.
The host also requests SDK driver turns between calls. Each turn uses the guest's
existing scheduler and reports its next timer delay; the host waits for that
deadline or new ingress. A drive turn retains imported service calls through
completion, including calls made by autonomous actors. These operations run
inside a Tokio runtime.

The host oracle exercises admission drain, caller cancellation, snapshot expiry,
non-terminating candidate initialization, retained-state recovery, predecessor
fencing, autonomous service calls without ingress, cache reopening and disposal.
The cache also rejects corrupt entries without overwriting them. Fenced network
deployment remains active implementation work.
