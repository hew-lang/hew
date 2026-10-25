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

Portable components, deployment migration and editor integration
remain in progress.
