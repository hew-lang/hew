# Experimental live execution

Stage the compiler, standard library, shared runtime, both live companions and
the prepared native node module together:

```sh
make live-sdk LIVE_SDK_DEST=/path/to/new/hew-live-sdk
export PATH=/path/to/new/hew-live-sdk/bin:$PATH
export HEW_WASI_SDK=/path/to/wasi-sdk
```

The destination must be new. The SDK uses the ordinary release layout and can
be moved after staging. The companions find their matching compiler, runtime,
portable libraries and node module relative to their executable. Build inputs
stay in the selected Cargo target; `make clean` cleans that target after its
consumers finish. Native preparation is implemented for ELF and Mach-O; the
end-to-end SDK controls currently run on Linux.

The staged reference source is `share/hew/live/router.hew`. For the persistent
source prompt, run `hew tool live --runtime /path/to/new/hew-live-sdk/lib/hew/libhew.so`
(use `libhew.dylib` on macOS).

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

### Process standard-input lines

```sh
printf 'first\n\nlast\n' | hew eval --lines 'if line != "" { println(line); }'
hew eval --lines -f filter.hew < events.log
```

The inline body or file contains ordinary Hew statements with an immutable
`line: string` binding. The harness compiles once and processes input in order.
Each iteration has its own local bindings; it can use `continue`, `break` or
`return` just like an ordinary loop. Output is streamed as the program runs.
`--show-wrapper` prints the generated program on stderr; diagnostics identify
the original inline body or file.

Input follows `std.io.read_line`: LF and CRLF delimit lines, blank lines are
preserved, a final unterminated line is processed, and invalid UTF-8 becomes
U+FFFD. No input means no iterations. Read or execution failure exits nonzero;
`--timeout` bounds the complete execution after compilation. `--json` captures
the output and terminal result using the existing eval contract. Standard input
belongs to the data stream, so `-f -` is unavailable in this mode.

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
The cache also rejects corrupt entries without overwriting them.

### Authenticated source deployment

`make live-node` builds the experimental `hew-live-node` companion. Its native
transport is ordinary Hew source using `Node`, authenticated peer sessions and a
`#[wire]` control actor. The companion loads that code against one shared native
runtime; each portable deployment keeps its own component Store and guest runtime.
Only the explicitly authorized peer receives the control actor grant.

The staged SDK includes its same-build transport. With individually built
companions, prepare it once in an unused output directory:

```sh
hew-live-node prepare-transport --compiler /path/to/hew \
  --runtime /path/to/libhew.so --output /path/to/live-node
```

Use ordinary `HEW_NODE_CONFIG` or `HEW_NODE_*` settings for each node's identity,
cluster enrolment, transport and bind address. Set `HEW_LIVE_NODE_MODULE` to the
prepared directory. `HEW_LIVE_RUNTIME` selects the shared runtime when it is not
beside the companion or under the SDK's `lib/hew` directory.

```sh
hew-live-node serve --cache component-cache --allow-peer AUTHORIZED_NODE_ID
hew-live-node attach --endpoint HOST:PORT --node SERVER_NODE_ID
```

`serve` provides reference router services: destination lookup returns the source
name, optionally prefixed by `--destination-prefix`, and emitted deliveries go to
standard error. Embed `network::Server::open` with application `Services`
implementations to provide real destinations and
emission. Closing standard input shuts down the reference server. Its peer grant
owner cancels its observation turn, revokes the export and waits for cleanup.
A new authenticated session for the same authorized identity can reconnect.

`attach` reads and writes JSON lines for a local editor. Its remote operations
use the typed control actor. Every request has `id` and `op`; successful replies
have the same `id`, `ok: true` and `result`. Failures have `ok: false` and `error`.
Standard output contains only protocol replies. The operations are:

- `context`: `source` names the ordinary source entry. This local operation
  returns `version: 1`, the selected SDK `compiler` path and exact generated
  `sources: [{path, source}]`. Editors register them with that compiler's LSP
  as virtual analysis inputs; no generated module is written into the project.
- `build_prepare`: `source` names an ordinary Hew router source file. The SDK
  generates service bindings and lifecycle glue, builds a portable component,
  uploads it and returns its `digest`. This never activates the candidate.
- `prepare`: `component` names an already built portable component.
- `status`: returns the current `epoch` and `digest`; epoch zero means no active
  deployment.
- `replace`: supplies `digest`, the observed `predecessor` epoch and `within_ms`.
  A successful reply includes the new `epoch`, `digest` and retained `accepted`
  count. A stale predecessor leaves the active instance unchanged.
- `route`: supplies an event's `source` and `body` and returns `accepted`.
- `preview`: supplies a prepared `digest`, `within_ms`, and a `preview` object
  containing independent `state`, `input` and ordered service `fixtures`.
  The result reports `accepted`, `needs_input`, fixture consumption and whether
  guest cleanup completed. Production services and the active deployment are
  untouched. Missing or mismatched fixtures request input instead of performing
  the operation.

Source builds use `HEW_LIVE_COMPILER`, `HEW_WASI_SDK` and
`HEW_LIVE_WASM_LIB_DIR`, with corresponding command-line options and SDK-relative
fallbacks. The compiler defaults to the sibling `hew` executable. Generated C
headers travel with the companion. The source builder compiles the original file
using the normal project manifest, installed packages and relative-import resolver.
Only the generated `router_services` module uses the compiler source overlay;
the SDK does not write into the authored source tree or relocate diagnostic paths. The reference contract requires public `create`,
`route`, `snapshot` and `dispose` functions, as shown in
`tests/fixtures/router.hew`. `hew-live-node build --source router.hew --output
router.wasm` runs this build without connecting to a node.

The Linux native QUIC regression, `HEW_WASI_SDK=/path/to/wasi-sdk make
test-live-network`, executes source preparation, activation, state-preserving
replacement, stale-predecessor refusal, client reconnection, cache reuse after
host restart and orderly shutdown with or without a connected peer. The protocol
must not automatically retry an unconfirmed replacement after disconnect; query
status after reconnecting and reconcile its epoch and digest first.

The VS Code live session prepares saved source through this companion and shows
the active and prepared identities separately. Activation is explicit. Observe
associates invocation metadata with the code digest and activation epoch; its
desktop companion exposes status and isolated preview. Ordinary trace collection
records metadata without source event bodies or service responses. Preview uses
operator-supplied state, inputs and fixtures; metadata-only traces do not supply
those responses.

The same network oracle also installs identical component bytes on two running
nodes with different destination services. Select only that witness with
`LIVE_NETWORK_TEST_ARGS=one_portable_source_uses_two_nodes_destination_grants`.

### Measuring authoring and runtime loops

Use a staged SDK from the same revision as the host library:

```sh
HEW_WASI_SDK=/path/to/wasi-sdk make bench-live-loop \
  LIVE_BENCH_SDK=/path/to/live-sdk > live-loop.json
```

The JSON report contains machine and binary identities, every timing sample,
median and nearest-rank p95, and an informative comparison with 250 ms. Defaults
are 20 samples and four retained generations; set `LIVE_BENCH_SAMPLES` and
`LIVE_BENCH_GENERATIONS` to change them. Checks validate each evaluated result,
route and preserved snapshot before accepting its measurement.

First-process eval and warm-filesystem process runs are separate from two-line
edits in one retained `hew tool live` session. OS caches are not flushed. Source
build and component preparation are measured separately; cached activation,
invocation and replacement consume prepared handles without compiling. The
replacement samples alternate two distinct prepared code identities. The
asynchronous comparison includes guest work and scheduling as well as service
dispatch. Replacement pause covers snapshot through commit with no concurrent
ingress. Linux RSS is recorded with prepared generations and active instances
retained, then released; it includes engine and allocator retention and is not
an allocation count or a leak diagnosis. Other platforms report no RSS value.
