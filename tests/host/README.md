# C host components

This experimental, same-build static-link client calls compiled Hew functions,
edits a copied JSON configuration and inspects owned errors. It uses the opaque
types and release operations in `hew-cabi/include/hew_host.h`.

```sh
hew build tests/host/config_policy.hew --emit-obj \
  --export-c normalize_label=config_normalize_label -o config_policy.o
```

This produces `config_policy.o` and `config_policy.h`. Select public monomorphic
root-source functions, repeating `--export-c SOURCE=SYMBOL` for each export.
Scalars use fixed-width C types; `bool` uses `uint8_t` values zero and one.
Unit arguments and results need no C slot. Other values have generated opaque
types and typed release functions. Strings retain the existing text contract:
the wrapper borrows a text input, giving a consuming Hew parameter its own
retained value. Empty text results are non-null owners. A logical Hew fault returns
`HEW_LOGICAL_FAULT`, preserving its diagnostic bytes without printing or unwinding
through C. The client can inspect the error after another successful call.

The input is a library without `main`. Other public functions do not become C
exports. Synchronous calls complete on one host thread, including cooperative suspension and
mandatory cleanup. Actor and I/O components require `hew_runtime_new()` before
use. Release every owner before runtime shutdown and before unloading its code.
There is no cross-version binary compatibility promise or independent-runtime
isolation promise.

`copy_component.hew` hosts the same confined order copier used by the supplied
directory capability acceptance. Its trusted launcher functions open directories;
the confined constructor consumes them, and the confined run function borrows the
instance. Generate the component header with:

```sh
hew build tests/host/copy_component.hew --emit-obj \
  --pkg-path tests/core-acceptance/fixtures/construction-capabilities \
  --export-c open_input=orders_open_input \
  --export-c open_output=orders_open_output \
  --export-c create=orders_create --export-c run=orders_run \
  --export-c shutdown=orders_shutdown -o copy_component.o
```

An opaque consuming argument takes a pointer to the C owner slot. The wrapper
clears that slot when handing ownership to Hew, including when construction or
cleanup faults. Keep borrowed owners alive through the invocation;
mutable arguments require exclusive access. Generated `TYPE_release(&owner,
&error)` also clears the slot and drives the canonical resource cleanup to
completion, reporting any cleanup fault. Releasing a null slot succeeds.
Pointer result and error slots must start null and must not alias inputs or each
other. Scalar output slots are written only on success. Passing a wrong opaque
type, a stale handle or aliased consuming slots violates the C preconditions.

## Drive calls from an event loop

Select `--export-c-async SOURCE=SYMBOL` to start a call and poll it without
blocking the host event loop. The selector works for both ordinary functions and
functions that suspend. For example, the copier's `run` function above generates:

```c
typedef struct HewCall_orders_run HewCall_orders_run;
int32_t orders_run_start(HewRuntime *runtime, const HewOwned_CopyInstance *instance,
                        const HewWaker *waker, HewCall_orders_run **out,
                        HewError **error);
int32_t orders_run_poll(HewCall_orders_run *call);
void orders_run_cancel(HewCall_orders_run *call);
int32_t orders_run_result(HewCall_orders_run *call, int64_t *out, HewError **error);
int32_t orders_run_release(HewCall_orders_run **call, HewError **error);
```

The generated opaque value spelling encodes its exact checked source type;
include the generated header instead of copying declarations. `HewWaker`
contains a context pointer and thread-safe `wake`, `retain` and `release`
callbacks. Wake schedules a later poll on the host thread. It must never resume,
cancel or release the call inline. A callback can arrive while polling or after
a runtime operation has finished, so its context and callback code stay live
until the last retained reference is released.

Start returns `HEW_OK` after admission and hands back a call owner. Consuming
owner slots are then empty. Refused admission consumes no inputs. Keep borrowed
owners and mutable slots valid until poll returns `HEW_CALL_READY`; a pending
poll returns `HEW_CALL_PENDING` immediately. Start, poll, cancel, result and
release run on the same host thread, without overlapping or re-entering.

Cancel is an idempotent request. Continue polling until cleanup finishes, then
take the result or owned error once. Success already reached before cancellation
stays successful. Taking a result early or twice returns `HEW_INVALID_STATE`.
Releasing pending work or an untaken result also returns `HEW_INVALID_STATE`,
leaving the call owner and result storage untouched. Release the completed call
after taking its result; its owner slot becomes null. A second release of that
null slot succeeds. Release returned values separately before runtime shutdown.

[`asyncio_orders.py`](../../examples/embedding/asyncio_orders.py) runs the real
supplied-directory copier with Python's asyncio loop. Its wake callback uses
`loop.call_soon_threadsafe`; no host worker thread or periodic polling is needed.
Asyncio cancellation requests native cancellation, drains cleanup and disposes
any owned result before propagating `CancelledError`. Cleanup faults remain
available as the cancellation exception's cause.

Build and run it on Linux with the matching compiler and archive:

```sh
make hew-native
mkdir -p target/async-orders/input target/async-orders/output
printf 'order-41' > target/async-orders/input/orders.csv
target/debug/hew build tests/host/copy_component.hew --emit-obj \
  --pkg-path tests/core-acceptance/fixtures/construction-capabilities \
  --export-c open_input=orders_open_input \
  --export-c open_output=orders_open_output \
  --export-c-async create=orders_create \
  --export-c-async run=orders_run \
  --export-c-async shutdown=orders_shutdown -o target/async-orders/copier.o
clang -shared target/async-orders/copier.o target/debug/libhew.a \
  -lpthread -lm -ldl -lrt -o target/async-orders/copier.so
python3 examples/embedding/asyncio_orders.py target/async-orders/copier.so \
  target/async-orders/input target/async-orders/output
```

This prints `16` and writes `receipt:order-41` to `output/receipt.csv`. If using
`CARGO_TARGET_DIR`, replace the example's `target` paths with that directory.

Run the C11, C++17 and asyncio clients at Hew O0 and O2 with:

```sh
make test-host-client
```

The client exercises repeated borrowed calls, consuming shutdown, early release,
failed construction, actor-call faults and combined primary/cleanup faults. The
source negative checks also refuse ambient acquisition from a confined actor
initializer. Linux checks actual descriptor release in addition to the owned
output and error assertions.
The async cases exercise nonblocking readiness, refused early release, one-shot
results, cancellation cleanup, combined faults and the actual asyncio copier.

The runner uses Clang by default. `HOST_CLIENT_ARGS='--cc gcc --cxx g++'` selects
GCC on Linux. Windows requires the existing MSVC/SDK environment and Clang from
the supported LLVM installation; macOS requires the normal Apple development
tools. The client links the compiler's matching `libhew.a` or `hew.lib`.

For Linux safety evidence, run `make test-host-safety`. It builds the compiler
and runtime with the existing `core-safety-build` target. This instruments the
generated Hew object and C/C++ client as well as the Rust archive, and enables
unsuppressed ASan/LSan for the C/C++ clients. The Python shared-library consumer
runs in the ordinary host target. Ordinary host-client success is not sanitizer evidence.
