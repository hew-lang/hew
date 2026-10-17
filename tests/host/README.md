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
exports. Calls complete on one host thread, including cooperative suspension and
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
cleanup faults. Borrowed arguments retain their owners through the invocation;
mutable arguments require exclusive access. Generated `TYPE_release(&owner,
&error)` also clears the slot and drives the canonical resource cleanup to
completion, reporting any cleanup fault. Releasing a null slot succeeds.
Pointer result and error slots must start null and must not alias inputs or each
other. Scalar output slots are written only on success. Passing a wrong opaque
type, a stale handle or aliased consuming slots violates the C preconditions.

Run the complete C11 and C++17 client at Hew O0 and O2 with:

```sh
make test-host-client
```

The client exercises repeated borrowed calls, consuming shutdown, early release,
failed construction, actor-call faults and combined primary/cleanup faults. The
source negative checks also refuse ambient acquisition from a confined actor
initializer. Linux checks actual descriptor release in addition to the owned
output and error assertions.

The runner uses Clang by default. `HOST_CLIENT_ARGS='--cc gcc --cxx g++'` selects
GCC on Linux. Windows requires the existing MSVC/SDK environment and Clang from
the supported LLVM installation; macOS requires the normal Apple development
tools. The client links the compiler's matching `libhew.a` or `hew.lib`.

For Linux safety evidence, run `make test-host-safety`. It builds the compiler
and runtime with the existing `core-safety-build` target. This instruments the
generated Hew object and C/C++ client as well as the Rust archive, and enables
unsuppressed ASan/LSan. Ordinary host-client success is not sanitizer evidence.
