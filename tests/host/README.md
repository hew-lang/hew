# Synchronous C host client

This experimental, same-build static-link client calls compiled Hew functions,
edits a copied JSON configuration and inspects owned errors. It uses the opaque
types and release operations in `hew-cabi/include/hew_host.h`.

```sh
hew build tests/host/config_policy.hew --emit-obj \
  --export-c normalize_label=config_normalize_label -o config_policy.o
```

This produces `config_policy.o` and `config_policy.h`. The selected source
declaration must be public and monomorphic, accept one shared or consumed string
and return a string. The C wrapper always borrows its input and returns an
independent text owner or an owned error. A consuming Hew parameter receives its
own retained value. Empty results are non-null owners. A logical Hew fault returns
`HEW_LOGICAL_FAULT`, preserving its diagnostic bytes without printing or unwinding
through C. The client can inspect the error after another successful call.

The input is a library without `main`. Selection is explicit: other public
functions do not become C exports. This initial boundary admits scalar/string
execution and direct Hew calls. Actors, asynchronous work, callbacks, resource
handles and other runtime operations are not yet admitted. Calls require one
host thread and no scheduler setup. All owners must be released before unloading
the code. There is no cross-version binary compatibility promise.

Run the complete C11 and C++17 client at Hew O0 and O2 with:

```sh
make test-host-client
```

The runner uses Clang by default. `HOST_CLIENT_ARGS='--cc gcc --cxx g++'` selects
GCC on Linux. Windows requires the existing MSVC/SDK environment and Clang from
the supported LLVM installation; macOS requires the normal Apple development
tools. The client links the compiler's matching `libhew.a` or `hew.lib`.

For Linux safety evidence, run `make test-host-safety`. It builds the compiler
and runtime with the existing `core-safety-build` target. This instruments the
generated Hew object and C/C++ client as well as the Rust archive, and enables
unsuppressed ASan/LSan. Ordinary host-client success is not sanitizer evidence.
