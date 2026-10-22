# Hew Sandbox VM

`@hew-lang/sandbox-vm` executes verified ownership SIR packages compiled by
`@hew-lang/wasm`. It consumes the shared compiler's decisions for value moves,
borrows, cleanup, checked failures, actor protocols and suspension edges.

Actors, tasks and pipes use resumable frames on a deterministic scheduler.
Timers advance virtual time. Hosts can supply real origin-private filesystem
(OPFS) directory handles; network and ambient filesystem operations are refused
before execution. Other missing operations are reported separately as
`not_implemented`. The loader is the capability admission authority.

## Browser API

```js
import { compileToSandboxBytecode } from "@hew-lang/wasm";
import { runBytecode } from "@hew-lang/sandbox-vm";

const { diagnostics, bytecode } = JSON.parse(compileToSandboxBytecode(source));
if (bytecode) {
  const trace = await runBytecode(bytecode, {
    replay: { step_budget: 100000 },
  });
}
```

Initialize the Wasm compiler using its generated default export before calling
it. `runProgram(source, stdin)` is also available for existing page bridges;
install `globalThis.compileToSandboxBytecode` first. Both entrypoints return a Promise. The page result includes
`stdout`, `exit_code`, `diagnostics`, `status`, `sandbox_rejections`,
`compiler_version` and `hew_version`.

To grant filesystem authority, pass only the directories the program needs:

```js
const trace = await runBytecode(bytecode, {
  directoryRoots: { read: inputDirectory, write: outputDirectory },
  signal: controller.signal,
});
```

Both roots are optional and default to no authority. The handles must belong to
this origin's OPFS; external picker-backed handles are refused because their
symlink confinement is not standardized. Existing Hew `fs.open_read_dir(".")`
and `fs.open_write_dir(".")` acquire the supplied roots. Relative traversal stays
beneath the current directory owner, including after narrowing with `subdir`.
Children remain usable after closing a parent. Each `file.write` publishes its
bytes before it returns, even while the file owner remains open.

`AbortSignal` requests cancellation through the source's authored cleanup edges.
Filesystem acquisition publishes its owner before observing cancellation, as on
native hosts. An in-progress file operation drains or aborts its browser writer
before cancellation releases its inputs. Pure execution retains the deterministic
virtual scheduler and runs until a host wait or completion. Pass the same options
as the fourth argument to `runProgram(source, stdin, files, options)`; signals are
for direct calls rather than worker message payloads.

A `sandbox_rejected` result includes rejection entries with `category`, `code`,
`capability`, `message` and `span`. Categories are `native_only`,
`not_implemented` and `invalid_package`. Compiler errors, language panics,
checked traps, VM failures and step-budget exhaustion have distinct statuses.
Consumers can offer explicit remote execution for native capabilities.

## Validation

From the repository root:

```sh
make npm-packages
make sandbox-parity
make sandbox-fixtures-record
make sandbox-fixtures-check
```

The [package contract](bytecode/package-v1.md) describes the SIR projection.
Source fixtures are compiled by the public compiler and executed to record
deterministic traces. Tests also compare executable sources with native Hew.

With `hew-wasm/pkg` and `hew-sandbox-vm/dist` built, serve the repository on
localhost and open `hew-sandbox-vm/test/browser/directory-handles.html` to run the
real browser filesystem controls. They compile the ordinary Hew source fixture,
use a temporary OPFS directory, verify disk contents, confinement and cancellation,
then remove their entries. The page accepts a `compiler` module URL query parameter
to reuse another built compiler package.

`RunOptions.argv` supplies process arguments, including the executable name at
index zero. It defaults to `["main.hew"]` and is copied for each run. For example,
`runBytecode(bytecode, { argv: ["greet", "--", "Zoë", ""] })` supplies those exact
strings to `std.os.args()`. The VM never reads Node or browser process arguments.

Typed serialization executes the shared Rust codecs in `@hew-lang/wasm`.
Pass its initialized `executeCodec` export as `runBytecode`'s `codecBridge`
option. The Node test host loads that package automatically; in a source
checkout, run `make wasm` to build `hew-wasm/pkg` first. An explicit
`HEW_WASM_MODULE` module URL can select another build of the same package.

JSON, TOML and YAML values use this same bridge for parsing and formatting.
Their ordinary value operations execute in the VM, including independent copies,
checked access, mutation and cleanup. The bridge preserves full-width JSON
integers, YAML mapping keys and tags, and TOML datetimes using the native engines.

UTC calendar operations also require the initialized `executeCalendar`
export as `calendarBridge`. Both hosts use the shared Chrono calendar engine;
the bridge preserves integer seconds and nanoseconds exactly.
