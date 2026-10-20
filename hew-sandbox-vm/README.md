# Hew Sandbox VM

`@hew-lang/sandbox-vm` executes verified ownership SIR packages compiled by
`@hew-lang/wasm`. It consumes the shared compiler's decisions for value moves,
borrows, cleanup, checked failures, actor protocols and suspension edges.

Actors, tasks and pipes use resumable frames on a deterministic scheduler.
Timers advance virtual time. Native filesystem and network operations are
refused before execution; other missing operations are reported separately as
`not_implemented`. The loader is the capability admission authority.

## Browser API

```js
import { compileToSandboxBytecode } from "@hew-lang/wasm";
import { runBytecode } from "@hew-lang/sandbox-vm";

const { diagnostics, bytecode } = JSON.parse(compileToSandboxBytecode(source));
if (bytecode) {
  const trace = runBytecode(bytecode, { replay: { step_budget: 100000 } });
}
```

Initialize the Wasm compiler using its generated default export before calling
it. `runProgram(source, stdin)` is also available for existing page bridges;
install `globalThis.compileToSandboxBytecode` first. Its result includes
`stdout`, `exit_code`, `diagnostics`, `status`, `sandbox_rejections`,
`compiler_version` and `hew_version`.

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

`RunOptions.argv` supplies process arguments, including the executable name at
index zero. It defaults to `["main.hew"]` and is copied for each run. For example,
`runBytecode(bytecode, { argv: ["greet", "--", "Zoë", ""] })` supplies those exact
strings to `std.os.args()`. The VM never reads Node or browser process arguments.

Typed serialization executes the shared Rust codecs in `@hew-lang/wasm`.
Pass its initialized `executeCodec` export as `runBytecode`'s `codecBridge`
option. The Node test host loads that package automatically; in a source
checkout, run `make wasm` to build `hew-wasm/pkg` first. An explicit
`HEW_WASM_MODULE` module URL can select another build of the same package.

UTC calendar operations also require the initialized `executeCalendar`
export as `calendarBridge`. Both hosts use the shared Chrono calendar engine;
the bridge preserves integer seconds and nanoseconds exactly.
