# Interactive Debugging for Hew

**Date:** 2026-03-02
**Status:** Design approved, pending implementation

## Overview

Add full source-level interactive debugging to Hew: DWARF debug info emission in the compiler, and a DAP (Debug Adapter Protocol) integration in the VS Code extension. Supports both GDB and LLDB backends from day one, with Hew-specific actor debugging features.

## Current State

- **vscode-hew** provides syntax highlighting + LSP only. No debugging support.
- **`hew debug` CLI** compiles with debug flags, launches GDB/LLDB, loads `scripts/debug/hew-gdb.py` with Hew-aware pretty-printers and commands (`hew-actors`, `hew-bt`, `hew-break-receive`).
- **No DWARF debug info is emitted.** The codegen creates MLIR `FileLineColLoc` source locations but never calls `llvm::DIBuilder`. The file identifier is hardcoded to `"<unknown>"`. GDB can break on symbol names but can't step through `.hew` source lines.

## Part 1: DWARF Debug Info Emission (Compiler)

### Problem

Three gaps prevent source-level debugging:

1. **Source filename not threaded through.** The CLI has the path, but it's lost before codegen. The MessagePack AST has no `source_path` field. `MLIRGen` hardcodes `fileIdentifier = "<unknown>"`.
2. **Spans are byte offsets, not line/column.** The parser stores `Range<usize>` byte ranges. DWARF needs line:column. A line map must be computed and passed to codegen.
3. **No `DIBuilder` calls.** After `translateModuleToLLVMIR()`, no DWARF metadata is created.

### Approach: LLVM Post-Pass with Line Map

#### 1. Extend the MessagePack AST

In `hew-serialize/src/msgpack.rs`, add to `TypedProgram`:

```rust
struct TypedProgram<'a> {
    // ... existing fields ...
    source_path: Option<String>,       // absolute path to .hew source
    line_map: Option<Vec<usize>>,      // byte offset where each line starts
}
```

The line map is a sorted vector: `line_map[0]` is byte offset of line 1, `line_map[1]` is line 2, etc. Binary search converts any byte offset span to a line:column pair.

Compute the line map in `hew-cli/src/compile.rs` from the source text before serialization.

#### 2. Fix MLIRGen Source Locations

In `hew-codegen`:

- Deserialize `source_path` and `line_map` from the MessagePack input in `codegen_main.cpp`.
- Pass them to `MLIRGen` constructor. Set `fileIdentifier` to the actual source filename.
- Update `MLIRGen::loc(const ast::Span &span)` to convert byte offset to line:column using the line map:

```cpp
mlir::Location MLIRGen::loc(const ast::Span &span) {
    auto [line, col] = byteOffsetToLineCol(span.start);
    return mlir::FileLineColLoc::get(fileIdentifier, line, col);
}
```

#### 3. Emit DWARF via DIBuilder

In `codegen.cpp`, after `translateModuleToLLVMIR()` and when `debug_info` is true:

1. Create `llvm::DIBuilder` with the LLVM module.
2. Create `DICompileUnit` (producer: `"hew"`, language: `DW_LANG_C` — or register a custom DWARF language ID later).
3. Create `DIFile` from the source path.
4. Walk each LLVM function:
   - Create `DISubprogram` with the function's source location (line, column, scope).
   - Attach `DISubprogram` to the function.
   - Walk each instruction: if it has a debug location from MLIR translation, create a `DILocation` and attach it.
5. For `let`/`var` bindings: emit `DILocalVariable` + `llvm.dbg.declare`/`llvm.dbg.value` intrinsics where feasible.
6. Call `DIBuilder::finalize()`.

The existing `skipOptimization` path (triggered by `--debug`) already preserves control flow structure that debuggers need.

#### 4. Multi-file Support

For multi-module programs, each module gets its own `source_path` and `line_map` in the serialized AST. The `DIBuilder` creates one `DIFile` per source file, and `DISubprogram`/`DILocation` reference the correct file.

### Result

Compiled binaries with `--debug` contain full DWARF: source file paths, line tables, function metadata, and optionally local variable info. GDB/LLDB can step through `.hew` source, set breakpoints by line, and show meaningful callstacks.

## Part 2: VS Code DAP Integration (Extension)

### Architecture

```
VS Code UI <-> DAP (JSON over stdio) <-> HewDebugAdapter (TypeScript, in vscode-hew)
                                                |
                                        GDB MI / LLDB MI (stdio)
                                                |
                                        gdb / lldb process
                                                |
                                        compiled hew binary (with DWARF)
```

### Components

#### 1. package.json Debugger Contribution

Declare a `"hew"` debug type with launch/attach configurations:

```jsonc
"contributes": {
  "debuggers": [{
    "type": "hew",
    "label": "Hew Debug",
    "languages": ["hew"],
    "configurationAttributes": {
      "launch": {
        "properties": {
          "program": { "type": "string", "description": "Path to .hew file or compiled binary" },
          "args": { "type": "array", "items": { "type": "string" } },
          "cwd": { "type": "string" },
          "stopOnEntry": { "type": "boolean", "default": false },
          "debuggerBackend": { "type": "string", "enum": ["gdb", "lldb", "auto"], "default": "auto" }
        },
        "required": ["program"]
      }
    },
    "configurationSnippets": [{
      "label": "Hew: Launch",
      "body": {
        "type": "hew",
        "request": "launch",
        "name": "Debug ${1:program}",
        "program": "^\"\\${workspaceFolder}/${2:main.hew}\""
      }
    }]
  }]
}
```

#### 2. HewDebugAdapterFactory

Implements `vscode.DebugAdapterDescriptorFactory`. On `createDebugAdapterDescriptor`:

- If `program` is a `.hew` file, compile it with `hew build --debug` first.
- Resolve debugger backend: `"auto"` → LLDB on macOS, GDB on Linux, user-overridable.
- Return `new vscode.DebugAdapterInlineImplementation(new HewDebugSession(backend))`.

#### 3. HewDebugSession (Core DAP ↔ MI Translation)

Extends `DebugSession` from `@vscode/debugadapter`. Key request mappings:

| DAP Request | GDB MI Command |
|---|---|
| `launch` | `-file-exec-and-symbols`, `-exec-run` |
| `setBreakpoints` | `-break-insert <file>:<line>` |
| `threads` | `-thread-info` |
| `stackTrace` | `-stack-list-frames` |
| `scopes` / `variables` | `-stack-list-variables`, `-var-create`, `-var-evaluate-expression` |
| `continue` / `next` / `stepIn` / `stepOut` | `-exec-continue`, `-exec-next`, `-exec-step`, `-exec-finish` |
| `evaluate` (debug console) | `-data-evaluate-expression` |
| `disconnect` | `-gdb-exit` |

Async MI notifications (`*stopped`, `*running`, `=breakpoint-modified`) are parsed and emitted as DAP events (`StoppedEvent`, `ContinuedEvent`, `BreakpointEvent`).

#### 4. Backend Abstraction

A `MIBackend` interface with `GDBBackend` and `LLDBBackend` implementations:

```typescript
interface MIBackend {
  spawn(executable: string): ChildProcess;
  formatBreakInsert(file: string, line: number): string;
  formatExecRun(args: string[]): string;
  parseStopRecord(record: MIRecord): StopReason;
  loadHelperScripts(): string[];  // hew-gdb.py for GDB, equivalent for LLDB
}
```

Differences between backends are minor — mostly command syntax and async event format. The abstraction keeps the `HewDebugSession` backend-agnostic.

#### 5. Actor-Specific Debugging (Day One)

**Custom DAP requests:**

- `hew/listActors` — sends `hew-actors` GDB command (from `hew-gdb.py`), parses output into structured actor list. Surfaced in a custom VS Code tree view panel ("Hew Actors").
- `hew/breakOnReceive` — sends `hew-break-receive <actor> [method]` via MI. Exposed as context menu on actors in the tree view.

**Callstack filtering:**

- Equivalent of `hew-bt` from the GDB script. Runtime-internal frames (scheduler, message dispatch) are filtered from the call stack by default. A toggle in the UI ("Show Runtime Frames") reveals them.
- Filter by checking frame source paths — frames not from `.hew` files or without DWARF info are hidden.

**Pretty-printed variables:**

- For GDB: load `hew-gdb.py` at session start (already done by `hew debug`). Strings, vecs, hashmaps, actor refs show meaningful values.
- For LLDB: translate the pretty-printers to LLDB Python formatters (or use `SBType` summary providers). Ship as `scripts/debug/hew-lldb.py`.

#### 6. User Experience

- **F5 to debug:** User opens a `.hew` file, presses F5. The extension auto-detects the file, compiles with `--debug`, launches the debugger. No manual compilation step.
- **Breakpoints:** Click the gutter in any `.hew` file. Red dots appear. Breakpoints hit correctly because DWARF maps lines back to source.
- **Variables panel:** Shows `let`/`var` bindings with Hew-aware formatting (strings as text, vecs as arrays, etc.).
- **Actors panel:** Custom tree view listing active actors, their state, and mailbox depth. Right-click to break on receive.
- **Debug console:** Evaluate Hew expressions (via GDB/LLDB expression evaluator on the compiled C-level representations).

## Implementation Phases

### Phase 1: DWARF Emission
1. Add `source_path` + `line_map` to MessagePack AST (hew-serialize, hew-cli)
2. Thread filename into codegen, fix `MLIRGen::loc()` to use real line:column
3. Add `DIBuilder` post-pass in `codegen.cpp`
4. Verify: `hew build --debug foo.hew && lldb ./foo` → can set breakpoint by `.hew` line, step through source

### Phase 2: Basic DAP Adapter
1. Add debugger contribution to vscode-hew `package.json`
2. Implement `HewDebugAdapterFactory` with auto-compilation
3. Implement `HewDebugSession` with core DAP requests (launch, breakpoints, stepping, callstack, variables)
4. GDB + LLDB backend abstraction
5. Verify: F5 on `.hew` file in VS Code → breakpoints, stepping, callstack work

### Phase 3: Actor Debugging
1. Port `hew-gdb.py` commands to DAP custom requests
2. Create LLDB equivalent (`hew-lldb.py`)
3. Build "Hew Actors" tree view panel
4. Callstack filtering (hide runtime frames)
5. Pretty-printer integration for both backends

### Phase 4: Polish
1. Conditional breakpoints, logpoints, hit counts
2. Watch expressions
3. Exception/panic breakpoints
4. Multi-file project debugging (auto-discover entry point)
5. Attach to running process
