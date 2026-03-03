# Interactive Debugging Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add full source-level interactive debugging to Hew — DWARF debug info in the compiler + DAP adapter in VS Code extension with actor-aware debugging.

**Architecture:** Extend the Rust serializer to include source path + line map in the MessagePack AST. Fix the C++ codegen to use real filenames and line:column in MLIR locations. Add a DIBuilder post-pass after LLVM IR translation. Build a TypeScript DAP adapter in vscode-hew that speaks GDB MI / LLDB MI to both backends, with Hew-specific actor debugging from day one.

**Tech Stack:** Rust (hew-serialize, hew-cli), C++ (hew-codegen, LLVM DIBuilder API), TypeScript (vscode-hew, @vscode/debugadapter), GDB MI protocol, LLDB MI protocol.

---

## Phase 1: DWARF Debug Info Emission

### Task 1: Add source_path and line_map to MessagePack AST (Rust)

**Files:**
- Modify: `hew-serialize/src/msgpack.rs:34-49` (TypedProgram struct)
- Modify: `hew-serialize/src/msgpack.rs:66-81` (serialize_to_msgpack function)
- Modify: `hew-serialize/src/msgpack.rs:95-103` (TypedProgramJson struct)
- Modify: `hew-serialize/src/msgpack.rs:121-141` (serialize_to_json function)
- Test: `hew-serialize/src/msgpack.rs:155-343` (existing round-trip tests)

**Step 1: Add fields to TypedProgram**

In `hew-serialize/src/msgpack.rs`, add `source_path` and `line_map` to the `TypedProgram` struct:

```rust
#[derive(Debug, Serialize)]
struct TypedProgram<'a> {
    items: &'a Vec<Spanned<hew_parser::ast::Item>>,
    module_doc: &'a Option<String>,
    expr_types: &'a [ExprTypeEntry],
    handle_types: Vec<String>,
    handle_type_repr: HashMap<String, String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    module_graph: Option<&'a ModuleGraph>,
    /// Absolute path to the source .hew file (for DWARF debug info).
    #[serde(skip_serializing_if = "Option::is_none")]
    source_path: Option<&'a str>,
    /// Byte offset of the start of each line in the source file.
    /// line_map[0] = offset of line 1, line_map[1] = offset of line 2, etc.
    /// Used by codegen to convert byte-offset spans to line:column for DWARF.
    #[serde(skip_serializing_if = "Option::is_none")]
    line_map: Option<&'a [usize]>,
}
```

Do the same for `TypedProgramJson`.

**Step 2: Update serialize_to_msgpack signature and body**

```rust
pub fn serialize_to_msgpack(
    program: &hew_parser::ast::Program,
    expr_types: Vec<ExprTypeEntry>,
    handle_types: Vec<String>,
    handle_type_repr: HashMap<String, String>,
    source_path: Option<&str>,
    line_map: Option<&[usize]>,
) -> Vec<u8> {
    let typed = TypedProgram {
        items: &program.items,
        module_doc: &program.module_doc,
        expr_types: &expr_types,
        handle_types,
        handle_type_repr,
        module_graph: program.module_graph.as_ref(),
        source_path,
        line_map,
    };
    rmp_serde::to_vec_named(&typed).expect("AST MessagePack serialization failed")
}
```

Do the same for `serialize_to_json`.

**Step 3: Fix all call sites**

Every existing call to `serialize_to_msgpack` and `serialize_to_json` must pass the two new arguments. For now pass `None, None` to keep behavior unchanged. Search the workspace for all call sites:

```bash
rg 'serialize_to_msgpack\(' --type rust
rg 'serialize_to_json\(' --type rust
```

Update each call to add `None, None` as the last two arguments.

**Step 4: Run tests to verify nothing broke**

Run: `make test-rust`
Expected: All existing tests pass. The new fields are `skip_serializing_if = "Option::is_none"` so the MessagePack output is unchanged when None.

**Step 5: Commit**

```bash
git add hew-serialize/src/msgpack.rs <all modified call sites>
git commit -m "feat(serialize): add source_path and line_map fields to TypedProgram"
```

---

### Task 2: Compute line map and thread source path through CLI (Rust)

**Files:**
- Modify: `hew-cli/src/compile.rs:58-430` (compile function)

**Step 1: Add a line_map_from_source helper**

Add this function near the top of `compile.rs` (or in a utility module):

```rust
/// Build a line map: a Vec where entry[i] is the byte offset of the start of line (i+1).
/// Line 1 always starts at offset 0.
fn line_map_from_source(source: &str) -> Vec<usize> {
    let mut map = vec![0usize]; // line 1 starts at byte 0
    for (i, byte) in source.bytes().enumerate() {
        if byte == b'\n' {
            map.push(i + 1); // next line starts after the newline
        }
    }
    map
}
```

**Step 2: Compute and pass to serializer**

In `compile()`, after reading the source file and before calling `serialize_to_msgpack`, compute the line map and get the absolute source path:

```rust
let source = std::fs::read_to_string(input)...;  // already exists
let abs_source_path = std::fs::canonicalize(input)
    .map(|p| p.display().to_string())
    .unwrap_or_else(|_| input.to_string());
let line_map = if options.debug {
    Some(line_map_from_source(&source))
} else {
    None
};
```

Then update the `serialize_to_msgpack` call (around line 311):

```rust
let ast_data = hew_serialize::serialize_to_msgpack(
    &program,
    expr_type_map,
    handle_types,
    handle_type_repr,
    if options.debug { Some(abs_source_path.as_str()) } else { None },
    line_map.as_deref(),
);
```

**Step 3: Add a unit test for line_map_from_source**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_map_simple() {
        let map = line_map_from_source("hello\nworld\n");
        assert_eq!(map, vec![0, 6, 12]);
    }

    #[test]
    fn test_line_map_single_line() {
        let map = line_map_from_source("no newline");
        assert_eq!(map, vec![0]);
    }

    #[test]
    fn test_line_map_empty() {
        let map = line_map_from_source("");
        assert_eq!(map, vec![0]);
    }
}
```

**Step 4: Run tests**

Run: `make test-rust`
Expected: All pass including new unit tests.

**Step 5: Commit**

```bash
git add hew-cli/src/compile.rs
git commit -m "feat(cli): compute line map and thread source path to serializer in debug builds"
```

---

### Task 3: Deserialize source_path and line_map in C++ codegen

**Files:**
- Modify: `hew-codegen/src/msgpack_reader.cpp` (parseProgram function)
- Modify: `hew-codegen/include/hew/ast_types.h` (Program struct)
- Modify: `hew-codegen/src/codegen_main.cpp:155-165` (pass to MLIRGen)

**Step 1: Add fields to the C++ Program struct**

In `ast_types.h`, add to the `Program` struct:

```cpp
struct Program {
    std::vector<std::pair<Item, Span>> items;
    // ... existing fields ...

    /// Source file path for DWARF debug info (empty if not provided).
    std::string source_path;
    /// Line map: byte offset of the start of each line. Empty if not provided.
    std::vector<size_t> line_map;
};
```

**Step 2: Deserialize the new fields in msgpack_reader.cpp**

In `parseProgram()`, after unpacking existing fields, add optional extraction of `source_path` and `line_map`:

```cpp
// These fields are optional — old serializations won't have them.
if (auto it = map.find("source_path"); it != map.end()) {
    if (it->second.type == msgpack::type::STR) {
        program.source_path = it->second.as<std::string>();
    }
}
if (auto it = map.find("line_map"); it != map.end()) {
    if (it->second.type == msgpack::type::ARRAY) {
        auto &arr = it->second.via.array;
        program.line_map.reserve(arr.size);
        for (uint32_t i = 0; i < arr.size; ++i) {
            program.line_map.push_back(arr.ptr[i].as<size_t>());
        }
    }
}
```

The exact deserialization pattern should follow how the existing fields are read — check `parseProgram()` for the map-based field extraction pattern used for `expr_types`, `handle_types`, etc. and mirror it.

**Step 3: Pass source info to MLIRGen**

In `codegen_main.cpp`, after parsing and before MLIRGen construction (around line 160):

```cpp
hew::MLIRGen mlirGen(context, opts.target_triple,
                      program.source_path, program.line_map);
```

This requires updating the MLIRGen constructor (done in the next task).

**Step 4: Build to verify compilation**

Run: `make`
Expected: Build succeeds (MLIRGen constructor change is done in the next task, so this may need to be done together — or temporarily keep the old constructor and add the new one in Task 4).

**Step 5: Commit**

```bash
git add hew-codegen/
git commit -m "feat(codegen): deserialize source_path and line_map from MessagePack AST"
```

---

### Task 4: Fix MLIRGen source locations to use real file + line:column

**Files:**
- Modify: `hew-codegen/include/hew/mlir/MLIRGen.h:53-746` (class declaration)
- Modify: `hew-codegen/src/mlir/MLIRGen.cpp:104-110` (constructor)
- Modify: `hew-codegen/src/mlir/MLIRGen.cpp:157-159` (loc method)

**Step 1: Add line map and byte-to-line-col converter to MLIRGen**

In `MLIRGen.h`, add private members:

```cpp
private:
    /// Line map from serialized AST: byte offset of each line start.
    std::vector<size_t> lineMap;

    /// Convert a byte offset to (line, column), both 1-based.
    /// Returns (0, 0) if lineMap is empty (no debug info).
    std::pair<unsigned, unsigned> byteOffsetToLineCol(size_t offset) const;
```

**Step 2: Update constructor to accept and store source path + line map**

In `MLIRGen.cpp`, update the constructor:

```cpp
MLIRGen::MLIRGen(mlir::MLIRContext &context, const std::string &targetTriple,
                 const std::string &sourcePath, const std::vector<size_t> &lineMap)
    : context(context), builder(&context), targetTriple(targetTriple),
      currentLoc(builder.getUnknownLoc()), lineMap(lineMap) {
  fileIdentifier = builder.getStringAttr(
      sourcePath.empty() ? "<unknown>" : sourcePath);
  isWasm32_ = targetTriple.find("wasm32") != std::string::npos;
  cachedSizeType_ = mlir::IntegerType::get(&context, isWasm32_ ? 32 : 64);
}
```

**Step 3: Implement byteOffsetToLineCol**

```cpp
std::pair<unsigned, unsigned> MLIRGen::byteOffsetToLineCol(size_t offset) const {
    if (lineMap.empty()) return {0, 0};
    // Binary search: find the last line whose start offset <= offset
    auto it = std::upper_bound(lineMap.begin(), lineMap.end(), offset);
    if (it == lineMap.begin()) return {1, static_cast<unsigned>(offset + 1)};
    --it;
    unsigned line = static_cast<unsigned>(std::distance(lineMap.begin(), it)) + 1;
    unsigned col = static_cast<unsigned>(offset - *it) + 1;
    return {line, col};
}
```

**Step 4: Update loc() to use real line:column**

```cpp
mlir::Location MLIRGen::loc(const ast::Span &span) {
    auto [line, col] = byteOffsetToLineCol(span.start);
    if (line == 0) {
        // No line map — fall back to byte offset as before
        return mlir::FileLineColLoc::get(
            fileIdentifier, static_cast<unsigned>(span.start), 0);
    }
    return mlir::FileLineColLoc::get(fileIdentifier, line, col);
}
```

**Step 5: Update MLIRGen.h constructor declaration**

```cpp
explicit MLIRGen(mlir::MLIRContext &context,
                 const std::string &targetTriple = "",
                 const std::string &sourcePath = "",
                 const std::vector<size_t> &lineMap = {});
```

**Step 6: Build and test**

Run: `make`
Expected: Compiles. MLIR output (via `hew build --emit-mlir --debug test.hew`) now shows real filename and line:column in locations instead of `<unknown>:byteoffset:0`.

Run: `make test`
Expected: All tests pass.

**Step 7: Commit**

```bash
git add hew-codegen/
git commit -m "feat(codegen): use real source file and line:column in MLIR locations"
```

---

### Task 5: Emit DWARF debug info via DIBuilder

**Files:**
- Modify: `hew-codegen/src/codegen.cpp:4355-4441` (lowerToLLVMIR function)
- Modify: `hew-codegen/include/hew/codegen.h` (Codegen class — add members)
- Create: `hew-codegen/src/debug_info.cpp` (DIBuilder wrapper)
- Create: `hew-codegen/include/hew/debug_info.h` (header)
- Modify: `hew-codegen/CMakeLists.txt` (add new source file)

This is the largest task. It adds DWARF metadata after MLIR→LLVM translation.

**Step 1: Create debug_info.h**

```cpp
#pragma once

#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/Module.h"
#include <string>
#include <vector>

namespace hew {

/// Attach DWARF debug info to an LLVM module produced by MLIR translation.
/// Walks all functions and instructions, creating DISubprogram and DILocation
/// from the MLIR source locations preserved during translation.
///
/// \param module       The LLVM module to annotate.
/// \param sourcePath   Absolute path to the .hew source file.
/// \param lineMap      Byte offset of each line start (for span→line:col conversion).
void emitDebugInfo(llvm::Module &module, const std::string &sourcePath,
                   const std::vector<size_t> &lineMap);

} // namespace hew
```

**Step 2: Create debug_info.cpp**

```cpp
#include "hew/debug_info.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include <algorithm>

namespace hew {

void emitDebugInfo(llvm::Module &module, const std::string &sourcePath,
                   const std::vector<size_t> &lineMap) {
    if (sourcePath.empty()) return;

    auto &ctx = module.getContext();
    llvm::DIBuilder dib(module);

    // Split source path into directory + filename
    auto lastSlash = sourcePath.rfind('/');
    std::string dir, file;
    if (lastSlash != std::string::npos) {
        dir = sourcePath.substr(0, lastSlash);
        file = sourcePath.substr(lastSlash + 1);
    } else {
        dir = ".";
        file = sourcePath;
    }

    auto *diFile = dib.createFile(file, dir);
    auto *cu = dib.createCompileUnit(
        llvm::dwarf::DW_LANG_C,  // closest standard language ID
        diFile,
        "hew",                   // producer
        /*isOptimized=*/false,
        /*Flags=*/"",
        /*RV=*/0);

    // Create a basic subroutine type (void function — we refine later)
    auto *voidTy = dib.createSubroutineType(dib.getOrCreateTypeArray({}));

    for (auto &fn : module) {
        if (fn.isDeclaration()) continue;

        // Try to find the source location from the first instruction with debug loc
        unsigned fnLine = 0;
        for (auto &bb : fn) {
            for (auto &inst : bb) {
                if (auto dl = inst.getDebugLoc()) {
                    fnLine = dl.getLine();
                    break;
                }
            }
            if (fnLine) break;
        }

        auto *sp = dib.createFunction(
            diFile,             // scope
            fn.getName(),       // name
            fn.getName(),       // linkage name
            diFile,             // file
            fnLine,             // line number
            voidTy,             // type (simplified)
            fnLine,             // scope line
            llvm::DINode::FlagPrototyped,
            llvm::DISubprogram::SPFlagDefinition);
        fn.setSubprogram(sp);

        // Attach DILocation to every instruction that has an MLIR-derived debug loc
        for (auto &bb : fn) {
            for (auto &inst : bb) {
                if (auto dl = inst.getDebugLoc()) {
                    // Re-create the location under the new subprogram scope
                    auto *newDL = llvm::DILocation::get(
                        ctx, dl.getLine(), dl.getCol(), sp);
                    inst.setDebugLoc(llvm::DebugLoc(newDL));
                } else {
                    // Instructions without source locations get the function's location
                    auto *defaultDL = llvm::DILocation::get(ctx, fnLine, 0, sp);
                    inst.setDebugLoc(llvm::DebugLoc(defaultDL));
                }
            }
        }
    }

    dib.finalize();

    // Add module flags for DWARF version and debug info version
    module.addModuleFlag(llvm::Module::Warning, "Dwarf Version", 4);
    module.addModuleFlag(llvm::Module::Warning, "Debug Info Version",
                         llvm::DEBUG_METADATA_VERSION);
}

} // namespace hew
```

**Step 3: Add debug_info.cpp to CMakeLists.txt**

Find the `add_library` or source file list in `hew-codegen/CMakeLists.txt` and add `src/debug_info.cpp`.

**Step 4: Call emitDebugInfo after LLVM translation**

In `codegen.cpp`, in `lowerToLLVMIR()`, after the `translateModuleToLLVMIR` call (around line 4416) and before the optimization pass (line 4422):

```cpp
auto llvmModule = mlir::translateModuleToLLVMIR(module, llvmContext);
if (!llvmModule) { ... }

// Emit DWARF debug info if in debug mode
if (skipOptimization && !sourcePath_.empty()) {
    hew::emitDebugInfo(*llvmModule, sourcePath_, lineMap_);
}
```

This requires `sourcePath_` and `lineMap_` to be stored on the `Codegen` class. Add them as members and set them from the `CodegenOptions`:

In `codegen.h`:
```cpp
class Codegen {
    // ... existing ...
    std::string sourcePath_;
    std::vector<size_t> lineMap_;
};
```

In `CodegenOptions` (wherever it's defined):
```cpp
struct CodegenOptions {
    // ... existing ...
    std::string source_path;
    std::vector<size_t> line_map;
};
```

Thread the source_path and line_map from `codegen_main.cpp` through `CodegenOptions` to `Codegen`.

**Step 5: Build**

Run: `make`
Expected: Compiles successfully. The `#include` paths for LLVM DIBuilder headers should already be available since the project links against LLVM.

**Step 6: Verify DWARF output**

Create a test file `test_debug.hew`:
```hew
fn main() {
    let x = 42
    print(x)
}
```

```bash
hew build --debug test_debug.hew -o test_debug
llvm-dwarfdump test_debug | head -40
```

Expected: See `DW_TAG_compile_unit` with producer `"hew"`, `DW_TAG_subprogram` for `main`, line numbers referencing `test_debug.hew`.

```bash
lldb test_debug
(lldb) breakpoint set --file test_debug.hew --line 2
(lldb) run
```

Expected: Breakpoint hits at line 2. `bt` shows `test_debug.hew:2`.

**Step 7: Commit**

```bash
git add hew-codegen/
git commit -m "feat(codegen): emit DWARF debug info via DIBuilder for source-level debugging"
```

---

### Task 6: Verify end-to-end DWARF with GDB and LLDB

**Files:**
- Create: `tests/debug/test_breakpoint.hew` (simple test program)
- Create: `tests/debug/run_debug_test.sh` (automated verification script)

**Step 1: Create a test Hew program**

```hew
fn add(a: Int, b: Int) -> Int {
    return a + b
}

fn main() {
    let x = 10
    let y = 20
    let result = add(x, y)
    print(result)
}
```

**Step 2: Create an automated verification script**

```bash
#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
HEW="${SCRIPT_DIR}/../../target/debug/hew"
TEST_FILE="${SCRIPT_DIR}/test_breakpoint.hew"

echo "=== Building with debug info ==="
"$HEW" build --debug "$TEST_FILE" -o /tmp/hew_debug_test

echo "=== Checking DWARF info ==="
if command -v llvm-dwarfdump &>/dev/null; then
    llvm-dwarfdump /tmp/hew_debug_test | grep -q "DW_TAG_compile_unit" || {
        echo "FAIL: No DW_TAG_compile_unit found"; exit 1
    }
    llvm-dwarfdump /tmp/hew_debug_test | grep -q "test_breakpoint.hew" || {
        echo "FAIL: Source filename not in DWARF"; exit 1
    }
    echo "PASS: DWARF info contains compile unit and source filename"
else
    echo "SKIP: llvm-dwarfdump not found"
fi

echo "=== Testing GDB breakpoint (non-interactive) ==="
if command -v gdb &>/dev/null; then
    GDB_OUTPUT=$(gdb -batch -ex "break test_breakpoint.hew:6" -ex "run" -ex "bt" /tmp/hew_debug_test 2>&1)
    if echo "$GDB_OUTPUT" | grep -q "test_breakpoint.hew"; then
        echo "PASS: GDB backtrace shows .hew source file"
    else
        echo "FAIL: GDB backtrace missing .hew source"
        echo "$GDB_OUTPUT"
        exit 1
    fi
fi

echo "=== All debug info tests passed ==="
```

**Step 3: Run the verification**

```bash
chmod +x tests/debug/run_debug_test.sh
make && tests/debug/run_debug_test.sh
```

**Step 4: Commit**

```bash
git add tests/debug/
git commit -m "test: add DWARF debug info verification tests"
```

---

## Phase 2: VS Code DAP Adapter

### Task 7: Add npm dependencies and debugger contribution to vscode-hew

**Files:**
- Modify: `~/projects/hew-lang/vscode-hew/package.json`

**Step 1: Add DAP dependencies**

Add to `dependencies` in package.json:

```json
"dependencies": {
    "vscode-languageclient": "^9.0.1",
    "@vscode/debugadapter": "^1.68.0"
}
```

**Step 2: Add debugger contribution**

Add to the `contributes` section:

```json
"debuggers": [{
    "type": "hew",
    "label": "Hew Debug",
    "languages": ["hew"],
    "configurationAttributes": {
        "launch": {
            "required": ["program"],
            "properties": {
                "program": {
                    "type": "string",
                    "description": "Path to .hew source file or compiled binary to debug"
                },
                "args": {
                    "type": "array",
                    "items": { "type": "string" },
                    "default": [],
                    "description": "Command-line arguments for the program"
                },
                "cwd": {
                    "type": "string",
                    "description": "Working directory for the program"
                },
                "stopOnEntry": {
                    "type": "boolean",
                    "default": false,
                    "description": "Stop at entry point of the program"
                },
                "debuggerBackend": {
                    "type": "string",
                    "enum": ["gdb", "lldb", "auto"],
                    "default": "auto",
                    "description": "Debugger backend: gdb, lldb, or auto (LLDB on macOS, GDB on Linux)"
                }
            }
        }
    },
    "configurationSnippets": [{
        "label": "Hew: Launch Program",
        "description": "Build and debug a Hew program",
        "body": {
            "type": "hew",
            "request": "launch",
            "name": "Debug ${1:main.hew}",
            "program": "^\"\\${workspaceFolder}/${2:main.hew}\""
        }
    }],
    "initialConfigurations": [{
        "type": "hew",
        "request": "launch",
        "name": "Debug Hew Program",
        "program": "${workspaceFolder}/main.hew"
    }]
}]
```

Also add a `hew.debugger.hewPath` configuration for finding the hew binary:

```json
"hew.debugger.hewPath": {
    "type": "string",
    "default": "",
    "description": "Path to the hew compiler binary for debug builds. If empty, uses the same search as hew.formatterPath."
}
```

**Step 3: Install dependencies**

```bash
cd ~/projects/hew-lang/vscode-hew
npm install @vscode/debugadapter
```

**Step 4: Build to verify package.json is valid**

```bash
npm run build:dev
```

**Step 5: Commit**

```bash
git add package.json package-lock.json
git commit -m "feat: add DAP debugger contribution and @vscode/debugadapter dependency"
```

---

### Task 8: Implement MI parser (GDB Machine Interface protocol)

**Files:**
- Create: `~/projects/hew-lang/vscode-hew/src/debug/mi-parser.ts`
- Create: `~/projects/hew-lang/vscode-hew/tests/mi-parser.test.ts`

The MI (Machine Interface) protocol is a structured text protocol used by both GDB and LLDB. Outputs look like:

```
*stopped,reason="breakpoint-hit",bkptno="1",frame={func="main",file="test.hew",line="5"}
^done,threads=[{id="1",target-id="Thread 0x7f..."}]
~"Breakpoint 1 at 0x..."
```

**Step 1: Write tests for MI parsing**

```typescript
// tests/mi-parser.test.ts
import { describe, it, expect } from 'vitest';
import { parseMIOutput, MIRecord } from '../src/debug/mi-parser';

describe('MI Parser', () => {
    it('parses result record', () => {
        const record = parseMIOutput('^done,bkpt={number="1",file="test.hew",line="5"}');
        expect(record.type).toBe('result');
        expect(record.class).toBe('done');
        expect(record.data.bkpt.number).toBe('1');
        expect(record.data.bkpt.file).toBe('test.hew');
    });

    it('parses async stopped record', () => {
        const record = parseMIOutput('*stopped,reason="breakpoint-hit",frame={func="main",file="test.hew",line="3"}');
        expect(record.type).toBe('exec');
        expect(record.class).toBe('stopped');
        expect(record.data.reason).toBe('breakpoint-hit');
        expect(record.data.frame.func).toBe('main');
    });

    it('parses thread info', () => {
        const record = parseMIOutput('^done,threads=[{id="1",name="main"}]');
        expect(record.data.threads).toHaveLength(1);
        expect(record.data.threads[0].id).toBe('1');
    });

    it('parses console output', () => {
        const record = parseMIOutput('~"Hello world\\n"');
        expect(record.type).toBe('console');
        expect(record.data).toBe('Hello world\n');
    });

    it('handles token prefixed records', () => {
        const record = parseMIOutput('42^done');
        expect(record.token).toBe(42);
        expect(record.type).toBe('result');
        expect(record.class).toBe('done');
    });
});
```

**Step 2: Run tests to verify they fail**

```bash
cd ~/projects/hew-lang/vscode-hew
npx vitest run tests/mi-parser.test.ts
```

Expected: FAIL — module not found.

**Step 3: Implement MI parser**

```typescript
// src/debug/mi-parser.ts

export interface MIRecord {
    token?: number;
    type: 'result' | 'exec' | 'status' | 'notify' | 'console' | 'target' | 'log';
    class?: string;
    data: any;
}

const RECORD_PREFIXES: Record<string, MIRecord['type']> = {
    '^': 'result',
    '*': 'exec',
    '+': 'status',
    '=': 'notify',
    '~': 'console',
    '@': 'target',
    '&': 'log',
};

export function parseMIOutput(line: string): MIRecord {
    let pos = 0;
    let token: number | undefined;

    // Parse optional token (digits at start)
    while (pos < line.length && line[pos] >= '0' && line[pos] <= '9') pos++;
    if (pos > 0) token = parseInt(line.substring(0, pos), 10);

    const prefix = line[pos];
    const type = RECORD_PREFIXES[prefix];
    if (!type) return { type: 'console', data: line };

    pos++; // skip prefix char

    // Console/target/log output: rest is a C-string
    if (type === 'console' || type === 'target' || type === 'log') {
        return { token, type, data: parseCString(line, pos) };
    }

    // Result/exec/status/notify: class,key=value,...
    const commaIdx = line.indexOf(',', pos);
    let cls: string;
    let rest: string;
    if (commaIdx === -1) {
        cls = line.substring(pos);
        rest = '';
    } else {
        cls = line.substring(pos, commaIdx);
        rest = line.substring(commaIdx + 1);
    }

    return { token, type, class: cls, data: rest ? parseMIValue(rest) : {} };
}

function parseCString(line: string, pos: number): string {
    if (line[pos] !== '"') return line.substring(pos);
    let result = '';
    pos++; // skip opening quote
    while (pos < line.length && line[pos] !== '"') {
        if (line[pos] === '\\' && pos + 1 < line.length) {
            pos++;
            switch (line[pos]) {
                case 'n': result += '\n'; break;
                case 't': result += '\t'; break;
                case '\\': result += '\\'; break;
                case '"': result += '"'; break;
                default: result += line[pos]; break;
            }
        } else {
            result += line[pos];
        }
        pos++;
    }
    return result;
}

// Parse MI key=value pairs into an object
function parseMIValue(text: string): any {
    const result: any = {};
    let pos = 0;

    while (pos < text.length) {
        // Skip whitespace and commas
        while (pos < text.length && (text[pos] === ',' || text[pos] === ' ')) pos++;
        if (pos >= text.length) break;

        // Read key
        const eqIdx = text.indexOf('=', pos);
        if (eqIdx === -1) break;
        const key = text.substring(pos, eqIdx);
        pos = eqIdx + 1;

        // Read value
        const [value, newPos] = readValue(text, pos);
        result[key] = value;
        pos = newPos;
    }

    return result;
}

function readValue(text: string, pos: number): [any, number] {
    if (pos >= text.length) return ['', pos];

    if (text[pos] === '"') {
        // C-string value
        let result = '';
        pos++; // skip opening quote
        while (pos < text.length && text[pos] !== '"') {
            if (text[pos] === '\\' && pos + 1 < text.length) {
                pos++;
                switch (text[pos]) {
                    case 'n': result += '\n'; break;
                    case 't': result += '\t'; break;
                    case '\\': result += '\\'; break;
                    case '"': result += '"'; break;
                    default: result += text[pos]; break;
                }
            } else {
                result += text[pos];
            }
            pos++;
        }
        if (pos < text.length) pos++; // skip closing quote
        return [result, pos];
    }

    if (text[pos] === '{') {
        // Tuple/object
        pos++; // skip {
        const obj: any = {};
        while (pos < text.length && text[pos] !== '}') {
            while (pos < text.length && (text[pos] === ',' || text[pos] === ' ')) pos++;
            if (text[pos] === '}') break;
            const eqIdx = text.indexOf('=', pos);
            if (eqIdx === -1) break;
            const key = text.substring(pos, eqIdx);
            pos = eqIdx + 1;
            const [value, newPos] = readValue(text, pos);
            obj[key] = value;
            pos = newPos;
        }
        if (pos < text.length) pos++; // skip }
        return [obj, pos];
    }

    if (text[pos] === '[') {
        // List
        pos++; // skip [
        const arr: any[] = [];
        while (pos < text.length && text[pos] !== ']') {
            while (pos < text.length && (text[pos] === ',' || text[pos] === ' ')) pos++;
            if (text[pos] === ']') break;
            const [value, newPos] = readValue(text, pos);
            arr.push(value);
            pos = newPos;
        }
        if (pos < text.length) pos++; // skip ]
        return [arr, pos];
    }

    // Bare word (shouldn't happen in well-formed MI, but handle gracefully)
    const end = text.indexOf(',', pos);
    if (end === -1) return [text.substring(pos), text.length];
    return [text.substring(pos, end), end];
}
```

**Step 4: Run tests**

```bash
npx vitest run tests/mi-parser.test.ts
```

Expected: All pass.

**Step 5: Commit**

```bash
git add src/debug/mi-parser.ts tests/mi-parser.test.ts
git commit -m "feat: add GDB/LLDB Machine Interface (MI) output parser"
```

---

### Task 9: Implement MI backend abstraction (GDB + LLDB)

**Files:**
- Create: `~/projects/hew-lang/vscode-hew/src/debug/mi-backend.ts`
- Create: `~/projects/hew-lang/vscode-hew/src/debug/mi-session.ts`

**Step 1: Create the backend interface and implementations**

```typescript
// src/debug/mi-backend.ts
import { ChildProcess, spawn } from 'child_process';

export interface MIBackend {
    name: string;
    spawn(extraArgs?: string[]): ChildProcess;
    execAndSymbolsCmd(executable: string): string;
    execRunCmd(args: string[]): string;
    breakInsertCmd(file: string, line: number): string;
    threadInfoCmd(): string;
    stackListFramesCmd(threadId: number): string;
    stackListVariablesCmd(threadId: number, frameId: number): string;
    continueCmd(threadId?: number): string;
    nextCmd(threadId?: number): string;
    stepCmd(threadId?: number): string;
    finishCmd(threadId?: number): string;
    evalCmd(expr: string): string;
    exitCmd(): string;
    loadHelperScript(): string | undefined;
}

export class GDBBackend implements MIBackend {
    name = 'gdb';
    private helperScriptPath: string | undefined;

    constructor(private gdbPath: string = 'gdb', helperScriptPath?: string) {
        this.helperScriptPath = helperScriptPath;
    }

    spawn(extraArgs: string[] = []): ChildProcess {
        const args = ['--interpreter=mi3', '--quiet', ...extraArgs];
        return spawn(this.gdbPath, args, { stdio: ['pipe', 'pipe', 'pipe'] });
    }

    execAndSymbolsCmd(executable: string) { return `-file-exec-and-symbols ${executable}`; }
    execRunCmd(args: string[]) { return args.length ? `-exec-run -- ${args.join(' ')}` : '-exec-run'; }
    breakInsertCmd(file: string, line: number) { return `-break-insert ${file}:${line}`; }
    threadInfoCmd() { return '-thread-info'; }
    stackListFramesCmd(threadId: number) { return `-stack-list-frames --thread ${threadId}`; }
    stackListVariablesCmd(threadId: number, frameId: number) { return `-stack-list-variables --thread ${threadId} --frame ${frameId} --all-values`; }
    continueCmd(threadId?: number) { return threadId ? `-exec-continue --thread ${threadId}` : '-exec-continue'; }
    nextCmd(threadId?: number) { return threadId ? `-exec-next --thread ${threadId}` : '-exec-next'; }
    stepCmd(threadId?: number) { return threadId ? `-exec-step --thread ${threadId}` : '-exec-step'; }
    finishCmd(threadId?: number) { return threadId ? `-exec-finish --thread ${threadId}` : '-exec-finish'; }
    evalCmd(expr: string) { return `-data-evaluate-expression "${expr}"`; }
    exitCmd() { return '-gdb-exit'; }
    loadHelperScript() { return this.helperScriptPath ? `-interpreter-exec console "source ${this.helperScriptPath}"` : undefined; }
}

export class LLDBBackend implements MIBackend {
    name = 'lldb';

    constructor(private lldbMiPath: string = 'lldb-mi') {}

    spawn(extraArgs: string[] = []): ChildProcess {
        return spawn(this.lldbMiPath, extraArgs, { stdio: ['pipe', 'pipe', 'pipe'] });
    }

    execAndSymbolsCmd(executable: string) { return `-file-exec-and-symbols ${executable}`; }
    execRunCmd(args: string[]) { return args.length ? `-exec-run -- ${args.join(' ')}` : '-exec-run'; }
    breakInsertCmd(file: string, line: number) { return `-break-insert ${file}:${line}`; }
    threadInfoCmd() { return '-thread-info'; }
    stackListFramesCmd(threadId: number) { return `-stack-list-frames --thread ${threadId}`; }
    stackListVariablesCmd(threadId: number, frameId: number) { return `-stack-list-variables --thread ${threadId} --frame ${frameId} --all-values`; }
    continueCmd(threadId?: number) { return threadId ? `-exec-continue --thread ${threadId}` : '-exec-continue'; }
    nextCmd(threadId?: number) { return threadId ? `-exec-next --thread ${threadId}` : '-exec-next'; }
    stepCmd(threadId?: number) { return threadId ? `-exec-step --thread ${threadId}` : '-exec-step'; }
    finishCmd(threadId?: number) { return threadId ? `-exec-finish --thread ${threadId}` : '-exec-finish'; }
    evalCmd(expr: string) { return `-data-evaluate-expression "${expr}"`; }
    exitCmd() { return '-gdb-exit'; }
    loadHelperScript() { return undefined; } // LLDB helpers loaded differently
}

export function detectBackend(preference: string): MIBackend {
    if (preference === 'gdb') return new GDBBackend();
    if (preference === 'lldb') return new LLDBBackend();
    // auto: LLDB on macOS, GDB on Linux
    if (process.platform === 'darwin') return new LLDBBackend();
    return new GDBBackend();
}
```

**Step 2: Create MI session manager**

```typescript
// src/debug/mi-session.ts
import { ChildProcess } from 'child_process';
import { EventEmitter } from 'events';
import { MIBackend } from './mi-backend';
import { parseMIOutput, MIRecord } from './mi-parser';

export class MISession extends EventEmitter {
    private process: ChildProcess | undefined;
    private buffer = '';
    private tokenCounter = 1;
    private pendingCommands = new Map<number, {
        resolve: (record: MIRecord) => void;
        reject: (err: Error) => void;
    }>();

    constructor(private backend: MIBackend) {
        super();
    }

    start(): void {
        this.process = this.backend.spawn();

        this.process.stdout?.on('data', (data: Buffer) => {
            this.buffer += data.toString();
            this.processBuffer();
        });

        this.process.stderr?.on('data', (data: Buffer) => {
            this.emit('log', data.toString());
        });

        this.process.on('exit', (code) => {
            this.emit('exit', code);
        });
    }

    async sendCommand(command: string): Promise<MIRecord> {
        if (!this.process?.stdin) throw new Error('MI process not started');

        const token = this.tokenCounter++;
        const line = `${token}${command}\n`;

        return new Promise((resolve, reject) => {
            this.pendingCommands.set(token, { resolve, reject });
            this.process!.stdin!.write(line);
        });
    }

    kill(): void {
        this.process?.kill();
    }

    private processBuffer(): void {
        const lines = this.buffer.split('\n');
        this.buffer = lines.pop() || ''; // keep incomplete last line

        for (const line of lines) {
            const trimmed = line.trim();
            if (!trimmed || trimmed === '(gdb)' || trimmed === '(lldb)') continue;

            const record = parseMIOutput(trimmed);

            // If it has a token, resolve the pending command
            if (record.token !== undefined && this.pendingCommands.has(record.token)) {
                const pending = this.pendingCommands.get(record.token)!;
                this.pendingCommands.delete(record.token);
                if (record.class === 'error') {
                    pending.reject(new Error(record.data?.msg || 'MI command failed'));
                } else {
                    pending.resolve(record);
                }
            }

            // Emit async notifications
            if (record.type === 'exec') {
                this.emit('exec', record);
            } else if (record.type === 'notify') {
                this.emit('notify', record);
            } else if (record.type === 'console') {
                this.emit('console', record.data);
            }
        }
    }
}
```

**Step 3: Build**

```bash
npm run build:dev
```

Expected: Compiles without errors.

**Step 4: Commit**

```bash
git add src/debug/mi-backend.ts src/debug/mi-session.ts
git commit -m "feat: add MI backend abstraction with GDB and LLDB implementations"
```

---

### Task 10: Implement HewDebugSession (DAP ↔ MI translation)

**Files:**
- Create: `~/projects/hew-lang/vscode-hew/src/debug/hew-debug-session.ts`

This is the core DAP adapter — translates VS Code debug protocol requests into MI commands.

**Step 1: Implement the debug session**

```typescript
// src/debug/hew-debug-session.ts
import {
    DebugSession,
    InitializedEvent,
    StoppedEvent,
    TerminatedEvent,
    ThreadEvent,
    OutputEvent,
    Thread,
    StackFrame,
    Scope,
    Source,
    Variable,
} from '@vscode/debugadapter';
import { DebugProtocol } from '@vscode/debugprotocol';
import { MISession } from './mi-session';
import { MIBackend, detectBackend, GDBBackend } from './mi-backend';
import { execFileSync } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';

interface HewLaunchArgs extends DebugProtocol.LaunchRequestArguments {
    program: string;
    args?: string[];
    cwd?: string;
    stopOnEntry?: boolean;
    debuggerBackend?: 'gdb' | 'lldb' | 'auto';
}

// Runtime frame filtering patterns — hew runtime internals
const RUNTIME_FRAME_PATTERNS = [
    /^hew_runtime_/,
    /^__pthread/,
    /^__libc/,
    /^std::rt::/,
    /^core::ops::/,
    /^tokio::/,
    /^__GI_/,
    /^_start$/,
    /^__libc_start/,
];

export class HewDebugSession extends DebugSession {
    private miSession: MISession | undefined;
    private backend: MIBackend | undefined;
    private compiledBinary: string | undefined;
    private showRuntimeFrames = false;

    // Variable reference tracking for scopes/variables
    private variableHandles = new Map<number, { threadId: number; frameId: number }>();
    private nextVarRef = 1;

    constructor() {
        super();
        this.setDebuggerLinesStartAt1(true);
        this.setDebuggerColumnsStartAt1(true);
    }

    protected initializeRequest(response: DebugProtocol.InitializeResponse): void {
        response.body = response.body || {};
        response.body.supportsConfigurationDoneRequest = true;
        response.body.supportsFunctionBreakpoints = true;
        response.body.supportsEvaluateForHovers = true;
        this.sendResponse(response);
        this.sendEvent(new InitializedEvent());
    }

    protected async launchRequest(response: DebugProtocol.LaunchResponse, args: HewLaunchArgs): Promise<void> {
        try {
            // Step 1: Compile if program is a .hew file
            let executable = args.program;
            if (executable.endsWith('.hew')) {
                executable = await this.compileProgramDebug(executable, args.cwd);
            }
            this.compiledBinary = executable;

            // Step 2: Set up MI backend
            this.backend = detectBackend(args.debuggerBackend || 'auto');
            this.miSession = new MISession(this.backend);

            // Wire up events
            this.miSession.on('exec', (record: any) => this.handleExecRecord(record));
            this.miSession.on('console', (text: string) => {
                this.sendEvent(new OutputEvent(text, 'console'));
            });
            this.miSession.on('exit', () => {
                this.sendEvent(new TerminatedEvent());
            });

            // Step 3: Start debugger
            this.miSession.start();

            // Load helper scripts (hew-gdb.py for GDB)
            const helperCmd = this.backend.loadHelperScript();
            if (helperCmd) {
                await this.miSession.sendCommand(helperCmd);
            }

            // Load executable
            await this.miSession.sendCommand(this.backend.execAndSymbolsCmd(executable));

            // Set working directory if specified
            if (args.cwd) {
                await this.miSession.sendCommand(`-environment-cd ${args.cwd}`);
            }

            this.sendResponse(response);

            // Run unless stopOnEntry
            if (!args.stopOnEntry) {
                await this.miSession.sendCommand(this.backend.execRunCmd(args.args || []));
            } else {
                // Set temporary breakpoint at main, then run
                await this.miSession.sendCommand(this.backend.breakInsertCmd('main', 0).replace(/main:0/, '-t main'));
                await this.miSession.sendCommand(this.backend.execRunCmd(args.args || []));
            }
        } catch (err: any) {
            response.success = false;
            response.message = err.message;
            this.sendResponse(response);
        }
    }

    protected configurationDoneRequest(response: DebugProtocol.ConfigurationDoneResponse): void {
        this.sendResponse(response);
    }

    protected async setBreakPointsRequest(
        response: DebugProtocol.SetBreakpointsResponse,
        args: DebugProtocol.SetBreakpointsArguments
    ): Promise<void> {
        const source = args.source.path || args.source.name || '';
        const breakpoints: DebugProtocol.Breakpoint[] = [];

        // Clear existing breakpoints for this file first
        // (MI doesn't have a per-file clear, so we delete all and re-set)

        for (const bp of args.breakpoints || []) {
            try {
                const result = await this.miSession!.sendCommand(
                    this.backend!.breakInsertCmd(source, bp.line)
                );
                breakpoints.push({
                    verified: true,
                    line: bp.line,
                    id: parseInt(result.data?.bkpt?.number || '0', 10),
                    source: { path: source }
                });
            } catch {
                breakpoints.push({ verified: false, line: bp.line });
            }
        }

        response.body = { breakpoints };
        this.sendResponse(response);
    }

    protected async threadsRequest(response: DebugProtocol.ThreadsResponse): Promise<void> {
        try {
            const result = await this.miSession!.sendCommand(this.backend!.threadInfoCmd());
            const threads: Thread[] = (result.data?.threads || []).map((t: any) =>
                new Thread(parseInt(t.id, 10), t.name || `Thread ${t.id}`)
            );
            response.body = { threads: threads.length ? threads : [new Thread(1, 'main')] };
        } catch {
            response.body = { threads: [new Thread(1, 'main')] };
        }
        this.sendResponse(response);
    }

    protected async stackTraceRequest(
        response: DebugProtocol.StackTraceResponse,
        args: DebugProtocol.StackTraceArguments
    ): Promise<void> {
        try {
            const result = await this.miSession!.sendCommand(
                this.backend!.stackListFramesCmd(args.threadId)
            );
            const rawFrames: any[] = result.data?.stack || [];
            const frames: StackFrame[] = [];

            for (const f of rawFrames) {
                const frame = f.frame || f;
                const funcName = frame.func || '??';

                // Filter runtime frames unless showRuntimeFrames is enabled
                if (!this.showRuntimeFrames && RUNTIME_FRAME_PATTERNS.some(p => p.test(funcName))) {
                    continue;
                }

                const line = parseInt(frame.line || '0', 10);
                const source = frame.fullname || frame.file
                    ? new Source(
                        path.basename(frame.file || ''),
                        frame.fullname || frame.file
                    )
                    : undefined;

                frames.push(new StackFrame(
                    parseInt(frame.level || '0', 10),
                    funcName,
                    source,
                    line
                ));
            }

            response.body = { stackFrames: frames, totalFrames: frames.length };
        } catch {
            response.body = { stackFrames: [], totalFrames: 0 };
        }
        this.sendResponse(response);
    }

    protected scopesRequest(response: DebugProtocol.ScopesResponse, args: DebugProtocol.ScopesArguments): void {
        const ref = this.nextVarRef++;
        this.variableHandles.set(ref, { threadId: 1, frameId: args.frameId });
        response.body = {
            scopes: [new Scope('Locals', ref, false)]
        };
        this.sendResponse(response);
    }

    protected async variablesRequest(
        response: DebugProtocol.VariablesResponse,
        args: DebugProtocol.VariablesArguments
    ): Promise<void> {
        const handle = this.variableHandles.get(args.variablesReference);
        if (!handle) {
            response.body = { variables: [] };
            this.sendResponse(response);
            return;
        }

        try {
            const result = await this.miSession!.sendCommand(
                this.backend!.stackListVariablesCmd(handle.threadId, handle.frameId)
            );
            const vars: Variable[] = (result.data?.variables || []).map((v: any) => ({
                name: v.name || '?',
                value: v.value || '<unavailable>',
                variablesReference: 0,
            }));
            response.body = { variables: vars };
        } catch {
            response.body = { variables: [] };
        }
        this.sendResponse(response);
    }

    protected async continueRequest(response: DebugProtocol.ContinueResponse, args: DebugProtocol.ContinueArguments): Promise<void> {
        await this.miSession!.sendCommand(this.backend!.continueCmd(args.threadId));
        response.body = { allThreadsContinued: true };
        this.sendResponse(response);
    }

    protected async nextRequest(response: DebugProtocol.NextResponse, args: DebugProtocol.NextArguments): Promise<void> {
        await this.miSession!.sendCommand(this.backend!.nextCmd(args.threadId));
        this.sendResponse(response);
    }

    protected async stepInRequest(response: DebugProtocol.StepInResponse, args: DebugProtocol.StepInArguments): Promise<void> {
        await this.miSession!.sendCommand(this.backend!.stepCmd(args.threadId));
        this.sendResponse(response);
    }

    protected async stepOutRequest(response: DebugProtocol.StepOutResponse, args: DebugProtocol.StepOutArguments): Promise<void> {
        await this.miSession!.sendCommand(this.backend!.finishCmd(args.threadId));
        this.sendResponse(response);
    }

    protected async evaluateRequest(
        response: DebugProtocol.EvaluateResponse,
        args: DebugProtocol.EvaluateArguments
    ): Promise<void> {
        try {
            const result = await this.miSession!.sendCommand(this.backend!.evalCmd(args.expression));
            response.body = { result: result.data?.value || '', variablesReference: 0 };
        } catch (err: any) {
            response.body = { result: err.message, variablesReference: 0 };
        }
        this.sendResponse(response);
    }

    protected async disconnectRequest(response: DebugProtocol.DisconnectResponse): Promise<void> {
        try {
            await this.miSession?.sendCommand(this.backend!.exitCmd());
        } catch { /* ignore */ }
        this.miSession?.kill();

        // Clean up compiled binary if we created it
        if (this.compiledBinary && this.compiledBinary !== this.compiledBinary) {
            try { fs.unlinkSync(this.compiledBinary); } catch { /* ignore */ }
        }

        this.sendResponse(response);
    }

    // --- Custom DAP requests for Hew actor debugging ---

    protected customRequest(command: string, response: DebugProtocol.Response, args: any): void {
        switch (command) {
            case 'hew/listActors':
                this.handleListActors(response);
                break;
            case 'hew/breakOnReceive':
                this.handleBreakOnReceive(response, args);
                break;
            case 'hew/toggleRuntimeFrames':
                this.showRuntimeFrames = !this.showRuntimeFrames;
                response.body = { showRuntimeFrames: this.showRuntimeFrames };
                this.sendResponse(response);
                break;
            default:
                super.customRequest(command, response, args);
        }
    }

    // --- Private helpers ---

    private async compileProgramDebug(hewFile: string, cwd?: string): Promise<string> {
        const hewBin = this.findHewBinary();
        if (!hewBin) throw new Error('hew compiler not found. Set hew.debugger.hewPath in settings.');

        const outputName = path.basename(hewFile, '.hew');
        const outputPath = path.join(cwd || path.dirname(hewFile), outputName);

        this.sendEvent(new OutputEvent(`Compiling ${hewFile} with debug info...\n`, 'console'));

        try {
            execFileSync(hewBin, ['build', '--debug', hewFile, '-o', outputPath], {
                cwd: cwd || path.dirname(hewFile),
                timeout: 60000,
            });
        } catch (err: any) {
            throw new Error(`Compilation failed: ${err.stderr?.toString() || err.message}`);
        }

        return outputPath;
    }

    private findHewBinary(): string | undefined {
        // Reuse the same binary search logic as the extension (simplified)
        try {
            execFileSync(process.platform === 'win32' ? 'where' : 'which', ['hew'], { stdio: 'pipe' });
            return 'hew';
        } catch {
            return undefined;
        }
    }

    private handleExecRecord(record: any): void {
        if (record.class === 'stopped') {
            const reason = record.data?.reason || 'pause';
            let dapReason: string;
            switch (reason) {
                case 'breakpoint-hit': dapReason = 'breakpoint'; break;
                case 'end-stepping-range': dapReason = 'step'; break;
                case 'signal-received': dapReason = 'exception'; break;
                case 'exited':
                case 'exited-normally':
                    this.sendEvent(new TerminatedEvent());
                    return;
                default: dapReason = 'pause';
            }
            const threadId = parseInt(record.data?.['thread-id'] || '1', 10);
            this.sendEvent(new StoppedEvent(dapReason, threadId));
        }
    }

    private async handleListActors(response: DebugProtocol.Response): Promise<void> {
        if (this.backend instanceof GDBBackend) {
            try {
                // Use the hew-actors GDB command
                const result = await this.miSession!.sendCommand(
                    '-interpreter-exec console "hew-actors"'
                );
                response.body = { output: result.data || '' };
            } catch (err: any) {
                response.body = { output: err.message };
            }
        } else {
            response.body = { output: 'Actor listing not yet supported for LLDB' };
        }
        this.sendResponse(response);
    }

    private async handleBreakOnReceive(response: DebugProtocol.Response, args: any): Promise<void> {
        const { actorName, methodName } = args;
        if (this.backend instanceof GDBBackend) {
            const cmd = methodName
                ? `-interpreter-exec console "hew-break-receive ${actorName} ${methodName}"`
                : `-interpreter-exec console "hew-break-receive ${actorName}"`;
            try {
                await this.miSession!.sendCommand(cmd);
                response.body = { success: true };
            } catch (err: any) {
                response.body = { success: false, message: err.message };
            }
        } else {
            // For LLDB, set breakpoint on the function name pattern directly
            const pattern = methodName
                ? `${actorName}_receive_${methodName}`
                : `${actorName}_dispatch`;
            try {
                await this.miSession!.sendCommand(`-break-insert -r ${pattern}`);
                response.body = { success: true };
            } catch (err: any) {
                response.body = { success: false, message: err.message };
            }
        }
        this.sendResponse(response);
    }
}
```

**Step 2: Build**

```bash
npm run build:dev
```

Expected: Compiles without errors.

**Step 3: Commit**

```bash
git add src/debug/hew-debug-session.ts
git commit -m "feat: implement HewDebugSession — DAP to GDB/LLDB MI translation layer"
```

---

### Task 11: Wire debug adapter into extension activation

**Files:**
- Modify: `~/projects/hew-lang/vscode-hew/src/extension.ts`

**Step 1: Register the debug adapter factory**

Add to the `activate` function in `extension.ts`:

```typescript
import { HewDebugSession } from './debug/hew-debug-session';

// Inside activate():

// Register debug adapter
const debugAdapterFactory = new HewDebugAdapterFactory();
context.subscriptions.push(
    vscode.debug.registerDebugAdapterDescriptorFactory('hew', debugAdapterFactory)
);

// Register debug configuration provider for auto-detecting .hew files
context.subscriptions.push(
    vscode.debug.registerDebugConfigurationProvider('hew', new HewDebugConfigProvider())
);
```

**Step 2: Implement the factory and config provider (in extension.ts or a separate file)**

```typescript
class HewDebugAdapterFactory implements vscode.DebugAdapterDescriptorFactory {
    createDebugAdapterDescriptor(
        _session: vscode.DebugSession
    ): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
        return new vscode.DebugAdapterInlineImplementation(new HewDebugSession());
    }
}

class HewDebugConfigProvider implements vscode.DebugConfigurationProvider {
    resolveDebugConfiguration(
        folder: vscode.WorkspaceFolder | undefined,
        config: vscode.DebugConfiguration,
    ): vscode.ProviderResult<vscode.DebugConfiguration> {
        // If launch.json is missing or empty, provide a default
        if (!config.type && !config.request && !config.name) {
            const editor = vscode.window.activeTextEditor;
            if (editor && editor.document.languageId === 'hew') {
                config.type = 'hew';
                config.name = 'Debug Hew Program';
                config.request = 'launch';
                config.program = editor.document.uri.fsPath;
            }
        }

        if (!config.program) {
            return vscode.window.showInformationMessage('Cannot find a Hew program to debug').then(() => undefined);
        }

        return config;
    }
}
```

**Step 3: Build and test locally**

```bash
npm run build:dev
```

Expected: Extension builds. To test, open VS Code with the extension dev host (F5 from vscode-hew workspace), open a `.hew` file, and press F5 — should show the Hew debug configuration.

**Step 4: Commit**

```bash
git add src/extension.ts
git commit -m "feat: wire HewDebugSession into extension activation with debug adapter factory"
```

---

## Phase 3: Actor Debugging

### Task 12: Create LLDB helper script (equivalent of hew-gdb.py)

**Files:**
- Create: `~/projects/hew-lang/hew/scripts/debug/hew-lldb.py`

**Step 1: Implement LLDB formatters and commands**

```python
# scripts/debug/hew-lldb.py
"""LLDB extensions for debugging Hew programs."""

import lldb


# --- Type Summary Providers ---

def hew_string_summary(valobj, internal_dict):
    """Pretty-print hew_string_t { data: *const char, len: usize }."""
    data = valobj.GetChildMemberWithName('data')
    length = valobj.GetChildMemberWithName('len')
    if not data or not length:
        return '<invalid hew_string_t>'
    l = length.GetValueAsUnsigned(0)
    if l == 0:
        return '""'
    err = lldb.SBError()
    s = data.GetPointeeData(0, l).GetString(err, 0)
    if err.Fail():
        return f'<error reading string: {err.GetCString()}>'
    return f'"{s}"'


def hew_vec_summary(valobj, internal_dict):
    """Pretty-print Hew Vec (opaque pointer)."""
    return f'Vec@{valobj.GetValue()}'


def hew_hashmap_summary(valobj, internal_dict):
    """Pretty-print Hew HashMap (opaque pointer)."""
    return f'HashMap@{valobj.GetValue()}'


def hew_actor_ref_summary(valobj, internal_dict):
    """Pretty-print Hew ActorRef."""
    return f'ActorRef@{valobj.GetValue()}'


# --- Custom LLDB Commands ---

def hew_actors_cmd(debugger, command, result, internal_dict):
    """List active Hew actors."""
    target = debugger.GetSelectedTarget()
    process = target.GetProcess()
    if not process or not process.IsValid():
        result.AppendMessage('No running process.')
        return

    # Try to read actor count from runtime
    actor_count = target.FindFirstGlobalVariable('hew_runtime_actor_count')
    if actor_count.IsValid():
        result.AppendMessage(f'Active actors: {actor_count.GetValueAsUnsigned(0)}')
    else:
        result.AppendMessage('Actor count not available (release build or no actors spawned).')
        result.AppendMessage('Tip: Compile with `hew build --debug` for actor introspection.')


def hew_break_receive_cmd(debugger, command, result, internal_dict):
    """Set breakpoint on actor receive handler.

    Usage: hew-break-receive <actor_name> [method_name]
    """
    args = command.split()
    if not args:
        result.AppendMessage('Usage: hew-break-receive <actor_name> [method_name]')
        return

    target = debugger.GetSelectedTarget()
    actor_name = args[0]
    method_name = args[1] if len(args) > 1 else None

    if method_name:
        patterns = [
            f'{actor_name}_receive_{method_name}',
            f'{actor_name}_{method_name}',
        ]
    else:
        patterns = [f'{actor_name}_dispatch']

    for pattern in patterns:
        bp = target.BreakpointCreateByRegex(pattern)
        if bp.GetNumLocations() > 0:
            result.AppendMessage(f'Breakpoint set: {pattern} ({bp.GetNumLocations()} location(s))')
            return

    result.AppendMessage(f'No matching functions found for actor {actor_name}')


def hew_bt_cmd(debugger, command, result, internal_dict):
    """Hew-focused backtrace (filters runtime internals)."""
    target = debugger.GetSelectedTarget()
    process = target.GetProcess()
    thread = process.GetSelectedThread()

    skip_prefixes = [
        'hew_runtime_', '__pthread', '__libc', 'std::rt::',
        'core::ops::', 'tokio::', '__GI_', '_start', '__libc_start',
    ]

    for i in range(thread.GetNumFrames()):
        frame = thread.GetFrameAtIndex(i)
        name = frame.GetFunctionName() or '??'
        if any(name.startswith(p) for p in skip_prefixes):
            continue
        line_entry = frame.GetLineEntry()
        if line_entry.IsValid():
            filename = line_entry.GetFileSpec().GetFilename()
            line = line_entry.GetLine()
            result.AppendMessage(f'#{i}: {name} at {filename}:{line}')
        else:
            result.AppendMessage(f'#{i}: {name}')


# --- Registration ---

def __lldb_init_module(debugger, internal_dict):
    # Type summaries
    debugger.HandleCommand('type summary add -F hew_lldb.hew_string_summary hew_string_t')
    debugger.HandleCommand('type summary add -F hew_lldb.hew_vec_summary hew_vec')
    debugger.HandleCommand('type summary add -F hew_lldb.hew_hashmap_summary hew_hashmap')
    debugger.HandleCommand('type summary add -F hew_lldb.hew_actor_ref_summary hew_actor_ref')

    # Custom commands
    debugger.HandleCommand('command script add -f hew_lldb.hew_actors_cmd hew-actors')
    debugger.HandleCommand('command script add -f hew_lldb.hew_break_receive_cmd hew-break-receive')
    debugger.HandleCommand('command script add -f hew_lldb.hew_bt_cmd hew-bt')

    print('Hew LLDB extensions loaded. Commands: hew-actors, hew-break-receive, hew-bt')
```

**Step 2: Update `hew debug` CLI to use LLDB helper when LLDB is selected**

In `hew-cli/src/main.rs`, in the lldb branch of `cmd_debug()`, add script loading:

```rust
} else if which_exists("lldb") {
    let lldb_script = find_lldb_script(); // new helper
    let mut lldb_args = Vec::new();
    if let Some(script) = &lldb_script {
        lldb_args.push("-o".to_string());
        lldb_args.push(format!("command script import {}", script));
    }
    lldb_args.push("--".to_string());
    lldb_args.push(tmp_bin_str.clone());
    lldb_args.extend(program_args.iter().cloned());
    ("lldb".to_string(), lldb_args)
}
```

Add `find_lldb_script()` analogous to `find_gdb_script()`, looking for `hew-lldb.py`.

**Step 3: Commit**

```bash
git add scripts/debug/hew-lldb.py hew-cli/src/main.rs
git commit -m "feat: add LLDB debug helper script with pretty-printers and actor commands"
```

---

### Task 13: Build Hew Actors tree view panel in VS Code

**Files:**
- Create: `~/projects/hew-lang/vscode-hew/src/debug/actors-tree-view.ts`
- Modify: `~/projects/hew-lang/vscode-hew/src/extension.ts`
- Modify: `~/projects/hew-lang/vscode-hew/package.json` (add view contribution)

**Step 1: Add view container contribution to package.json**

Add to `contributes`:

```json
"viewsContainers": {
    "debugpanel": [{
        "id": "hew-debug",
        "title": "Hew Debug",
        "icon": "icons/hew-dark.png"
    }]
},
"views": {
    "debug": [{
        "id": "hewActors",
        "name": "Hew Actors",
        "when": "debugType == 'hew'"
    }]
},
"menus": {
    "view/item/context": [{
        "command": "hew.debug.breakOnReceive",
        "when": "view == hewActors",
        "group": "navigation"
    }]
},
"commands": [{
    "command": "hew.debug.breakOnReceive",
    "title": "Break on Receive",
    "category": "Hew Debug"
}, {
    "command": "hew.debug.toggleRuntimeFrames",
    "title": "Toggle Runtime Frames in Call Stack",
    "category": "Hew Debug"
}]
```

**Step 2: Implement the tree data provider**

```typescript
// src/debug/actors-tree-view.ts
import * as vscode from 'vscode';

export class ActorTreeItem extends vscode.TreeItem {
    constructor(
        public readonly actorName: string,
        public readonly actorState: string,
    ) {
        super(actorName, vscode.TreeItemCollapsibleState.None);
        this.description = actorState;
        this.contextValue = 'hewActor';
    }
}

export class HewActorsProvider implements vscode.TreeDataProvider<ActorTreeItem> {
    private _onDidChangeTreeData = new vscode.EventEmitter<ActorTreeItem | undefined>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

    private actors: ActorTreeItem[] = [];

    refresh(actors: ActorTreeItem[]): void {
        this.actors = actors;
        this._onDidChangeTreeData.fire(undefined);
    }

    getTreeItem(element: ActorTreeItem): vscode.TreeItem {
        return element;
    }

    getChildren(): ActorTreeItem[] {
        return this.actors;
    }
}
```

**Step 3: Register in extension.ts**

```typescript
// In activate():
const actorsProvider = new HewActorsProvider();
context.subscriptions.push(
    vscode.window.registerTreeDataProvider('hewActors', actorsProvider)
);

context.subscriptions.push(
    vscode.commands.registerCommand('hew.debug.breakOnReceive', (item: ActorTreeItem) => {
        const session = vscode.debug.activeDebugSession;
        if (session) {
            session.customRequest('hew/breakOnReceive', { actorName: item.actorName });
        }
    })
);

context.subscriptions.push(
    vscode.commands.registerCommand('hew.debug.toggleRuntimeFrames', () => {
        const session = vscode.debug.activeDebugSession;
        if (session) {
            session.customRequest('hew/toggleRuntimeFrames');
        }
    })
);
```

**Step 4: Build**

```bash
npm run build:dev
```

**Step 5: Commit**

```bash
git add src/debug/actors-tree-view.ts src/extension.ts package.json
git commit -m "feat: add Hew Actors tree view panel for actor debugging in VS Code"
```

---

## Phase 4: Integration Testing & Polish

### Task 14: End-to-end integration test

**Files:**
- Create: `~/projects/hew-lang/vscode-hew/tests/debug-session.test.ts`

**Step 1: Write integration test for the debug session**

```typescript
import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { HewDebugSession } from '../src/debug/hew-debug-session';

// Note: Full integration testing requires a compiled hew binary with DWARF.
// These tests verify the session lifecycle and MI command generation.

describe('HewDebugSession', () => {
    it('creates a new session', () => {
        const session = new HewDebugSession();
        expect(session).toBeDefined();
    });
});
```

This is intentionally minimal — full integration tests require the compiled hew binary and a running GDB/LLDB. More comprehensive tests should be added once Phase 1 (DWARF) is complete.

**Step 2: Run all tests**

```bash
cd ~/projects/hew-lang/vscode-hew
npm test
```

Expected: All tests pass (grammar tests + new debug tests).

**Step 3: Full end-to-end manual verification**

1. Build the compiler: `cd ~/projects/hew-lang/hew && make`
2. Create test file `~/test-debug.hew`:
   ```hew
   actor Counter {
       var count: Int = 0

       receive Increment() {
           count = count + 1
       }

       receive GetCount() -> Int {
           return count
       }
   }

   fn main() {
       let counter = spawn Counter()
       send counter.Increment()
       let result = ask counter.GetCount()
       print(result)
   }
   ```
3. Open `~/test-debug.hew` in VS Code with the dev extension
4. Set breakpoint on `count = count + 1`
5. Press F5 → should compile, launch, hit breakpoint
6. Check: call stack shows `.hew` file, variables panel shows `count`
7. Check: Hew Actors panel lists the Counter actor

**Step 4: Commit**

```bash
git add tests/debug-session.test.ts
git commit -m "test: add debug session integration test scaffold"
```

---

### Task 15: Update esbuild config to bundle debug modules

**Files:**
- Modify: `~/projects/hew-lang/vscode-hew/package.json` (esbuild script)

**Step 1: Verify esbuild bundles the new debug modules**

The existing esbuild config bundles from `src/extension.ts` as entry point. Since the debug modules are imported from `extension.ts`, they should be automatically included. Verify:

```bash
npm run build:dev
ls -la out/extension.js
```

The bundle size should have increased. Check that `@vscode/debugadapter` is not in the `--external` list (only `vscode` should be external).

If `@vscode/debugadapter` needs to be bundled (it's a regular dependency, not a VS Code API), it should work automatically. If it causes issues, it may need to be added as a separate entry point or handled differently.

**Step 2: Test the production build**

```bash
npm run build
npm run package
```

Expected: `.vsix` package builds successfully.

**Step 3: Commit if any changes were needed**

```bash
git add package.json
git commit -m "chore: ensure debug modules are bundled in production build"
```
