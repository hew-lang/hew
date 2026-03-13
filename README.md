# Node Builtin Codegen Implementation Guide

This directory contains reference documentation for implementing codegen lowering for Hew's `Node::*` builtins.

## Documents in This Directory

1. **BUILTIN_LOWERING_REFERENCE.md** (Comprehensive)
   - Complete analysis of how println, spawn, close, sleep_ms are lowered
   - Serialization format for builtin calls
   - Typechecker registration patterns
   - Full context and examples

2. **NODE_BUILTIN_IMPLEMENTATION_PLAN.md** (Step-by-Step)
   - 3-step implementation process
   - Exact code for all 6 Node builtins
   - Testing checklist
   - Files to modify with line counts

3. **QUICK_REFERENCE.md** (Lookup)
   - File locations and line numbers
   - Implementation quick steps
   - Pattern checklist
   - Common mistakes to avoid

## Quick Start

### What needs to be implemented?

Add codegen lowering for 6 Node builtins:

- `Node::start(addr: String) -> Unit`
- `Node::shutdown() -> Unit`
- `Node::connect(addr: String) -> Unit`
- `Node::register(name: String, actor_ref: T) -> Unit`
- `Node::lookup(name: String) -> T`
- `Node::set_transport(config: T) -> Unit`

### Where?

**2 files only:**

1. `/home/slepp/projects/hew-lang/hew/hew-types/src/check.rs` (2 new lines)
2. `/home/slepp/projects/hew-lang/hew/hew-codegen/src/mlir/MLIRGen.cpp` (~70 new lines)

### How?

1. **Typechecker:** Add 2 builtin registrations
2. **Codegen:** Add 6 names to StringSet + 6 if-blocks

See NODE_BUILTIN_IMPLEMENTATION_PLAN.md for exact code.

## Key Insight: The Pattern

All Node builtins follow the same pattern as existing builtins like `sleep_ms()` and `close()`:

```cpp
if (name == "Node::something") {
  // Validate args
  auto arg = generateExpression(...);
  if (!arg) return nullptr;

  // Call C FFI function via RuntimeCallOp
  hew::RuntimeCallOp::create(builder, location,
    mlir::TypeRange{/* return type or empty */},
    mlir::SymbolRefAttr::get(&context, "hew_node_something"),
    mlir::ValueRange{arg});

  // For void: return nullptr
  // For value: return .getResult()
}
```

## Reference Materials

### How println is lowered

- Entry point: `MLIRGenExpr.cpp:1407-1413`
- Implementation: `MLIRGenExpr.cpp:1792-1815`
- Uses custom `hew::PrintOp` (not RuntimeCallOp)

### How spawn creates actors

- Implementation: `MLIRGenActor.cpp:852-950`
- Returns actor pointer (handle)
- Calls generated C init functions

### How close/stop work (reference for Node builtins)

- close(): `MLIRGen.cpp:1277-1288`
- stop(): `MLIRGen.cpp:1264-1275`
- Both void-returning, take actor argument

### Serialization format

- AST: `hew-parser/src/ast.rs` line 180-185 (Expr::Call)
- MessagePack: `hew-serialize/src/msgpack.rs` line 85-107
- Schema version: 1

### Typechecker registration

- Existing registrations: `check.rs:742-750`
- Pattern: `register_builtin_fn(name, params, return_type)`
- Node::lookup uses generic TypeVar for polymorphism

## Files to Study

### Core Implementation Files

- `/home/slepp/projects/hew-lang/hew/hew-codegen/src/mlir/MLIRGen.cpp`
  - generateBuiltinCall() - main dispatcher (line 943)
  - sleep_ms pattern (line 1121)
  - close/stop/link patterns (line 1264+)

- `/home/slepp/projects/hew-lang/hew/hew-codegen/src/mlir/MLIRGenExpr.cpp`
  - generateFunctionCallExpr() - call entry point (line 1368)
  - println dispatch (line 1407)
  - generatePrintCall() - custom handler (line 1792)
  - module method calls (line 3155)

- `/home/slepp/projects/hew-lang/hew/hew-types/src/check.rs`
  - Builtin registration (line 740+)
  - register_builtin_fn (line 784)

### Reference Files

- `/home/slepp/projects/hew-lang/hew/hew-codegen/src/mlir/MLIRGenActor.cpp`
  - generateSpawnExpr() - actor creation (line 852)

- `/home/slepp/projects/hew-lang/hew/hew-parser/src/ast.rs`
  - Expr enum (line 121+)
  - Expr::Call (line 180)
  - CallArg enum (line 85)

## Next Steps

1. **Read** BUILTIN_LOWERING_REFERENCE.md for deep understanding
2. **Follow** NODE_BUILTIN_IMPLEMENTATION_PLAN.md for step-by-step implementation
3. **Use** QUICK_REFERENCE.md while coding for fast lookups
4. **Implement** the 2-file changes
5. **Test** with sample Hew code using Node:: calls

## Notes

- All Node:: builtins should call corresponding `hew_node_*` C functions
- Two builtins (Node::connect, Node::set_transport) are NOT in typechecker yet - add them first
- Four builtins (start, shutdown, register, lookup) already exist in typechecker
- C++ code uses RuntimeCallOp for all FFI calls
- Rust typechecker uses register_builtin_fn to define signatures

---

**Status:** Ready for implementation  
**Total Changes:** 2 files, ~72 lines of code  
**Effort:** 30-60 minutes for implementation + testing
