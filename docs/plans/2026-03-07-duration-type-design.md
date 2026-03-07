# Duration Type Design

**Date:** 2026-03-07
**Status:** Approved

## Summary

Add `duration` as a distinct primitive type stored as `i64` nanoseconds. Type-safe arithmetic prevents unit confusion. Methods and operators lower to inline LLVM integer ops — no runtime functions needed.

## Internal Representation

- Stored as `i64` nanoseconds
- Max range: ~292 years (sufficient for any service lifetime)
- `Copy`, `Send`, comparable
- Does not implicitly convert to or from integers
- Maps to `i64` at the MLIR/LLVM level

## Literals

Existing parser unchanged — already produces nanosecond values:

```hew
let t = 5s;       // 5_000_000_000 ns
let t = 100ms;    // 100_000_000 ns
let t = 500us;    // 500_000 ns
let t = 10ns;     // 10 ns
let t = 1m;       // 60_000_000_000 ns
let t = 2h;       // 7_200_000_000_000 ns
```

## Arithmetic Operators

| Expression | Result | Codegen |
|-----------|--------|---------|
| `duration + duration` | `duration` | `arith.addi` |
| `duration - duration` | `duration` | `arith.subi` |
| `duration * int` | `duration` | `arith.muli` |
| `int * duration` | `duration` | `arith.muli` |
| `duration / int` | `duration` | `arith.divsi` |
| `duration / duration` | `i64` | `arith.divsi` |
| `duration % duration` | `duration` | `arith.remsi` |
| `-duration` | `duration` | `arith.subi(0, d)` or LLVM negation |
| `duration == duration` | `bool` | `arith.cmpi eq` |
| `duration != duration` | `bool` | `arith.cmpi ne` |
| `duration < duration` | `bool` | `arith.cmpi slt` |
| `duration <= duration` | `bool` | `arith.cmpi sle` |
| `duration > duration` | `bool` | `arith.cmpi sgt` |
| `duration >= duration` | `bool` | `arith.cmpi sge` |

Compile errors (type checker rejects):
- `duration + int` — no implicit mixing
- `duration * float` — avoid precision confusion
- `duration * duration` — nonsensical

## Methods

All lower to inline integer ops. LLVM constant-folds when operands are known.

| Method | Signature | Codegen |
|--------|-----------|---------|
| `.nanos()` | `() -> i64` | identity (no-op) |
| `.micros()` | `() -> i64` | `arith.divsi %d, 1000` |
| `.millis()` | `() -> i64` | `arith.divsi %d, 1_000_000` |
| `.secs()` | `() -> i64` | `arith.divsi %d, 1_000_000_000` |
| `.mins()` | `() -> i64` | `arith.divsi %d, 60_000_000_000` |
| `.hours()` | `() -> i64` | `arith.divsi %d, 3_600_000_000_000` |
| `.abs()` | `() -> duration` | `math.absi` or branch |
| `.is_zero()` | `() -> bool` | `arith.cmpi eq, %d, 0` |

## Constructor

```hew
let d = duration::from_nanos(raw_i64_value);
```

At codegen level this is a no-op type cast — the value is already i64.

## Integration: `| after` and `budget()`

Clean break: `| after` and supervisor `budget()` require `duration`. Bare integers rejected.

```hew
let result = await task | after 5s;                    // OK
let result = await task | after duration::from_nanos(n); // OK
let result = await task | after 5000;                  // ERROR: expected duration
```

## Implementation Layers

### 1. Type system (`hew-types/src/ty.rs`)
- Add `Ty::Duration` variant to `Ty` enum
- `is_duration()`, `is_primitive()`, `is_copy()` helpers
- Display as `"duration"`

### 2. Type checker (`hew-types/src/check.rs`)
- `Literal::Duration(_)` synthesizes to `Ty::Duration` (currently `Ty::I64`)
- Binary ops: extend `check_binary_op()` with duration rules
- Methods: add `(Ty::Duration, _)` arm in `check_method_call()`
- Register `duration::from_nanos` as builtin constructor
- Update `| after` and `budget()` type checking to require `Ty::Duration`

### 3. Serializer (`hew-serialize/src/enrich.rs`)
- `Ty::Duration` → `TypeExpr::Named { name: "duration", type_args: None }`

### 4. Codegen (`hew-codegen/lib/MLIRGen/`)
- `convertType()`: map `"duration"` → `builder.getI64Type()`
- Binary ops: duration arithmetic emits same i64 MLIR ops (type checker already validated)
- Methods: emit inline `arith.divsi` / `arith.cmpi` / `math.absi`
- `from_nanos`: no-op (value passthrough)

### 5. Spec (`docs/specs/HEW-SPEC.md`)
- Update §10.3 to remove "not yet implemented" note
- Document methods, arithmetic, constructor

### 6. Tests
- Type checker unit tests for all arithmetic combinations
- Type checker error tests for rejected combos (duration+int, duration*float)
- E2E codegen tests for method correctness
- E2E test for `| after` requiring duration

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| i64 nanoseconds, not struct | Zero codegen complexity; single LLVM register; proven by Go |
| No runtime functions | All ops are trivial integer arithmetic; inline in MLIR; LLVM constant-folds |
| No float multiplication | Prevents precision confusion; add `.mul_f64()` method later if needed |
| Clean break for `\| after` | Whole point of distinct type is preventing unit confusion |
| Truncation semantics | `1500ms.secs()` returns `1` not `1.5`; matches Go; predictable |
