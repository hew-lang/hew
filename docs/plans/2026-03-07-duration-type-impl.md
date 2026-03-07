# Duration Type Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `duration` as a distinct primitive type with type-safe arithmetic and inline methods.

**Architecture:** Add `Ty::Duration` to the type system. Type checker enforces arithmetic rules (duration+duration OK, duration+int ERROR). Serializer maps to `"duration"`. Codegen maps to `i64`. Methods emit inline MLIR integer ops — no runtime functions.

**Tech Stack:** Rust (hew-types, hew-serialize), C++ (hew-codegen/MLIR)

**Design doc:** `docs/plans/2026-03-07-duration-type-design.md`

---

### Task 1: Add Ty::Duration variant to type system

**Files:**
- Modify: `hew-types/src/ty.rs:48-140` (Ty enum)
- Modify: `hew-types/src/ty.rs:397-401` (is_primitive)
- Modify: `hew-types/src/ty.rs:513-535` (Display impl)

**Step 1: Add Duration variant to Ty enum**

In `hew-types/src/ty.rs`, add `Duration` after `Bytes` (line 77):

```rust
    /// Ref-counted byte buffer
    Bytes,
    /// Duration in nanoseconds (distinct from i64)
    Duration,
    /// Unit type (void)
    Unit,
```

**Step 2: Add is_duration() helper**

After `is_bytes()` (line 380):

```rust
    /// Check if this is the duration type.
    #[must_use]
    pub fn is_duration(&self) -> bool {
        matches!(self, Ty::Duration)
    }
```

**Step 3: Include Duration in is_primitive() and is_copy()**

Update `is_primitive()` at line 399:

```rust
    pub fn is_primitive(&self) -> bool {
        self.is_integer() || self.is_float() || matches!(self, Ty::Bool | Ty::Char | Ty::Unit | Ty::Duration)
    }
```

`is_copy()` already delegates to `is_primitive()`, so no change needed there.

**Step 4: Add Display impl**

After `Ty::Bytes => write!(f, "bytes"),` at line 533:

```rust
            Ty::Duration => write!(f, "duration"),
```

**Step 5: Build and verify**

Run: `cargo build -p hew-types 2>&1 | head -50`

Expected: Compiler errors about non-exhaustive match patterns wherever Ty is matched — this is expected and will be fixed in subsequent tasks. Verify the Ty enum itself compiles.

**Step 6: Fix exhaustive match warnings in ty.rs**

Search for any match blocks in ty.rs that need a `Ty::Duration` arm. Add them following the `Ty::Char` pattern (Duration is a scalar like Char). Common patterns:
- `substitute()`: `Ty::Duration => Ty::Duration` (no children to substitute)
- `any_child()`: `Ty::Duration => false`
- `map_children()`: `Ty::Duration => Ty::Duration`

**Step 7: Commit**

```
feat(types): add Ty::Duration variant to type system
```

---

### Task 2: Type checker — Duration literal synthesis

**Files:**
- Modify: `hew-types/src/check.rs:3426-3429` (literal synthesis)

**Step 1: Write failing test**

Add to the test module at the bottom of `hew-types/src/check.rs`:

```rust
#[test]
fn duration_literal_has_duration_type() {
    let source = "let d = 5s;";
    let mut parser = Parser::new(source);
    let program = parser.parse_program();
    let mut checker = Checker::new();
    let _ = checker.check_program(&program.items);
    // Duration literal should have type Duration, not I64
    let errors: Vec<_> = checker.errors().iter().collect();
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
}
```

**Step 2: Change Duration literal type**

At line 3428, split `Duration` from `Integer`:

```rust
Expr::Literal(Literal::Bool(_)) => Ty::Bool,
Expr::Literal(Literal::Char(_)) => Ty::Char,
Expr::Literal(Literal::Integer { .. }) => Ty::I64,
Expr::Literal(Literal::Duration(_)) => Ty::Duration,
```

**Step 3: Run tests**

Run: `make test-rust 2>&1 | tail -30`

Expected: Some existing tests may fail if they relied on duration being I64. Fix those by updating expectations.

**Step 4: Commit**

```
feat(types): duration literals synthesize to Ty::Duration
```

---

### Task 3: Type checker — Duration arithmetic rules

**Files:**
- Modify: `hew-types/src/check.rs:4300-4346` (check_binary_op arithmetic branch)

**Step 1: Write failing tests**

```rust
#[test]
fn duration_add_duration() {
    let source = "let a = 5s + 3s;";
    let errors = check_source(source);
    assert!(errors.is_empty(), "duration + duration should be valid: {errors:?}");
}

#[test]
fn duration_mul_int() {
    let source = "let a = 5s * 3;";
    let errors = check_source(source);
    assert!(errors.is_empty(), "duration * int should be valid: {errors:?}");
}

#[test]
fn duration_div_duration() {
    let source = "let a = 10s / 2s;";
    let errors = check_source(source);
    assert!(errors.is_empty(), "duration / duration should be valid: {errors:?}");
}

#[test]
fn duration_add_int_error() {
    let source = "let a = 5s + 42;";
    let errors = check_source(source);
    assert!(!errors.is_empty(), "duration + int should be an error");
}

#[test]
fn duration_mul_duration_error() {
    let source = "let a = 5s * 3s;";
    let errors = check_source(source);
    assert!(!errors.is_empty(), "duration * duration should be an error");
}
```

**Step 2: Add duration arithmetic rules**

In `check_binary_op()` at line 4300, add a duration check **before** the numeric check (line 4306). Insert before `if left_resolved.is_numeric() && right_resolved.is_numeric()`:

```rust
BinaryOp::Add
| BinaryOp::Subtract
| BinaryOp::Multiply
| BinaryOp::Divide
| BinaryOp::Modulo => {
    // Duration arithmetic rules (checked before numeric)
    if left_resolved.is_duration() || right_resolved.is_duration() {
        return self.check_duration_arithmetic(
            op, &left_resolved, &right_resolved, &left.1,
        );
    }
    // ... existing numeric check continues
```

**Step 3: Implement check_duration_arithmetic helper**

Add this method to the Checker impl:

```rust
fn check_duration_arithmetic(
    &mut self,
    op: BinaryOp,
    left: &Ty,
    right: &Ty,
    span: &Span,
) -> Ty {
    match (left, right, op) {
        // duration + duration → duration
        // duration - duration → duration
        (Ty::Duration, Ty::Duration, BinaryOp::Add | BinaryOp::Subtract) => Ty::Duration,
        // duration * int → duration
        (Ty::Duration, r, BinaryOp::Multiply) if r.is_integer() => Ty::Duration,
        // int * duration → duration
        (l, Ty::Duration, BinaryOp::Multiply) if l.is_integer() => Ty::Duration,
        // duration / int → duration
        (Ty::Duration, r, BinaryOp::Divide) if r.is_integer() => Ty::Duration,
        // duration / duration → i64
        (Ty::Duration, Ty::Duration, BinaryOp::Divide) => Ty::I64,
        // duration % duration → duration
        (Ty::Duration, Ty::Duration, BinaryOp::Modulo) => Ty::Duration,
        _ => {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!("cannot apply `{op:?}` to `{left}` and `{right}`"),
            );
            Ty::Error
        }
    }
}
```

**Step 4: Handle duration comparisons**

In the comparison branch of `check_binary_op()` (around line 4380+), duration comparisons need to be accepted. Find where `==`, `!=`, `<`, `<=`, `>`, `>=` are handled and ensure `Ty::Duration == Ty::Duration` typechecks. If comparisons use `expect_type`, Duration == Duration should already unify. Verify with a test:

```rust
#[test]
fn duration_comparison() {
    let source = "let a = 5s < 10s;";
    let errors = check_source(source);
    assert!(errors.is_empty(), "duration < duration should be valid: {errors:?}");
}
```

**Step 5: Handle unary negation**

Find where `UnaryOp::Negate` is type-checked. Ensure Duration is accepted (it should produce Duration). If the check requires `is_numeric()`, add `|| ty.is_duration()`:

```rust
#[test]
fn duration_negation() {
    let source = "let a = -5s;";
    let errors = check_source(source);
    assert!(errors.is_empty(), "negated duration should be valid: {errors:?}");
}
```

**Step 6: Run all tests**

Run: `make test-rust 2>&1 | tail -30`

**Step 7: Commit**

```
feat(types): type-safe duration arithmetic rules
```

---

### Task 4: Type checker — Duration methods

**Files:**
- Modify: `hew-types/src/check.rs` (check_method_call, around line 5538+)

**Step 1: Write failing tests**

```rust
#[test]
fn duration_millis_method() {
    let source = "let d = 5s; let ms: i64 = d.millis();";
    let errors = check_source(source);
    assert!(errors.is_empty(), "d.millis() should return i64: {errors:?}");
}

#[test]
fn duration_abs_method() {
    let source = "let d = -5s; let a = d.abs();";
    let errors = check_source(source);
    assert!(errors.is_empty(), "d.abs() should return duration: {errors:?}");
}

#[test]
fn duration_is_zero_method() {
    let source = "let d = 0s; let z: bool = d.is_zero();";
    let errors = check_source(source);
    assert!(errors.is_empty(), "d.is_zero() should return bool: {errors:?}");
}
```

**Step 2: Add Duration method dispatch**

In `check_method_call()`, find the section with `(Ty::String, _)` arm. Add before or after it:

```rust
(Ty::Duration, _) => {
    if !args.is_empty() {
        self.report_error(
            TypeErrorKind::WrongArity,
            span,
            format!("duration method `{method}` takes no arguments"),
        );
    }
    match method.as_str() {
        "nanos" => Ty::I64,
        "micros" => Ty::I64,
        "millis" => Ty::I64,
        "secs" => Ty::I64,
        "mins" => Ty::I64,
        "hours" => Ty::I64,
        "abs" => Ty::Duration,
        "is_zero" => Ty::Bool,
        _ => {
            self.report_error(
                TypeErrorKind::UnknownMethod,
                span,
                format!("no method `{method}` on type `duration`"),
            );
            Ty::Error
        }
    }
}
```

**Step 3: Run tests**

Run: `make test-rust 2>&1 | tail -30`

**Step 4: Commit**

```
feat(types): duration methods (.millis(), .secs(), .abs(), etc.)
```

---

### Task 5: Type checker — duration::from_nanos constructor and | after enforcement

**Files:**
- Modify: `hew-types/src/check.rs` (builtins, timeout expr)

**Step 1: Write failing tests**

```rust
#[test]
fn duration_from_nanos() {
    let source = "let d = duration::from_nanos(1000);";
    let errors = check_source(source);
    assert!(errors.is_empty(), "duration::from_nanos should work: {errors:?}");
}
```

**Step 2: Register duration::from_nanos as builtin**

Find where `register_builtin_fn` calls are made (around line 579-605). Add:

```rust
self.register_builtin_fn("duration::from_nanos", vec![Ty::I64], Ty::Duration);
```

Also check how module-qualified builtins are resolved. Search for `::` handling in `check_call` or `check_path`. The `duration::from_nanos` syntax may need special handling if the checker resolves module paths differently from function names. Look at how `bytes::new()` is handled as a precedent.

**Step 3: Update | after to require Duration**

At line 4056, change:

```rust
// Before:
self.check_against(&duration.0, &duration.1, &Ty::I64);
// After:
self.check_against(&duration.0, &duration.1, &Ty::Duration);
```

**Step 4: Update select timeout to require Duration**

At line 4031, add a type check:

```rust
let dur_ty = self.synthesize(&tc.duration.0, &tc.duration.1);
let dur_resolved = self.subst.resolve(&dur_ty);
if !dur_resolved.is_duration() && dur_resolved != Ty::Error {
    self.report_error(
        TypeErrorKind::TypeMismatch,
        &tc.duration.1,
        format!("timeout requires `duration`, found `{dur_resolved}`"),
    );
}
```

**Step 5: Run tests**

Run: `make test-rust 2>&1 | tail -30`

Fix any existing tests that pass raw integers to `| after`.

**Step 6: Commit**

```
feat(types): duration::from_nanos constructor, | after requires duration
```

---

### Task 6: Serializer — Ty::Duration to TypeExpr

**Files:**
- Modify: `hew-serialize/src/enrich.rs:177-180` area

**Step 1: Add Duration serialization**

After `Ty::Char` mapping (line 177-180), add:

```rust
Ty::Duration => TypeExpr::Named {
    name: "duration".into(),
    type_args: None,
},
```

**Step 2: Fix any other exhaustive matches in the serializer**

Search `hew-serialize/` for matches on `Ty::` and add `Ty::Duration` arms where needed.

**Step 3: Run tests**

Run: `make test-rust 2>&1 | tail -30`

**Step 4: Commit**

```
feat(serialize): add Ty::Duration serialization
```

---

### Task 7: Codegen — Type mapping and method emission

**Files:**
- Modify: `hew-codegen/src/mlir/MLIRGen.cpp:252` (convertType)
- Modify: `hew-codegen/src/mlir/MLIRGenExpr.cpp:2990` area (method dispatch)

**Step 1: Add duration type mapping**

In `MLIRGen.cpp`, after the `"char"` check (line 252-253):

```cpp
if (name == "duration")
    return builder.getI64Type();
```

**Step 2: Add duration method codegen**

In `MLIRGenExpr.cpp`, before the numeric conversion methods section (line 2990), add:

```cpp
// --- Duration methods (.nanos, .micros, .millis, .secs, .mins, .hours, .abs, .is_zero) ---
// Duration is stored as i64 nanoseconds. Methods are inline integer division.
if (receiverType == builder.getI64Type()) {
    // Check if the resolved type from the type checker is duration.
    // Since duration maps to i64 in MLIR, we use the method name to distinguish.
    auto i64Type = builder.getI64Type();
    auto boolType = builder.getI1Type();

    if (method == "nanos") {
        return receiver;  // identity — already nanoseconds
    } else if (method == "micros") {
        auto divisor = mlir::arith::ConstantOp::create(builder, location, i64Type,
                           builder.getI64IntegerAttr(1000));
        return mlir::arith::DivSIOp::create(builder, location, receiver, divisor);
    } else if (method == "millis") {
        auto divisor = mlir::arith::ConstantOp::create(builder, location, i64Type,
                           builder.getI64IntegerAttr(1000000));
        return mlir::arith::DivSIOp::create(builder, location, receiver, divisor);
    } else if (method == "secs") {
        auto divisor = mlir::arith::ConstantOp::create(builder, location, i64Type,
                           builder.getI64IntegerAttr(1000000000));
        return mlir::arith::DivSIOp::create(builder, location, receiver, divisor);
    } else if (method == "mins") {
        auto divisor = mlir::arith::ConstantOp::create(builder, location, i64Type,
                           builder.getI64IntegerAttr(60000000000LL));
        return mlir::arith::DivSIOp::create(builder, location, receiver, divisor);
    } else if (method == "hours") {
        auto divisor = mlir::arith::ConstantOp::create(builder, location, i64Type,
                           builder.getI64IntegerAttr(3600000000000LL));
        return mlir::arith::DivSIOp::create(builder, location, receiver, divisor);
    } else if (method == "abs") {
        // abs for i64: if negative, negate
        auto zero = mlir::arith::ConstantOp::create(builder, location, i64Type,
                        builder.getI64IntegerAttr(0));
        auto neg = mlir::arith::SubIOp::create(builder, location, zero, receiver);
        auto isNeg = mlir::arith::CmpIOp::create(builder, location,
                         mlir::arith::CmpIPredicate::slt, receiver, zero);
        return mlir::arith::SelectOp::create(builder, location, isNeg, neg, receiver);
    } else if (method == "is_zero") {
        auto zero = mlir::arith::ConstantOp::create(builder, location, i64Type,
                        builder.getI64IntegerAttr(0));
        return mlir::arith::CmpIOp::create(builder, location,
                   mlir::arith::CmpIPredicate::eq, receiver, zero);
    }
}
```

**Important caveat:** Since duration maps to `i64` in MLIR, we can't distinguish `i64.nanos()` from `duration.nanos()` at the codegen level by type alone. The type checker already validated that only `Ty::Duration` values have these methods, so by the time we reach codegen, the method name on an i64 value is safe. However, if there's a `resolvedTypeOf()` mechanism that carries the Hew-level type, use that instead. Check how the codegen distinguishes String methods from other pointer methods for precedent.

**Step 3: Add duration::from_nanos codegen**

Search for how `bytes::new()` or similar module-qualified constructors are generated. `duration::from_nanos(x)` should just pass through the i64 value unchanged — it's a no-op at the MLIR level.

**Step 4: Build**

Run: `make 2>&1 | tail -30`

**Step 5: Commit**

```
feat(codegen): duration type mapping and inline method emission
```

---

### Task 8: E2E tests

**Files:**
- Create: `tests/duration_basic.hew`
- Create: `tests/duration_basic.expected`

**Step 1: Write E2E test program**

```hew
// tests/duration_basic.hew
fn main() {
    let d = 5s;
    let ms = d.millis();
    println(ms);           // 5000

    let sum = 1s + 500ms;
    println(sum.millis()); // 1500

    let scaled = 2s * 3;
    println(scaled.secs()); // 6

    let ratio = 10s / 2s;
    println(ratio);         // 5

    let neg = -1s;
    println(neg.abs().millis()); // 1000

    let d2 = duration::from_nanos(2000000000);
    println(d2.secs());    // 2

    let z = 0s;
    println(z.is_zero());  // true
}
```

**Step 2: Write expected output**

```
5000
1500
6
5
1000
2
true
```

**Step 3: Run E2E tests**

Run: `make test-codegen 2>&1 | tail -30`

**Step 4: Commit**

```
test: add duration type E2E tests
```

---

### Task 9: Update spec and docs

**Files:**
- Modify: `docs/specs/HEW-SPEC.md` (§10.3)
- Modify: `docs/specs/grammar.ebnf` (Type production)
- Modify: `docs/specs/Hew.g4` (type_ rule)

**Step 1: Update HEW-SPEC.md §10.3**

Remove the "Implementation note" about duration not being implemented. Update the section to document the implemented methods and arithmetic.

**Step 2: Add `duration` to grammar Type productions**

In `grammar.ebnf`, add `"duration"` to the Type production comment about type aliases.

In `Hew.g4`, add `'duration'` to `primitiveType`.

**Step 3: Commit**

```
docs: update spec and grammars for duration type
```

---

### Task 10: Final verification

**Step 1: Run full test suite**

Run: `make test`

**Step 2: Run lint**

Run: `make lint`

**Step 3: Fix any remaining issues**

**Step 4: Final commit if needed, then create PR**
