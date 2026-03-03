# Collection Literal Ergonomics

Three changes to make Vec and HashMap initialization more ergonomic through type-driven coercion.

## Feature 1: Fix empty `[]` coercion to Vec

`let v: Vec<i32> = [1, 2, 3]` already works via array-to-Vec coercion in the type checker. But `let v: Vec<i32> = []` fails at codegen because `generateArrayExpr()` returns nullptr for empty arrays, causing the let statement to skip variable declaration entirely.

**Fix:** In `generateArrayExpr()`, when the array is empty and `pendingDeclaredType` is `Vec<T>`, emit `VecNewOp` instead of returning nullptr.

**Files:** `hew-codegen/src/mlir/MLIRGenExpr.cpp` (generateArrayExpr, ~line 3334)

## Feature 2: Empty map literal `{}`

`let m: HashMap<String, i32> = {}` coerces to an empty HashMap when the type context expects it.

**Parser:** `{}` remains a Block (changing it to MapLiteral would break every `_ => {}` and `None => {}` in the codebase). The disambiguation happens in the type checker instead.

**AST:** Add `MapLiteral { entries: Vec<(Spanned<Expr>, Spanned<Expr>)> }` to the `Expr` enum (used by Feature 3).

**Type checker:** In `check_against`, when expected type is `HashMap<K,V>` and expression is an empty Block (no stmts, no trailing expr), coerce to the expected HashMap type.

**Codegen:** When an empty Block has `pendingDeclaredType` of `HashMapType`, emit `HashMapNewOp` directly instead of generating a block.

## Feature 3: Initialized map literal `{"key": value, ...}`

`{"a": 1, "b": 2}` coerces to `HashMap<String, i32>` when the type context expects it.

**Parser:** Extend `{` handling: if the next token after `{` is a string literal followed by `:`, parse as a map literal with comma-separated `expr : expr` entries. Otherwise fall back to block parsing.

**Type checker:** When expected type is `HashMap<K,V>` and expression is `MapLiteral` with entries, check each key against `K` and each value against `V`.

**Codegen:** Emit `HashMapNewOp` followed by `HashMapInsertOp` for each entry.

## Parsing disambiguation

- `{}` → Block (empty HashMap coercion handled by type checker when expected type is HashMap)
- `{"string": expr, ...}` → MapLiteral (2-token lookahead: `StringLit` then `:` after `{`)
- `{ stmt; ... }` → Block (anything else)

## Serialization

Add `MapLiteral` to the MessagePack serialization layer alongside the existing `Array` handling. The entries serialize as pairs of expressions.

## Testing

Each feature gets E2E tests:
- `let v: Vec<i32> = []; v.push(1); println(v.len());` → `1`
- `let m: HashMap<String, i32> = {}; m.insert("a", 1); println(m.len());` → `1`
- `let m: HashMap<String, i32> = {"a": 1, "b": 2}; println(m.len()); println(m.get("a"));` → `2\nSome(1)`
