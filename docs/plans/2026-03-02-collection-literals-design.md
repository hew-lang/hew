# Collection Literal Ergonomics

Three changes to make Vec and HashMap initialization more ergonomic through type-driven coercion.

## Feature 1: Fix empty `[]` coercion to Vec

`let v: Vec<i32> = [1, 2, 3]` already works via array-to-Vec coercion in the type checker. But `let v: Vec<i32> = []` fails at codegen because `generateArrayExpr()` returns nullptr for empty arrays, causing the let statement to skip variable declaration entirely.

**Fix:** In `generateArrayExpr()`, when the array is empty and `pendingDeclaredType` is `Vec<T>`, emit `VecNewOp` instead of returning nullptr.

**Files:** `hew-codegen/src/mlir/MLIRGenExpr.cpp` (generateArrayExpr, ~line 3334)

## Feature 2: Empty map literal `{}`

Add `{}` as an empty map literal that coerces to `HashMap<K,V>` when the type context expects it.

**Parser:** When `{` is immediately followed by `}` in expression position, parse as `Expr::MapLiteral { entries: [] }` instead of an empty block. Empty blocks evaluate to Unit and are rarely useful standalone.

**AST:** Add `MapLiteral { entries: Vec<(Spanned<Expr>, Spanned<Expr>)> }` to the `Expr` enum.

**Type checker:** In `check_against`, when expected type is `HashMap<K,V>` and expression is `MapLiteral` with zero entries, coerce to the expected HashMap type.

**Codegen:** Generate `HashMapNewOp` for empty `MapLiteral`.

## Feature 3: Initialized map literal `{"key": value, ...}`

`{"a": 1, "b": 2}` coerces to `HashMap<String, i32>` when the type context expects it.

**Parser:** Extend `{` handling: if the next token after `{` is a string literal followed by `:`, parse as a map literal with comma-separated `expr : expr` entries. Otherwise fall back to block parsing.

**Type checker:** When expected type is `HashMap<K,V>` and expression is `MapLiteral` with entries, check each key against `K` and each value against `V`.

**Codegen:** Emit `HashMapNewOp` followed by `HashMapInsertOp` for each entry.

## Parsing disambiguation

- `{}` → MapLiteral (empty)
- `{"string": expr, ...}` → MapLiteral (string-keyed, unambiguous since `"string": expr` is never valid in a block)
- `{ stmt; ... }` → Block (anything else)
- `{ () }` → explicit empty block if ever needed

## Serialization

Add `MapLiteral` to the MessagePack serialization layer alongside the existing `Array` handling. The entries serialize as pairs of expressions.

## Testing

Each feature gets E2E tests:
- `let v: Vec<i32> = []; v.push(1); println(v.len());` → `1`
- `let m: HashMap<String, i32> = {}; m.insert("a", 1); println(m.len());` → `1`
- `let m: HashMap<String, i32> = {"a": 1, "b": 2}; println(m.len()); println(m.get("a"));` → `2\nSome(1)`
