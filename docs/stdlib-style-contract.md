# Stdlib `int`-Surface Style Contract

> **Audience:** anyone writing or reviewing a stdlib module (`std/**/*.hew`).
> This contract is enforced by `scripts/lint-stdlib-int-surface.sh` and by the
> `stdlib-lint` CI workflow.

---

## Why this contract exists

`int` is the user-facing integer type in Hew. It is a compiler alias for
`Ty::I64` (64-bit, platform-native sign extension). Historically, stdlib
modules have mixed `int`, `i32`, and `i64` on their public surfaces, creating
two observable problems:

1. **Usability.** A user passes an `int` literal to a function that demands
   `i32` and gets a type error whose root cause is a stdlib implementation
   detail, not the user's code.
2. **Drift.** New stdlib modules copy an old one and re-introduce
   width-specific public APIs. Without a written contract or a lint gate,
   corrective one-module-per-PR migrations recur indefinitely (33 such PRs
   were counted in the v0.4 cluster-F review).

This contract draws a hard line between the public-facing surface (always
`int`) and the C-ABI seam (allowed `i32`/`i64` with a required comment).

---

## The six invariants

### 1. Public API width

Every `pub fn` parameter, return type, trait-method signature, and any struct
field that is accessible through a public handle accessor **must** use `int`
(or a parameterised wrapper such as `Option<int>`, `Result<int, _>`,
`Vec<int>`).

Width-specific types (`i32`, `i64`) are **forbidden** on the public surface.

### 2. Internal ABI seams

`i32`/`i64` are permitted in exactly two locations:

- Inside an `extern "C" { ... }` block — these are ABI declarations against
  runtime C symbols and must match the runtime symbol table exactly.
- On a line that ends with a `// INTERNAL-ABI: <one-sentence reason>` comment.
  This covers struct fields that must be 32-bit to match a C struct layout, or
  narrowing coercions at the call site:

  ```hew
  unsafe { hew_foo(x as i32) }  // INTERNAL-ABI: runtime symbol takes i32 status code
  ```

The `<reason>` must be a complete sentence that explains *why the width is
mandated externally*, not just *what the type is*.

### 3. Fallible conversion

The sole parse-to-integer shape is:

```hew
pub fn try_to_int(s: String) -> Result<int, ParseError>
```

No `i32`/`i64` sentinel returns (`-1` meaning "missing"). If a callee needs
to express absence, it returns `Option<int>` or `Result<int, E>`.

### 4. Handle lifetime

A handle type is any type that has an `extern "C" { fn hew_*_free(...); }`
declaration. Handle types that own OS resources must expose `close()` and/or
`free()` as explicit trait methods. Automatic drop is permitted, but the
explicit release API must also exist.

### 5. Naming convention

Fallible variants are named `try_*`. Non-fallible variants either panic on
error or return a zero-valued default (documented in the `///` doc comment).
Dual variants always come in pairs (`read`/`try_read`). Width-specific
overload suffixes (`_i32`, `_i64`) are deprecated: use the unsuffixed name
with `int` instead.

### 6. Wire annotations and `int` width

`#[wire]`-annotated types use `int` in their fields. The wire codec layer is
free to encode a narrower representation on the wire when the schema calls for
it, but the Hew-visible type is always `int`. Cross-node messaging does not
require `i32`/`i64` on the Hew source.

---

## Canonical examples

### Clean pattern — `std/deque.hew:31-45`

```hew
trait DequeMethods {
    fn push_front(dq: Deque, value: int);
    fn pop_front(dq: Deque) -> int;
    fn len(dq: Deque) -> int;
    // ...
}

impl DequeMethods for Deque {
    fn push_front(dq: Deque, value: int) { unsafe { hew_deque_push_front(dq, value as i64) }; }
    fn pop_front(dq: Deque) -> int { unsafe { hew_deque_pop_front(dq) } }
    fn len(dq: Deque) -> int { unsafe { hew_deque_len(dq) } }
}

// extern "C" block uses i64 to match the runtime C ABI exactly:
extern "C" {
    fn hew_deque_push_front(dq: Deque, value: i64);
    fn hew_deque_pop_front(dq: Deque) -> i64;
    fn hew_deque_len(dq: Deque) -> i64;
}
```

The public surface uses `int`. The `extern "C"` block uses `i64`. The
narrowing/widening (`value as i64`) happens at the call site inside `unsafe`,
not in the signature.

### Known leak — `std/channel/channel.hew:119` (closed by foundations/stdlib-int-surface)

```hew
// Before migration — wrong:
pub fn new(capacity: i64) -> (Sender, Receiver) { ... }

// After migration — correct:
pub fn new(capacity: int) -> (Sender, Receiver) { ... }
```

---

## The `std/builtins.hew` exemption

`std/builtins.hew` is excluded from the lint. Its contents are
compiler-intrinsic *declarations* that must literally match the type
registrations in `hew-types/src/check/registration.rs`. If those registrations
use `i64`, the declaration must too. This exemption is a named-file exact
match — it does not extend to any other module.

---

## Lint and enforcement

The lint lives at `scripts/lint-stdlib-int-surface.sh`. It is wired into:

- A pre-push hook (install: `ln -sf ../../../scripts/pre-push-stdlib-int.sh .git/hooks/pre-push.d/stdlib-int-surface`)
- The `stdlib-lint` CI workflow (`.github/workflows/stdlib-lint.yml`)

Running the lint locally:

```bash
bash scripts/lint-stdlib-int-surface.sh
```

A clean stdlib prints nothing and exits 0. A violation prints the file, line
number, and offending text, then exits 1 with a pointer back to this document.

---

## LESSONS rows strengthened by this contract

- **`spec-surface-alignment` (P1):** After this contract lands, stdlib `.hew`
  sources are a second authority that must match `hew-types/src/check/registration.rs`.
  A `pub fn` using `i32`/`i64` without `// INTERNAL-ABI:` or `extern "C"` scope
  is a fail-closed violation.
- **`shared-user-surface-paths` (P1):** Every stdlib module is a user-facing
  surface; dual `i32`/`i64`/`int` coexistence is drift between public-facing
  and ABI-internal entry points. This contract and its lint explicitly partition them.
- **`serializer-fail-closed` (P0):** The JSON/YAML/TOML bool-encoding shims
  (`set_bool`, `array_push_bool`) pass `val as i32`. This is a `bool → i32`
  conversion at the C boundary and is a permitted internal-ABI shape. It must
  NOT be "cleaned up" by coercing through `bool → int → i32`.
- **`generated-narrowing-guards` (P0):** The `as i32` / `as i64` casts at
  `extern "C"` call sites are the only legal location for width narrowing in
  stdlib source.
