# Stdlib Type-Surface Style Contract

> **Audience:** anyone writing or reviewing a stdlib module (`std/**/*.hew`).

---

## Why this contract exists

v0.5 adopts an explicit-width-only integer naming policy. The `int` and `uint`
aliases (formerly compiler aliases for `i64` and `u64`) are removed. Stdlib
modules must use explicit widths on all public surfaces, removing the ambiguity
that led readers to assume Go/Swift platform-sized semantics.

---

## The invariants

### 1. Public API integer width

Every `pub fn` parameter, return type, trait-method signature, and struct field
**must** use an explicit-width integer:

- Signed: `i8`, `i16`, `i32`, `i64`
- Unsigned: `u8`, `u16`, `u32`, `u64`
- Platform-sized (pointer math only): `isize`, `usize`

The removed aliases `int` / `Int` / `uint` are **not valid type names** and will
produce a compile error with a suggestion to use `i64` or `isize`.

Choose `i64` by default for general-purpose integer values. Choose `usize` for
collection lengths and indices. Choose narrower widths only when the external
ABI requires it (e.g. C errno is `i32`, file descriptors are `i32`).

### 2. Internal ABI seams

`i32`/`i64` inside `extern "C" { ... }` blocks must match the runtime symbol
table exactly. Narrowing casts at the call site (`x as i32`) happen inside
`unsafe`, not in the public signature:

```hew
pub fn len(dq: Deque) -> i64 { unsafe { hew_deque_len(dq) } }

extern "C" {
    fn hew_deque_len(dq: Deque) -> i64;
}
```

### 3. Fallible conversion

The sole parse-to-integer shape is:

```hew
pub fn string_to_int(s: string) -> i64 { ... }
```

No sentinel returns (`-1` meaning "missing"). Express absence with
`Option<i64>` or `Result<i64, E>`.

### 4. Handle lifetime

Handle types must expose `close()` and/or `free()` as explicit trait methods.
Automatic drop is permitted, but the explicit release API must also exist.

### 5. Naming convention

Fallible variants are named `try_*`. Non-fallible variants either panic or
return a documented default. Dual variants come in pairs (`read`/`try_read`).

### 6. Wire annotations

`#[wire]`-annotated types use explicit-width integer fields. Choose the width
that matches the schema contract — `i64` for general counters, narrower widths
only when the wire protocol requires them.

---

## Canonical example

```hew
trait DequeMethods {
    fn push_front(dq: Deque, value: i64);
    fn pop_front(dq: Deque) -> i64;
    fn len(dq: Deque) -> i64;
}

impl DequeMethods for Deque {
    fn push_front(dq: Deque, value: i64) { unsafe { hew_deque_push_front(dq, value) }; }
    fn pop_front(dq: Deque) -> i64 { unsafe { hew_deque_pop_front(dq) } }
    fn len(dq: Deque) -> i64 { unsafe { hew_deque_len(dq) } }
}

extern "C" {
    fn hew_deque_push_front(dq: Deque, value: i64);
    fn hew_deque_pop_front(dq: Deque) -> i64;
    fn hew_deque_len(dq: Deque) -> i64;
}
```

---

## Migration from `int`

If you encounter `int` or `uint` in a `.hew` file, replace with `i64` or `u64`
respectively. The compiler will suggest the replacement:

```
error: unknown type `int`; use `i64` for fixed 64-bit integers or `isize` for
       pointer-sized integers
```

The `scripts/lint-stdlib-int-surface.sh` script may still reference the old
`int` surface contract — it should be updated or removed.
