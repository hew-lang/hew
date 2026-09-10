# Hew Language Guide

A guide to the native language surface. Examples describe the current contracts;
implementation gaps are identified where relevant. This document is not a record
of execution on every target. See the [language specification](specs/HEW-SPEC-2026.md)
for the full contract.

An actor gets its own handle by writing `self`, so it can hand another actor a way to call back:

<!-- doctest: skip -->

```hew
actor Worker {
    var registry: LocalPid<Registry>,
    receive fn enrol() {
        let _ = registry.take(self);   // `self` is this worker's LocalPid<Worker>
    }
    receive fn done() { println("called back"); }
}
```

## Toolchain quick-start

```
# Scaffold an idiomatic package, then work from anywhere inside it
hew init myproject
cd myproject

# Check, build, and run the entry point from hew.toml
hew check
hew build
hew run

# Pass arguments to your program (the -- separator is required)
hew run -- Alice 42

# Explicit-file forms remain available for standalone sources
hew check hello.hew
hew build hello.hew
hew run hello.hew
```

`hew build`, `hew run`, and `hew check` locate the nearest `hew.toml` by
walking up from the current directory. You can also name a package directory,
as in `hew build .` or `hew build path/to/package`. The optional
`[package] main` field selects an entry point relative to the manifest and
defaults to `main.hew`. A build writes the binary into the package root, named
after the last dotted component of the package name; `-o PATH` overrides it.
If the package declares `[native]`, Hew builds that crate first and links it as
a prerequisite.

The `--` separator is mandatory when passing program arguments — everything before `--` is parsed as `hew run` options, and everything after is forwarded to your program as `os.args()`. Without `--`, unrecognised flags produce a usage error.

**Working inside the Hew source checkout?** An in-checkout compiler can resolve
the repository's `std/` through the development fallback. Set `HEWPATH` or
`HEW_STD` when selecting an alternate installation or when another directory
layout would otherwise be ambiguous. See
[Module search paths & stdlib discovery](../README.md#module-search-paths--stdlib-discovery)
for the documented resolver precedence.

## Core idioms

- Primitive types are lowercase: `i64`, `string`, `f64`, `bool`, `char` — never `Int`, `String`, `Float`.
- Integer literals default to `i64`, float literals to `f64`. Contextual types select the integer range, including the full `u64` range; there is no bigint source type.
- Interpolate `Display` values with `f"x={x}"`. Structural debug rendering is a future idea, not a current formatting contract.
- Convert numbers with `as`: `x as i64`, `pi as i32`. It is the only conversion mechanism.
- Never mix integer widths in one expression; cast the narrower operand up first: `x as i64 + 1`.
- **`var` for mutable bindings, `let` for immutable.** Hew does not have `let mut` — use `var` whenever you need to reassign a name, assign through a place (`p.x`, `v[0]`, `m["k"]`, `t.0`), or call a method that mutates its receiver. Collections are values, so `var v: Vec<i64> = Vec.new(); v.push(1)` is the mutating form and `let v` refuses it. Handles are the exception by category, not by syntax: `let d = deque.new(); d.push_back(1)` is fine because `d` names a resource rather than holding a value.
- Dispatch with `match`, not if/else chains — `match` on a closed enum enforces exhaustiveness.
- Iterate counts with `for i in 0..n` (exclusive) or `0..=n` (inclusive); the binding must be a named identifier.
- Read collection elements with `v[i]` (returns `T`, traps on out-of-bounds) or `.get(i)` (returns `Option<T>`, never traps) — both universal across element types.
- `Vec<string>` supports `v[i]` (returns a fresh owned `string`; the Vec stays usable), `.get(i)`, range-slices, and for-in. Both accessors work for `Vec<enum>` too.
- Build maps/sets with `Type.new()` + `.insert()`; bind with `var`, since `.insert()` mutates the receiver.
- Use `.get` for optional collection reads and `match`, `??`, `?` or expression-local `handle` as appropriate. Use `expect(reason)` for a deliberate invariant assertion.
- Commas separate record fields and enum variants in declarations and values. Semicolons terminate executable statements and bodyless function declarations.
- Declare records with `type Name { field: T, }` and enums with `enum Choice { First, Second, }`.
- Inside an actor body, `self` as a value is its handle (`LocalPid<Self>` in this build); `self.field` accesses state, and bare field names also work. `this` is not a keyword.
- Every actor call waits for completion, including a void handler. Use `mailbox(target, on_full: ...)` for submission-only delivery; handle its outcome.
- Ask (request-reply) is `ref.method(arg)` and returns `Result<R, ActorError>` — match `Ok`/`Err`. The call waits; `fork` runs it concurrently.
- Sending a value into an actor delivers a logical snapshot; the sender's binding stays valid afterward — no `clone` needed to keep using it. Types that cannot be sent are rejected at compile time.
- Ordinary data has value semantics. Mutating one value does not silently mutate another; borrowing and consuming uses are checked, including collection-element loans.
- Last expression of a block (no trailing semicolon) is its value; a trailing `;` makes it unit.
- Lean on the safe stdlib trio: `std.string`, `std.math`, `std.iter`. Do not import `std.option`/`std.result`.
- Negate a bool with `!`: `!x` is `true` when `x` is `false`.
- `break` and `continue` work in both `loop {}` and `while` loops.
- Iterate all HashMap keys with `.keys()` → `Vec<string>`; all values with `.values()` → `Vec<V>`.
- Coming from Rust/Go/TS: there is no `struct` keyword — `type` declares both plain data records and generic containers, so one keyword covers what Rust splits into `struct`/`enum` shapes. Writing `struct` is a parse error with a `write \`type Name { ... }\`` hint.

## Primitives

### Type names and literal defaults

```hew
fn main() {
    let a = 42;          // i64
    let c: i32 = 7;
    let f = 3.14;        // f64
    let h: bool = true;
    let j: char = 'A';
    let k: string = "hello";
    println(f"a={a} c={c} f={f} h={h} j={j} k={k}");
}
```

Use lowercase names always. Let literals default (`i64`/`f64`); annotate only for narrower widths. `char` uses single quotes, `string` double; they are distinct types. The compiler rejects `Int`/`int`/`String`/`uint` and names the correct type in the diagnostic.

### Integer and string literal forms

```hew
fn main() {
    let hex = 0xFF;
    let bin = 0b1010;
    let oct = 0o17;
    let big = 1_000_000;
    let s: string = "a\tb \"q\"";
    println(f"hex={hex} bin={bin} oct={oct} big={big}");
    println(s);
}
```

Use `0x`/`0b`/`0o` prefixes and `_` digit separators. Escapes are C-style: `\n \t \" \' \\`.

### f-string interpolation

```hew
fn main() {
    let a = 42;
    let name = "hew";
    println(f"a={a} name={name} next={a + 1} len={name.len()}");
}
```

Prefix interpolated strings with `f`. Expressions inside braces use Display;
use an explicit Display implementation to choose the user-facing text.
Structural inspection is not a promised alternative formatting API.

### Numeric conversion via `as`

```hew
fn main() {
    let i: i32 = 42;
    let w: i64 = i as i64;      // widening
    let f: f64 = i as f64;      // int -> float
    let pi: f64 = 3.99;
    let n: i32 = pi as i32;     // float -> int, truncates toward zero -> 3
    println(f"w={w} f={f} n={n}");
}
```

Use `as` for all defined numeric conversions: widening, narrowing, int↔float, and signed↔unsigned. Integer narrowing keeps the target width's low bits. Integer→float and float→float casts round to the target float type. Float→int casts truncate toward zero for in-range finite values and use defined saturation for every edge case:

| Source value                              | Signed result      | Unsigned result    |
| ----------------------------------------- | ------------------ | ------------------ |
| In-range finite                           | Truncated toward 0 | Truncated toward 0 |
| `+Inf` or greater than the target maximum | Target maximum     | Target maximum     |
| `-Inf` or less than the target minimum    | Target minimum     | `0`                |
| `NaN`                                     | `0`                | `0`                |

Use `.try_to_X()` when a conversion must fail instead of losing information. It returns `Option<X>` and produces `Some` only when the value round-trips through the target type exactly.

```hew
fn main() {
    let bytes: i64 = 255;
    let byte: Option<u8> = bytes.try_to_u8();       // Some(255)

    let negative: i32 = -1;
    let unsigned: Option<u32> = negative.try_to_u32(); // None

    let exact: i32 = 16777216;
    let exact_float: Option<f32> = exact.try_to_f32(); // Some(16777216.0)

    let inexact: i32 = 16777217;
    let rounded_float: Option<f32> = inexact.try_to_f32(); // None
}
```

The available methods are `.try_to_i8()`, `.try_to_i16()`, `.try_to_i32()`, `.try_to_i64()`, `.try_to_u8()`, `.try_to_u16()`, `.try_to_u32()`, `.try_to_u64()`, `.try_to_isize()`, `.try_to_usize()`, `.try_to_f32()`, and `.try_to_f64()`. They return `None` for out-of-range values, negative values converted to unsigned targets, `NaN`, infinities, nonzero fractional parts in float→integer conversions, and inexact integer→float or float→float conversions.

`char as <integer>` extracts the Unicode scalar value — the codepoint. `'A' as i64` is `65`, and `s[i] as i64` reads the codepoint of the char at codepoint offset `i` (the read primitive for text/byte parsers). Wider integer targets zero-extend; narrower ones truncate to the low byte (`'€' as u8` is `172`), following the same width rules as above. The reverse — `<integer> as char` — is not an `as` cast (not every integer is a valid scalar value); build a one-character string with `string.from_char(code)` instead.

### Mixed-width arithmetic requires a cast

```hew
fn main() {
    let x: i32 = 1;
    let y: i64 = x as i64 + 1;   // cast the narrow operand up first
    println(f"y={y}");
}
```

Never mix widths in one expression. Cast the narrower operand with `as` before combining; integer literals adopt the surrounding type.

### Explicit-intent arithmetic — `.wrapping_*` / `.checked_*` / `.saturating_*`

```hew
fn main() {
    let a: i32 = 2147483647;
    let wrapped: i32 = a.wrapping_add(1);
    println(wrapped);          // -2147483648 (wraps around)

    let checked: Option<i32> = a.checked_add(1);
    match checked {
        .Some(v) => println(v),
        .None => println("overflow"),   // overflow
    }

    let clamped: i32 = a.saturating_add(1);
    println(clamped);          // 2147483647 (clamped to i32.MAX)
}
```

The plain `+`, `-`, `*` operators **trap on overflow** — Hew's default
integer arithmetic is checked, not wrapping. Every integer type (`i8`…`i64`,
`isize`, `u8`…`u64`, `usize`) also has three `add`/`sub`/`mul` method
families for opting into a different overflow policy explicitly.
`.wrapping_add()`/`.wrapping_sub()`/`.wrapping_mul()` wrap on overflow — the
same behaviour §12.2 documents for the `&+`/`&-`/`&*` operators, spelled as a
method rather than an operator. `.checked_add()`/`.checked_sub()`/
`.checked_mul()` return `Option<T>` (`None` on overflow, `Some(v)`
otherwise). `.saturating_add()`/`.saturating_sub()`/`.saturating_mul()` clamp
to the type's max/min instead of wrapping or trapping.

Only `add`/`sub`/`mul` are implemented today — no `div`/`rem`/shift variants
exist yet, and none of the three families apply to `f32`/`f64`. These
methods and the `&+`/`&-`/`&*` operators are the explicit-intent spellings
for wrapping (or clamping, or `Option`-returning) arithmetic; the plain
operators keep trapping by default.

## Bindings — var and let

### var for mutable, let for immutable

```hew
fn main() {
    var count = 0;
    count = count + 1;
    count += 1;
    println(count);          // 2

    var greeting = "hello";
    greeting = greeting + " world";
    println(greeting);       // hello world

    let frozen = 42;
    // frozen = 43;           // compile error: cannot assign to immutable binding
    println(frozen);         // 42
}
```

> **Coming from Rust?** Hew does not have `let mut`. Use `var` for any binding you will reassign; use `let` for everything else. Writing `let mut x = 0` is one compile error carrying the fix-it that rewrites it to `var x = 0`. `mut` stays reserved because `*mut T` uses it in foreign declarations, but it is never a binding modifier.

Use `var` when the name will be reassigned, when a field or element under it will be assigned, or when you will call a method that mutates the receiver. Collections are values, so `.push()`/`.insert()` are `var self` methods: `var v: Vec<i64> = Vec.new(); v.push(1)` is the working form and a `let` binding refuses the call. Handles are different — `let d = deque.new(); d.push_back(1)` is fine, because `d` names a resource instead of holding a value.

Compound-assign operators (`+=`, `-=`, `*=`, `/=`) are available for `var` bindings.

The `!` prefix operator negates a `bool`:

```hew
fn main() {
    let x = true;
    println(!x);               // false
    let empty = false;
    if !empty { println("not empty"); }  // not empty
}
```

`!` is the logical NOT operator. You can also use `== false` or `!= true` — all three spellings are accepted, but `!x` is the shortest form.

### var binding with a type annotation

```hew
fn main() {
    var total: i64 = 0;
    var items: Vec<string> = Vec.new();
    items.push("a");
    items.push("b");
    for s in items { total += s.len() as i64; }
    println(total);   // 2
}
```

Annotate a `var` (or `let`) binding when the type cannot be inferred from the initial value (e.g. an empty `Vec.new()`). Use `var` only when the name itself will be rebound; `let` is correct for a collection you only mutate via methods — the compiler warns if you `var`-declare a binding that is never rebound.

## Control flow

### if/else as an expression

```hew
fn grade(n: i64) -> string {
    if n >= 90 { "A" } else if n >= 80 { "B" } else { "C" }
}
```

Use if/else as an expression to produce a value; the last expression of each block is its value. An `if` used as an expression needs an `else`, and every branch must produce the same type.

### match on literals with wildcard

```hew
fn name_of(day: i64) -> string {
    match day {
        1 => "Mon",
        2 => "Tue",
        3 => "Wed",
        _ => "other",
    }
}
```

`match` returns a value. Arms are comma-separated; `_` is the catch-all final arm.

### match guards and block-bodied arms

```hew
fn classify(n: i64) -> string {
    match n {
        x if x < 0 => "negative",
        0 => "zero",
        x if x % 2 == 0 => { "even" },
        _ => "odd",
    }
}
```

Bind with a name then guard: `x if cond =>`. A block-bodied arm `=> { ... }` still needs a trailing comma after `}`. Guards do not contribute to exhaustiveness — keep a `_` or unguarded fallback.

### match on enum variants binding payloads

```hew
enum Event { Number(i64), Text(string), Empty, }
fn describe(e: Event) -> string {
    match e {
        .Number(n) if n > 100 => "big number",
        .Number(n) => "number",
        .Text(s) => s,
        .Empty => "empty",
    }
}
```

Variant arms bind their payload positionally. Construct values with dotted variant names (`.Number(5)`, `.Empty`). Enum variants are separated by commas.

### match exhaustiveness is enforced

```hew
enum Colour { Red, Green, Blue, }
// match c { .Red => 1, .Green => 2 }  // compile error: non-exhaustive match: missing Blue
fn code(c: Colour) -> i64 { match c { .Red => 1, .Green => 2, .Blue => 3 } }
```

Omit `_` when matching a closed enum so the compiler forces every variant. A missing variant is a hard error naming it.

### Full-field record pattern

```hew
type Point { x: i64, y: i64, }
fn sum(p: Point) -> i64 {
    match p {
        Point { x, y } => x + y,
    }
}
```

Destructure a record by naming every field. There is no `{ .. }` rest pattern; bind fields you do not need to a throwaway name.

### ranges in for-loops

```hew
fn main() {
    var sum = 0;
    for i in 0..5 { sum += i; }      // 0..n exclusive -> 0+1+2+3+4 = 10
    var count = 0;
    for i in 0..=3 { count += 1; }   // 0..=n inclusive -> 0,1,2,3 = 4
    println(f"sum={sum} count={count}");
}
```

`0..n` iterates 0 through n-1; `0..=n` includes n. The loop binding may be a named identifier or `_` (wildcard). Use `_` when the loop body does not need the iteration value.

### for x in collection

```hew
fn main() {
    var v: Vec<i64> = Vec.new();
    v.push(10); v.push(20); v.push(30);
    var total = 0;
    for x in v { total += x; }
    println(total);   // 60
}
```

`for x in collection` binds each element value. Index Vec elements with `v[i]` (traps on out-of-bounds) or `.get(i)` (returns `Option<T>`).

### while loop

```hew
fn main() {
    var n = 27;
    var steps = 0;
    while n != 1 {
        if n % 2 == 0 { n = n / 2; } else { n = 3 * n + 1; }
        steps += 1;
    }
    println(steps);   // 111
}
```

Condition is a bare expression (no parens); body is a brace block. Compound-assign (`+=`) is available. Reach for `while` only when the loop is genuinely condition-driven — for a fixed count use `for i in 0..n` (or `(0..n).rev()` / `.step_by(k)`), which says the iteration bound up front instead of hand-rolling an init/increment.

### loop + break (statement form)

```hew
fn main() {
    var n = 0;
    loop {
        n += 1;
        if n == 5 { break; }
    }
    println(n);   // 5
}
```

`loop` is a statement, not an expression — capture results in a `var` declared before the loop, then `break`.

### break and continue in while loops

```hew
fn main() {
    var i = 0;
    while i < 10 {
        if i == 7 { break; }
        if i % 2 == 0 { i += 1; continue; }
        println(i);   // 1, 3, 5
        i += 1;
    }
}
```

`break` and `continue` work inside `while` loops, not just `loop` blocks. `break` exits the loop immediately; `continue` skips the rest of the body and re-evaluates the condition.

### if let on Option

```hew
fn main() {
    let some: Option<i64> = .Some(42);
    if let .Some(v) = some { println(v); } else { println(-1); }
}
```

Use `if let .Some(v) = opt` for a one-armed destructure; prefer `match` when you want the unwrapped-or-default as an expression result. Every pattern `match` accepts works here and in `while let`: unit variants, records, tuples, literals, or-patterns and nested patterns.

### Chained conditions

A condition joins `let` patterns and boolean tests with `&&`. Each `let` binds its names for the tests to its right and for the block; nothing it binds is visible in the `else` arm, and the condition stops at the first operand that fails.

```hew
fn main() {
    let first: Option<i64> = .Some(20);
    let second: Result<string, string> = .Ok("ready");
    if let .Some(n) = first && n > 10 && let .Ok(text) = second {
        println(f"{n}: {text}");
    } else {
        println("not ready");
    }
}
```

`||` cannot join a `let` pattern — write the alternatives as separate arms, or match on the value.

### let … else

A `let` with a refutable pattern takes an `else` block for the no-match path. The block must diverge, so the bindings are live for the rest of the enclosing block:

```hew
import std.string;

fn port(raw: Option<string>) -> Result<i64, string> {
    let .Some(text) = raw else {
        return .Err("port missing");
    };
    let .Ok(value) = string.to_int(text) else {
        return .Err("port is not a number");
    };
    .Ok(value)
}
```

A refutable pattern in a plain `let` without `else` is `E_REFUTABLE_LET`; an `else` block that can fall through is `E_LET_ELSE_FALLTHROUGH`.

## Collections — Vec

### Construct, populate, measure

```hew
fn main() {
    var v: Vec<i64> = Vec.new();
    v.push(10);
    v.push(20);
    v.push(30);
    println(v.len());   // 3
    println(v[0]);      // 10
}
```

Annotate the binding type so the element type is inferred. `.len()` returns `i64`.

### v[i] — trapping element accessor

```hew
type Point { x: i64, y: i64, }
fn main() {
    var v: Vec<Point> = Vec.new();
    v.push(Point { x: 1, y: 2 });
    v.push(Point { x: 3, y: 4 });
    let p = v[1];
    println(p.x);  // 3
}
```

```hew
fn main() {
    var names: Vec<string> = Vec.new();
    names.push("ada");
    names.push("alan");
    let who = names[1];        // owned string read; names retains its element
    println(who);              // alan
    println(names[0]);         // ada — names is still usable
}
```

`v[i]` traps on an out-of-bounds index. For supported cloneable elements,
it reads an owned value without moving the element out; a concrete
`Vec<string>` read leaves the vector usable and may copy or share storage.
Clone-free elements are borrowed instead. In a generic body with unbounded
`T`, indexing borrows at every instantiation, even when instantiated with a
cloneable type. A borrowed read cannot consume the element; use `into_iter()`
when ownership is needed. This does not promise admission for every type.

### .get(i) — safe Option accessor

```hew
fn main() {
    var v: Vec<string> = Vec.new();
    v.push("a");
    v.push("b");
    match v.get(0) {
        .Some(s) => println(s),   // a
        .None => println("oob"),
    }
}
```

`.get(i)` returns `Option<T>` for every element type, including `Vec<string>`
and `Vec<enum>`. It never traps: an out-of-bounds index yields `None`. Consume
it with `match` (or guard `i < v.len()` and index with `v[i]` when you only need
the trapping read). Use `.get(i)` whenever the index may be invalid.

### Range-slice v[a..b] returns a new Vec

```hew
fn main() {
    var v: Vec<i64> = Vec.new();
    v.push(10); v.push(20); v.push(30); v.push(40);
    let s = v[1..3];
    println(s.len());    // 2
    println(s[0]);       // 20
}
```

`v[a..b]` yields a fresh `Vec<T>` (half-open, `b` exclusive) with its own
`.len()`/`.get()`/for-in, for every element type including `Vec<string>`.
(Single elements come straight from `v[i]`; the older `v[i..i+1]` slice is no
longer required for that.)

### for-in over Vec

```hew
fn main() {
    var v: Vec<string> = Vec.new();
    v.push("alpha");
    v.push("beta");
    for s in v {
        println(s);
    }
}
```

Prefer for-in for read-only traversal of any element type, including
`Vec<enum>`.

### Index-loop with v[i] or .get(i)

```hew
enum Colour { Red, Green, Blue, }
fn main() {
    var v: Vec<Colour> = Vec.new();
    v.push(Colour.Red);
    v.push(Colour.Blue);
    let c = v[1];
    match c {
        Colour.Red => println("red"),
        Colour.Green => println("green"),
        Colour.Blue => println("blue"),
    }
}
```

When you index by a computed offset, `v[i]` returns the element and traps if the
offset is out of range; `v.get(i)` returns `Option<T>` and yields `None`
instead. Both work for every element type, enums included. Hoist `let n =
v.len()` before a `for i in 0 .. n` loop.

### Out-of-bounds and .pop()

```hew
fn main() {
    var v: Vec<i64> = Vec.new();
    v.push(1);
    v.push(2);
    let last = v.pop();   // i64 — the removed element, returned directly
    println(last);        // 2
}
```

`v[i]` and `.pop()` return `T` directly and trap on a bad index or empty vec;
`.get(i)` returns `Option<T>` and never traps. Guard with `i < v.len()` (and
check `.len()` before `.pop()`), or use `.get(i)` for the non-trapping read.

### .set(i, v) — write by index

```hew
fn main() {
    var v: Vec<i64> = Vec.new();
    v.push(10); v.push(20); v.push(30);
    v.set(1, 99);
    println(v[0]);   // 10
    println(v[1]);   // 99
    println(v[2]);   // 30
}
```

`v.set(i, value)` overwrites the element at index `i` in place. It complements `v[i]` (read) — reading uses the subscript, writing uses `.set()`. Out-of-bounds traps at runtime; guard with `i < v.len()`.

### .contains(), .clear(), .append()

```hew
fn main() {
    var v: Vec<i64> = Vec.new();
    v.push(1); v.push(2); v.push(3);
    println(v.contains(2));   // true
    println(v.contains(9));   // false

    var v2: Vec<i64> = Vec.new();
    v2.push(4); v2.push(5);
    v.append(v2);             // v is now [1, 2, 3, 4, 5]
    println(v.len());         // 5

    v.clear();                // removes all elements
    println(v.len());         // 0
}
```

`.contains(x)` returns `bool` — works for scalars, strings, and records. `.append(v2)` appends every element of `v2` to `v` in order. `.clear()` empties the Vec without freeing it (you can push again afterward).

### Vec<Vec<T>> — nested Vecs

```hew
fn main() {
    var matrix: Vec<Vec<i64>> = Vec.new();

    var row0: Vec<i64> = Vec.new();
    row0.push(1); row0.push(2); row0.push(3);

    var row1: Vec<i64> = Vec.new();
    row1.push(4); row1.push(5); row1.push(6);

    matrix.push(row0);
    matrix.push(row1);

    println(matrix.len());            // 2
    let r = matrix[1];
    println(r[2]);                    // 6
}
```

`Vec<Vec<T>>` works for any element type, including other `Vec<T>` and `HashMap<K,V>`. Use `v[i]` to retrieve the inner Vec, then chain further operations.

## Collections — HashMap and HashSet

### Create and insert into a HashMap

```hew
fn main() {
    var m: HashMap<string, i64> = HashMap.new();
    m.insert("alice", 10);
    m.insert("bob", 20);
    println(m.len());   // 2
}
```

Use `var` — `.insert` mutates the receiver, and a mutating method needs a `var` binding. The element types may be inferred from later use; annotate when that use does not determine them. `.insert` returns unit and overwrites on duplicate key.

### Look up a key (returns Option)

```hew
fn main() {
    var m: HashMap<string, i64> = HashMap.new();
    m.insert("alice", 10);
    match m.get("alice") {
        .Some(v) => println(f"alice={v}"),
        .None => println("alice missing"),
    }
}
```

`m.get(k)` returns an optional value. For clone-free values it is a borrowed read: the map must remain live and cannot be mutated while that loan is in use. Use `.get` when absence is expected; indexing traps on a missing key.

### Membership, remove, length

```hew
fn main() {
    var m: HashMap<string, i64> = HashMap.new();
    m.insert("alice", 1);
    m.insert("bob", 2);
    let has_alice = m.contains_key("alice");   // true
    let removed = match m.remove("bob") {
        .Some(v) => f"removed bob={v}",
        .None => "bob not present",
    };
    println(f"has={has_alice} {removed} len={m.len()}");
}
```

`.contains_key(k)` returns `bool`; `.remove(k)` returns `Option<V>` — the removed value if the key was present, or `None`. Consume it with `match`. Bind a method result to a `let` before interpolating — nested double-quotes break the f-string parser. Test emptiness with `m.len() == 0`, or remove every entry at once with `m.clear()`.

### Supported HashMap value types

```hew
type User { name: string, score: i64, }

fn main() {
    // Scalar values
    var flags: HashMap<string, bool> = HashMap.new();
    flags.insert("debug", true);
    var ratios: HashMap<string, f64> = HashMap.new();
    ratios.insert("pi", 3.14);

    // User-defined records work as values
    var users: HashMap<string, User> = HashMap.new();
    users.insert("alice", User { name: "Alice", score: 100 });

    // Vec<T> also works as a value
    var tags: HashMap<string, Vec<string>> = HashMap.new();
    var v: Vec<string> = Vec.new();
    v.push("admin");
    tags.insert("alice", v);
}
```

Keys can be `string`, any integer type, `f64`, `bool`, or `char`. Value types
include `i64`, `string`, `bool`, `f64`, user-defined records, and `Vec<T>`.
Note what indexing returns: `m[k]` yields the bare value `V` and traps with
`IndexOutOfBounds` when the key is absent, so use `.get(k)` — which returns
`Option<V>` — whenever the key may be missing.

### Mutating a HashMap value (copy-rebuild-reinsert)

`HashMap` has no `.get_mut()`. The idiom for updating an existing value is to read it, compute the new value, and reinsert:

```hew
fn main() {
    var scores: HashMap<string, i64> = HashMap.new();
    scores.insert("alice", 10);
    scores.insert("bob", 20);

    // Increment alice's score
    match scores.get("alice") {
        .Some(v) => scores.insert("alice", v + 5),
        .None => {},
    }

    match scores.get("alice") {
        .Some(v) => println(f"alice={v}"),   // alice=15
        .None => println("missing"),
    }
}
```

`.get(k)` returns an owned copy of the value (not a reference). Modify the copy and reinsert with `.insert(k, new_value)`. This is the only mutation path — there is no `.get_mut()` or entry API.

### Iterating a HashMap — .keys() and .values()

```hew
fn main() {
    var m: HashMap<string, i64> = HashMap.new();
    m.insert("alice", 10);
    m.insert("bob", 20);
    m.insert("carol", 30);

    let ks = m.keys();         // Vec<string> — all keys
    let vs = m.values();       // Vec<i64>    — all values (same order)

    for k in ks {
        println(k);            // alice, bob, carol (order unspecified)
    }
    for v in vs {
        println(v);            // 10, 20, 30 (order matches keys())
    }
}
```

`.keys()` returns a `Vec<string>` snapshot of every key; `.values()` returns a `Vec<V>` snapshot of the corresponding values. Order is unspecified but both snapshots use the same internal order, so `keys()[i]` maps to `values()[i]`. Both return new Vecs — safe to iterate or pass to other functions.

### Create and use a HashSet

```hew
fn main() {
    var s: HashSet<i64> = HashSet.new();
    s.insert(1);
    s.insert(2);
    s.insert(2);                 // dedups
    let n = s.len();             // 2
    let has1 = s.contains(1);    // true
    let removed = s.remove(1);   // true
    println(f"n={n} has1={has1} removed={removed}");
}
```

Set membership is `.contains(x)` (note: `.contains_key` is the HashMap spelling). Inserts dedup automatically. Supported element types are `string` plus the scalar value types — any integer width, `f64`, `bool`, and `char`.

`.to_vec()` returns a `Vec<T>` snapshot of every element (order unspecified) — the same eager-clone pattern `HashMap.keys()`/`.values()` use. `.clear()` removes every element and resets `.len()` to 0, same as `HashMap.clear()`.

## Functions and ownership

### fn declaration with params and return type

```hew
fn add(a: i64, b: i64) -> i64 { a + b }
fn square(n: i64) -> i64 { return n * n; }
fn main() { println(add(2, 3)); println(square(5)); }   // 5, 25
```

Prefer the bare trailing expression (no semicolon) as the return value; reserve explicit `return expr;` for early exits. A trailing semicolon turns the last expression into unit.

### Unit return and bare early return

```hew
fn maybe_print(x: i64) {
    if x < 0 { return; }
    println(x);
}
fn main() { maybe_print(-1); maybe_print(42); }   // prints only 42
```

Omit the arrow for unit-returning fns (not `-> ()`). Use bare `return;` for early exit.

### Exit codes — main() -> i32 and exit()

The return value of `main() -> i32` (or `main() -> i64`) becomes the process exit code. A unit `main()` has no code of its own, so it exits 0 — even if it prints an error message. Use `exit(code)` to exit with a specific code from inside a unit `main()`.

One thing overrides a zero: an actor fault that no supervisor recovered. The full rule, applied on every way a program can end (returning from `main`, or calling `exit`), is:

- a non-zero code you chose — returned from `main` or passed to `exit` — is the exit code, unchanged;
- otherwise, if any actor crashed and no supervisor recovered it, the exit code is 1;
- otherwise 0.

So `exit(0)` does not paper over a crashed actor, and `return 7` is still 7 when one crashed. See HEW-SPEC-2026 §5.8.

```hew
// Pattern 1: return the code directly
fn main() -> i32 {
    println("all good");
    0
}
```

```hew
import std.os;

// Pattern 2: check args, exit non-zero on error
fn main() -> i32 {
    let arguments = os.args();
    if arguments.len() < 2 {
        println("usage: prog <arg>");
        return 1;
    }
    match arguments.get(1) {
        .Some(argument) => println(f"arg: {argument}"),
        .None => return 1,
    }
    0
}
```

```hew
// Pattern 3: exit() builtin from a unit main
fn main() {
    println("something failed");
    exit(1);            // exits with code 1 immediately
}
```

| Declaration        | How to exit non-zero                                           |
| ------------------ | -------------------------------------------------------------- |
| `fn main() -> i32` | return the code as the last expression or with `return N;`     |
| `fn main() -> i64` | same — return the code value                                   |
| `fn main()` (unit) | call `exit(N)` explicitly; the function itself can only exit 0 |

Shell pipelines and `&&` chains read the exit code — write `main() -> i32` for any program that signals failure to the caller. `assert(false)`, `panic(...)`, and traps (div-by-zero, an out-of-range index) are faults under the same rule: each writes one line to stderr — `hew: failure: DivideByZero (202)`, or the panic text after the kind — and exits 1. The number in that line names the failure; it is not the exit code.

### Return the collection a helper builds

```hew
fn filled() -> Vec<i64> {
    var values: Vec<i64> = Vec.new();
    values.push(1);
    values.push(2);
    values.push(3);
    values
}
fn main() {
    let values = filled();
    println(values.len());
}
```

Return a value when a helper builds a collection. An ordinary borrowed
parameter does not grant permission to mutate the caller's binding.

### Reuse a passed value without clone (intra-actor)

```hew
fn total(v: Vec<i64>) -> i64 {
    var sum: i64 = 0;
    let n = v.len();
    for i in 0 .. n { sum = sum + v[i]; }
    sum
}
fn main() {
    var xs: Vec<i64> = Vec.new();
    xs.push(10); xs.push(20);
    println(total(xs));
    println(total(xs));   // total borrows xs, so it remains usable
}
```

An ordinary borrowed argument stays usable after the call. A parameter marked
`consume` transfers ownership. Clone-free collection elements move on owning
ingress and borrow on read; do not generalize that rule into requiring a manual
clone for every string insertion. Follow the operation's declared contract.

### .clone() produces an independent copy

```hew
fn main() {
    var a: Vec<i64> = Vec.new();
    a.push(1); a.push(2);
    var b = a.clone();
    b.push(99);
    println(a.len());   // 2
    println(b.len());   // 3
}
```

Use an explicit clone when the operation requires a second owned copy and the
type supports it. Ordinary data passed to an actor preserves value semantics;
a resource transfer has a different, consuming contract.

### clone x — the canonical duplication prefix

```hew
fn main() {
    var a: Vec<i64> = Vec.new();
    a.push(1); a.push(2);
    var b = clone a;    // independent copy — same effect as `a.clone()`
    b.push(99);
    println(a.len());   // 2
    println(b.len());   // 3
}
```

`clone x` is the canonical way to duplicate a value: a contextual prefix that
reads the operand without consuming it and yields an owned copy, dropped
normally. It is exactly equivalent to the `x.clone()` method form and resolves
the same way — strings and `Vec<T>` clone; a type with no clone path fails
closed at compile time (the same diagnostic as `x.clone()`), never a silent
alias. `clone` is not a reserved word, so it stays usable as an ordinary
identifier or method name; it only acts as the prefix when an operand follows
directly (`clone x`, `clone foo.bar()`), never in `clone(args)` or `clone.field`.

It binds at unary precedence and takes the whole postfix chain: `clone x.f()`
clones the result of `x.f()`, and `clone a + b` is `(clone a) + b`. Ordinary
Hew uses values, implicit sharing, and `clone`; it has no borrow expression or
reference type. Writing `&x` is rejected with a diagnostic pointing you at
`clone x`. The separate `&T` spelling is confined to foreign declarations; see
[Appendix A](#appendix-a---ffi-boundary-types).

### Shared ownership inside one actor with Rc and Weak

`Rc<T>` gives multiple bindings ownership of one payload inside a single
actor. Construct it with `Rc.new(value)` and create another strong owner with
`.clone()`. Both handles are affine: assigning one without cloning moves it.
Neither `Rc<T>` nor `Weak<T>` can be sent to another actor.

```hew
fn main() {
    let value = Rc.new(7);
    let alias = value.clone();
    value.set(9);
    println(alias.get());          // 9
    println(value.strong_count()); // 2
}
```

`.get()` requires a `Copy` payload. `.set(value)` works for supported aggregate
payloads and replaces the whole shared value, consuming the replacement.

Use `Weak<T>` for graph back-edges so the graph does not form a strong cycle:

```hew
type Node {
    label: string,
    parent: Option<Weak<Node>>,
}

fn main() {
    let root = Rc.new(Node { label: "root", parent: None });
    let weak = root.downgrade();
    root.set(Node { label: "child", parent: Some(weak.clone()) });

    match weak.upgrade() {
        .Some(owner) => println(owner.strong_count()),
        .None => println("payload already released"),
    }
}
```

`upgrade()` returns `Some` only while at least one strong owner exists. There
is no `Weak.new()`; construction starts with `Option<Weak<T>>.None`, then uses
`downgrade()` and `set()`. Strong `Rc` cycles leak. `Rc.new_cyclic`, direct
deref/borrow access to the payload, and cross-actor transfer are not supported.

> **Reference cycles leak silently.** Hew reclaims shared storage with reference
> counts and has no cycle collector. A stored self-referential structure leaks
> when a field owns a strong reference back to its own refcounted container:
> every count in the cycle stays above zero after the outside owners disappear.
> Hew emits no diagnostic and no runtime warning for this leak. Use `Weak<T>`
> for local graph back-edges, as above, or redesign the structure as a tree or
> DAG.

### Record parameters and return values

```hew
type Point { x: i64, y: i64, }
fn translate(p: Point, dx: i64, dy: i64) -> Point {
    Point { x: p.x + dx, y: p.y + dy }
}
fn main() {
    let p = Point { x: 1, y: 2 };
    let q = translate(p, 10, 20);
    println(q.x);   // 11
    println(p.x);   // 1 (original still usable)
}
```

For transformations, return a new record value or mutate a `var` binding. Immutability belongs to the binding.

### Snapshot-on-send: the sender keeps its value

```hew
actor Sink { let id: i64, receive fn take(data: string) -> i64 { data.len() } }
fn main() {
    let s = spawn Sink(id: 0);
    let msg: string = "hello";
    let n = s.take(msg);         // receiver gets a snapshot of msg
    match n { .Ok(len) => println(len), .Err(_) => println("ask failed") }
    println(msg.len());   // 5 — msg still valid after the send
}
```

Passing a value into an actor's `receive fn` sends the receiver a logical snapshot: the receiver observes an independent value, and the sender's binding stays valid. Reuse after send — including sending the same value to many actors in a loop — is ordinary code with no `clone` ceremony. Types that cannot cross an actor boundary (such as `Rc<T>` and `Weak<T>`) are still rejected with a compile-time diagnostic. `actor.method(...)` on a request-reply fn returns `Result<R, ActorError>` — match it.

### Strings and scalars are freely reusable

```hew
fn shout(s: string) -> string { s + "!" }
fn double(n: i64) -> i64 { n * 2 }
fn main() {
    let greeting = "hi";
    println(shout(greeting));
    println(shout(greeting));   // string reused freely
    let x = 21;
    println(double(x));
    println(x);                 // scalars are copied
}
```

Pass strings and scalars without ceremony and keep using them. Concatenation with `+` builds a new string; it does not mutate the argument.

## Types — records and enums

### Record declaration, construction, field access

```hew
type Point { x: i64, y: i64, }
fn main() {
    let p = Point { x: 3, y: 4 };
    println(p.x);
    println(p.y);
}
```

Record fields use `name: T,` with commas and no `let`/`var` prefix. A `let`
binding is immutable; a `var` binding permits field updates.

### Mutable record via var binding

```hew
type Point { x: i64, y: i64, }
fn main() {
    var p = Point { x: 1, y: 2 };
    p.x = 10;
    println(p.x);   // 10
}
```

Bind with `var` to reassign fields; `let` is immutable. Immutability is on the binding, not the type.

### Nested record fields

```hew
type Point { x: i64, y: i64, }
type Line { start: Point, end: Point, }
fn main() {
    let l = Line { start: Point { x: 0, y: 0 }, end: Point { x: 3, y: 4 } };
    println(l.start.x);
    println(l.end.y);
}
```

Compose records by nesting; access depth-chains directly. Every field must be supplied — there is no partial/default fill.

### Structural separators

Use commas for record fields, enum variants and actor state fields. Executable
statements and bodyless function declarations end with semicolons.

```hew
type Point { x: i64, y: i64, }
enum Colour { Red, Green, Blue, }

fn main() {
    let point = Point { x: 1, y: 2 };
}
```

### Enum with unit, tuple, and record variants

```hew
enum Shape {
    Empty,
    Circle(f64),
    Rect { w: f64, h: f64 }
}
fn area(s: Shape) -> f64 {
    match s {
        .Empty => 0.0,
        .Circle(r) => 3.14159 * r * r,
        .Rect { w, h } => w * h,
    }
}
fn main() {
    println(area(.Circle(2.0)));
    println(area(Shape.Rect { w: 3.0, h: 4.0 }));
}
```

Mix unit, tuple, and record variants in one enum. Commas separate variants and record-variant fields; the variant pattern uses `{ w, h }` shorthand. Construct variants with dotted names such as `Shape.Rect { w: 3.0, h: 4.0 }`.

### Pattern destructuring in match

```hew
enum Cmd {
    Move(i64, i64),
    Stop,
    Speak { text: string }
}
fn describe(c: Cmd) -> string {
    match c {
        .Move(0, 0) => "noop",
        .Move(x, _) => "move",
        .Stop => "stop",
        .Speak { text } => text,
    }
}
```

Combine literal patterns for special cases above general binding patterns — order matters, specific arms first. Wildcard `_` ignores a payload slot.

### Qualified variant construction

```hew
indirect enum Expr { Lit(i64), Add(Expr, Expr), }
fn eval(e: Expr) -> i64 {
    match e {
        .Lit(n) => n,
        .Add(l, r) => eval(l) + eval(r),
    }
}
fn main() {
    let e = Expr.Add(Expr.Lit(10), Expr.Lit(5));
    println(eval(e));   // 15
}
```

Use `EnumName.Variant` to qualify construction or disambiguate across modules. In a match, use the contextual `.Variant` pattern when the scrutinee type selects the enum.

The bare spelling — a variant name with neither the dot nor the type qualifier — is not the language. Since v0.6.0 it is rejected in expression position with `E_BARE_VARIANT_EXPR` and in pattern position with `E_BARE_VARIANT_PATTERN`, each with a fix-it that inserts the dot. `hew fmt --migrate` applies both across a source tree.

### Self-referential recursive enum (indirect)

```hew
indirect enum Expr {
    Lit(i64),
    Add(Expr, Expr),
    Neg(Expr),
}
fn eval(e: Expr) -> i64 {
    match e {
        .Lit(n) => n,
        .Add(l, r) => eval(l) + eval(r),
        .Neg(inner) => 0 - eval(inner),
    }
}
fn main() {
    let e = Expr.Add(.Lit(1), .Neg(.Lit(2)));
    println(eval(e));   // -1
}
```

Prefix the enum keyword with `indirect` for self-referential variants (AST/tree types). It applies to the whole enum and heap-allocates behind a pointer. Construct and match exactly like a regular enum. (Use it for local values and function args.)

### User-defined generic enum

```hew
enum MyOpt<T> {
    Has(T),
    Empty,
}
fn main() {
    let a: MyOpt<i64> = .Has(42);
    match a {
        .Has(v) => println(v),
        .Empty => println(-1),
    }
}
```

Parameterize an enum with `<T>` for container-like sum types; annotate the binding so the type argument is fixed.

### Struct or enum as a receive fn message parameter

```hew
type Record { key: i64, val: i64, }
actor Sink {
    var last: i64,
    init() { last = 0; }
    receive fn put(r: Record) { last = r.val; }
    receive fn get() -> i64 { last }
}
fn main() {
    let s = spawn Sink();
    let _ = s.put(Record { key: 1, val: 99 });
    let r = s.get();
    match r { .Ok(v) => println(v), .Err(_) => println("err") }
}
```

Structs and enums cross the actor boundary as message payloads — pass them as receive-fn parameters. Keep the actor's own state fields scalar and feed structured data in via messages. Access state by bare field name.

### Block-bodied match arms

```hew
enum Op { Inc(i64), Reset, }
actor Acc {
    var total: i64,
    init() { total = 0; }
    receive fn apply(op: Op) {
        match op {
            .Inc(n) => {
                total = total + n;
            },
            .Reset => {
                total = 0;
            },
        }
    }
    receive fn value() -> i64 { total }
}
```

When a match arm runs statements (e.g. an assignment), wrap the body in `{ ... }` and put a comma after the closing brace. An assignment is not an expression, so a bare arm cannot hold it.

## Actors

### Actor declaration with state fields

```hew
actor Bank {
    var balance: i64 = 0,
    receive fn deposit(amt: i64) { balance = balance + amt; }
    receive fn balance_of() -> i64 { balance }
}
fn main() {
    let acct = spawn Bank(balance: 100);
    let _ = acct.deposit(50);
    let r = acct.balance_of();
    match r { .Ok(v) => println(f"balance={v}"), .Err(_) => println("ask failed") }
}
```

Use `var` for fields a handler mutates (give a default), `let` for fields set once at spawn. `spawn` passes by name every field that has no default and is not assigned in `init`.

### Fields initialized by init

```hew
actor Worker {
    var label: string,
    let count: i64,
    init(name: string, size: i64) {
        label = name.to_upper();
        count = size + 1;
    }
    receive fn label() -> string { label }
}
fn main() {
    let worker = spawn Worker(name: "ready", size: 6);
    println(worker.label().expect("label"));
    close(worker);
}
```

A field without a default that `init` assigns belongs to `init`: `spawn` cannot name it, and `init` must assign it on every path before it finishes. Read it, or call an actor method, only after every such field is assigned, and initialize it in every arm of a branch or before the branch, never inside a loop body. A later assignment in `init` replaces the value. If `init` faults part way, what it stored is released along with the spawn's own arguments; no handler sees partial state.

### Bare field access (read and write)

```hew
actor Counter {
    var count: i64 = 0,
    receive fn increment(n: i64) { count = count + n; }
    receive fn total() -> i64 { count }
}
```

Reference and assign state fields by bare name — there is no field prefix. State persists across invocations. Keep handler param names distinct from field names (shadowing is an error). Inside an actor body, `self` as a value is the actor's own handle of type `LocalPid<Self>`; `self.field` accesses its state.

### spawn returns LocalPid<ActorType>

```hew
actor Greeter {
    let name: i64,
    receive fn greet() -> i64 { name }
}
fn main() {
    let g: LocalPid<Greeter> = spawn Greeter(name: 5);
    let r = g.greet();
    match r { .Ok(v) => println(f"name={v}"), .Err(_) => println("ask failed") }
}
```

Let the actor handle type infer. This build spells local handles `LocalPid<T>`; the intended unified `Pid<T>` surface is pending. Handlers may take multiple arguments.

> **Spelling in this build.** One actor identity `Pid<A>` covers local and remote actors (HEW-SPEC-2026 §2.1.1). The compiler still spells the local case `LocalPid<A>` and the remote case `RemotePid<A>`, which is what the examples in this guide write.

### Calling a handler that returns nothing

```hew
actor Logger {
    var n: i64 = 0,
    receive fn log(msg: i64) { println(f"log: {msg}"); n = n + 1; }
    receive fn ping() { println("pong"); }
}
fn main() {
    let lg = spawn Logger(n: 0);
    let _ = lg.log(7);    // waits until the handler has finished
    let _ = lg.ping();
}
```

A call on an actor handle waits, whether or not the handler returns a value: `lg.log(7)` has type `Result<(), ActorError>` and comes back only once the handler's turn is over, so the log line is written before the next statement runs. `?` propagates a failure, `match` or `handle` inspects it, and `let _ =` discards it on purpose. Dropping it as a bare statement is `E_SEND_RESULT_DROPPED`, because an ignored failure loses work silently.

### Submitting without waiting

```hew
actor Logger {
    var n: i64 = 0,
    receive fn log(msg: i64) { println(f"log: {msg}"); n = n + 1; }
}
fn main() {
    let lg = spawn Logger(n: 0);
    let inbox = mailbox(lg, on_full: .Reject);
    let _ = inbox.log(7);    // accepted, not processed
}
```

`mailbox(target, on_full: ...)` is a one-way view: its calls return a delivery
outcome after submission, without waiting for the handler to finish. A
value-returning handler is refused on this view; use `fork target.m(..)` for a
concurrent completion call. If a submitted `fails` handler returns an error,
the actor faults with that error's Display text and its supervisor decides.

`policy(target, on_full: ...)` is the other view: its calls still complete — the handler result wrapped in `Result<R, ActorError<E, Req>>`, with `Req` inferred for rejected requests — and the policy chooses only what happens when the destination mailbox is full. `.Wait` is what a bare handle does. `.Reject` refuses instead of parking and reports `ActorError.Rejected(failure)`. Read `failure.reason` for the refusal reason. The owned request remains in `failure.message`: `.retry()` consumes it and resubmits to the original actor; `.to(other)` consumes it and resubmits to a compatible handler. Both wait for completion. Dropping the request releases its payload. Only a rejection is safely retryable. `policy` completes, `mailbox` submits.

Use a mailbox view when submission must not wait for handler completion.
Admission may still wait under `.Wait`. A callback into an actor whose handler
is waiting needs submission semantics to avoid a completion-call cycle.

### Ask / request-reply

```hew
actor Counter {
    var count: i64 = 0,
    receive fn increment(n: i64) { count = count + n; }
    receive fn total() -> i64 { count }
}
fn main() {
    let c = spawn Counter(count: 0);
    let _ = c.increment(10); let _ = c.increment(20); let _ = c.increment(12);
    let r = c.total();
    match r { .Ok(v) => println(f"total={v}"), .Err(_) => println("ask failed") }
}
```

Write request-reply as `ref.method(args)` and match `Ok`/`Err` — the call waits on its own, with no `await`. The reply value is the trailing expression of the `receive fn`. A call yields `Result<R, ActorError<E>>`, where `E` is the declared handler error
(or `Never`), rather than bare `R`. To run an ask concurrently, `fork` it and `await` the task.

### What `await` waits for

`await task` consumes a `Task<T>` and yields `T`. `await tasks` consumes a
`Vec<Task<T>>` and yields `Vec<T>` in vector order. If `T` is a `Result`,
that result remains an ordinary value. Other waiting operations are calls:

| You want                          | You write                            |
| --------------------------------- | ------------------------------------ |
| a reply from an actor             | `pid.method(args)`                   |
| that reply concurrently           | `let t = fork pid.method(args);` then `await t` |
| an actor to stop, and to wait     | `close(pid)`                         |
| an actor to stop, without waiting | `fork close(pid)`                    |
| to wait for a stop someone else asked for | `closed(pid)`                |
| each item of another actor's stream | `for x in pid.stream()`            |

Do not put `await` on an actor call, actor handle or generator operation.
Use it on the task created by `fork`.

A scope is a value-producing expression that owns its child tasks. Plain calls
inside it remain ordinary calls; only `fork` starts concurrent work. Scope exit
waits for child cleanup before returning its value.

```hew
fn left() -> i64 { 20 }
fn right() -> i64 { 22 }
fn main() {
    let total = scope {
        let a = fork left();
        let b = fork right();
        (await a) + (await b)
    };
    println(total);
}
```

An ordinary `Err` is a value, not a scope fault. A child fault or cancellation
uses structured cleanup; `scope within d { ... } handle failure { ... }`
can recover only after that cleanup. Parent cancellation continues outward.

### Ask try-sugar in a Result-returning fn

```hew
actor Counter {
    var count: i64 = 0,
    receive fn bump() -> i64 { count = count + 1; count }
}
fn run() -> i64 fails string {
    let c = spawn Counter(count: 0);
    match c.bump() {
        .Ok(v) => match c.bump() {
            .Ok(w) => v + w,
            .Err(_) => { return error "call failed"; },
        },
        .Err(_) => { return error "call failed"; },
    }
}
fn main() { match run() { .Ok(t) => println(f"total={t}"), .Err(_) => println("failed") } }
```

A completion call returns `Result<R, ActorError<E>>` in this build. Match or
recover the envelope, or propagate it with `?` when the enclosing error type
allows that propagation. There is one propagation spelling: `?` goes on the
expression, so a forked call propagates as `(await task)?`. See the pending
request-recovery contract above before writing explicit envelope types.

### Lifecycle hooks #[on(start)] and #[on(stop)]

```hew
actor Boot {
    var ready: i64 = 0,
    #[on(start)] fn boot() { ready = 99; println("started"); }
    #[on(stop)] fn done() { println("stopped"); }
    receive fn status() -> i64 { ready }
}
fn main() {
    let b = spawn Boot(ready: 0);
    let r = b.status();
    match r { .Ok(v) => println(f"ready={v}"), .Err(_) => println("ask failed") }
}
```

Put post-spawn initialization in `#[on(start)]` and teardown in `#[on(stop)]`. Both take no params; fields are in scope by bare name. They are plain fns, not receive fns. `#[on(start)]` appears at most once; `#[on(stop)]` may repeat.

### Own a resource with an actor

Keep a file, socket, or other non-sendable value inside one actor. Open it in
the actor's startup code, send ordinary data in messages, and release it when
the actor stops. The resource never crosses an actor boundary; callers share
the actor handle instead.

```hew
actor FileWriter {
    // Stand-in for the private descriptor of a file or socket.
    var descriptor: i64 = -1,

    #[on(start)]
    fn open() {
        descriptor = 7;
    }

    receive fn write(line: string) {
        println(f"sink {descriptor}: {line}");
    }

    #[on(stop)]
    fn close() {
        println(f"closed sink {descriptor}");
        descriptor = -1;
    }
}

fn main() {
    let writer = spawn FileWriter(descriptor: -1);
    let _ = writer.write("service started");
}
```

Do not put the sink or socket in a message. Give every operation that needs it
a `receive fn` on its owning actor, then send the operation's sendable inputs.
Replace the stand-in assignments and prints with the library operations that
open, use, and close the real resource.

### #[on(crash)] hook

```hew
import std.failure.{ CrashInfo, CrashAction };

actor Risky {
    var n: i64 = 0,
    #[on(start)] fn boot() { n = 1; }
    #[on(crash)] fn on_fail(info: CrashInfo) -> CrashAction { panic("crash observed") }
    receive fn value() -> i64 { n }
}
fn main() {
    let r = spawn Risky(n: 0);
    let v = r.value();
    match v { .Ok(x) => println(f"n={x}"), .Err(_) => println("failed") }
}
```

Declare `#[on(crash)]` as `fn name(info: CrashInfo) -> CrashAction` and satisfy the return type with a diverging `panic(...)` body. The body is observation/logging only; supervisor restart policy drives behaviour.

### Free functions callable from receive fns

```hew
fn double(x: i64) -> i64 { x * 2 }
actor Calc {
    var acc: i64 = 0,
    receive fn apply(n: i64) -> i64 { acc = acc + double(n); acc }
}
fn main() {
    let calc = spawn Calc(acc: 0);
    let r = calc.apply(5);
    match r { .Ok(v) => println(f"acc={v}"), .Err(_) => println("ask failed") }
}
```

Put logic shared between actors in top-level free functions and call them from receive fns.

### Actor methods

A plain `fn` in an actor body is an actor method: a helper over that actor's own state, with the same bare field access a handler has. Call it by bare name from the actor's receive fns, lifecycle hooks, `init`, and sibling methods.

```hew
actor Counter {
    var count: i64 = 0,
    fn next() -> i64 { count + 1 }
    receive fn increment() { count = next(); }
}
```

An actor method has no mailbox slot, so it is unreachable from outside the actor — `Counter.next()` from `main` is `E_ACTOR_METHOD_OUTSIDE`. Send the actor a message instead; only a `receive fn` is reachable through a pid.

### Actor-to-actor messaging

```hew
actor Worker {
    let id: i64,
    receive fn work(n: i64) -> i64 { n * id }
}
actor Manager {
    var worker: LocalPid<Worker>,
    receive fn dispatch(n: i64) -> i64 {
        let r = worker.work(n);
        match r { .Ok(v) => v, .Err(_) => -1 }
    }
}
fn main() {
    let w = spawn Worker(id: 3);
    let m = spawn Manager(worker: w);
    let r = m.dispatch(7);
    match r { .Ok(v) => println(f"result={v}"), .Err(_) => println("failed") }
}
```

Spawn the dependency first, pass its `LocalPid<Dep>` into the dependent actor's spawn, store it in a field, and call `dep.method(...)` from a handler. Asking another actor yields `Result<R, ActorError>` like any ask.

### Lambda actors — `actor |params| { .. }`

An `actor |params| { .. }` expression declares an actor with no source name.
Its captures become the actor's state, its body becomes its one handler, and
it evaluates to a `LambdaPid<Msg, Reply>` handle.

```hew
fn main() {
    let factor = 3;
    let scale = actor |n: i64| -> i64 {
        return n * factor;
    };
    match scale(7) {
        .Ok(v) => println(v),
        .Err(_) => println("the call failed"),
    }
    close(scale);
}
```

Calling the handle is the completion call: it waits for the handler and yields
`Result<R, ActorError>`, exactly as a call on a named actor's pid does. A
multi-parameter lambda is called with one argument per parameter.

A `LambdaPid<Msg, Reply>` is an ordinary value. Store it in a record field or a
`Vec` and call it where it is stored; a handle read out of a collection is
borrowed, and the call addresses the actor through the borrow without taking
it. `close(handle)` stops the actor and waits for its terminal cleanup, and
`closed(handle)` observes a stop someone else requested.

Both delivery views apply to a lambda handle. `mailbox(handle, on_full: ..)`
submits one way, so it accepts only a lambda that owes its caller nothing; a
lambda that returns a value is refused there, and the handle itself is how you
wait for the reply. `policy(handle, on_full: ..)` completes like the handle and
chooses only how a full mailbox is answered.

```hew
fn main() {
    let log = actor |line: string| {
        println(line);
    };
    let inbox = mailbox(log, on_full: .Reject);
    let _ = inbox("queued");
    close(log);
}
```

A lambda that captures its own handle is refused (`E_RECURSIVE_LAMBDA_ACTOR`):
the handle and the state seat it would live in own each other. Declare a named
actor when a handler needs to reach its own actor — inside a named actor's
body, `self` is that handle.

### Avoid reference cycles in actor state

Hew has no cycle collector and no weak reference for sendable strong handles.
If one actor's state owns a strong handle to a second actor and the second
actor's state owns a strong handle back, dropping every outside handle leaves
both reference counts above zero. Both actors and every value reachable from
their state then leak.

The leak is silent: there is no compiler diagnostic and no runtime warning.
Break the ownership cycle by storing a stable actor id and looking up a strong
handle only when it is needed. For named local actors, store the non-owning
`LocalPid<T>` shown above instead of a strong handle. Keep ownership flowing in
one direction when neither form is available.

Do not rely on a future cycle collector to reclaim an ownership cycle.

### Timers and scheduling — sleep and sleep_until

Hew provides two blocking/suspending timer builtins backed by a single
hierarchical timer wheel. The wheel is tickless — the scheduler thread parks
until the next deadline and never spins when no timers are armed.

**`sleep(d: duration)`** — suspend for a duration. Inside an actor handler
the actor suspends cooperatively and the worker is freed for other actors;
ordinary helper calls inherit their execution context. There is no actor-only
await spelling for timers.

```hew
actor Ticker {
    receive fn run(count: i64) {
        var i = 0;
        while i < count {
            sleep(5ms);
            println(f"tick {i}");
            i = i + 1;
        }
    }
}

fn main() {
    let t = spawn Ticker;
    let _ = t.run(3);
    close(t);
    // tick 0
    // tick 1
    // tick 2
}
```

**`sleep_until(target: instant)`** — suspend until a monotonic `instant`.
Returns immediately if `target` is already in the past. Combine with
`instant.now()` and duration arithmetic to build deadline-bounded loops:

```hew
fn main() {
    let t0 = instant.now();
    let deadline = t0 + 50ms;
    // ... do some work ...
    sleep_until(deadline);    // waits only the remaining time
    let elapsed = t0.elapsed();
    println(elapsed.millis() >= 45);   // true
}
```

A handler that waits on a deadline releases its worker, so sibling actors
keep running while the timer is armed. Two workers napping for the same
interval finish in about one interval, not two:

```hew
actor Sleeper {
    receive fn nap() -> i64 {
        sleep_until(instant.now() + 300ms);
        1
    }
}

fn main() {
    let a = spawn Sleeper();
    let b = spawn Sleeper();
    let start = instant.now();
    let first = fork a.nap();
    let second = fork b.nap();
    let _ = await first;
    let _ = await second;
    println(start.elapsed().millis() < 500);   // true, not the 600 ms two naps would take
}
```

**Duration literals:** `10ms`, `2s`, `500ms`, `1ns`, `1us`. These produce a
`duration` value. `instant + duration` and `duration + instant` both produce
a new `instant`.

**Timer model:** both `sleep` and `sleep_until` arm a single entry in the
runtime's two-level hierarchical wheel (256 × 1 ms slots + 64 × 256 ms
slots; overflow list for intervals > 16 s). There is one shared wheel per
process; all sleeping actors and threads compete for its entries. Scheduling
granularity is 1 ms on the native target. Sub-millisecond values in actor
handlers convert to 0 ms (next tick); in `fn main` the OS sleep is
nanosecond-precise (subject to OS granularity, typically ≥ 1 µs on
Linux/macOS).

### Periodic receive handlers — `#[every(duration)]`

`#[every(duration)]` marks a zero-argument `receive fn` as periodic. The timer
starts when the actor is spawned and repeats until the actor stops. Each firing
is dispatched through the actor mailbox as its own message, so ordinary receive
handlers can run between ticks.

```hew
actor Pulse {
    var count: i64 = 0,

    #[every(50ms)]
    receive fn tick() {
        count = count + 1;
    }

    receive fn total() -> i64 {
        count
    }
}

fn main() {
    let p = spawn Pulse(count: 0);
    sleep(250ms);
    let r = p.total();
    match r {
        .Ok(n) => println(f"ticks={n}"),
        .Err(_) => println("ask failed"),
    }
}
```

Periodic handlers preserve the actor's single-threaded state model: a tick never
runs concurrently with another receive handler on the same actor. It is just a
separate mailbox dispatch armed by the runtime timer.

When the runtime shuts down, periodic-timer admission closes first. A periodic
tick becomes live work only when the timer ticker claims it for callback
delivery. Shutdown waits for callbacks already claimed, then cancels every
pending periodic entry, including an entry that is already due but has not yet
been claimed. Such an entry is not delivered during shutdown.

### Cancellable long-running work in actor handlers

A receive handler runs to completion before the actor observes the next message.
If a handler writes `while running { sleep(...) }` and expects another receive
handler to set `running = false`, the stop message cannot be dispatched until
the loop exits. Use a periodic receive handler and a flag instead:

```hew
actor Worker {
    var running: bool = true,
    var ticks: i64 = 0,

    #[every(25ms)]
    receive fn tick() {
        if !running {
            return;
        }
        ticks = ticks + 1;
    }

    receive fn halt() {
        running = false;
    }

    receive fn total() -> i64 {
        ticks
    }
}

fn main() {
    let worker = spawn Worker(running: true, ticks: 0);
    sleep(150ms);
    let _ = worker.halt();
    sleep(150ms);
    let r = worker.total();
    match r {
        .Ok(n) => println(f"ticks={n}"),
        .Err(_) => println("ask failed"),
    }
}
```

Here `receive fn halt()` is a user-defined actor message, and it is the flag
flip, not the actor's death. It is unrelated to the `#[on(stop)]` lifecycle
hook, which is a plain `fn` invoked when the actor is tearing down. The
`sleep_loop_blocks_mailbox` lint warns on the mailbox starvation shape where a
receive handler loops around `sleep`/`sleep_until` without an in-loop exit path.

`stop` is a reserved handler name, so the handler above is `halt`, not `stop`.
Stopping is a method with one signature: inside the actor, `self.stop()`
finishes the current handler, runs `#[on(stop)]`, and stops; from outside,
`pid.stop()` requests the same and returns `()`, doing nothing if the actor has
already stopped or crashed. A `receive fn stop()` is `E_RESERVED_HANDLER_NAME`,
whose fix-it is to rename the handler or to call `self.stop()`. The shipped
compiler still exposes the older free-function `stop(actor)` builtin
(hew-lang/hew#3193).

### Accepting connections and reading in a handler

`listener.accept()` and `conn.read()` are plain suspending calls. They park the
calling coroutine on the reactor while other actors can run; adding `await`
is not a switch from blocking I/O to coroutine I/O. Use the declared return
type to handle failures, and `fork` when the operation should run concurrently.

See [the network module](../std/net/net.hew) for the current operations and
[the suspension contract](specs/HEW-SPEC-2026.md#40-suspension-normative).

## State machines

### Machine declaration: events, states, transitions, step(), state_name()

```hew
machine Counter {
    events { Inc, Reset, }
    state Zero,
    state NonZero { value: i64, },
    on Inc: Zero => NonZero { Counter.NonZero { value: 1 } }
    on Inc: NonZero => NonZero reenter { Counter.NonZero { value: self.value + 1 } }
    on Reset: NonZero => Zero { Counter.Zero }
    default { state }
}
fn main() {
    var c = Counter.Zero;
    c.step(.Inc); c.step(.Inc); c.step(.Inc);
    println(c.state_name());            // NonZero
    match c {
        .Zero => println("is zero"),
        .NonZero { value } => println(f"value={value}"),  // value=3
    }
    c.step(.Reset);
    println(c.state_name());            // Zero
}
```

A machine is a value type. State constructors use a machine-qualified dotted form both inside and outside transition bodies (`Counter.NonZero { value: 1 }`, `Counter.Zero`); contextual event arguments use `.Variant` (`step(.Inc)`). Event constructors use the event-type qualifier outside contextual positions (`CounterEvent.Inc`). This behaviour keeps state-constructor resolution explicit. `step()` mutates in place — the receiver must be a `var`. End with `default { state }` to make uncovered cells a no-op stay. Every (state, event) cell must be covered or it is a compile error.

> **Why `default` is a blanket catch-all, and what that costs you.** Without
> `default`, every `(state, event)` pair not covered by an explicit `on`
> rule is a compile error — this is what makes a machine an exhaustive
> state/event matrix instead of an ad-hoc set of handlers. `default { state
}` exists because most machines have far more legal no-op transitions
> (an `Ajar` door ignoring a second `Open`) than meaningful ones, and
> writing every one of those out by hand is pure noise. The trade-off:
> `default` is all-or-nothing per machine, not per state. Adding it makes
> every currently-unlisted `(state, event)` pair a silent stay — including
> ones you meant to leave unhandled as a genuine error (a `Withdraw` event
> reaching a `Closed` account, say). There is no middle ground today between
> "enumerate every single cell by hand, so the compiler's exhaustiveness
> check catches anything you forgot" and "add `default`, so every gap —
> intentional or not — silently becomes a no-op stay". If some unhandled
> transitions in a machine should be hard errors and others should be
> silent no-ops, the only current option is to omit `default` and write out
> every cell explicitly, including the no-op ones (`on Bump: Dead => Dead
reenter { state }`), so the compiler forces you to make each one a
> deliberate decision.

### Transition heads: the source is a pattern, the target is an expression

`on Event: Source => Target` reads symmetrically, but the two sides are different
grammatical things and they take different spellings.

The **source** is a pattern matched against the machine's current state. It is
never evaluated, has no expected type, and is the one pattern position exempt from
`E_BARE_VARIANT_PATTERN`. Write it bare (`Off`), as a qualified path into a
composite (`Connected.Active`, which resolves to the leaf `Active`), or as the
wildcard `_`. A leading `.` is rejected there:

```
on Toggle: .Off => .On,
           ^ error: a machine transition source state is a pattern and cannot be
             written with a leading `.`
```

The **target** is an expression checked against the machine's state enum, so it
takes the contextual form: `=> .On`, `=> .Faulted { code: event.code }`. The bare
form still parses, but since v0.6.0 it is rejected with `E_BARE_VARIANT_EXPR` and
a fix-it that inserts the dot — the same rule every other
enum-in-expected-type position follows. Run `hew fmt --migrate` to apply the
fix-its across a legacy source. A `_` target (paired with a body that
computes the next state) is a wildcard rather than a variant, so it takes no dot.

```hew
machine Switch {
    events { Toggle, }
    state Off,
    state On,
    on Toggle: Off => .On,
    on Toggle: On => .Off,
}
```

The formatter re-emits whichever target spelling you wrote; it never adds or
removes the dot.

### State field holding a Vec

```hew
machine Log {
    events { Append { item: i64, }, Clear, }
    state Empty,
    state Filled { items: Vec<i64>, },
    on Append(item): Empty => Filled {
        var v: Vec<i64> = Vec.new(); v.push(item); Filled { items: v }
    }
    on Append(item): Filled => Filled reenter {
        var v = self.items; v.push(item); Filled { items: v }
    }
    on Clear: Filled => Empty { Log.Empty }
    default { state }
}
fn main() {
    var log = Empty;
    let _ = log.step(Append { item: 10 });
    let _ = log.step(Append { item: 20 });
    match log {
        .Empty => println("empty"),
        .Filled { items } => {
            println(f"count={items.len()}");   // count=2
            println(f"first={items[0]}");  // first=10
        },
    }
}
```

Read the prior vec out of `self.items`, push, and rebuild the variant. Access elements with `v[i]` and `.len()`.

### Event payload access

```hew
machine Acc {
    events { Add { n: i64, } }
    state Seed,
    state Total { sum: i64, },
    on Add: Seed => Total { Total { sum: event.n } }
    on Add: Total => Total reenter { Total { sum: self.sum + event.n } }
}
fn make_add(n: i64) -> AccEvent { AccEvent.Add { n: n } }
fn main() {
    var a = Seed;
    a.step(make_add(5));
    a.step(make_add(7));
    match a {
        .Seed => println("seed"),
        .Total { sum } => println(f"sum={sum}"),   // sum=12
    }
}
```

Prefer the head binding `on Add(n): ...` so payload names are declared at the rule site; `event.n` is the equivalent fallback. `self.field` reads the source state; `event.field` reads the event payload. The compiler generates a companion enum `{MachineName}Event` you can name in signatures and construct with `MachineEvent.Variant`.

### Wildcard transitions and if-expression bodies

```hew
machine Conn {
    events { Start, Bump, Kill, }
    state Idle,
    state Live { hits: i64, },
    state Dead,
    on Start: Idle => Live { Live { hits: 0 } }
    on Bump: Live => _ {
        if self.hits + 1 >= 3 { Conn.Dead } else { Live { hits: self.hits + 1 } }
    }
    on Kill: _ => Dead { Conn.Dead }
    on Start: _ => _ { state }
    on Bump: _ => _ { state }
}
fn main() {
    var c = Idle;
    let _ = c.step(.Start);
    let _ = c.step(.Bump);
    let _ = c.step(.Bump);
    let _ = c.step(.Bump);
    println(c.state_name());   // Dead
}
```

`on E: _ => _ { state }` is the canonical "ignore this event everywhere it isn't explicitly handled". A wildcard-target body may return any variant, so an `if` returning different states is legal there and in a `reenter` body. An explicit cell rule always wins over a wildcard.

### Driving a machine through a free-function parameter

```hew
machine Door {
    events { Open, Close, }
    state Shut,
    state Ajar { angle: i64, },
    on Open: Shut => Ajar { Ajar { angle: 90 } }
    on Close: Ajar => Shut { Door.Shut }
    default { state }
}
fn drive(d: Door) -> string {
    var local = d;
    local.step(.Open);
    local.state_name()
}
fn main() { println(drive(.Shut)); }   // Ajar
```

Pass machines by value into and out of free functions and mutate a local `var`. A machine also works as an actor state field — `var d: Door = Shut;` in an actor body, with `d.step(Open)` inside a `receive fn`, compiles and runs (the field rides the enum clone/drop substrate). What does not work is a machine inside a plain record: `.step()` requires a `var`-bound receiver, and a record field is not one, so `r.d.step(Open)` is rejected with `` `.step()` requires a mutable binding receiver; this expression is not declared with `var` ``. Copy the field into a local `var`, step it, and write it back.

## Generics

### Generic function with a trait bound

```hew
trait Named { fn name(self) -> string; }
type User { name: string, }
impl Named for User { fn name(self) -> string { self.name } }
fn announce<T: Named>(item: T) { println(item.name()); }
fn main() { announce(User { name: "Bob" }); }   // Bob
```

Declare the impl as `impl Trait for Type`, bound the parameter `<T: Trait>`, and call the method on the param. Monomorphized per concrete type. A bare `impl Type { ... }` does not satisfy the bound — always write `impl Trait for Type`.

### Multi-bound and where-clause functions

```hew
trait HasName { fn name(self) -> string; }
trait HasScore { fn score(self) -> i64; }
type Player { name: string, score: i64, }
impl HasName for Player { fn name(self) -> string { self.name } }
impl HasScore for Player { fn score(self) -> i64 { self.score } }
fn report<T: HasName + HasScore>(item: T) {
    print(item.name()); print(": "); println(item.score());
}
fn main() { report(Player { name: "Zoe", score: 42 }); }   // Zoe: 42
```

Use `<T: A + B>` for inline multi-bounds; the `where T: A,` form is equivalent and reads better with several params. Each concrete type needs explicit `impl Trait for Type`.

### Unbounded generic function

```hew
fn identity<T>(x: T) -> T { x }
fn main() { println(identity(99)); println(identity(2.5)); println(identity(true)); }
```

Unbounded `<T>` works when you only move/return the value. To call any method or operator on the value, add the relevant trait bound first.

### Generic free function over a generic record

```hew
type Pair<A, B> { first: A, second: B, }
fn fst<A, B>(p: Pair<A, B>) -> A { p.first }
fn main() { let p = Pair { first: 100, second: 2.5 }; println(fst(p)); }   // 100
```

Read fields of a generic record inside a generic free function — prefer a free `fn fst<A,B>(p: Pair<A,B>)` over a generic impl method.

### Generic record type with all-bitcopy fields

```hew
type Pair<A, B> { first: A, second: B, }
fn main() {
    let p = Pair { first: 10, second: 3.5 };
    println(p.first);
    println(p.second);
}
```

Use generic records as lightweight bitcopy containers over scalar types (`i64`, `f64`, `bool`). The fields may also be owned — see the next example.

### Generic record type with an owned field

```hew
type Pair<A, B> { first: A, second: B, }
fn make() -> Pair<i64, string> { Pair { first: 1, second: "owned" } }
fn main() {
    let p = make();
    println(p.first);
    println(p.second);
}
```

Generic records derive ownership and cleanup from their substituted field
types. Owned fields need not be cloneable: a supported resource or opaque field
does not require replacing the generic record with a concrete declaration.
Copying still requires the relevant clone capability; borrowing, transferring
and cleanup follow the instantiated record's ownership contract. This is not a
promise that every opaque type or use site is supported.

### Generic function over Vec<T>

```hew
fn count<T>(items: Vec<T>) -> i64 { items.len() }
fn main() {
    var v: Vec<i64> = Vec.new();
    v.push(10); v.push(20); v.push(30);
    println(count(v));   // 3
}
```

Accept `Vec<T>` in a generic function and use `.len()`/`v[i]`.

### Vec of a concrete enum (including string payload)

```hew
enum Shape { Circle(f64), Named(string), }
fn main() {
    var v: Vec<Shape> = Vec.new();
    v.push(Shape.Circle(1.5));
    v.push(Shape.Named("square"));
    println(v.len());   // 2
}
```

A monomorphic enum, even one carrying a string payload, is a valid Vec element. Construct variants as `Enum.Variant(payload)`. Generic type parameters (`Vec<T>`) are also supported for element methods (`get`, `push`, `set`, `pop`, `contains`, indexing, and range-slice).

### Vec of a concrete record

```hew
type Point { x: i64, y: i64, }
fn main() {
    var v: Vec<Point> = Vec.new();
    v.push(Point { x: 1, y: 2 });
    v.push(Point { x: 3, y: 4 });
    let got = v[1];
    println(got.x);   // 3
    println(got.y);   // 4
}
```

Store concrete records in a Vec and bind the element with `let got = v[i]` before reading fields.

### Explicit type argument on a call

When a generic function takes no arguments from which the type can be inferred, use `func<T>()` to supply the type argument explicitly:

```hew
fn make_vec<T>() -> Vec<T> {
    Vec.new()
}
fn main() {
    var v = make_vec<i64>();
    v.push(10);
    v.push(20);
    println(v.len());   // 2
}
```

**An explicit type argument is one way to pin a generic constructor's type argument, not the only one.** The checker also resolves `T` from the **expected return type** at the call site — a `let` binding's declared type, a function's declared return type, or an argument position — without an explicit type argument:

```hew
type Stack<T> { items: Vec<T>, }

fn new_empty<T>() -> Stack<T> { Stack { items: Vec.new() } }

fn make_i64_stack() -> Stack<i64> {
    new_empty()                    // return-position inference — no explicit type argument
}

fn main() {
    let s: Stack<i64> = new_empty();      // let-annotation inference — no explicit type argument
    let s2 = new_empty<i64>();          // explicit type argument
    let s3 = make_i64_stack();
    println(s.items.len());
    println(s2.items.len());
    println(s3.items.len());
}
```

Only a **genuinely unconstrained** call — no explicit type argument, no annotation, no usage that pins `T` — fails, and it fails with the same clean `cannot infer type for local binding` / `consider adding a type annotation` diagnostic as any other unconstrained generic call. There is no separate NYI/MIR-lowering error for this pattern; add an explicit type argument or a type annotation to resolve it.

### Inherent impl on a generic record

Methods on a generic record need the impl itself to carry the type parameter — `impl<T> Stack<T> { ... }`, not `impl Stack<T> { ... }`. The bare form leaves `T` unbound inside the impl body and every use of `T` fails with `unknown type 'T'`.

```hew
type Stack<T> { items: Vec<T> }

impl<T> Stack<T> {
    fn push_item(consume self, consume v: T) -> Stack<T> {
        var items = self.items;
        items.push(v);
        Stack { items: items }
    }
    fn len(self) -> i64 {
        self.items.len()
    }
}

fn new_stack<T>() -> Stack<T> {
    Stack { items: Vec.new() }
}

fn main() {
    let s = new_stack<i64>();
    let s2 = s.push_item(1);
    let s3 = s2.push_item(2);
    println(s3.len());   // 2
}
```

Construct the empty generic record through the constructor function `new_stack`, either with an explicit type argument (`new_stack<i64>()`) or a `let` type annotation (`let s: Stack<i64> = new_stack();`). Both resolve `T` from the call site and lower identically. A bare `Stack { items: Vec.new() }` construction with no surrounding annotation is still ambiguous and needs one. `push_item` consumes the old stack and the new element, takes its items into a
mutable local, and returns the rebuilt stack. The old stack is no longer usable;
`len` only borrows it.

### Monomorphic functions as values (cross-module)

A monomorphic function exported from another module can be passed as a
first-class value; its type is recovered from the declared signature.

<!-- doctest: skip -->

```hew
import math_utils;

fn apply(f: fn(i64) -> i64, x: i64) -> i64 {
    f(x)
}

fn main() {
    // Monomorphic cross-module function as a value
    let sq: fn(i64) -> i64 = math_utils.square;
    println(apply(sq, 7));                       // 49

    // Inline: pass a cross-module fn directly to a higher-order function
    println(apply(math_utils.add_one, 10));       // 11
}
```

Where `math_utils.hew` exports:

<!-- doctest: skip -->

```hew
pub fn add_one(x: i64) -> i64 { x + 1 }
pub fn square(x: i64) -> i64 { x * x }
pub fn identity<T>(x: T) -> T { x }
```

> **Generic functions are not usable as values.** Capturing a _generic_
> function as a value fails in both directions. From another module
> (`let id: fn(i64) -> i64 = math_utils.identity;`) it is rejected with
> ``E_NOT_YET_IMPLEMENTED: MIR lowering for named function
`math_utils$identity` used as a value (only non-generic named functions are
currently supported)``. From the current module it is rejected earlier, as a
> type mismatch (``expected `fn(i64) -> i64`, found `fn(T) -> T` ``). Calling
> an imported generic directly (`math_utils.identity(5)`) works normally —
> only the value position is closed. Write a monomorphic wrapper when you need
> one as a value.

### Generic Display and println

`println` and `print` accept any type `T: Display`. For primitive types
(`i64`, `f64`, `bool`, `char`, `string`) the `Display` impl is built in and
`println(v)` works without further setup. For a user-defined type, implement
`Display` and call `println` through a generic wrapper or use f-string
interpolation:

```hew
type Celsius { degrees: f64, }

impl Display for Celsius {
    fn fmt(self) -> string {
        f"{self.degrees}°C"
    }
}

// Generic helper: accepts any T that implements Display
fn show<T: Display>(label: string, value: T) {
    println(f"{label}: {value}");
}

fn main() {
    show("int", 42);               // int: 42
    show("float", 2.718);          // float: 2.718
    show("str", "Hew");            // str: Hew
    let temp = Celsius { degrees: 36.6 };
    show("temp", temp);            // temp: 36.6°C
}
```

`println(x)` called directly on a user type (without an f-string or a `<T:
Display>` wrapper) does not dispatch — the checker emits
`E_HIR: builtin call has no registered monomorphic overload`. Two correct
forms: `println(f"{val}")` (f-string interpolation) or a generic function
that takes `T: Display` and calls `println` internally. The `fmt` method is
also callable directly: `val.fmt()` returns the string representation.

## Errors — Result and Option

### Result construction and matching

```hew
fn divide(a: i64, b: i64) -> Result<i64, string> {
    if b == 0 { Err("division by zero") } else { Ok(a / b) }
}
fn main() {
    match divide(10, 2) { .Ok(v) => println(f"ok: {v}"), .Err(e) => println(f"err: {e}") }
    match divide(1, 0) { .Ok(v) => println(f"ok: {v}"), .Err(e) => println(f"err: {e}") }
}
```

Construct with bare `Ok(v)`/`Err(e)`; consume with `match` covering both arms. The type is inferred from the function's declared return type.

### Option construction and matching

```hew
fn first_positive(a: i64) -> Option<i64> {
    if a > 0 { Some(a) } else { None }
}
fn main() {
    match first_positive(5) { .Some(v) => println(f"some: {v}"), .None => println("none") }
    let n: Option<i64> = None;
    match n { .Some(v) => println(f"some: {v}"), .None => println("none") }
}
```

`Some(7)` infers `Option<i64>` on its own. A standalone `None` needs an annotation — `let n: Option<i64> = None`.

### ? operator for propagation (Result and Option)

```hew
fn divide(a: i64, b: i64) -> Result<i64, string> {
    if b == 0 { Err("division by zero") } else { Ok(a / b) }
}
fn chain(a: i64, b: i64, c: i64) -> Result<i64, string> {
    let x = divide(a, b)?;
    let y = divide(x, c)?;
    Ok(y)
}
fn main() {
    match chain(100, 5, 2) { .Ok(v) => println(f"ok: {v}"), .Err(e) => println(f"err: {e}") }
    match chain(100, 0, 2) { .Ok(v) => println(f"ok: {v}"), .Err(e) => println(f"err: {e}") }
}
```

Use `?` to unwrap-or-early-return inside a fn that itself returns Result/Option; chain multiple `?` for sequential steps. The enclosing fn must return Result/Option — `?` in a `()`-returning fn is rejected.

### ? on Option propagates None

```hew
fn first(o: Option<i64>) -> Option<i64> {
    let v = o?;
    Some(v + 1)
}
fn main() {
    match first(Some(5)) { .Some(v) => println(f"some: {v}"), .None => println("none") }
    let n: Option<i64> = None;
    match first(n) { .Some(v) => println(f"some: {v}"), .None => println("none") }
}
```

Inside an Option-returning fn, `o?` yields the inner value on Some and early-returns None on None.

### Hand-rolled unwrap_or via match

```hew
fn unwrap_or(r: Result<i64, string>, fallback: i64) -> i64 {
    match r { .Ok(v) => v, .Err(_) => fallback }
}
fn main() {
    let ok: Result<i64, string> = Ok(42);
    let err: Result<i64, string> = Err("bad");
    println(unwrap_or(ok, 0));    // 42
    println(unwrap_or(err, -1));  // -1
}
```

For unwrap_or/is_ok/is_err on Result, write a tiny `match` helper inline. This is the reliable path — do not import `std.result`/`std.option`, and do not use the `.unwrap_or()` method form.

### Option .is_some() / .is_none()

```hew
fn main() -> i64 {
    let a: Option<i64> = Some(5);
    let b: Option<i64> = None;
    if a.is_some() == false { return 1; }
    if b.is_none() == false { return 3; }
    println("option method predicates ok");
    0
}
```

For Option presence checks, `.is_some()`/`.is_none()` read cleanly and return `bool`. For all other unwrap needs, use `match`.

### match as a value-producing expression

```hew
fn classify(o: Option<i64>) -> string {
    match o {
        .Some(v) => if v > 0 { "positive" } else { "non-positive" },
        .None => "missing",
    }
}
fn main() {
    println(classify(Some(5)));
    println(classify(Some(-1)));
    let n: Option<i64> = None;
    println(classify(n));
}
```

Each arm yields a value and the whole `match` is the function's return value. Arms may be if-expressions or blocks; all arms must produce the same type.

### Fallible op with no success value: sentinel payload

```hew
fn validate(x: i64) -> Result<i64, string> {
    if x < 0 { Err("negative") } else { Ok(0) }
}
fn main() {
    match validate(5)  { .Ok(_) => println("valid"), .Err(e) => println(f"err: {e}") }
    match validate(-1) { .Ok(_) => println("valid"), .Err(e) => println(f"err: {e}") }
}
```

For a fallible operation with no meaningful success value, `Result<(), E>` and `Ok(())` are the shapes to reach for — that is what `os.set_env` and a `main` that can fail both return. The `Result<i64, E>` with `Ok(0)` form above is a payload nobody reads; it appears here because older stdlib signatures used it, not because it is the shape to copy.

### Errors that compose: `dyn Error`

`?` is exact. The error type of the operand has to be the error type of the enclosing function — there is no `From`, no `#[from]`, and no conversion the compiler inserts on your behalf, so two concrete error enums never flow into one another by accident.

A function that calls into several modules can use `dyn Error`. Convert each
concrete error at an explicit failure return; `?` does not implicitly erase a
concrete error into `dyn Error`:

```hew
import std.fs;

enum PortError {
    NotANumber(string),
}

impl Display for PortError {
    fn fmt(self) -> string {
        match self { .NotANumber(s) => f"NotANumber: {s} is not a port number" }
    }
}

impl Error for PortError {}

fn parse_port(text: string) -> Result<i64, PortError> {
    Err(PortError.NotANumber(text))
}

fn load_port(path: string) -> i64 fails dyn Error {
    let text = fs.read(path) handle problem {
        return error problem;
    };
    parse_port(text) handle problem {
        return error problem;
    }
}
```

`Error` is a prelude trait whose supertrait is `Display`, so every std error type prints itself and a `dyn Error` prints through the supertrait: `f"{e}"` works on the erased value. Where you want a concrete error type instead of the trait object, convert explicitly with `.map_err(f)` on a `Result` or `.ok_or(e)` on an `Option`. Crash on the spot with `.expect(reason)`, the one deliberate invariant assertion; `unwrap()` is not a method in Hew.

`fn main() -> Result<(), E>` needs `E: Error`. On `Err(e)` the runtime writes `error: {e}` to stderr and exits 1.

### Fallible means `Result`

Standard-library functions report failure in the type system. A fallible call returns `Result<T, E>` under its plain name — there is no `try_read` beside `read` — and a lookup that can miss returns `Option<T>`, so `os.env("PORT")` gives you `None` rather than an empty string you have to guess about. Nothing returns a status integer or a sentinel value.

The exception is indexing: `v[i]` and `m[k]` trap when the index or key is absent, because that is a bug in the program rather than a condition to handle. Use `.get()` when a miss is expected.

## Strings

### f-string interpolation

```hew
fn main() {
    let name = "world";
    let n = 42;
    println(f"n={n} expr={n + 1} up={name.to_upper()}");
}
```

Arbitrary expressions (arithmetic, no-arg method calls, field access) work inside `{}`. You cannot put a `"..."` string literal inside the `{}` of an f-string — bind it to a `let` first.

### .split(sep) and reading elements

```hew
fn main() {
    let parts = "a,b,c".split(",");
    println(f"count={parts.len()}");
    for i in 0 .. parts.len() {
        let p = parts[i];
        println(p);
    }
}
```

Read elements with `parts[i]`, or `.get(i)` when absence is expected. String
elements can be copied out while the vector remains usable. For clone-free
elements, reads and ordinary iteration borrow; `into_iter()` takes ownership.

### .trim()

```hew
fn main() {
    let t = "  hello world  ".trim();
    println(f"[{t}]");   // [hello world]
}
```

Trims leading/trailing whitespace, returns a new string.

### .starts_with / .ends_with and .slice

```hew
fn main() {
    let path = "src/main.hew";
    if path.starts_with("src/") {
        let rest = path.slice(4, path.len());
        println(rest);   // main.hew
    }
}
```

`.starts_with`/`.ends_with` return `bool`. `.slice(a, b)` is half-open `[a, b)`; use `s.slice(start, s.len())` to slice to the end. No negative-index support.

### .find returns Option<i64>

```hew
fn main() {
    match "hello world".find("world") {
        .Some(idx) => println(f"find={idx}"),   // 6
        .None => println("not found"),
    }
    match "hello".find("xyz") {
        .Some(idx) => println(f"find={idx}"),
        .None => println("not found"),
    }
}
```

`.find(needle)` returns `Option<i64>` — `Some(byte_index)` of the first occurrence, or `None` when the needle does not occur. Consume it with `match`, or use `.unwrap_or(-1)` when a sentinel is convenient.

### .len() and .contains()

```hew
fn main() {
    let t = "hello world";
    println(f"len={t.len()}");
    let has = t.contains("world");
    println(f"contains={has}");
}
```

`.len()` returns `i64` (safe directly in an f-string). `.contains(sub)` returns `bool` — bind to a `let` before interpolating.

### std.string module functions

```hew
import std.string;
fn main() {
    println(string.from_int(42));            // 42
    let n = match string.to_int("42") {
        .Ok(v) => v,
        .Err(_) => 0,
    };
    println(f"n={n}");                        // 42
    println(string.pad_left("7", 3, "0"));   // 007
    println(string.join(["a", "b", "c"], ", "));  // a, b, c
    let c = string.count("abcabc", "abc");
    println(f"count={c}");                    // 2
}
```

Import `std.string` and call via the module name. `from_int`/`to_float` for conversions; `to_int` and `to_float` return Results: match, propagate or recover the error.
`string.join(Vec<string>, sep)` assembles text; it is unrelated to task joining.

### Concatenation, char round-trip, escapes

```hew
import std.string;
fn main() {
    let g = "Hello" + ", " + "world";
    println(g);
    match "Z".char_at(0) {
        .Some(ch) => println(string.from_char(ch as i64)),   // Z
        .None => println("out of bounds"),
    }
    var acc = "";
    for i in 0 .. 3 { acc = acc + "x"; }
    println(acc);                       // xxx
}
```

Build strings with `+`. `char_at` returns `Option<char>` — `Some` of the Unicode scalar at that codepoint offset, or `None` when out of bounds; consume it with `match`. Cast a `char` to its codepoint with `as i64`, then `string.from_char` renders it back. Strings are immutable — concatenation produces new strings.

## Traits and stdlib

### Trait declaration + impl + dot-call

```hew
trait Greet {
    fn greet(self) -> string;
}
type Person { name: string }
impl Greet for Person {
    fn greet(self) -> string {
        f"Hello, {self.name}!"
    }
}
fn main() {
    let p = Person { name: "Ada" };
    println(p.greet());   // Hello, Ada!
    println(p.greet());   // still valid — `self` borrows
}
```

There are three receivers and one spelling each. `self` borrows: the caller's
binding stays valid, so `p.greet()` twice is ordinary code. `var self` mutates
the receiver in place and requires a `var` binding — `let p = ...; p.bump()` is
refused with "requires a mutable binding receiver". `consume self` takes the
value, and any later use of the binding is a use-after-consume error. Naming the
first parameter after the target type (`fn greet(p: Person)`) is not a receiver
form; the fix-it rewrites it to `self`.

`var self` is available on trait methods. On an inherent `impl` method it is
refused today (`E_LIMIT_INHERENT_VAR_SELF`), because an inherent method receives
the value and the mutation would not reach the caller — declare the method on a
trait with a `var self` receiver and implement that trait for your type.

A trait method can carry a default body (`fn shout(self) -> string { self.greet() + "!!!" }` inside the trait declaration); an `impl` only needs to supply the methods it overrides, and an uncalled default falls back to the trait's body, dispatching through `self.method()` like any other trait call. Defaults work within a single file and across a file import (`import "other.hew";`, including a default that dispatches back through a required method declared in another file). Defaults also work when only the _trait_ comes from a directory module (`import mymod.{ Greet };`) and the implementing type is local. They do not yet resolve when the implementing type is ALSO imported from a directory module (e.g. `import gm.{ Dog };` where `Dog` and its `impl Greet for Dog` both live in `gm`) — calling an inherited default on that receiver fails with `no method 'greet' on 'gm.Dog'` rather than falling back to the trait's default body; that gap is tracked separately.

### Display trait (fmt) for f-string interpolation

```hew
type Point { x: f64, y: f64 }
impl Display for Point {
    fn fmt(self) -> string {
        f"({self.x}, {self.y})"
    }
}
fn main() {
    let pt = Point { x: 1.0, y: 2.0 };
    println(f"point = {pt}");   // point = (1, 2)
}
```

Implement `Display` via the `fmt` method and interpolate with f-strings. To print a user type, wrap it: `println(f"{value}")` — `println(value)` on a bare user Display type does not dispatch.

### Calling a Display fmt method directly

```hew
type Tag { id: i64 }
impl Display for Tag {
    fn fmt(self) -> string {
        f"#{self.id}"
    }
}
fn main() {
    let t = Tag { id: 7 };
    let s = t.fmt();
    println(s);   // #7
}
```

An explicitly-implemented trait method is callable directly with dot-syntax.

### Associated type in a trait

```hew
trait Counter {
    type Item;
    fn next_val(self) -> Self.Item;
}
type Ticker { current: i64 }
impl Counter for Ticker {
    type Item = i64;
    fn next_val(self) -> i64 {
        self.current + 1
    }
}
fn main() {
    let t = Ticker { current: 41 };
    println(t.next_val());   // 42
}
```

Declare `type Item;` in the trait, bind it with `type Item = <concrete>;` in the impl, and write the concrete type in the impl method signature. At most one associated type per trait.

### Trait impl for a builtin type (Vec)

```hew
trait Summable {
    fn total(self) -> i64;
}
impl Summable for Vec<i64> {
    fn total(self) -> i64 {
        var acc = 0;
        for x in self { acc += x; }
        acc
    }
}
fn main() {
    let v = [10, 20, 30];
    println(v.total());   // 60
}
```

You may add trait impls for builtin nominal types like `Vec`. (A bare inherent `impl Vec<T> { ... }` is reserved for stdlib and not available.)

### Builtin Option/Result via match (no import)

```hew
fn main() {
    let x = Some(42);
    let v = match x { .Some(n) => n, .None => 0 };
    println(v);   // 42
    let r: Result<i64, i64> = Ok(7);
    let w = match r { .Ok(n) => n, .Err(e) => e };
    println(w);   // 7
    let y: Option<i64> = None;
    println(match y { .Some(n) => n, .None => -1 });   // -1
}
```

Option/Result and their constructors are builtin — do not import `std.option`/`std.result`. Unwrap with `match`. Annotate a standalone `None` with a type.

### The ? operator on Result

```hew
fn try_parse(s: string) -> Result<i64, string> {
    if s == "bad" { Err("bad input") } else { Ok(10) }
}
fn parse_add(a: string, b: string) -> Result<i64, string> {
    let x = try_parse(a)?;
    let y = try_parse(b)?;
    Ok(x + y)
}
fn main() {
    match parse_add("a", "b") { .Ok(n) => println(n), .Err(e) => println(e) }       // 20
    match parse_add("bad", "b") { .Ok(n) => println(n), .Err(e) => println(e) }     // bad input
}
```

Use `?` to short-circuit Err and propagate it; the enclosing fn must return a Result whose Err type matches.

### std.string helpers

```hew
import std.string;
fn main() {
    println(string.from_int(42));            // 42
    println(string.to_int("100").unwrap_or(0));  // 100
    println(string.repeat("*", 3));          // ***
    println(string.pad_left("7", 3, "0"));   // 007
}
```

Import `std.string` and call via the module name. Most case/slice/trim/find operations are builtin methods on `string` itself; `std.string` is for conversions and padding. `to_int` returns `Option<i64>` — use `.unwrap_or(default)` or `match` to handle a parse failure.

### std.math helpers

```hew
import std.math;
fn main() {
    println(math.sqrt(16.0));      // 4
    println(math.abs(-5.0));       // 5
    println(math.max(3.0, 7.0));   // 7
    println(math.min(3.0, 7.0));   // 3
}
```

`abs`/`min`/`max` are generic over Num (work on `i64` and `f64`); `sqrt`/`pow`/`floor`/`ceil`/`round` take `f64`. Use `math.pi()`/`math.e()` (functions, not bare constants).

### std.iter — lazy iterator combinators

```hew
import std.iter;
fn main() {
    let v: Vec<i64> = [1, 2, 3, 4, 5];
    println(iter.sum(iter.map(v.iter(), |x: i64| x * 2)));         // 30
    println(iter.sum(iter.filter(v.iter(), |x: i64| x % 2 == 0))); // 6
    println(iter.fold(v.iter(), 0, |acc: i64, x: i64| acc + x));   // 15
}
```

`std.iter` builds lazy adapters (`map`, `filter`, `take`, `skip`) over any `Iterator`; terminal helpers (`fold`, `count`, `collect`, `any`, `all`, `sum`, `sum_f64`, `product`, `product_f64`) drive an adapter chain to completion. Drive a `Vec<T>` through the lazy surface via `v.iter()` (clones elements out, `v` stays live) or `v.into_iter()` (consumes `v`).

A `for` loop or index read borrows clone-free elements. In a generic body with
unbounded `T`, the same borrowing rule holds at every instantiation. Use
`into_iter()` to take ownership of elements, or an explicit clone with `T: Clone`.
Do not assume reads of cloneable elements are clone-free. Iterator adapters
consume the iterator they wrap; the source collection and its iterator have
distinct ownership obligations.

### std.sort — sorting vectors

```hew
import std.sort;
fn main() {
    var nums: Vec<i64> = Vec.new();
    nums.push(3); nums.push(1); nums.push(4); nums.push(1); nums.push(5);
    let sorted   = sort.sort_ints(nums);            // returns new Vec — original unchanged
    let reversed = sort.reverse_ints(sorted);
    println(sorted[0]);    // 1
    println(reversed[0]);  // 5

    var words: Vec<string> = Vec.new();
    words.push("banana"); words.push("apple"); words.push("cherry");
    let sw = sort.sort_strings(words);
    println(sw[0]);        // apple

    var floats: Vec<f64> = Vec.new();
    floats.push(3.14); floats.push(1.41); floats.push(2.72);
    let sf = sort.sort_floats(floats);
    println(sf[0]);        // 1.41
}
```

`sort<T: Ord>` returns a new sorted Vec (ascending) and `reverse<T>` returns a new reversed one; the original is never modified. One generic function covers every element type — the `sort_ints` / `sort_strings` / `sort_floats` family it replaces is listed in [the v0.6.0 migration note](migrations/v0.6.0.md). Integer and string sorting use iterative merge passes, so their comparison count is O(n log n); float sorting retains its total-order runtime implementation.

### std.random — pseudo-random number generation

```hew
import std.random;
fn main() {
    random.seed(42);                   // deterministic sequence
    let r = random.random();           // f64 in [0.0, 1.0)
    let n = random.randint(1, 7);      // i64 in [1, 7)  — like a d6 roll
    let g = random.gauss(0.0, 1.0);   // Gaussian sample

    println(f"{r} {n} {g}");
}
```

Backed by a CPython-compatible MT19937 Mersenne Twister — the same seed produces the same sequence as CPython's `random` module. Call `seed(n)` first for reproducible output; without a seed, the state is initialised from OS entropy. `randint(lo, hi)` returns in the half-open range `[lo, hi)`.

### std.time.datetime — timestamps and date arithmetic

```hew
import std.time.datetime;
fn main() {
    let now = datetime.now_ms();             // i64 epoch milliseconds
    println(datetime.to_iso8601(now));       // 2026-06-23T18:42:22Z
    match datetime.format(now, "%Y-%m-%d") {
        .Ok(text) => println(text),
        .Err(error) => println(f"format error: {error}"),
    }

    println(datetime.year(now));    // 2026
    println(datetime.month(now));   // 6
    println(datetime.day(now));     // 23
    println(datetime.hour(now));    // 18
    println(datetime.minute(now));  // 42

    let tomorrow = datetime.add_days(now, 1);
    let diff = datetime.diff_secs(tomorrow, now);
    println(diff);                  // 86400

    match datetime.parse("2026-01-01T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ") {
        .Ok(ts) => println(datetime.year(ts)),   // 2026
        .Err(e) => println(f"parse error: {e}"),
    }
}
```

Timestamps are `i64` epoch milliseconds throughout. `to_iso8601` formats as RFC 3339 UTC; `format(ts, fmt)` uses strftime-style patterns. `year`/`month`/`day`/`hour`/`minute`/`second`/`weekday` extract components. `add_days` / `add_hours` perform arithmetic; `diff_secs` returns the signed difference. Use `try_parse` when the input may be malformed; the format string must describe a complete date and time (a date-only pattern such as `"%Y-%m-%d"` fails with "input is not enough for unique date and time").

### std.deque — double-ended queue

```hew
import std.deque;
fn main() {
    let dq = deque.new();
    dq.push_back(1);
    dq.push_back(2);
    dq.push_front(0);     // [0, 1, 2]
    println(dq.len());    // 3
    println(dq.pop_front());  // 0
    println(dq.pop_back());   // 2
    println(dq.is_empty());   // false
    dq.close();
}
```

`deque.new()` returns a resource `Deque` handle. `push_front` / `push_back` add to either end; `pop_front` / `pop_back` remove and return the element (traps on empty). Current element type is `i64`. It closes automatically at scope exit; call `close()` to release it early.

### Multiple stdlib imports coexisting

```hew
import std.string;
import std.math;
import std.iter;
fn main() {
    println(string.from_int(math.max(2, 9)));   // 9
    let v: Vec<i64> = [1, 2, 3];
    println(iter.sum(v.iter()));                 // 6
}
```

`std.string`, `std.math`, `std.iter` import together cleanly — the safe stdlib trio to lean on. An unused import warns but still compiles.

### std.encoding.json — parsing and values

JSON values use automatic value cleanup. There is no manual `free()` step.
Parsing, encoding and typed access return errors explicitly; a missing field
is distinct from accessing a field on a non-object.

```hew
import std.encoding.json;

fn main() {
    let document = json.parse("{\"name\": \"Hew\"}") handle error {
        println(f"invalid JSON: {error}");
        return;
    };
    let field = document.get_field("name") handle error {
        println(f"field access failed: {error}");
        return;
    };
    let name = field else {
        println("name is absent");
        return;
    };
    let text = name.get_string() handle error {
        println(f"name is not text: {error}");
        return;
    };
    println(text);
}
```

`json.parse` returns `Result<Value, ParseError>`; `get_field` returns
`Result<Option<Value>, AccessError>`. `stringify` returns
`Result<string, EncodeError>`. Use `json.Value` in an explicit type annotation,
or import `Value` by name. See the [JSON module](../std/encoding/json/json.hew)
for constructors and collection operations.

### Importing your own modules

<!-- doctest: skip -->

```hew
import "helpers.hew";

fn main() {
    println(shout("hi"));
}
```

A quoted string path (`import "relative/path.hew";`) pulls in a sibling
source file directly — no project layout is required. This is the fastest
way to split a script into files.

For a project with a `src/` tree, `import src.a.b.c;` addresses
`src/a/b/c.hew` by dotted path and binds the module under its last
segment (`c` here) — call its public items as `c.function_name()` /
`c.CONST_NAME`, the same dotted-access form used for `std.string`,
`std.math`, and every other stdlib module:

```hew
import std.string;

fn main() {
    println(string.from_int(42));
}
```

Selective import (`import src.a.b.{Thing, other_fn};`) brings specific
names into scope unqualified instead of binding the module name. Both forms
resolve `pub` items only — a non-`pub` fn, type, `machine`, or `const` is
invisible outside its defining file.

**Reserved-word module path segments.** A dotted path segment may be a
keyword: `import src.workflow.machine.{Thing};`. Bare and selective imports
parse and resolve normally, as do `type` and `trait` segments.
The remaining restriction is on the _binding_, not the path — a bare
`import src.workflow.machine;` binds the module under the name `machine`,
and writing `machine.Thing` is then a parse error
(``unexpected `.` in block``). The same is true of `actor`. Use the
selective (`.{Name}`) form for a keyword-named module, or reach it via the quoted string-path import form above. Wildcard imports are retired.

**Directory-form modules.** A directory whose entry file's stem matches
the directory name (`greeting/greeting.hew`) is one module spanning every
`.hew` file in that directory — its peer files see each other's
declarations without any imports between them. A peer file has no import
identity of its own; reach its declarations through the directory module
(`import greeting;`), not the peer file directly. Imports between
modules, directory-form or single-file, must not form a cycle.

### `pub const` — module-level constants

```hew
const MAX_RETRIES: i64 = 3;

fn main() {
    println(MAX_RETRIES);
}
```

`const NAME: Type = expr;` declares a module-level constant; `pub const`
exports it. A const is evaluated once and is immutable — there is no `var
const`. Read a `pub const` from another module via dotted access on the
imported module (`reasons.MAX_RETRIES`) — selective import of a `const`
(`import src.reasons.reasons.{MAX_RETRIES};`, then bare `MAX_RETRIES`)
currently fails to resolve; dotted access is the only working form today.
Prefer a `pub const` over a zero-argument wrapper function
(`pub fn max_retries() -> i64 { 3 }`) for a fixed value — the const form is
shorter, makes the value's constancy visible at the call site, and avoids a
function-call indirection for something that never varies.

### Suppressing a lint

```hew
// hew:allow(dead_code)
pub fn reserved_for_future_use() -> i64 { 1 }

fn main() {
    println("hi");
}
```

`hew check`/`hew build`/`hew test` run a lint sweep after type-checking.
`dead_code` (an unreachable, never-called function) is the one most projects
hit first — it fires on **every** unreferenced function, `pub` included.
This is deliberate, not a bug: Hew has no package/library manifest yet to
mark a crate's public API surface as "used by design", so until that lands,
a `pub` function with no caller inside the program you're compiling is
indistinguishable from genuinely dead code, and the lint stays honest about
that rather than special-casing `pub` and going quiet on real dead code in
library-shaped files.

Suppress a finding in-source with `// hew:allow(<lint-name>)` (or
`// hew:allow(all)`) directly above the flagged item — this wins even under
`-D`. Suppress project-wide from the command line with `-A <lint-name>`
(`hew build -A dead_code`), or promote a lint with `-W`/`-D`. All three
flags are repeatable and accept `all` in place of a lint name. The
`dead_code` diagnostic's own suggested fix (`prefix with underscore:
_name`) is aimed at genuinely-unused private helpers; for a deliberately
exported function, reach for `// hew:allow(dead_code)` or `-A dead_code`
instead of renaming your public API to satisfy the linter.

## Structural equality

### Records and payload-bearing enums

Records and enums support `==` and `!=` out of the box; no extra
implementation is needed. Equality is structural: two record values are equal
when every field is equal, and two enum values are equal when they carry the
same variant and every payload field is equal. Fields of any equality-eligible
type participate — integers, `bool`, `char`, `string`, `duration`, nested
records/enums, and **floating-point** fields (`f64`/`f32`, see below).

```hew
type Point { x: i64, y: i64, }

enum Color {
    Red,
    Green,
    Blue,
    Custom(i64),
}

fn main() {
    // Record equality — field-by-field
    let a = Point { x: 1, y: 2 };
    let b = Point { x: 1, y: 2 };
    let c = Point { x: 1, y: 3 };
    println(a == b);   // true
    println(a == c);   // false
    println(a != c);   // true

    // Enum equality — unit variants
    println(Color.Red == Color.Red);     // true
    println(Color.Red == Color.Green);   // false

    // Enum equality — payload-bearing variants
    println(Color.Custom(42) == Color.Custom(42));   // true
    println(Color.Custom(42) == Color.Custom(99));   // false
    println(Color.Custom(42) != Color.Red);           // true
}
```

### Vec.contains relies on structural equality

`Vec<T>.contains(v)` walks the vector using the same element-wise equality,
so records and enums with payloads work transparently:

```hew
type Point { x: i64, y: i64, }

enum Tag { A, B(i64), }

fn main() {
    // Primitive and string elements
    var nums: Vec<i64> = Vec.new();
    nums.push(10); nums.push(20); nums.push(30);
    println(nums.contains(20));   // true
    println(nums.contains(99));   // false

    var words: Vec<string> = Vec.new();
    words.push("hello"); words.push("world");
    println(words.contains("hello"));   // true
    println(words.contains("bye"));     // false

    // Record elements
    var pts: Vec<Point> = Vec.new();
    pts.push(Point { x: 1, y: 2 });
    pts.push(Point { x: 3, y: 4 });
    println(pts.contains(Point { x: 1, y: 2 }));   // true
    println(pts.contains(Point { x: 5, y: 6 }));   // false

    // Payload enum elements
    var tags: Vec<Tag> = Vec.new();
    tags.push(Tag.A);
    tags.push(Tag.B(7));
    println(tags.contains(Tag.A));      // true
    println(tags.contains(Tag.B(7)));   // true
    println(tags.contains(Tag.B(8)));   // false
}
```

### Floating-point fields use bitwise (total) equality

Structural equality over a float field is **bitwise**, not IEEE numeric. The
compiler compares the raw bit patterns of the two floats, so:

- it is **reflexive**: `x == x` holds for every value, including `NaN`. Two
  `NaN` values compare _equal_ when their bit patterns are identical.
- `+0.0` and `-0.0` are **distinct** (their bit patterns differ), so a record
  holding `+0.0` is not equal to one holding `-0.0`.

This is deliberately different from the IEEE numeric `==` you get on a bare
`f64`/`f32` expression, where `NaN != NaN` and `+0.0 == -0.0`:

```hew
type Vec2 { x: f64, y: f64 }

fn main() {
    let nan = 0.0 / 0.0;

    // Scalar `==` is IEEE numeric.
    println(nan == nan);   // false  (NaN != NaN)
    println(0.0 == -0.0);  // true   (signed zeros compare equal)

    // Structural `==` over a record is bitwise/total.
    let a = Vec2 { x: nan, y: 1.0 };
    let b = Vec2 { x: nan, y: 1.0 };
    println(a == b);       // true   (identical bit patterns, reflexive)

    let pz = Vec2 { x: 0.0, y: 0.0 };
    let nz = Vec2 { x: -0.0, y: 0.0 };
    println(pz == nz);     // false  (+0.0 and -0.0 differ in bits)
}
```

Bitwise semantics are what structural equality needs to stay reflexive. This
guarantee applies to **structural positions** — record and enum fields, and
`HashMap`/`HashSet` keys — so a float-bearing record or enum can always find
itself in a `HashMap`/`HashSet` lookup, and dedup over records works correctly.
The hash of a float field is computed from the same bit pattern, so `==`
implies an equal hash and a float-bearing `record` is a sound `HashMap` key.

> **Sharp edge:** `Vec<f64>` and bare scalar `==` on `f64` remain IEEE.
> `[nan].contains(nan)` is `false` (scalar IEEE `==`, NaN ≠ NaN), while
> `HashSet<f64>` (which stores `f64` as a structural position) treats two
> identical NaN bit-patterns as equal. The reflexive/bitwise guarantee does
> not extend to `Vec<f64>.contains` or direct `f64 == f64` expressions.

```hew
type Coord { x: f64, y: f64 }

fn main() {
    var m: HashMap<Coord, i64> = HashMap.new();
    m.insert(Coord { x: 1.5, y: 2.5 }, 42);
    let v = m.get(Coord { x: 1.5, y: 2.5 });
    match v {
        .Some(n) => println(n),   // 42 — structurally equal key round-trips
        .None => println(-1),
    }
}
```

Float ordering (`<`, `<=`, `>`, `>=`) is unaffected: it stays IEEE-partial. Only
equality (`==`/`!=`) and hashing use the bitwise/total form.

### bytes field restriction

Equality is rejected at compile time for any record or enum type that
contains a `bytes` field. The checker emits a diagnostic rather than
comparing raw buffer bytes, which would produce unreliable results for
refcounted heap handles:

<!-- doctest: skip -->

```hew
type Packet { data: bytes, }
// Packet { data: bytes } == Packet { data: bytes }
// ^^^ rejected: `==` on record type `Packet` is not supported because a
//     field or payload contains layout-managed/non-Copy data `bytes`
```

Compare individual eligible fields or use a method that extracts the
comparable portion instead.

## Shipped surfaces

Each subsection below is a runnable snippet; a full idiomatic program per
surface lives under [`examples/v05/surfaces/`](../examples/v05/surfaces)
(text surfaces), [`examples/channel/`](../examples/channel) (channels), or
[`examples/net/`](../examples/net) (networking).

### `#[wire]` — network-serializable schema types

```hew
#[wire]
type UserCreated {
    id: u64 @1,
    name: string @2,
}

fn main() {
    let e = UserCreated { id: 42, name: "ada" };
    let j = e.to_json();
    println(j);                          // {"id":42,"name":"ada"}
    match UserCreated.from_json(j) {
        .Ok(back) => println(back.name),  // ada
        .Err(_) => println("parse failed"),
    }
}
```

Each field carries a `@N` tag — a stable wire identifier that must never be
reused, even if the field is later removed (HEW-SPEC-2026.md §7.2). A field
marked `optional` must have type `Option<T>`; the marker admits only a value
that can represent absence. Presence and value shape are separate contracts:

| Declaration | Encode | Missing key | Explicit `null` |
| --- | --- | --- | --- |
| `value: T @1` | key is always emitted | decode error | decode error |
| `value: Option<T> @1` | key is always emitted; `None` is `null` | decode error | `None` |
| `value: Option<T> @1 optional` | `None` omits the key; `Some` emits it | `None` | `None` |

`Option<Option<T>>` is rejected because the null encoding cannot distinguish
its inhabitants. Unknown numeric CBOR tags and unknown JSON/YAML names remain
tolerated. Changing an existing field between required and `optional` changes
wire behaviour, so `hew wire check` rejects the change in either direction.

`e.to_json()` and `TypeName.from_json(text)` round-trip a wire type through
JSON; the latter is a call on the type name itself rather than a source
spelling. Bare values that implement `Serializable`, including admitted
`HashMap` and `HashSet` shapes, use the generic `std.encoding.wire` facade:

```hew
import std.encoding.wire;

#[wire]
type Feature {
    enabled: bool @1,
}

fn main() {
    var features: HashMap<string, Feature> = HashMap.new();
    features.insert("preview", Feature { enabled: true });

    let json = wire.to_json(features);
    let parsed = wire.from_json<HashMap<string, Feature>>(json) handle error {
        println(f"decode failed: {error}");
        return;
    };
    println(parsed.len());
    let cbor = wire.encode(features);
    let decoded = wire.decode<HashMap<string, Feature>>(cbor);
    println(decoded.len());
}
```

`wire.to_yaml` and `wire.from_yaml<T>` provide the equivalent YAML surface.
These functions use the same typed codec descriptor and compiler-emitted
thunks as `#[wire]` methods and actor transport; collections do not need
format-specific methods or a wrapper record. Text parsing returns
`Result<T, string>`. Binary `decode<T>` retains the trusted-input,
trap-on-malformed-data contract of `TypeName.decode(bytes)`.

The runtime envelope used for actor-to-actor message transport is CBOR; the
text surfaces are for cross-service and file I/O. Use `hew wire check
<file.hew> --against <baseline.hew>` to check schema
compatibility between two versions of a wire type.

Full example: [`examples/playground/types/wire_types.hew`](../examples/playground/types/wire_types.hew). Spec: HEW-SPEC-2026.md §7.

### `#[resource]` and `#[linear]` — compiler-checked resource ownership

```hew
#[resource]
type Conn {
    fd: i64
}
impl Conn {
    fn close(consume self) {
        println(f"closing fd {self.fd}");
    }
}

fn main() {
    let c: Conn = Conn { fd: 7 };
    println("work");
    // c drops at scope exit here; Conn.close(c) runs automatically.
}
```

```hew
#[linear]
type Tx { id: i64 }
impl Tx {
    fn commit(consume self) { println(f"commit {self.id}"); }
    fn rollback(consume self) { println(f"rollback {self.id}"); }
}

fn main() {
    let t = Tx { id: 1 };
    t.commit();   // commit 1
}
```

Both attributes are affine — the compiler enforces a single live binding per
value via the move checker — but they differ in what happens at scope exit.
A `#[resource]` type auto-closes through `close(consume self)` returning unit
unless closed early; a `#[linear]` type has **no** implicit drop at all — leaving one
unconsumed at scope exit is a compile error. Neither supports a
user-defined `impl Drop`.

The `close` method (for `#[resource]`) and any `consume self` method (for
`#[linear]`) must be declared in a sibling `impl` block, never inline in the
type body — an inline declaration is rejected at parse time. A factory
function that constructs and returns a `#[resource]` or `#[linear]` value
works the same whether it lives in the current module or an imported one —
close/consume discipline is enforced on the value, not on where it was built.

Full example (`#[linear]`): [`examples/v05/linear/accept/linear_consumed_via_rollback_on_err.hew`](../examples/v05/linear/accept/linear_consumed_via_rollback_on_err.hew). For `#[resource]`, see HEW-SPEC-2026.md §3.7.8.

### `#[opaque]` — FFI-backed handle types

```hew
#[opaque]
pub type FileHandle {
}
```

An `#[opaque]` declaration exposes an external handle without fields or a
record-literal constructor. Use its declared ownership and release contract;
opacity does not make a handle freely copyable or remove cleanup obligations.
A resource's close method consumes its receiver and returns unit. Fallible
completion is a separate operation whose result the caller handles.

Supported local actor boundaries may transfer owned resources. Remote messages
need serializable data, not process-local handles. Consult the particular
module's declaration instead of assuming every opaque type has the same copy,
release or actor-admission behaviour.

### Typed streams — `sink.send(x)` / `stream.recv()`

```hew
import std.stream;
import std.encoding.utf8;

actor Echo {
    let n: i64,
    receive fn run(unused: i64) {
        let (sink, input) = match stream.bytes_pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
        for i in 0..n {
            sink.send(f"x{i}".to_bytes());
        }
        sink.close();
        var done = false;
        while !done {
            let item = input.recv();
            match item {
                .Some(b) => match utf8.decode(b) {
                    .Ok(text) => println(text), // x0, x1
                    .Err(error) => println(f"invalid text: {error}"),
                },
                .None => { done = true; },
            }
        }
    }
}

fn main() {
    let e = spawn Echo(n: 2);
    match e.run(0) {
        .Ok(_) => {},
        .Err(error) => println(f"stream operation failed: {error}"),
    }
    close(e);
}
```

`sink.send(x)` and `stream.recv()` suspend the calling coroutine
instead of OS-parking a worker, so a stream stage frees its worker while waiting.
Only `Stream<bytes>` / `Sink<bytes>` suspend (the canonical element type), and the
canonical method names are `recv()` / `send()` — not `next()` / `write()`. `recv()`
yields `Option<bytes>` (`None` is EOF); match it, never unwrap. `sink.send`
is statement-position only. Build the pipe with the public
`std.stream.bytes_pipe(capacity)` constructor — no raw extern, no `unsafe` — and
turn text into a frame with the public `string.to_bytes()` surface. Keep both
ends in one handler: moving an owned `Stream`/`Sink` into actor state is not
yet supported (`OwnedHandleAggregateExtractionUnsupported`). Full example:
[`examples/v05/surfaces/typed_streams.hew`](../examples/v05/surfaces/typed_streams.hew).

### Channels — bounded delivery

`channel.new(capacity)` constructs sender and receiver halves. `rx.recv()`
waits for an item and yields `Option<T>`; `None` means the channel is closed.
A full `tx.send(value)` parks the coroutine until it can proceed. The halves
have automatic cleanup; explicit `.close()` ends the corresponding half early.

The native path supports send/receive suspension, non-parking `try_recv`,
and channel halves stored in records and vectors. Channel-receive selection
registers alongside tasks and timers: write `value from rx.recv() => ...`,
without `await` in its source. Actor-call and stream selection remain separate
implementation gaps; a parsed arm alone does not establish native support. See the
[channel module](../std/channel/channel.hew) for declarations and the
[selection contract](specs/HEW-SPEC-2026.md#411-select) for intended behaviour.

### Regex captures — `capture` / `find_all` / `find_all_submatch`

```hew
import std.text.regex;

fn main() {
    let re = regex.new("(?P<k>[a-z]+)=(?P<v>[0-9]+)");
    let text = "a=1 bb=22";
    match re.capture_named(text, "v") {
        .Some(v) => println(f"first value: {v}"),   // first value: 1
        .None => println("none"),
    }
    let rows = re.find_all_submatch(text);
    for i in 0..rows.len() {
        let key = match rows.group(i, 1) { .Some(g) => g, .None => "?" };
        let val = match rows.group(i, 2) { .Some(g) => g, .None => "?" };
        println(f"{key} -> {val}");   // a -> 1 ; bb -> 22
    }
    re.close();
}
```

Compile a pattern with `regex.new` (panics on bad syntax; use `try_new` for a
`Result`). `capture(input, group)` / `capture_named(input, name)` return the
indexed/named submatch of the FIRST match as an `Option<string>` — group 0 is the
whole match. `find_all` returns every whole match as a `Vec<string>`;
`find_all_submatch` returns a row-major `CaptureMatches` table — `rows.len()`
rows, `rows.width()` groups each, read with `rows.group(row, col)` /
`rows.whole(row)`. `Pattern` is a `#[resource]` — it auto-closes at scope
exit; call `close()` to release it early. Full example:
[`examples/v05/surfaces/regex_captures.hew`](../examples/v05/surfaces/regex_captures.hew).

### Templates — `parse` + `render_try`

```hew
import std.text.template;

fn main() {
    var ctx = template.new_ctx();
    ctx.set_str("name", "Hew");
    var xs: Vec<string> = Vec.new();
    xs.push("a");
    xs.push("b");
    ctx.set_list("xs", xs);
    let t = template.parse("hi {{.name}}:{{range .xs}} {{.}}{{end}}") handle error {
        println(f"template parse failed: {error}");
        return;
    };
    match template.render_try(t, ctx) {
        .Ok(s) => println(s),   // hi Hew: a b
        .Err(_) => println("error"),
    }
}
```

Build a flat `Ctx` with `new_ctx()` in a `var` binding, then call its
`set_str`, `set_int`, `set_bool` or `set_list` methods. `parse` compiles a
Go-style template — `{{.key}}` substitutes, `{{if .key}}…{{end}}` is conditional,
`{{range .list}}…{{.}}…{{end}}` iterates with `.` bound to each item. Render with
the free function `template.render_template(t, ctx)` (panics on error) or
`template.render_try(t, ctx)` (returns `Result`); the method form `t.render(ctx)`
is not yet implemented. `TemplateError` has no `Display`, so handle the `Err` arm
with a literal message rather than interpolating it. Full example:
[`examples/v05/surfaces/template_render.hew`](../examples/v05/surfaces/template_render.hew).

### Unicode — rune helpers + classification predicates

```hew
import std.text.unicode;

fn main() {
    let s = "Aé!";
    println(f"runes={unicode.rune_count(s)} bytes={s.len()}");   // runes=3 bytes=4
    let runes = unicode.runes(s);
    for cp in runes {
        let up = unicode.is_upper(cp);
        println(f"cp={cp} upper={up} width={unicode.rune_len(cp)}");
    }
}
```

`rune_count(s)` counts codepoints (vs `s.len()` bytes); `runes(s)` decodes a
string into a `Vec<i64>` of codepoints; `codepoint_at` / `try_codepoint_at` read
one rune at a byte offset; `rune_len(cp)` is a rune's UTF-8 width. Classify a
codepoint with the predicates `is_upper` / `is_lower` / `is_digit` / `is_letter`
/ `is_space` / `is_punct` / `is_alnum` / `is_valid_rune`, and case-fold with
`to_upper` / `to_lower` / `to_title` (all over `i64` codepoints). Dispatch with
`match` guards over the predicates rather than range checks. Full example:
[`examples/v05/surfaces/unicode_runes.hew`](../examples/v05/surfaces/unicode_runes.hew).

### Scanner — line and word tokenisation

```hew
import std.io.scanner;

fn main() {
    var sc = scanner.from_string("alpha beta\ncolour");
    sc = scanner.with_split(sc, .SplitWords);
    sc = scanner.scan(sc);
    while scanner.has_next(sc) {
        println(scanner.text(sc));
        sc = scanner.scan(sc);
    }

    let (next, line) = scanner.next_line(scanner.from_string("first\nsecond"));
    match line {
        .Some(s) => println(s),   // first
        .None => println("none"),
    }
    let _ = next;
}
```

`scanner.from_string` and `scanner.from_stdin` construct a `Scanner` directly;
`scanner.from_file(path)` returns `Result<Scanner, fs.IoError>`, so match
`Ok`/`Err`. Drive the value-state API with `scan(sc) -> Scanner`, then read the
current token with `text(sc)` only when `has_next(sc)` is true. Use
`with_split(sc, SplitWords)` for whitespace tokens; the default is
`SplitLines`. `next_line(sc)` is a convenience helper returning the updated
scanner plus `Option<string>`. Full example:
[`examples/v05/surfaces/scanner_tokens.hew`](../examples/v05/surfaces/scanner_tokens.hew).

### Suspending HTTP — async client + server

The flagship networking surface is a suspending HTTP/1.1 client and
server built on `net.connect` / `net.listen` plus the pure-Hew codecs in
`std.net.http.http_async_client` / `http_async_server`. A server handler
accepts a connection, drives a `conn.read_string()` loop until the
request is buffered, then replies; a client writes a request and reads the
response. Each call suspends the handler (not the worker), so one worker can
serve and fetch on the same thread. The request/response codecs are pure and
runnable in isolation:

```hew
import std.net.http.http_async_client;

fn main() {
    let parts = http_async_client.split_address("http://127.0.0.1:8080/health");
    println(f"connect to {parts.0}");   // connect to 127.0.0.1:8080
    let raw = "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"ok\":true}";
    let resp = http_async_client.parse_response(raw);
    println(f"status={resp.status()} type={resp.content_type()} body={resp.body()}");
}
```

Build requests with `build_get(host, path)` / `build_request(...)` and parse
replies with `parse_response` → `status()` / `body()` / `header(name)` /
`content_type()`; the server side parses with `parse_request` → `method()` /
`path()` / `header(name)` / `is_valid()` and builds replies with
`response_text` / `response_json`. The `Listener` / `Connection` handles must be
handler LOCALS — an opaque handle in actor state is rejected by the
supervisor-restart clone gate. Full client+server program (two routes):
[`examples/net/http_await_service.hew`](../examples/net/http_await_service.hew)
(run with `HEW_WORKERS=1` to see the single-worker serve+fetch proof).

### Deadline forms — `scope within d`, a `select` timer arm, and socket timeouts

There is no timeout combinator. A read or an accept is a plain suspending
call, and a deadline over it comes from one of three places:

| Deadline                             | Bounds                                    |
| ------------------------------------ | ----------------------------------------- |
| `scope within d { .. } handle failure { .. }` | every child started in the block   |
| `after d => ..` inside `select { }`  | the wait for the first of several sources |
| `conn.set_read_timeout(ms)` / `set_write_timeout(ms)` | that socket's own operations |

If explicit runtime shutdown begins while a read or accept is already parked,
it resumes as `Err(NetError.Cancelled(0))`. A socket timeout reports
`Err(NetError.TimedOut(_))`.

A deadline starts cancellation; it does not skip child or resource cleanup.
Use a value-producing scope and recover its structured failure after cleanup:

```hew
fn work() -> i64 { sleep(10ms); 42 }
fn main() {
    let result = scope within 1s {
        let task = fork work();
        await task
    } handle failure {
        println("work did not complete");
        0
    };
    println(result);
}
```

An operation's ordinary `Err` stays a value to handle at the call. Explicit
`exit(code)` flushes standard streams and terminates without scope cleanup.
An unrecovered fault prints its typed diagnostic and exits 1; an explicit
non-zero program exit status is preserved.

### Remote ask suspension — a remote `Pid<T>.ask` from an actor handler

```hew
actor Echo {
    receive fn handle(req: i64) -> i64 { req }
}

impl ActorMsg for Echo {
    type Msg = i64;
    type Reply = i64;
}

actor Client {
    receive fn go(unused: i64) {
        let found: Result<RemotePid<Echo>, LookupError> = Node.lookup("echo");
        match found {
            .Ok(peer) => {
                let reply = peer.ask(7, 1000);
                match reply {
                    .Ok(n) => println(f"answer={n}"),
                    .Err(_) => println("ask failed"),
                }
            },
            .Err(_) => println("lookup failed"),
        }
    }
}
```

`Pid<T>` names an actor, wherever it lives; a lookup through the node registry
returns one for a peer. From an actor handler, `peer.ask(msg, timeout_ms)`
lowers to the cross-node suspending remote-ask path and returns
`Result<T.Reply, ActorError>` on resume; match `Ok`/`Err` instead of assuming a
reply. A same-node lookup can still return a handle that routes through the
remote path, and the local-mailbox bridge for that path is scoped to fail
closed as `ActorError.RoutingFailed`; use a direct actor call when both actors
are intentionally local.

> **Not yet in this build.** The one-identity surface is ratified
> (HEW-SPEC-2026 §2.1.1): `Pid<A>` names a local or remote actor and
> `RemotePid` is the internal wire form. The compiler still spells the two
> types separately, which is why the example above writes `RemotePid<Echo>`
> and `LocalPid<T>` appears elsewhere in this guide.

Full example:
[`examples/distributed/kv_client.hew`](../examples/distributed/kv_client.hew)
and [`examples/distributed/kv_server.hew`](../examples/distributed/kv_server.hew).

#### Key-backed node identity and peer authentication (native only)

Every distributed node identity is derived from its stable authenticated public
credential. `NodeConfig.key` names the file that holds it, `Node.start` loads or
creates it, and `Node.identity_key()` returns the public half as lowercase
hexadecimal for out-of-band exchange.

The runtime computes:

```text
SHA-256(
  "hew-node-id-v1\0"
  || credential-kind byte
  || credential length as u32 big-endian
  || canonical credential bytes
)[0..16]
```

The resulting `NodeId` renders as 32 lowercase hexadecimal digits. The
credential-kind byte is `1` for a TCP Noise static key and `2` for a canonical
TLS leaf SPKI. Keeping the key preserves the `NodeId`; rotating the key rotates
the identity.

`NodeConfig.peers` lists the peer credentials this node will accept, and a
peer's receiver-local non-zero `u16` route slot is its one-based position in
that list, so the first entry is slot `1`. Slot `0` is reserved for local
dispatch. A route slot is only a compact alias inside the configuring process:
it never becomes the peer's identity and may differ on every node.

Starting a node is one call, and it returns `Result<(), NodeError>`, so a
refused start stops the program instead of printing and carrying on:

<!-- doctest: skip -->

```hew
var config = NodeConfig.at("0.0.0.0:9000");
config.transport = "quic-mesh";
config.key = "node.key";
config.peers = ["3059301306072a8648ce3d020106082a8648ce3d030107"];
println(f"pin this credential on peers: {Node.identity_key()}");

match Node.start(config) {
    .Ok(_) => println("node up"),
    .Err(e) => println(f"node refused: {e}"),
}
```

`Node.set_transport`, `Node.load_keys`, and `Node.allow_peer` are gone: each
carried one fact that is now a `NodeConfig` field. The shipped compiler still
has the older call sequence and an untyped `Node.start(addr)`
(hew-lang/hew#3256).

On TCP, the pinned credential is the peer's 32-byte Noise public key. On
quic-mesh, it is the peer certificate's canonical SPKI. An unbound or mismatched
credential is rejected before the peer becomes routable.

A client selects its local pin when connecting. The slot it names is the
server's position in the client's own `NodeConfig.peers`, so a client that
lists the server first dials slot `1`:

<!-- doctest: skip -->

```hew
match Node.connect("1@127.0.0.1:9000") {
    .Ok(_) => {},
    .Err(e) => println(f"dial refused: {e}"),
}
```

The `1@` prefix is the client's route slot for that server, and it is local to
the client: the same server may sit at a different slot on every node. The
authenticated key, not the numeric prefix, determines the server's `NodeId`.

Each successful start also advances a durable non-zero session incarnation in
the key's journal. A same-key restart therefore has the same `NodeId` and a
higher session. A remote handle carries the complete `Location`:

```text
{ node: NodeId, slot: u64, incarnation: u32 }
```

The value is an allocation-free 32-byte inline handle. It has no reference
count, does not keep the actor alive, and cannot be constructed from raw
integers. A PID captured before a same-key restart fails with `StaleRef`, even
if the replacement process reuses the same actor slot.

Registry names are discovery aliases. Repointing a name affects future
`Node.lookup` calls but does not rewrite or revoke a previously issued
handle.

The registry knows what it holds. `Node.register(name, pid)` returns
`Result<(), RegisterError>` and records the actor's declaration identity beside
its location; `Node.lookup<A>(name)` compares that record against `A` and
answers `Err(LookupError.TypeMismatch)` when they disagree, so the type
argument on a lookup is checked rather than trusted. `Node.register` is the one
registration verb: it registers locally whether or not a node has started, and
publishes cluster-wide once one has.

Remote monitors deliver one typed notification through `#[on(down)]`:

<!-- doctest: skip -->

```hew
import std.link_monitor.{DownNotification, DownReason};

actor Watcher {
    #[on(down)]
    fn on_down(note: DownNotification) {
        match note.reason {
            DownReason.Exited => println("peer exited"),
            DownReason.Crashed(_) => println("peer crashed"),
            DownReason.MonitorLost => println("peer became unreachable"),
            DownReason.LocalShutdown => println("local node shut down"),
        }
    }
}
```

`MonitorRef.id()` matches `DownNotification.monitor`. Closing the handle
removes the registration and delivers no notification.

The complete protocol and identity rules are normative in
[`HEW-DIST-SPEC.md`](specs/HEW-DIST-SPEC.md). Distributed nodes, remote
messaging, and link/monitor propagation are native-only; wasm32 rejects these
surfaces.

### TLS client — free-function surface

```hew
import std.net.tls;

fn main() {
    let stream = tls.connect("example.com", 443);
    let req = "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n";
    let payload = req.to_bytes();
    let sent = tls.write(stream, payload).expect("write succeeds");
    println(f"sent {sent}/{payload.len()} bytes");
    // match tls.read(stream, 256) { .Ok(data) => ..., .Err(_) => ... }
    tls.close(stream);
}
```

Use the FREE-FUNCTION surface — `tls.connect(host, port)` (system-root verified),
`tls.write`, `tls.read`, `tls.close` — each returning a `Result`. Request/response bodies
are `bytes`: build a payload with the public `string.to_bytes()` surface and decode
with `bytes.to_string()`. There is also a method form (`stream.read(n)` /
`stream.write(payload)`, from `trait TlsStreamMethods`) — it type-checks and
compiles, and carries the same runtime caveat as the free functions below.

> **Known gap:** `tls.connect` does not record a failed handshake in
> `last_error()` — a failed connect returns a zero-value `TlsStream` with no
> way to retrieve why. `tls.write` sends correctly against a real endpoint,
> but `tls.read` currently crashes the process with a memory-safety panic
> (`ptr::copy_nonoverlapping` alignment violation) on a real connection —
> do not call it yet; the commented-out line above shows the intended shape
> once the data-plane FFI bridge is fixed.

Full example: [`examples/net/tls_client.hew`](../examples/net/tls_client.hew).

### `process.run` vs `process.run_argv` — shell vs no-shell

```hew
import std.process;

fn main() {
    let out = process.run("echo shell-form").expect("run succeeds");
    println(out.stdout.trim());

    var args: Vec<string> = Vec.new();
    args.push("no-shell-form");
    let result = process.run_argv("echo", args).expect("run_argv succeeds");
    println(result.stdout.trim());
}
```

`process.run(command: string)` hands `command` to the system shell (`sh -c`
on POSIX) and returns `Result<CommandOutput, ProcessError>`. Because it goes
through a shell, `command` is subject to shell quoting, globbing, and
injection the same way a hand-built shell string always is — building
`command` by concatenating untrusted input is a shell-injection bug in Hew
exactly as it would be in any language's `system()`/`sh -c` wrapper. This is
existing, documented behaviour, not a defect: a shell-executing `run` is
useful for exactly the cases where you want shell features (pipes,
redirects, globs), and `process.run_argv(command, args: Vec<string>)` is the
no-shell alternative — it execs `command` directly with `args` as an argv
array, so arguments containing spaces, quotes, or shell metacharacters pass
through literally with no injection surface. Prefer `run_argv`/`try_run_argv`
whenever any part of the command line comes from outside your source code;
reach for `run`/`try_run` only when you deliberately want shell semantics.

## Testing — `hew test`

### Discovering tests: `#[test]` functions and file layout

```hew
fn add(a: i64, b: i64) -> i64 { a + b }

#[test]
fn add_two_positive_numbers_returns_sum() {
    assert_eq(add(2, 3), 5);
}
```

`hew test <path>` (a file or a directory) discovers every function tagged
`#[test]` and runs each in its own isolated compiled program. A file is a
_test file_ — eligible for discovery at all — when its name ends in
`_test.hew`, or when it lives inside a directory named `tests/`; a `#[test]`
function in a plain, non-matching file is never discovered, silently.
Running `hew test .` over a directory recurses and aggregates every
discovered test file into one report:

```
running 3 tests
test add_two_positive_numbers_returns_sum ... ok
test divide_by_zero_traps ... ok
test slow_integration_probe ... ignored
test result: ok. 2 passed; 0 failed; 1 ignored
```

Each test compiles and runs as its own native subprocess — one test's
panic, timeout, or stray `std.process.exit` cannot take down another
test in the same run, and there is no shared global state between tests by
default.

> **Discovery is per-file, not per-project.** A `#[test]` fn in a file that
> doesn't end in `_test.hew` and isn't under `tests/` is invisible to `hew
test` — no warning, no error, it's simply never found. If a suite's pass
> count looks lower than expected, run `hew test <path> --list` first to see
> exactly what was discovered before debugging individual tests.

### Assertions: `assert`, `assert_eq`, `assert_ne`

`assert(condition: bool)` panics when `condition` is `false`. `assert_eq(a,
b)` and `assert_ne(a, b)` compare with the same equality `==` uses and panic
with both values on failure — prefer them over `assert(a == b)` for the
readable failure message. Both operands share one type `T: Eq + Display`, so a
value has to be comparable and able to print itself. `Option` and `Result` gain
`Display` at v0.7.0, and until then a test compares one by matching on it:

```
---- wrong_expectation_fails ----
assertion failed: left != right
  left: 4
  right: 5
```

These are the same panic-based builtins used everywhere else in Hew
(`assert`, `assert_eq`, `assert_ne` — see "Assertions" above); a test
failure IS a panic, nothing test-framework-specific.

### `#[should_panic]` — tests that must panic

```hew
fn divide(a: i64, b: i64) -> i64 { a / b }

#[test]
#[should_panic]
fn divide_by_zero_traps() {
    divide(10, 0);
}
```

A `#[should_panic]` test passes when the function body panics (including a
runtime trap, like the integer division above) and fails when it completes
normally. There is no message-matching form (no `#[should_panic(expected =
"...")]`) — it only checks that a panic occurred.

### `#[ignore]` — skip by default

```hew
#[test]
#[ignore]
fn slow_integration_probe() {
    assert(true);
}
```

An `#[ignore]`d test is discovered and listed but not run by a plain `hew
test` — it reports `ignored`, not `ok`. Run it (and every other ignored test
in the run) with `--include-ignored`.

### `#[serial]` — mutual exclusion for shared-state tests

Tag a test `#[serial]` when it touches process-wide shared state (an
environment variable, a fixed filesystem path, a fixed port) that would
race against another test running concurrently. All `#[serial]`-tagged
tests in a run are mutually exclusive with each other (never execute
concurrently), while non-`#[serial]` tests keep running in parallel around
them. `#[serial]` does not, by itself, fix a resource conflict — it only
serializes the tests that opt in; isolate the resource (a per-test temp
dir, an ephemeral port) instead of reaching for `#[serial]` wherever
possible, and use it as the fallback for the cases that generally can't be
isolated (a fixed env var read by library code you don't control, for
example).

### Filtering, listing, and sharding

```
hew test .                    # run everything discovered under .
hew test . --list             # list discovered test identities, run nothing
hew test . --filter add_two   # run only tests whose name contains "add_two"
hew test . --partition hash:1/2   # run this run's stable 1-of-2 shard
```

`--filter <pattern>` matches on substring of the test's bare name.
`--partition hash:SHARD/TOTAL` (one-based `SHARD`) splits the discovered
set into `TOTAL` stable hash-based shards — the same test always lands in
the same shard across runs, which is what a sharded CI matrix needs.
`--list` prints `<file>::<test-name>` identities without compiling or
running anything; run it first when a discovery count is a surprise.

### Output formats: text and `--format junit`

`--format junit` emits a JUnit XML report instead of the default coloured
text — wire it into CI systems that already consume JUnit:

```
hew test . --format junit
```

```xml
<?xml version="1.0" encoding="UTF-8"?>
<testsuites name="hew test" tests="3" failures="0" skipped="1" time="0.43">
  <testsuite name="math_test.hew" tests="3" failures="0" skipped="1" time="0.43">
    <testcase name="add_two_positive_numbers_returns_sum" classname="math_test.hew" time="0.22"></testcase>
    <testcase name="divide_by_zero_traps" classname="math_test.hew" time="0.21"></testcase>
    <testcase name="slow_integration_probe" classname="math_test.hew" time="0.00"><skipped/></testcase>
  </testsuite>
</testsuites>
```

`--no-color` drops ANSI colour codes from the default text output (useful
for log capture that doesn't strip escape codes on its own).

### Timeouts

`--timeout <duration>` (default `30`, meaning 30 seconds; accepts `500ms`,
`30s`, `1m`) bounds each individual test's wall-clock time — a hung test
fails with a timeout message instead of hanging the whole run. Use
`--jobs N` (`-j`) to cap how many tests compile/run concurrently; it
defaults to the host's physical core count.

### The empty-run gate — `--allow-empty`

`hew test <path>` fails (non-zero exit) when discovery finds no tests to run:
`No test files found.` when the path itself is empty or typo'd, `No test
functions found.` when a discovered test file has zero `#[test]` functions —
this is deliberate, so a typo'd path or an empty `tests/` directory doesn't
silently report success in CI.
Pass `--allow-empty` for the rare case where an empty result is
legitimately fine (e.g. a generated/optional test directory that may not
exist yet on some branches).

## Appendix A - FFI boundary types

Hew uses `&T` only inside the parameter and return type trees of functions in
an `extern` block. It describes an immutable, non-owning foreign view with the
same one-pointer ABI as C's `const T*`:

```hew
extern "C" {
    fn current_value() -> &i64;
    fn read_value(value: &i64) -> i64;
}

fn read_current() -> i64 {
    let view = unsafe { current_value() };
    unsafe { read_value(view) }
}
```

The foreign implementation owns the pointee and must keep it alive for every
use. A view returned by foreign code may be held in an inferred local, read, or
passed to another foreign function only while that foreign lifetime guarantee
holds. Hew does not retain, clone, or drop the pointee.

Ordinary function signatures, fields, aliases, and other Hew declarations use
`T`, not `&T`. There is no `&expr` operation and no conversion from `T` to
`&T`. Mutable FFI access uses the raw-pointer spelling `*mut T`; Hew does not
provide `&mut T` or `&var T`.
