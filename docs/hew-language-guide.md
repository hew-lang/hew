# Hew Language Guide

A reference for writing correct idiomatic Hew. Every example below was executed against `target/debug/hew` and ran clean.

## Toolchain quick-start

```
# Run a program
hew run hello.hew

# Pass arguments to your program (the -- separator is required)
hew run hello.hew -- Alice 42

# Scaffold a new project (creates main.hew; see examples/ for patterns)
hew init myproject

# Build a standalone binary
hew build hello.hew

# Check for errors without running
hew check hello.hew
```

The `--` separator is mandatory when passing program arguments — everything before `--` is parsed as `hew run` options, and everything after is forwarded to your program as `os.args()`. Without `--`, unrecognised flags produce a usage error.

**Working inside the Hew source checkout?** The stdlib resolver will find both the checkout's `std/` and the compiler's own stdlib and reject the ambiguity. Move your project to a directory outside the checkout, or use a standalone `hew init` directory.

## Core idioms

- Primitive types are lowercase: `i64`, `string`, `f64`, `bool`, `char` — never `Int`, `String`, `Float`.
- Integer literals default to `i64`, float literals to `f64`. Annotate only for narrower widths.
- Interpolate strings with the `f` prefix: `f"x={x}"`. A plain `"{x}"` prints literal braces.
- Convert numbers with `as`: `x as i64`, `pi as i32`. It is the only conversion mechanism.
- Never mix integer widths in one expression; cast the narrower operand up first: `x as i64 + 1`.
- **`var` for mutable bindings, `let` for immutable.** Hew does not have `let mut` — use `var` whenever you need to reassign a name or mutate a scalar field. `let` bindings are immutable (but `let` collections have interior mutability).
- Dispatch with `match`, not if/else chains — `match` on a closed enum enforces exhaustiveness.
- Iterate counts with `for i in 0..n` (exclusive) or `0..=n` (inclusive); the binding must be a named identifier.
- Read collection elements with `v[i]` (returns `T`, traps on out-of-bounds) or `.get(i)` (returns `Option<T>`, never traps) — both universal across element types.
- `Vec<string>` supports `v[i]` (returns a fresh owned `string`; the Vec stays usable), `.get(i)`, range-slices, and for-in. Both accessors work for `Vec<enum>` too.
- Build maps/sets with `::new()` + `.insert()`; bind with `let` (interior mutability, not reassignment).
- Look up `HashMap`/`Option`/`Result` with `match`, not subscript or `.unwrap()`.
- **Enum variants use `;` separators; record fields also use `;` in type definitions but `,` in construction literals.** These are different — `type T { a: i64; b: i64; }` defines, `T { a: 1, b: 2 }` constructs.
- Declare records with `type Name { field: T; }` (semicolons); enum variants are `;`-separated.
- Access actor state by bare field name inside handlers — no `self.` or `this.`.
- Fire-and-forget actor sends have no return type and no `await`: `ref.method(arg);`.
- Ask (request-reply) is `await ref.method(arg)` and returns `Result<R, AskError>` — match `Ok`/`Err`.
- Passing a value into an actor consumes it; duplicate it first with `clone x` (canonical) if you keep using it afterward.
- Within a fn or actor there is no borrow checker: pass values freely, mutations to Vec/HashMap persist in the caller.
- Last expression of a block (no trailing semicolon) is its value; a trailing `;` makes it unit.
- Lean on the safe stdlib trio: `std::string`, `std::math`, `std::iter`. Do not import `std::option`/`std::result`.
- Negate a bool with `!`: `!x` is `true` when `x` is `false`.
- `break` and `continue` work in both `loop {}` and `while` loops.
- Iterate all HashMap keys with `.keys()` → `Vec<string>`; all values with `.values()` → `Vec<V>`.

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

Always prefix interpolated strings with `f`. Arbitrary expressions are allowed inside `{}`. A plain string does not interpolate.

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

| Source value | Signed result | Unsigned result |
| --- | --- | --- |
| In-range finite | Truncated toward 0 | Truncated toward 0 |
| `+Inf` or greater than the target maximum | Target maximum | Target maximum |
| `-Inf` or less than the target minimum | Target minimum | `0` |
| `NaN` | `0` | `0` |

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
        Some(v) => println(v),
        None => println("overflow"),   // overflow
    }

    let clamped: i32 = a.saturating_add(1);
    println(clamped);          // 2147483647 (clamped to i32::MAX)
}
```

Every integer type (`i8`…`i64`, `isize`, `u8`…`u64`, `usize`) has three
`add`/`sub`/`mul` method families for spelling overflow intent explicitly.
`.wrapping_add()`/`.wrapping_sub()`/`.wrapping_mul()` wrap on overflow — the
same behaviour §12.2 already documents for the `&+`/`&-`/`&*` operators,
spelled as a method rather than an operator. `.checked_add()`/`.checked_sub()`/
`.checked_mul()` return `Option<T>` (`None` on overflow, `Some(v)`
otherwise). `.saturating_add()`/`.saturating_sub()`/`.saturating_mul()` clamp
to the type's max/min instead of wrapping or failing.

Only `add`/`sub`/`mul` are implemented today — no `div`/`rem`/shift variants
exist yet, and none of the three families apply to `f32`/`f64`. These
methods are an explicit-intent spelling, not a different default: the plain
`+`/`-`/`*` operators already wrap on overflow per §12.2.

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

> **Coming from Rust?** Hew does not have `let mut`. Use `var` for any binding you will reassign; use `let` for everything else. Writing `let mut x = 0` is a compile error — the parser rejects `mut` as unexpected.

Use `var` when the name will be reassigned or when a scalar field on a `var`-bound record will be mutated. `let` collections (`Vec`, `HashMap`, `HashSet`) have interior mutability — `.push()`/`.insert()` work on a `let` binding because you are not reassigning the binding itself. Use `var` only when you need to rebind the name to a new value.

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
    let items: Vec<string> = Vec::new();
    items.push("a");
    items.push("b");
    for s in items { total += s.len() as i64; }
    println(total);   // 2
}
```

Annotate a `var` (or `let`) binding when the type cannot be inferred from the initial value (e.g. an empty `Vec::new()`). Use `var` only when the name itself will be rebound; `let` is correct for a collection you only mutate via methods — the compiler warns if you `var`-declare a binding that is never rebound.

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
enum Event { Number(i64); Text(string); Empty; }
fn describe(e: Event) -> string {
    match e {
        Number(n) if n > 100 => "big number",
        Number(n) => "number",
        Text(s) => s,
        Empty => "empty",
    }
}
```

Variant arms bind their payload positionally. Construct values with bare variant names (`Number(5)`, `Empty`). Enum variants are declared `;`-separated.

### match exhaustiveness is enforced

```hew
enum Colour { Red; Green; Blue; }
// match c { Red => 1, Green => 2 }  // compile error: non-exhaustive match: missing Blue
fn code(c: Colour) -> i64 { match c { Red => 1, Green => 2, Blue => 3 } }
```

Omit `_` when matching a closed enum so the compiler forces every variant. A missing variant is a hard error naming it.

### full-field struct pattern

```hew
type Point { x: i64; y: i64; }
fn sum(p: Point) -> i64 {
    match p {
        Point { x, y } => x + y,
    }
}
```

Destructure a struct by naming every field. There is no `{ .. }` rest pattern; bind fields you do not need to a throwaway name.

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
    var v: Vec<i64> = Vec::new();
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
    let some: Option<i64> = Some(42);
    if let Some(v) = some { println(v); } else { println(-1); }
}
```

Use `if let Some(v) = opt` for a one-armed destructure; prefer `match` when you want the unwrapped-or-default as an expression result.

## Collections — Vec

### Construct, populate, measure

```hew
fn main() {
    let v: Vec<i64> = Vec::new();
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
type Point { x: i64; y: i64; }
fn main() {
    let v: Vec<Point> = Vec::new();
    v.push(Point { x: 1, y: 2 });
    v.push(Point { x: 3, y: 4 });
    let p = v[1];
    println(p.x);  // 3
}
```

```hew
fn main() {
    let names: Vec<string> = Vec::new();
    names.push("ada");
    names.push("alan");
    let who = names[1];        // fresh owned string (a clone)
    println(who);              // alan
    println(names[0]);         // ada — names is still usable
}
```

`v[i]` returns the element value `T` directly and works for every element type
— scalars, strings, records, tuples, and enums. It **traps** (aborts) on an
out-of-bounds index, so reach for it when the index is known valid (e.g. `i <
v.len()` already holds). A `Vec<string>` index returns a fresh owned `string`
(an element clone, not a move-out), so the Vec stays fully usable afterward.

### .get(i) — safe Option accessor

```hew
fn main() {
    let v: Vec<string> = Vec::new();
    v.push("a");
    v.push("b");
    match v.get(0) {
        Some(s) => println(s),   // a
        None => println("oob"),
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
    let v: Vec<i64> = Vec::new();
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
    let v: Vec<string> = Vec::new();
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
enum Colour { Red; Green; Blue; }
fn main() {
    let v: Vec<Colour> = Vec::new();
    v.push(Colour::Red);
    v.push(Colour::Blue);
    let c = v[1];
    match c {
        Colour::Red => println("red"),
        Colour::Green => println("green"),
        Colour::Blue => println("blue"),
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
    let v: Vec<i64> = Vec::new();
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
    let v: Vec<i64> = Vec::new();
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
    let v: Vec<i64> = Vec::new();
    v.push(1); v.push(2); v.push(3);
    println(v.contains(2));   // true
    println(v.contains(9));   // false

    let v2: Vec<i64> = Vec::new();
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
    let matrix: Vec<Vec<i64>> = Vec::new();

    let row0: Vec<i64> = Vec::new();
    row0.push(1); row0.push(2); row0.push(3);

    let row1: Vec<i64> = Vec::new();
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
    let m: HashMap<string, i64> = HashMap::new();
    m.insert("alice", 10);
    m.insert("bob", 20);
    println(m.len());   // 2
}
```

Use `let` (not `var`) — these have interior mutability so the binding is never reassigned. The annotation is required for inference. `.insert` returns unit and overwrites on duplicate key.

### Look up a key (returns Option)

```hew
fn main() {
    let m: HashMap<string, i64> = HashMap::new();
    m.insert("alice", 10);
    match m.get("alice") {
        Some(v) => println(f"alice={v}"),
        None => println("alice missing"),
    }
}
```

`m.get(k)` returns `Option<V>`; consume it with `match`. Maps have no subscript — always use `.get`.

### Membership, remove, length

```hew
fn main() {
    let m: HashMap<string, i64> = HashMap::new();
    m.insert("alice", 1);
    m.insert("bob", 2);
    let has_alice = m.contains_key("alice");   // true
    let removed = m.remove("bob");             // true if present
    println(f"has={has_alice} removed={removed} len={m.len()}");
}
```

`.contains_key(k)` and `.remove(k)` both return `bool`. Bind a method result to a `let` before interpolating — nested double-quotes break the f-string parser. Test emptiness with `m.len() == 0`.

### Supported HashMap value types

```hew
type User { name: string; score: i64; }

fn main() {
    // Scalar values
    let flags: HashMap<string, bool> = HashMap::new();
    flags.insert("debug", true);
    let ratios: HashMap<string, f64> = HashMap::new();
    ratios.insert("pi", 3.14);

    // User-defined records work as values
    let users: HashMap<string, User> = HashMap::new();
    users.insert("alice", User { name: "Alice", score: 100 });

    // Vec<T> also works as a value
    let tags: HashMap<string, Vec<string>> = HashMap::new();
    let v: Vec<string> = Vec::new();
    v.push("admin");
    tags.insert("alice", v);
}
```

Keys are `string`. Value types include `i64`, `string`, `bool`, `f64`, user-defined records, and `Vec<T>`.

### Mutating a HashMap value (copy-rebuild-reinsert)

`HashMap` has no `.get_mut()`. The idiom for updating an existing value is to read it, compute the new value, and reinsert:

```hew
fn main() {
    let scores: HashMap<string, i64> = HashMap::new();
    scores.insert("alice", 10);
    scores.insert("bob", 20);

    // Increment alice's score
    match scores.get("alice") {
        Some(v) => scores.insert("alice", v + 5),
        None => {},
    }

    match scores.get("alice") {
        Some(v) => println(f"alice={v}"),   // alice=15
        None => println("missing"),
    }
}
```

`.get(k)` returns an owned copy of the value (not a reference). Modify the copy and reinsert with `.insert(k, new_value)`. This is the only mutation path — there is no `.get_mut()` or entry API.

### Iterating a HashMap — .keys() and .values()

```hew
fn main() {
    let m: HashMap<string, i64> = HashMap::new();
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
    let s: HashSet<i64> = HashSet::new();
    s.insert(1);
    s.insert(2);
    s.insert(2);                 // dedups
    let n = s.len();             // 2
    let has1 = s.contains(1);    // true
    let removed = s.remove(1);   // true
    println(f"n={n} has1={has1} removed={removed}");
}
```

Set membership is `.contains(x)` (note: `.contains_key` is the HashMap spelling). Inserts dedup automatically. Supported element types are `i64` and `string`.

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

The return value of `main() -> i32` (or `main() -> i64`) becomes the process exit code. A unit `main()` always exits 0 — even if it prints an error message. Use `exit(code)` to exit with a specific code from inside a unit `main()`.

```hew
// Pattern 1: return the code directly
fn main() -> i32 {
    println("all good");
    0
}
```

```hew
import std::os;

// Pattern 2: check args, exit non-zero on error
fn main() -> i32 {
    if os.args_count() < 2 {
        println("usage: prog <arg>");
        return 1;
    }
    println(f"arg: {os.args(1)}");
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

| Declaration | How to exit non-zero |
|-------------|----------------------|
| `fn main() -> i32` | return the code as the last expression or with `return N;` |
| `fn main() -> i64` | same — return the code value |
| `fn main()` (unit) | call `exit(N)` explicitly; the function itself can only exit 0 |

Shell pipelines and `&&` chains read the exit code — write `main() -> i32` for any program that signals failure to the caller. `assert(false)`, `panic(...)`, and traps (div-by-zero) all exit non-zero via the runtime trap handler and do not need an explicit return.

### Mutation through a parameter persists

```hew
fn fill(v: Vec<i64>) { v.push(1); v.push(2); v.push(3); }
fn main() {
    let xs: Vec<i64> = Vec::new();
    fill(xs);
    println(xs.len());   // 3
}
```

To have a helper build a collection, pass it and mutate in place — the caller observes the result. This is reference semantics; `let`/`var` only controls rebinding the name, not interior mutation.

### Reuse a passed value without clone (intra-actor)

```hew
fn total(v: Vec<i64>) -> i64 {
    var sum: i64 = 0;
    let n = v.len();
    for i in 0 .. n { sum = sum + v[i]; }
    sum
}
fn main() {
    let xs: Vec<i64> = Vec::new();
    xs.push(10); xs.push(20);
    println(total(xs));
    println(total(xs));   // still valid; no move within a fn/actor
}
```

Inside a single actor and in free functions, ordinary function calls do not move
their arguments, so you can keep using those values afterward. Ownership-sink
operations still move managed values: for example, `HashSet.insert(x)` and
`HashMap.insert(k, v)` take ownership of managed string keys/elements/values. If
you need to keep using the original after such an insert, pass `clone x` (or
`x.clone()`) into the collection.

### .clone() produces an independent copy

```hew
fn main() {
    let a: Vec<i64> = Vec::new();
    a.push(1); a.push(2);
    let b = a.clone();
    b.push(99);
    println(a.len());   // 2
    println(b.len());   // 3
}
```

Use `.clone()` only when you need a second independent copy — e.g. before sending one to an actor and keeping one.

### clone x — the canonical duplication prefix

```hew
fn main() {
    let a: Vec<i64> = Vec::new();
    a.push(1); a.push(2);
    let b = clone a;    // independent copy — same effect as `a.clone()`
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
clones the result of `x.f()`, and `clone a + b` is `(clone a) + b`. There is no
`&x` borrow expression in Hew — `&` is bitwise-and and the type-level borrow
marker (`&T`) only; writing `&x` is rejected with a diagnostic pointing you at
`clone x`.

### Struct value param and returning a struct

```hew
type Point { x: i64; y: i64; }
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

For transformations, construct and return a fresh struct literal. Struct fields are immutable by default — produce a new struct rather than mutating.

### Move-on-send: passing into an actor consumes it

```hew
actor Sink { let id: i64; receive fn take(data: string) -> i64 { data.len() } }
fn main() {
    let s = spawn Sink(id: 0);
    let msg: string = "hello";
    let n = await s.take(clone msg);   // send a copy to keep msg
    match n { Ok(len) => println(len), Err(_) => println("ask failed") }
    println(msg.len());   // 5, msg still valid
}
```

Passing a value into an actor's `receive fn` consumes it; reusing it after is a hard `use of moved value` error. Duplicate it first with `clone x` (or the equivalent `x.clone()`) to keep the original. `await actor.method(...)` on a request-reply fn returns `Result<R, AskError>` — match it.

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
type Point { x: i64; y: i64; }
fn main() {
    let p = Point { x: 3, y: 4 };
    println(p.x);
    println(p.y);
}
```

Struct fields use bare `name: T;` with semicolon terminators and no `let`/`var` prefix (that prefix is for actor fields only). Fields are immutable under `let`.

### Mutable struct via var binding

```hew
type Point { x: i64; y: i64; }
fn main() {
    var p = Point { x: 1, y: 2 };
    p.x = 10;
    println(p.x);   // 10
}
```

Bind with `var` to reassign fields; `let` is immutable. Immutability is on the binding, not the type.

### Nested struct fields

```hew
type Point { x: i64; y: i64; }
type Line { start: Point; end: Point; }
fn main() {
    let l = Line { start: Point { x: 0, y: 0 }, end: Point { x: 3, y: 4 } };
    println(l.start.x);
    println(l.end.y);
}
```

Compose records by nesting; access depth-chains directly. Every field must be supplied — there is no partial/default fill.

### Separator syntax: `;` in definitions, `,` in construction

> **Syntax callout — this trips up almost everyone:**
>
> | Context | Separator |
> |---------|-----------|
> | `type` field definitions | `;` (semicolon) — idiomatic; `,` also accepted |
> | `enum` variant separators | `;` (semicolon) — required; `,` is an error |
> | Record construction literals | `,` (comma) — required; `;` is an error |
>
> ```hew
> // Definition — semicolons throughout (idiomatic)
> type Point { x: i64; y: i64; }
> enum Color { Red; Green; Blue; }
>
> // Construction — commas (required)
> let p = Point { x: 1, y: 2 };
> let c = Red;
> ```
>
> The compiler error for a semicolon in a construction literal is "expected `}`, found `;`". Enum variants reject commas with "use `;` instead of `,` to separate variants". Type field definitions accept both separators, but `;` is the idiomatic style.

### Enum with unit, tuple, and struct variants

```hew
enum Shape {
    Empty;
    Circle(f64);
    Rect { w: f64; h: f64 }
}
fn area(s: Shape) -> f64 {
    match s {
        Empty => 0.0,
        Circle(r) => 3.14159 * r * r,
        Rect { w, h } => w * h,
    }
}
fn main() {
    println(area(Circle(2.0)));
    println(area(Rect { w: 3.0, h: 4.0 }));
}
```

Mix unit, tuple, and struct variants in one enum. Enum variants are separated by `;`. Struct-variant fields use `;` separators; the variant pattern uses `{ w, h }` shorthand. Construct variants by bare name.

### Pattern destructuring in match

```hew
enum Cmd {
    Move(i64, i64);
    Stop;
    Speak { text: string }
}
fn describe(c: Cmd) -> string {
    match c {
        Move(0, 0) => "noop",
        Move(x, _) => "move",
        Stop => "stop",
        Speak { text } => text,
    }
}
```

Combine literal patterns for special cases above general binding patterns — order matters, specific arms first. Wildcard `_` ignores a payload slot.

### Qualified variant construction

```hew
indirect enum Expr { Lit(i64); Add(Expr, Expr); }
fn eval(e: Expr) -> i64 {
    match e {
        Lit(n) => n,
        Add(l, r) => eval(l) + eval(r),
    }
}
fn main() {
    let e = Expr::Add(Expr::Lit(10), Expr::Lit(5));
    println(eval(e));   // 15
}
```

Bare variant names are the common idiom; use `EnumName::Variant` to disambiguate across modules. Match arm patterns use the bare variant name even when construction used the qualified form.

### Self-referential recursive enum (indirect)

```hew
indirect enum Expr {
    Lit(i64);
    Add(Expr, Expr);
    Neg(Expr);
}
fn eval(e: Expr) -> i64 {
    match e {
        Lit(n) => n,
        Add(l, r) => eval(l) + eval(r),
        Neg(inner) => 0 - eval(inner),
    }
}
fn main() {
    let e = Add(Lit(1), Neg(Lit(2)));
    println(eval(e));   // -1
}
```

Prefix the enum keyword with `indirect` for self-referential variants (AST/tree types). It applies to the whole enum and heap-allocates behind a pointer. Construct and match exactly like a regular enum. (Use it for local values and function args.)

### User-defined generic enum

```hew
enum MyOpt<T> {
    Has(T);
    Empty;
}
fn main() {
    let a: MyOpt<i64> = Has(42);
    match a {
        Has(v) => println(v),
        Empty => println(-1),
    }
}
```

Parameterize an enum with `<T>` for container-like sum types; annotate the binding so the type argument is fixed.

### Struct or enum as a receive fn message parameter

```hew
type Record { key: i64; val: i64; }
actor Sink {
    var last: i64;
    init() { last = 0; }
    receive fn put(r: Record) { last = r.val; }
    receive fn get() -> i64 { last }
}
fn main() {
    let s = spawn Sink();
    s.put(Record { key: 1, val: 99 });
    let r = await s.get();
    match r { Ok(v) => println(v), Err(_) => println("err") }
}
```

Structs and enums cross the actor boundary as message payloads — pass them as receive-fn parameters. Keep the actor's own state fields scalar and feed structured data in via messages. Access state by bare field name.

### Block-bodied match arms

```hew
enum Op { Inc(i64); Reset; }
actor Acc {
    var total: i64;
    init() { total = 0; }
    receive fn apply(op: Op) {
        match op {
            Inc(n) => {
                total = total + n;
            },
            Reset => {
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
    var balance: i64 = 0;
    receive fn deposit(amt: i64) { balance = balance + amt; }
    receive fn balance_of() -> i64 { balance }
}
fn main() {
    let acct = spawn Bank(balance: 100);
    acct.deposit(50);
    let r = await acct.balance_of();
    match r { Ok(v) => println(f"balance={v}"), Err(_) => println("ask failed") }
}
```

Use `var` for fields a handler mutates (give a default), `let` for fields set once at spawn. `spawn` must pass every field by name.

### Bare field access (read and write)

```hew
actor Counter {
    var count: i64 = 0;
    receive fn increment(n: i64) { count = count + n; }
    receive fn total() -> i64 { count }
}
```

Reference and assign state fields by bare name — no `self`/`this` prefix. State persists across invocations. Keep handler param names distinct from field names (shadowing is an error).

### spawn returns LocalPid<ActorType>

```hew
actor Greeter {
    let name: i64;
    receive fn greet() -> i64 { name }
}
fn main() {
    let g: LocalPid<Greeter> = spawn Greeter(name: 5);
    let r = await g.greet();
    match r { Ok(v) => println(f"name={v}"), Err(_) => println("ask failed") }
}
```

The handle type is `LocalPid<T>` (the actor type itself). Let it infer, or annotate when storing the handle in a field. Each `receive fn` takes zero or one argument — bundle multiple values into a struct.

### Fire-and-forget send

```hew
actor Logger {
    var n: i64 = 0;
    receive fn log(msg: i64) { println(f"log: {msg}"); n = n + 1; }
    receive fn ping() { println("pong"); }
}
fn main() {
    let lg = spawn Logger(n: 0);
    lg.log(7);    // no await; type ()
    lg.ping();
}
```

Call a return-less `receive fn` directly with no `await` — the checker derives fire-and-forget from the absent return type. The call has type `()`; do not bind or interpolate it.

### Ask / request-reply via await

```hew
actor Counter {
    var count: i64 = 0;
    receive fn increment(n: i64) { count = count + n; }
    receive fn total() -> i64 { count }
}
fn main() {
    let c = spawn Counter(count: 0);
    c.increment(10); c.increment(20); c.increment(12);
    let r = await c.total();
    match r { Ok(v) => println(f"total={v}"), Err(_) => println("ask failed") }
}
```

Write request-reply as `await ref.method(args)` and match `Ok`/`Err`. The reply value is the trailing expression of the `receive fn`. Ask always yields `Result<R, AskError>`, never bare `R`.

### await position rules

`await` can appear in three contexts:

**1. Statement position in any fn or receive fn** — the most common case, and it always works:

```hew
actor Src { receive fn val() -> i64 { 42 } }
actor Consumer {
    var src: LocalPid<Src>;
    receive fn run(unused: i64) -> i64 {
        // await as a statement, inside a receive fn — always valid
        let r = await src.val();
        match r { Ok(v) => v, Err(_) => -1 }
    }
}
fn main() {
    let s = spawn Src();
    let c = spawn Consumer(src: s);
    let r = await c.run(0);
    match r { Ok(v) => println(f"v={v}"), Err(_) => println("err") }
}
```

**2. `scope{}` body for concurrent tasks** — use `scope{}` with `fork` for structured concurrency. `await` suspensions inside a `scope{}` do not block its sibling forks:

<!-- doctest: skip -->
```hew
scope {
    fork { result_a = work_a(); };
    fork { result_b = work_b(); };
}
// Both forks have joined here
```

**3. `await` as a value in non-statement positions is rejected** — `await` cannot be used as a function argument, binary operand, or let-binding right-hand side nested inside a larger expression. Bind the awaited result to a `let` first:

<!-- doctest: skip -->
```hew
// Wrong: await as function argument
// println(await actor.method());   // compile error

// Right: bind first
let r = await actor.method();
match r { Ok(v) => println(v), Err(_) => println("err") }
```

Note that `.send()` is accepted as an actor method name and compiles correctly — there is no compiler-level restriction on it.

### Ask try-sugar in a Result-returning fn

```hew
actor Counter {
    var count: i64 = 0;
    receive fn bump() -> i64 { count = count + 1; count }
}
fn run() -> Result<i64, AskError> {
    let c = spawn Counter(count: 0);
    let v? = await c.bump();
    let w? = await c.bump();
    Ok(v + w)
}
fn main() { match run() { Ok(t) => println(f"total={t}"), Err(_) => println("failed") } }
```

Inside a fn returning `Result<_, AskError>`, use `let v? = await ...` to unwrap the Ok and auto-propagate Err. The `?` goes on the binding, not the expression.

### Lifecycle hooks #[on(start)] and #[on(stop)]

```hew
actor Boot {
    var ready: i64 = 0;
    #[on(start)] fn boot() { ready = 99; println("started"); }
    #[on(stop)] fn done() { println("stopped"); }
    receive fn status() -> i64 { ready }
}
fn main() {
    let b = spawn Boot(ready: 0);
    let r = await b.status();
    match r { Ok(v) => println(f"ready={v}"), Err(_) => println("ask failed") }
}
```

Put post-spawn initialization in `#[on(start)]` and teardown in `#[on(stop)]`. Both take no params; fields are in scope by bare name. They are plain fns, not receive fns. `#[on(start)]` appears at most once; `#[on(stop)]` may repeat.

### #[on(crash)] hook

```hew
actor Risky {
    var n: i64 = 0;
    #[on(start)] fn boot() { n = 1; }
    #[on(crash)] fn on_fail(info: CrashInfo) -> CrashAction { panic("crash observed") }
    receive fn value() -> i64 { n }
}
fn main() {
    let r = spawn Risky(n: 0);
    let v = await r.value();
    match v { Ok(x) => println(f"n={x}"), Err(_) => println("failed") }
}
```

Declare `#[on(crash)]` as `fn name(info: CrashInfo) -> CrashAction` and satisfy the return type with a diverging `panic(...)` body. The body is observation/logging only; supervisor restart policy drives behaviour.

### Free functions callable from receive fns

```hew
fn double(x: i64) -> i64 { x * 2 }
actor Calc {
    var acc: i64 = 0;
    receive fn apply(n: i64) -> i64 { acc = acc + double(n); acc }
}
fn main() {
    let calc = spawn Calc(acc: 0);
    let r = await calc.apply(5);
    match r { Ok(v) => println(f"acc={v}"), Err(_) => println("ask failed") }
}
```

Put shared/helper logic in top-level free functions and call them from receive fns. (An actor's own non-receive `fn` method is not callable — extract helpers to free functions.)

### Actor-to-actor messaging

```hew
actor Worker {
    let id: i64;
    receive fn work(n: i64) -> i64 { n * id }
}
actor Manager {
    var worker: LocalPid<Worker>;
    receive fn dispatch(n: i64) -> i64 {
        let r = await worker.work(n);
        match r { Ok(v) => v, Err(_) => -1 }
    }
}
fn main() {
    let w = spawn Worker(id: 3);
    let m = spawn Manager(worker: w);
    let r = await m.dispatch(7);
    match r { Ok(v) => println(f"result={v}"), Err(_) => println("failed") }
}
```

Spawn the dependency first, pass its `LocalPid<Dep>` into the dependent actor's spawn, store it in a field, and `await dep.method(...)` from a handler. Awaiting another actor yields `Result<R, AskError>` like any ask.

### Timers and scheduling — sleep and sleep_until

Hew provides two blocking/suspending timer builtins backed by a single
hierarchical timer wheel. The wheel is tickless — the scheduler thread parks
until the next deadline and never spins when no timers are armed.

**`sleep(d: duration)`** — suspend for a duration. Inside an actor handler
the actor suspends cooperatively and the worker is freed for other actors;
in `fn main` or a free function the calling thread blocks.

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
    t.run(3);
    sleep(100ms);
    // tick 0
    // tick 1
    // tick 2
}
```

**`sleep_until(target: instant)`** — suspend until a monotonic `instant`.
Returns immediately if `target` is already in the past. Combine with
`instant::now()` and duration arithmetic to build deadline-bounded loops:

```hew
fn main() {
    let t0 = instant::now();
    let deadline = t0 + 50ms;
    // ... do some work ...
    sleep_until(deadline);    // waits only the remaining time
    let elapsed = t0.elapsed();
    println(elapsed.millis() >= 45);   // true
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
    var count: i64 = 0;

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
    let r = await p.total();
    match r {
        Ok(n) => println(f"ticks={n}"),
        Err(_) => println("ask failed"),
    }
}
```

Periodic handlers preserve the actor's single-threaded state model: a tick never
runs concurrently with another receive handler on the same actor. It is just a
separate mailbox dispatch armed by the runtime timer.

### Cancellable long-running work in actor handlers

A receive handler runs to completion before the actor observes the next message.
If a handler writes `while running { sleep(...) }` and expects another receive
handler to set `running = false`, the stop message cannot be dispatched until
the loop exits. Use a periodic receive handler and a flag instead:

```hew
actor Worker {
    var running: bool = true;
    var ticks: i64 = 0;

    #[every(25ms)]
    receive fn tick() {
        if !running {
            return;
        }
        ticks = ticks + 1;
    }

    receive fn stop() {
        running = false;
    }

    receive fn total() -> i64 {
        ticks
    }
}

fn main() {
    let worker = spawn Worker(running: true, ticks: 0);
    sleep(150ms);
    worker.stop();
    sleep(150ms);
    let r = await worker.total();
    match r {
        Ok(n) => println(f"ticks={n}"),
        Err(_) => println("ask failed"),
    }
}
```

Here `receive fn stop()` is a user-defined actor message. It is unrelated to the
`#[on(stop)]` lifecycle hook, which is a plain `fn` invoked when the actor is
tearing down. The `sleep_loop_blocks_mailbox` lint warns on the mailbox
starvation shape where a receive handler loops around `sleep`/`sleep_until`
without an in-loop exit path.

## State machines

### Machine declaration: events, states, transitions, step(), state_name()

```hew
machine Counter {
    events { Inc; Reset; }
    state Zero;
    state NonZero { value: i64; }
    on Inc: Zero => NonZero { NonZero { value: 1 } }
    on Inc: NonZero => NonZero reenter { NonZero { value: self.value + 1 } }
    on Reset: NonZero => Zero { Zero }
    default { state }
}
fn main() {
    var c = Zero;
    c.step(Inc); c.step(Inc); c.step(Inc);
    println(c.state_name());            // NonZero
    match c {
        Zero => println("is zero"),
        NonZero { value } => println(f"value={value}"),  // value=3
    }
    c.step(Reset);
    println(c.state_name());            // Zero
}
```

A machine is a value type. Inside the body use bare names (`NonZero { value: 1 }`, `step(Inc)`); outside use the qualifier (`Counter::Zero`, `CounterEvent::Inc`). `step()` mutates in place — the receiver must be a `var`. End with `default { state }` to make uncovered cells a no-op stay. Every (state, event) cell must be covered or it is a compile error.

### State field holding a Vec

```hew
machine Log {
    events { Append { item: i64; } Clear; }
    state Empty;
    state Filled { items: Vec<i64>; }
    on Append(item): Empty => Filled {
        let v: Vec<i64> = Vec::new(); v.push(item); Filled { items: v }
    }
    on Append(item): Filled => Filled reenter {
        let v = self.items; v.push(item); Filled { items: v }
    }
    on Clear: Filled => Empty { Empty }
    default { state }
}
fn main() {
    var log = Empty;
    log.step(Append { item: 10 });
    log.step(Append { item: 20 });
    match log {
        Empty => println("empty"),
        Filled { items } => {
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
    events { Add { n: i64; } }
    state Seed;
    state Total { sum: i64; }
    on Add: Seed => Total { Total { sum: event.n } }
    on Add: Total => Total reenter { Total { sum: self.sum + event.n } }
}
fn make_add(n: i64) -> AccEvent { AccEvent::Add { n: n } }
fn main() {
    var a = Seed;
    a.step(make_add(5));
    a.step(make_add(7));
    match a {
        Seed => println("seed"),
        Total { sum } => println(f"sum={sum}"),   // sum=12
    }
}
```

Prefer the head binding `on Add(n): ...` so payload names are declared at the rule site; `event.n` is the equivalent fallback. `self.field` reads the source state; `event.field` reads the event payload. The compiler generates a companion enum `{MachineName}Event` you can name in signatures and construct with `MachineEvent::Variant`.

### Wildcard transitions and if-expression bodies

```hew
machine Conn {
    events { Start; Bump; Kill; }
    state Idle;
    state Live { hits: i64; }
    state Dead;
    on Start: Idle => Live { Live { hits: 0 } }
    on Bump: Live => Live reenter {
        if self.hits + 1 >= 3 { Dead } else { Live { hits: self.hits + 1 } }
    }
    on Kill: _ => Dead { Dead }
    on Start: _ => _ { state }
    on Bump: _ => _ { state }
}
fn main() {
    var c = Idle;
    c.step(Start); c.step(Bump); c.step(Bump); c.step(Bump);
    println(c.state_name());   // Dead
}
```

`on E: _ => _ { state }` is the canonical "ignore this event everywhere it isn't explicitly handled". A wildcard-target body may return any variant, so an `if` returning different states is legal there and in a `reenter` body. An explicit cell rule always wins over a wildcard.

### Driving a machine through a free-function parameter

```hew
machine Door {
    events { Open; Close; }
    state Shut;
    state Ajar { angle: i64; }
    on Open: Shut => Ajar { Ajar { angle: 90 } }
    on Close: Ajar => Shut { Shut }
    default { state }
}
fn drive(d: Door) -> string {
    var local = d;
    local.step(Open);
    local.state_name()
}
fn main() { println(drive(Door::Shut)); }   // Ajar
```

Pass machines by value into and out of free functions and mutate a local `var`. (Drive machines via local `var`, free-fn params, or the whole actor-message payload — not as an embedded named field of an actor or struct.)

## Generics

### Generic function with a trait bound

```hew
trait Named { fn name(val: Self) -> string; }
type User { name: string; }
impl Named for User { fn name(u: User) -> string { u.name } }
fn announce<T: Named>(item: T) { println(item.name()); }
fn main() { announce(User { name: "Bob" }); }   // Bob
```

Declare the impl as `impl Trait for Type`, bound the parameter `<T: Trait>`, and call the method on the param. Monomorphized per concrete type. A bare `impl Type { ... }` does not satisfy the bound — always write `impl Trait for Type`.

### Multi-bound and where-clause functions

```hew
trait HasName { fn name(val: Self) -> string; }
trait HasScore { fn score(val: Self) -> i64; }
type Player { name: string; score: i64; }
impl HasName for Player { fn name(p: Player) -> string { p.name } }
impl HasScore for Player { fn score(p: Player) -> i64 { p.score } }
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
type Pair<A, B> { first: A; second: B; }
fn fst<A, B>(p: Pair<A, B>) -> A { p.first }
fn main() { let p = Pair { first: 100, second: 2.5 }; println(fst(p)); }   // 100
```

Read fields of a generic record inside a generic free function — prefer a free `fn fst<A,B>(p: Pair<A,B>)` over a generic impl method.

### Generic record type with all-bitcopy fields

```hew
type Pair<A, B> { first: A; second: B; }
fn main() {
    let p = Pair { first: 10, second: 3.5 };
    println(p.first);
    println(p.second);
}
```

Use generic records as lightweight bitcopy containers over scalar types (`i64`, `f64`, `bool`). The fields may also be owned — see the next example.

### Generic record type with an owned field

```hew
type Pair<A, B> { first: A; second: B; }
fn make() -> Pair<i64, string> { Pair { first: 1, second: "owned" } }
fn main() {
    let p = make();
    println(p.first);
    println(p.second);
}
```

A generic record instantiation may carry owned fields (`string`, `Vec<T>`, nested records). Each instantiation (`Pair<i64, string>`, `Pair<string, Vec<i64>>`) drops and clones its owned fields per concrete instantiation — pass it by value, return it, store it. A field whose substituted type has no clone helper (an `#[opaque]` handle) fails closed at compile time, so write a concrete record for those.

### Generic function over Vec<T>

```hew
fn count<T>(items: Vec<T>) -> i64 { items.len() }
fn main() {
    let v: Vec<i64> = Vec::new();
    v.push(10); v.push(20); v.push(30);
    println(count(v));   // 3
}
```

Accept `Vec<T>` in a generic function and use `.len()`/`v[i]`.

### Vec of a concrete enum (including string payload)

```hew
enum Shape { Circle(f64); Named(string); }
fn main() {
    let v: Vec<Shape> = Vec::new();
    v.push(Shape::Circle(1.5));
    v.push(Shape::Named("square"));
    println(v.len());   // 2
}
```

A monomorphic enum, even one carrying a string payload, is a valid Vec element. Construct variants as `Enum::Variant(payload)`. Generic type parameters (`Vec<T>`) are also supported for element methods (`get`, `push`, `set`, `pop`, `contains`, indexing, and range-slice).

### Vec of a concrete record

```hew
type Point { x: i64; y: i64; }
fn main() {
    let v: Vec<Point> = Vec::new();
    v.push(Point { x: 1, y: 2 });
    v.push(Point { x: 3, y: 4 });
    let got = v[1];
    println(got.x);   // 3
    println(got.y);   // 4
}
```

Store concrete records in a Vec and bind the element with `let got = v[i]` before reading fields.

### Turbofish — explicit type argument on a call

When a generic function takes no arguments from which the type can be inferred, use the turbofish syntax `func::<T>()` to supply the type argument explicitly:

```hew
fn make_vec<T>() -> Vec<T> {
    Vec::new()
}
fn main() {
    let v = make_vec::<i64>();
    v.push(10);
    v.push(20);
    println(v.len());   // 2
}
```

> **Note:** Generic constructor functions called *without* turbofish fail — the error depends on context: if `T` is entirely unconstrained (no usage to narrow it), the type-checker reports `cannot infer type for local binding`; once usage constrains `T`, the call emits `E_NOT_YET_IMPLEMENTED: MIR lowering for function call is not implemented yet`. Annotating the binding without turbofish (`let s: Stack<i64> = new_empty()`) does not help — it still emits `E_NOT_YET_IMPLEMENTED`. Two workarounds:
>
> ```hew
> type Stack<T> { items: Vec<T>; }
>
> fn new_empty<T>() -> Stack<T> { Stack { items: Vec::new() } }
>
> fn main() {
>     // Option A — turbofish on the call (preferred when using a ctor fn)
>     let s = new_empty::<i64>();
>
>     // Option B — construct inline with a type annotation on the binding
>     let s2: Stack<i64> = Stack { items: Vec::new() };
> }
> ```
>
> Note: `Stack { items: Vec::new() }` *without* a type annotation on the binding also fails — with `cannot infer type for local binding` when `T` is unconstrained, or `E_MIR: unknown type 'T' at the MIR boundary` once usage constrains `T`. Always provide a type annotation when constructing a generic record inline.

### Generic functions as values (cross-module)

A generic function exported from another module can be passed as a first-class
value. The concrete type arguments are inferred from the receiving variable's
declared type or the parameter type at the call site — the context fully
determines the monomorphisation.

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

    // Generic cross-module function; type inferred from the annotation
    let id: fn(i64) -> i64 = math_utils.identity;
    println(apply(id, 5));                        // 5

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

The type annotation on the `let` binding (e.g. `fn(i64) -> i64`) is what
drives inference for `identity<T>` — without it the checker cannot determine
`T` and rejects the assignment with a diagnostic asking for an explicit
annotation. Monomorphic cross-module functions (`square`, `add_one`) do not
need an annotation; the type is recovered from the function's declared
signature directly.

> **Scope:** this path fires only for cross-module functions
> (`module.fn_name`). Capturing a generic function from the *current* module
> as a value is not supported — split the generic helper into a separate
> module if you need it as a value.

### Generic Display and println

`println` and `print` accept any type `T: Display`. For primitive types
(`i64`, `f64`, `bool`, `char`, `string`) the `Display` impl is built in and
`println(v)` works without further setup. For a user-defined type, implement
`Display` and call `println` through a generic wrapper or use f-string
interpolation:

```hew
type Celsius { degrees: f64; }

impl Display for Celsius {
    fn fmt(c: Celsius) -> string {
        f"{c.degrees}°C"
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

### Generics NYI — current limitations

Two patterns type-check but fail at call or check time:

1. **Generic constructor functions without turbofish** (`fn ctor<T>() -> MyType<T>` called without turbofish): call sites emit `E_NOT_YET_IMPLEMENTED: MIR lowering for function call is not implemented yet` (or `cannot infer type for local binding` when `T` is unconstrained). Annotating the binding without turbofish (`let x: MyType<i64> = ctor()`) does not resolve it — `E_NOT_YET_IMPLEMENTED` still fires. Fix: add turbofish (`ctor::<i64>()`) or skip the ctor fn and construct inline with an annotated binding (`let x: MyType<i64> = MyType { ... }`). See the Turbofish section above for examples.

2. **`impl Trait for GenericRecord<ConcreteType>`**: trait method body type-checking for multiple concrete instantiations of the same generic record has a known resolution bug where the later-declared impl's type substitution is used for all impls. Until this is fixed, implement trait methods for a concrete (non-generic) record type.

## Errors — Result and Option

### Result construction and matching

```hew
fn divide(a: i64, b: i64) -> Result<i64, string> {
    if b == 0 { Err("division by zero") } else { Ok(a / b) }
}
fn main() {
    match divide(10, 2) { Ok(v) => println(f"ok: {v}"), Err(e) => println(f"err: {e}") }
    match divide(1, 0) { Ok(v) => println(f"ok: {v}"), Err(e) => println(f"err: {e}") }
}
```

Construct with bare `Ok(v)`/`Err(e)`; consume with `match` covering both arms. The type is inferred from the function's declared return type.

### Option construction and matching

```hew
fn first_positive(a: i64) -> Option<i64> {
    if a > 0 { Some(a) } else { None }
}
fn main() {
    match first_positive(5) { Some(v) => println(f"some: {v}"), None => println("none") }
    let n: Option<i64> = None;
    match n { Some(v) => println(f"some: {v}"), None => println("none") }
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
    match chain(100, 5, 2) { Ok(v) => println(f"ok: {v}"), Err(e) => println(f"err: {e}") }
    match chain(100, 0, 2) { Ok(v) => println(f"ok: {v}"), Err(e) => println(f"err: {e}") }
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
    match first(Some(5)) { Some(v) => println(f"some: {v}"), None => println("none") }
    let n: Option<i64> = None;
    match first(n) { Some(v) => println(f"some: {v}"), None => println("none") }
}
```

Inside an Option-returning fn, `o?` yields the inner value on Some and early-returns None on None.

### Hand-rolled unwrap_or via match

```hew
fn unwrap_or(r: Result<i64, string>, fallback: i64) -> i64 {
    match r { Ok(v) => v, Err(_) => fallback }
}
fn main() {
    let ok: Result<i64, string> = Ok(42);
    let err: Result<i64, string> = Err("bad");
    println(unwrap_or(ok, 0));    // 42
    println(unwrap_or(err, -1));  // -1
}
```

For unwrap_or/is_ok/is_err on Result, write a tiny `match` helper inline. This is the reliable path — do not import `std::result`/`std::option`, and do not use `.unwrap()`/`.unwrap_or()` method form.

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
        Some(v) => if v > 0 { "positive" } else { "non-positive" },
        None => "missing",
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
    match validate(5)  { Ok(_) => println("valid"), Err(e) => println(f"err: {e}") }
    match validate(-1) { Ok(_) => println("valid"), Err(e) => println(f"err: {e}") }
}
```

For a fallible operation with no meaningful success value, return `Result<i64, E>` and `Ok(0)`; match the success arm with `Ok(_)`. (Avoid `Ok(())` / `Result<(), E>`.)

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

Read elements with `parts[i]` (or `.get(i)` for an `Option`) and iterate by index — the index read clones the element and leaves `parts` usable. (`for p in parts` also works but consumes the Vec, so use the indexed loop when you need `parts` again afterward.)

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

### .find / .index_of with -1 sentinel

```hew
fn main() {
    let idx = "hello world".find("world");
    println(f"find={idx}");    // 6
    let miss = "hello".find("xyz");
    if miss < 0 { println("not found"); }
}
```

`.find`/`.index_of` return `i64`, with `-1` as the not-found sentinel. Bind to a `let`, then guard with `< 0`.

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

### std::string module functions

```hew
import std::string;
fn main() {
    println(string.from_int(42));            // 42
    let n = string.to_int("42");
    println(f"n={n}");                        // 42
    println(string.pad_left("7", 3, "0"));   // 007
    println(string.join(["a", "b", "c"], ", "));  // a, b, c
    let c = string.count("abcabc", "abc");
    println(f"count={c}");                    // 2
}
```

Import `std::string` and call via the module name. `from_int`/`to_int`/`to_float` for conversions; `join(Vec<string>, sep)` for assembly; `pad_left`/`pad_right` for fixed width. `to_int`/`to_float` return `0`/`0.0` on parse failure — use `string.try_to_int`/`try_to_float` for a `Result` when you need to distinguish failure.

### Concatenation, char round-trip, escapes

```hew
import std::string;
fn main() {
    let g = "Hello" + ", " + "world";
    println(g);
    let code = "Z".char_at(0);
    println(string.from_char(code));   // Z
    var acc = "";
    for i in 0 .. 3 { acc = acc + "x"; }
    println(acc);                       // xxx
}
```

Build strings with `+`. `char_at` gives the code point (`i64`); `string.from_char` renders it back. To take the codepoint of a `char` value directly — for instance one read with `s[i]` — cast it: `s[i] as i64`. Strings are immutable — concatenation produces new strings.

## Traits and stdlib

### Trait declaration + impl + dot-call

```hew
trait Greet {
    fn greet(val: Self) -> string;
}
type Person { name: string }
impl Greet for Person {
    fn greet(p: Person) -> string {
        f"Hello, {p.name}!"
    }
}
fn main() {
    let p = Person { name: "Ada" };
    println(p.greet());   // Hello, Ada!
}
```

Use Go-style named receivers: the first parameter whose type matches the impl target is the receiver (no `self` keyword). The receiver is consumed by value. Every trait method must be explicitly implemented per impl — there are no usable default bodies.

### Display trait (fmt) for f-string interpolation

```hew
type Point { x: f64; y: f64 }
impl Display for Point {
    fn fmt(p: Point) -> string {
        f"({p.x}, {p.y})"
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
    fn fmt(t: Tag) -> string {
        f"#{t.id}"
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
    fn next_val(c: Self) -> Self::Item;
}
type Ticker { current: i64 }
impl Counter for Ticker {
    type Item = i64;
    fn next_val(t: Ticker) -> i64 {
        t.current + 1
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
    fn total(v: Self) -> i64;
}
impl Summable for Vec<i64> {
    fn total(v: Vec<i64>) -> i64 {
        var acc = 0;
        for x in v { acc += x; }
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
    let v = match x { Some(n) => n, None => 0 };
    println(v);   // 42
    let r: Result<i64, i64> = Ok(7);
    let w = match r { Ok(n) => n, Err(e) => e };
    println(w);   // 7
    let y: Option<i64> = None;
    println(match y { Some(n) => n, None => -1 });   // -1
}
```

Option/Result and their constructors are builtin — do not import `std::option`/`std::result`. Unwrap with `match`. Annotate a standalone `None` with a type.

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
    match parse_add("a", "b") { Ok(n) => println(n), Err(e) => println(e) }       // 20
    match parse_add("bad", "b") { Ok(n) => println(n), Err(e) => println(e) }     // bad input
}
```

Use `?` to short-circuit Err and propagate it; the enclosing fn must return a Result whose Err type matches.

### std::string helpers

```hew
import std::string;
fn main() {
    println(string.from_int(42));            // 42
    println(string.to_int("100"));           // 100
    println(string.repeat("*", 3));          // ***
    println(string.pad_left("7", 3, "0"));   // 007
}
```

Import `std::string` and call via the module name. Most case/slice/trim/find operations are builtin methods on `string` itself; `std::string` is for conversions and padding.

### std::math helpers

```hew
import std::math;
fn main() {
    println(math.sqrt(16.0));      // 4
    println(math.abs(-5.0));       // 5
    println(math.max(3.0, 7.0));   // 7
    println(math.min(3.0, 7.0));   // 3
}
```

`abs`/`min`/`max` are generic over Num (work on `i64` and `f64`); `sqrt`/`pow`/`floor`/`ceil`/`round` take `f64`. Use `math.pi()`/`math.e()` (functions, not bare constants).

### std::iter Vec helpers

```hew
import std::iter;
fn main() {
    let v = [1, 2, 3, 4, 5];
    println(iter.sum(iter.map_int(v, |x| x * 2)));         // 30
    println(iter.sum(iter.filter_int(v, |x| x % 2 == 0))); // 6
    println(iter.fold_int(v, 0, |acc, x| acc + x));        // 15
}
```

For Vec processing use the typed monomorphic helpers (`map_int`/`map_str`/`map_f64`, `filter_*`, `fold_*`, `sum`, `any`/`all`, `take_*`/`skip_*`/`count_*`). Pass closures as `|x| body`.

### std::sort — sorting vectors

```hew
import std::sort;
fn main() {
    let nums: Vec<i64> = Vec::new();
    nums.push(3); nums.push(1); nums.push(4); nums.push(1); nums.push(5);
    let sorted   = sort.sort_ints(nums);       // returns new Vec — original unchanged
    let reversed = sort.reverse_ints(sorted);
    println(sorted[0]);    // 1
    println(reversed[0]);  // 5

    let words: Vec<string> = Vec::new();
    words.push("banana"); words.push("apple"); words.push("cherry");
    let sw = sort.sort_strings(words);
    println(sw[0]);        // apple

    let floats: Vec<f64> = Vec::new();
    floats.push(3.14); floats.push(1.41); floats.push(2.72);
    let sf = sort.sort_floats(floats);
    println(sf[0]);        // 1.41
}
```

`sort_ints` / `sort_strings` / `sort_floats` return new sorted Vecs (ascending); `reverse_ints` / `reverse_strings` / `reverse_floats` return new reversed Vecs. The original is never modified.

### std::random — pseudo-random number generation

```hew
import std::random;
fn main() {
    random.seed(42);                   // deterministic sequence
    let r = random.random();           // f64 in [0.0, 1.0)
    let n = random.randint(1, 7);      // i64 in [1, 7)  — like a d6 roll
    let g = random.gauss(0.0, 1.0);   // Gaussian sample

    let v: Vec<i64> = Vec::new();
    v.push(10); v.push(20); v.push(30);
    random.shuffle(v);                 // in-place Fisher-Yates shuffle
    println(v[0]);                     // non-deterministic (seeded above)
}
```

Backed by a CPython-compatible MT19937 Mersenne Twister — the same seed produces the same sequence as CPython's `random` module. Call `seed(n)` first for reproducible output; without a seed, the state is initialised from OS entropy. `randint(lo, hi)` returns in the half-open range `[lo, hi)`.

### std::time::datetime — timestamps and date arithmetic

```hew
import std::time::datetime;
fn main() {
    let now = datetime.now_ms();             // i64 epoch milliseconds
    println(datetime.to_iso8601(now));       // 2026-06-23T18:42:22Z
    println(datetime.format(now, "%Y-%m-%d")); // 2026-06-23

    println(datetime.year(now));    // 2026
    println(datetime.month(now));   // 6
    println(datetime.day(now));     // 23
    println(datetime.hour(now));    // 18
    println(datetime.minute(now));  // 42

    let tomorrow = datetime.add_days(now, 1);
    let diff = datetime.diff_secs(tomorrow, now);
    println(diff);                  // 86400

    match datetime.try_parse("2026-01-01", "%Y-%m-%d") {
        Ok(ts) => println(datetime.year(ts)),   // 2026
        Err(e) => println(f"parse error: {e}"),
    }
}
```

Timestamps are `i64` epoch milliseconds throughout. `to_iso8601` formats as RFC 3339 UTC; `format(ts, fmt)` uses strftime-style patterns. `year`/`month`/`day`/`hour`/`minute`/`second`/`weekday` extract components. `add_days` / `add_hours` perform arithmetic; `diff_secs` returns the signed difference. Use `try_parse` when the input may be malformed.

### std::deque — double-ended queue

```hew
import std::deque;
fn main() {
    let dq = deque.new();
    dq.push_back(1);
    dq.push_back(2);
    dq.push_front(0);     // [0, 1, 2]
    println(dq.len());    // 3
    println(dq.pop_front());  // 0
    println(dq.pop_back());   // 2
    println(dq.is_empty());   // false
    dq.free();
}
```

`deque.new()` returns an opaque `Deque` handle. `push_front` / `push_back` add to either end; `pop_front` / `pop_back` remove and return the element (traps on empty). Current element type is `i64`. Call `free()` when done — the Deque is a heap-managed handle.

### Multiple stdlib imports coexisting

```hew
import std::string;
import std::math;
import std::iter;
fn main() {
    println(string.from_int(math.max(2, 9)));   // 9
    let v = [1, 2, 3];
    println(iter.sum(v));                        // 6
}
```

`std::string`, `std::math`, `std::iter` import together cleanly — the safe stdlib trio to lean on. An unused import warns but still compiles.

### std::encoding::json — parsing and building JSON

```hew
import std::encoding::json;

fn main() {
    // Parse a JSON string
    let val = json.parse("{\"name\": \"Hew\", \"version\": 2}");

    // Read fields
    let name_v = val.get_field("name");
    println(name_v.get_string());       // Hew
    name_v.free();

    let ver_v = val.get_field("version");
    println(ver_v.get_int());           // 2
    ver_v.free();
    val.free();
}
```

```hew
import std::encoding::json;

fn main() {
    // Build a JSON object
    let obj = json.object()
        .with_string("host", "localhost")
        .with_int("port", 8080)
        .with_bool("debug", true);
    println(obj.stringify());   // {"debug":true,"host":"localhost","port":8080} (key order may vary)
    obj.free();
}
```

**`type_of()` constants** — `type_of()` returns an `i32` tag:

| Value | Type |
|-------|------|
| 0 | null |
| 1 | bool |
| 2 | i64 (integer) |
| 3 | f64 (float) |
| 4 | string |
| 5 | array |
| 6 | object |

```hew
import std::encoding::json;

fn main() {
    let s = json.string_value("hello");
    println(s.type_of());   // 4
    s.free();
}
```

**Fallible parsing with `try_parse`** returns `Result<Value, ParseError>`. Match `Err(_)` for the error case — the module-qualified `ParseError::Invalid(msg)` pattern in match arms is currently unsupported at parse time:

```hew
import std::encoding::json;

fn main() {
    match json.try_parse("{\"ok\": true}") {
        Ok(v) => {
            println("parsed ok");
            v.free();
        },
        Err(_) => println("parse failed"),
    }
    match json.try_parse("not valid json {") {
        Ok(v) => { v.free(); },
        Err(_) => println("bad json rejected"),
    }
}
```

**Memory management:** every `Value` (from `parse`, `get_field`, `array_get`, `from_*`, `object()`, `array()`) must be freed with `.free()` before it goes out of scope. Omitting `.free()` is a resource leak. Values returned by `get_field` and `array_get` are heap-allocated clones — free them independently of the parent.

## Structural equality

### Records and payload-bearing enums

Records and enums support `==` and `!=` out of the box; no extra
implementation is needed. Equality is structural: two record values are equal
when every field is equal, and two enum values are equal when they carry the
same variant and every payload field is equal. Fields of any equality-eligible
type participate — integers, `bool`, `char`, `string`, `duration`, nested
records/enums, and **floating-point** fields (`f64`/`f32`, see below).

```hew
type Point { x: i64; y: i64; }

enum Color {
    Red;
    Green;
    Blue;
    Custom(i64);
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
    println(Color::Red == Color::Red);     // true
    println(Color::Red == Color::Green);   // false

    // Enum equality — payload-bearing variants
    println(Color::Custom(42) == Color::Custom(42));   // true
    println(Color::Custom(42) == Color::Custom(99));   // false
    println(Color::Custom(42) != Color::Red);           // true
}
```

### Vec::contains relies on structural equality

`Vec<T>.contains(v)` walks the vector using the same element-wise equality,
so records and enums with payloads work transparently:

```hew
type Point { x: i64; y: i64; }

enum Tag { A; B(i64); }

fn main() {
    // Primitive and string elements
    let nums: Vec<i64> = Vec::new();
    nums.push(10); nums.push(20); nums.push(30);
    println(nums.contains(20));   // true
    println(nums.contains(99));   // false

    let words: Vec<string> = Vec::new();
    words.push("hello"); words.push("world");
    println(words.contains("hello"));   // true
    println(words.contains("bye"));     // false

    // Record elements
    let pts: Vec<Point> = Vec::new();
    pts.push(Point { x: 1, y: 2 });
    pts.push(Point { x: 3, y: 4 });
    println(pts.contains(Point { x: 1, y: 2 }));   // true
    println(pts.contains(Point { x: 5, y: 6 }));   // false

    // Payload enum elements
    let tags: Vec<Tag> = Vec::new();
    tags.push(Tag::A);
    tags.push(Tag::B(7));
    println(tags.contains(Tag::A));      // true
    println(tags.contains(Tag::B(7)));   // true
    println(tags.contains(Tag::B(8)));   // false
}
```

### Floating-point fields use bitwise (total) equality

Structural equality over a float field is **bitwise**, not IEEE numeric. The
compiler compares the raw bit patterns of the two floats, so:

- it is **reflexive**: `x == x` holds for every value, including `NaN`. Two
  `NaN` values compare *equal* when their bit patterns are identical.
- `+0.0` and `-0.0` are **distinct** (their bit patterns differ), so a record
  holding `+0.0` is not equal to one holding `-0.0`.

This is deliberately different from the IEEE numeric `==` you get on a bare
`f64`/`f32` expression, where `NaN != NaN` and `+0.0 == -0.0`:

```hew
record Vec2 { x: f64, y: f64 }

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
> not extend to `Vec<f64>::contains` or direct `f64 == f64` expressions.

```hew
record Coord { x: f64, y: f64 }

fn main() {
    let m: HashMap<Coord, i64> = HashMap::new();
    m.insert(Coord { x: 1.5, y: 2.5 }, 42);
    let v = m.get(Coord { x: 1.5, y: 2.5 });
    match v {
        Some(n) => println(n),   // 42 — structurally equal key round-trips
        None => println(-1),
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
type Packet { data: bytes; }
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
        Ok(back) => println(back.name),  // ada
        Err(_) => println("parse failed"),
    }
}
```

Each field carries a `@N` tag — a stable wire identifier that must never be
reused, even if the field is later removed (HEW-SPEC-2026.md §7.2). A field
can also be marked `optional`, which only affects wire-decode behaviour (a
missing field decodes without error); the field's Hew-level type stays the
bare element type, not `Option<T>` — see
[`examples/playground/types/wire_types.hew`](../examples/playground/types/wire_types.hew)
for that form.

`e.to_json()` and `TypeName.from_json(text)` (a call on the type name
itself, not `::`) round-trip a wire type through JSON. The runtime envelope
used for actor-to-actor message transport is CBOR; the `std::encoding::*`
surface (JSON, MessagePack, ...) is for cross-service and file I/O. Use `hew
wire check <file.hew> --against <baseline.hew>` to check schema
compatibility between two versions of a wire type.

Full example: [`examples/playground/types/wire_types.hew`](../examples/playground/types/wire_types.hew). Spec: HEW-SPEC-2026.md §7.

### `#[resource]` and `#[linear]` — compiler-checked resource ownership

```hew
#[resource]
type Conn {
    fd: i64
}
impl Conn {
    fn close(c: Conn) {
        println(f"closing fd {c.fd}");
    }
}

fn main() {
    let c: Conn = Conn { fd: 7 };
    println("work");
    // c drops at scope exit here; Conn::close(c) runs automatically.
}
```

```hew
#[linear]
type Tx { id: i64 }
impl Tx {
    fn commit(consuming self) { println(f"commit {self.id}"); }
    fn rollback(consuming self) { println(f"rollback {self.id}"); }
}

fn main() {
    let t = Tx { id: 1 };
    t.commit();   // commit 1
}
```

Both attributes are affine — the compiler enforces a single live binding per
value via the move checker — but they differ in what happens at scope exit.
A `#[resource]` type auto-closes (discarding the `Result`) unless closed
early; a `#[linear]` type has **no** implicit drop at all — leaving one
unconsumed at scope exit is a compile error. Neither supports a
user-defined `impl Drop`.

The `close` method (for `#[resource]`) and any `consuming self` method (for
`#[linear]`) must be declared in a sibling `impl` block, never inline in the
type body — an inline declaration is rejected at parse time.

Full example (`#[linear]`): [`examples/v05/linear/accept/linear_consumed_via_rollback_on_err.hew`](../examples/v05/linear/accept/linear_consumed_via_rollback_on_err.hew). For `#[resource]`, see HEW-SPEC-2026.md §3.7.8.

### `#[opaque]` — FFI-backed handle types

```hew
#[opaque]
pub type FileHandle {
}
```

An `#[opaque]` type body must be empty — it declares a handle with no
fields, produced only via FFI. There is no struct-literal constructor;
values come only from a foreign function. Most opaque types require an
explicit `.close()`/`.free()` call since there is no implicit drop
(HEW-SPEC-2026.md §3.10.7). Over twenty stdlib types use this pattern,
including `Deque`, `json.Value`, `csv.Reader`, `regex.Pattern`, and
`net.Connection`.

`std/deque.hew`'s `#[opaque]\npub type Deque { }` plus its sibling `trait
DequeMethods` / `impl DequeMethods for Deque` is the canonical shape to
copy for a new opaque type.

### Typed streams — `await sink.send(x)` / `await stream.recv()`

```hew
import std::stream;

actor Echo {
    let n: i64;
    receive fn run(unused: i64) {
        let (sink, input) = stream.bytes_pipe(4);
        for i in 0..n {
            await sink.send(f"x{i}".to_bytes());
        }
        sink.close();
        var done = false;
        while !done {
            let item = await input.recv();
            match item {
                Some(b) => println(b.to_string()),  // x0, x1
                None => { done = true; },
            }
        }
    }
}

fn main() {
    let e = spawn Echo(n: 2);
    e.run(0);
    sleep(300ms);
}
```

`await sink.send(x)` and `await stream.recv()` suspend the calling coroutine
instead of OS-parking a worker, so a stream stage frees its worker while waiting.
Only `Stream<bytes>` / `Sink<bytes>` suspend (the canonical element type), and the
canonical method names are `recv()` / `send()` — not `next()` / `write()`. `recv()`
yields `Option<bytes>` (`None` is EOF); match it, never unwrap. `await sink.send`
is statement-position only. Build the pipe with the public
`std::stream.bytes_pipe(capacity)` constructor — no raw extern, no `unsafe` — and
turn text into a frame with the public `string.to_bytes()` surface. In v0.5 keep
both ends in one handler: moving an owned `Stream`/`Sink` into actor state is NYI
(owned-handle aggregate state lands in v0.5.1). Full example:
[`examples/v05/surfaces/typed_streams.hew`](../examples/v05/surfaces/typed_streams.hew).

### Channels — `channel.new`, `await rx.recv()`, and select arms

```hew
import std::channel::channel;

actor Inbox {
    receive fn run(unused: i64) {
        let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(4);
        tx.send("ready");
        tx.close();

        match await rx.recv() {
            Some(msg) => println(msg),   // ready
            None => println("closed"),
        }

        select {
            again from rx.recv() => {
                match again {
                    Some(msg) => println(msg),
                    None => println("closed"),
                }
            },
            after 1s => println("timeout"),
        };

        rx.close();
    }
}
```

Use `channel.new(capacity)` to build a bounded MPSC channel. `Sender<T>` is
cloneable, and both channel handles are closed automatically at scope exit;
call `.close()` only when you need to end production or reception before then.
`await rx.recv()` returns `Option<T>`: `Some(value)` for a received item and
`None` when the channel is closed. `rx.try_recv()` never suspends and returns
`None` for both empty and closed. In `select`, write the sealed channel arm as
`pat from rx.recv()` and match the bound `Option<T>`. Full examples:
[`examples/channel/await_recv_actor.hew`](../examples/channel/await_recv_actor.hew)
and [`examples/channel/select_recv.hew`](../examples/channel/select_recv.hew).

### Regex captures — `capture` / `find_all` / `find_all_submatch`

```hew
import std::text::regex;

fn main() {
    let re = regex.new("(?P<k>[a-z]+)=(?P<v>[0-9]+)");
    let text = "a=1 bb=22";
    match re.capture_named(text, "v") {
        Some(v) => println(f"first value: {v}"),   // first value: 1
        None => println("none"),
    }
    let rows = re.find_all_submatch(text);
    for i in 0..rows.len() {
        let key = match rows.group(i, 1) { Some(g) => g, None => "?" };
        let val = match rows.group(i, 2) { Some(g) => g, None => "?" };
        println(f"{key} -> {val}");   // a -> 1 ; bb -> 22
    }
    re.free();
}
```

Compile a pattern with `regex.new` (panics on bad syntax; use `try_new` for a
`Result`). `capture(input, group)` / `capture_named(input, name)` return the
indexed/named submatch of the FIRST match as an `Option<string>` — group 0 is the
whole match. `find_all` returns every whole match as a `Vec<string>`;
`find_all_submatch` returns a row-major `CaptureMatches` table — `rows.len()`
rows, `rows.width()` groups each, read with `rows.group(row, col)` /
`rows.whole(row)`. Always `free()` a compiled `Pattern`. Full example:
[`examples/v05/surfaces/regex_captures.hew`](../examples/v05/surfaces/regex_captures.hew).

### Templates — `parse` + `render_try`

```hew
import std::text::template;

fn main() {
    let ctx = template.new_ctx();
    template.ctx_set_str(ctx, "name", "Hew");
    let xs: Vec<string> = Vec::new();
    xs.push("a");
    xs.push("b");
    template.ctx_set_list(ctx, "xs", xs);
    let t = template.parse("hi {{.name}}:{{range .xs}} {{.}}{{end}}");
    match template.render_try(t, ctx) {
        Ok(s) => println(s),   // hi Hew: a b
        Err(_) => println("error"),
    }
}
```

Build a flat `Ctx` with `new_ctx()` then `ctx_set_str` / `ctx_set_int` /
`ctx_set_bool` (scalars) and `ctx_set_list` (a `Vec<string>`). `parse` compiles a
Go-style template — `{{.key}}` substitutes, `{{if .key}}…{{end}}` is conditional,
`{{range .list}}…{{.}}…{{end}}` iterates with `.` bound to each item. Render with
the free function `template.render_template(t, ctx)` (panics on error) or
`template.render_try(t, ctx)` (returns `Result`); the method form `t.render(ctx)`
is deferred in v0.5. `TemplateError` has no `Display`, so handle the `Err` arm
with a literal message rather than interpolating it. Full example:
[`examples/v05/surfaces/template_render.hew`](../examples/v05/surfaces/template_render.hew).

### Unicode — rune helpers + classification predicates

```hew
import std::text::unicode;

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
import std::io::scanner;

fn main() {
    var sc = scanner.from_string("alpha beta\ncolour");
    sc = scanner.with_split(sc, SplitWords);
    sc = scanner.scan(sc);
    while scanner.has_next(sc) {
        println(scanner.text(sc));
        sc = scanner.scan(sc);
    }

    let (next, line) = scanner.next_line(scanner.from_string("first\nsecond"));
    match line {
        Some(s) => println(s),   // first
        None => println("none"),
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

### HTTP over `await` — async client + server

The flagship v0.5 networking surface is an `await`-suspended HTTP/1.1 client and
server built on `net.connect` / `net.listen` plus the pure-Hew codecs in
`std::net::http::http_async_client` / `http_async_server`. A server handler
`await`s a connection, drives an `await conn.read_string()` loop until the
request is buffered, then replies; a client writes a request and `await`s the
response. Every `await` suspends the handler (not the worker), so one worker can
serve and fetch on the same thread. The request/response codecs are pure and
runnable in isolation:

```hew
import std::net::http::http_async_client;

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

### Deadline forms — `await conn.read_string() | after d` and `await ln.accept() | after d`

The `| after duration` timeout combinator works with the two blocking network
operations, converting a plain suspend into a timed suspend that returns
`Result` on expiry:

| Suspend form | Return type |
| --- | --- |
| `await conn.read_string()` | `string` |
| `await conn.read_string() \| after d` | `Result<string, IoError>` |
| `await conn.read()` | `bytes` |
| `await conn.read() \| after d` | `Result<bytes, IoError>` |
| `await ln.accept()` | `net.Connection` |
| `await ln.accept() \| after d` | `Result<net.Connection, IoError>` |

Use inside a `scope` body to bound how long a handler waits for a peer:

<!-- doctest: skip -->
```hew
scope {
    fork {
        match await conn.read_string() | after 5s {
            Ok(data) => conn.write_string(data),
            Err(_)   => conn.close(),
        }
    }
}
```

The deadline form requires `await` — `conn.read_string() | after d` without
`await` is a type error (checker expressions.rs:1778-1800).

### Remote ask suspension — `RemotePid<T>.ask` from an actor handler

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
        let found: Result<RemotePid<Echo>, LookupError> = Node::lookup("echo");
        match found {
            Ok(peer) => {
                let reply = peer.ask(7, 1000);
                match reply {
                    Ok(n) => println(f"answer={n}"),
                    Err(_) => println("ask failed"),
                }
            },
            Err(_) => println("lookup failed"),
        }
    }
}
```

`RemotePid<T>` names an actor discovered through the node registry. From an
actor handler, `peer.ask(msg, timeout_ms)` lowers to the cross-node suspending
remote-ask path and returns `Result<T::Reply, AskError>` on resume; match
`Ok`/`Err` instead of assuming a reply. A same-node lookup can still return a
`RemotePid<T>`, but the local-mailbox bridge for the remote-ask path is scoped
to fail closed as `AskError::RoutingFailed`; use a `LocalPid<T>` direct actor
call when both actors are intentionally local. Full example:
[`examples/distributed/kv_client.hew`](../examples/distributed/kv_client.hew)
and [`examples/distributed/kv_server.hew`](../examples/distributed/kv_server.hew).

#### Peer authentication (native quic-mesh)

On the QUIC mesh transport, peers authenticate by mutual TLS with pinned public
keys. Call these before `Node::start`:

<!-- doctest: skip -->
```hew
Node::set_transport("quic-mesh");
Node::load_keys("node.key");        // mint+persist this node's identity (stable SPKI)
Node::allow_peer("3059…0107");      // pin a peer's SPKI (lowercase hex); fail-closed
Node::start("0.0.0.0:9000");
```

`load_keys` loads the node's TLS identity from the keyfile, creating one on
first run so the public key stays stable across restarts. `allow_peer` adds a
peer's certificate SPKI to the fail-closed allowlist — an unpinned peer's
handshake is rejected. These are native-only; WASM/sandbox builds carry no
networking. See [`examples/distributed_hello.hew`](../examples/distributed_hello.hew).

### TLS client — free-function surface (with a v0.5 data-plane caveat)

```hew
import std::net::tls;

fn main() {
    let stream = tls.connect("example.com", 443);
    let req = "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n";
    let payload = req.to_bytes();
    let sent = tls.write(stream, payload);
    println(f"sent {sent}/{payload.len()} bytes");
    // match tls.read(stream, 256) { Ok(data) => ..., Err(_) => ... }
    tls.close(stream);
}
```

Use the FREE-FUNCTION surface — `tls.connect(host, port)` (system-root verified),
`tls.write` / `tls.try_write`, `tls.read`, `tls.close`. Request/response bodies
are `bytes`: build a payload with the public `string.to_bytes()` surface and decode
with `bytes.to_string()`. The method form (`stream.read(n)`) is NOT supported yet.

> **Known gap (v0.5):** the TLS data-plane FFI bridge
> (`hew_tls_write_result` / `hew_tls_read_result`) is wired to the legacy
> `(ptr, len)` / `HewVec` byte ABI while Hew's `bytes` is a `BytesTriple`, so
> `tls.write` currently transmits 0 bytes (the length is dropped at the boundary)
> and the response read does not complete. `tls.connect` also does not record a
> failed handshake in `last_error()`. The snippet above is the correct caller
> shape and type-checks/runs to completion (reporting the short write); the
> encrypted round-trip works once the runtime bridge adopts the `BytesTriple`
> ABI. Tracked for the under-the-hood TLS workstream.

Full example (fail-closed on the short write so it terminates):
[`examples/net/tls_client.hew`](../examples/net/tls_client.hew).
