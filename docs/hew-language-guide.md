# Hew v0.5 Language Guide

A reference for writing correct idiomatic Hew. Every example below was executed against `target/debug/hew` and ran clean.

## Core idioms

- Primitive types are lowercase: `i64`, `string`, `f64`, `bool`, `char` — never `Int`, `String`, `Float`.
- Integer literals default to `i64`, float literals to `f64`. Annotate only for narrower widths.
- Interpolate strings with the `f` prefix: `f"x={x}"`. A plain `"{x}"` prints literal braces.
- Convert numbers with `as`: `x as i64`, `pi as i32`. It is the only conversion mechanism.
- Never mix integer widths in one expression; cast the narrower operand up first: `x as i64 + 1`.
- Dispatch with `match`, not if/else chains — `match` on a closed enum enforces exhaustiveness.
- Iterate counts with `for i in 0..n` (exclusive) or `0..=n` (inclusive); the binding must be a named identifier.
- Read collection elements with `.get(i)` — universal across element types. `v[i]` works for scalars, strings, records, and tuples.
- `Vec<string>` supports `v[i]` (returns a fresh owned `string`; the Vec stays usable), `.get(i)`, range-slices, and for-in. For `Vec<enum>`, prefer `.get(i)`.
- Build maps/sets with `::new()` + `.insert()`; bind with `let` (interior mutability, not reassignment).
- Look up `HashMap`/`Option`/`Result` with `match`, not subscript or `.unwrap()`.
- Declare records with `type Name { field: T; }` (semicolons); enum variants are `;`-separated.
- Access actor state by bare field name inside handlers — no `self.` or `this.`.
- Fire-and-forget actor sends have no return type and no `await`: `ref.method(arg);`.
- Ask (request-reply) is `await ref.method(arg)` and returns `Result<R, AskError>` — match `Ok`/`Err`.
- Passing a value into an actor consumes it; duplicate it first with `clone x` (canonical) if you keep using it afterward.
- Within a fn or actor there is no borrow checker: pass values freely, mutations to Vec/HashMap persist in the caller.
- Last expression of a block (no trailing semicolon) is its value; a trailing `;` makes it unit.
- Lean on the safe stdlib trio: `std::string`, `std::math`, `std::iter`. Do not import `std::option`/`std::result`.

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

Use `as` for all numeric conversions — widening, narrowing, int↔float, signed↔unsigned. Float→int truncates toward zero. (Avoid casting negative values to unsigned types.)

### Mixed-width arithmetic requires a cast

```hew
fn main() {
    let x: i32 = 1;
    let y: i64 = x as i64 + 1;   // cast the narrow operand up first
    println(f"y={y}");
}
```

Never mix widths in one expression. Cast the narrower operand with `as` before combining; integer literals adopt the surrounding type.

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

`for x in collection` binds each element value. Index Vec elements with `.get(i)`, not `v[i]`.

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
    println(v.get(0));  // 10
}
```

Annotate the binding type so the element type is inferred. `.len()` returns `i64`.

### .get(i) — universal element accessor

```hew
fn main() {
    let v: Vec<string> = Vec::new();
    v.push("a");
    v.push("b");
    println(v.get(0));  // a
    println(v.get(1));  // b
}
```

`.get(i)` is the default accessor — it works for every element type including `Vec<string>` and `Vec<enum>`. Returns `T` directly; out-of-bounds panics. Guard with `i < v.len()` if the index may be invalid.

### v[i] indexing (scalar / string / record / tuple)

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
    println(names.get(0));     // ada — names is still usable
}
```

Use `v[i]` for scalars, strings, records, and tuples where it reads cleanest; it returns the value. A `Vec<string>` index returns a fresh owned `string` (an element clone, not a move-out), so the Vec stays fully usable afterward. For `Vec<enum>`, prefer `.get(i)`.

### Range-slice v[a..b] returns a new Vec

```hew
fn main() {
    let v: Vec<i64> = Vec::new();
    v.push(10); v.push(20); v.push(30); v.push(40);
    let s = v[1..3];
    println(s.len());    // 2
    println(s.get(0));   // 20
}
```

`v[a..b]` yields a fresh `Vec<T>` (half-open, `b` exclusive) with its own `.len()`/`.get()`/for-in, for every element type including `Vec<string>`. (Single elements come straight from `v[i]`; the older `v[i..i+1]` slice is no longer required for that.)

### for-in over Vec (scalars / strings / records / tuples)

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

Prefer for-in for read-only traversal of scalar/string/record/tuple Vecs. For `Vec<enum>`, loop by index with `.get(i)` instead.

### Index-loop for Vec<enum> and uncertain element types

```hew
enum Colour { Red; Green; Blue; }
fn main() {
    let v: Vec<Colour> = Vec::new();
    v.push(Colour::Red);
    v.push(Colour::Blue);
    let c = v.get(1);
    match c {
        Colour::Red => println("red"),
        Colour::Green => println("green"),
        Colour::Blue => println("blue"),
    }
}
```

For `Vec<enum>` traverse with `for i in 0 .. v.len()` and `v.get(i)` — the only accessor that does not trap on enum elements. Hoist `let n = v.len()` before the loop.

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

`.get(i)`, `v[i]`, and `.pop()` all return `T` directly (no `Option`) and trap on a bad index or empty vec; guard with `i < v.len()` (and check `.len()` before `.pop()`) for safe access.

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
fn main() {
    let flags: HashMap<string, bool> = HashMap::new();
    flags.insert("debug", true);
    let ratios: HashMap<string, f64> = HashMap::new();
    ratios.insert("pi", 3.14);
}
```

Keys are `string`; verified value types are `i64`, `string`, `bool`, `f64`.

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

Set membership is `.contains(x)` (note: `.contains_key` is the HashMap spelling). Inserts dedup automatically. Supported element types are `i64` and `string`. There is no iteration over maps or sets — track keys externally in a `Vec` and probe with `.get`/`.contains`.

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
    for i in 0 .. n { sum = sum + v.get(i); }
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

Mix unit, tuple, and struct variants in one enum. Struct-variant fields use `;` separators; the variant pattern uses `{ w, h }` shorthand. Construct variants by bare name.

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
            println(f"first={items.get(0)}");  // first=10
        },
    }
}
```

Read the prior vec out of `self.items`, push, and rebuild the variant. Access elements with `.get(i)` and `.len()`.

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

Accept `Vec<T>` in a generic function and use `.len()`/`.get(i)`.

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

A monomorphic enum, even one carrying a string payload, is a valid Vec element. Construct variants as `Enum::Variant(payload)`. (Only concrete/monomorphic record and enum element types are supported as Vec elements.)

### Vec of a concrete record

```hew
type Point { x: i64; y: i64; }
fn main() {
    let v: Vec<Point> = Vec::new();
    v.push(Point { x: 1, y: 2 });
    v.push(Point { x: 3, y: 4 });
    let got = v.get(1);
    println(got.x);   // 3
    println(got.y);   // 4
}
```

Store concrete records in a Vec and bind the element with `let got = v.get(i)` before reading fields.

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
    println(f"n={n} expr={n + 1} up={name.to_uppercase()}");
}
```

Arbitrary expressions (arithmetic, no-arg method calls, field access) work inside `{}`. You cannot put a `"..."` string literal inside the `{}` of an f-string — bind it to a `let` first.

### .split(sep) and reading elements

```hew
fn main() {
    let parts = "a,b,c".split(",");
    println(f"count={parts.len()}");
    for i in 0 .. parts.len() {
        let p = parts.get(i);
        println(p);
    }
}
```

Read elements with `.get(i)` or `parts[i]` and iterate by index — both clone the element and leave `parts` usable. (`for p in parts` also works but consumes the Vec, so use the indexed loop when you need `parts` again afterward.)

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

Build strings with `+`. `char_at` gives the code point (`i64`); `string.from_char` renders it back. Strings are immutable — concatenation produces new strings.

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

Implement Display via the `fmt` method (the spec's `display` name is aspirational) and interpolate with f-strings. To print a user type, wrap it: `println(f"{value}")` — `println(value)` on a bare user Display type does not dispatch.

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

## v0.5 surfaces

The v0.5 release adds async networking, channels, and text surfaces. Each
subsection below is a runnable snippet; a full idiomatic program per surface
lives under [`examples/v05/surfaces/`](../examples/v05/surfaces) (text
surfaces), [`examples/channel/`](../examples/channel) (channels), or
[`examples/net/`](../examples/net) (networking).

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
    sleep_ms(300);
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
    for i in 0..runes.len() {
        let cp = runes.get(i);
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
