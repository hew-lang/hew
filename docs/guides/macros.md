# Declarative macros

A macro inserts ordinary Hew syntax before type checking. Use a function for
ordinary computation; a macro is useful when a repeated pattern needs to create
declarations or place a captured expression inside control flow.

```hew
macro record {
    ($name:ident, $constructor:ident, $value:expr) => items {
        type $name { value: i64; }
        fn $constructor() -> $name { $name { value: $value } }
    }
}
record!(Count, initial_count, 42)

macro twice {
    ($value:expr) => expr { $value + $value }
}
fn main() {
    println(initial_count().value);
    println(twice!(1 + 2));
}
```

The two lines print `42` and `6`. Captured expressions retain their structure:
`$value * 2` with `1 + 2` means `(1 + 2) * 2`.

Rules appear inside a module-level `macro name` declaration. Each rule has a
parenthesized pattern, `=>`, and either `expr { expression }` or
`items { declarations }`. Rules are tried in order. The first complete match
selects the rule; a later type error does not try another rule.

Patterns can capture `ident`, `expr`, `ty`, `pat`, `block` and `literal`
fragments. Other tokens match literally. Use `$(...),*` or `$(...),+` for a
separated repetition, or `$(...)?` for an optional group. Repetitions cannot
nest. A template repetition must use captures from the same pattern group.

```hew
macro sum {
    ($($value:expr),*) => expr {
        { var total = 0; $(total += $value;)* total }
    }
}
```

Template names have their own syntax context. A `total` in the caller does not
conflict with the local in `sum!`, and two invocations can each create a private
helper with the same spelling. Names supplied through `ident` captures deliberately
remain visible in the caller. Free names in a template resolve in the macro's
defining module. Export a macro with `pub macro`; its free references must also
be public. Import and qualify macros in the same way as other module items:
`import counters.{record as make_record};` then `make_record!(...)`.

Expansion does not evaluate an argument. `twice!(next())` calls `next()` twice.
To evaluate once, place the capture in a template local and use that local.
The expanded code receives normal type, ownership, suspension and actor checks.

Template-written `return`, `?`, `break` and `continue` must target a function or
loop created in the template. Captured control flow retains its caller target;
a loop inside a macro cannot catch a caller's `break`. Expansion is limited to
64 nested invocations and one million generated nodes per outermost invocation.

Use `hew tool expand file.hew` to read the expanded program, or
`hew tool expand file.hew --at 20:5` to select an invocation. Context comments
such as `helper/*ctx:1*/` distinguish generated names; this display is for
inspection and does not encode hygiene in compilable source. The language server
exposes the same view through `hew/expandMacro`, and generated declarations link
back to their invocation and selected rule. The formatter preserves macro bodies
and invocation arguments as source token trees.
