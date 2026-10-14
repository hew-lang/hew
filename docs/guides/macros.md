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

## Attribute macros

An attribute rule wraps a function with ordinary generated declarations:

```hew
macro logged {
    #[($label:literal)] $f:fn => items {
        $f.vis fn $f.name $f.generics ($f.params) $f.ret $f.where {
            println($label);
            $f.body
        }
    }
}

#[logged("answer")]
fn answer() -> i64 { return 42; }
```

Use `#[logged]` for an empty argument pattern, or qualify an imported macro as
`#[observability.logged("answer")]`. A macro may contain both attribute and
`!` rules; each invocation considers only its own rule form.

`$f:fn` matches a free function or method. `$m:method` requires a receiver;
place this rule first when a forwarding wrapper needs a different method call.
Append `fails` to require a fallible function. Attribute rules always produce
`items` and preserve exactly one function named by the caller's `$f.name`.

The whole `$f` inserts the function. Its projections are `.vis`, `.name`,
`.generics`, `.params`, `.args`, `.ret`, `.where` and `.body`. `.params` inserts
parameters without parentheses; `.args` inserts non-receiver forwarding
arguments. `.ret` includes the complete return and `fails` clauses. A `method`
capture also permits `.receiver`. Optional clauses insert nothing when absent.
These are projections of function captures; `$value.len()` on an expression
capture remains ordinary member access.

Place `$f.body` as a generated function's body, or directly as a statement or tail within it,
at most once per function. Its `return`, `?` and `return error` target that
function. A closure, loop or nested block cannot surround the projected body.
Caller bindings retain their contexts and cannot see template locals.

Attribute macros apply to free functions, impl methods, default trait methods
with bodies, and plain actor methods. They cannot rewrite receive handlers,
lifecycle hooks, extern functions, bodiless trait signatures or type headers.
Use `#[intercept]` for per-delivery actor behaviour.

The attribute nearest the function expands first: in `#[outer] #[inner] fn f`,
`outer` runs around `inner`. Other attributes are held aside and reattached to
the function carrying the caller's name, so `#[test]` remains discoverable.
Built-in attribute and macro names are reserved. Unresolved attribute names
remain errors after macro resolution.
