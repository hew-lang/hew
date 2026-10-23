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

## Inspect and test an expansion

Use `hew tool expand file.hew` to read the complete expanded document, or
`hew tool expand file.hew --at 20:5` to select the innermost invocation at a
one-based line and character column. Both views expand nested invocations.
Redirect the output to a file when you want to compare it or keep it beside the
original source. Add `--json` to receive `text`, `inspectionOnly` and `origins`;
each origin names the macro and gives its invocation and selected rule as an
original file path and UTF-8 byte span (`start`, inclusive; `end`, exclusive).
These spans refer to original files, not positions in the rendered text.

Context comments such as `helper/*ctx:1*/` distinguish generated names. This is
an inspection view: comments cannot encode hygiene in compilable source. In
particular, removing them can merge distinct generated bindings, and imported
macros can refer to names in their defining module that are unavailable in the
caller. Imports remain imports; expansion does not bundle every dependency into
one standalone file. Simple expansions without these dependencies can be valid
ordinary Hew, but use the original source for checking, testing and execution.
The formatter preserves macro bodies and invocation arguments as source token
trees.

In VS Code, run **Hew: Expand Macro at Cursor** or **Hew: Show Expanded Document**.
The result opens beside the source as a read-only Hew document. It refreshes
when the source's diagnostics update after analysis; **Hew: Refresh Expansion**
requests a fresh view explicitly. **Hew: Go to Expansion Source** opens the
original document, an invocation or its selected rule. Type errors and other
semantic diagnostics stay attached to the original source. Generated declarations
also link back to their invocation and selected rule through normal definition
navigation.

Other language-server clients can send `hew/expandMacro` with
`{ "textDocument": { "uri": "file:///path/to/file.hew" } }`. Add a zero-based LSP
`position` for one invocation. The default result is text or `null` when no view
is available. Add `"includeMetadata": true` for the same object returned by CLI
`--json`. Browser clients use the shared `expand_macro(source, offset)` renderer;
omitting the UTF-8 byte offset requests the whole document.

Test a macro by invoking it from an ordinary Hew test. A separate test file can
import the public macro without exercising unrelated application code:

```hew
// arithmetic.hew
pub macro twice {
    ($value:expr) => expr { $value + $value }
}
```

```hew,ignore
// arithmetic_test.hew
import arithmetic.{twice};

#[test]
fn expands_captured_expression() {
    assert(twice!(20 + 1) == 42);
}
```

Run `hew check arithmetic_test.hew`, then `hew test arithmetic_test.hew`.
The regular test runner compiles the expansion through the same type, ownership
and native execution path as application code. Macro-generated `#[test]`
functions are discovered by the same runner and editor test interface.

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
