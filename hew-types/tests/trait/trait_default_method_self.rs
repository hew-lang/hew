//! Tests for trait default method bodies resolving sibling trait methods on `Self`.
//!
//! Prior to this fix, a default method body that called another trait method via
//! `self.other()` would fail type-checking with "no method `other` on `Self`".

use crate::common;

use common::typecheck_isolated as typecheck;
use hew_types::error::TypeErrorKind;

/// A default method body calling a required (non-default) method must type-check.
#[test]
fn default_method_calls_required_method_typechecks() {
    let output = typecheck(
        r#"
        trait Greeter {
            fn name(self) -> string;
            fn greet(self) -> string {
                "Hi " + self.name()
            }
        }

        type Person { name: string }

        impl Greeter for Person {
            fn name(p: Person) -> string {
                p.name
            }
        }

        fn main() {
            let p = Person { name: "Alice" };
            let s = p.greet();
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected no type errors; got: {:#?}",
        output.errors
    );
}

/// A default method body calling another default method (chain) must type-check.
#[test]
fn default_method_calls_another_default_method_typechecks() {
    let output = typecheck(
        r#"
        trait Describable {
            fn kind(self) -> string;
            fn label(self) -> string {
                "[" + self.kind() + "]"
            }
            fn describe(self) -> string {
                "Type: " + self.label()
            }
        }

        type Widget { id: i64 }

        impl Describable for Widget {
            fn kind(w: Widget) -> string {
                "widget"
            }
        }

        fn main() {
            let w = Widget { id: 1 };
            let s = w.describe();
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected no type errors; got: {:#?}",
        output.errors
    );
}

/// Overriding a default method must still produce no errors.
#[test]
fn overriding_default_method_typechecks() {
    let output = typecheck(
        r#"
        trait Greeter {
            fn name(self) -> string;
            fn greet(self) -> string {
                "Hi " + self.name()
            }
        }

        type Person { name: string }
        type Robot { id: i64 }

        impl Greeter for Person {
            fn name(p: Person) -> string { p.name }
        }

        impl Greeter for Robot {
            fn name(r: Robot) -> string { "R-bot" }
            fn greet(r: Robot) -> string { "Beep " + r.name() }
        }

        fn main() {
            let p = Person { name: "Bob" };
            let r = Robot { id: 1 };
            let s1 = p.greet();
            let s2 = r.greet();
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected no type errors; got: {:#?}",
        output.errors
    );
}

/// A default method body calling a missing method must still report an error —
/// the fix must not suppress genuine "no method" diagnostics.
#[test]
fn default_method_calling_undefined_method_errors() {
    let output = typecheck(
        r#"
        trait Broken {
            fn greet(self) -> string {
                "Hi " + self.nonexistent()
            }
        }

        type Thing { x: i64 }

        impl Broken for Thing {}

        fn main() {
            let t = Thing { x: 1 };
            let s = t.greet();
        }
        "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::UndefinedMethod),
        "expected UndefinedMethod error for nonexistent method; got: {:#?}",
        output.errors
    );
}
