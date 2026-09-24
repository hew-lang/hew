//! `trait Error: Display` and the `dyn Error` surface it enables.
//!
//! `std/builtins.hew` declares `Error` with `Display` as its only obligation:
//! an error is a value that can say what went wrong. Two checker facts follow
//! and are pinned here.
//!
//! * `impl Error for X {}` requires `impl Display for X`. The obligation is
//!   reported where the promise is made, in either declaration order.
//! * A type that implements `Error` coerces to `dyn Error` at every error
//!   position - `return error e`, `?` into a `fails dyn Error` caller and
//!   `Err(e)` - while a type that does not stays a type mismatch.
//!
//! Native execution of a `dyn Error` value is a separate matter: SIR has no
//! value contract for any trait object yet, so these programs check and then
//! fail closed at semantic lowering.

use crate::common;

use common::typecheck;
use hew_types::error::TypeErrorKind;

#[test]
fn impl_error_without_display_is_refused() {
    let output = typecheck(
        r"
        type ParseFailure { detail: string }

        impl Error for ParseFailure {}
        ",
    );
    let error = output
        .errors
        .iter()
        .find(|err| err.kind == TypeErrorKind::BoundsNotSatisfied)
        .unwrap_or_else(|| {
            panic!(
                "expected a supertrait obligation, got: {:#?}",
                output.errors
            )
        });
    assert!(
        error.message.contains("supertrait `Display`") && error.message.contains("ParseFailure"),
        "diagnostic should name the missing supertrait and the type: {}",
        error.message
    );
}

#[test]
fn impl_error_before_its_display_impl_is_accepted() {
    // The obligation is checked against the whole program's impl set, not the
    // impls seen so far, so writing `impl Error` first is legal.
    let output = typecheck(
        r#"
        type ParseFailure { detail: string }

        impl Error for ParseFailure {}

        impl Display for ParseFailure {
            fn fmt(value: ParseFailure) -> string {
                f"parse failed: {value.detail}"
            }
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected a clean check, got: {:#?}",
        output.errors
    );
}

#[test]
fn error_impl_coerces_to_dyn_error_at_every_error_position() {
    let output = typecheck(
        r#"
        type ParseFailure { detail: string }

        impl Display for ParseFailure {
            fn fmt(value: ParseFailure) -> string {
                f"parse failed: {value.detail}"
            }
        }

        impl Error for ParseFailure {}

        fn parse(text: string) -> i64 fails dyn Error {
            if text == "" {
                return error ParseFailure { detail: "empty" }
            }
            return 1
        }

        fn parse_twice(text: string) -> i64 fails dyn Error {
            let first = parse(text)?;
            return first + 1
        }

        fn wrapped(text: string) -> Result<i64, dyn Error> {
            if text == "" {
                return Err(ParseFailure { detail: "empty" })
            }
            return Ok(1)
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected a clean check, got: {:#?}",
        output.errors
    );
}

#[test]
fn type_without_error_impl_does_not_coerce_to_dyn_error() {
    let output = typecheck(
        r#"
        type Bare { detail: string }

        fn parse() -> i64 fails dyn Error {
            return error Bare { detail: "empty" }
        }
        "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|err| matches!(err.kind, TypeErrorKind::Mismatch { .. })
                && err.message.contains("dyn Error")
                && err.message.contains("Bare")),
        "expected a `dyn Error` mismatch on the un-erasable payload, got: {:#?}",
        output.errors
    );
}

#[test]
fn unknown_trait_in_dyn_type_is_refused_once() {
    let output = typecheck(
        r"
        fn produce() -> dyn Nope {
            return 1
        }
        ",
    );
    let refusals: Vec<_> = output
        .errors
        .iter()
        .filter(|err| err.kind == TypeErrorKind::UndefinedType && err.message.contains("`Nope`"))
        .collect();
    assert_eq!(
        refusals.len(),
        1,
        "a `dyn` annotation is resolved twice; the refusal must report once: {:#?}",
        output.errors
    );
    assert!(
        refusals[0].message.contains("unknown trait"),
        "diagnostic should name the unknown trait: {}",
        refusals[0].message
    );
}

/// The entry exit path renders a `dyn` error through `Display.fmt`'s own
/// slot, even when another bound declares a method named `fmt`.
#[test]
fn entry_dyn_error_renders_through_the_display_fmt_slot() {
    let output = typecheck(
        r#"
        trait Pretty { fn fmt(self) -> string; }
        type Failure { detail: string }
        impl Display for Failure { fn fmt(value: Failure) -> string { "display" } }
        impl Error for Failure {}
        impl Pretty for Failure { fn fmt(self) -> string { "pretty" } }
        fn main() -> Result<(), dyn (Pretty + Error)> {
            Err(Failure { detail: "x" })
        }
        "#,
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let Some(hew_types::EntryExitPlan {
        action:
            hew_types::EntryExitAction::Result {
                display: hew_types::EntryDisplayTarget::DynSlot { slot, method },
                ..
            },
        ..
    }) = &output.entry_exit_plan
    else {
        panic!(
            "expected a dyn entry exit plan: {:#?}",
            output.entry_exit_plan
        );
    };
    // `Pretty.fmt` occupies slot 3; `Display.fmt`, reached through `Error`,
    // is slot 4.
    assert_eq!(*slot, 4);
    assert_eq!(method.full_path(), "std.builtins.Display::fmt");
}
