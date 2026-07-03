//! Verify that `dyn Trait` in a type-annotation position lowers cleanly
//! through `lower_type`, producing `ResolvedTy::TraitObject` with no
//! `NotYetImplemented` diagnostic.
//!
//! These tests pin the `TypeExpr::TraitObject` arm added to `lower_type`
//! (hew-hir/src/lower.rs). Prior to that arm the catch-all `_` emitted
//! `unsupported(…, "type-expression", "slice-2")` and returned `Unit`,
//! preventing any `dyn Trait` annotation from reaching MIR.

use crate::support;

use hew_hir::HirDiagnosticKind;

fn lower(source: &str) -> hew_hir::LowerOutput {
    support::checker_pipeline::lower_through_checker(source)
}

// ─── basic parameter annotation ─────────────────────────────────────────────

/// `fn f(s: dyn Shape) -> i64` — `dyn Trait` in a function parameter type
/// annotation must lower without any `NotYetImplemented` diagnostic.
#[test]
fn dyn_trait_parameter_annotation_lowers_without_nyi() {
    let src = r"
trait Shape {
    fn area(val: Self) -> i64;
}

type Circle { radius: i64; }
impl Shape for Circle {
    fn area(c: Circle) -> i64 { c.radius * c.radius * 3 }
}

fn measure(s: dyn Shape) -> i64 {
    s.area()
}

fn main() -> i64 {
    let c = Circle { radius: 5 };
    measure(c)
}
";
    let output = lower(src);
    let nyi: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| matches!(&d.kind, HirDiagnosticKind::NotYetImplemented { .. }))
        .collect();
    assert!(
        nyi.is_empty(),
        "dyn Trait parameter annotation must not emit NotYetImplemented; got: {nyi:#?}"
    );
}

// ─── let-binding annotation ──────────────────────────────────────────────────

/// `let d: dyn Display = 42` — `dyn Trait` in a let-binding type annotation
/// must lower without any `NotYetImplemented` diagnostic.
#[test]
fn dyn_trait_let_annotation_lowers_without_nyi() {
    let src = r"
fn main() {
    let d: dyn Display = 42;
}
";
    let output = lower(src);
    let nyi: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| matches!(&d.kind, HirDiagnosticKind::NotYetImplemented { .. }))
        .collect();
    assert!(
        nyi.is_empty(),
        "dyn Trait let-binding annotation must not emit NotYetImplemented; got: {nyi:#?}"
    );
}

// ─── associated-type bound annotation ────────────────────────────────────────

/// `fn f(idx: dyn Index<Output = i32>) -> i32` — `dyn Trait` with an
/// associated-type binding in a parameter annotation must lower without NYI.
#[test]
fn dyn_trait_assoc_type_annotation_lowers_without_nyi() {
    let src = r"
fn first(idx: dyn Index<Output = i32>) -> i32 {
    idx[2]
}
";
    let output = lower(src);
    let nyi: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| matches!(&d.kind, HirDiagnosticKind::NotYetImplemented { .. }))
        .collect();
    assert!(
        nyi.is_empty(),
        "dyn Trait with assoc-type binding must not emit NotYetImplemented; got: {nyi:#?}"
    );
}

// ─── two-trait-impl vtable dispatch round-trip ───────────────────────────────

/// Two concrete types coerced to `dyn Shape`, dispatched through vtable.
/// This is the minimal end-to-end probe: trait + 2 impls + dyn-dispatched
/// call → no NYI diagnostics in the lowered HIR.
#[test]
fn dyn_trait_two_impl_dispatch_lowers_without_nyi() {
    let src = r"
trait Shape {
    fn area(val: Self) -> i64;
}

type Circle { radius: i64; }
impl Shape for Circle {
    fn area(c: Circle) -> i64 { c.radius * c.radius * 3 }
}

type Square { side: i64; }
impl Shape for Square {
    fn area(s: Square) -> i64 { s.side * s.side }
}

fn measure(s: dyn Shape) -> i64 {
    s.area()
}

fn main() -> i64 {
    let c = Circle { radius: 2 };
    let s = Square { side: 3 };
    measure(c) + measure(s)
}
";
    let output = lower(src);
    let nyi: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| matches!(&d.kind, HirDiagnosticKind::NotYetImplemented { .. }))
        .collect();
    assert!(
        nyi.is_empty(),
        "dyn Trait two-impl dispatch must not emit NotYetImplemented; got: {nyi:#?}"
    );
}
