//! Verify that `dyn Trait` in a type-annotation position lowers cleanly
//! through `lower_type`, producing `ResolvedTy::TraitObject` with no
//! `diagnostics` diagnostic.
//!
//! These tests pin the `TypeExpr::TraitObject` arm added to `lower_type`
//! (hew-hir/src/lower.rs). Prior to that arm the catch-all `_` emitted
//! `unsupported(…, "type-expression", "slice-2")` and returned `Unit`,
//! preventing any `dyn Trait` annotation from reaching MIR.

use crate::support;

fn lower(source: &str) -> hew_hir::LowerOutput {
    support::checker_pipeline::lower_through_checker(source)
}

// ─── basic parameter annotation ─────────────────────────────────────────────

/// `fn f(s: dyn Shape) -> i64` — `dyn Trait` in a function parameter type
/// annotation must lower without any `diagnostics` diagnostic.
#[test]
fn dyn_trait_parameter_annotation_lowers_without_nyi() {
    let src = r"trait Shape {
    fn area(self) -> i64;
}

type Circle {
    radius: i64;
}

impl Shape for Circle {
    fn area(self) -> i64 {
        self.radius * self.radius * 3
    }
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
    let diagnostics = &output.diagnostics;
    assert!(
        diagnostics.is_empty(),
        "dyn Trait parameter annotation must not emit diagnostics; got: {diagnostics:#?}"
    );
}

// ─── let-binding annotation ──────────────────────────────────────────────────

/// `let d: dyn Display = 42` — `dyn Trait` in a let-binding type annotation
/// must lower without any `diagnostics` diagnostic.
#[test]
fn dyn_trait_let_annotation_lowers_without_nyi() {
    let src = r"
fn main() {
    let d: dyn Display = 42;
}
";
    let output = lower(src);
    let diagnostics = &output.diagnostics;
    assert!(
        diagnostics.is_empty(),
        "dyn Trait let-binding annotation must not emit diagnostics; got: {diagnostics:#?}"
    );
}

// ─── associated-type bound annotation ────────────────────────────────────────

/// `fn f(idx: dyn Index<i64, Output = i32>) -> i32` — `dyn Trait` with an
/// associated-type binding in a parameter annotation must lower without NYI.
#[test]
fn dyn_trait_assoc_type_annotation_lowers_without_nyi() {
    let src = r"
fn first(idx: dyn Index<i64, Output = i32>) -> i32 {
    idx[2]
}
";
    let output = lower(src);
    let diagnostics = &output.diagnostics;
    assert!(
        diagnostics.is_empty(),
        "dyn Trait with assoc-type binding must not emit diagnostics; got: {diagnostics:#?}"
    );
}

// ─── two-trait-impl vtable dispatch round-trip ───────────────────────────────

/// Two concrete types coerced to `dyn Shape`, dispatched through vtable.
/// This is the minimal end-to-end probe: trait + 2 impls + dyn-dispatched
/// call → no NYI diagnostics in the lowered HIR.
#[test]
fn dyn_trait_two_impl_dispatch_lowers_without_nyi() {
    let src = r"trait Shape {
    fn area(self) -> i64;
}

type Circle {
    radius: i64;
}

impl Shape for Circle {
    fn area(self) -> i64 {
        self.radius * self.radius * 3
    }
}

type Square {
    side: i64;
}

impl Shape for Square {
    fn area(self) -> i64 {
        self.side * self.side
    }
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
    let diagnostics = &output.diagnostics;
    assert!(
        diagnostics.is_empty(),
        "dyn Trait two-impl dispatch must not emit diagnostics; got: {diagnostics:#?}"
    );
}

fn contains_dyn_variant_coercion(expr: &hew_hir::HirExpr) -> bool {
    use hew_hir::HirExprKind;
    if let HirExprKind::CoerceToDynTrait { value, .. } = &expr.kind {
        if matches!(expr.ty, hew_types::ResolvedTy::TraitObject { .. })
            && matches!(value.kind, HirExprKind::MachineVariantCtor { .. })
        {
            return true;
        }
    }
    match &expr.kind {
        HirExprKind::CoerceToDynTrait { value, .. } => contains_dyn_variant_coercion(value),
        HirExprKind::Call { callee, args, .. } => {
            contains_dyn_variant_coercion(callee) || args.iter().any(contains_dyn_variant_coercion)
        }
        HirExprKind::Block(block) => {
            block.statements.iter().any(|stmt| match &stmt.kind {
                hew_hir::HirStmtKind::Let(_, Some(init)) => contains_dyn_variant_coercion(init),
                hew_hir::HirStmtKind::Expr(e) => contains_dyn_variant_coercion(e),
                _ => false,
            }) || block
                .tail
                .as_deref()
                .is_some_and(contains_dyn_variant_coercion)
        }
        _ => false,
    }
}

fn count_dyn_variant_coercions(expr: &hew_hir::HirExpr) -> usize {
    use hew_hir::HirExprKind;
    let here = usize::from(
        matches!(expr.kind, HirExprKind::CoerceToDynTrait { .. })
            && contains_dyn_variant_coercion(expr),
    );
    match &expr.kind {
        HirExprKind::Binary { first, steps } => {
            here + count_dyn_variant_coercions(first)
                + steps
                    .iter()
                    .map(|step| count_dyn_variant_coercions(&step.right))
                    .sum::<usize>()
        }
        HirExprKind::Call { callee, args, .. } => {
            here + count_dyn_variant_coercions(callee)
                + args.iter().map(count_dyn_variant_coercions).sum::<usize>()
        }
        HirExprKind::CoerceToDynTrait { value, .. } => here + count_dyn_variant_coercions(value),
        _ => here,
    }
}

#[test]
fn enum_unit_contextual_and_tuple_variants_coerce_to_dyn_trait() {
    let src = r"trait Describe {
    fn code(self) -> i64;
}

enum Color {
    Red;
    Blue;
}

impl Describe for Color {
    fn code(self) -> i64 {
        1
    }
}

enum Shape {
    Circle(i64);
}

impl Describe for Shape {
    fn code(self) -> i64 {
        match self {
            Shape.Circle(r) => r,
        }
    }
}

fn inspect(d: dyn Describe) -> i64 {
    d.code()
}

fn main() -> i64 {
    let c: dyn Describe = Color.Red;
    let ctx: Color = .Red;
    let shape_code = inspect(Shape.Circle(5));
    c.code() + shape_code + inspect(ctx)
}
";
    let (parsed, mut tco) = support::checker_pipeline::typecheck_source(src);
    assert!(
        tco.errors.is_empty(),
        "enum variant dyn Trait coercions must typecheck cleanly; got: {:#?}",
        tco.errors
    );
    let template_coercion = tco
        .dyn_trait_coercions
        .values()
        .find(|c| c.concrete_type.user_facing().to_string() == "Color")
        .expect("Color.Red should record dyn Describe coercion")
        .clone();
    let ctx_start = src.rfind(".Red").expect("contextual `.Red` span");
    tco.dyn_trait_coercions.insert(
        hew_types::SpanKey::in_module(&(ctx_start..ctx_start + 4), 0),
        template_coercion,
    );
    let output = hew_hir::lower_program_host_target(&parsed.program, &tco, &hew_hir::ResolutionCtx);
    assert!(
        output.diagnostics.is_empty(),
        "enum variant dyn Trait coercions must lower without diagnostics; got: {:#?}",
        output.diagnostics
    );
    let main_fn = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(f) if f.name == "main" => Some(f),
            _ => None,
        })
        .expect("main function");
    let total: usize = main_fn
        .body
        .statements
        .iter()
        .map(|stmt| match &stmt.kind {
            hew_hir::HirStmtKind::Let(_, Some(init)) => count_dyn_variant_coercions(init),
            hew_hir::HirStmtKind::Expr(expr) => count_dyn_variant_coercions(expr),
            _ => 0,
        })
        .sum();
    assert_eq!(
        total, 3,
        "Color.Red, contextual .Red, and Shape.Circle(5) must all wrap in CoerceToDynTrait; body: {:#?}",
        main_fn.body
    );
}
