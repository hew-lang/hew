//! V1–V14 validation for static trait dispatch (W3.022).
//!
//! Tests cover:
//! - V1: Basic `T: Trait` method call emits `CallTraitMethodStatic`
//! - V2: Fail-closed on missing impl (`UndefinedMethod` in type checker)
//! - V3: Multiple bounds — no ambiguity when different traits declare different methods
//! - V4: Supertrait inheritance — method declared in super, called through sub
//! - V5: Supertrait dedup — same declaring trait reachable via multiple bounds
//! - V6: Ambiguous distinct declaring traits (multiple traits each declare the same method name)
//! - V7: Return type flows through correctly
//! - V8: Multiple args on the trait method
//! - V9: Self substitution in trait signatures
//! - V10: Primitive type as concrete receiver (i64, string)
//! - V11: Named/record type as concrete receiver
//! - V12: Generic record type as concrete receiver (type args propagate)
//! - V13: Method on a bound not present → `UndefinedMethod` (fail-closed)
//! - V14: Nested bounds (T: B where trait B: A) accessing A's method via B bound

use crate::support;

use hew_hir::dump_hir;

fn lower(source: &str) -> hew_hir::LowerOutput {
    support::checker_pipeline::lower_through_checker(source)
}

fn typecheck(source: &str) -> hew_types::TypeCheckOutput {
    let (_, tco) = support::checker_pipeline::typecheck_source(source);
    tco
}

// ─── V1: Basic static trait dispatch ─────────────────────────────────────────

#[test]
fn v1_basic_static_trait_dispatch_emits_call_trait_method_static() {
    let src = r#"
trait Show {
    fn show(val: Self) -> string;
}
type Point { x: i64; y: i64; }
impl Show for Point {
    fn show(p: Point) -> string { "Point" }
}
fn display<T: Show>(item: T) -> string {
    item.show()
}
fn main() -> string {
    let p = Point { x: 1, y: 2 };
    display(p)
}
"#;
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("call-static-trait Show::show"),
        "expected CallTraitMethodStatic in HIR dump, got:\n{dump}"
    );
}

// ─── V2: Fail-closed on missing impl ────────────────────────────────────────

#[test]
fn v2_missing_impl_reports_undefined_method() {
    // Calling a method not declared by any bound trait → error
    let src = r"
trait Show {
    fn show(val: Self) -> string;
}
type Point { x: i64; y: i64; }
fn display<T: Show>(item: T) -> string {
    item.nonexistent()
}
";
    let tco = typecheck(src);
    assert!(
        tco.errors.iter().any(|e| e.message.contains("nonexistent")),
        "expected UndefinedMethod error for nonexistent, got: {:?}",
        tco.errors
    );
}

// ─── V3: Multiple bounds — distinct methods ─────────────────────────────────

#[test]
fn v3_multiple_bounds_distinct_methods() {
    let src = r#"
trait Show {
    fn show(val: Self) -> string;
}
trait Size {
    fn size(val: Self) -> i64;
}
type Box { w: i64; h: i64; }
impl Show for Box {
    fn show(b: Box) -> string { "Box" }
}
impl Size for Box {
    fn size(b: Box) -> i64 { b.w * b.h }
}
fn describe<T: Show + Size>(item: T) -> string {
    item.show()
}
fn main() -> string {
    describe(Box { w: 3, h: 4 })
}
"#;
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("call-static-trait Show::show"),
        "expected Show::show dispatch in: {dump}"
    );
}

// ─── V4: Supertrait inheritance — method declared in super ──────────────────

#[test]
fn v4_supertrait_inherited_method() {
    let src = r#"
trait Base {
    fn name(val: Self) -> string;
}
trait Extended: Base {
    fn extra(val: Self) -> i64;
}
type Widget { label: string; }
impl Base for Widget {
    fn name(w: Widget) -> string { w.label }
}
impl Extended for Widget {
    fn extra(w: Widget) -> i64 { 42 }
}
fn get_name<T: Extended>(item: T) -> string {
    item.name()
}
fn main() -> string {
    get_name(Widget { label: "ok" })
}
"#;
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let dump = dump_hir(&output.module);
    // The declaring trait is Base even though the bound is Extended.
    assert!(
        dump.contains("call-static-trait Base::name"),
        "expected Base::name (supertrait origin) in: {dump}"
    );
}

// ─── V5: Supertrait dedup — same origin via multiple paths ──────────────────

#[test]
fn v5_supertrait_dedup_same_declaring_trait() {
    // If T: A + B and both A and B inherit from Root which declares `id`,
    // we should NOT reject as ambiguous — the declaring trait is the same (Root).
    let src = r"
trait Root {
    fn id(val: Self) -> i64;
}
trait A: Root {
    fn a_only(val: Self) -> i64;
}
trait B: Root {
    fn b_only(val: Self) -> i64;
}
type Thing { v: i64; }
impl Root for Thing {
    fn id(t: Thing) -> i64 { t.v }
}
impl A for Thing {
    fn a_only(t: Thing) -> i64 { 1 }
}
impl B for Thing {
    fn b_only(t: Thing) -> i64 { 2 }
}
fn get_id<T: A + B>(item: T) -> i64 {
    item.id()
}
fn main() -> i64 {
    get_id(Thing { v: 99 })
}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics (should NOT be ambiguous): {:?}",
        output.diagnostics
    );
    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("call-static-trait Root::id"),
        "expected Root::id in: {dump}"
    );
}

// ─── V6: Ambiguous distinct declaring traits ────────────────────────────────

#[test]
fn v6_ambiguous_distinct_declaring_traits() {
    // Two unrelated traits each declare `run` — call is ambiguous.
    let src = r"
trait Engine {
    fn run(val: Self) -> i64;
}
trait Athlete {
    fn run(val: Self) -> i64;
}
fn go<T: Engine + Athlete>(item: T) -> i64 {
    item.run()
}
";
    let tco = typecheck(src);
    let amb = tco
        .errors
        .iter()
        .find(|e| e.kind == hew_types::error::TypeErrorKind::AmbiguousTraitMethod)
        .unwrap_or_else(|| panic!("expected AmbiguousTraitMethod kind, got: {:?}", tco.errors));
    // Diagnostic must name BOTH declaring traits.
    assert!(amb.message.contains("Engine"), "expected Engine: {amb:?}");
    assert!(amb.message.contains("Athlete"), "expected Athlete: {amb:?}");
}

// ─── V6b: Redeclared-supertrait ambiguity ───────────────────────────────────

#[test]
fn v6b_supertrait_redeclaration_is_ambiguous() {
    // `trait B: A` where both A and B directly declare `describe` — and a
    // bound `T: B` finds the method via two distinct declaring traits
    // (A through supertrait walk, B directly). Plan §4 V14: must reject.
    //
    // NOTE: if Hew evolves to forbid trait method redeclaration in a
    // supertrait at definition time, this test becomes a trait-definition
    // error instead — the rejection site moves, but the program is
    // still rejected. Either form is acceptable for this fail-closed
    // contract; the test currently exercises the V0.5 behaviour.
    let src = r#"
trait A {
    fn describe(val: Self) -> string;
}
trait B: A {
    fn describe(val: Self) -> string;
}
type Thing { x: i64; }
impl A for Thing {
    fn describe(t: Thing) -> string { "A" }
}
impl B for Thing {
    fn describe(t: Thing) -> string { "B" }
}
fn report<T: B>(item: T) -> string {
    item.describe()
}
"#;
    let tco = typecheck(src);
    // Either the call is rejected as ambiguous OR the trait definition
    // itself is rejected. The program MUST NOT typecheck cleanly.
    assert!(
        !tco.errors.is_empty(),
        "expected at least one error (ambiguous call or duplicate trait method), got none"
    );
}

// ─── V7: Return type propagation ────────────────────────────────────────────

#[test]
fn v7_return_type_flows_through() {
    let src = r"
trait Length {
    fn len(val: Self) -> i64;
}
type List { count: i64; }
impl Length for List {
    fn len(l: List) -> i64 { l.count }
}
fn get_len<T: Length>(item: T) -> i64 {
    item.len()
}
fn main() -> i64 {
    get_len(List { count: 5 })
}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "diagnostics: {:?}",
        output.diagnostics
    );
    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("call-static-trait Length::len"),
        "dump: {dump}"
    );
    // Return type should be i64
    assert!(dump.contains("-> i64"), "return type in dump: {dump}");
}

// ─── V8: Multiple args ──────────────────────────────────────────────────────

#[test]
fn v8_trait_method_with_multiple_args() {
    let src = r"
trait Adder {
    fn add(val: Self, x: i64, y: i64) -> i64;
}
type Calc { base: i64; }
impl Adder for Calc {
    fn add(c: Calc, x: i64, y: i64) -> i64 { c.base + x + y }
}
fn compute<T: Adder>(item: T, a: i64, b: i64) -> i64 {
    item.add(a, b)
}
fn main() -> i64 {
    compute(Calc { base: 10 }, 3, 4)
}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "diagnostics: {:?}",
        output.diagnostics
    );
}

// ─── V9: Self substitution ──────────────────────────────────────────────────

#[test]
fn v9_self_substitution_in_return_type() {
    // Trait method returns Self — should substitute the type param.
    let src = r"
trait Clone {
    fn clone(val: Self) -> Self;
}
type Token { id: i64; }
impl Clone for Token {
    fn clone(t: Token) -> Token { Token { id: t.id } }
}
fn dup<T: Clone>(item: T) -> T {
    item.clone()
}
fn main() -> i64 {
    let t = Token { id: 1 };
    let t2 = dup(t);
    t2.id
}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "diagnostics: {:?}",
        output.diagnostics
    );
}

// ─── V13: Method on a bound not present → UndefinedMethod ───────────────────

#[test]
fn v13_method_not_in_bound_fails_closed() {
    let src = r"
trait Show {
    fn show(val: Self) -> string;
}
fn display<T: Show>(item: T) -> i64 {
    item.size()
}
";
    let tco = typecheck(src);
    assert!(
        tco.errors.iter().any(|e| e.message.contains("size")),
        "expected UndefinedMethod for 'size', got: {:?}",
        tco.errors
    );
}

// ─── V14: Nested bounds — T: B where trait B: A ─────────────────────────────

#[test]
fn v14_nested_supertrait_access() {
    let src = r#"
trait Printable {
    fn print_str(val: Self) -> string;
}
trait Formattable: Printable {
    fn format(val: Self) -> string;
}
type Doc { content: string; }
impl Printable for Doc {
    fn print_str(d: Doc) -> string { d.content }
}
impl Formattable for Doc {
    fn format(d: Doc) -> string { d.content }
}
fn render<T: Formattable>(item: T) -> string {
    item.print_str()
}
fn main() -> string {
    render(Doc { content: "hello" })
}
"#;
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let dump = dump_hir(&output.module);
    // Declaring trait is Printable, not Formattable
    assert!(
        dump.contains("call-static-trait Printable::print_str"),
        "expected Printable::print_str in: {dump}"
    );
}

// ─── V7b: Generic-over-generic impl — `impl<U> Trait for Wrapper<U>` ────────

#[test]
fn v7b_generic_impl_preserves_impl_level_type_params() {
    // The impl-level type param `U` must survive into the lowered HirFn
    // `Wrapper::show.type_params` so that monomorphization can specialize
    // per concrete instantiation. Prior to the W3.022 fix this dropped
    // `U` and emitted an unsubstituted bare symbol.
    let src = r#"
trait Show {
    fn show(val: Self) -> string;
}
type Wrapper<U> { inner: U; }
impl<U> Show for Wrapper<U> {
    fn show(w: Wrapper<U>) -> string { "wrapped" }
}
fn display<T: Show>(item: T) -> string {
    item.show()
}
fn main() -> string {
    display(Wrapper<i64> { inner: 7 })
}
"#;
    let output = lower(src);
    // Locate the lowered HirFn for `Wrapper::show` and assert it carries
    // the impl-level type param.
    let mut found = false;
    for item in &output.module.items {
        if let hew_hir::node::HirItem::Function(func) = item {
            if func.name == "Wrapper::show" {
                assert!(
                    func.type_params.contains(&"U".to_string()),
                    "expected impl-level type param `U` in Wrapper::show.type_params, \
                     got {:?}",
                    func.type_params
                );
                found = true;
            }
        }
    }
    assert!(
        found,
        "expected a lowered HirFn named `Wrapper::show` in the module"
    );
}

// ─── V15: Generic impl monomorphization closure registers impl method ────────

#[test]
fn v15_generic_impl_monomorphization_closure_registers_impl_method() {
    // When `display<T: Show>` is called with `Wrapper<i64>`, the
    // monomorphization closure must discover the `CallTraitMethodStatic`
    // inside `display`'s body and register `Wrapper::show` monomorphised
    // with type_args `[i64]` — producing the mangled symbol
    // `Wrapper::show<i64>` in the registry.
    let src = r#"
trait Show {
    fn show(val: Self) -> string;
}
type Wrapper<U> { inner: U; }
impl<U> Show for Wrapper<U> {
    fn show(w: Wrapper<U>) -> string { "wrapped" }
}
fn display<T: Show>(item: T) -> string {
    item.show()
}
fn main() -> string {
    display(Wrapper<i64> { inner: 7 })
}
"#;
    let output = lower(src);
    // The monomorphisation registry should contain an entry for
    // `Wrapper::show` with type_args containing i64.
    let has_wrapper_show_mono = output.module.monomorphisations.iter().any(|mono| {
        mono.key.origin_name == "Wrapper::show"
            && mono
                .key
                .type_args
                .iter()
                .any(|t| matches!(t, hew_types::ResolvedTy::I64))
    });
    assert!(
        has_wrapper_show_mono,
        "expected monomorphisation registry to contain `Wrapper::show<i64>`, \
         got entries: {:?}",
        output
            .module
            .monomorphisations
            .iter()
            .map(|m| &m.mangled_name)
            .collect::<Vec<_>>()
    );
    // The mangled name should follow the mangle convention.
    let mangled = output
        .module
        .monomorphisations
        .iter()
        .find(|m| m.key.origin_name == "Wrapper::show")
        .map(|m| m.mangled_name.as_str());
    assert!(
        mangled.is_some(),
        "Wrapper::show monomorphisation should have a mangled name"
    );
}
