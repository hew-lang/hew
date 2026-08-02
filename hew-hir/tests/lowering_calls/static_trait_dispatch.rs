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

use hew_hir::{
    dump_hir, lower_program_host_target, HirExpr, HirExprKind, HirItem, HirStmtKind, ResolutionCtx,
};
use hew_parser::ast::{Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModuleId};
use hew_types::{module_registry::ModuleRegistry, CallTarget, Checker};

fn lower(source: &str) -> hew_hir::LowerOutput {
    support::checker_pipeline::lower_through_checker(source)
}

fn typecheck(source: &str) -> hew_types::TypeCheckOutput {
    let (_, tco) = support::checker_pipeline::typecheck_source(source);
    tco
}

/// Construct a real three-module program with two imported modules whose full
/// paths may share the same final component. The root's imports carry
/// parser-produced items, while the graph gives the checker/lowerer the module
/// ownership context that qualifies the declarations.
fn multi_module_program(root_src: &str, modules: &[(&str, &str)]) -> Program {
    let root = hew_parser::parse(root_src);
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:#?}",
        root.errors
    );
    let root_id = ModuleId::root();
    let mut graph = ModuleGraph::new(root_id.clone());
    let mut source_items = std::collections::HashMap::new();

    for (name, source) in modules {
        let parsed = hew_parser::parse(source);
        assert!(
            parsed.errors.is_empty(),
            "{name} parse errors: {:#?}",
            parsed.errors
        );
        let items: Vec<_> = parsed
            .program
            .items
            .iter()
            .filter(|(item, _)| !matches!(item, Item::Import(_)))
            .cloned()
            .collect();
        let id = ModuleId::new(name.split("::").map(String::from).collect());
        graph
            .add_module(Module {
                id: id.clone(),
                items: items.clone(),
                imports: Vec::new(),
                source_paths: Vec::new(),
                doc: None,
            })
            .expect("unique imported module");
        graph.topo_order.push(id);
        source_items.insert((*name).to_string(), items);
    }

    let mut root_items = root.program.items.clone();
    for (item, _) in &mut root_items {
        if let Item::Import(import) = item {
            let full_path = import.path.join("::");
            if let Some(items) = source_items.get(&full_path) {
                import.resolved_items = Some(items.clone());
            }
        }
    }
    graph
        .add_module(Module {
            id: root_id,
            items: root_items.clone(),
            imports: Vec::new(),
            source_paths: Vec::new(),
            doc: None,
        })
        .expect("root module");
    Program {
        items: root_items,
        module_graph: Some(graph),
        ..root.program
    }
}

#[allow(
    deprecated,
    reason = "the assertion reads the legacy static-dispatch carrier"
)]
fn walk_calls(expr: &HirExpr, calls: &mut Vec<CallTarget>) {
    match &expr.kind {
        HirExprKind::Call {
            target,
            callee,
            args,
            ..
        } => {
            calls.push(target.clone());
            walk_calls(callee, calls);
            for arg in args {
                walk_calls(arg, calls);
            }
        }
        HirExprKind::Block(block) => {
            for stmt in &block.statements {
                if let HirStmtKind::Expr(expr) | HirStmtKind::Let(_, Some(expr)) = &stmt.kind {
                    walk_calls(expr, calls);
                }
            }
            if let Some(tail) = &block.tail {
                walk_calls(tail, calls);
            }
        }
        HirExprKind::CallTraitMethodStatic { target, .. } => calls.push(target.clone()),
        _ => {}
    }
}

fn walk_block_calls(block: &hew_hir::HirBlock, calls: &mut Vec<CallTarget>) {
    for stmt in &block.statements {
        if let HirStmtKind::Expr(expr) | HirStmtKind::Let(_, Some(expr)) = &stmt.kind {
            walk_calls(expr, calls);
        }
    }
    if let Some(tail) = &block.tail {
        walk_calls(tail, calls);
    }
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "the cross-module identity regression keeps setup, lowering, and every same-leaf assertion together"
)]
fn e2e_multi_module_same_leaf_generic_and_specialized_dispatch_stays_canonical() {
    let program = multi_module_program(
        r"
import left::render::{Render as LeftRender, Box as LeftBox, identity as left_identity};
import right::render::{Render as RightRender, Box as RightBox, identity as right_identity};

fn use_left<T: LeftRender>(value: T) -> string { value.render() }
fn use_right<T: RightRender>(value: T) -> string { value.render() }
fn main() -> string {
    let a = LeftBox<i64> { value: 1 };
    let b = RightBox<bool> { value: true };
    let left_direct = left_identity();
    let right_direct = right_identity();
    let ignored = use_left(a);
    use_right(b)
}
",
        &[
            (
                "left::render",
                r#"
pub trait Render {
    fn render(value: Self) -> string;
}
pub type Box<T> { value: T; }
pub fn identity() -> string { "left-direct" }
impl<T> Render for Box<T> {
    fn render(value: Box<T>) -> string { "left-generic" }
}
impl Render for Box<i64> {
    fn render(value: Box<i64>) -> string { "left-i64" }
}
"#,
            ),
            (
                "right::render",
                r#"
pub trait Render {
    fn render(value: Self) -> string;
}
pub type Box<T> { value: T; }
pub fn identity() -> string { "right-direct" }
impl<T> Render for Box<T> {
    fn render(value: Box<T>) -> string { "right-generic" }
}
impl Render for Box<string> {
    fn render(value: Box<string>) -> string { "right-string" }
}
"#,
            ),
        ],
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    assert!(
        tco.impl_method_declaration_ids
            .contains_key("left.render.Box$$i64::render"),
        "checker must publish the exact emitted specialized impl symbol; keys: {:#?}",
        tco.impl_method_declaration_ids.keys().collect::<Vec<_>>()
    );
    let output = lower_program_host_target(&program, &tco, &ResolutionCtx);
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );

    let mut static_targets = Vec::new();
    for item in &output.module.items {
        let HirItem::Function(function) = item else {
            continue;
        };
        if matches!(function.name.as_str(), "use_left" | "use_right") {
            walk_block_calls(&function.body, &mut static_targets);
        }
    }
    let mut identities: Vec<_> = static_targets
        .into_iter()
        .filter_map(|target| match target {
            CallTarget::StaticTraitMethod {
                declaring_trait,
                method,
            } => Some((
                declaring_trait.full_path().to_string(),
                method.full_path().to_string(),
            )),
            _ => None,
        })
        .collect();
    identities.sort();
    assert_eq!(
        identities,
        vec![
            (
                "left.render.Render".to_string(),
                "left.render.Render::render".to_string()
            ),
            (
                "right.render.Render".to_string(),
                "right.render.Render::render".to_string()
            ),
        ],
        "the typechecker-selected static-call targets must preserve module ownership"
    );

    let mut main_targets = Vec::new();
    for item in &output.module.items {
        let HirItem::Function(function) = item else {
            continue;
        };
        if function.name == "main" {
            walk_block_calls(&function.body, &mut main_targets);
        }
    }
    let direct_identities: Vec<_> = main_targets
        .into_iter()
        .filter_map(|target| match target {
            CallTarget::User(id) => Some(id.full_path().to_string()),
            _ => None,
        })
        .collect();
    let left_direct = direct_identities
        .iter()
        .find(|identity| identity.as_str() == "left.render.identity")
        .expect("left imported alias must retain its source declaration identity");
    let right_direct = direct_identities
        .iter()
        .find(|identity| identity.as_str() == "right.render.identity")
        .expect("right imported alias must retain its source declaration identity");
    assert_ne!(
        left_direct, right_direct,
        "same-leaf imported functions must retain distinct source identities"
    );

    let index = hew_hir::dispatch::build_trait_impl_method_index(&output.module.items);
    let left_specialized = index
        .iter()
        .find(|(key, _)| {
            key.declaring_trait.full_path() == "left.render.Render"
                && key.self_type.nominal.declaration().full_path() == "left.render.Box"
                && key.self_type.args == vec![hew_types::ResolvedTy::I64]
        })
        .expect("left.render's i64 specialization must be indexed structurally");
    let right_generic = index
        .iter()
        .find(|(key, _)| {
            key.declaring_trait.full_path() == "right.render.Render"
                && key.self_type.nominal.declaration().full_path() == "right.render.Box"
                && key.self_type.args.is_empty()
        })
        .expect("right.render's generic impl must be indexed structurally");
    assert_ne!(
        left_specialized.0.declaring_trait, right_generic.0.declaring_trait,
        "same leaf trait names from different modules must not collide"
    );
    assert_ne!(
        left_specialized.0.method, right_generic.0.method,
        "same leaf method names from different modules must not collide"
    );
    assert_ne!(
        left_specialized.0.self_type.nominal, right_generic.0.self_type.nominal,
        "same leaf nominal names from different modules must not collide"
    );

    let left_selected = hew_hir::dispatch::lookup_trait_impl_entry_by_id(
        &index,
        &left_specialized.0.declaring_trait,
        &hew_types::NominalInstance {
            nominal: left_specialized.0.self_type.nominal.clone(),
            args: vec![hew_types::ResolvedTy::I64],
        },
        &left_specialized.0.method,
    )
    .expect("left i64 dispatch must resolve its specialization");
    assert_eq!(
        left_specialized.0.method.full_path(),
        "left.render.Render::render",
        "the static registry key must use the checker-selected trait declaration identity"
    );
    assert_eq!(
        left_selected.method.full_path(),
        "left.render.Box::<impl left.render.Render for left.render.Box<i64>>::render",
        "the emitted body keeps its distinct checker implementation declaration identity: {left_selected:?}"
    );
    assert_ne!(
        left_specialized.0.method, left_selected.method,
        "static lookup must not conflate a trait method declaration with an implementation declaration"
    );
    assert_eq!(
        left_selected.method_symbol,
        left_specialized.1.method_symbol
    );
    assert!(
        left_selected.impl_type_params.is_empty(),
        "left i64 dispatch must select the specialized impl: {left_selected:?}"
    );

    let right_selected = hew_hir::dispatch::lookup_trait_impl_entry_by_id(
        &index,
        &right_generic.0.declaring_trait,
        &hew_types::NominalInstance {
            nominal: right_generic.0.self_type.nominal.clone(),
            args: vec![hew_types::ResolvedTy::Bool],
        },
        &right_generic.0.method,
    )
    .expect("right bool dispatch must fall back to its generic impl");
    assert_eq!(right_selected.method_symbol, right_generic.1.method_symbol);
    assert_eq!(
        right_selected.impl_type_params,
        vec!["T".to_string()],
        "right bool dispatch must select the generic impl"
    );
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

// ─── V15: Generic static dispatch keeps canonical monomorph identity ────────

#[test]
fn v15_static_dispatch_monomorphization_keeps_canonical_owner_and_typed_args() {
    // `display<T: Show>` is instantiated with the exact root nominal
    // `Wrapper<i64>`. The registry authority is the outer declaration's
    // ItemId plus this typed argument spine; it must not manufacture the
    // legacy leaf-derived `Wrapper::show` string as a second identity.
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
    let expected_args = vec![hew_types::ResolvedTy::named_user(
        "Wrapper",
        vec![hew_types::ResolvedTy::I64],
    )];
    let display_mono = output
        .module
        .monomorphisations
        .iter()
        .find(|mono| mono.key.linker_symbol == "display")
        .expect("expected canonical `display<Wrapper<i64>>` monomorphisation");
    assert_eq!(display_mono.key.type_args, expected_args);
    assert_eq!(display_mono.mangled_name, "display$$Wrapper$li64$g");
    assert!(
        output
            .module
            .monomorphisations
            .iter()
            .all(|mono| mono.key.declaration.full_path() != "Wrapper::show"),
        "static dispatch must retain the checker implementation declaration rather than a leaf-derived `Wrapper::show` identity: {:#?}",
        output.module.monomorphisations
    );
}
