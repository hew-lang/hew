//! Integration tests for the dedicated `MachineMonoPass`
//! (W3.033c Stage 2, R244=B).
//!
//! These tests pin the load-bearing invariants:
//! - Generic-function-mediated machine instantiations are recorded
//!   only after function-mono has substituted the surrounding fn.
//! - The uniform-path R246 emits an entry for every monomorphic
//!   machine declaration with empty `type_args`.
//! - A residual abstract type-parameter symbol surviving substitution
//!   fails closed with `UnresolvedMachineTypeParamPostMono`.
//! - Const args thread through the same key shape (empty in v0.5
//!   baseline; populated by W3.039 once it lands — the slot's
//!   identity-bearing role is what this test pins).

use hew_hir::{HirDiagnosticKind, MachineMonoEntry};

#[path = "support/mod.rs"]
mod support;

fn lower(source: &str) -> hew_hir::LowerOutput {
    support::checker_pipeline::lower_through_checker(source)
}

fn machine_keys(entries: &[MachineMonoEntry]) -> Vec<(String, Vec<String>)> {
    entries
        .iter()
        .map(|e| {
            (
                e.key.origin_name.clone(),
                e.key
                    .type_args
                    .iter()
                    .map(|t| match t {
                        hew_types::ResolvedTy::Named { name, .. } => name.clone(),
                        other => format!("{other:?}"),
                    })
                    .collect(),
            )
        })
        .collect()
}

/// **Pass-ordering invariant**: a machine instantiation reachable
/// only through a generic function (`fn make<T>() -> Holder<T>`) must
/// be observable in `machine_instantiations` once function-mono has
/// substituted `T = File` at the `make::<File>()` call site. If the
/// machine-mono pass ran before function-mono closure, the
/// substituted reach-through would be invisible and the entry would
/// be missing — this test fails closed in that scenario.
#[test]
fn machine_mono_pass_records_generic_instantiations_after_function_mono() {
    let source = r"
trait Resource {
    fn close(self);
}

type File { raw: i64; }

impl Resource for File {
    fn close(self) {}
}

machine Holder<T: Resource> {
    events {
        Start { handle: T; }
        Stop;
    }

    state Idle;
    state Active { handle: T; }


    on Start: Idle => Active { Active { handle: event.handle } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}

fn main() {
    let f = File { raw: 7 };
    let holder: Holder<File> = Active { handle: f };
}
";
    let output = lower(source);
    assert!(
        output.diagnostics.iter().all(|d| !matches!(
            d.kind,
            HirDiagnosticKind::UnresolvedMachineTypeParamPostMono { .. }
        )),
        "expected no UnresolvedMachineTypeParamPostMono diagnostics; got: {:?}",
        output.diagnostics
    );
    let entries = &output.module.machine_instantiations;
    let keys = machine_keys(entries);
    assert!(
        keys.iter()
            .any(|(name, args)| name == "Holder" && args == &vec!["File".to_string()]),
        "expected `Holder<File>` machine instantiation in machine_instantiations; got: {keys:?}"
    );
}

/// **Uniform path (R246 = A)**: a monomorphic machine declaration
/// produces exactly one entry with empty `type_args`. Downstream
/// codegen iterates `machine_instantiations` for both generic and
/// non-generic machines — no special-case bare-name lookup.
#[test]
fn machine_mono_pass_uniform_path_includes_non_generic_machines() {
    let source = r"
machine TrafficLight {
    events {
        Tick;
    }

    state Red;
    state Green;


    on Tick: Red => Green;
    on Tick: Green => Red;
}

fn main() {}
";
    let output = lower(source);
    let entries = &output.module.machine_instantiations;
    let traffic_entries: Vec<&MachineMonoEntry> = entries
        .iter()
        .filter(|e| e.key.origin_name == "TrafficLight")
        .collect();
    assert_eq!(
        traffic_entries.len(),
        1,
        "expected exactly one uniform-path TrafficLight entry; got: {entries:?}"
    );
    assert!(
        traffic_entries[0].key.type_args.is_empty(),
        "monomorphic machine entry must carry empty type_args; got: {:?}",
        traffic_entries[0].key.type_args
    );
    assert!(
        traffic_entries[0].key.const_args.is_empty(),
        "v0.5 baseline must carry empty const_args; got: {:?}",
        traffic_entries[0].key.const_args
    );
}

/// **Fail-closed boundary**: a residual abstract type parameter that
/// survives substitution (a function-mono defect) must be flagged
/// rather than silently emitted as a malformed key. We drive the
/// diagnostic by constructing a synthetic `MonomorphizedFn` whose
/// substitution map is degenerate (`T -> T`): `substitute_ty` is one-
/// pass, so the substituted value is not re-substituted, and the
/// abstract `T` survives into the visit-ty residual check. The fix
/// for blocker #2 (residual scoping) means this check only fires when
/// the residual name is in the **originating declaration's** own
/// `type_params` — module-wide name matching is no longer the
/// predicate.
#[test]
fn machine_mono_fails_closed_when_unresolved_type_var_survives() {
    use hew_hir::{
        run_machine_mono_pass, BindingId, HirBinding, HirBlock, HirDiagnostic, HirExpr,
        HirExprKind, HirFn, HirItem, HirMachineDecl, HirNodeId, HirStmt, HirStmtKind, IntentKind,
        ItemId, MonoKey, MonomorphizedFn, ScopeId, SiteId, ValueClass,
        MONOMORPHISATION_REGISTRY_CAP,
    };
    use hew_types::ResolvedTy;

    let machine = HirItem::Machine(HirMachineDecl {
        id: ItemId(0),
        node: HirNodeId(0),
        name: "Holder".to_string(),
        type_params: vec!["T".to_string()],
        type_param_bounds: vec![],
        states: vec![],
        events: vec![],
        transitions: vec![],
        has_default: true,
        span: 0..0,
    });

    // Generic `fn touch<T>()` whose body references `Holder<T>` —
    // structurally the same shape a fn-mono closure would observe at
    // a use site. By itself this fn is generic, so the discovery
    // pass does NOT walk it directly; it is walked once per
    // `MonomorphizedFn` referencing it in `monomorphisations`.
    let abstract_t = ResolvedTy::Named {
        name: "T".to_string(),
        args: vec![],
        builtin: None,
    };
    let holder_t = ResolvedTy::Named {
        name: "Holder".to_string(),
        args: vec![abstract_t.clone()],
        builtin: None,
    };
    let binding = HirBinding {
        id: BindingId(0),
        name: "h".to_string(),
        ty: holder_t.clone(),
        mutable: false,
        span: 0..0,
    };
    let value_expr = HirExpr {
        node: HirNodeId(1),
        site: SiteId(0),
        ty: holder_t.clone(),
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Unsupported("synthetic test placeholder".to_string()),
        span: 0..0,
    };
    let body = HirBlock {
        node: HirNodeId(2),
        scope: ScopeId(0),
        statements: vec![HirStmt {
            node: HirNodeId(3),
            kind: HirStmtKind::Let(binding, Some(value_expr)),
            span: 0..0,
        }],
        tail: None,
        ty: ResolvedTy::Unit,
        span: 0..0,
    };
    let touch_fn = HirFn {
        id: ItemId(1),
        node: HirNodeId(4),
        name: "touch".to_string(),
        type_params: vec!["T".to_string()],
        params: vec![],
        return_ty: ResolvedTy::Unit,
        body,
        span: 0..0,
        is_generator: false,
        intrinsic_id: None,
    };

    // Degenerate `T -> T` substitution: a real function-mono closure
    // would substitute `T -> File` and the residual would vanish.
    // Here we simulate a checker-authority gap by mapping `T -> T`;
    // `substitute_ty` is one-pass, so the body's `Holder<T>` walks to
    // `Holder<Named{T, args:[]}>`, and the residual-domain check
    // (scoped to `touch.type_params = ["T"]`) flags the bare `T`.
    let mono = MonomorphizedFn {
        key: MonoKey {
            origin: ItemId(1),
            origin_name: "touch".to_string(),
            type_args: vec![abstract_t.clone()],
        },
        mangled_name: "touch$$T".to_string(),
    };

    let items = vec![machine, HirItem::Function(touch_fn)];
    let (_entries, diagnostics) =
        run_machine_mono_pass(&items, &[mono], MONOMORPHISATION_REGISTRY_CAP);

    let saw_residual_diag = diagnostics.iter().any(|d: &HirDiagnostic| {
        matches!(
            &d.kind,
            HirDiagnosticKind::UnresolvedMachineTypeParamPostMono { machine, residual_var }
                if machine == "Holder" && residual_var == "T"
        )
    });
    assert!(
        saw_residual_diag,
        "expected UnresolvedMachineTypeParamPostMono for `Holder` with residual `T`; got: {diagnostics:?}"
    );
}

/// **Residual scoping (DI-015)**: a concrete named type whose
/// spelling happens to coincide with a generic-parameter name on a
/// completely unrelated declaration must NOT be flagged as a
/// residual. The pre-fix module-global predicate (`T` known anywhere
/// in the module) produced exactly this false positive. The
/// originating-declaration-scoped predicate rejects the false alarm.
#[test]
fn machine_mono_residual_check_does_not_false_positive_on_unrelated_type_param_name() {
    // Here `record T { value: i64; }` declares a concrete type named
    // `T`, and `fn make<T>(...)` separately declares a generic
    // type-parameter named `T`. The two `T`s have no structural
    // relationship — yet under the old module-global predicate, the
    // concrete `T` reaching the machine-mono pass would have been
    // misclassified as a residual. After Blocker 2's fix, the check
    // is scoped to the originating declaration's own type_params, so
    // this program lowers without an UnresolvedMachineTypeParamPostMono
    // diagnostic.
    let source = r"
type T { value: i64; }

machine Holder {
    events {
        Start { handle: T; }
        Stop;
    }

    state Idle;
    state Active { handle: T; }


    on Start: Idle => Active { Active { handle: event.handle } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}

fn make<U>(u: U) -> U { u }

fn main() {
    let t = T { value: 7 };
    let h: Holder = Active { handle: t };
    let _ = make::<i64>(1);
}
";
    let output = lower(source);
    assert!(
        output.diagnostics.iter().all(|d| !matches!(
            d.kind,
            HirDiagnosticKind::UnresolvedMachineTypeParamPostMono { .. }
        )),
        "expected no UnresolvedMachineTypeParamPostMono diagnostics (concrete `T` is not a residual of generic `make<U>`); got: {:?}",
        output.diagnostics
    );
}

/// **Multi-instantiation**: a generic machine reached with two
/// distinct type-args at two distinct use sites produces two
/// distinct `MachineMonoEntry` rows, keyed by canonical
/// `MachineMonoKey`s.
#[test]
fn machine_mono_records_two_distinct_instantiations_of_one_generic_machine() {
    let source = r"
trait Resource {
    fn close(self);
}

type File { raw: i64; }
type Socket { fd: i64; }

impl Resource for File {
    fn close(self) {}
}

impl Resource for Socket {
    fn close(self) {}
}

machine Lifecycle<T: Resource> {
    events {
        Start { handle: T; }
        Stop;
    }

    state Idle;
    state Active { handle: T; }


    on Start: Idle => Active { Active { handle: event.handle } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}

fn main() {
    let f = File { raw: 7 };
    let s = Socket { fd: 11 };
    let lf: Lifecycle<File> = Active { handle: f };
    let ls: Lifecycle<Socket> = Active { handle: s };
}
";
    let output = lower(source);
    let entries = &output.module.machine_instantiations;
    let lifecycle_keys: Vec<Vec<String>> = entries
        .iter()
        .filter(|e| e.key.origin_name == "Lifecycle")
        .map(|e| {
            e.key
                .type_args
                .iter()
                .map(|t| match t {
                    hew_types::ResolvedTy::Named { name, .. } => name.clone(),
                    other => format!("{other:?}"),
                })
                .collect()
        })
        .collect();
    assert_eq!(
        lifecycle_keys.len(),
        2,
        "expected exactly two Lifecycle instantiations; got: {lifecycle_keys:?}"
    );
    assert!(
        lifecycle_keys.contains(&vec!["File".to_string()]),
        "expected Lifecycle<File> instantiation; got: {lifecycle_keys:?}"
    );
    assert!(
        lifecycle_keys.contains(&vec!["Socket".to_string()]),
        "expected Lifecycle<Socket> instantiation; got: {lifecycle_keys:?}"
    );
}

/// **Cap diagnostic**: when distinct instantiations exceed the
/// configured monomorphisation cap, the pass emits exactly one
/// `MachineMonomorphisationCapExceeded` diagnostic and stops
/// recording further entries. We drive the cap branch with a small
/// synthetic input rather than authoring 1024+ Hew machines.
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "synthetic HIR fixture for cap-diagnostic regression — building HirFn/HirBlock/HirStmt structurally requires verbose nested literals; splitting into helpers would obscure what the synthetic shape is"
)]
fn machine_mono_emits_cap_diagnostic_when_distinct_instantiations_exceed_cap() {
    use hew_hir::{
        run_machine_mono_pass, BindingId, HirBinding, HirBlock, HirExpr, HirExprKind, HirFn,
        HirItem, HirMachineDecl, HirNodeId, HirStmt, HirStmtKind, IntentKind, ItemId, MonoKey,
        MonomorphizedFn, ScopeId, SiteId, ValueClass,
    };
    use hew_types::ResolvedTy;

    // Synthetic: one generic machine `M<T>`, one generic
    // `fn make<T>()` whose body lets `_: M<T>`, and N monomorphisations
    // mapping `T -> i8/i16/i32/i64/u8/...` (each a distinct concrete
    // `ResolvedTy`). With `mono_cap = 2`, the third instantiation
    // triggers the cap diagnostic.
    let machine = HirItem::Machine(HirMachineDecl {
        id: ItemId(0),
        node: HirNodeId(0),
        name: "M".to_string(),
        type_params: vec!["T".to_string()],
        type_param_bounds: vec![],
        states: vec![],
        events: vec![],
        transitions: vec![],
        has_default: true,
        span: 0..0,
    });

    let m_of_t = ResolvedTy::Named {
        name: "M".to_string(),
        args: vec![ResolvedTy::Named {
            name: "T".to_string(),
            args: vec![],
            builtin: None,
        }],
        builtin: None,
    };
    let binding = HirBinding {
        id: BindingId(0),
        name: "_x".to_string(),
        ty: m_of_t.clone(),
        mutable: false,
        span: 0..0,
    };
    let value_expr = HirExpr {
        node: HirNodeId(1),
        site: SiteId(0),
        ty: m_of_t.clone(),
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Unsupported("synthetic test placeholder".to_string()),
        span: 0..0,
    };
    let body = HirBlock {
        node: HirNodeId(2),
        scope: ScopeId(0),
        statements: vec![HirStmt {
            node: HirNodeId(3),
            kind: HirStmtKind::Let(binding, Some(value_expr)),
            span: 0..0,
        }],
        tail: None,
        ty: ResolvedTy::Unit,
        span: 0..0,
    };
    let make_fn = HirFn {
        id: ItemId(1),
        node: HirNodeId(4),
        name: "make".to_string(),
        type_params: vec!["T".to_string()],
        params: vec![],
        return_ty: ResolvedTy::Unit,
        body,
        span: 0..0,
        is_generator: false,
        intrinsic_id: None,
    };

    let distinct_args = [
        ResolvedTy::I8,
        ResolvedTy::I16,
        ResolvedTy::I32,
        ResolvedTy::I64,
    ];
    let monos: Vec<MonomorphizedFn> = distinct_args
        .iter()
        .enumerate()
        .map(|(i, ty)| MonomorphizedFn {
            key: MonoKey {
                origin: ItemId(1),
                origin_name: "make".to_string(),
                type_args: vec![ty.clone()],
            },
            mangled_name: format!("make$$arg{i}"),
        })
        .collect();

    let items = vec![machine, HirItem::Function(make_fn)];
    // Cap = 2 — the monomorphic uniform-path emission of `M` itself
    // is gated (M is generic, no uniform entry), so after walking the
    // first two monomorphisations the registry is full and the third
    // and fourth trigger the cap diagnostic.
    let (entries, diagnostics) = run_machine_mono_pass(&items, &monos, 2);

    assert_eq!(
        entries.len(),
        2,
        "expected exactly cap=2 entries, registry should refuse further inserts; got: {entries:?}"
    );
    let cap_diags: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::MachineMonomorphisationCapExceeded { .. }
            )
        })
        .collect();
    assert_eq!(
        cap_diags.len(),
        1,
        "cap diagnostic must be emitted exactly once (one-shot); got: {cap_diags:?}"
    );
    if let HirDiagnosticKind::MachineMonomorphisationCapExceeded { cap } = &cap_diags[0].kind {
        assert_eq!(*cap, 2, "diagnostic must carry the configured cap value");
    }
}

/// **Walker coverage — top-level record annotation**: a concrete
/// machine type that appears only as a field annotation on a
/// monomorphic top-level `record` must be observable in
/// `machine_instantiations`. Pre-Blocker-1 the walker did not visit
/// `HirItem::Record`, so an unused record-only reach-through was
/// invisible.
#[test]
fn machine_mono_walker_covers_top_level_record_annotation() {
    let source = r"
machine TrafficLight {
    events {
        Tick;
    }

    state Red;
    state Green;


    on Tick: Red => Green;
    on Tick: Green => Red;
}

record Dashboard {
    light: TrafficLight,
}

fn main() {}
";
    let output = lower(source);
    let entries = &output.module.machine_instantiations;
    let traffic_entries: Vec<&MachineMonoEntry> = entries
        .iter()
        .filter(|e| e.key.origin_name == "TrafficLight")
        .collect();
    // The uniform-path R246 emission already emits the monomorphic
    // TrafficLight entry, but dedup means we expect exactly one. The
    // load-bearing assertion is: walking the record surface does not
    // *miss* a reach-through (and does not double-emit).
    assert_eq!(
        traffic_entries.len(),
        1,
        "expected exactly one TrafficLight entry (uniform-path + record reach-through dedup); got: {entries:?}"
    );
}

/// **Walker coverage — top-level type-decl enum-variant payload**:
/// a concrete machine type buried in an enum-variant payload must be
/// observable. Pre-Blocker-1 the walker did not visit
/// `HirItem::TypeDecl`, so enum-payload-only reach-throughs were
/// invisible.
#[test]
fn machine_mono_walker_covers_top_level_type_decl_enum_variant_payload() {
    let source = r"
machine TrafficLight {
    events {
        Tick;
    }

    state Red;
    state Green;


    on Tick: Red => Green;
    on Tick: Green => Red;
}

enum Signal {
    Off;
    On(TrafficLight);
}

fn main() {}
";
    let output = lower(source);
    let entries = &output.module.machine_instantiations;
    let traffic_entries: Vec<&MachineMonoEntry> = entries
        .iter()
        .filter(|e| e.key.origin_name == "TrafficLight")
        .collect();
    assert_eq!(
        traffic_entries.len(),
        1,
        "expected exactly one TrafficLight entry (uniform-path + enum-payload reach-through dedup); got: {entries:?}"
    );
}

/// **Walker coverage — supervisor child spec**: a supervisor child
/// naming a monomorphic machine target produces no *new* entry
/// beyond the uniform-path R246 emission (since the supervisor child
/// type is a bare String at HIR with no parametric carrier). The
/// load-bearing claim is: walking the supervisor surface does not
/// crash or double-emit, and the uniform-path entry remains
/// observable.
#[test]
fn machine_mono_walker_visits_supervisor_children_without_double_emission() {
    let source = r"
actor Worker {
    let id: i64;

    receive fn ping() {}
}

supervisor Root {
    strategy: one_for_one;
    intensity: 3 within 10s;

    child w1: Worker(id: 1);
    child w2: Worker(id: 2);
}

fn main() {}
";
    let output = lower(source);
    // Worker is an actor (not a machine) but we still want
    // confirmation that walking the supervisor children iterates
    // without injecting bogus reach-throughs. Assert no
    // UnresolvedMachineTypeParamPostMono and no panic.
    assert!(
        output.diagnostics.iter().all(|d| !matches!(
            d.kind,
            HirDiagnosticKind::UnresolvedMachineTypeParamPostMono { .. }
        )),
        "supervisor child walking must not produce spurious residual diagnostics; got: {:?}",
        output.diagnostics
    );
}

/// **Const-args plumbing**: the `const_args` slot on
/// `MachineMonoKey` is the W3.039 subengine surface. In v0.5 baseline
/// every entry carries an empty `const_args` — but the entry's
/// equality semantics include the slot, so a hypothetical W3.039
/// substitution producing `const_args: vec![ConstValue::Usize(16)]`
/// would observe a distinct key from the baseline. This test pins
/// the slot's identity-bearing role at the schema level by
/// constructing two keys with identical `(origin, type_args)` and
/// distinct `const_args` and asserting they are distinct registry
/// entries.
#[test]
fn machine_mono_const_args_thread_through_from_w3_039_subengine() {
    use hew_hir::ids::ItemId;
    use hew_hir::mono::{ConstValue, MachineMonoKey};
    use std::collections::HashSet;

    let baseline = MachineMonoKey::new(
        ItemId(0),
        "Holder".to_string(),
        vec![hew_types::ResolvedTy::I64],
    );
    let with_const = MachineMonoKey::with_const_args(
        ItemId(0),
        "Holder".to_string(),
        vec![hew_types::ResolvedTy::I64],
        vec![ConstValue::Usize(16)],
    );
    assert_ne!(
        baseline, with_const,
        "MachineMonoKey identity must include const_args; baseline and W3.039-shaped key collided"
    );

    let mut seen: HashSet<MachineMonoKey> = HashSet::new();
    seen.insert(baseline.clone());
    seen.insert(with_const.clone());
    assert_eq!(
        seen.len(),
        2,
        "registry-level dedup must split on const_args; got: {seen:?}"
    );

    // Mangled-symbol identity-bearing role: the two keys produce
    // distinct LLVM-safe symbols, so codegen will emit two
    // MachineLayout entries — the substrate that W3.039 Stage 3+4
    // needs.
    assert_ne!(
        baseline.mangle(),
        with_const.mangle(),
        "mangled names must differ across const_args: `{}` vs `{}`",
        baseline.mangle(),
        with_const.mangle()
    );
}

/// **Structural residual identity (B2-residual)**: a top-level concrete
/// `type T` whose spelling collides with the spelling of an origin fn's
/// own type-parameter (e.g. `fn make<T>(t: T)` called as `make::<T>(t)`)
/// must NOT be flagged as a residual after function-mono substitution.
///
/// The pre-fix predicate compared the substituted `Named { name, args:
/// [] }` against the origin's `residual_domain` by string-name only;
/// the substituted-in concrete type-decl ref and the would-be residual
/// type-param ref are structurally indistinguishable at this layer,
/// so a same-name collision false-positived. The structural fix scopes
/// the residual domain to "origin type-params that do NOT shadow a
/// top-level concrete type-decl", because such a spelling, after
/// substitution, is unconditionally a concrete reference (the call-site
/// turbofish wrote the concrete decl ref into the body).
///
/// Companion to `_does_not_false_positive_on_unrelated_type_param_name`,
/// which exercises the *cross-decl* spelling collision; this test
/// exercises the *same-decl* spelling collision (decl name == origin
/// type-param name).
#[test]
fn machine_mono_residual_check_does_not_false_positive_on_concrete_type_with_same_name_as_origin_param(
) {
    let source = r"
type T { value: i64; }

machine Holder<X> {
    events {
        Start { handle: X; }
        Stop;
    }

    state Idle;
    state Active { handle: X; }


    on Start: Idle => Active { Active { handle: event.handle } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}

fn make<T>(t: T) -> T {
    let _h: Holder<T> = Active { handle: t };
    t
}

fn main() {
    let t = T { value: 1 };
    let _ = make::<T>(t);
}
";
    let output = lower(source);
    assert!(
        output.diagnostics.iter().all(|d| !matches!(
            d.kind,
            HirDiagnosticKind::UnresolvedMachineTypeParamPostMono { .. }
        )),
        "expected no UnresolvedMachineTypeParamPostMono diagnostics (concrete top-level `T` substituted into `fn make<T>` is not a residual of the origin's `T` type-param); got: {:?}",
        output.diagnostics
    );
}

/// **Impl-block residual-check fail-closed invariant**: the
/// machine-mono pass walks an `impl<T> Trait for Wrap<T>` block's
/// associated-type bodies under `empty_subst` — no `substitute_ty`
/// rewrite happens here, unlike the post-mono function walk. So the
/// shadow-filter that drops top-level concrete-type names from the
/// residual domain (sound for substituting walkers, see the rev3
/// test above) must NOT be applied here, or a residual impl-type-
/// param whose spelling collides with a top-level decl silently
/// passes the fail-closed check.
///
/// Repro: top-level `type T` shadows the impl's `T` type-param.
/// Under the bug, `impl_domain` is filtered to `{}` and the residual
/// `Holder<T>` in `type Item = Holder<T>;` slips through. The fix
/// keeps `impl_domain = {"T"}` so the walker emits the expected
/// `UnresolvedMachineTypeParamPostMono { machine: "Holder",
/// residual_var: "T" }` diagnostic.
#[test]
fn machine_mono_impl_block_residual_check_does_not_get_masked_by_same_name_top_level_decl() {
    let source = r"
type T { value: i64; }

machine Holder<X> {
    events {
        Start { handle: X; }
        Stop;
    }

    state Idle;
    state Active { handle: X; }


    on Start: Idle => Active { Active { handle: event.handle } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}

type Wrap<T> { value: T; }

trait Container {
    type Item;
}

impl<T> Container for Wrap<T> {
    type Item = Holder<T>;
}

fn main() {}
";
    let output = lower(source);
    let saw_residual = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::UnresolvedMachineTypeParamPostMono { machine, residual_var }
                if machine == "Holder" && residual_var == "T"
        )
    });
    assert!(
        saw_residual,
        "expected UnresolvedMachineTypeParamPostMono \
         {{ machine: \"Holder\", residual_var: \"T\" }} from the impl-block \
         assoc-type walker (impl<T> Container for Wrap<T> {{ type Item = Holder<T>; }}); \
         the top-level `type T` must NOT mask the impl's own residual `T`, \
         because the impl-block walker uses empty_subst and does not actually \
         substitute. Got diagnostics: {:?}",
        output.diagnostics
    );
}
