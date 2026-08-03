use hew_hir::{lower_program, verify_hir, HirDiagnosticKind, IntentKind, ResolutionCtx};
use hew_mir::{
    lower_hir_module, CmpPred, FloatWidth, Instr, MirCheck, MirDiagnosticKind, MirStatement, Place,
    Terminator, TrapKind,
};
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

// ---------- @resource / @linear surface ----------
//
// These tests exercise the user-source surface for `#[resource]` and
// `#[linear]` ownership-discipline markers. The substrate they ride
// on:
//   - parser carries `TypeDecl.resource_marker` + `consuming_methods`
//   - HIR lowers `Item::TypeDecl` into `HirItem::TypeDecl` and emits
//     `ResourceMissingClose` / `LinearNoConsumingMethods` diagnostics
//     on structurally invalid declarations
//   - HIR populates `HirModule.type_classes` so that
//     `ValueClass::of_ty(Named{T})` resolves to `Linear` /
//     `AffineResource` when `T` carries the corresponding marker
//   - MIR's existing forward-scan `MustConsume` check fires when a
//     `Linear` binding reaches a `Return` site without being consumed
//
// What this surface CANNOT exercise today (deferred to a follow-on
// cluster — method-call HIR lowering, `?` operator, and runtime Drop
// dispatch):
//   - calling a consuming method (`t.commit()`) to satisfy a `Linear`
//     binding's must-consume obligation
//   - `?`-bearing fixtures (`resource_early_close_propagates_err`)
//   - emitting a binary that calls `close(consuming self)` on scope
//     exit — codegen `Instr::Drop { drop_fn: Some(_) }` is fail-closed
//     per the substrate-PR hardening

#[test]
fn linear_unconsumed_single_exit_fires_must_consume() {
    // A `#[linear]` binding live at the implicit return fires
    // `MirCheck::MustConsume`. No consuming method is needed in the
    // body to drive the check — only the marker registration that
    // makes `ValueClass::of_ty(Named{Txn})` resolve to `Linear`.
    let src = r"
        #[linear]
        type Txn {
            id: i64
            fn commit(consuming self) -> i64 { 0 }
        }
        fn main() -> i64 {
            let t = Txn { id: 0 };
            42
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    // No HIR-validation diagnostics: `Txn` has a consuming method.
    assert!(
        !output.diagnostics.iter().any(|d| matches!(
            d.kind,
            HirDiagnosticKind::LinearNoConsumingMethods { .. }
                | HirDiagnosticKind::ResourceMissingClose { .. }
        )),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "verify diagnostics: {verify:?}");
    let pipeline = lower_hir_module(&output.module);

    let func = pipeline
        .checked_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main is in checked_mir");
    let must_consume = func.checks.iter().find_map(|check| match check {
        MirCheck::MustConsume { name, .. } if name == "t" => Some(()),
        _ => None,
    });
    assert!(
        must_consume.is_some(),
        "MustConsume should fire for unconsumed @linear binding `t`; checks: {:?}",
        func.checks
    );
    // The caret must anchor at the binding, not the distant exit: `bind_site`
    // (the `let t = …` introduction) is distinct from `exit_site` (the implicit
    // tail return). Pointing the diagnostic at the unconsumed binding is the
    // whole reason both sites are carried.
    let sites = func.checks.iter().find_map(|check| match check {
        MirCheck::MustConsume {
            name,
            bind_site,
            exit_site,
            ..
        } if name == "t" => Some((*bind_site, *exit_site)),
        _ => None,
    });
    let (bind_site, exit_site) = sites.expect("MustConsume carries bind/exit sites");
    assert_ne!(
        bind_site, exit_site,
        "bind_site must differ from exit_site so the caret lands on the binding"
    );
    // A declared `#[linear]` type must not produce a spurious UnknownType
    // diagnostic — the MIR layer must honour the HIR checker's type_classes
    // registry rather than treating every Named type as unknown.
    assert!(
        !pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UnknownType { name } if name == "Txn"
        )),
        "registered @linear type Txn must not produce UnknownType: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn linear_no_consuming_methods_declared_fires_hir_diagnostic() {
    // `#[linear]` type whose body declares zero `consuming self`
    // methods is structurally invalid: no exit path can ever exhaust
    // a binding of this type. HIR lowering emits
    // `LinearNoConsumingMethods` at type registration.
    let src = r"
        #[linear]
        type Bad {
            x: i64
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let diag = output.diagnostics.iter().find(|d| {
        matches!(d.kind, HirDiagnosticKind::LinearNoConsumingMethods { ref name } if name == "Bad")
    });
    assert!(
        diag.is_some(),
        "LinearNoConsumingMethods should fire for `Bad`; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn linear_with_consuming_method_emits_no_diagnostic() {
    // A `#[linear]` type that declares at least one `consuming self` method
    // is structurally valid; HIR must not emit `LinearNoConsumingMethods`.
    let src = r"
        #[linear]
        type Token {
            id: i64
            fn consume(consuming self) -> i64 { 0 }
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        !output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::LinearNoConsumingMethods { .. })),
        "valid @linear type must not produce LinearNoConsumingMethods: {:?}",
        output.diagnostics
    );
}

#[test]
fn resource_missing_close_method_fires_hir_diagnostic() {
    // `#[resource]` type whose body has no method named `close`
    // declared with `consuming self`. The implicit-drop contract
    // requires this method; HIR lowering rejects.
    let src = r"
        #[resource]
        type Sock {
            fd: i64
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let diag = output.diagnostics.iter().find(|d| {
        matches!(d.kind, HirDiagnosticKind::ResourceMissingClose { ref name } if name == "Sock")
    });
    assert!(
        diag.is_some(),
        "ResourceMissingClose should fire for `Sock`; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn resource_with_close_method_emits_no_diagnostic() {
    // A `#[resource]` type that declares `close` in a sibling inherent-impl
    // block (the v0.5 ratified surface — W3.030 Q-α-B) satisfies the
    // implicit-drop contract; HIR must not emit `ResourceMissingClose`,
    // `ResourceCloseSourceUnsupported`, or `ResourceCloseMustReturnUnit`.
    let src = r"
        #[resource]
        type File {
            fd: i64
        }
        impl File {
            fn close(self) { }
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        !output.diagnostics.iter().any(|d| matches!(
            d.kind,
            HirDiagnosticKind::ResourceMissingClose { .. }
                | HirDiagnosticKind::ResourceCloseSourceUnsupported { .. }
                | HirDiagnosticKind::ResourceCloseMustReturnUnit { .. }
        )),
        "valid @resource type with inherent-impl close must not produce \
         resource diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn resource_inline_close_method_fires_source_unsupported() {
    // W3.030 Q-α-B: a `#[resource]` whose `close` body is declared inline
    // via `TypeBodyItem::Method` is rejected at the HIR boundary. The
    // inline form is the silent-trap path (E8) — body not lowered, drop
    // dispatcher emits an unresolvable symbol, fail-closed at link time.
    // Surface here as a named compile-time diagnostic.
    let src = r"
        #[resource]
        type Sock {
            fd: i64
            fn close(consuming self) -> i64 { 0 }
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let count = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(d.kind, HirDiagnosticKind::ResourceCloseSourceUnsupported { ref name } if name == "Sock")
        })
        .count();
    assert_eq!(
        count, 1,
        "ResourceCloseSourceUnsupported should fire exactly once for `Sock`; \
         capabilities: hew_mir::ModuleCapabilities::EMPTY,
         diagnostics: {:?}",
        output.diagnostics
    );
    // R-4 determinism: exactly one named diagnostic on the `close` slot —
    // no parallel `ResourceMissingClose` even though `consuming_methods`
    // technically contains "close".
    assert!(
        !output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::ResourceMissingClose { .. })),
        "inline-close rejection must short-circuit `ResourceMissingClose`: {:?}",
        output.diagnostics
    );
}

#[test]
fn resource_inline_and_inherent_impl_close_single_diagnostic() {
    // R-4 determinism contract — when both the inline `TypeBodyItem::Method`
    // form AND the inherent-impl form are declared, the inline rejection
    // short-circuits further checks and exactly one named diagnostic fires.
    let src = r"
        #[resource]
        type Conn {
            fd: i64
            fn close(consuming self) -> i64 { 0 }
        }
        impl Conn {
            fn close(self) { }
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let resource_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::ResourceMissingClose { .. }
                    | HirDiagnosticKind::ResourceCloseSourceUnsupported { .. }
                    | HirDiagnosticKind::ResourceCloseMustReturnUnit { .. }
            )
        })
        .collect();
    assert_eq!(
        resource_diags.len(),
        1,
        "expected exactly one resource-close diagnostic, got: {resource_diags:?}"
    );
    assert!(
        matches!(
            resource_diags[0].kind,
            HirDiagnosticKind::ResourceCloseSourceUnsupported { ref name } if name == "Conn"
        ),
        "expected `ResourceCloseSourceUnsupported`, got: {:?}",
        resource_diags[0].kind
    );
}

#[test]
fn resource_close_non_unit_return_rejected() {
    // W3.030 Q-β-C: a `#[resource]` whose inherent-impl `close` returns
    // anything other than unit is rejected at the HIR boundary. Fallible
    // cleanup composes through `defer`, not through a non-unit `close`
    // return — the implicit-drop contract dispatches on every exit path
    // and propagating a value off, say, `Trap` or `Cancel` has no defined
    // semantics in v0.5.
    let src = r"
        #[resource]
        type Conn {
            fd: i64
        }
        impl Conn {
            fn close(self) -> i64 { 0 }
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let diag = output.diagnostics.iter().find(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::ResourceCloseMustReturnUnit { ref name, ref return_ty }
                if name == "Conn" && return_ty == "i64"
        )
    });
    assert!(
        diag.is_some(),
        "ResourceCloseMustReturnUnit should fire for `Conn` returning `i64`; \
         diagnostics: {:?}",
        output.diagnostics
    );
    assert!(
        !output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::ResourceMissingClose { .. })),
        "non-unit return must not also fire ResourceMissingClose: {:?}",
        output.diagnostics
    );
}

#[test]
fn resource_close_explicit_unit_return_accepted() {
    // Explicit `-> ()` return type is unit; same outcome as no return
    // type. Confirms the unit detector handles the `Tuple([])` shape.
    let src = r"
        #[resource]
        type Conn {
            fd: i64
        }
        impl Conn {
            fn close(self) -> () { }
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        !output.diagnostics.iter().any(|d| matches!(
            d.kind,
            HirDiagnosticKind::ResourceMissingClose { .. }
                | HirDiagnosticKind::ResourceCloseSourceUnsupported { .. }
                | HirDiagnosticKind::ResourceCloseMustReturnUnit { .. }
        )),
        "`-> ()` should be treated as unit: {:?}",
        output.diagnostics
    );
}

fn pipeline(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");
    lower_hir_module(&output.module)
}

#[test]
fn elaborated_mir_makes_owned_string_drop_explicit() {
    let pipeline = pipeline(r#"fn main() { let s = "hello"; }"#);
    let func = &pipeline.elaborated_mir[0];
    assert!(func
        .statements
        .iter()
        .any(|stmt| { matches!(stmt, MirStatement::Drop { name, .. } if name == "s") }));
}

#[test]
fn bitcopy_arithmetic_has_no_drop() {
    let pipeline = pipeline("fn main() -> i64 { let x = 1 + 2; return x; }");
    let func = &pipeline.elaborated_mir[0];
    assert!(!func
        .statements
        .iter()
        .any(|stmt| matches!(stmt, MirStatement::Drop { .. })));
}

#[test]
fn checked_mir_rejects_use_after_consume() {
    let pipeline = lower_source(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(self) { } }
        fn main() -> i64 {
            let s = File { fd: 1 };
            let _t = s;
            s.fd
        }
    ",
    );

    assert!(pipeline.diagnostics.iter().any(|diagnostic| matches!(
        diagnostic.kind,
        MirDiagnosticKind::UseAfterConsume { ref name, .. } if name == "s"
    )));
}

#[test]
fn checked_mir_finding_carries_consume_and_use_sites() {
    // The payload-bearing `MirCheck::UseAfterConsume` shape projects
    // through to the diagnostic so a CLI consumer can point at both
    // ends of the bug, not just the binding name.
    let pipeline = lower_source(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(self) { } }
        fn main() -> i64 {
            let s = File { fd: 1 };
            let _t = s;
            s.fd
        }
    ",
    );
    let func = pipeline
        .checked_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main is in checked_mir");
    let finding = func
        .checks
        .iter()
        .find_map(|check| match check {
            MirCheck::UseAfterConsume {
                name,
                consumed_at,
                used_at,
                ..
            } if name == "s" => Some((*consumed_at, *used_at)),
            _ => None,
        })
        .expect("UseAfterConsume finding for s");
    assert_ne!(
        finding.0, finding.1,
        "consume and use sites are distinct: {finding:?}"
    );
}

// ---------- B1: use-after-move INTO AN AGGREGATE (tuple) ----------
//
// An owned single-owner operand (`@resource` / `@linear`) moved into a
// tuple constructor must consume its source binding, so a later use is
// rejected at CHECK time. Before the fix the aggregate-construct move
// did not mark the source consumed, so `(s, r); s.close()` passed
// `hew check` and double-freed at runtime (explicit close + the
// moved-into tuple's scope-exit drop). Copy operands (BitCopy ints)
// carry no single-owner drop obligation and must NOT be flagged; heap-
// owning CowValue operands (strings, bytes, containers, owned records)
// are move sources at aggregate ingress and must be tracked.

#[test]
fn checked_mir_rejects_use_after_move_into_tuple() {
    // `h` is an `@resource` (AffineResource) value moved into the tuple
    // `(h, g)`. The trailing read of `h.fd` is a use after the move and
    // must surface `UseAfterConsume`.
    let p = lower_source(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(self) { } }
        fn use_after_move_into_tuple() -> i64 {
            let h = File { fd: 1 };
            let g = File { fd: 2 };
            let pair = (h, g);
            let leaked = h.fd;
            let _ = pair;
            leaked
        }
    ",
    );
    assert!(
        p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "h"
        )),
        "use of `h` after it was moved into the tuple `(h, g)` must surface \
         UseAfterConsume: {:?}",
        p.diagnostics
    );
}

#[test]
fn checked_mir_rejects_duplicate_owned_handle_in_one_tuple() {
    // The same owned handle placed into a tuple TWICE (`(h, h)`) double-moves:
    // both aggregate fields would free the one handle. The second placement is
    // a use-after-move and must surface `UseAfterConsume` (caught at the
    // aggregate-alias marker, not just at a later separate use).
    let p = lower_source(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(self) { } }
        fn duplicate_owned_in_tuple() -> (File, File) {
            let h = File { fd: 1 };
            return (h, h);
        }
    ",
    );
    assert!(
        p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "h"
        )),
        "placing the same owned handle `h` into a tuple twice must surface \
         UseAfterConsume: {:?}",
        p.diagnostics
    );
}

#[test]
fn checked_mir_rejects_duplicate_owned_param_in_one_tuple() {
    // The owned PARAMETER analogue — a param is not in `owned_locals` but is
    // still a single owner, so `(p, p)` must be rejected too (the value-class
    // guard, not `owned_locals` membership, is the authority).
    let p = lower_source(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(self) { } }
        fn duplicate_owned_param(p: File) -> (File, File) {
            return (p, p);
        }
    ",
    );
    assert!(
        p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "p"
        )),
        "placing the same owned param `p` into a tuple twice must surface \
         UseAfterConsume: {:?}",
        p.diagnostics
    );
}

#[test]
fn tuple_build_and_return_without_post_move_use_is_accepted() {
    // Positive control: building `(a, b)` from owned resources and
    // returning the tuple — with NO post-move use of `a`/`b` — must NOT
    // produce a false-positive UseAfterConsume.
    let p = lower_source(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(self) { } }
        fn build_pair() -> (File, File) {
            let a = File { fd: 1 };
            let b = File { fd: 2 };
            let pair = (a, b);
            return pair;
        }
    ",
    );
    assert!(
        !p.diagnostics
            .iter()
            .any(|d| matches!(&d.kind, MirDiagnosticKind::UseAfterConsume { .. })),
        "a legitimate tuple build + return with no post-move use must NOT \
         fire UseAfterConsume: {:?}",
        p.diagnostics
    );
}

// ---------- #1295: `#[resource]` inherent `close(self)` consume ----------
//
// A `#[resource]` type's inherent `fn close(self)` is the implicit-drop
// dispatch target AND a terminal consuming method. Calling it explicitly moves
// the receiver, so (a) a later use is rejected at CHECK time and (b) the
// scope-exit implicit drop is suppressed on the consumed path — the close fires
// exactly once. The checker registers the resource `close` into the consume
// path; HIR lowers the receiver with `IntentKind::Consume`; MIR transitions the
// binding to `Consumed` and excludes it from the function-exit drop set.

/// Reading the receiver after a `#[resource]` inherent `close()` is rejected at
/// the earliest ownership boundary: the checker reports `UseAfterMove`, before
/// invalid source can reach HIR/MIR.
#[test]
fn checked_mir_rejects_use_after_resource_close() {
    let parsed = hew_parser::parse(
        r"
        #[resource]
        type Conn { id: i64 }
        impl Conn { fn close(self) { } }
        fn serve() -> i64 {
            let c = Conn { id: 1 };
            c.close();
            c.id
        }
    ",
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&parsed.program);
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == hew_types::error::TypeErrorKind::UseAfterMove),
        "reading `c.id` after `c.close()` must fail at checker boundary: {:?}",
        output.errors
    );
}

/// A second `close()` of a `#[resource]` value is rejected — the first call
/// consumed the receiver.
#[test]
fn checked_mir_rejects_second_resource_close() {
    let parsed = hew_parser::parse(
        r"
        #[resource]
        type Conn { id: i64 }
        impl Conn { fn close(self) { } }
        fn serve() {
            let c = Conn { id: 1 };
            c.close();
            c.close();
        }
    ",
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&parsed.program);
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == hew_types::error::TypeErrorKind::UseAfterMove),
        "a second `c.close()` must fail at checker boundary: {:?}",
        output.errors
    );
}

/// An explicitly-closed `#[resource]` value is excluded from the function-exit
/// drop set: the explicit `close()` is the single release, and the scope-exit
/// implicit drop is suppressed (no double-close). The drop plan for `serve`
/// carries no `DropKind::Resource` for `c`.
#[test]
fn explicit_resource_close_suppresses_scope_exit_drop() {
    let p = lower_source(
        r"
        #[resource]
        type Conn { id: i64 }
        impl Conn { fn close(self) { } }
        fn serve() {
            let c = Conn { id: 1 };
            c.close();
        }
    ",
    );
    assert!(
        p.diagnostics.is_empty(),
        "a single explicit close must type-check cleanly: {:?}",
        p.diagnostics
    );
    let serve = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "serve")
        .expect("serve function present");
    assert!(
        serve.drop_plans.iter().all(|(_, plan)| plan
            .drops
            .iter()
            .all(|d| d.kind != hew_mir::DropKind::Resource)),
        "an explicitly-closed `#[resource]` must NOT also earn a scope-exit \
         resource drop — that is the #1295 double-close: {:?}",
        serve.drop_plans
    );
}

/// A `#[resource]` value that is NEVER explicitly closed keeps its scope-exit
/// implicit drop — the regression guard for the fix. The drop plan for `serve`
/// carries a `DropKind::Resource` for `c`.
#[test]
fn never_closed_resource_keeps_scope_exit_drop() {
    let p = lower_source(
        r"
        #[resource]
        type Conn { id: i64 }
        impl Conn { fn close(self) { } }
        fn serve() {
            let c = Conn { id: 1 };
            let _ = c.id;
        }
    ",
    );
    assert!(
        p.diagnostics.is_empty(),
        "a never-closed resource must type-check cleanly: {:?}",
        p.diagnostics
    );
    let serve = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "serve")
        .expect("serve function present");
    assert!(
        serve.drop_plans.iter().any(|(_, plan)| plan
            .drops
            .iter()
            .any(|d| d.kind == hew_mir::DropKind::Resource)),
        "a never-explicitly-closed `#[resource]` must keep its scope-exit \
         implicit drop so it is released exactly once: {:?}",
        serve.drop_plans
    );
}

#[test]
fn monitor_registration_resource_close_elaborates_scope_exit_drop() {
    let p = lower_source(
        r#"
        #[resource]
        type MonitorRegistration { ref_id: i64 }
        impl MonitorRegistration {
            fn close(monitor_ref: MonitorRegistration) {
                unsafe {
                    hew_actor_demonitor(monitor_ref.ref_id)
                };
            }
        }
        extern "C" {
            fn hew_actor_demonitor(ref_id: i64);
        }
        fn serve() {
            let monitor_ref = MonitorRegistration { ref_id: 7 };
            let _ = monitor_ref.ref_id;
        }
    "#,
    );
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let serve = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "serve")
        .expect("serve function present");
    let resource_drops: Vec<_> = serve
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|d| d.kind == hew_mir::DropKind::Resource)
        .collect();
    assert!(
        !resource_drops.is_empty(),
        "monitor registration must keep its scope-exit resource close: {:?}",
        serve.drop_plans
    );
    assert!(
        resource_drops.iter().any(|d| {
            d.drop_fn
                == Some(hew_mir::DropFnSpec::UserClose(
                    "MonitorRegistration::close".to_string(),
                ))
        }),
        "monitor registration scope-exit drop must dispatch its close method: {:?}",
        serve.drop_plans
    );
}

/// A generator extracted from a returned tuple is a transferred projection:
/// the tuple slot is neutralized before the handle is re-packed and consumed.
/// The MIR obligation model must count that as one owner moving through the
/// chain, never as a retained second owner.
#[test]
fn generator_projection_transfer_repack_stays_one_owner() {
    let pipeline = lower_source(
        r"
        gen fn count() -> i64 { yield 1; yield 2; yield 3 }

        fn make() -> (Generator<i64, ()>, i64) {
            let g = count();
            return (g, 0);
        }

        fn main() {
            let pair = make();
            let g = pair.0;
            let repacked = (g, 99);
            var total = 0;
            for n in repacked.0 {
                total = total + n;
            }
            println(total);
        }
        ",
    );
    assert!(
        !pipeline.diagnostics.iter().any(|diagnostic| matches!(
            diagnostic.kind,
            MirDiagnosticKind::ObligationUnderReleased { .. }
                | MirDiagnosticKind::ObligationOverReleased { .. }
                | MirDiagnosticKind::OwnedHandleAggregateExtractionUnsupported { .. }
        )),
        "a neutralized generator projection/repack is one transferred owner:\n{}\ndiagnostics: {:#?}",
        hew_mir::dump_mir(&pipeline, hew_mir::DumpStage::Raw),
        pipeline.diagnostics
    );
}

/// #1933 — a `#[resource]` consumed on ONE branch of an `if` (live on the
/// fall-through) reaches the function's single exit at dataflow state
/// `MaybeConsumed`. The binding must STAY in the scope-exit drop set there
/// (so the not-consumed runtime path still closes it — no leak) AND the
/// surviving `DropKind::Resource` must carry a path-sensitive runtime
/// drop-flag `guard` (so the consumed runtime path does NOT double-close the
/// non-idempotent user `close`). Before the fix the consume site removed the
/// binding from the owned ledger path-insensitively, so the per-exit drop set
/// was empty and the not-consumed branch leaked.
#[test]
fn conditional_resource_close_keeps_flag_guarded_drop() {
    let p = lower_source(
        r"
        #[resource]
        type Conn { id: i64 }
        impl Conn { fn close(self) { } }
        fn serve(flag: bool) {
            let c = Conn { id: 1 };
            if flag {
                c.close();
            }
        }
    ",
    );
    assert!(
        p.diagnostics.is_empty(),
        "a conditionally-closed resource must type-check cleanly: {:?}",
        p.diagnostics
    );
    let serve = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "serve")
        .expect("serve function present");
    let resource_drops: Vec<_> = serve
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|d| d.kind == hew_mir::DropKind::Resource)
        .collect();
    assert!(
        !resource_drops.is_empty(),
        "a conditionally-closed `#[resource]` must KEEP a scope-exit resource \
         drop so the not-consumed branch is still released exactly once (#1933 \
         leak): {:?}",
        serve.drop_plans
    );
    assert!(
        resource_drops.iter().all(|d| d.guard.is_some()),
        "every surviving conditional resource close must carry a \
         path-sensitive drop-flag guard so the consumed branch does not \
         double-close the non-idempotent user `close` at the MaybeConsumed \
         shared exit (#1941 double-free): {:?}",
        serve.drop_plans
    );
}

/// A heap-owning `#[resource]` is both a resource and structurally an owned
/// record. Its recursive `RecordInPlace` helper is the complete ritual: user
/// `close(self)` first, then owned fields in reverse order. After a conditional
/// by-value hand-off the source slot is neutralized, so the whole helper must
/// retain the resource drop flag that distinguishes the consumed path from the
/// live path.
#[test]
fn conditional_heap_resource_move_keeps_flag_guarded_record_drop() {
    let p = lower_source(
        r"
        #[resource]
        type Buf { data: Vec<i64> }
        impl Buf {
            fn close(self) {
                let _ = self.data.len();
            }
        }
        fn make() -> Buf {
            Buf { data: Vec::new() }
        }
        fn sink(b: Buf) {
            b.close();
        }
        fn run(flag: bool) {
            let b = make();
            if flag {
                sink(b);
            }
        }
    ",
    );
    assert!(
        p.diagnostics.is_empty(),
        "a conditional heap-resource move must lower cleanly: {:?}",
        p.diagnostics
    );
    let run = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "run")
        .expect("run function present");
    let drops: Vec<_> = run
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .collect();
    assert!(
        drops
            .iter()
            .any(|drop| drop.kind == hew_mir::DropKind::RecordInPlace && drop.guard.is_some()),
        "the live side of a conditional heap-resource move must retain a \
         flag-guarded recursive record drop: {:?}",
        run.drop_plans
    );
    assert!(
        drops
            .iter()
            .all(|drop| drop.kind != hew_mir::DropKind::Resource),
        "a heap-owning resource must not bypass recursive field teardown through \
         the scalar Resource close path: {:?}",
        run.drop_plans
    );
}

/// A direct String-record construction can satisfy the syntactic ingress proof
/// used by the conditional-record protocol even when its declared type is a
/// `#[resource]`. The marker is authoritative for the flag: this binding gets
/// exactly the affine/resource guard, never a second zero-only
/// conditional-record flag. Because the record owns a string field, teardown
/// still uses the recursive `RecordInPlace` helper rather than the scalar
/// close-only path.
#[test]
fn direct_string_resource_move_has_only_resource_flag_authority() {
    let p = lower_source(
        r#"
        #[resource]
        type Token { text: string }
        impl Token {
            fn close(self) {
                let _ = self.text.len();
            }
        }
        fn sink(token: Token) {
            token.close();
        }
        fn run(flag: bool) {
            let token = Token { text: "payload".to_upper() };
            if flag {
                sink(token);
            }
        }
    "#,
    );
    assert!(
        p.diagnostics.is_empty(),
        "a direct String resource transfer must lower cleanly: {:?}",
        p.diagnostics
    );
    let run = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "run")
        .expect("run function present");
    let drops = run
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| &plan.drops)
        .collect::<Vec<_>>();
    let affine_guard = drops
        .iter()
        .find_map(|drop| {
            (drop.kind == hew_mir::DropKind::RecordInPlace)
                .then_some(drop.guard)
                .flatten()
        })
        .expect("the conditional resource transfer needs one affine guard");
    assert!(
        drops
            .iter()
            .all(|drop| drop.kind != hew_mir::DropKind::Resource),
        "a field-bearing resource must not bypass recursive teardown through \
         the scalar close-only path: \
         {run:#?}"
    );

    let checked = p
        .checked_mir
        .iter()
        .find(|f| f.name == "run")
        .expect("run checked MIR present");
    let zero_init_places = checked
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::ConstI64 { dest, value: 0 } => Some(*dest),
            _ => None,
        })
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(
        zero_init_places,
        std::collections::HashSet::from([affine_guard]),
        "the resource binding must not receive a second, zero-only \
         conditional-record flag: {checked:#?}"
    );
    assert!(
        checked
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instr| matches!(
                instr,
                Instr::ConstI64 { dest, value: 1 } if *dest == affine_guard
            )),
        "the direct transfer must execute the resource flag handoff: \
         {checked:#?}"
    );
}

/// Control for the flag scoping: a `#[resource]` consumed UNCONDITIONALLY (a
/// single straight-line `close()`) reaches its exit at `Consumed`, so the
/// per-exit dataflow filter EXCLUDES it entirely — no scope-exit resource
/// drop, guarded or not. The flag mechanism only does runtime work at a
/// genuine `MaybeConsumed` join, never on an unconditional consume.
#[test]
fn unconditional_resource_close_emits_no_scope_exit_drop() {
    let p = lower_source(
        r"
        #[resource]
        type Conn { id: i64 }
        impl Conn { fn close(self) { } }
        fn serve() {
            let c = Conn { id: 1 };
            c.close();
        }
    ",
    );
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let serve = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "serve")
        .expect("serve function present");
    assert!(
        serve.drop_plans.iter().all(|(_, plan)| plan
            .drops
            .iter()
            .all(|d| d.kind != hew_mir::DropKind::Resource)),
        "an unconditionally-closed `#[resource]` must earn NO scope-exit \
         resource drop (the explicit close is the single release): {:?}",
        serve.drop_plans
    );
}

/// A never-closed `#[resource]` earns its implicit scope-exit close, and that
/// close is FLAG-GUARDED too — the same exactly-once machinery the
/// conditional case rides, so a never-closed resource on a `MaybeConsumed`
/// path (e.g. a later move) remains safe. (An unconditional fall-through is
/// `Live` at exit, not `MaybeConsumed`, but the guard is still attached: it
/// is keyed by the binding having a user-close drop-flag, not by the exit
/// state, and reads `flag == 0` so the close always fires here.)
#[test]
fn never_closed_resource_keeps_flag_guarded_scope_exit_drop() {
    let p = lower_source(
        r"
        #[resource]
        type Conn { id: i64 }
        impl Conn { fn close(self) { } }
        fn serve() {
            let c = Conn { id: 1 };
            let _ = c.id;
        }
    ",
    );
    assert!(p.diagnostics.is_empty(), "{:?}", p.diagnostics);
    let serve = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "serve")
        .expect("serve function present");
    let resource_drops: Vec<_> = serve
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|d| d.kind == hew_mir::DropKind::Resource)
        .collect();
    assert!(
        !resource_drops.is_empty(),
        "a never-closed `#[resource]` must keep its implicit scope-exit \
         close: {:?}",
        serve.drop_plans
    );
    assert!(
        resource_drops.iter().all(|d| d.guard.is_some()),
        "the implicit scope-exit close of a non-idempotent user `#[resource]` \
         must carry a path-sensitive drop-flag guard: {:?}",
        serve.drop_plans
    );
}

/// #1941 — a `#[resource]` moved BY VALUE into an ordinary free function is an
/// ownership transfer into the callee (Hew has no by-reference parameters):
/// the callee owns and closes it, so the caller's binding transitions to
/// `Consumed` and earns NO scope-exit close. Before the fix the argument
/// lowered `IntentKind::Read`, the caller stayed `Live`, and the caller's
/// implicit drop double-closed what the callee already closed.
#[test]
fn value_move_into_free_fn_consumes_caller_binding() {
    let p = lower_source(
        r"
        #[resource]
        type Conn { id: i64 }
        impl Conn { fn close(self) { } }
        fn sink(c: Conn) { c.close(); }
        fn run() {
            let c = Conn { id: 1 };
            sink(c);
        }
    ",
    );
    assert!(
        p.diagnostics.is_empty(),
        "a clean by-value move of a resource into a consumer must type-check: \
         {:?}",
        p.diagnostics
    );
    let run = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "run")
        .expect("run function present");
    assert!(
        run.drop_plans.iter().all(|(_, plan)| plan
            .drops
            .iter()
            .all(|d| d.kind != hew_mir::DropKind::Resource)),
        "a `#[resource]` moved by value into a consumer must earn NO \
         scope-exit close in the CALLER — the callee owns and closes it; a \
         caller close is the #1941 double-close: {:?}",
        run.drop_plans
    );
}

/// #1941 negative — reading a `#[resource]` after moving it by value into a
/// free function is a use-after-move. The MIR move-checker must surface
/// `UseAfterConsume`. Before the fix the by-value argument lowered
/// `IntentKind::Read`, so the move was invisible and the trailing use
/// compiled clean (use-after-free admitted).
#[test]
fn checked_mir_rejects_use_after_move_into_free_fn() {
    let p = lower_source(
        r"
        #[resource]
        type Conn { id: i64 }
        impl Conn {
            fn close(self) { }
            fn id_of(self) -> i64 { self.id }
        }
        fn sink(c: Conn) { c.close(); }
        fn run() -> i64 {
            let c = Conn { id: 1 };
            sink(c);
            c.id_of()
        }
    ",
    );
    assert!(
        p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "c"
        )),
        "reading `c` after moving it by value into `sink` must surface \
         UseAfterConsume: {:?}",
        p.diagnostics
    );
}

#[test]
fn copy_operands_moved_into_tuple_are_not_flagged() {
    // Copy-operand control: BitCopy ints placed into a tuple are shared,
    // not moved — reusing the source `a` after `(a, 6)` must stay clean
    // (the false-positive guard the fix must preserve).
    let p = lower_source(
        r"fn main() -> i64 {
            let a = 5;
            let pair = (a, 6);
            let reused = a;
            let _ = pair;
            reused
        }",
    );
    assert!(
        !p.diagnostics
            .iter()
            .any(|d| matches!(&d.kind, MirDiagnosticKind::UseAfterConsume { .. })),
        "reusing a BitCopy operand after it was placed into a tuple must NOT \
         fire UseAfterConsume: {:?}",
        p.diagnostics
    );
}

#[test]
fn cowvalue_string_shared_into_tuple_allows_post_share_use() {
    // Tuple ingress retains a string that stays live, so the source and tuple
    // element are independent owners.
    let p = lower_source(
        r#"fn main() -> string {
            let a = "he" + "llo";
            let pair = (a, "world");
            let reused = a;
            let _ = pair;
            reused
        }"#,
    );
    assert!(
        !p.diagnostics
            .iter()
            .any(|d| matches!(&d.kind, MirDiagnosticKind::UseAfterConsume { .. })),
        "retained tuple ingress must keep the source usable: {:?}",
        p.diagnostics
    );
}

#[test]
fn managed_string_moved_into_hashset_insert_rejects_post_move_use() {
    let p = lower_source(
        r#"fn main() -> string {
            let words: HashSet<string> = HashSet::new();
            let word = "colour";
            words.insert(word);
            word
        }"#,
    );
    assert!(
        p.diagnostics.iter().any(
            |d| matches!(&d.kind, MirDiagnosticKind::UseAfterConsume { name, .. } if name == "word")
        ),
        "reusing a managed string after HashSet.insert moves it into the set \
         must fire UseAfterConsume: {:?}",
        p.diagnostics
    );
}

#[test]
fn managed_string_moved_into_hashmap_insert_rejects_post_move_use() {
    let p = lower_source(
        r#"fn main() -> string {
            let labels: HashMap<string, string> = HashMap::new();
            let key = "k";
            let value = "v";
            labels.insert(key, value);
            value
        }"#,
    );
    assert!(
        p.diagnostics.iter().any(
            |d| matches!(&d.kind, MirDiagnosticKind::UseAfterConsume { name, .. } if name == "value")
        ),
        "reusing a managed string after HashMap.insert moves it into the map \
         must fire UseAfterConsume: {:?}",
        p.diagnostics
    );
}

#[test]
fn managed_string_insert_without_reuse_stays_clean() {
    let p = lower_source(
        r#"fn main() -> i64 {
            let words: HashSet<string> = HashSet::new();
            let word = "behaviour";
            words.insert(word);
            words.len()
        }"#,
    );
    assert!(
        !p.diagnostics
            .iter()
            .any(|d| matches!(&d.kind, MirDiagnosticKind::UseAfterConsume { .. })),
        "moving a managed string into HashSet.insert without reusing it must not \
         over-reject: {:?}",
        p.diagnostics
    );
}

#[test]
fn checked_mir_rejects_use_of_uninitialised_binding() {
    // `let x;` declares without initialising; the subsequent `let _y = x;`
    // reads `x` before any `Bind` for it. The check fires on the
    // statement stream, independent of whether the backend can lower the
    // value (the spine rejects this for unrelated reasons too — what we
    // care about here is that the MirCheck variant is populated and the
    // diagnostic surface carries the binding name).
    let pipeline = pipeline("fn f() { let x; let _y = x; }");

    let init_check = pipeline
        .checked_mir
        .iter()
        .flat_map(|f| f.checks.iter())
        .find(|check| matches!(check, MirCheck::InitialisedBeforeUse { name, .. } if name == "x"));
    assert!(
        init_check.is_some(),
        "InitialisedBeforeUse finding expected for x: {:?}",
        pipeline
            .checked_mir
            .iter()
            .flat_map(|f| f.checks.iter())
            .collect::<Vec<_>>()
    );
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::InitialisedBeforeUse { name, .. } if name == "x"
        )),
        "MirDiagnostic projection must carry the InitialisedBeforeUse \
         finding to the CLI rejection channel: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn checked_mir_rejects_use_after_consume_inside_block_expression() {
    // Block expressions used to drop nested `HirStmtKind::Let` /
    // `HirStmtKind::Return` statements before they reached the
    // checker-authority stream; the move-checker only saw
    // `HirStmtKind::Expr` forwards. A real use-after-consume inside
    // `{ ... }` would compile cleanly and emit a binary. Pin the
    // recursive-lowering path here.
    let pipeline = lower_source(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(self) { } }
        fn main() -> i64 {
            { let s = File { fd: 1 }; let _t = s; let _u = s.fd; }
            42
        }
    ",
    );
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "s"
        )),
        "block-nested use-after-consume must surface: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn checked_mir_rejects_use_of_uninitialised_binding_inside_block_expression() {
    // Companion to the use-after-consume case: a block-nested
    // `let x; let _y = x;` exercises the same lowering recursion and
    // must reach the move-checker.
    let pipeline = pipeline("fn main() -> i64 { { let x; let _y = x; } return 42; }");
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::InitialisedBeforeUse { name, .. } if name == "x"
        )),
        "block-nested initialised-before-use must surface: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn checked_mir_rejects_use_after_consume_inside_if_arm() {
    // `if` arms are themselves `HirExpr`s — when an arm is a block
    // expression, the recursion path runs through `HirExprKind::If`
    // -> `lower_value(then_expr)` -> `HirExprKind::Block`. Pin that
    // the arm's nested `let` statements reach the move-checker.
    let pipeline = lower_source(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(self) { } }
        fn main() -> i64 {
            let _r = if true {
                let s = File { fd: 1 };
                let _t = s;
                let _u = s.fd;
                7
            } else {
                8
            };
            42
        }
    ",
    );
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "s"
        )),
        "if-arm-nested use-after-consume must surface: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn checked_mir_accepts_spine_integer_function() {
    // The v0.5 integer spine must not produce any MirCheck findings —
    // the move-checker is fail-closed only on real legality violations.
    let pipeline = pipeline("fn main() -> i64 { let x = 1 + 2; return x; }");
    for func in &pipeline.checked_mir {
        assert!(
            func.checks.is_empty(),
            "spine integer function {} has unexpected checks: {:?}",
            func.name,
            func.checks
        );
    }
}

#[test]
fn cross_function_call_types_lower_via_call_terminator() {
    // Direct calls to user-defined functions in the same module are now
    // lowered via `Terminator::Call` — no `NotYetImplemented` for function
    // calls to module functions. The pipeline() helper asserts HIR is clean;
    // this test pins the MIR acceptance shape: both functions appear in
    // `raw_mir` and the diagnostic stream is clean.
    let pipeline = lower_source(
        "fn add(a: i64, b: i64) -> i64 { return a + b; } \
         fn main() -> i64 { return add(0, 1); }",
    );
    let names: Vec<&str> = pipeline.raw_mir.iter().map(|f| f.name.as_str()).collect();
    assert!(
        names.contains(&"add"),
        "raw_mir must include add: {names:?}"
    );
    assert!(
        names.contains(&"main"),
        "raw_mir must include main: {names:?}"
    );
    // Direct user-function calls must not produce NotYetImplemented.
    let nyi_diags: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|d| matches!(&d.kind, MirDiagnosticKind::NotYetImplemented { .. }))
        .collect();
    assert!(
        nyi_diags.is_empty(),
        "direct user-function calls must lower without NotYetImplemented: {nyi_diags:?}"
    );
    // Must have no diagnostics at all.
    assert!(
        pipeline.diagnostics.is_empty(),
        "direct user-function call pipeline must have no MIR diagnostics: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn unknown_user_type_rejected_at_mir_boundary() {
    // D10: Named user types with no known ValueClass must be rejected at the
    // MIR boundary so they cannot reach the backend.
    let parsed = hew_parser::parse("fn f(x: Foo) -> Foo { return x; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    // HIR has no error — type annotations resolve to Named for any identifier.
    // The fail-closed boundary is MIR.
    assert!(
        output.diagnostics.is_empty(),
        "HIR should not error on undeclared type name alone: {:?}",
        output.diagnostics
    );
    let pipeline = lower_hir_module(&output.module);
    assert!(
        pipeline
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, MirDiagnosticKind::UnknownType { .. })),
        "unknown named type must produce UnknownType at MIR boundary: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn registered_fieldless_user_type_has_zero_field_layout() {
    // A fieldless declared type is a concrete zero-field record. Its layout is
    // sufficient for MIR and codegen readiness.
    let parsed = hew_parser::parse(
        r"
        #[linear]
        type Token {}
        impl Token {
            fn consume(consuming self) -> i64 { 0 }
        }
        fn f(x: Token) -> i64 { 0 }
        ",
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let pipeline = lower_hir_module(&output.module);

    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );
    assert!(
        pipeline
            .record_layouts
            .iter()
            .any(|layout| layout.name == "Token" && layout.field_tys.is_empty()),
        "fieldless Token must carry a concrete zero-field record layout: {:?}",
        pipeline.record_layouts
    );
}

/// A generic record INSTANTIATION carrying an owned (string) field is now
/// ADMITTED by value (no W3.029) and earns a tag-aware `RecordInPlace` drop on
/// the mangled-key thunk (`Wrapper$$string`). Migrated forward from
/// `non_bitcopy_user_record_instantiation_reports_precise_valueclass_diagnostic`,
/// which pinned the now-reversed reject behaviour — the generic-instantiation
/// owned-aggregate capability is the residual fail-closed witness this fix
/// closes. The W3.029 gate is retained for unregistered/unclassifiable shapes
/// (see `generic_record_owned_aggregate_admission` unit tests in lower.rs).
#[test]
fn non_bitcopy_user_record_instantiation_admits_with_record_drop() {
    let parsed = hew_parser::parse(
        r#"
        pub type Wrapper<T> { inner: T }
        fn main() -> i64 {
            let w = Wrapper { inner: "owned" };
            println(w.inner);
            0
        }
        "#,
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(tc_output.errors.is_empty(), "{:?}", tc_output.errors);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let pipeline = lower_hir_module(&output.module);

    assert!(
        !source_has_w3029_in(&pipeline, "Wrapper$$string"),
        "generic owned-record instantiation Wrapper<string> must be admitted, \
         not W3.029-rejected: {:?}",
        pipeline.diagnostics
    );
    assert!(
        pipeline_has_record_inplace_drop(&pipeline),
        "the admitted generic owned record must earn a RecordInPlace scope-exit \
         drop on its mangled-key thunk: {:?}",
        pipeline.elaborated_mir
    );
    assert!(
        !pipeline
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, MirDiagnosticKind::DecisionMapTotal { .. })),
        "admitting the generic owned record must keep the decision map total: {:?}",
        pipeline.diagnostics
    );
}

/// Lower `src` to the MIR pipeline, asserting front-half cleanliness.
fn lower_source(src: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(tc_output.errors.is_empty(), "{:?}", tc_output.errors);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    lower_hir_module(&output.module)
}

/// True when the pipeline elaborated at least one `DropKind::RecordInPlace`
/// scope-exit drop — i.e. an owned aggregate record was admitted by value AND
/// earned a tag-aware drop.
fn pipeline_has_record_inplace_drop(pipeline: &hew_mir::IrPipeline) -> bool {
    pipeline.elaborated_mir.iter().any(|f| {
        f.drop_plans.iter().any(|(_, plan)| {
            plan.drops
                .iter()
                .any(|d| d.kind == hew_mir::DropKind::RecordInPlace)
        })
    })
}

/// Value-class capstone — a monomorphic owned record copied (rebound) by value
/// is ADMITTED (no W3.029) and the original earns a `RecordInPlace` drop.
/// Migrated forward from `user_record_string_copy_still_reports_w3029`, which
/// pinned the now-reversed reject behaviour.
#[test]
fn user_record_string_copy_admits_with_record_drop() {
    let pipeline = lower_source(
        r#"
        type User { name: string }
        fn main() -> i64 {
            let first = "Ada";
            let last = "Lovelace";
            let full = first + " " + last;
            let user = User { name: full };
            let copy = user;
            println(copy.name);
            0
        }
        "#,
    );
    assert!(
        !source_has_w3029_in(&pipeline, "User"),
        "monomorphic owned record copy must be admitted, not W3.029-rejected: {:?}",
        pipeline.diagnostics
    );
    assert!(
        pipeline_has_record_inplace_drop(&pipeline),
        "the admitted owned record must earn a RecordInPlace scope-exit drop: {:?}",
        pipeline.elaborated_mir
    );
}

/// A monomorphic owned record returned by value is ADMITTED (no W3.029). The
/// returned binding ESCAPES, so it must NOT earn a scope-exit drop in the
/// returning function (the caller owns it); the fail-closed escape-scan
/// excludes it. Migrated forward from the reject-pinning test.
#[test]
fn user_record_string_return_admits_and_excludes_drop_on_escape() {
    let pipeline = lower_source(
        r#"
        type User { name: string }
        fn make() -> User {
            let first = "Ada";
            let last = "Lovelace";
            let full = first + " " + last;
            let user = User { name: full };
            return user;
        }
        fn main() -> i64 { 0 }
        "#,
    );
    assert!(
        !source_has_w3029_in(&pipeline, "User"),
        "monomorphic owned record return must be admitted, not W3.029-rejected: {:?}",
        pipeline.diagnostics
    );
    let make = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == "make")
        .expect("make function present");
    assert!(
        make.drop_plans.iter().all(|(_, plan)| plan
            .drops
            .iter()
            .all(|d| d.kind != hew_mir::DropKind::RecordInPlace)),
        "a returned (escaped) record must NOT earn a RecordInPlace drop in its \
         producing function — that would double-free the caller's value: {:?}",
        make.drop_plans
    );
}

/// True when the named function earns at least one `RecordInPlace` drop.
fn fn_has_record_inplace_drop(pipeline: &hew_mir::IrPipeline, fn_name: &str) -> bool {
    pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present in elaborated_mir"))
        .drop_plans
        .iter()
        .any(|(_, plan)| {
            plan.drops
                .iter()
                .any(|d| d.kind == hew_mir::DropKind::RecordInPlace)
        })
}

/// Mailbox delivery transfers a fresh string owner into the receive frame.
/// Even a completely ignored parameter must therefore have one unguarded
/// scope-exit release; ordinary by-value string parameters remain borrows and
/// do not take this actor-only path.
#[test]
fn recv_handler_ignored_string_param_earns_scope_exit_drop() {
    let pipeline = lower_source(
        r#"
        actor Sink {
            var seen: i64;
            receive fn take(label: string) { seen = seen + 1; }
        }
        fn main() -> i64 {
            let sink = spawn Sink(seen: 0);
            sink.take("unused".to_upper());
            0
        }
        "#,
    );
    let handler = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == "Sink__recv__take")
        .expect("receive handler present");
    assert!(
        handler
            .drop_plans
            .iter()
            .any(|(_, plan)| plan.drops.iter().any(|drop| matches!(
                drop.kind,
                hew_mir::DropKind::CowHeap { .. }
            ) && drop.guard.is_none())),
        "an ignored mailbox-owned string must earn one unguarded handler-exit drop: {:?}",
        handler.drop_plans
    );
}

/// A mailbox-owned string moved into actor state on one branch and merely read
/// on the other reaches the shared return as `MaybeConsumed`. The elaborated
/// drop must survive behind one runtime flag, and both flag values must be
/// present in realized MIR: zero at entry, one immediately on the transfer
/// path.
#[test]
fn recv_handler_conditional_string_transfer_uses_guarded_drop() {
    let pipeline = lower_source(
        r#"
        actor Keeper {
            var seen: i64;
            var last: string;
            receive fn take(label: string, keep: bool) {
                if keep {
                    last = label;
                } else {
                    seen = seen + label.len();
                }
            }
        }
        fn main() -> i64 {
            let keeper = spawn Keeper(seen: 0, last: "seed");
            keeper.take("next".to_upper(), true);
            0
        }
        "#,
    );
    let handler = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == "Keeper__recv__take")
        .expect("receive handler present");
    let guard = handler
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| &plan.drops)
        .find_map(|drop| {
            matches!(drop.kind, hew_mir::DropKind::CowHeap { .. })
                .then_some(drop.guard)
                .flatten()
        })
        .expect("conditional mailbox string drop must carry a runtime guard");
    let checked = pipeline
        .checked_mir
        .iter()
        .find(|f| f.name == "Keeper__recv__take")
        .expect("checked receive handler present");
    for expected in [0, 1] {
        assert!(
            checked
                .blocks
                .iter()
                .flat_map(|block| &block.instructions)
                .any(|instr| matches!(
                    instr,
                    Instr::ConstI64 { dest, value }
                        if *dest == guard && *value == expected
                )),
            "conditional mailbox string flag must be assigned {expected}: {:?}",
            checked.blocks
        );
    }
}

/// A path-sensitive mailbox drop flag cannot erase the ordinary alias verdict.
/// On the `Wrap` branch the binding is only read into `RecordInit`, so the flag
/// remains zero and the handler's guarded scope-exit drop will run. That
/// aggregate must receive its own retain before construction. On the direct
/// state-transfer branch, the consuming `BindingRef` arms the flag and no
/// retain is needed.
#[test]
fn recv_handler_conditional_record_ingress_retains_before_guarded_drop() {
    let pipeline = lower_source(
        r"
        type Wrap { name: string }
        actor Fan {
            var held: Wrap;
            var last: string;
            receive fn route(label: string, wrap: bool) {
                if wrap {
                    held = Wrap { name: label };
                } else {
                    last = label;
                }
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let raw = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "Fan__recv__route")
        .expect("receive handler raw MIR present");
    let record_block = raw
        .blocks
        .iter()
        .find(|block| {
            block
                .instructions
                .iter()
                .any(|instr| matches!(instr, Instr::RecordInit { .. }))
        })
        .expect("record-ingress branch present");
    let record_index = record_block
        .instructions
        .iter()
        .position(|instr| matches!(instr, Instr::RecordInit { .. }))
        .expect("record init index");
    assert!(
        record_block.instructions[..record_index]
            .iter()
            .any(|instr| matches!(
                instr,
                Instr::StringRetain {
                    value: Place::Local(0),
                    condition: hew_mir::StringRetainCondition::Always,
                }
            )),
        "the unconsumed mailbox string copied into a record must be retained \
         before the aggregate becomes an owner: {:?}",
        record_block.instructions
    );

    let direct_store_block = raw
        .blocks
        .iter()
        .find(|block| {
            block.instructions.iter().any(|instr| {
                matches!(
                    instr,
                    Instr::ActorStateFieldStore {
                        field_offset: hew_mir::FieldOffset(1),
                        src: Place::Local(0),
                        ..
                    }
                )
            })
        })
        .expect("direct state-transfer branch present");
    assert!(
        direct_store_block
            .instructions
            .iter()
            .any(|instr| matches!(instr, Instr::ConstI64 { value: 1, .. })),
        "the direct ownership-transfer branch must arm its drop flag: {:?}",
        direct_store_block.instructions
    );
    assert!(
        direct_store_block
            .instructions
            .iter()
            .all(|instr| !matches!(instr, Instr::StringRetain { .. })),
        "an armed direct handoff must not mint an unbalanced extra owner: {:?}",
        direct_store_block.instructions
    );

    let handler = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == "Fan__recv__route")
        .expect("receive handler elaborated MIR present");
    assert!(
        handler
            .drop_plans
            .iter()
            .any(|(_, plan)| plan.drops.iter().any(|drop| matches!(
                drop.kind,
                hew_mir::DropKind::CowHeap { .. }
            ) && drop.guard.is_some())),
        "the retained read branch and armed handoff branch must converge on one \
         guarded source drop: {:?}",
        handler.drop_plans
    );
}

/// A projected record is a byte-copy alias even though its nested string is not
/// directly visible to the string share scanner. Before the alias enters a new
/// owning record, MIR must carry one recursive retain authority.
#[test]
fn actor_state_nested_record_alias_ingress_retains_recursive_string_leaves() {
    let pipeline = lower_source(
        r"
        record Leaf { text: string }
        record Wrap { leaf: Leaf }
        actor Keeper {
            var cur: Wrap;
            receive fn rewrite() {
                cur = Wrap { leaf: cur.leaf };
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "Keeper__recv__rewrite")
        .expect("nested record alias handler raw MIR present");
    let conditions = handler
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::StringRetain { value, condition } => Some((*value, condition)),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        conditions,
        vec![(
            Place::Local(1),
            &hew_mir::StringRetainCondition::AggregateBorrowedIngress
        )],
        "the projected Leaf alias needs exactly one recursive owner mint: {:?}",
        handler.blocks
    );
}

/// The recursive retain authority must tag-dispatch an inline enum and descend
/// through a record payload rather than treating the enum carrier as leafless.
#[test]
fn actor_state_enum_payload_alias_ingress_retains_recursive_string_leaves() {
    let pipeline = lower_source(
        r"
        record Leaf { text: string }
        enum Payload { Empty; Hold(Leaf); }
        record Wrap { payload: Payload }
        actor Keeper {
            var cur: Wrap;
            receive fn rewrite() {
                cur = Wrap { payload: cur.payload };
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "Keeper__recv__rewrite")
        .expect("enum payload alias handler raw MIR present");
    let conditions = handler
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::StringRetain { value, condition } => Some((*value, condition)),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        conditions,
        vec![(
            Place::Local(1),
            &hew_mir::StringRetainCondition::AggregateBorrowedIngress
        )],
        "the projected Payload alias needs exactly one recursive owner mint: {:?}",
        handler.blocks
    );
}

/// A whole-value actor-state load is already deep-cloned by its `Owned` load
/// mode. Its one proven store escape must consume that clone without an
/// additional aggregate-ingress retain.
#[test]
fn actor_state_owned_record_self_store_does_not_double_retain_strings() {
    let pipeline = lower_source(
        r"
        record Profile { name: string }
        actor Keeper {
            var prof: Profile;
            receive fn rewrite() {
                prof = prof;
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "Keeper__recv__rewrite")
        .expect("record self-store handler raw MIR present");
    assert!(
        handler
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instr| matches!(
                instr,
                Instr::ActorStateFieldLoad {
                    mode: hew_mir::ActorStateLoadMode::Owned,
                    ..
                }
            )),
        "whole-record self-store must retain through its Owned load: {:?}",
        handler.blocks
    );
    assert!(
        !handler
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instr| matches!(
                instr,
                Instr::StringRetain {
                    condition: hew_mir::StringRetainCondition::AggregateBorrowedIngress,
                    ..
                }
            )),
        "the Owned load already minted the recursive owner; a second aggregate \
         retain would leak: {:?}",
        handler.blocks
    );
}

/// A cyclic record ingress carries the exact actor-state leaf path while
/// retaining each borrowed incoming string. The overwrite helper releases the
/// old String owner even on equal-pointer replacement, balancing one mint
/// against one release on every iteration.
#[test]
fn recv_handler_loop_carried_record_ingress_retains_borrowed_owner() {
    let pipeline = lower_source(
        r"
        type Wrap { name: string }
        actor Fan {
            var seen: i64;
            var held: Wrap;
            receive fn route(label: string, count: i64) {
                var i = 0;
                while i < count {
                    held = Wrap { name: label };
                    i = i + 1;
                }
                seen = seen + 1;
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let raw = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "Fan__recv__route")
        .expect("receive handler raw MIR present");
    let record_block = raw
        .blocks
        .iter()
        .find(|block| {
            block
                .instructions
                .iter()
                .any(|instr| matches!(instr, Instr::RecordInit { .. }))
        })
        .expect("loop record-ingress block present");
    let record_index = record_block
        .instructions
        .iter()
        .position(|instr| matches!(instr, Instr::RecordInit { .. }))
        .expect("record init index");
    assert!(
        matches!(
            record_block.instructions[..record_index].last(),
            Some(Instr::StringRetain {
                value: Place::Local(0),
                condition: hew_mir::StringRetainCondition::ActorStateRecordBorrowedIngress {
                    state_field: hew_mir::FieldOffset(1),
                    record_path,
                    ..
                },
            }) if record_path.as_slice() == [hew_mir::FieldOffset(0)]
        ),
        "loop-carried ingress must retain the mailbox string at held.name: {:?}",
        record_block.instructions
    );
    assert!(
        !raw.blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instr| matches!(
                instr,
                Instr::StringRetain {
                    condition: hew_mir::StringRetainCondition::Always,
                    ..
                }
            )),
        "the cyclic ingress must use the actor-state ingress witness: {:?}",
        raw.blocks
    );

    let handler = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == "Fan__recv__route")
        .expect("receive handler elaborated MIR present");
    assert!(
        handler.drop_plans.iter().any(|(exit, plan)| matches!(
            exit,
            hew_mir::ExitPath::Return { .. }
        ) && plan
            .drops
            .iter()
            .any(|drop| matches!(drop.kind, hew_mir::DropKind::CowHeap { .. }))),
        "zero iterations must still release the mailbox string through the \
         handler-exit drop: {:?}",
        handler.drop_plans
    );
}

/// The cyclic state-leaf proof follows the constructed record through
/// whole-value moves and through enclosing `RecordInit`s. These are the two
/// minimal shapes that defeated the original adjacent-instruction match and
/// leaked one string per delivered message.
#[test]
fn recv_handler_loop_record_ingress_tracks_move_chains_and_nested_leaf_paths() {
    let pipeline = lower_source(
        r"
        type Wrap { name: string }
        type Inner { name: string }
        type Outer { inner: Inner }
        actor Fan {
            var seen: i64;
            var flat: Wrap;
            var nested: Outer;
            receive fn route_move(label: string, count: i64) {
                var i = 0;
                while i < count {
                    let next = Wrap { name: label };
                    flat = next;
                    i = i + 1;
                }
                seen = seen + 1;
            }
            receive fn route_nested(label: string, count: i64) {
                var i = 0;
                while i < count {
                    nested = Outer { inner: Inner { name: label } };
                    i = i + 1;
                }
                seen = seen + 1;
            }
        }
        fn main() -> i64 { 0 }
        ",
    );

    for (handler_name, state_field, expected_path) in [
        (
            "Fan__recv__route_move",
            hew_mir::FieldOffset(1),
            vec![hew_mir::FieldOffset(0)],
        ),
        (
            "Fan__recv__route_nested",
            hew_mir::FieldOffset(2),
            vec![hew_mir::FieldOffset(0), hew_mir::FieldOffset(0)],
        ),
    ] {
        let handler = pipeline
            .raw_mir
            .iter()
            .find(|function| function.name == handler_name)
            .unwrap_or_else(|| panic!("{handler_name} raw MIR present"));
        let conditions = handler
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .filter_map(|instr| match instr {
                Instr::StringRetain { condition, .. } => Some(condition),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert!(
            conditions.iter().any(|condition| matches!(
                condition,
                hew_mir::StringRetainCondition::ActorStateRecordBorrowedIngress {
                    state_field: actual_state,
                    record_path,
                    ..
                } if *actual_state == state_field && *record_path == expected_path
            )),
            "{handler_name} must carry the exact actor-state leaf path: {conditions:?}"
        );
        assert!(
            conditions
                .iter()
                .all(|condition| !matches!(condition, hew_mir::StringRetainCondition::Always)),
            "{handler_name} must use the actor-state ingress witness: \
             {conditions:?}"
        );
    }
}

/// The loop-carried state sink may land in a successor block when a branch
/// separates construction from assignment. The lineage proof must cross the
/// split/join while keeping the ingress retain on the path-defining
/// construction.
#[test]
fn recv_handler_loop_record_ingress_tracks_cfg_split_to_state_sink() {
    let pipeline = lower_source(
        r"
        type Wrap { name: string }
        actor Fan {
            var seen: i64;
            var held: Wrap;
            receive fn route(label: string, count: i64) {
                var i = 0;
                while i < count {
                    let next = Wrap { name: label };
                    if i > 0 { seen = seen + 1; }
                    held = next;
                    i = i + 1;
                }
                seen = seen + 1;
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "Fan__recv__route")
        .expect("split-ingress handler raw MIR present");
    let record_block = handler
        .blocks
        .iter()
        .find(|block| {
            block
                .instructions
                .iter()
                .any(|instr| matches!(instr, Instr::RecordInit { .. }))
        })
        .expect("record construction block");
    let retain_block = handler
        .blocks
        .iter()
        .find(|block| {
            block.instructions.windows(2).any(|pair| {
                matches!(
                    pair,
                    [
                        Instr::StringRetain {
                            condition:
                                hew_mir::StringRetainCondition::ActorStateRecordBorrowedIngress {
                                    state_field: hew_mir::FieldOffset(1),
                                    record_path,
                                    ..
                                },
                            ..
                        },
                        Instr::RecordInit { .. }
                    ] if record_path.as_slice() == [hew_mir::FieldOffset(0)]
                )
            })
        })
        .unwrap_or_else(|| {
            panic!(
                "ingress retain immediately before split-ingress construction: {:?}",
                handler.blocks
            )
        });
    assert_eq!(
        record_block.id, retain_block.id,
        "the ingress retain must stay on the path-defining construction"
    );
    assert!(
        handler.blocks.iter().any(|block| {
            block.id != retain_block.id
                && block
                    .instructions
                    .iter()
                    .any(|instr| matches!(instr, Instr::ActorStateFieldStore { .. }))
        }),
        "the regression witness must keep construction and state store in different blocks: {:?}",
        handler.blocks
    );
    assert!(
        handler
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .all(|instr| !matches!(
                instr,
                Instr::StringRetain {
                    condition: hew_mir::StringRetainCondition::Always,
                    ..
                }
            )),
        "the split cyclic ingress must use the actor-state ingress witness: {:?}",
        handler.blocks
    );
}

/// Alternative constructors and alternative nested leaf paths must keep their
/// ingress retains on the branch that actually selected that value. Moving
/// every retain to the shared state store would execute all alternatives.
#[test]
fn recv_handler_loop_record_ingress_keeps_branch_selected_paths_exclusive() {
    let pipeline = lower_source(
        r"
        type Wrap { name: string }
        type Inner { name: string }
        type Pair { left: Inner; right: Inner }
        actor Fan {
            var seen: i64;
            var flat: Wrap;
            var pair: Pair;
            var repeated: Pair;
            receive fn route(label: string, other: string, count: i64) {
                var i = 0;
                while i < count {
                    let next = if i % 2 == 0 {
                        Wrap { name: label }
                    } else {
                        Wrap { name: label }
                    };
                    flat = next;
                    let named = Inner { name: label };
                    let alternate = Inner { name: other };
                    let nested = if i % 2 == 0 {
                        Pair { left: named, right: alternate }
                    } else {
                        Pair { left: alternate, right: named }
                    };
                    pair = nested;
                    repeated = Pair {
                        left: Inner { name: label },
                        right: Inner { name: label }
                    };
                    i = i + 1;
                }
                seen = seen + 1;
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "Fan__recv__route")
        .expect("branch-selected handler raw MIR present");
    let mut conditional = Vec::new();
    let mut ordinary = 0usize;
    for block in &handler.blocks {
        for instr in &block.instructions {
            if let Instr::StringRetain {
                condition:
                    hew_mir::StringRetainCondition::ActorStateRecordBorrowedIngress {
                        state_field,
                        record_path,
                    },
                ..
            } = instr
            {
                conditional.push((block.id, state_field.0, record_path.clone()));
            }
            ordinary += usize::from(matches!(
                instr,
                Instr::StringRetain {
                    condition: hew_mir::StringRetainCondition::Always,
                    ..
                }
            ));
        }
    }
    assert_eq!(
        conditional
            .iter()
            .filter(|(_, field, path)| {
                *field == 1 && path.as_slice() == [hew_mir::FieldOffset(0)]
            })
            .count(),
        2,
        "each alternative flat constructor needs one exclusive retain: {conditional:?}"
    );
    assert_eq!(
        ordinary, 2,
        "the two cross-field sources must each fall back to one path-safe owner mint: {:?}",
        handler.blocks
    );
    assert_eq!(
        conditional
            .iter()
            .filter(|(_, field, _)| *field == 3)
            .count(),
        2,
        "the two repeated-source occurrences each need a retain site: {conditional:?}"
    );
}

#[test]
fn recv_handler_nested_branch_ingress_retains_before_join() {
    let pipeline = lower_source(
        r"
        type Inner { name: string }
        type Outer { inner: Inner }
        actor Fan {
            var held: Outer;
            receive fn route(label: string, count: i64) {
                var i = 0;
                while i < count {
                    let selected = if i % 2 == 0 {
                        Inner { name: label }
                    } else {
                        Inner { name: label }
                    };
                    held = Outer { inner: selected };
                    i = i + 1;
                }
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "Fan__recv__route")
        .expect("nested branch handler raw MIR present");
    let retain_blocks = handler
        .blocks
        .iter()
        .filter_map(|block| {
            block.instructions.iter().any(|instr| matches!(
                instr,
                Instr::StringRetain {
                    condition: hew_mir::StringRetainCondition::ActorStateRecordBorrowedIngress {
                        state_field: hew_mir::FieldOffset(0),
                        record_path,
                    },
                    ..
                } if record_path.as_slice()
                    == [hew_mir::FieldOffset(0), hew_mir::FieldOffset(0)]
            ))
            .then_some(block.id)
        })
        .collect::<Vec<_>>();
    assert_eq!(
        retain_blocks.len(),
        2,
        "both nested alternatives need a branch-local retain: {:?}",
        handler.blocks
    );
    assert_ne!(
        retain_blocks[0], retain_blocks[1],
        "nested alternatives must not be hoisted together at the common Outer constructor"
    );
}

#[test]
fn recv_handler_branch_to_two_state_fields_mints_one_source_owner() {
    let pipeline = lower_source(
        r"
        type Wrap { name: string }
        actor Fan {
            var one: Wrap;
            var two: Wrap;
            receive fn route(label: string, choose_one: bool, count: i64) {
                var i = 0;
                while i < count {
                    let next = Wrap { name: label };
                    if choose_one { one = next; } else { two = next; }
                    i = i + 1;
                }
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "Fan__recv__route")
        .expect("two-state-field handler raw MIR present");
    let retains = handler
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter(|instr| {
            matches!(
                instr,
                Instr::StringRetain {
                    value: Place::Local(0),
                    condition: hew_mir::StringRetainCondition::Always,
                }
            )
        })
        .count();
    assert_eq!(
        retains, 1,
        "mutually exclusive state sinks must fall back to one source-construction owner mint: \
         {:?}",
        handler.blocks
    );
}

#[test]
fn recv_handler_simultaneous_distinct_sources_keep_two_owner_mints() {
    let pipeline = lower_source(
        r"
        type Wrap { name: string }
        actor Fan {
            var one: Wrap;
            var two: Wrap;
            receive fn route(left: string, right: string, count: i64) {
                var i = 0;
                while i < count {
                    let first = Wrap { name: left };
                    let second = Wrap { name: right };
                    one = first;
                    two = second;
                    i = i + 1;
                }
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "Fan__recv__route")
        .expect("simultaneous-distinct-source handler raw MIR present");
    let state_ingress_retains =
        handler
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .filter(|instr| {
                matches!(
                    instr,
                    Instr::StringRetain {
                        condition:
                            hew_mir::StringRetainCondition::ActorStateRecordBorrowedIngress { .. },
                        ..
                    }
                )
            })
            .count();
    assert_eq!(
        state_ingress_retains, 2,
        "distinct source constructions executed in the same iteration each require an owner mint: \
         {:?}",
        handler.blocks
    );
}

/// A constructed local that can leave the handler without reaching actor state
/// still needs an ordinary owner for its scope-exit record drop. The CFG proof
/// must reject the state-ingress witness on that path.
#[test]
fn recv_handler_record_ingress_with_non_store_exit_retains_unconditionally() {
    let pipeline = lower_source(
        r"
        type Wrap { name: string }
        actor Fan {
            var held: Wrap;
            receive fn route(label: string, store: bool, count: i64) {
                var i = 0;
                while i < count {
                    let next = Wrap { name: label };
                    if !store { return; }
                    held = next;
                    i = i + 1;
                }
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "Fan__recv__route")
        .expect("early-exit handler raw MIR present");
    let conditions = handler
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::StringRetain { condition, .. } => Some(condition),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert!(
        conditions
            .iter()
            .any(|condition| matches!(condition, hew_mir::StringRetainCondition::Always)),
        "the non-store exit must keep an owner for the local record drop: {conditions:?}"
    );
    assert!(
        conditions.iter().all(|condition| !matches!(
            condition,
            hew_mir::StringRetainCondition::ActorStateRecordBorrowedIngress { .. }
        )),
        "a state-ingress owner is unsound when another path drops the local record: \
         {conditions:?}"
    );
}

/// True when the named function earns a top-level `bytes` release.
fn fn_has_bytes_drop(pipeline: &hew_mir::IrPipeline, fn_name: &str) -> bool {
    pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present in elaborated_mir"))
        .drop_plans
        .iter()
        .any(|(_, plan)| {
            plan.drops.iter().any(|drop| {
                matches!(
                    drop.kind,
                    hew_mir::DropKind::CowHeap {
                        release: hew_mir::CowHeapRelease::Bytes
                    }
                )
            })
        })
}

/// A copy-mode mailbox transfers the sole `bytes` refcount into the receive
/// handler. A borrow-only handler is the terminal owner and must release it.
/// This is the compiler half of active-mode socket delivery: the reactor sends
/// the same ordinary mailbox payload as any Hew actor sender.
#[test]
fn recv_handler_borrow_only_bytes_param_earns_bytes_drop() {
    let pipeline = lower_source(
        r"
        actor Sink {
            var seen: i64;
            receive fn take(data: bytes) {
                seen = seen + data.len();
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    assert!(
        fn_has_bytes_drop(&pipeline, "Sink__recv__take"),
        "a borrow-only receive-handler bytes param must earn the balancing \
         CowHeap(Bytes) drop: {:?}",
        pipeline.elaborated_mir
    );
    let handler = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "Sink__recv__take")
        .expect("Sink__recv__take must be present");
    assert!(
        handler.drop_plans.iter().all(|(_, plan)| {
            plan.drops
                .iter()
                .filter(|drop| {
                    matches!(
                        drop.kind,
                        hew_mir::DropKind::CowHeap {
                            release: hew_mir::CowHeapRelease::Bytes
                        }
                    )
                })
                .count()
                <= 1
        }),
        "one mailbox reference must never produce duplicate Bytes drops in a \
         single exit plan: {:#?}",
        handler.drop_plans
    );
}

/// Counterfactual: ordinary by-value Hew calls borrow `bytes`; the caller keeps
/// the refcount. The actor-ingress rule must not widen into free functions.
#[test]
fn ordinary_borrow_only_bytes_param_does_not_earn_bytes_drop() {
    let pipeline = lower_source(
        r"
        fn observe(data: bytes) -> i64 { data.len() }
        fn main() -> i64 { 0 }
        ",
    );
    assert!(
        !fn_has_bytes_drop(&pipeline, "observe"),
        "an ordinary by-value bytes parameter is caller-owned and must not be \
         dropped by the callee: {:?}",
        pipeline.elaborated_mir
    );
}

/// Counterfactual: forwarding the mailbox-owned triple into another actor
/// transfers its one refcount again. The first handler must suppress its drop;
/// the final recipient becomes the terminal owner.
#[test]
fn recv_handler_forwarded_bytes_param_transfers_drop_to_recipient() {
    let pipeline = lower_source(
        r"
        actor Recipient {
            receive fn take(data: bytes) { let _ = data.len(); }
        }
        actor Forwarder {
            let recipient: LocalPid<Recipient>;
            receive fn forward(data: bytes) { recipient.take(data); }
        }
        fn main() -> i64 { 0 }
        ",
    );
    assert!(
        !fn_has_bytes_drop(&pipeline, "Forwarder__recv__forward"),
        "the forwarding handler transferred its only bytes refcount and must \
         not drop it again: {:?}",
        pipeline.elaborated_mir
    );
    assert!(
        fn_has_bytes_drop(&pipeline, "Recipient__recv__take"),
        "the terminal recipient must own and drop the forwarded bytes: {:?}",
        pipeline.elaborated_mir
    );
}

/// #2747 — a by-value owned-aggregate RECORD message param delivered to an actor
/// `receive fn` and only BORROWED (`b.payload.len()`) is a transient
/// read-and-discard the handler frame solely owns: the mailbox `memcpy`
/// consumed the caller's original. It must earn a `RecordInPlace` scope-exit
/// drop in the handler so its collection field buffer is freed once per
/// delivered message (pre-fix it leaked — the handler was never registered as
/// owning the delivered record).
#[test]
fn recv_handler_borrow_only_record_param_earns_record_inplace_drop() {
    let pipeline = lower_source(
        r"
        type Boxed { payload: Vec<i64> }
        actor Sink {
            var seen: i64;
            receive fn take(b: Boxed) {
                seen = seen + b.payload.len();
            }
            receive fn total() -> i64 { seen }
        }
        fn main() -> i64 {
            let sink = spawn Sink(seen: 0);
            sink.take(Boxed { payload: [1, 2, 3] });
            0
        }
        ",
    );
    assert!(
        fn_has_record_inplace_drop(&pipeline, "Sink__recv__take"),
        "a borrow-only receive-handler record param must earn a RecordInPlace \
         scope-exit drop (else its field buffer leaks per message): {:?}",
        pipeline.elaborated_mir
    );
}

/// #2747 boundary (the double-free guard) — when the handler MOVES the delivered
/// record's owned field INTO actor state (`store = b.payload`), the field
/// escapes; the synthesised `state_drop_fn` is that buffer's single free. The
/// escape-scan must therefore SUPPRESS the handler's `RecordInPlace` drop for
/// that record — admitting it would free the same buffer twice. Fail-closed: a
/// retained record is excluded, never re-admitted.
#[test]
fn recv_handler_record_param_retained_into_state_suppresses_record_inplace_drop() {
    let pipeline = lower_source(
        r"
        type Boxed { payload: Vec<i64> }
        actor Sink {
            var seen: i64;
            var store: Vec<i64>;
            receive fn take(b: Boxed) {
                store = b.payload;
                seen = seen + store.len();
            }
            receive fn total() -> i64 { seen }
        }
        fn main() -> i64 {
            let sink = spawn Sink(seen: 0, store: []);
            sink.take(Boxed { payload: [1, 2, 3] });
            0
        }
        ",
    );
    assert!(
        !fn_has_record_inplace_drop(&pipeline, "Sink__recv__take"),
        "a receive-handler record whose owned field is retained into actor \
         state must NOT earn a RecordInPlace drop — the state_drop_fn is its \
         single free; admitting it double-frees: {:?}",
        pipeline.elaborated_mir
    );
}

/// A receive parameter forwarded to another mailbox has exactly one owner on
/// each transport edge. The handler moves the Vec into the prepared send
/// carrier, so neither the source parameter nor the carrier may remain in a
/// scope-exit drop plan. The `Send.cleanup_plan` is the carrier's sole local
/// release authority and codegen emits it only on the mutually-exclusive
/// transport-failure edge.
#[test]
fn recv_handler_forwarded_vec_param_has_no_scope_exit_double_release() {
    let pipeline = lower_source(
        r"
        actor Consumer {
            receive fn take(values: Vec<i64>) {}
        }
        actor Relay {
            let consumer: LocalPid<Consumer>;
            receive fn forward(values: Vec<i64>) {
                consumer.take(values);
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let raw = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "Relay__recv__forward")
        .expect("Relay.forward raw MIR present");
    let (send_value, cleanup_plan) = raw
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            Terminator::Send {
                value,
                cleanup_plan,
                ..
            } => Some((*value, cleanup_plan)),
            _ => None,
        })
        .expect("Relay.forward must terminate one block with a mailbox send");
    assert!(
        cleanup_plan.is_some(),
        "the send must carry one typed cleanup witness for the transport-error edge"
    );
    let param = Place::Local(0);
    assert!(
        raw.blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instr| matches!(
                instr,
                Instr::Move { dest, src } if *dest == send_value && *src == param
            )),
        "the receive parameter must move into the prepared send carrier: {:?}",
        raw.blocks
    );

    let elaborated = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == "Relay__recv__forward")
        .expect("Relay.forward elaborated MIR present");
    assert!(
        elaborated.drop_plans.iter().all(|(_, plan)| plan
            .drops
            .iter()
            .all(|drop| drop.place != param && drop.place != send_value)),
        "a delivered or accepted mailbox owner must not retain a handler-exit \
         release for the source or carrier: {:?}",
        elaborated.drop_plans
    );

    assert_eq!(
        raw.blocks
            .iter()
            .filter(|block| matches!(
                &block.terminator,
                Terminator::Send {
                    cleanup_plan: Some(_),
                    ..
                }
            ))
            .count(),
        1,
        "the single send terminator must be the sole carrier of the typed \
         undelivered-send cleanup authority: {:?}",
        raw.blocks
    );
}

/// A local owned record consumed into actor state on only one branch remains
/// owned by the handler on the sibling branch. Its scope-exit drop therefore
/// needs a path-sensitive guard: skip after the handoff, fire when the handoff
/// did not execute. Retracting the binding globally at the state-store site
/// leaks one string allocation per non-store invocation.
#[test]
fn conditional_record_state_handoff_keeps_guarded_scope_exit_drop() {
    let pipeline = lower_source(
        r"
        type Wrap { name: string }
        actor Fan {
            var held: Wrap;
            receive fn route(label: string, store: bool) -> i64 {
                let next = Wrap { name: label };
                if store { held = next; }
                7
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let handler = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "Fan__recv__route")
        .expect("conditional-handoff handler elaborated MIR present");
    let record_drops = handler
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.kind, hew_mir::DropKind::RecordInPlace))
        .collect::<Vec<_>>();
    let guard = record_drops.iter().find_map(|drop| drop.guard).expect(
        "the fresh record needs a guarded scope-exit drop for the sibling \
             non-consuming edge",
    );
    let checked = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "Fan__recv__route")
        .expect("conditional-handoff handler checked MIR present");
    for expected in [0, 1] {
        assert!(
            checked
                .blocks
                .iter()
                .flat_map(|block| &block.instructions)
                .any(|instr| matches!(
                    instr,
                    Instr::ConstI64 { dest, value }
                        if *dest == guard && *value == expected
                )),
            "the conditional record owner flag must be assigned {expected}: \
             {checked:#?}"
        );
    }
}

/// A mutable direct record that is consumed and then overwritten belongs only
/// to #2301's overwrite protocol. Giving it the conditional-record flag as
/// well lets assignment reset one family while the other still describes the
/// moved value, causing a shared-exit double release.
#[test]
fn mutable_conditional_record_consume_then_overwrite_uses_only_overwrite_protocol() {
    let pipeline = lower_source(
        r#"
        type Wrap { name: string }
        fn run(take: bool) -> i64 {
            var current = Wrap { name: "old".to_upper() };
            var seen: i64 = 0;
            if take {
                let moved: Wrap = current;
                seen = moved.name.len();
            }
            current = Wrap { name: "new".to_upper() };
            seen + current.name.len()
        }
        "#,
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "the mutable consume/reassign control must lower cleanly: {:?}",
        pipeline.diagnostics
    );
    let run = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "run")
        .expect("mutable consume/reassign function present");
    assert!(
        run.drop_plans
            .iter()
            .flat_map(|(_, plan)| &plan.drops)
            .filter(|drop| matches!(drop.kind, hew_mir::DropKind::RecordInPlace))
            .all(|drop| drop.guard.is_none()),
        "the mutable binding must not acquire a conditional RecordInPlace \
         guard in addition to #2301's overwrite guard: {run:#?}"
    );
    let checked = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "run")
        .expect("mutable consume/reassign checked MIR present");
    assert!(
        checked
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instr| matches!(instr, Instr::IntCmp { .. })),
        "the control must genuinely exercise #2301's guarded old-value \
         release before overwrite: {checked:#?}"
    );
}

/// Mailbox send currently retains/copies a record ingress rather than consuming
/// the source binding. Keep this shape outside the conditional-owner protocol:
/// the source remains the ordinary unguarded owner and the send carrier owns
/// its retained leaves.
#[test]
fn conditional_record_send_remains_outside_conditional_owner_protocol() {
    let pipeline = lower_source(
        r"
        type Wrap { name: string }
        actor Sink {
            receive fn take(value: Wrap) {}
        }
        fn route(sink: LocalPid<Sink>, label: string, send_it: bool) {
            let next = Wrap { name: label };
            if send_it {
                sink.take(next);
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "the conditional send control must lower cleanly: {:?}",
        pipeline.diagnostics
    );
    let route = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "route")
        .expect("conditional send function present");
    assert!(
        route
            .drop_plans
            .iter()
            .flat_map(|(_, plan)| &plan.drops)
            .filter(|drop| matches!(drop.kind, hew_mir::DropKind::RecordInPlace))
            .all(|drop| drop.guard.is_none()),
        "retain-backed send ingress must not acquire conditional consume \
         authority: {route:#?}"
    );
    assert!(
        route.statements.iter().any(|statement| matches!(
            statement,
            MirStatement::Use {
                name,
                intent: IntentKind::Read,
                ..
            } if name == "next"
        )),
        "the send control must pin the retain-backed Read ingress: {route:#?}"
    );
}

/// Aggregate embedding currently records an `AggregateAlias` rather than a
/// direct conditional consume of the source. Do not broaden W60.115 to this
/// shape without a separate alias-aware proof; fail closed by withholding the
/// conditional-record flag.
#[test]
fn conditional_record_aggregate_embedding_remains_fail_closed() {
    let pipeline = lower_source(
        r"
        type Wrap { name: string }
        type Envelope { value: Wrap }
        fn consume(envelope: Envelope) -> i64 {
            envelope.value.name.len()
        }
        fn route(label: string, embed: bool) -> i64 {
            let next = Wrap { name: label };
            if embed {
                consume(Envelope { value: next })
            } else {
                0
            }
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "the conditional aggregate control must lower cleanly: {:?}",
        pipeline.diagnostics
    );
    let route = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "route")
        .expect("conditional aggregate function present");
    assert!(
        route
            .drop_plans
            .iter()
            .flat_map(|(_, plan)| &plan.drops)
            .filter(|drop| matches!(drop.kind, hew_mir::DropKind::RecordInPlace))
            .all(|drop| drop.guard.is_none()),
        "aggregate-alias embedding must remain outside the conditional-owner \
         protocol until it has its own alias-aware proof: {route:#?}"
    );
    assert!(
        route.statements.iter().any(|statement| matches!(
            statement,
            MirStatement::AggregateAlias { name, .. } if name == "next"
        )),
        "the control must genuinely exercise aggregate embedding: {route:#?}"
    );
}

/// The conditional-record flag is deliberately narrower than the general
/// owned-record value class. A record containing a Vec does not prove that
/// every field was independently retained by the fresh construction, so a
/// branch-local consume must not re-admit its source drop behind this flag.
#[test]
fn conditional_record_handoff_with_vec_field_is_not_flagged() {
    let pipeline = lower_source(
        r"
        type Bag { name: string, values: Vec<i64> }
        actor Fan {
            var held: Bag;
            receive fn route(label: string, store: bool) -> i64 {
                let values: Vec<i64> = Vec::new();
                let next = Bag { name: label, values: values };
                if store { held = next; }
                7
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let handler = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "Fan__recv__route")
        .expect("Vec-record conditional-handoff handler present");
    assert!(
        handler
            .drop_plans
            .iter()
            .flat_map(|(_, plan)| &plan.drops)
            .filter(|drop| matches!(drop.kind, hew_mir::DropKind::RecordInPlace))
            .all(|drop| drop.guard.is_none()),
        "a record with a collection field must not enter the String/BitCopy \
         conditional-owner protocol: {handler:#?}"
    );
}

/// Merely rebinding a whole record does not establish the direct-construction
/// ingress proof. Keep it outside the conditional-record flag protocol.
#[test]
fn conditional_record_handoff_from_whole_value_rebind_is_not_flagged() {
    let pipeline = lower_source(
        r"
        type Wrap { name: string }
        actor Fan {
            var held: Wrap;
            receive fn route(input: Wrap, store: bool) -> i64 {
                let next = input;
                if store { held = next; }
                7
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let handler = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "Fan__recv__route")
        .expect("whole-value rebind handler present");
    assert!(
        handler
            .drop_plans
            .iter()
            .flat_map(|(_, plan)| &plan.drops)
            .filter(|drop| matches!(drop.kind, hew_mir::DropKind::RecordInPlace))
            .all(|drop| drop.guard.is_none()),
        "a whole-value rebind must not inherit direct RecordInit ownership \
         authority: {handler:#?}"
    );
}

/// A fresh record that never transfers remains the ordinary sole owner. It
/// still gets an unguarded `RecordInPlace` scope-exit drop.
#[test]
fn unconsumed_fresh_string_record_keeps_unguarded_drop() {
    let pipeline = lower_source(
        r#"
        type Wrap { name: string }
        fn main() -> i64 {
            let next = Wrap { name: "local".to_upper() };
            next.name.len()
        }
        "#,
    );
    let main = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main elaborated MIR present");
    assert!(
        main.drop_plans
            .iter()
            .flat_map(|(_, plan)| &plan.drops)
            .any(|drop| {
                matches!(drop.kind, hew_mir::DropKind::RecordInPlace) && drop.guard.is_none()
            }),
        "an unconsumed fresh record must retain its ordinary unguarded drop: \
         {main:#?}"
    );
}

/// A functional-update (`..user`) consumes the source record's fields into the
/// new record; both are owned-by-value and admitted. The source's owned field
/// moves out, so the new record owns it. Migrated forward from the reject test.
#[test]
fn user_record_string_functional_update_admits() {
    let pipeline = lower_source(
        r#"
        type User { name: string }
        fn main() -> i64 {
            let first = "Ada";
            let last = "Lovelace";
            let full = first + " " + last;
            let user = User { name: full };
            let updated = User { name: "Grace", ..user };
            println(updated.name);
            0
        }
        "#,
    );
    assert!(
        !source_has_w3029_in(&pipeline, "User"),
        "owned record functional-update must be admitted, not W3.029-rejected: {:?}",
        pipeline.diagnostics
    );
    assert!(
        pipeline_has_record_inplace_drop(&pipeline),
        "the functional-update result owns its fields and must earn a \
         RecordInPlace drop: {:?}",
        pipeline.elaborated_mir
    );
}

/// A record nested inside another record (`Boxed { user: User }`) is admitted:
/// the outer record's drop thunk recurses into the nested record field.
/// Migrated forward from the reject test.
#[test]
fn user_record_string_nested_record_admits() {
    let pipeline = lower_source(
        r#"
        type User { name: string }
        type Boxed { user: User }
        fn main() -> i64 {
            let first = "Ada";
            let last = "Lovelace";
            let full = first + " " + last;
            let user = User { name: full };
            let boxed = Boxed { user: user };
            println(boxed.user.name);
            0
        }
        "#,
    );
    assert!(
        !source_has_w3029_in(&pipeline, "User") && !source_has_w3029_in(&pipeline, "Boxed"),
        "nested owned record must be admitted, not W3.029-rejected: {:?}",
        pipeline.diagnostics
    );
    assert!(
        pipeline_has_record_inplace_drop(&pipeline),
        "the outer record owning a nested owned record must earn a RecordInPlace \
         drop (its thunk recurses into the nested field): {:?}",
        pipeline.elaborated_mir
    );
}

/// Helper: is there a W3.029 reject for `record_name` in this pipeline?
fn source_has_w3029_in(pipeline: &hew_mir::IrPipeline, record_name: &str) -> bool {
    pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::UnsupportedUserRecordValueClass { name, .. }
                if name == record_name
        )
    })
}

#[test]
fn record_field_closure_accepts_registered_enum_field_type() {
    // Mirrors the Stage-1-provable part of the CrashNotification.kind: CrashKind
    // route: the record field closure is walked, and a registered enum field type
    // is accepted as codegen-ready by MIR.
    let parsed = hew_parser::parse(
        r"
        type CrashNotification {
            kind: CrashKind
        }
        enum CrashKind { Fatal }
        fn main() -> i64 { 0 }
        ",
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let pipeline = lower_hir_module(&output.module);

    assert!(
        !pipeline.diagnostics.iter().any(|d| {
            matches!(d.kind, MirDiagnosticKind::UnknownType { ref name } if name == "CrashKind")
        }),
        "registered CrashKind enum field must be MIR-ready: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn record_field_closure_rejects_unready_user_field_type() {
    let parsed = hew_parser::parse(
        r"
        type Wrapper {
            missing: Missing
        }
        fn main() -> i64 { 0 }
        ",
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let pipeline = lower_hir_module(&output.module);

    assert!(
        pipeline.diagnostics.iter().any(|d| {
            matches!(d.kind, MirDiagnosticKind::UnknownType { ref name } if name == "Missing")
        }),
        "unready record field type must fail MIR field-closure readiness: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn nested_tuple_user_type_rejected_at_mir_boundary() {
    let parsed = hew_parser::parse("fn f(x: (Foo, i64)) -> (Foo, i64) { return x; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let pipeline = lower_hir_module(&output.module);

    assert!(
        pipeline.diagnostics.iter().any(|d| {
            matches!(d.kind, MirDiagnosticKind::UnknownType { ref name } if name == "Foo")
        }),
        "nested tuple Foo must produce UnknownType at MIR boundary: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn nested_array_user_type_rejected_at_mir_boundary() {
    let parsed = hew_parser::parse("fn f(x: [Foo; 2]) -> [Foo; 2] { return x; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let pipeline = lower_hir_module(&output.module);

    assert!(
        pipeline.diagnostics.iter().any(|d| {
            matches!(d.kind, MirDiagnosticKind::UnknownType { ref name } if name == "Foo")
        }),
        "nested array Foo must produce UnknownType at MIR boundary: {:?}",
        pipeline.diagnostics
    );
}

// ---------- Slice 0: bool literal + comparison binop lowering ----------
//
// CFG construction needs a constructible condition Place for `If`
// lowering. Without bool literals + comparison binops at MIR, every
// non-trivial `If` condition emits `NotYetImplemented` and the
// fixture corpus collapses to "integer non-zero" conditions only.
// These tests pin the seam.

#[test]
fn lower_bool_literal_true_emits_consti64_one() {
    let pipeline = pipeline("fn main() -> i64 { let r = true; 42 }");
    assert!(
        pipeline.diagnostics.is_empty(),
        "bool literal must lower cleanly: {:?}",
        pipeline.diagnostics
    );
    let func = &pipeline.raw_mir[0];
    let has_const_one = func.blocks[0]
        .instructions
        .iter()
        .any(|i| matches!(i, Instr::ConstI64 { value: 1, .. }));
    assert!(
        has_const_one,
        "bool literal `true` must lower to ConstI64 {{ value: 1, .. }}: {:#?}",
        func.blocks[0].instructions
    );
}

#[test]
fn lower_bool_literal_false_emits_consti64_zero() {
    let pipeline = pipeline("fn main() -> i64 { let r = false; 42 }");
    let func = &pipeline.raw_mir[0];
    let has_const_zero = func.blocks[0]
        .instructions
        .iter()
        .filter(|i| matches!(i, Instr::ConstI64 { value: 0, .. }))
        .count()
        >= 1;
    assert!(
        has_const_zero,
        "bool literal `false` must lower to ConstI64 {{ value: 0, .. }}: {:#?}",
        func.blocks[0].instructions
    );
}

#[test]
fn lower_equality_cmp_emits_intcmp_eq() {
    let pipeline = pipeline("fn main() -> i64 { let r = 1 == 1; 42 }");
    assert!(
        pipeline.diagnostics.is_empty(),
        "comparison binop must lower cleanly: {:?}",
        pipeline.diagnostics
    );
    let func = &pipeline.raw_mir[0];
    let has_eq = func.blocks[0].instructions.iter().any(|i| {
        matches!(
            i,
            Instr::IntCmp {
                pred: CmpPred::Eq,
                ..
            }
        )
    });
    assert!(
        has_eq,
        "`1 == 1` must lower to IntCmp(Eq): {:#?}",
        func.blocks[0].instructions
    );
}

#[test]
fn lower_all_six_comparison_preds() {
    // One fixture per CmpPred variant. Pins that the BinaryOp ->
    // CmpPred routing in lower_binary is complete.
    let cases: &[(&str, CmpPred)] = &[
        ("fn f() -> i64 { let r = 1 == 2; 0 }", CmpPred::Eq),
        ("fn f() -> i64 { let r = 1 != 2; 0 }", CmpPred::NotEq),
        ("fn f() -> i64 { let r = 1 < 2; 0 }", CmpPred::SignedLess),
        ("fn f() -> i64 { let r = 1 <= 2; 0 }", CmpPred::SignedLessEq),
        ("fn f() -> i64 { let r = 1 > 2; 0 }", CmpPred::SignedGreater),
        (
            "fn f() -> i64 { let r = 1 >= 2; 0 }",
            CmpPred::SignedGreaterEq,
        ),
    ];
    for (src, expected) in cases {
        let pipeline = pipeline(src);
        let func = &pipeline.raw_mir[0];
        let got = func.blocks[0]
            .instructions
            .iter()
            .find_map(|i| match i {
                Instr::IntCmp { pred, .. } => Some(*pred),
                _ => None,
            })
            .unwrap_or_else(|| panic!("no IntCmp emitted for {src}"));
        assert_eq!(got, *expected, "wrong CmpPred for {src}");
    }
}

#[test]
fn lower_unsigned_ordering_ops_select_unsigned_predicates() {
    // Unsigned integer ordering comparisons must emit the Unsigned* predicate
    // family, not the Signed* family.  A signed predicate on a high-bit-set
    // unsigned value (e.g. 0x8000…u64) would treat it as negative, producing
    // a silently wrong boolean.
    let cases: &[(&str, CmpPred)] = &[
        (
            "fn f() -> i64 { let a: u64 = 1; let b: u64 = 2; let r = a < b; 0 }",
            CmpPred::UnsignedLess,
        ),
        (
            "fn f() -> i64 { let a: u64 = 1; let b: u64 = 2; let r = a <= b; 0 }",
            CmpPred::UnsignedLessEq,
        ),
        (
            "fn f() -> i64 { let a: u64 = 1; let b: u64 = 2; let r = a > b; 0 }",
            CmpPred::UnsignedGreater,
        ),
        (
            "fn f() -> i64 { let a: u64 = 1; let b: u64 = 2; let r = a >= b; 0 }",
            CmpPred::UnsignedGreaterEq,
        ),
        (
            "fn f() -> i64 { let a: u32 = 1; let b: u32 = 2; let r = a < b; 0 }",
            CmpPred::UnsignedLess,
        ),
        (
            "fn f() -> i64 { let a: u8 = 1; let b: u8 = 2; let r = a > b; 0 }",
            CmpPred::UnsignedGreater,
        ),
    ];
    for (src, expected) in cases {
        let pipeline = pipeline(src);
        let func = &pipeline.raw_mir[0];
        let got = func.blocks[0]
            .instructions
            .iter()
            .find_map(|i| match i {
                Instr::IntCmp { pred, .. } => Some(*pred),
                _ => None,
            })
            .unwrap_or_else(|| panic!("no IntCmp emitted for {src}"));
        assert_eq!(got, *expected, "wrong CmpPred for {src}");
    }
}

#[test]
fn lower_signed_ordering_ops_still_select_signed_predicates() {
    // Signed integer ordering comparisons must continue to emit the Signed*
    // predicate family — the unsigned fix must not affect signed operands.
    let cases: &[(&str, CmpPred)] = &[
        ("fn f() -> i64 { let r = 1 < 2; 0 }", CmpPred::SignedLess),
        ("fn f() -> i64 { let r = 1 <= 2; 0 }", CmpPred::SignedLessEq),
        ("fn f() -> i64 { let r = 1 > 2; 0 }", CmpPred::SignedGreater),
        (
            "fn f() -> i64 { let r = 1 >= 2; 0 }",
            CmpPred::SignedGreaterEq,
        ),
        (
            "fn f() -> i64 { let a: i32 = 1; let b: i32 = 2; let r = a < b; 0 }",
            CmpPred::SignedLess,
        ),
        (
            "fn f() -> i64 { let a: i8 = 1; let b: i8 = 2; let r = a > b; 0 }",
            CmpPred::SignedGreater,
        ),
    ];
    for (src, expected) in cases {
        let pipeline = pipeline(src);
        let func = &pipeline.raw_mir[0];
        let got = func.blocks[0]
            .instructions
            .iter()
            .find_map(|i| match i {
                Instr::IntCmp { pred, .. } => Some(*pred),
                _ => None,
            })
            .unwrap_or_else(|| panic!("no IntCmp emitted for {src}"));
        assert_eq!(got, *expected, "wrong CmpPred for {src}");
    }
}

#[test]
fn lower_float_comparisons_emit_floatcmp_for_f64_and_f32() {
    let cases: &[(&str, CmpPred, FloatWidth)] = &[
        (
            "fn f() -> i64 { let a: f64 = 1.0; let b: f64 = 2.0; let r = a == b; 0 }",
            CmpPred::Eq,
            FloatWidth::F64,
        ),
        (
            "fn f() -> i64 { let a: f64 = 1.0; let b: f64 = 2.0; let r = a != b; 0 }",
            CmpPred::NotEq,
            FloatWidth::F64,
        ),
        (
            "fn f() -> i64 { let a: f64 = 1.0; let b: f64 = 2.0; let r = a < b; 0 }",
            CmpPred::SignedLess,
            FloatWidth::F64,
        ),
        (
            "fn f() -> i64 { let a: f64 = 1.0; let b: f64 = 2.0; let r = a <= b; 0 }",
            CmpPred::SignedLessEq,
            FloatWidth::F64,
        ),
        (
            "fn f() -> i64 { let a: f64 = 1.0; let b: f64 = 2.0; let r = a > b; 0 }",
            CmpPred::SignedGreater,
            FloatWidth::F64,
        ),
        (
            "fn f() -> i64 { let a: f64 = 1.0; let b: f64 = 2.0; let r = a >= b; 0 }",
            CmpPred::SignedGreaterEq,
            FloatWidth::F64,
        ),
        (
            "fn f() -> i64 { let a: f32 = 1.0; let b: f32 = 2.0; let r = a < b; 0 }",
            CmpPred::SignedLess,
            FloatWidth::F32,
        ),
    ];
    for (src, expected_pred, expected_width) in cases {
        let pipeline = pipeline(src);
        let func = &pipeline.raw_mir[0];
        let (got_pred, got_width) = func.blocks[0]
            .instructions
            .iter()
            .find_map(|i| match i {
                Instr::FloatCmp { pred, width, .. } => Some((*pred, *width)),
                _ => None,
            })
            .unwrap_or_else(|| panic!("no FloatCmp emitted for {src}"));
        assert_eq!(got_pred, *expected_pred, "wrong FloatCmp pred for {src}");
        assert_eq!(got_width, *expected_width, "wrong FloatCmp width for {src}");
        assert!(
            !func.blocks[0]
                .instructions
                .iter()
                .any(|i| matches!(i, Instr::IntCmp { .. })),
            "float comparison must not lower to IntCmp for {src}: {:#?}",
            func.blocks[0].instructions
        );
    }
}

#[test]
fn lower_unsupported_binop_fails_closed_with_diagnostic() {
    // Previously `lower_binary` silently popped the dest local and
    // returned None for any non-{Add,Sub,Mul} binop, letting the
    // caller's `decide` run while the emitter produced no instruction
    // — a quiet fail-soft. Now the unsupported branch emits a
    // `NotYetImplemented` so the CLI rejection surface catches the
    // construct.
    //
    // Bitwise (&, |, ^) and shift (<<, >>), divide, and modulo are now
    // all wired. This test verifies the complementary property: that
    // implemented operators do NOT emit NotYetImplemented. Regression
    // against the earlier fail-soft: if lower_binary were to accidentally
    // fall through to the catch-all for a wired operator, no instruction
    // would be emitted and a NotYetImplemented would appear.
    for src in [
        "fn main() -> i64 { let a: i64 = 1; let b: i64 = 2; a & b }",
        "fn main() -> i64 { let a: i64 = 1; let b: i64 = 2; a | b }",
        "fn main() -> i64 { let a: i64 = 1; let b: i64 = 2; a ^ b }",
    ] {
        let parsed = hew_parser::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let output = lower_program(
            &parsed.program,
            &TypeCheckOutput::default(),
            &ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
        let pipeline = lower_hir_module(&output.module);
        assert!(
            pipeline.diagnostics.is_empty(),
            "Implemented bitwise op must not emit NotYetImplemented: \
             {src} => {:?}",
            pipeline.diagnostics
        );
    }
}

#[test]
fn divide_lowers_cleanly_with_trap_edges() {
    // B-5: integer `/` is now wired — it must lower without a
    // NotYetImplemented diagnostic and produce a DivideByZero trap edge.
    let parsed = hew_parser::parse("fn main() -> i64 { let r = 1 / 2; 0 }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "Divide should parse + lower cleanly through HIR: {:?}",
        output.diagnostics
    );
    let pipeline = lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "Divide must lower without NotYetImplemented after B-5: {:?}",
        pipeline.diagnostics
    );
    // At least one DivideByZero trap block must be present.
    let has_dbz_trap = pipeline.raw_mir[0].blocks.iter().any(|b| {
        matches!(
            b.terminator,
            Terminator::Trap {
                kind: TrapKind::DivideByZero
            }
        )
    });
    assert!(
        has_dbz_trap,
        "Divide must produce a DivideByZero trap block after B-5"
    );
}

// ---------- Slice 2: CFG construction for HirExprKind::If ----------
//
// Real If lowering builds a Branch on the entry block; then/else arm
// blocks each Goto a join block; the join block carries the result
// local that each arm Move'd into. The CFG shape is observable on
// RawMirFunction.blocks.

#[test]
fn if_expression_builds_four_blocks() {
    // entry (Branch) + then (Goto) + else (Goto) + join (Return) = 4
    let p = pipeline("fn main() -> i64 { let r = if 1 == 1 { 7 } else { 8 }; r }");
    assert!(
        p.diagnostics.is_empty(),
        "If expression must lower cleanly: {:?}",
        p.diagnostics
    );
    let func = &p.raw_mir[0];
    assert_eq!(
        func.blocks.len(),
        4,
        "If expression must produce four blocks (entry, then, else, join); got {} blocks: {:#?}",
        func.blocks.len(),
        func.blocks
            .iter()
            .map(|b| &b.terminator)
            .collect::<Vec<_>>()
    );
}

#[test]
fn if_expression_entry_block_terminates_with_branch() {
    let p = pipeline("fn main() -> i64 { let r = if 1 == 1 { 7 } else { 8 }; r }");
    let func = &p.raw_mir[0];
    match &func.blocks[0].terminator {
        Terminator::Branch {
            then_target,
            else_target,
            ..
        } => {
            assert_ne!(then_target, else_target, "branch targets must differ");
        }
        other => panic!("entry block must end in Branch; got {other:?}"),
    }
}

#[test]
fn if_expression_arm_blocks_goto_join() {
    let p = pipeline("fn main() -> i64 { let r = if 1 == 1 { 7 } else { 8 }; r }");
    let func = &p.raw_mir[0];
    let (then_target, else_target) = match &func.blocks[0].terminator {
        Terminator::Branch {
            then_target,
            else_target,
            ..
        } => (*then_target, *else_target),
        _ => panic!("expected Branch"),
    };
    let then_block = func.blocks.iter().find(|b| b.id == then_target).unwrap();
    let else_block = func.blocks.iter().find(|b| b.id == else_target).unwrap();
    let Terminator::Goto { target: then_goto } = then_block.terminator else {
        panic!("then arm must Goto join")
    };
    let Terminator::Goto { target: else_goto } = else_block.terminator else {
        panic!("else arm must Goto join")
    };
    assert_eq!(then_goto, else_goto, "both arms must Goto the same join");
}

#[test]
fn if_expression_arm_blocks_write_result_local() {
    // Each arm's tail value is Move'd into the result local; the join
    // block's value Place is the result local. Pin the alloca-result-
    // local pattern is in use (no phi at MIR layer).
    let p = pipeline("fn main() -> i64 { let r = if 1 == 1 { 7 } else { 8 }; r }");
    let func = &p.raw_mir[0];
    let (then_target, else_target) = match &func.blocks[0].terminator {
        Terminator::Branch {
            then_target,
            else_target,
            ..
        } => (*then_target, *else_target),
        _ => panic!(),
    };
    for target in [then_target, else_target] {
        let block = func.blocks.iter().find(|b| b.id == target).unwrap();
        let has_move = block
            .instructions
            .iter()
            .any(|i| matches!(i, Instr::Move { .. }));
        assert!(
            has_move,
            "arm block {} must emit Move into result local; got {:#?}",
            target, block.instructions
        );
    }
}

#[test]
fn if_expression_join_block_terminates_with_return() {
    // The trailing `r` in `... else { 8 }; r` reads the result local
    // in the join block; the function-tail Return terminator lives on
    // the join block (now the cursor's block at function_body
    // finalisation).
    let p = pipeline("fn main() -> i64 { let r = if 1 == 1 { 7 } else { 8 }; r }");
    let func = &p.raw_mir[0];
    let return_blocks: Vec<_> = func
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Return))
        .collect();
    assert_eq!(
        return_blocks.len(),
        1,
        "exactly one Return-terminated block per simple If: {:#?}",
        func.blocks
            .iter()
            .map(|b| &b.terminator)
            .collect::<Vec<_>>()
    );
}

#[test]
fn if_no_else_unit_typed_lowers_without_diagnostic() {
    // HIR types `if x { 7 }` (no else) as Unit. Our `lower_if` accepts
    // `else_expr: None` and emits an else block that Goto's join with
    // no Move. No NotYetImplemented here.
    let p = pipeline("fn main() -> i64 { let _r = if 1 == 1 { 7 }; 42 }");
    assert!(
        p.diagnostics.is_empty(),
        "else-less If must lower cleanly: {:?}",
        p.diagnostics
    );
    let func = &p.raw_mir[0];
    assert_eq!(
        func.blocks.len(),
        4,
        "else-less If still produces entry/then/else/join CFG: {} blocks",
        func.blocks.len()
    );
}

#[test]
fn sequential_ifs_each_contribute_three_blocks() {
    // Two sequential let-init Ifs in the same function. Each
    // contributes a 3-block split (then + else + join); the second
    // If's entry block is the first If's join block. The trailing
    // `a + b` is an integer `+` and lowers under B-2 to a checked-
    // arithmetic instruction plus an extra Branch on the overflow
    // flag (trap-block + continuation-block successors). So the
    // function has: entry (Branch of If1), then1, else1, join1 (=
    // entry of If2's Branch), then2, else2, join2 (which finishes
    // by emitting the `a + b` checked op and is terminated by the
    // overflow Branch), trap-bb, cont-bb. 9 blocks total.
    // Pins that the cursor correctly continues lowering into the
    // join block after one If and that subsequent `alloc_block`
    // calls (the trap/cont pair from `+`) remain monotone past the
    // If chain.
    let p = pipeline(
        "fn main() -> i64 { \
            let a = if 1 == 1 { 7 } else { 8 }; \
            let b = if 1 == 0 { 9 } else { 10 }; \
            a + b \
        }",
    );
    assert!(
        p.diagnostics.is_empty(),
        "sequential Ifs must lower cleanly: {:?}",
        p.diagnostics
    );
    let func = &p.raw_mir[0];
    assert_eq!(
        func.blocks.len(),
        9,
        "sequential Ifs + trailing `+` produce 9 blocks (2 Ifs × 3 + trap + cont \
         + the join shared between If2 and the `+`): {:#?}",
        func.blocks
            .iter()
            .map(|b| (b.id, &b.terminator))
            .collect::<Vec<_>>()
    );
    // Exactly one Return (the post-`+` continuation block, which is
    // where the function tail value lives now that `+` introduces
    // its own CFG split).
    let returns = func
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Return))
        .count();
    assert_eq!(returns, 1, "exactly one Return on linear If chain");
    // Three Branches: one per If, plus the overflow Branch from the
    // trailing `a + b` under B-2's trap-on-overflow lowering.
    let branches = func
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Branch { .. }))
        .count();
    assert_eq!(branches, 3);
    // Exactly one IntegerOverflow trap (from the `a + b`).
    let traps = func
        .blocks
        .iter()
        .filter(|b| {
            matches!(
                b.terminator,
                Terminator::Trap {
                    kind: hew_mir::TrapKind::IntegerOverflow
                }
            )
        })
        .count();
    assert_eq!(traps, 1, "trailing `+` emits one IntegerOverflow trap");
}

// ---------- Slice 3: per-block dataflow over the 4-state lattice ----------
//
// The new dataflow path makes branch-sensitive consume-tracking real.
// The flat-stream scan that worked under single-block MIR could not
// distinguish "consumed on one arm, used after join" from
// "consumed-then-used in the same arm" (both flatten the same way).
// Under the lattice, the former produces a MaybeConsumed state at
// the join, and a subsequent use is UseAfterConsume.

#[test]
fn cross_arm_consume_then_use_after_join_rejects() {
    // Consume `s` in the then arm only. After the join, `s`'s state
    // is MaybeConsumed (one path Consumed, one path Live). The next
    // use of `s` must be flagged as UseAfterConsume.
    let p = lower_source(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(self) { } }
        fn main() -> i64 {
            let s = File { fd: 1 };
            let _r = if true { let _t = s; 7 } else { 8 };
            s.fd
        }
    ",
    );
    assert!(
        p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "s"
        )),
        "cross-arm consume + post-join use must surface UseAfterConsume: {:?}",
        p.diagnostics
    );
}

#[test]
fn consume_in_both_arms_then_use_after_join_rejects() {
    // Consume in BOTH arms: state at join is Consumed. Post-join
    // use rejects as plain UseAfterConsume (not the MaybeConsumed
    // variant).
    let p = lower_source(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(self) { } }
        fn main() -> i64 {
            let s = File { fd: 1 };
            let _r = if true { let _a = s; 7 } else { let _b = s; 8 };
            s.fd
        }
    ",
    );
    assert!(
        p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "s"
        )),
        "both-arm consume + post-join use must reject: {:?}",
        p.diagnostics
    );
}

#[test]
fn consume_in_both_arms_without_post_join_use_is_accepted() {
    // Consumed on every reachable path, never used after the join.
    // The dataflow must NOT emit UseAfterConsume here.
    let p = lower_source(
        r"
        #[resource]
        type File { fd: i64 }
        impl File { fn close(self) { } }
        fn main() -> i64 {
            let s = File { fd: 1 };
            let _r = if true { let _a = s; 7 } else { let _b = s; 8 };
            0
        }
    ",
    );
    assert!(
        !p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "s"
        )),
        "both-arm consume without post-join use must NOT fire UseAfterConsume: {:?}",
        p.diagnostics
    );
}

#[test]
fn maybe_consumed_at_join_without_post_join_use_is_accepted() {
    // `s` is consumed in the then arm, Live in the else arm. At the
    // join its state is MaybeConsumed. With no post-join use of `s`,
    // neither UseAfterConsume nor InitialisedBeforeUse should fire.
    // Pins the `Consumed ⊓ Live = MaybeConsumed` lattice cell
    // together with the "no diagnostic without a post-join use"
    // boundary.
    //
    // The `Uninit ⊓ Live = Uninit` cell (binding initialised only on
    // one arm, read after join) is not reachable from Hew source today:
    // HIR has no assignment statement (`HirStmtKind` is `Let | Expr |
    // Return`), so a binding declared before an `if` cannot be
    // conditionally initialised inside an arm. If an assignment surface
    // is added in a future version, add a companion reject test here.
    let p = pipeline(
        r#"fn main() -> i64 {
            let s = "hello";
            let _r = if 1 == 1 { let _a = s; 7 } else { 8 };
            0
        }"#,
    );
    // `s` is MaybeConsumed at the join; no post-join use → no diagnostic.
    assert!(
        !p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { .. }
                | MirDiagnosticKind::InitialisedBeforeUse { .. }
        )),
        "MaybeConsumed-without-post-join-use must accept: {:?}",
        p.diagnostics
    );
}

#[test]
fn linear_consumed_in_both_branches_accepted() {
    // @linear binding consumed in BOTH arms via consuming method.
    // Without a method-call surface today, we approximate via
    // `let _t = txn; ...` in both arms (consume by move). Slice 5's
    // fixtures pin the method-consume form once the surface arrives.
    let src = r"
        #[linear]
        type Txn {
            id: i64
            fn commit(consuming self) -> i64 { 0 }
        }
        fn main() -> i64 {
            let t = Txn { id: 0 };
            let _r = if 1 == 1 { let _a = t; 7 } else { let _b = t; 8 };
            42
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        !output.diagnostics.iter().any(|d| matches!(
            d.kind,
            HirDiagnosticKind::LinearNoConsumingMethods { .. }
                | HirDiagnosticKind::ResourceMissingClose { .. }
        )),
        "HIR must accept the type decl: {:?}",
        output.diagnostics
    );
    let p = lower_hir_module(&output.module);
    let func = p
        .checked_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main is in checked_mir");
    let has_must_consume = func.checks.iter().any(|c| {
        matches!(
            c,
            MirCheck::MustConsume { name, .. } if name == "t"
        )
    });
    assert!(
        !has_must_consume,
        "@linear binding consumed on every path must NOT fire MustConsume: {:?}",
        func.checks
    );
}

#[test]
fn linear_consuming_method_retires_initializer_publication_owner() {
    let pipeline = lower_source_checked_verified(
        r"
        #[linear]
        type Tx { id: i64 }
        impl Tx {
            fn commit(consuming self) {}
        }
        fn main() {
            let tx = Tx { id: 1 };
            tx.commit();
        }
        ",
    );
    assert!(
        !pipeline.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            MirDiagnosticKind::MustConsume { name, .. } if name == "__hew_produced_value"
        )),
        "the initializer publication transfers into `tx`; it must not leave a synthetic linear owner: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn generic_loop_accumulator_keeps_its_pre_loop_initialisation() {
    let pipeline = lower_source(
        r"
        enum Step { More(i64); Done; }
        fn next() -> Step { Step::Done }
        fn fold<B>(init: B, f: fn(B, i64) -> B) -> B {
            var acc = init;
            loop {
                match next() {
                    Step::More(item) => {
                        acc = f(acc, item);
                    },
                    Step::Done => {
                        break;
                    },
                }
            }
            acc
        }
        fn step(value: i64, item: i64) -> i64 { value + item }
        fn main() -> i64 { fold(1, step) }
        ",
    );
    assert!(
        !pipeline.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            MirDiagnosticKind::InitialisedBeforeUse { name, .. } if name == "acc"
        )),
        "a pre-loop initializer dominates the loop exit: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn linear_consumed_only_in_then_branch_rejects() {
    // The plan's canary fixture (linear_consumed_only_some_branches).
    // @linear binding consumed in then arm only; else arm leaves it
    // Live; at the Return-terminated join, state is MaybeConsumed.
    // The per-exit MustConsume check fires.
    let src = r"
        #[linear]
        type Txn {
            id: i64
            fn commit(consuming self) -> i64 { 0 }
        }
        fn main() -> i64 {
            let t = Txn { id: 0 };
            let _r = if 1 == 1 { let _a = t; 7 } else { 8 };
            42
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let p = lower_hir_module(&output.module);
    let func = p
        .checked_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main is in checked_mir");
    assert!(
        func.checks.iter().any(|c| matches!(
            c,
            MirCheck::MustConsume { name, .. } if name == "t"
        )),
        "@linear binding consumed on only one branch must fire MustConsume \
         from MaybeConsumed-at-Return: {:?}",
        func.checks
    );
}

#[test]
fn lower_string_literal_emits_string_lit_instr() {
    // `let s = "hello"` must lower to `Instr::StringLit` with the decoded
    // bytes `b"hello"` in the raw MIR instruction stream.
    let pipeline = pipeline(r#"fn main() -> i64 { let s = "hello"; 0 }"#);
    assert!(
        pipeline.diagnostics.is_empty(),
        "string literal must lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
    let func = &pipeline.raw_mir[0];
    let found = func.blocks[0].instructions.iter().find_map(|i| match i {
        Instr::StringLit { bytes, dest } => Some((bytes.clone(), *dest)),
        _ => None,
    });
    let (bytes, dest) =
        found.expect("string literal `\"hello\"` must produce Instr::StringLit in raw MIR");
    assert_eq!(
        bytes, b"hello",
        "Instr::StringLit bytes must be the decoded UTF-8 literal"
    );
    assert!(
        matches!(dest, Place::Local(_)),
        "Instr::StringLit dest must be a Local place"
    );
}

#[test]
fn lower_escaped_string_literal_carries_decoded_bytes() {
    // Escape sequences are decoded by the parser; MIR must carry the
    // decoded bytes, not the source escape notation.
    let pipeline = pipeline(r#"fn main() -> i64 { let s = "foo\nbar"; 0 }"#);
    assert!(
        pipeline.diagnostics.is_empty(),
        "escaped string literal must lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
    let func = &pipeline.raw_mir[0];
    let bytes = func.blocks[0]
        .instructions
        .iter()
        .find_map(|i| match i {
            Instr::StringLit { bytes, .. } => Some(bytes.clone()),
            _ => None,
        })
        .expect("escaped string literal must produce Instr::StringLit");
    // Parser decodes `\n` to a single newline byte (0x0A).
    assert_eq!(
        bytes, b"foo\nbar",
        "Instr::StringLit bytes must carry the decoded byte, not the escape notation"
    );
}

/// Run a source through the full checker + HIR lowering + HIR verifier, then
/// MIR-lower it, asserting NO diagnostic fires at any stage. Used to pin
/// error-prop lowerings that previously typechecked cleanly but failed closed
/// downstream (HIR verify / MIR lowering).
fn lower_source_checked_verified(src: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(src);
    assert!(parsed.errors.is_empty(), "parse: {:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "typecheck: {:?}",
        tc_output.errors
    );
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "hir lower: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "hir verify: {verify:?}");
    let pipeline = lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir lower: {:?}",
        pipeline.diagnostics
    );
    pipeline
}

#[test]
fn let_else_nested_tuple_payload_escapes_leaf_binders() {
    // `let Ok((n, s)) = e else { return … };` must destructure the tuple and
    // bind BOTH `n` and `s` into the enclosing scope. Pre-fix the leaf binders
    // were dropped, so the rest of the block referenced unbound names and the
    // HIR verifier rejected them with `identifier has no binding`. This pins
    // the whole pipeline staying diagnostic-free.
    lower_source_checked_verified(
        r#"
        fn make_pair(good: bool) -> Result<(i64, string), string> {
            if good { Ok((7, "p")) } else { Err("no") }
        }
        fn parse(good: bool) -> Result<i64, string> {
            let Ok((n, s)) = make_pair(good) else { return Err("fallback") };
            let _ = s;
            Ok(n + 35)
        }
        fn main() { let _ = parse(true); }
        "#,
    );
}

#[test]
fn unannotated_let_rhs_if_with_return_branch_infers_value_type() {
    // An unannotated `let x = if c { v } else { return … }` must carry the
    // value branch's type (the diverging else is `Never`, not `Unit`), so a
    // later `x + 1` lowers as integer arithmetic. Pre-fix the else block typed
    // `Unit`, the binding mis-typed, and `x + 1` failed closed at MIR with
    // "binary + on non-integer type".
    lower_source_checked_verified(
        r#"
        fn adjust(n: i64) -> Result<i64, string> {
            let adjusted = if n > 0 { n } else { return Err("non-positive") };
            Ok(adjusted + 1)
        }
        fn main() { let _ = adjust(1); }
        "#,
    );
}

#[test]
fn return_in_binary_operand_position_infers_value_type() {
    // The same Never-branch inference must hold when the `if`/`else` sits
    // directly in a binary-operand position: `ten + if c { … } else { return … }`.
    lower_source_checked_verified(
        r#"
        fn combine(n: i64) -> Result<i64, string> {
            let ten = 10;
            Ok(ten + if n == 1 { n + 1 } else { return Err("bad-arg") })
        }
        fn main() { let _ = combine(1); }
        "#,
    );
}
