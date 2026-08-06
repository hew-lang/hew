//! The shell-drop-safety cap on the `string_binder_read_is_user_fn_borrow`
//! exemption must be a POSITIVE predicate over payload classes whose shell
//! drop cannot double-release, not "owns no heap".
//!
//! `EnumInPlace` seeds the enum clone/drop helper synthesis. `Stream<T>` and
//! `Sink<T>` are pointer-backed IO handles with real close symbols — yet the
//! MIR heap authority's builtin leaf set omits them and its generic `Named`
//! arm only recurses into type arguments, so `Stream<i64>` / `Sink<i64>`
//! answer "owns no heap". The old `String || !ty_owns_heap_mir` spelling
//! therefore re-admitted exactly the composites the cap exists to exclude.
//!
//! A BARE `#[opaque]` handle sibling IS admitted: its thunk drop is a
//! structural no-op with no other close authority anywhere (double-free
//! impossible by construction), and codegen emits a trap-body clone for an
//! opaque-carrying enum seeded only for its drop helper. A
//! lifecycle-registered (`#[resource]`) handle still classifies `Resource`
//! and stays refused — a second close of a real resource is observable (the
//! S2200 double-close class).
use super::*;

fn builtin(name: &str, args: Vec<ResolvedTy>) -> ResolvedTy {
    ResolvedTy::Named {
        name: name.to_string(),
        args,
        builtin: None,
        is_opaque: false,
    }
}

fn opaque(name: &str) -> ResolvedTy {
    ResolvedTy::Named {
        name: name.to_string(),
        args: vec![],
        builtin: None,
        is_opaque: true,
    }
}

/// A two-variant `Result`-shaped layout named for the mangled key the
/// lookup resolves, with `ok` in the first variant and `string` in the
/// second.
fn result_layout(name: &str, ok: Vec<ResolvedTy>) -> crate::model::EnumLayout {
    crate::model::EnumLayout {
        name: name.to_string(),
        tag_width: 1,
        variants: vec![
            crate::model::MachineVariantLayout {
                name: "Ok".to_string(),
                field_tys: ok,
                field_names: vec![],
            },
            crate::model::MachineVariantLayout {
                name: "Err".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }
}

fn admits_with_registry(ok: Vec<ResolvedTy>, registry: &hew_hir::LifecycleRegistry) -> bool {
    let args = vec![
        ok.first().cloned().unwrap_or(ResolvedTy::Unit),
        ResolvedTy::String,
    ];
    let ty = builtin("Result", args.clone());
    let key = crate::lower::mangle_layout_key("Result", &args);
    enum_payloads_are_shell_drop_safe(&ty, &[result_layout(&key, ok)], &[], &[], registry)
}

fn admits(ok: Vec<ResolvedTy>) -> bool {
    admits_with_registry(ok, &hew_hir::LifecycleRegistry::default())
}

#[test]
fn plain_string_payloads_are_admitted() {
    assert!(
        admits(vec![ResolvedTy::String]),
        "the f-string interpolation fix must survive: `Result<string, string>` \
         is the shape the exemption exists for"
    );
}

#[test]
fn scalar_payloads_are_admitted() {
    assert!(
        admits(vec![ResolvedTy::I64]),
        "a scalar payload is a genuine bit-copy leaf"
    );
    assert!(
        admits(vec![ResolvedTy::Tuple(vec![
            ResolvedTy::I64,
            ResolvedTy::Bool
        ])]),
        "a tuple of scalars is bit-copy through"
    );
}

#[test]
fn scalar_argument_io_handle_payloads_are_refused() {
    // The load-bearing case: SCALAR type arguments, so the heap authority
    // answers "owns no heap" for both handles. Only a positive bit-copy
    // predicate rejects them.
    let stream = builtin("Stream", vec![ResolvedTy::I64]);
    let sink = builtin("Sink", vec![ResolvedTy::I64]);
    assert!(
        !crate::model::ty_owns_heap_mir(&stream, &HashMap::new(), &[]),
        "guard: the heap authority does NOT see Stream<i64> as heap-owning — \
         that is exactly why `!ty_owns_heap_mir` was the wrong predicate"
    );
    assert!(
        !admits(vec![ResolvedTy::Tuple(vec![stream, sink])]),
        "`Result<(Stream<i64>, Sink<i64>), string>` must stay OUT of the \
         exemption: its `EnumInPlace` drop would seed a clone helper the \
         IoHandle class cannot synthesise"
    );
}

#[test]
fn string_argument_io_handle_payloads_are_refused() {
    assert!(
        !admits(vec![ResolvedTy::Tuple(vec![
            builtin("Stream", vec![ResolvedTy::String]),
            builtin("Sink", vec![ResolvedTy::String]),
        ])]),
        "the heap-argument spelling stays refused too"
    );
}

#[test]
fn bare_opaque_payload_sibling_is_admitted() {
    // The hybrid-enum callee-drop shape: `Mixed { Text(string); Opaque(Handle) }`
    // passed by value declines the snapshot carrier, so the tag-aware
    // `EnumInPlace` shell drop is the callee's ONE balancing release. The
    // bare-opaque variant's thunk drop is a structural no-op with no other
    // close authority anywhere, so admitting it cannot double-release.
    assert!(
        admits(vec![opaque("Handle")]),
        "a bare `#[opaque]` handle sibling must not exclude the shell drop — \
         excluding it leaks BOTH variants of the hybrid enum on every call"
    );
}

#[test]
fn lifecycle_registered_resource_beside_string_is_refused() {
    // Rule-17 MIXED case: an affine `#[resource]` leaf sitting BESIDE the
    // clone-drop-safe string leaf. The shell drop would run the registered
    // close a second time (the S2200 double-close class) — the registry entry
    // is exactly what demotes the same spelling from "bare opaque" to
    // "resource", so the two cases differ ONLY by the registry.
    let mut registry = hew_hir::LifecycleRegistry::default();
    registry
        .admit_opaque_resource(hew_hir::OpaqueResourceLifecycle {
            resource_declaration: hew_types::DefId::new("Handle"),
            close_declaration: hew_types::DefId::new("Handle::close"),
            release_declaration: hew_types::DefId::new("Handle::close"),
            close_symbol: "Handle::close".to_string(),
            release_symbol: "Handle::close".to_string(),
            discharge_depth: hew_types::ffi_contracts::ReleaseDischargeDepth::Shallow,
            producer_declarations: std::collections::BTreeSet::new(),
            producer_symbols: std::collections::BTreeSet::new(),
            producer_modules: std::collections::BTreeSet::new(),
        })
        .expect("unique test lifecycle");
    assert!(
        admits_with_registry(
            vec![opaque("Handle")],
            &hew_hir::LifecycleRegistry::default()
        ),
        "guard: without the registry entry the same spelling is a bare opaque \
         handle and is admitted — proving the registry is the discriminator"
    );
    assert!(
        !admits_with_registry(vec![opaque("Handle")], &registry),
        "a lifecycle-registered resource has a REAL close; the shell drop \
         beside the string leaf must stay refused (S2200 double-close class)"
    );
}

#[test]
fn record_payloads_are_refused() {
    assert!(
        !admits(vec![builtin("Connection", vec![])]),
        "an unregistered user nominal is not a proven-inert payload and \
         stays refused (fail-closed: it keeps leaking, exactly as before)"
    );
    assert!(
        !admits(vec![builtin("Rec", vec![])]),
        "a user record payload is not a bit-copy leaf and stays refused \
         (fail-closed: it keeps leaking, exactly as before, and compiles)"
    );
}

#[test]
fn an_unresolvable_layout_is_refused() {
    assert!(
        !enum_payloads_are_shell_drop_safe(
            &builtin("Result", vec![ResolvedTy::String]),
            &[],
            &[],
            &[],
            &hew_hir::LifecycleRegistry::default(),
        ),
        "no layout means no proof"
    );
}
