//! The clone-safety cap on the `string_binder_read_is_user_fn_borrow`
//! exemption must be a POSITIVE bit-copy predicate, not "owns no heap".
//!
//! `EnumInPlace` seeds the enum clone/drop helper synthesis, and clone
//! totality refuses every `IoHandle` / closure-pair / `#[resource]` /
//! opaque-handle class. `Stream<T>` and `Sink<T>` are pointer-backed IO
//! handles with no duplication helper — yet the MIR heap authority's builtin
//! leaf set omits them and its generic `Named` arm only recurses into type
//! arguments, so `Stream<i64>` / `Sink<i64>` answer "owns no heap". The old
//! `String || !ty_owns_heap_mir` spelling therefore re-admitted exactly the
//! composites the cap exists to exclude, re-opening the clone-synthesis
//! refusal.
//!
//! A NESTED enum payload (`Result<Status, i64>` whose `Ok` payload is itself a
//! scalar-and-string enum) recurses: its `EnumInPlace` clone half walks the
//! nested enum's own helper, so it is synthesizable iff the nested payloads
//! are. The plain-string-only spelling rejected it on its `Named` payload and
//! leaked the inner `string` sibling (#2717); a nested enum carrying an
//! IoHandle/resource/record payload still stays refused.
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

fn admits(ok: Vec<ResolvedTy>) -> bool {
    let args = vec![
        ok.first().cloned().unwrap_or(ResolvedTy::Unit),
        ResolvedTy::String,
    ];
    let ty = builtin("Result", args.clone());
    let key = crate::lower::mangle_layout_key("Result", &args);
    enum_payloads_are_clone_synthesizable(&ty, &[result_layout(&key, ok)])
}

/// A single-argument `Status`-shaped nested enum: one scalar variant and one
/// `string` variant, named for the mangled key the outer `Ok` payload resolves.
fn status_layout(name: &str, scalar_or_handle: ResolvedTy) -> crate::model::EnumLayout {
    crate::model::EnumLayout {
        name: name.to_string(),
        tag_width: 1,
        variants: vec![
            crate::model::MachineVariantLayout {
                name: "Loaded".to_string(),
                field_tys: vec![scalar_or_handle],
                field_names: vec![],
            },
            crate::model::MachineVariantLayout {
                name: "Described".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }
}

/// `Result<Status, i64>` where `Status` is the nested enum built from
/// `inner_payload` and a `string`. Resolves both the outer `Result` layout and
/// the nested `Status` layout so the recursion has both to walk.
fn admits_nested(inner_payload: ResolvedTy) -> bool {
    let status = builtin("Status", vec![]);
    let outer_args = vec![status.clone(), ResolvedTy::I64];
    let ty = builtin("Result", outer_args.clone());
    let result_key = crate::lower::mangle_layout_key("Result", &outer_args);
    // `find_enum_layout` keys an empty-args `Named` on the bare name, so the
    // nested `Status` layout is named directly.
    enum_payloads_are_clone_synthesizable(
        &ty,
        &[
            result_layout(&result_key, vec![status]),
            status_layout("Status", inner_payload),
        ],
    )
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
fn opaque_and_resource_payloads_are_refused() {
    assert!(
        !admits(vec![opaque("Value")]),
        "an `#[opaque]` handle has no duplication helper and must stay refused"
    );
    assert!(
        !admits(vec![builtin("Connection", vec![])]),
        "a `#[resource]`-class handle must stay refused"
    );
    assert!(
        !admits(vec![builtin("Rec", vec![])]),
        "a user record payload is not a bit-copy leaf and stays refused \
         (fail-closed: it keeps leaking, exactly as before, and compiles)"
    );
}

#[test]
fn nested_scalar_and_string_enum_payload_is_admitted() {
    // The #2717 headline shape: `Result<Status, i64>` whose `Ok(Status)`
    // payload is itself a `Loaded(i64) | Described(string)` enum. The nested
    // enum is fully clone-synthesizable, so the outer composite keeps its
    // borrow exemption and its recursive `EnumInPlace` frees the inner string.
    assert!(
        admits_nested(ResolvedTy::I64),
        "a nested scalar-and-string enum payload recurses to synthesizable — \
         the plain-string-only cap leaked the inner Described(string)"
    );
}

#[test]
fn nested_enum_with_io_handle_payload_is_refused() {
    // Fail-closed through the recursion: a nested enum carrying an IoHandle
    // has no duplication helper, so the outer composite must stay refused —
    // re-admitting it would seed a clone helper synthesis that cannot resolve.
    assert!(
        !admits_nested(builtin("Stream", vec![ResolvedTy::I64])),
        "a nested enum whose sibling holds a Stream handle stays refused"
    );
    assert!(
        !admits_nested(opaque("Value")),
        "a nested enum whose sibling holds an opaque handle stays refused"
    );
}

#[test]
fn an_unresolvable_layout_is_refused() {
    assert!(
        !enum_payloads_are_clone_synthesizable(&builtin("Result", vec![ResolvedTy::String]), &[]),
        "no layout means no proof"
    );
    assert!(
        !enum_payloads_are_clone_synthesizable(
            &builtin("Result", vec![builtin("Status", vec![])]),
            &[result_layout(
                &crate::lower::mangle_layout_key("Result", &[builtin("Status", vec![])]),
                vec![builtin("Status", vec![])],
            ),]
        ),
        "a nested enum with no resolvable layout fails closed rather than \
         admitting the outer composite"
    );
}
