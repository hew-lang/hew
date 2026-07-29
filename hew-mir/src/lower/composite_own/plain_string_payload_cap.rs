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
    enum_payloads_are_plain_string(&ty, &[result_layout(&key, ok)])
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
fn an_unresolvable_layout_is_refused() {
    assert!(
        !enum_payloads_are_plain_string(&builtin("Result", vec![ResolvedTy::String]), &[]),
        "no layout means no proof"
    );
}
