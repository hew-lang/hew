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
    admits_with_records(ok, &HashMap::new())
}

fn admits_with_records(
    ok: Vec<ResolvedTy>,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
) -> bool {
    let args = vec![
        ok.first().cloned().unwrap_or(ResolvedTy::Unit),
        ResolvedTy::String,
    ];
    let ty = builtin("Result", args.clone());
    let key = crate::lower::mangle_layout_key("Result", &args);
    enum_payloads_are_plain_string(&ty, &[result_layout(&key, ok)], record_field_orders)
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
        "an UNRESOLVABLE record payload (no field order registered) has no proof \
         its leaves are clone-drop-safe and stays refused (fail-closed)"
    );
}

#[test]
fn nested_string_enum_payloads_are_admitted() {
    // `Outer::Wrap(Inner)` where `Inner::Text(string)` — the whole-nested-enum
    // handoff shape. The inner enum's leaves are all `string`, so the outer
    // composite is clone-drop-safe and must be admitted.
    let inner = builtin("Inner", vec![]);
    let inner_layout = crate::model::EnumLayout {
        name: "Inner".to_string(),
        tag_width: 1,
        variants: vec![crate::model::MachineVariantLayout {
            name: "Text".to_string(),
            field_tys: vec![ResolvedTy::String],
            field_names: vec![],
        }],
        is_indirect: false,
    };
    let args = vec![inner.clone(), ResolvedTy::String];
    let outer = builtin("Result", args.clone());
    let key = crate::lower::mangle_layout_key("Result", &args);
    assert!(
        enum_payloads_are_plain_string(
            &outer,
            &[result_layout(&key, vec![inner]), inner_layout],
            &HashMap::new()
        ),
        "a nested enum whose leaves are all string must be admitted"
    );
}

#[test]
fn record_of_strings_is_admitted_and_record_with_resource_is_refused() {
    let mut orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    orders.insert(
        "Holder".to_string(),
        vec![
            ("label".to_string(), ResolvedTy::String),
            ("count".to_string(), ResolvedTy::I64),
        ],
    );
    assert!(
        admits_with_records(vec![builtin("Holder", vec![])], &orders),
        "a record payload whose fields are all string/scalar leaves is \
         clone-drop-safe and must be admitted"
    );
    orders.insert(
        "Guarded".to_string(),
        vec![("conn".to_string(), builtin("Connection", vec![]))],
    );
    assert!(
        !admits_with_records(vec![builtin("Guarded", vec![])], &orders),
        "a record payload carrying a `#[resource]`-class leaf must stay refused"
    );
}

#[test]
fn vec_of_clone_safe_elements_is_admitted_and_vec_of_resource_is_refused() {
    let vec_of = |elem: ResolvedTy| {
        ResolvedTy::named_builtin("Vec", hew_types::BuiltinType::Vec, vec![elem])
    };
    assert!(
        admits(vec![vec_of(ResolvedTy::String)]),
        "a `Vec<string>` payload clones and drops element-wise and must be admitted"
    );
    assert!(
        admits(vec![vec_of(vec_of(ResolvedTy::I64))]),
        "a `Vec<Vec<i64>>` payload recurses element-wise and stays clone-drop-safe"
    );
    assert!(
        !admits(vec![vec_of(opaque("Value"))]),
        "a `Vec<#[opaque]>` element has no duplication helper and must stay refused"
    );
    assert!(
        !admits(vec![vec_of(builtin("Connection", vec![]))]),
        "a `Vec<#[resource]>` element must stay refused"
    );
}

#[test]
fn an_unresolvable_layout_is_refused() {
    assert!(
        !enum_payloads_are_plain_string(
            &builtin("Result", vec![ResolvedTy::String]),
            &[],
            &HashMap::new()
        ),
        "no layout means no proof"
    );
}
