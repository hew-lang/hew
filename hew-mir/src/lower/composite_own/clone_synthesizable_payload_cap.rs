//! The clone-safety cap on the `string_binder_read_is_user_fn_borrow`
//! exemption must be a POSITIVE clone-drop-safe predicate, not "owns no heap".
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
//! The predicate recurses through every clone-drop-safe shape: a `string`, a
//! bit-copy scalar, an owned `Vec` of the same, a nested enum/record/tuple/array
//! built only from such leaves. A NESTED enum payload (`Result<Status, i64>`
//! whose `Ok` payload is itself a scalar-and-string enum) is synthesizable iff
//! its own payloads are — the plain-string-only spelling rejected it on its
//! `Named` payload and leaked the inner `string` sibling (#2717). A nested
//! enum/record/`Vec` carrying an IoHandle/resource/opaque payload still stays
//! refused (fail-closed).
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
    admits_with_classes(ok, record_field_orders, &hew_hir::TypeClassTable::default())
}

fn admits_with_classes(
    ok: Vec<ResolvedTy>,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    type_classes: &hew_hir::TypeClassTable,
) -> bool {
    let args = vec![
        ok.first().cloned().unwrap_or(ResolvedTy::Unit),
        ResolvedTy::String,
    ];
    let ty = builtin("Result", args.clone());
    let key = crate::lower::mangle_layout_key("Result", &args);
    enum_payloads_are_clone_synthesizable(
        &ty,
        &[result_layout(&key, ok)],
        record_field_orders,
        type_classes,
    )
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
        &HashMap::new(),
        &hew_hir::TypeClassTable::default(),
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
        enum_payloads_are_clone_synthesizable(
            &outer,
            &[result_layout(&key, vec![inner]), inner_layout],
            &HashMap::new(),
            &hew_hir::TypeClassTable::default()
        ),
        "a nested enum whose leaves are all string must be admitted"
    );
}

/// A `#[resource]` value beside a clone-drop-safe `string` in the SAME payload
/// must fail-closed the WHOLE composite. This is the exact class that leaked
/// through the record arm: a `#[resource]` record is field-bearing, so it sits
/// in `record_field_orders` keyed like a plain value record, and recursing into
/// its scalar field wrongly admitted it — seeding an `EnumInPlace` drop that
/// closed the affine resource a SECOND time (the observed `Result<Conn, string>`
/// double close). The value-class gate now refuses any affine leaf up front.
#[test]
fn resource_beside_string_payload_is_refused() {
    // Mark `Conn` as an affine `#[resource]` and register its (all-scalar)
    // field order, reproducing the field-bearing-resource-record shape.
    let mut classes = hew_hir::TypeClassTable::default();
    classes.insert(
        "Conn".to_string(),
        (hew_hir::ResourceMarker::Resource, None),
    );
    let mut orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    orders.insert(
        "Conn".to_string(),
        vec![("fd".to_string(), ResolvedTy::I64)],
    );

    // `Result<Conn, string>` — a resource record Ok payload beside the string
    // Err. Without the affine gate the record arm recursed into `fd: i64` and
    // admitted it; the gate refuses it.
    assert!(
        !admits_with_classes(vec![builtin("Conn", vec![])], &orders, &classes),
        "a `#[resource]` record payload is affine (its own close discipline, no \
         clone helper) and must stay refused even though its fields are scalar"
    );

    // `Result<(Conn, string), string>` — a resource leaf beside a string leaf
    // inside a tuple payload. The tuple recursion must still hit the affine leaf
    // and refuse the whole composite.
    assert!(
        !admits_with_classes(
            vec![ResolvedTy::Tuple(vec![
                builtin("Conn", vec![]),
                ResolvedTy::String
            ])],
            &orders,
            &classes
        ),
        "a resource leaf beside a string leaf in a tuple payload must refuse the \
         whole composite"
    );

    // Guard: the SAME record shape WITHOUT the resource marker is an ordinary
    // value record and stays admitted — proving the refusal keys on affinity,
    // not merely on the record being present.
    assert!(
        admits_with_classes(
            vec![builtin("Conn", vec![])],
            &orders,
            &hew_hir::TypeClassTable::default()
        ),
        "the same field order WITHOUT the resource marker is a plain value \
         record and stays admitted — the gate keys on affinity, not presence"
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
        !enum_payloads_are_clone_synthesizable(
            &builtin("Result", vec![ResolvedTy::String]),
            &[],
            &HashMap::new(),
            &hew_hir::TypeClassTable::default()
        ),
        "no layout means no proof"
    );
    assert!(
        !enum_payloads_are_clone_synthesizable(
            &builtin("Result", vec![builtin("Status", vec![])]),
            &[result_layout(
                &crate::lower::mangle_layout_key("Result", &[builtin("Status", vec![])]),
                vec![builtin("Status", vec![])],
            )],
            &HashMap::new(),
            &hew_hir::TypeClassTable::default()
        ),
        "a nested enum with no resolvable layout fails closed rather than \
         admitting the outer composite"
    );
}
