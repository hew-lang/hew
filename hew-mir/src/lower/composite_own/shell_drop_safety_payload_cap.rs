//! The shell-drop-safety cap on the `string_binder_read_is_user_fn_borrow`
//! exemption must be a POSITIVE predicate over payload classes whose shell
//! drop cannot double-release, not "owns no heap".
//!
//! `EnumInPlace` seeds the enum clone/drop helper synthesis, and clone
//! totality refuses every `IoHandle` / closure-pair / `#[resource]` class.
//! `Stream<T>` and `Sink<T>` are pointer-backed IO handles — yet the MIR heap
//! authority's builtin leaf set omits them and its generic `Named` arm only
//! recurses into type arguments, so `Stream<i64>` / `Sink<i64>` answer "owns
//! no heap". The old `String || !ty_owns_heap_mir` spelling therefore
//! re-admitted exactly the composites the cap exists to exclude.
//!
//! The predicate recurses through every shell-drop-safe shape: a `string`, a
//! bit-copy scalar, an owned `Vec` of the same, a nested enum/record/tuple/array
//! built only from such leaves. A NESTED enum payload (`Result<Status, i64>`
//! whose `Ok` payload is itself a scalar-and-string enum) is synthesizable iff
//! its own payloads are — the plain-string-only spelling rejected it on its
//! `Named` payload and leaked the inner `string` sibling (#2717).
//!
//! A BARE `#[opaque]` handle as a DIRECT variant payload IS admitted: its
//! thunk drop is a structural no-op with no other close authority anywhere
//! (double-free impossible by construction), and codegen emits a trap-body
//! clone for an opaque-carrying enum seeded only for its drop helper. A
//! lifecycle-registered (`#[resource]`) handle classifies `Resource` and
//! stays refused — a second close of a real resource is observable (the
//! S2200 double-close class). A NESTED opaque (inside a `Vec`, tuple, or
//! nested enum) stays refused: its release routes through a nested helper
//! family where the no-op field-drop argument does not hold.
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
    admits_full(
        ok,
        record_field_orders,
        &hew_hir::TypeClassTable::default(),
        &hew_hir::LifecycleRegistry::default(),
    )
}

fn admits_with_classes(
    ok: Vec<ResolvedTy>,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    type_classes: &hew_hir::TypeClassTable,
) -> bool {
    admits_full(
        ok,
        record_field_orders,
        type_classes,
        &hew_hir::LifecycleRegistry::default(),
    )
}

fn admits_with_registry(ok: Vec<ResolvedTy>, registry: &hew_hir::LifecycleRegistry) -> bool {
    admits_full(
        ok,
        &HashMap::new(),
        &hew_hir::TypeClassTable::default(),
        registry,
    )
}

fn admits_full(
    ok: Vec<ResolvedTy>,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    type_classes: &hew_hir::TypeClassTable,
    lifecycle_registry: &hew_hir::LifecycleRegistry,
) -> bool {
    let args = vec![
        ok.first().cloned().unwrap_or(ResolvedTy::Unit),
        ResolvedTy::String,
    ];
    let ty = builtin("Result", args.clone());
    let key = crate::lower::mangle_layout_key("Result", &args);
    enum_payloads_are_shell_drop_safe(
        &ty,
        &[result_layout(&key, ok)],
        record_field_orders,
        type_classes,
        &[],
        &[],
        lifecycle_registry,
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
    enum_payloads_are_shell_drop_safe(
        &ty,
        &[
            result_layout(&result_key, vec![status]),
            status_layout("Status", inner_payload),
        ],
        &HashMap::new(),
        &hew_hir::TypeClassTable::default(),
        &[],
        &[],
        &hew_hir::LifecycleRegistry::default(),
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
fn bytes_payloads_are_admitted() {
    // `bytes` is a refcounted CoW value with the same intrinsic dup+drop family
    // as `string`, so `Result<bytes, string>` is clone-drop-safe end to end and
    // must be admitted — the codec `try_gzip_decompress` shape that leaked its
    // payload while the cap was spelled "string or bit-copy".
    assert!(
        admits(vec![ResolvedTy::Bytes]),
        "a `bytes` payload shares string's clone+drop family and must be admitted"
    );
    assert!(
        admits(vec![ResolvedTy::Tuple(vec![
            ResolvedTy::Bytes,
            ResolvedTy::String
        ])]),
        "a `(bytes, string)` tuple payload is clone-drop-safe through both leaves"
    );
}

#[test]
fn bytes_beside_resource_payload_is_refused() {
    // Mixed-sibling: a clone-drop-safe `bytes` leaf sitting BESIDE an affine
    // `#[resource]` leaf must fail-closed the WHOLE composite. Admitting on the
    // bytes leaf alone would seed an `EnumInPlace` drop that closes the affine
    // resource a second time — the class rule 17 names. The `.all()` conjunction
    // over the value-class gate must hold for `bytes` exactly as it does for
    // `string`.
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
    assert!(
        !admits_with_classes(
            vec![ResolvedTy::Tuple(vec![
                ResolvedTy::Bytes,
                builtin("Conn", vec![])
            ])],
            &orders,
            &classes
        ),
        "a `bytes` leaf beside a `#[resource]` leaf must refuse the whole \
         composite — the bytes admit must not relax the affine conjunction"
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
fn resource_and_unresolvable_payloads_are_refused() {
    // NOTE: a DIRECT bare `#[opaque]` payload is ADMITTED (see
    // `bare_opaque_payload_sibling_is_admitted` — the hybrid-enum shell-drop
    // shape); only the lifecycle-registered spelling and every NESTED opaque
    // position stay refused.
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
        enum_payloads_are_shell_drop_safe(
            &outer,
            &[result_layout(&key, vec![inner]), inner_layout],
            &HashMap::new(),
            &hew_hir::TypeClassTable::default(),
            &[],
            &[],
            &hew_hir::LifecycleRegistry::default(),
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
        !enum_payloads_are_shell_drop_safe(
            &builtin("Result", vec![ResolvedTy::String]),
            &[],
            &HashMap::new(),
            &hew_hir::TypeClassTable::default(),
            &[],
            &[],
            &hew_hir::LifecycleRegistry::default(),
        ),
        "no layout means no proof"
    );
    assert!(
        !enum_payloads_are_shell_drop_safe(
            &builtin("Result", vec![builtin("Status", vec![])]),
            &[result_layout(
                &crate::lower::mangle_layout_key("Result", &[builtin("Status", vec![])]),
                vec![builtin("Status", vec![])],
            )],
            &HashMap::new(),
            &hew_hir::TypeClassTable::default(),
            &[],
            &[],
            &hew_hir::LifecycleRegistry::default(),
        ),
        "a nested enum with no resolvable layout fails closed rather than \
         admitting the outer composite"
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

/// A registered resource-record lifecycle for `Handle { close }`, the
/// declared-release carve-out's condition (a).
fn handle_record_lifecycle_registry() -> hew_hir::LifecycleRegistry {
    let mut classes = hew_hir::TypeClassTable::default();
    classes
        .admit_resource_record_lifecycle(hew_hir::ResourceRecordLifecycle {
            resource_declaration: hew_types::DefId::new("Handle"),
            close_declaration: hew_types::DefId::new("Handle::close"),
            close_symbol: "Handle::close".to_string(),
        })
        .expect("unique test lifecycle");
    classes.lifecycle_registry().clone()
}

#[test]
fn declared_release_record_payload_is_admitted() {
    // The adoption shape: a lifecycle-registered `#[resource]` record whose
    // only field is a bare `#[opaque]` handle (clause-3-clean). The declared
    // close is the composite's SOLE release authority — the shell's thunk
    // chain is the one place it is ever scheduled — so the carve-out must
    // admit the DIRECT payload; refusing it closes the handle ZERO times.
    let mut orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    orders.insert(
        "Handle".to_string(),
        vec![("raw".to_string(), opaque("Dq"))],
    );
    let mut classes = hew_hir::TypeClassTable::default();
    classes.insert(
        "Handle".to_string(),
        (
            hew_hir::ResourceMarker::Resource,
            Some("Handle::close".to_string()),
        ),
    );
    assert!(
        admits_full(
            vec![builtin("Handle", vec![])],
            &orders,
            &classes,
            &handle_record_lifecycle_registry(),
        ),
        "a lifecycle-registered resource record whose declared close is the \
         whole release plan must be admitted as a DIRECT payload — the shell \
         drop is its sole close authority"
    );
    // Scalar-field spelling of the same admission (`Conn { fd: i64 }`).
    orders.insert(
        "Handle".to_string(),
        vec![("fd".to_string(), ResolvedTy::I64)],
    );
    assert!(
        admits_full(
            vec![builtin("Handle", vec![])],
            &orders,
            &classes,
            &handle_record_lifecycle_registry(),
        ),
        "scalar fields pass clause 3 the same way an opaque handle does"
    );
}

#[test]
fn declared_release_record_with_teardown_freeable_field_is_refused() {
    // Clause-3 polarity: the SAME registered record with a `log: string`
    // field is NOT admitted — the post-close field-wise teardown really does
    // free `log`, so the declared close is not the whole drop plan and the
    // affine refusal stands (leak, never a double close).
    let mut orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    orders.insert(
        "Handle".to_string(),
        vec![
            ("raw".to_string(), opaque("Dq")),
            ("log".to_string(), ResolvedTy::String),
        ],
    );
    let mut classes = hew_hir::TypeClassTable::default();
    classes.insert(
        "Handle".to_string(),
        (
            hew_hir::ResourceMarker::Resource,
            Some("Handle::close".to_string()),
        ),
    );
    assert!(
        !admits_full(
            vec![builtin("Handle", vec![])],
            &orders,
            &classes,
            &handle_record_lifecycle_registry(),
        ),
        "a registered record with a field the teardown frees must stay refused \
         — clause 3 is the admission boundary, not the registry alone"
    );
}

#[test]
fn unregistered_affine_record_payload_is_still_refused() {
    // Condition (a) polarity: the same clause-3-clean field order WITHOUT a
    // lifecycle registration (a `#[resource]` marker with no admitted close)
    // keeps the affine refusal — the carve-out keys on the registry, not on
    // the marker.
    let mut orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    orders.insert(
        "Handle".to_string(),
        vec![("raw".to_string(), opaque("Dq"))],
    );
    let mut classes = hew_hir::TypeClassTable::default();
    classes.insert(
        "Handle".to_string(),
        (hew_hir::ResourceMarker::Resource, None),
    );
    assert!(
        !admits_full(
            vec![builtin("Handle", vec![])],
            &orders,
            &classes,
            &hew_hir::LifecycleRegistry::default(),
        ),
        "an unregistered affine record must stay on the value-class refusal"
    );
}

#[test]
fn declared_release_record_nested_beyond_depth_one_is_refused() {
    // Depth polarity: the admitted record reached through a NESTED aggregate
    // (a tuple payload) is beyond the candidate shell's own drop steps and
    // stays refused, exactly like the nested bare-opaque position.
    let mut orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    orders.insert(
        "Handle".to_string(),
        vec![("raw".to_string(), opaque("Dq"))],
    );
    let mut classes = hew_hir::TypeClassTable::default();
    classes.insert(
        "Handle".to_string(),
        (
            hew_hir::ResourceMarker::Resource,
            Some("Handle::close".to_string()),
        ),
    );
    assert!(
        !admits_full(
            vec![ResolvedTy::Tuple(vec![
                builtin("Handle", vec![]),
                ResolvedTy::I64
            ])],
            &orders,
            &classes,
            &handle_record_lifecycle_registry(),
        ),
        "a declared-release record inside a tuple payload sits at depth 2 and \
         must stay refused — the carve-out's soundness argument is depth-1 only"
    );
}

#[test]
fn bare_opaque_beside_resource_marker_is_refused() {
    // Belt-and-braces on the OTHER authority: the same `#[opaque]` spelling
    // carrying a `#[resource]` marker in the type-class table classifies
    // `AffineResource` and is refused by the value-class gate even with an
    // empty lifecycle registry — the two refusal authorities are independent.
    let mut classes = hew_hir::TypeClassTable::default();
    classes.insert(
        "Handle".to_string(),
        (hew_hir::ResourceMarker::Resource, None),
    );
    assert!(
        !admits_full(
            vec![opaque("Handle")],
            &HashMap::new(),
            &classes,
            &hew_hir::LifecycleRegistry::default(),
        ),
        "a `#[resource]`-marked opaque refuses through the value-class gate \
         independently of the lifecycle registry"
    );
}
