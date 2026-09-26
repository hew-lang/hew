//! Tests for `RuntimeCallFamily` symbol/family classification and semantic contracts.

use super::*;
use strum::IntoEnumIterator;

fn named_type(name: &str, builtin: Option<crate::BuiltinType>) -> ResolvedTy {
    match builtin {
        Some(crate::BuiltinType::ActorHandle) => ResolvedTy::actor_for_test(name, Vec::new()),
        Some(builtin) => ResolvedTy::named_builtin(builtin, Vec::new()),
        None => ResolvedTy::user_for_test(name, Vec::new()),
    }
}

#[test]
fn utf8_decode_result_contract_requires_exact_nominal_error_identity() {
    let exact_error = named_type("std.encoding.utf8.Utf8Error", None);
    let exact_result = ResolvedTy::Named {
        args: vec![ResolvedTy::String, exact_error.clone()],
        head: crate::TypeHead::Builtin(crate::BuiltinType::Result),
        is_opaque: false,
    };
    assert!(RuntimeVariantResultKind::Utf8Decode.matches(&exact_result));

    let lookalike_error = named_type("application.Utf8Error", None);
    let lookalike_result = ResolvedTy::Named {
        args: vec![ResolvedTy::String, lookalike_error],
        head: crate::TypeHead::Builtin(crate::BuiltinType::Result),
        is_opaque: false,
    };
    assert!(!RuntimeVariantResultKind::Utf8Decode.matches(&lookalike_result));

    let user_result =
        ResolvedTy::named_for_test("application.Result", vec![ResolvedTy::String, exact_error]);
    assert!(!RuntimeVariantResultKind::Utf8Decode.matches(&user_result));
}

#[test]
fn new_text_runtime_families_publish_closed_semantic_effects() {
    let decode = RuntimeCallFamily::BytesDecodeUtf8
        .semantic_contract()
        .expect("validating decode must have an ownership-SIR contract");
    assert_eq!(
        decode.arguments,
        &[RuntimeArgumentContract {
            ty: RuntimeValueKind::Bytes,
            effect: RuntimeArgumentEffect::Borrow,
        }]
    );
    assert_eq!(
        decode.result,
        RuntimeResultEffect::FreshOwnedVariant(RuntimeVariantResultKind::Utf8Decode)
    );
    assert!(decode.failures.is_empty());

    let lossy = RuntimeCallFamily::BytesDecodeUtf8Lossy
        .semantic_contract()
        .expect("lossy decode must have an ownership-SIR contract");
    assert_eq!(lossy.arguments, decode.arguments);
    assert_eq!(
        lossy.result,
        RuntimeResultEffect::FreshOwned(RuntimeValueKind::String)
    );
    assert!(lossy.failures.is_empty());

    assert!(decode.matches_signature(
        &crate::DefTable::new(),
        &[ResolvedTy::Bytes],
        &ResolvedTy::Named {
            args: vec![
                ResolvedTy::String,
                named_type("std.encoding.utf8.Utf8Error", None),
            ],
            head: crate::TypeHead::Builtin(crate::BuiltinType::Result),
            is_opaque: false
        },
    ));
    assert!(!decode.matches_signature(
        &crate::DefTable::new(),
        &[ResolvedTy::String],
        &ResolvedTy::String
    ));
    assert!(lossy.matches_signature(
        &crate::DefTable::new(),
        &[ResolvedTy::Bytes],
        &ResolvedTy::String
    ));

    let byte_len = RuntimeCallFamily::StringByteLen
        .semantic_contract()
        .expect("string byte length must have an ownership-SIR contract");
    assert_eq!(
        byte_len.arguments,
        &[RuntimeArgumentContract {
            ty: RuntimeValueKind::String,
            effect: RuntimeArgumentEffect::Borrow,
        }]
    );
    assert_eq!(
        byte_len.result,
        RuntimeResultEffect::BitCopy(RuntimeValueKind::I64)
    );
    assert!(byte_len.failures.is_empty());
}

use std::collections::{HashMap, HashSet};

#[test]
fn fixed_array_update_preserves_length_and_element_ownership() {
    let array = ResolvedTy::Array(Box::new(ResolvedTy::String), 2);
    let family = RuntimeCallFamily::Array(ArrayValueOp::Set);
    let contract = family.semantic_contract().unwrap();
    let arguments = [array.clone(), ResolvedTy::I64, ResolvedTy::String];
    assert!(contract.matches_signature(&crate::DefTable::new(), &arguments, &array));
    assert!(!contract.matches_signature(
        &crate::DefTable::new(),
        &arguments,
        &ResolvedTy::Array(Box::new(ResolvedTy::String), 3)
    ));
    assert!(!contract.matches_signature(
        &crate::DefTable::new(),
        &[array.clone(), ResolvedTy::I64, ResolvedTy::I64],
        &array
    ));
    assert!(!contract.matches_signature(
        &crate::DefTable::new(),
        &[array.clone(), ResolvedTy::U64, ResolvedTy::String],
        &array
    ));
    assert!(!contract.matches_signature(&crate::DefTable::new(), &arguments, &ResolvedTy::Unit));

    // Updating replaces the array owner; the index is copied and an
    // owning element follows its copy-or-move value boundary.
    assert!(family.consumes_receiver());
    assert_eq!(family.arg_consume_verdict(0), ConsumeVerdict::ProvenConsume);
    assert_eq!(family.arg_consume_verdict(1), ConsumeVerdict::ProvenBorrow);
    assert_eq!(
        family.arg_consume_verdict(2),
        ConsumeVerdict::ConservativeConsume
    );
}

#[test]
fn scalar_vec_result_ownership_is_family_keyed_and_total() {
    for family in all_vec_scalar_families() {
        let RuntimeCallFamily::VecScalar { op, elem } = family else {
            unreachable!("the scalar Vec matrix only yields VecScalar families");
        };
        let expected = if matches!(op, VecScalarOp::Pop | VecScalarOp::RemoveAt)
            && elem == VecScalarElem::Str
        {
            RuntimeResultOwnership::FreshOwnedString
        } else {
            RuntimeResultOwnership::Untracked
        };
        assert_eq!(family.result_ownership(), expected, "{family:?}");
    }
    for family in all_vec_contains_scalar_families() {
        assert_eq!(family.result_ownership(), RuntimeResultOwnership::Untracked);
    }
    assert_eq!(
        RuntimeCallFamily::VecClone.result_ownership(),
        RuntimeResultOwnership::FreshOwnedVec
    );
    assert_eq!(
        RuntimeCallFamily::VecNew.result_ownership(),
        RuntimeResultOwnership::FreshOwnedVec
    );
    assert_eq!(
        RuntimeCallFamily::VecJoinStr.result_ownership(),
        RuntimeResultOwnership::FreshOwnedString
    );
    assert_eq!(
        RuntimeCallFamily::BytesNew.result_ownership(),
        RuntimeResultOwnership::FreshOwnedBytes
    );
}

/// `from_c_symbol` reads the row table's symbol column backwards, so a
/// shared symbol silently costs an operation its reverse lookup. The print
/// operations are the one sanctioned sharing: their element type and
/// newline flag ride beside the symbol. Anything else sharing a symbol is
/// a table defect.
#[test]
fn only_the_print_operations_share_a_c_symbol() {
    let mut seen: HashMap<&'static str, RuntimeCallFamily> = HashMap::new();
    for family in all_runtime_call_families() {
        let symbol = family.row().symbol;
        if let Some(previous) = seen.insert(symbol, family) {
            assert!(
                matches!(family, RuntimeCallFamily::Print { .. })
                    && matches!(previous, RuntimeCallFamily::Print { .. }),
                "{previous:?} and {family:?} both claim {symbol:?}, so neither \
                 lifts back out of a C symbol"
            );
        }
    }
}

#[test]
fn canonical_stdlib_extern_descriptors_agree_with_runtime_symbols() {
    for entry in canonical_std_io_extern_signatures() {
        if let Some(family) = entry.family {
            assert_eq!(family.c_symbol(), entry.symbol, "{entry:?}");
        }
    }
}

/// Every operation lifts back out of its own symbol.
/// `record_runtime_method_call_rewrite` turns a checker-resolved C symbol
/// into a typed descriptor this way, so an operation the reverse lookup
/// drops is unreachable from that producer.
#[test]
fn runtime_call_family_round_trips_through_from_c_symbol() {
    for family in all_runtime_call_families() {
        let sym = family.c_symbol();
        let back = RuntimeCallFamily::from_c_symbol(sym);
        if matches!(family, RuntimeCallFamily::Print { .. }) {
            assert_eq!(
                back, None,
                "a print ABI symbol cannot infer its type or newline flag"
            );
            continue;
        }
        assert_eq!(
            back,
            Some(family),
            "from_c_symbol({sym:?}) returned {back:?}, expected Some({family:?}) \
             — the inverse arm is missing or wrong",
        );
    }
}

#[test]
fn mir_builtin_lift_excludes_codegen_only_partitions() {
    assert_eq!(
        RuntimeCallFamily::from_mir_builtin_symbol("hew_vec_push_layout"),
        None
    );
    assert_eq!(
        RuntimeCallFamily::from_mir_builtin_symbol("Node::start"),
        Some(RuntimeCallFamily::NodeStart)
    );
    assert_eq!(
        RuntimeCallFamily::from_mir_builtin_symbol("hew_hashmap_insert_layout"),
        Some(RuntimeCallFamily::HashMapInsertLayout)
    );
}

#[test]
fn runtime_capabilities_are_classified_by_family() {
    assert_eq!(
        RuntimeCallFamily::MetricCounterInc.runtime_capability(),
        Some(RuntimeCapability::Metrics)
    );
    assert_eq!(
        RuntimeCallFamily::NodeStart.runtime_capability(),
        Some(RuntimeCapability::Node)
    );
    assert_eq!(
        RuntimeCallFamily::StreamSendLayout.runtime_capability(),
        None
    );
}

#[test]
fn symbol_lifting_respects_codegen_partitions_and_required_attributes() {
    use RuntimeCallFamily as F;

    let mut expected: HashSet<RuntimeCallFamily> = [
        F::BytesNew,
        F::HashMapClearLayout,
        F::HashMapCloneLayout,
        F::HashSetClearLayout,
        F::HashSetCloneLayout,
        F::HashSetToVecLayout,
        F::VecAppend,
        F::VecClear,
        F::VecClone,
        F::VecCloneLayout,
        F::VecCloneOwned,
        F::VecTakeAll,
        F::VecContainsLayout,
        F::VecContainsOwned,
        F::VecNew,
        F::VecPopBool,
        F::VecPopLayout,
        F::VecPopOwned,
        F::VecPushBool,
        F::VecPushLayout,
        F::VecPushOwned,
        F::VecPushOwnedMove,
        F::VecRemoveAtBool,
        F::VecRemoveAtLayout,
        F::VecRemoveAtOwned,
        F::VecSetBool,
        F::VecSetLayout,
        F::VecSetOwned,
        F::VecSetOwnedMove,
        F::VecIsEmpty,
        F::VecJoinStr,
    ]
    .into_iter()
    .collect();
    expected.extend(
        VecScalarOp::iter()
            .flat_map(|op| VecScalarElem::iter().map(move |elem| F::VecScalar { op, elem })),
    );
    expected.extend(VecContainsScalarElem::iter().map(F::VecContainsScalar));
    let actual: HashSet<RuntimeCallFamily> = all_runtime_call_families()
        .into_iter()
        .filter(|family| family.is_codegen_partition_only())
        .collect();
    assert_eq!(
        actual, expected,
        "the codegen-only collection partition changed; review whether each \
         affected family should remain absent from MIR calls"
    );

    for family in all_runtime_call_families() {
        assert_eq!(
            RuntimeCallFamily::from_mir_builtin_symbol(family.c_symbol()).is_none(),
            expected.contains(&family) || matches!(family, F::Print { .. }),
            "MIR carrier classification drifted for {family:?}"
        );
    }
}

/// `from_c_symbol` returns `None` for strings the catalog does
/// not enumerate (open-set extern FFI symbols, user-trait method
/// keys like `i64::fmt`, garbage).
#[test]
fn from_c_symbol_rejects_unknown_strings() {
    assert!(RuntimeCallFamily::from_c_symbol("not_a_runtime_symbol").is_none());
    assert!(RuntimeCallFamily::from_c_symbol("").is_none());
    // User-trait method keys live in `RewriteToFunction.c_symbol` as
    // `Type::method` strings; they are open-set and MUST be rejected
    // so the typed-descriptor path leaves them alone.
    assert!(RuntimeCallFamily::from_c_symbol("i64::fmt").is_none());
    assert!(RuntimeCallFamily::from_c_symbol("MyType::greet").is_none());
}

/// `is_async_suspending` returns `Some(_)` for EXACTLY the symbols
/// the HIR await-classifier discriminates through `RuntimeCallFamily`
/// today: `hew_stream_send_layout` (backpressure-aware sink send) and
/// `hew_stream_next_layout` (stream recv).
/// Locks the consumer contract for the eventual migration.
///
/// Positive: the symbols listed map to the matching
/// `AsyncSuspendKind`. Negative: every other family returns `None`,
/// pinned by enumeration via `all_runtime_call_families`. ESP. the
/// `try_*` peers (`StreamTrySendLayout`, `StreamTryNextLayout`) MUST
/// stay non-suspending: those never touch the backpressure ramp.
#[test]
fn async_suspension_classification_preserves_operation_identity() {
    use RuntimeCallFamily as F;

    // Positive: exactly these (family, expected kind) tuples.
    let positives: &[(RuntimeCallFamily, AsyncSuspendKind)] = &[
        (F::StreamSendLayout, AsyncSuspendKind::SinkSend),
        (F::StreamNextLayout, AsyncSuspendKind::StreamRecv),
    ];
    for (family, kind) in positives {
        assert_eq!(
            family.is_async_suspending(),
            Some(*kind),
            "{family:?} must suspend with {kind:?}",
        );
    }

    // Explicit negative regression set: the try_* peers and the
    // close families the classifier never touches.
    let must_not_suspend: &[RuntimeCallFamily] = &[
        F::StreamTrySendLayout,
        F::StreamTryNextLayout,
        F::StreamClose,
        F::SinkClose,
    ];
    for family in must_not_suspend {
        assert_eq!(
            family.is_async_suspending(),
            None,
            "{family:?} must NOT suspend per the current HIR await-classifier",
        );
    }

    for op in AsyncIoOp::iter() {
        assert_eq!(
            F::AsyncIo(op).is_async_suspending(),
            Some(AsyncSuspendKind::NativeIo(op))
        );
    }
}

/// Fail-closed constructor: passing `Some(elem)` to a family that
/// does not accept an element-type degree of freedom returns
/// `Err(UnexpectedElem)`. Pins the substrate's "no silent default"
/// guarantee.
#[test]
fn descriptor_new_rejects_unexpected_elem() {
    let err = RuntimeCallDescriptor::new(RuntimeCallFamily::VecLen, Some(ResolvedTy::I64))
        .expect_err("VecLen must reject Some(elem)");
    assert!(matches!(err, DescriptorError::UnexpectedElem { .. }));

    let err = RuntimeCallDescriptor::new(RuntimeCallFamily::StreamClose, Some(ResolvedTy::Bool))
        .expect_err("StreamClose must reject Some(elem)");
    assert!(matches!(err, DescriptorError::UnexpectedElem { .. }));

    // Symmetric: every variant accepts `None`.
    for family in all_runtime_call_families() {
        RuntimeCallDescriptor::new(family, None).unwrap_or_else(|e| {
            panic!("descriptor with elem=None must construct for {family:?}: {e}")
        });
    }
}

/// The descriptor's accessor methods delegate to the family;
/// confirm the wire works for one representative variant from
/// each axis.
#[test]
fn descriptor_accessors_delegate_to_family() {
    let d = RuntimeCallDescriptor::new(RuntimeCallFamily::StreamNextLayout, None).unwrap();
    assert_eq!(d.family(), RuntimeCallFamily::StreamNextLayout);
    assert_eq!(d.elem(), None);
    assert_eq!(d.c_symbol(), "hew_stream_next_layout");
    // StreamNextLayout is one of the suspending classifier symbols.
    assert_eq!(d.is_async_suspending(), Some(AsyncSuspendKind::StreamRecv));

    // Non-suspending close peer: StreamClose / SinkClose are NOT
    // in the await-classifier set.
    let d = RuntimeCallDescriptor::new(RuntimeCallFamily::StreamClose, None).unwrap();
    assert_eq!(d.c_symbol(), "hew_stream_close");
    assert!(d.consumes_receiver());
    assert_eq!(d.is_async_suspending(), None);

    let d = RuntimeCallDescriptor::new(RuntimeCallFamily::VecLen, None).unwrap();
    assert_eq!(d.c_symbol(), "hew_vec_len");
    assert!(!d.consumes_receiver());
    assert_eq!(d.is_async_suspending(), None);
}

// -------------------------------------------------------------------------
// RuntimeDropDescriptor
// -------------------------------------------------------------------------

/// Round-trip parity with `runtime_drop_symbol`'s table in
/// `hew-codegen-rs/src/llvm.rs:18352`. Hard-coded mirror; a follow-up
/// migration deletes the string-keyed table and reads
/// `RuntimeDropDescriptor::c_symbol()` directly.
#[test]
fn drop_descriptor_c_symbols_match_codegen_table() {
    use std::collections::HashSet;
    // Mirror of the runtime_drop_symbol table (drop_fn_name → C symbol).
    // Listed here so a future change to either side fails this test
    // loudly (substrate-tests-the-substrate).
    let expected: &[(&str, &str)] = &[
        ("Stream::close", "hew_stream_close"),
        ("Sink::close", "hew_sink_close"),
        ("CancellationToken::release", "hew_cancel_token_release"),
        ("MonitorRef::close", "hew_actor_demonitor"),
    ];
    let mut by_name: HashMap<&'static str, RuntimeDropDescriptor> = HashMap::new();
    for d in all_runtime_drop_descriptors() {
        by_name.insert(d.drop_fn_name(), d);
    }
    for (name, sym) in expected {
        let d = by_name
            .get(name)
            .unwrap_or_else(|| panic!("missing RuntimeDropDescriptor for {name}"));
        assert_eq!(
            d.c_symbol(),
            *sym,
            "drop descriptor {d:?} c_symbol mismatch"
        );
    }
    // No extra descriptors (the inverse direction): every variant we
    // enumerated must be in the expected table. If a future
    // contributor adds a variant without updating the expected table
    // (and the codegen `runtime_drop_symbol` table), this fires.
    let expected_names: HashSet<&'static str> = expected.iter().map(|(n, _)| *n).collect();
    for d in all_runtime_drop_descriptors() {
        assert!(
            expected_names.contains(d.drop_fn_name()),
            "RuntimeDropDescriptor {d:?} has no entry in the \
             codegen `runtime_drop_symbol` parity table; add it to \
             both or remove the variant"
        );
    }
}

#[test]
fn builtin_resource_close_inventory_has_one_typed_descriptor_authority() {
    use crate::BuiltinType::*;

    let inventory = [
        (Stream, Some(RuntimeDropDescriptor::StreamClose)),
        (Sink, Some(RuntimeDropDescriptor::SinkClose)),
        (ActorFn, None),
        (
            CancellationToken,
            Some(RuntimeDropDescriptor::CancellationTokenRelease),
        ),
        (MonitorRef, Some(RuntimeDropDescriptor::MonitorRefClose)),
        // These marker-bearing internal carriers have no executable
        // runtime close contract. They must remain unsupported rather
        // than acquiring one by spelling or marker alone.
        (ActorHandle, None),
        (HewActor, None),
        (BoxedActor, None),
    ];

    for (builtin, expected) in inventory {
        assert_eq!(
            RuntimeDropDescriptor::for_builtin(builtin),
            expected,
            "typed lifecycle inventory drifted for {builtin:?}",
        );
    }
}

#[test]
fn runtime_resource_drop_operands_are_exhaustively_typed() {
    use RuntimeDropDescriptor::*;
    use RuntimeDropOperandShape::{HandlePtr, MonitorRefId};

    let expected = [
        (StreamClose, HandlePtr),
        (SinkClose, HandlePtr),
        (CancellationTokenRelease, HandlePtr),
        (MonitorRefClose, MonitorRefId),
    ];

    assert_eq!(all_runtime_drop_descriptors().len(), expected.len());
    for (descriptor, shape) in expected {
        assert_eq!(
            descriptor.operand_shape(),
            shape,
            "operand ABI drifted for {descriptor:?}"
        );
    }
}

/// `from_drop_fn_name` is a true inverse of `drop_fn_name` (every
/// variant has a unique name), and rejects user `<Type>::close`
/// spellings so the open-set arm stays open.
#[test]
fn drop_descriptor_name_round_trips() {
    for d in all_runtime_drop_descriptors() {
        assert_eq!(
            RuntimeDropDescriptor::from_drop_fn_name(d.drop_fn_name()),
            Some(d),
            "drop_fn_name round-trip failed for {d:?}"
        );
    }
    assert!(RuntimeDropDescriptor::from_drop_fn_name("MyType::close").is_none());
    assert!(RuntimeDropDescriptor::from_drop_fn_name("hew_duplex_close").is_none());
    assert!(RuntimeDropDescriptor::from_drop_fn_name("").is_none());
}

// The allowlist-coverage parity tests
// (`allowlist_subset_round_trips`, `every_allowlist_symbol_has_a_family`,
// `drop_descriptor_symbols_in_allowlist_or_pre_staged`, and
// `every_c_symbol_resolves_to_a_real_symbol`) require
// `hew_mir::runtime_symbols::is_known_runtime_symbol`, which lives
// in `hew-mir`. They moved to `hew-mir/tests/runtime_call_allowlist.rs`
// alongside the re-export shim and run as integration tests against
// the same substrate.
