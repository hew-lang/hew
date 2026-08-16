use super::{
    dataflow, is_unsupported_user_record_value_class_ty, BasicBlock, BindingId, Builder,
    BuiltinType, CmpPred, ExecutionContextReader, FloatWidth, HashMap, HashSet, HirBinding,
    HirBlock, HirConstValue, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule, HirStmt,
    HirStmtKind, Instr, IntArithOp, IntSignedness, IntentKind, MirCheck, MirConst, MirConstValue,
    MirDiagnostic, MirDiagnosticKind, NumericMethodOp, NumericSignedness, PointerWidth,
    ResolvedRef, ResolvedTy, ResourceMarker, Strategy, UnaryOp, ValueClass, CRASH_KIND_VARIANTS,
    HEW_CTX_OFFSET_ACTOR_ID, HEW_CTX_OFFSET_PARENT_SUPERVISOR, HEW_CTX_OFFSET_TRACE_SPAN,
    SENTINEL_CRASH_CODE_NODE, SENTINEL_CRASH_CODE_SITE, SENTINEL_DOWN_CRASH_KIND_BINDING,
    SENTINEL_DOWN_LOCAL_SLOT_BINDING, SENTINEL_DOWN_LOCATION_BINDING,
    SENTINEL_DOWN_MONITOR_ID_BINDING, SENTINEL_DOWN_REASON_KIND_BINDING,
    SENTINEL_DOWN_TARGET_KIND_BINDING, SENTINEL_EXIT_ACTOR_ID_BINDING,
    SENTINEL_EXIT_KIND_TAG_BINDING,
};

/// The synthetic `#[on(crash)]` handler's logical return type — `CrashAction`.
///
/// M-4: the emitted `__on_crash` function returns the `CrashAction` tagged-union
/// value by its natural enum-return path (every return position — tail and
/// explicit `return CrashAction::X;` — lowers identically). The runtime
/// `HewOnCrashFn` ABI mirrors the LLVM struct with a `#[repr(C)]` 2-byte struct
/// and decodes the tag byte. A `panic()`-diverging body returns no value.
pub(super) fn crash_action_return_ty() -> ResolvedTy {
    ResolvedTy::named_builtin(
        "std.failure.CrashAction",
        hew_types::BuiltinType::CrashAction,
        Vec::new(),
    )
}
/// Build the synthetic prologue body for an `#[on(exit)]` hook (M-7-R).
///
/// The runtime delivers a linked actor's `CrashNotification` as two raw ABI
/// params — `__exit_actor_id: u64` and `__exit_kind_tag: i32` (the already-
/// projected M-6 `CrashKind` tag). The user-visible `note: CrashNotification`
/// param is replaced by these two; the body gains a prologue that reconstructs
/// the typed value:
///
/// ```text
/// let note = match __exit_kind_tag {
///     0 => CrashNotification { actor_id: __exit_actor_id, kind: CrashKind::Crashed },
///     1 => CrashNotification { actor_id: __exit_actor_id, kind: CrashKind::HeapExceeded },
///     _ => CrashNotification { actor_id: __exit_actor_id, kind: CrashKind::PartitionDetected },
/// };
/// <original body>
/// ```
///
/// `note_param` carries the original binding id so user `note.actor_id` /
/// `note.kind` reads resolve. `CrashNotification` / `CrashKind` are std types.
#[expect(
    clippy::too_many_lines,
    reason = "single coherent HIR-construction unit (the synthetic prologue \
              match + CrashNotification rebuild); splitting it would scatter \
              the node shapes that must stay aligned"
)]
pub(super) fn build_exit_hook_body(body: HirBlock, note_param: &HirBinding) -> HirBlock {
    let span = note_param.span.clone();
    let crash_notification_ty = note_param.ty.clone();
    // Synthetic lifecycle payloads must carry the same canonical builtin
    // discriminator as checker-authored source values.  A source spelling is
    // presentation only here; using a user nominal would cross D10 and erase
    // the crash-hook value-class authority.
    let crash_kind_ty = hew_types::builtin_enums::resolved_monomorphic_builtin_enum_ty("CrashKind")
        .expect("generated builtin enum catalog must contain CrashKind");

    let actor_id_ref = || HirExpr {
        node: SENTINEL_CRASH_CODE_NODE,
        site: SENTINEL_CRASH_CODE_SITE,
        ty: ResolvedTy::U64,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::BindingRef {
            name: "__exit_actor_id".to_string(),
            resolved: ResolvedRef::Binding(SENTINEL_EXIT_ACTOR_ID_BINDING),
        },
        span: span.clone(),
    };

    // One match arm per CrashKind variant, each yielding a full
    // `CrashNotification { actor_id, kind: CrashKind::<V> }`.
    let arms: Vec<hew_hir::HirMatchArm> = CRASH_KIND_VARIANTS
        .iter()
        .enumerate()
        .map(|(idx, variant_name)| {
            // `CrashKind::<variant>` unit-variant constructor. A user enum
            // projects onto the tagged-union (machine) substrate, so a unit
            // variant value lowers as a payload-free `MachineVariantCtor` keyed
            // by the enum name and the variant's declaration index.
            let _ = variant_name; // name documented by CRASH_KIND_VARIANTS order
            let kind_value = HirExpr {
                node: SENTINEL_CRASH_CODE_NODE,
                site: SENTINEL_CRASH_CODE_SITE,
                ty: crash_kind_ty.clone(),
                value_class: ValueClass::BitCopy,
                intent: IntentKind::Read,
                kind: HirExprKind::MachineVariantCtor {
                    machine_name: "std.failure.CrashKind".to_string(),
                    state_idx: idx,
                    payload: None,
                },
                span: span.clone(),
            };
            let notif = HirExpr {
                node: SENTINEL_CRASH_CODE_NODE,
                site: SENTINEL_CRASH_CODE_SITE,
                ty: crash_notification_ty.clone(),
                value_class: ValueClass::BitCopy,
                intent: IntentKind::Unknown,
                kind: HirExprKind::StructInit {
                    name: "CrashNotification".to_string(),
                    type_args: Vec::new(),
                    fields: vec![
                        ("actor_id".to_string(), actor_id_ref()),
                        ("kind".to_string(), kind_value),
                    ],
                    base: None,
                },
                span: span.clone(),
            };
            let is_last = idx + 1 == CRASH_KIND_VARIANTS.len();
            let predicate = if is_last {
                hew_hir::HirMatchArmPredicate::Wildcard
            } else {
                hew_hir::HirMatchArmPredicate::Literal {
                    // `idx` is bounded by CRASH_KIND_VARIANTS.len() (3).
                    lit: HirLiteral::Integer(i64::try_from(idx).unwrap_or(0)),
                    ty: ResolvedTy::I32,
                }
            };
            hew_hir::HirMatchArm {
                scope: None,
                predicate,
                bindings: Vec::new(),
                payload_predicates: Vec::new(),
                payload_variant_predicates: Vec::new(),
                guard: None,
                body: notif,
                span: span.clone(),
            }
        })
        .collect();

    let kind_tag_ref = HirExpr {
        node: SENTINEL_CRASH_CODE_NODE,
        site: SENTINEL_CRASH_CODE_SITE,
        ty: ResolvedTy::I32,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::BindingRef {
            name: "__exit_kind_tag".to_string(),
            resolved: ResolvedRef::Binding(SENTINEL_EXIT_KIND_TAG_BINDING),
        },
        span: span.clone(),
    };

    let match_expr = HirExpr {
        node: SENTINEL_CRASH_CODE_NODE,
        site: SENTINEL_CRASH_CODE_SITE,
        ty: crash_notification_ty.clone(),
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Unknown,
        kind: HirExprKind::Match {
            scrutinee: Box::new(kind_tag_ref),
            arms,
        },
        span: span.clone(),
    };

    // `let note = match __exit_kind_tag { ... };` — preserve the original
    // binding id so user `note.<field>` reads resolve.
    let let_note = HirStmt {
        node: SENTINEL_CRASH_CODE_NODE,
        kind: HirStmtKind::Let(
            HirBinding {
                id: note_param.id,
                name: note_param.name.clone(),
                ty: crash_notification_ty,
                mutable: false,
                span: span.clone(),
                is_consume: false,
            },
            Some(match_expr),
        ),
        span: span.clone(),
    };

    let mut stmts = Vec::with_capacity(body.statements.len() + 1);
    stmts.push(let_note);
    stmts.extend(body.statements.iter().cloned());
    HirBlock {
        statements: stmts,
        ..body
    }
}

/// Rebuild the canonical `DownNotification` from the fixed mailbox ABI fields.
#[expect(
    clippy::too_many_lines,
    reason = "the synthetic typed payload rebuild is one coherent HIR construction"
)]
pub(super) fn build_down_hook_body(body: HirBlock, note_param: &HirBinding) -> HirBlock {
    let span = note_param.span.clone();
    // These are source-owned lifecycle declarations.  The mailbox ABI gives
    // us their values, but not permission to collapse them to leaf names:
    // HIR/MIR layout registries are keyed by the declaration owner.
    let monitor_id_ty = ResolvedTy::named_builtin(
        "std.link_monitor.MonitorId",
        hew_types::BuiltinType::MonitorId,
        Vec::new(),
    );
    let down_target_ty = ResolvedTy::named_builtin(
        "std.link_monitor.DownTarget",
        hew_types::BuiltinType::DownTarget,
        Vec::new(),
    );
    let down_reason_ty = ResolvedTy::named_builtin(
        "std.link_monitor.DownReason",
        hew_types::BuiltinType::DownReason,
        Vec::new(),
    );
    let crash_kind_ty = ResolvedTy::named_builtin(
        "std.failure.CrashKind",
        hew_types::BuiltinType::CrashKind,
        Vec::new(),
    );
    let location_ty =
        ResolvedTy::named_builtin("Location", hew_types::BuiltinType::Location, Vec::new());

    let binding_ref = |name: &str, id: BindingId, ty: ResolvedTy| HirExpr {
        node: SENTINEL_CRASH_CODE_NODE,
        site: SENTINEL_CRASH_CODE_SITE,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::BindingRef {
            name: name.to_string(),
            resolved: ResolvedRef::Binding(id),
        },
        span: span.clone(),
        ty,
    };
    let unit_variant = |machine_name: &str, state_idx: usize, ty: ResolvedTy| HirExpr {
        node: SENTINEL_CRASH_CODE_NODE,
        site: SENTINEL_CRASH_CODE_SITE,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::MachineVariantCtor {
            machine_name: machine_name.to_string(),
            state_idx,
            payload: None,
        },
        span: span.clone(),
        ty,
    };
    let match_arm = |tag: Option<i64>, body: HirExpr| hew_hir::HirMatchArm {
        scope: None,
        predicate: tag.map_or(hew_hir::HirMatchArmPredicate::Wildcard, |value| {
            hew_hir::HirMatchArmPredicate::Literal {
                lit: HirLiteral::Integer(value),
                ty: ResolvedTy::I32,
            }
        }),
        bindings: Vec::new(),
        payload_predicates: Vec::new(),
        payload_variant_predicates: Vec::new(),
        guard: None,
        body,
        span: span.clone(),
    };

    let monitor = HirExpr {
        node: SENTINEL_CRASH_CODE_NODE,
        site: SENTINEL_CRASH_CODE_SITE,
        ty: monitor_id_ty,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Unknown,
        kind: HirExprKind::StructInit {
            name: "std.link_monitor.MonitorId".to_string(),
            type_args: Vec::new(),
            fields: vec![(
                "value".to_string(),
                binding_ref(
                    "__down_monitor_id",
                    SENTINEL_DOWN_MONITOR_ID_BINDING,
                    ResolvedTy::U64,
                ),
            )],
            base: None,
        },
        span: span.clone(),
    };

    let target = HirExpr {
        node: SENTINEL_CRASH_CODE_NODE,
        site: SENTINEL_CRASH_CODE_SITE,
        ty: down_target_ty.clone(),
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Unknown,
        kind: HirExprKind::Match {
            scrutinee: Box::new(binding_ref(
                "__down_target_kind",
                SENTINEL_DOWN_TARGET_KIND_BINDING,
                ResolvedTy::I32,
            )),
            arms: vec![
                match_arm(
                    Some(0),
                    HirExpr {
                        node: SENTINEL_CRASH_CODE_NODE,
                        site: SENTINEL_CRASH_CODE_SITE,
                        ty: down_target_ty.clone(),
                        value_class: ValueClass::BitCopy,
                        intent: IntentKind::Read,
                        kind: HirExprKind::MachineVariantCtor {
                            machine_name: "std.link_monitor.DownTarget".to_string(),
                            state_idx: 0,
                            payload: Some(vec![(
                                "0".to_string(),
                                binding_ref(
                                    "__down_local_slot",
                                    SENTINEL_DOWN_LOCAL_SLOT_BINDING,
                                    ResolvedTy::U64,
                                ),
                            )]),
                        },
                        span: span.clone(),
                    },
                ),
                match_arm(
                    None,
                    HirExpr {
                        node: SENTINEL_CRASH_CODE_NODE,
                        site: SENTINEL_CRASH_CODE_SITE,
                        ty: down_target_ty.clone(),
                        value_class: ValueClass::BitCopy,
                        intent: IntentKind::Read,
                        kind: HirExprKind::MachineVariantCtor {
                            machine_name: "std.link_monitor.DownTarget".to_string(),
                            state_idx: 1,
                            payload: Some(vec![(
                                "0".to_string(),
                                binding_ref(
                                    "__down_location",
                                    SENTINEL_DOWN_LOCATION_BINDING,
                                    location_ty,
                                ),
                            )]),
                        },
                        span: span.clone(),
                    },
                ),
            ],
        },
        span: span.clone(),
    };

    let crash_kind = HirExpr {
        node: SENTINEL_CRASH_CODE_NODE,
        site: SENTINEL_CRASH_CODE_SITE,
        ty: crash_kind_ty.clone(),
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Unknown,
        kind: HirExprKind::Match {
            scrutinee: Box::new(binding_ref(
                "__down_crash_kind",
                SENTINEL_DOWN_CRASH_KIND_BINDING,
                ResolvedTy::I32,
            )),
            arms: vec![
                match_arm(
                    Some(0),
                    unit_variant("std.failure.CrashKind", 0, crash_kind_ty.clone()),
                ),
                match_arm(
                    Some(1),
                    unit_variant("std.failure.CrashKind", 1, crash_kind_ty.clone()),
                ),
                match_arm(
                    None,
                    unit_variant("std.failure.CrashKind", 2, crash_kind_ty.clone()),
                ),
            ],
        },
        span: span.clone(),
    };
    let reason = HirExpr {
        node: SENTINEL_CRASH_CODE_NODE,
        site: SENTINEL_CRASH_CODE_SITE,
        ty: down_reason_ty.clone(),
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Unknown,
        kind: HirExprKind::Match {
            scrutinee: Box::new(binding_ref(
                "__down_reason_kind",
                SENTINEL_DOWN_REASON_KIND_BINDING,
                ResolvedTy::I32,
            )),
            arms: vec![
                match_arm(
                    Some(0),
                    unit_variant("std.link_monitor.DownReason", 0, down_reason_ty.clone()),
                ),
                match_arm(
                    Some(1),
                    HirExpr {
                        node: SENTINEL_CRASH_CODE_NODE,
                        site: SENTINEL_CRASH_CODE_SITE,
                        ty: down_reason_ty.clone(),
                        value_class: ValueClass::BitCopy,
                        intent: IntentKind::Read,
                        kind: HirExprKind::MachineVariantCtor {
                            machine_name: "std.link_monitor.DownReason".to_string(),
                            state_idx: 1,
                            payload: Some(vec![("0".to_string(), crash_kind)]),
                        },
                        span: span.clone(),
                    },
                ),
                match_arm(
                    Some(2),
                    unit_variant("std.link_monitor.DownReason", 2, down_reason_ty.clone()),
                ),
                match_arm(
                    None,
                    unit_variant("std.link_monitor.DownReason", 3, down_reason_ty.clone()),
                ),
            ],
        },
        span: span.clone(),
    };

    let note = HirExpr {
        node: SENTINEL_CRASH_CODE_NODE,
        site: SENTINEL_CRASH_CODE_SITE,
        ty: note_param.ty.clone(),
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Unknown,
        kind: HirExprKind::StructInit {
            name: "std.link_monitor.DownNotification".to_string(),
            type_args: Vec::new(),
            fields: vec![
                ("monitor".to_string(), monitor),
                ("target".to_string(), target),
                ("reason".to_string(), reason),
            ],
            base: None,
        },
        span: span.clone(),
    };
    let let_note = HirStmt {
        node: SENTINEL_CRASH_CODE_NODE,
        kind: HirStmtKind::Let(
            HirBinding {
                id: note_param.id,
                name: note_param.name.clone(),
                ty: note_param.ty.clone(),
                mutable: false,
                span: span.clone(),
                is_consume: false,
            },
            Some(note),
        ),
        span,
    };

    let mut statements = Vec::with_capacity(body.statements.len() + 1);
    statements.push(let_note);
    statements.extend(body.statements.iter().cloned());
    HirBlock { statements, ..body }
}
pub(super) fn context_reader_offset(reader: ExecutionContextReader) -> usize {
    match reader {
        ExecutionContextReader::ActorId => HEW_CTX_OFFSET_ACTOR_ID,
        ExecutionContextReader::Supervisor => HEW_CTX_OFFSET_PARENT_SUPERVISOR,
        ExecutionContextReader::TraceSpan => HEW_CTX_OFFSET_TRACE_SPAN,
    }
}
pub(super) fn literal_match_scrutinee_ty(ty: &ResolvedTy) -> bool {
    ty.is_integer_literal_match_scrutinee()
        || matches!(ty, ResolvedTy::Bool | ResolvedTy::Char | ResolvedTy::String)
}
/// Classify a resolved integer type as signed or unsigned. Returns
/// `None` for non-integer types — callers that demand an integer
/// signedness (the B-2 overflow-trap lowering) fail closed when this
/// returns `None`. Platform-sized `Isize` / `Usize` are canonicalised
/// to their pointer-width LLVM type by codegen; here we only need the
/// signedness discriminator so the intrinsic family selection is
/// correct regardless of pointer width.
pub(super) fn integer_signedness(ty: &ResolvedTy) -> Option<IntSignedness> {
    match ty {
        ResolvedTy::I8
        | ResolvedTy::I16
        | ResolvedTy::I32
        | ResolvedTy::I64
        | ResolvedTy::Isize
        // `duration` is a newtype around a signed 8-byte nanosecond count.
        // Default arithmetic (`d1 + d2`, `d * n`, `d / n`) lowers through the
        // same B-2 overflow-trap / div-by-zero path as `i64`: the dest local
        // keeps its `Duration` type (so drop / value-class are unaffected),
        // but the arithmetic instruction treats it as a signed 8-byte integer.
        | ResolvedTy::Duration
        // `instant` is ABI-identical to i64 (a monotonic nanosecond timestamp).
        // When the left operand of `instant + duration` or `instant - duration`
        // was introduced via an annotation (`let t: instant`, `fn f(t: instant)`),
        // `binary_ty` preserves the original `Named{Instant}` result type so it
        // matches the `-> instant` return annotation. MIR therefore receives
        // `Named{Instant}` here and must classify it as signed-integer arithmetic.
        // Field-storage arms (`value_class`, `state_clone`, `primitive_to_llvm`)
        // are unchanged; the dest local keeps its `Named{Instant}` type.
        | ResolvedTy::Named {
            builtin: Some(hew_types::BuiltinType::Instant),
            ..
        } => Some(IntSignedness::Signed),
        ResolvedTy::U8
        | ResolvedTy::U16
        | ResolvedTy::U32
        | ResolvedTy::U64
        | ResolvedTy::Usize => Some(IntSignedness::Unsigned),
        _ => None,
    }
}
pub(super) fn numeric_method_op(op: NumericMethodOp) -> IntArithOp {
    match op {
        NumericMethodOp::Add => IntArithOp::Add,
        NumericMethodOp::Sub => IntArithOp::Sub,
        NumericMethodOp::Mul => IntArithOp::Mul,
    }
}
/// Upgrade or keep an integer comparison predicate based on operand signedness.
///
/// `Eq`/`NotEq` are bit-equality and are signedness-agnostic — returned
/// unchanged.  For ordering predicates, returns the `Unsigned*` variant when
/// both operands are unsigned integers so that `icmp ult/ule/ugt/uge` is
/// emitted rather than the signed equivalents.  This is the correctness
/// boundary: a signed predicate on an unsigned `0x8000_0000_0000_0000u64`
/// would treat it as negative, making `0x8000… > 1` silently return `false`.
///
/// Returns `None` if the operands have mismatched signedness (which the type
/// checker rejects before MIR, so this is a fail-closed guard for any future
/// regression).
pub(super) fn cmp_select_by_signedness(
    pred: CmpPred,
    lhs_ty: &ResolvedTy,
    rhs_ty: &ResolvedTy,
) -> Option<CmpPred> {
    // Equality is bit-equality: signedness-agnostic.
    if matches!(pred, CmpPred::Eq | CmpPred::NotEq) {
        return Some(pred);
    }
    let lhs_sign = integer_signedness(lhs_ty);
    let rhs_sign = integer_signedness(rhs_ty);
    match (lhs_sign, rhs_sign) {
        (Some(IntSignedness::Unsigned), Some(IntSignedness::Unsigned)) => {
            let unsigned_pred = match pred {
                CmpPred::SignedLess => CmpPred::UnsignedLess,
                CmpPred::SignedLessEq => CmpPred::UnsignedLessEq,
                CmpPred::SignedGreater => CmpPred::UnsignedGreater,
                CmpPred::SignedGreaterEq => CmpPred::UnsignedGreaterEq,
                // Already unsigned or non-ordering — pass through.
                other => other,
            };
            Some(unsigned_pred)
        }
        (Some(IntSignedness::Signed), Some(IntSignedness::Signed)) => {
            // Both signed: signed predicates are already correct.
            Some(pred)
        }
        (Some(_), Some(_)) => {
            // Mismatched signedness — the type checker should have rejected
            // this.  Fail closed: return None so the caller emits no
            // instruction rather than silently picking a wrong predicate.
            None
        }
        // Non-integer operands (floats, bools, etc.) take the float or
        // other branch before reaching IntCmp; pass through for those
        // callers.
        _ => Some(pred),
    }
}
pub(super) fn numeric_method_signedness(signedness: NumericSignedness) -> IntSignedness {
    match signedness {
        NumericSignedness::Signed => IntSignedness::Signed,
        NumericSignedness::Unsigned => IntSignedness::Unsigned,
    }
}
/// Return the bit-width for a concrete integer type.
///
/// `ptr_width` is the target pointer width threaded onto the builder
/// (`Builder::pointer_width`, derived from `TargetArch`). The platform-sized
/// `Isize`/`Usize` arms resolve to `ptr_width.bits()` (32 on wasm32, 64 on
/// native) so the shift-out-of-range bound matches the LLVM integer width
/// codegen emits for the type. Returns `None` only for non-integer types.
///
/// The width MUST come from `ptr_width` (target-derived), never a host
/// `cfg!(target_pointer_width)`: a cross-compile would otherwise emit the
/// host width into a different-width target — a fail-open shift guard.
pub(super) fn integer_bit_width(ty: &ResolvedTy, ptr_width: PointerWidth) -> Option<i64> {
    match ty {
        ResolvedTy::I8 | ResolvedTy::U8 => Some(8),
        ResolvedTy::I16 | ResolvedTy::U16 => Some(16),
        ResolvedTy::I32 | ResolvedTy::U32 => Some(32),
        ResolvedTy::I64 | ResolvedTy::U64 => Some(64),
        ResolvedTy::Isize | ResolvedTy::Usize => Some(ptr_width.bits()),
        // Non-integer types have no bit-width.
        _ => None,
    }
}
/// Classify a resolved type as a float width. Returns `None` for
/// non-float types. Used to dispatch float arithmetic lowering in
/// `lower_binary` and `lower_div_rem` before falling through to the
/// integer-only `IntArithChecked` / `lower_div_rem` paths.
pub(super) fn float_width(ty: &ResolvedTy) -> Option<FloatWidth> {
    match ty {
        ResolvedTy::F32 => Some(FloatWidth::F32),
        ResolvedTy::F64 => Some(FloatWidth::F64),
        _ => None,
    }
}
pub(super) fn unary_op_label(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Not => "!",
        UnaryOp::Negate => "-",
        UnaryOp::BitNot => "~",
        UnaryOp::RawDeref => "*",
    }
}
/// Return the signed minimum value for a concrete signed integer type
/// as an `i64`. Used to emit the `lhs == iN::MIN` constant in the
/// signed-MIN/-1 trap check for `/` and `%`.
///
/// `ptr_width` (target-derived, see [`integer_bit_width`]) resolves the
/// platform-sized `Isize` MIN to `i32::MIN`/`i64::MIN` by width. Returns `None`
/// for unsigned types (no MIN check) and non-integer types. Callers must
/// fail-closed when this returns `None`.
pub(super) fn signed_min_value(ty: &ResolvedTy, ptr_width: PointerWidth) -> Option<i64> {
    match ty {
        ResolvedTy::I8 => Some(i64::from(i8::MIN)),
        ResolvedTy::I16 => Some(i64::from(i16::MIN)),
        ResolvedTy::I32 => Some(i64::from(i32::MIN)),
        // `duration` is a signed 8-byte nanosecond count; its MIN is `i64::MIN`,
        // so `dur / int` gets the same signed-MIN/-1 trap guard as `i64 / int`.
        ResolvedTy::I64 | ResolvedTy::Duration => Some(i64::MIN),
        ResolvedTy::Isize => Some(ptr_width.isize_min()),
        // Unsigned types (including Usize): no MIN check needed.
        _ => None,
    }
}
pub(super) fn actor_name_from_handle_ty(ty: &ResolvedTy) -> Option<&str> {
    match ty {
        ResolvedTy::Named {
            args,
            builtin: Some(BuiltinType::LocalPid),
            ..
        } if args.len() == 1 => match &args[0] {
            ResolvedTy::Named { name, args, .. } if args.is_empty() => Some(name.as_str()),
            _ => None,
        },
        _ => None,
    }
}
pub(super) fn actor_name_from_remote_pid_ty(ty: &ResolvedTy) -> Option<&str> {
    match ty {
        ResolvedTy::Named {
            args,
            builtin: Some(BuiltinType::RemotePid),
            ..
        } if args.len() == 1 => match &args[0] {
            ResolvedTy::Named { name, args, .. } if args.is_empty() => Some(name.as_str()),
            _ => None,
        },
        _ => None,
    }
}
pub(super) fn named_type_marker(
    ty: &ResolvedTy,
    type_classes: &hew_hir::TypeClassTable,
) -> Option<ResourceMarker> {
    hew_hir::lookup_type_marker_for_ty(ty, type_classes)
}

/// Whether `ty` denotes a canonical stdlib lifecycle payload.
///
/// HIR may carry either a builtin discriminator or the exact module-owned
/// source identity. Bare and foreign same-short-name user types are distinct
/// nominal types and remain rejected.
pub(super) fn is_canonical_lifecycle_named_ty(
    ty: &ResolvedTy,
    builtin: hew_types::BuiltinType,
    source_identity: &str,
) -> bool {
    matches!(
        ty,
        ResolvedTy::Named {
            name,
            args,
            builtin: resolved_builtin,
            ..
        } if args.is_empty()
            && (*resolved_builtin == Some(builtin)
                || (resolved_builtin.is_none() && name == source_identity))
    )
}

fn builtin_registration_fields_match(
    actual: &[(String, ResolvedTy)],
    expected: &[hew_hir::builtin_type_classes::BuiltinTypeField],
) -> bool {
    actual.len() == expected.len()
        && actual
            .iter()
            .zip(expected)
            .all(|((name, ty), field)| name == field.name && *ty == field.ty.to_resolved_ty())
}
pub(super) fn is_crash_info_payload_ty(
    ty: &ResolvedTy,
    _type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
) -> bool {
    let ResolvedTy::Named { name, .. } = ty else {
        return false;
    };
    // M-5: `CrashInfo` now carries an owned `message: string`, so it is no
    // longer marker-`BitCopy`. The authoritative discriminant is the
    // `CrashInfo` role on the builtin registration, not the marker.
    if !is_canonical_lifecycle_named_ty(
        ty,
        hew_types::BuiltinType::CrashInfo,
        "std.failure.CrashInfo",
    ) {
        return false;
    }

    let canonical_name = hew_types::BuiltinType::CrashInfo.canonical_name();
    let Some(registration) =
        hew_hir::builtin_type_classes::builtin_type_registration(canonical_name)
    else {
        return false;
    };
    if registration.role != Some(hew_hir::builtin_type_classes::BuiltinTypeRole::CrashInfo) {
        return false;
    }
    let hew_hir::builtin_type_classes::BuiltinTypeShape::Struct(expected_fields) =
        registration.shape
    else {
        return false;
    };
    record_field_orders
        .get(
            &hew_hir::compiler_record_layout_key(hew_types::BuiltinType::CrashInfo, &[])
                .expect("CrashInfo has a compiler-owned struct layout"),
        )
        .or_else(|| record_field_orders.get(name))
        .or_else(|| record_field_orders.get("std.failure.CrashInfo"))
        .or_else(|| record_field_orders.get(canonical_name))
        .is_some_and(|actual_fields| {
            builtin_registration_fields_match(actual_fields, expected_fields)
        })
}
pub(super) fn register_builtin_record_layouts(
    record_layouts: &mut Vec<crate::model::RecordLayout>,
    record_field_orders: &mut HashMap<String, Vec<(String, ResolvedTy)>>,
) {
    for registration in hew_hir::builtin_type_classes::compiler_record_layout_registrations() {
        let hew_hir::builtin_type_classes::BuiltinTypeShape::Struct(fields) = registration.shape
        else {
            continue;
        };
        let key = hew_hir::compiler_record_layout_key(registration.builtin, &[])
            .expect("every Struct builtin registration has a compiler record key");
        if let Some(existing_fields) = record_field_orders.get(&key) {
            assert!(
                builtin_registration_fields_match(existing_fields, fields),
                "compiler record layout `{key}` was registered with a conflicting shape"
            );
            continue;
        }

        let fields: Vec<(String, ResolvedTy)> = fields
            .iter()
            .map(|field| (field.name.to_string(), field.ty.to_resolved_ty()))
            .collect();
        record_layouts.push(crate::model::RecordLayout {
            name: key.clone(),
            field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
            field_names: fields.iter().map(|(name, _)| name.clone()).collect(),
        });
        record_field_orders.insert(key, fields);
    }
}

/// The declaring module of every synthetic lifecycle shape below, read from
/// the checker's owner table rather than restated here — one owner set, one
/// place, so a moved declaration cannot key MIR's layouts differently from the
/// identity the checker minted.
fn link_monitor_owner() -> &'static str {
    hew_types::SOURCE_OWNED_LIFECYCLE_OWNERS
        .iter()
        .find(|owner| owner.declares.contains(&hew_types::BuiltinType::MonitorId))
        .map_or("std.link_monitor", |owner| owner.canonical_path)
}

/// Register the source-owned shapes that synthetic lifecycle hooks reconstruct
/// from the fixed mailbox ABI.  These declarations normally arrive through an
/// imported `std.link_monitor` HIR item; direct compilation of that stdlib
/// source has no importing program item, yet can still exercise the same
/// synthetic hook path.
///
/// Registration is under the DECLARATION OWNER only.  The leaf spelling was
/// published alongside it so a root compile of `std/link_monitor.hew` could
/// find the shapes under its own bare rendering; that second key put two
/// identities for one declaration into the global layout tables, where a user
/// `MonitorId` and the runtime one differ only by which was registered first.
/// Synthetic hook bodies construct through the owner-qualified identity, so
/// the leaf key had no remaining consumer.
#[expect(
    clippy::too_many_lines,
    reason = "the source-owned lifecycle layout catalog is one atomic mailbox ABI boundary"
)]
pub(super) fn register_lifecycle_hook_layouts(
    record_layouts: &mut Vec<crate::model::RecordLayout>,
    record_field_orders: &mut HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &mut Vec<crate::model::EnumLayout>,
) {
    let lifecycle_ty = |name: &str, builtin| ResolvedTy::named_builtin(name, builtin, Vec::new());
    let register_records =
        |prefix: &str,
         record_layouts: &mut Vec<crate::model::RecordLayout>,
         record_field_orders: &mut HashMap<String, Vec<(String, ResolvedTy)>>| {
            let named = |leaf: &str| {
                if prefix.is_empty() {
                    leaf.to_string()
                } else {
                    format!("{prefix}.{leaf}")
                }
            };
            for (name, fields) in [
                (
                    named("MonitorId"),
                    vec![("value".to_string(), ResolvedTy::U64)],
                ),
                (
                    named("DownNotification"),
                    vec![
                        (
                            "monitor".to_string(),
                            lifecycle_ty(&named("MonitorId"), hew_types::BuiltinType::MonitorId),
                        ),
                        (
                            "target".to_string(),
                            lifecycle_ty(&named("DownTarget"), hew_types::BuiltinType::DownTarget),
                        ),
                        (
                            "reason".to_string(),
                            lifecycle_ty(&named("DownReason"), hew_types::BuiltinType::DownReason),
                        ),
                    ],
                ),
            ] {
                if record_field_orders.contains_key(&name) {
                    continue;
                }
                record_layouts.push(crate::model::RecordLayout {
                    name: name.clone(),
                    field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
                    field_names: fields.iter().map(|(field, _)| field.clone()).collect(),
                });
                record_field_orders.insert(name, fields);
            }
        };
    let register_enums = |prefix: &str, enum_layouts: &mut Vec<crate::model::EnumLayout>| {
        let named = |leaf: &str| {
            if prefix.is_empty() {
                leaf.to_string()
            } else {
                format!("{prefix}.{leaf}")
            }
        };
        for (name, variants) in [
            (
                named("DownTarget"),
                vec![
                    crate::model::MachineVariantLayout {
                        name: "Local".to_string(),
                        field_tys: vec![ResolvedTy::U64],
                        field_names: Vec::new(),
                    },
                    crate::model::MachineVariantLayout {
                        name: "Remote".to_string(),
                        field_tys: vec![ResolvedTy::named_builtin(
                            "Location",
                            hew_types::BuiltinType::Location,
                            Vec::new(),
                        )],
                        field_names: Vec::new(),
                    },
                ],
            ),
            (
                named("DownReason"),
                vec![
                    crate::model::MachineVariantLayout {
                        name: "Exited".to_string(),
                        field_tys: Vec::new(),
                        field_names: Vec::new(),
                    },
                    crate::model::MachineVariantLayout {
                        name: "Crashed".to_string(),
                        field_tys: vec![ResolvedTy::named_builtin(
                            "std.failure.CrashKind",
                            hew_types::BuiltinType::CrashKind,
                            Vec::new(),
                        )],
                        field_names: Vec::new(),
                    },
                    crate::model::MachineVariantLayout {
                        name: "MonitorLost".to_string(),
                        field_tys: Vec::new(),
                        field_names: Vec::new(),
                    },
                    crate::model::MachineVariantLayout {
                        name: "LocalShutdown".to_string(),
                        field_tys: Vec::new(),
                        field_names: Vec::new(),
                    },
                ],
            ),
        ] {
            if enum_layouts.iter().any(|layout| layout.name == name) {
                continue;
            }
            let tag_width = u32::max(1, variants.len().next_power_of_two().trailing_zeros());
            enum_layouts.push(crate::model::EnumLayout {
                name,
                tag_width,
                variants,
                is_indirect: false,
            });
        }
    };
    let owner = link_monitor_owner();
    register_records(owner, record_layouts, record_field_orders);
    register_enums(owner, enum_layouts);
}
/// Register `EnumLayout` entries for monomorphic builtin enums declared
/// in `std/builtins.hew` (e.g. `LookupError`).
///
/// These enums have no `HirItem::TypeDecl` in user source (builtins.hew
/// is consumed by the checker for signature wiring, not the HIR third
/// pass) and no generic-enum-registry entry (they have no type params),
/// so MIR has no other path to learn their tagged-union layout. Codegen's
/// `register_enum_layouts` reads `pipeline.enum_layouts` to build LLVM
/// tagged-union types; `Builder::is_known_actor_runtime_ty` reads
/// `machine_layout_names` to classify the type as `BitCopy`.
///
/// Returns the set of exact registered identities so the caller can project
/// them through `machine_layout_key` and fold those canonical classification
/// keys into `machine_layout_names`. A same-leaf user enum is a distinct
/// nominal and therefore coexists with the generated builtin layout.
pub(super) fn register_builtin_monomorphic_enum_layouts(
    enum_layouts: &mut Vec<crate::model::EnumLayout>,
) -> Vec<String> {
    let existing: HashSet<String> = enum_layouts.iter().map(|el| el.name.clone()).collect();
    let mut registered = Vec::new();
    for spec in hew_types::builtin_enums::monomorphic_builtin_enums() {
        if existing.contains(spec.canonical_name) {
            continue;
        }
        let variant_count = u32::try_from(spec.variants.len().max(1)).unwrap_or(u32::MAX);
        let tag_width = u32::max(1, variant_count.next_power_of_two().trailing_zeros());
        let variants: Vec<crate::model::MachineVariantLayout> = spec
            .variants
            .iter()
            .map(|v| crate::model::MachineVariantLayout {
                name: v.name.to_string(),
                field_tys: Vec::new(),
                field_names: Vec::new(),
            })
            .collect();
        enum_layouts.push(crate::model::EnumLayout {
            name: spec.canonical_name.to_string(),
            tag_width,
            variants,
            is_indirect: false,
        });
        registered.push(spec.canonical_name.to_string());
    }
    registered
}
pub(super) fn method_name_from_id(method_id: &str) -> &str {
    method_id.rsplit("::").next().unwrap_or(method_id)
}
/// W3.031 Stage 1.6: walk a checker-substituted `FnSig` looking for
/// types that indicate substitution did not finish (and would render
/// the caller-side erased call type unbuildable in codegen). Returns
/// `Some(reason)` naming the first offender in declaration order
/// (params left-to-right, then return type), or `None` if the
/// signature is fully resolved.
///
/// "Unresolved" here means:
/// - `Ty::Var` — an inference variable that did not unify;
/// - `Ty::Error` — a checker poison value (a prior diagnostic fired);
/// - `Ty::AssocType` — an unprojected `Self::Foo` projection (the
///   trait-object bound was missing the corresponding assoc binding,
///   so [`Checker::apply_trait_object_bound_substitutions`] could not
///   reach the projection — copilot-instructions §3 / LESSONS
///   `checker-output-boundary`).
pub(super) fn unresolved_fn_sig_reason(sig: &hew_types::FnSig) -> Option<String> {
    fn first_unresolved(ty: &hew_types::Ty) -> Option<String> {
        use hew_types::Ty;
        match ty {
            Ty::Var(v) => Some(format!("Ty::Var({})", v.0)),
            Ty::Error => Some("Ty::Error".to_string()),
            Ty::AssocType {
                base,
                trait_name,
                assoc_name,
            } => Some(format!(
                "unresolved Ty::AssocType `{}.{assoc_name}` on base `{}`",
                trait_name,
                base.user_facing()
            )),
            Ty::Named { args, .. } => args.iter().find_map(first_unresolved),
            Ty::Tuple(items) => items.iter().find_map(first_unresolved),
            Ty::Array(inner, _) | Ty::Slice(inner) | Ty::Task(inner) => first_unresolved(inner),
            Ty::Pointer { pointee, .. } => first_unresolved(pointee),
            Ty::Function { params, ret, .. } => params
                .iter()
                .find_map(first_unresolved)
                .or_else(|| first_unresolved(ret)),
            Ty::Closure {
                params,
                ret,
                captures,
            } => params
                .iter()
                .chain(captures.iter())
                .find_map(first_unresolved)
                .or_else(|| first_unresolved(ret)),
            Ty::TraitObject { traits } => traits.iter().find_map(|bound| {
                bound.args.iter().find_map(first_unresolved).or_else(|| {
                    bound
                        .assoc_bindings
                        .iter()
                        .find_map(|(_, t)| first_unresolved(t))
                })
            }),
            _ => None,
        }
    }
    for (idx, p) in sig.params.iter().enumerate() {
        if let Some(reason) = first_unresolved(p) {
            return Some(format!("param #{idx}: {reason}"));
        }
    }
    if let Some(reason) = first_unresolved(&sig.return_type) {
        return Some(format!("return type: {reason}"));
    }
    None
}
pub(super) fn is_self_expr(expr: &HirExpr) -> bool {
    matches!(
        &expr.kind,
        HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Unresolved | ResolvedRef::Binding(_) | ResolvedRef::Item(_)
        } if name == "self"
    )
}
/// Run Checked MIR's legality passes over a function's statement
/// stream. Two real passes ship today (use-after-consume,
/// initialised-before-use); the aliasing, generator-borrow-across-
/// yield, and actor-send-escape variants are declared on `MirCheck`
/// but have no construction surface in the v0.5 integer spine yet
/// (no borrow ops in `Instr`, no projection variants on `Place`, no
/// construction site for `Terminator::Yield` / `Terminator::Send`).
/// The `MirCheck::DecisionMapTotal` invariant fires if any
/// `DecisionFact` in this function carries `Strategy::UnknownBlocked`.
///
/// Delegates to `dataflow::analyze` which runs the four-state lattice
/// (`Uninit / Live / Consumed / MaybeConsumed`) over the CFG's basic
/// blocks via a forward fixpoint. Per-block transfer emits
/// `InitialisedBeforeUse` on `Uninit` reads and `UseAfterConsume` on
/// `Consumed`/`MaybeConsumed` reads; the inter-block meet rule is
/// `Uninit ⊔ X = Uninit` (most-conservative). `If`-lowering (Slice 2)
/// produces `Branch` + two arm blocks + a join block, so the
/// path-sensitive cases that a flat-stream scan would mishandle
/// (false-positive on mutually-exclusive `consume` arms; false-negative
/// for a binding consumed on only one path) are handled correctly by
/// the per-block fixpoint. LESSONS: `boundary-fail-closed` — verify
/// the substrate is path-sensitive before relying on it for linear
/// safety, and mandate property tests on meet rules before landing.
pub(super) fn check_function(
    builder: &Builder,
    blocks: &[BasicBlock],
    func: &HirFn,
) -> dataflow::DataflowResult {
    // Collect the BindingId of each parameter so the dataflow checker can
    // pre-seed them as `Live` at function entry.  Parameters are initialised
    // by the calling convention (LLVM function argument + parameter prologue
    // in codegen), never by a `Bind` statement in the checker-authority stream.
    let param_ids: Vec<hew_hir::BindingId> = func.params.iter().map(|p| p.id).collect();
    let mut result = dataflow::analyze_with_binding_locals(
        blocks,
        &builder.type_classes,
        &param_ids,
        &builder.binding_locals,
    );
    let checks = &mut result.checks;

    // DecisionMapTotal. Every `DecisionFact` on this function must
    // carry a concrete `Strategy` — `Strategy::UnknownBlocked` is a
    // lowering escape valve that must never reach the emitter. This
    // pass is independent of the per-block dataflow.
    let offending: Vec<_> = builder
        .decisions
        .iter()
        .filter(|d| d.strategy == Strategy::UnknownBlocked)
        .filter(|d| !is_unsupported_user_record_value_class_ty(&d.ty, builder))
        .map(|d| d.site)
        .collect();
    if !offending.is_empty() {
        checks.push(MirCheck::DecisionMapTotal {
            offending_sites: offending,
        });
    }

    // WitnessOperandUnresolved. Every witness instruction
    // (`Instr::WitnessSizeOf` and friends) carries a `ResolvedTy` operand.
    // The construction boundary (`WitnessOperand::resolve`) already rejects
    // checker-internal `Ty` leaks; the verifier re-checks the residual
    // invariant that any `ResolvedTy::TypeParam` operand names a type
    // parameter declared on the enclosing function. An out-of-scope abstract
    // type is a lowering bug — surface it as a hard rejection.
    //
    // Monomorphic bodies carry no witness ops and no declared type params,
    // so this pass is a no-op on the codegen-bound pipeline; it guards the
    // abstract bodies routed to `polymorphic_mir`. Fast-path: skip the scan
    // (and the set allocation) entirely when no witness instruction is
    // present, which is every function in a monomorphic program.
    let has_witness_op = blocks.iter().any(|block| {
        block.instructions.iter().any(|instr| {
            matches!(
                instr,
                Instr::WitnessSizeOf { .. }
                    | Instr::WitnessAlignOf { .. }
                    | Instr::WitnessDropGlue { .. }
                    | Instr::WitnessMove { .. }
            )
        })
    });
    if has_witness_op {
        let declared_type_params: HashSet<String> = func.type_params.iter().cloned().collect();
        for block in blocks {
            for instr in &block.instructions {
                let operand = match instr {
                    Instr::WitnessSizeOf { ty, .. }
                    | Instr::WitnessAlignOf { ty, .. }
                    | Instr::WitnessDropGlue { ty, .. }
                    | Instr::WitnessMove { ty, .. } => Some(ty),
                    _ => None,
                };
                let Some(ty) = operand else { continue };
                for name in undeclared_type_params(ty, &declared_type_params) {
                    checks.push(MirCheck::WitnessOperandUnresolved {
                        ty: format!("{ty:?}"),
                        reason: format!(
                            "witness operand references type parameter `{name}` \
                             not declared on the enclosing function"
                        ),
                    });
                }
            }
        }
    }

    result
}
/// Collect every `ResolvedTy::TypeParam` name reachable inside `ty` that is
/// NOT present in `declared`. Used by the MIR witness-operand verifier to
/// reject abstract operands that escape their declaring scope. Returns names
/// in first-seen traversal order (deterministic for diagnostics).
fn undeclared_type_params(ty: &ResolvedTy, declared: &HashSet<String>) -> Vec<String> {
    let mut out = Vec::new();
    collect_undeclared_type_params(ty, declared, &mut out);
    out
}
fn collect_undeclared_type_params(
    ty: &ResolvedTy,
    declared: &HashSet<String>,
    out: &mut Vec<String>,
) {
    match ty {
        ResolvedTy::TypeParam { name } if !declared.contains(name) && !out.contains(name) => {
            out.push(name.clone());
        }
        ResolvedTy::Tuple(elems) => {
            for e in elems {
                collect_undeclared_type_params(e, declared, out);
            }
        }
        ResolvedTy::Array(inner, _) | ResolvedTy::Slice(inner) | ResolvedTy::Task(inner) => {
            collect_undeclared_type_params(inner, declared, out);
        }
        ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
            collect_undeclared_type_params(pointee, declared, out);
        }
        ResolvedTy::TraitObject { traits } => {
            for bound in traits {
                for a in &bound.args {
                    collect_undeclared_type_params(a, declared, out);
                }
                for (_, t) in &bound.assoc_bindings {
                    collect_undeclared_type_params(t, declared, out);
                }
            }
        }
        ResolvedTy::Named { args, .. } => {
            for a in args {
                collect_undeclared_type_params(a, declared, out);
            }
        }
        ResolvedTy::Function { params, ret } => {
            for p in params {
                collect_undeclared_type_params(p, declared, out);
            }
            collect_undeclared_type_params(ret, declared, out);
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => {
            for p in params {
                collect_undeclared_type_params(p, declared, out);
            }
            collect_undeclared_type_params(ret, declared, out);
            for c in captures {
                collect_undeclared_type_params(c, declared, out);
            }
        }
        _ => {}
    }
}
#[must_use]
/// True for the concrete integer types a folded integer const may carry.
/// `ResolvedTy` has no inference-variable form, so reaching this with a
/// non-integer type means the value/type pair disagreed — a fail-closed
/// signal handled by the const descriptor build.
fn is_concrete_integer_ty(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
    )
}
/// True for the string primitive in either resolved spelling: the bare
/// `ResolvedTy::String` or the `Named { name: "String" }` builtin form.
///
/// This is the sole authority for the string-constant type-compatibility
/// spelling set. [`build_const_descriptors`] applies it when authoring
/// `MirConstValue::Str` descriptors; codegen's const-global emission
/// re-validates through this same function as defence-in-depth and must
/// never respell the predicate locally, or the two layers could drift on
/// which spellings count as `String`. The accepted spelling set is pinned
/// by `string_const_ty_spelling_set_is_pinned` in the const-descriptor
/// diagnostics tests.
#[must_use]
pub fn is_string_const_ty(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::String)
        || matches!(ty, ResolvedTy::Named { name, .. } if name == "String")
}
fn is_float_const_ty(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::F32 | ResolvedTy::F64)
}
/// Build the module-level constant descriptors from a lowered [`HirModule`],
/// mirroring the regex-literal handle-array pattern: one [`MirConst`] per
/// `HirItem::Const`, in declaration order, with `const_id` as the 0-based
/// codegen global-slot index.
///
/// Returns the descriptors plus any fail-closed diagnostics raised when a
/// folded value disagrees with its declared type (integer value ⇒ integer
/// type, string value ⇒ string type). A descriptor is emitted only for a
/// well-typed const; a mismatched const is dropped with a diagnostic so
/// codegen never sees a mistyped global.
///
/// This is the const-substrate seam: HIR resolves `const` references to
/// `ResolvedRef::Const(item_id)` and folds declarations to
/// `HirConstValue`; this converts those into codegen-ready descriptors. The
/// codegen global-load slice consumes the result to back module globals and
/// resolve `ResolvedRef::Const(item_id)` references to their slot. It is kept
/// separate from `lower_hir_module` so the descriptor table can be wired onto
/// the pipeline together with its codegen consumer.
///
/// Risk 3 / type-inference-boundary (P0): `ResolvedTy` has no
/// inference-variable form, so a structurally non-concrete type cannot appear
/// here — but the value/type shapes are still asserted to agree and a
/// mismatch fails closed rather than emitting a mistyped descriptor.
#[must_use]
pub fn build_const_descriptors(module: &HirModule) -> (Vec<MirConst>, Vec<MirDiagnostic>) {
    let mut consts: Vec<MirConst> = Vec::new();
    let mut diagnostics: Vec<MirDiagnostic> = Vec::new();
    for item in &module.items {
        let HirItem::Const(c) = item else {
            continue;
        };
        let value = match &c.value {
            HirConstValue::Integer(v) => {
                if !is_concrete_integer_ty(&c.ty) {
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "const `{}` folded to an integer value but has \
                                 non-integer type `{:?}`",
                                c.name, c.ty
                            ),
                        },
                        note: "const descriptor build requires a concrete integer width \
                               (i8..i64 / u8..u64 / isize / usize) for an integer value"
                            .to_string(),
                    });
                    continue;
                }
                MirConstValue::Integer(*v)
            }
            HirConstValue::String(s) => {
                if !is_string_const_ty(&c.ty) {
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "const `{}` folded to a string value but has \
                                 non-string type `{:?}`",
                                c.name, c.ty
                            ),
                        },
                        note: "const descriptor build requires a String type for a \
                               string value"
                            .to_string(),
                    });
                    continue;
                }
                MirConstValue::Str(s.clone())
            }
            HirConstValue::Float(v) => {
                if !is_float_const_ty(&c.ty) {
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "const `{}` folded to a float value but has \
                                 non-float type `{:?}`",
                                c.name, c.ty
                            ),
                        },
                        note: "const descriptor build requires f32 or f64 for a \
                               float value"
                            .to_string(),
                    });
                    continue;
                }
                MirConstValue::Float(*v)
            }
        };
        let Ok(const_id) = u32::try_from(consts.len()) else {
            // Fail closed rather than saturating to a sentinel `u32::MAX`
            // slot (which would alias the descriptor and silently corrupt
            // codegen). Mirrors the sibling type-mismatch arms above: emit
            // a diagnostic and drop the descriptor instead of emitting one
            // with a fabricated id.
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: format!("const `{}` exceeds the u32 const-table index range", c.name),
                },
                note: "const descriptor build assigns a u32 const_id; a table with \
                       more than u32::MAX entries cannot be represented"
                    .to_string(),
            });
            continue;
        };
        consts.push(MirConst {
            const_id,
            item_id: c.id,
            name: c.name.clone(),
            ty: c.ty.clone(),
            value,
        });
    }
    (consts, diagnostics)
}
/// Returns `true` iff `ty` is the tell-shaped `Result<(), SendError>` that a
/// `.send` on an actor / `Duplex` handle yields when the reply type is unit.
///
/// The value-context materialization path constructs only this shape; ask-shaped
/// `Result<R, AskError>` (a non-unit `Duplex` reply) and any other result type
/// fail closed at the MIR producer (D2) rather than being mis-sized or bound to
/// nothing. The match is structural on the resolved type so a user alias that
/// merely *looks* like `Result<(), SendError>` by name still has to carry the
/// real `SendError` payload variant.
pub(super) fn is_unit_send_error_result(ty: &ResolvedTy) -> bool {
    let ResolvedTy::Named {
        args,
        builtin: Some(hew_types::BuiltinType::Result),
        ..
    } = ty
    else {
        return false;
    };
    let Some(send_error_name) = hew_types::builtin_enums::monomorphic_builtin_enum("SendError")
        .map(|fact| fact.canonical_name)
    else {
        return false;
    };
    matches!(
        args.as_slice(),
        [
            ResolvedTy::Unit,
            ResolvedTy::Named {
                name,
                builtin: Some(hew_types::BuiltinType::SendError),
                ..
            }
        ] if name == send_error_name
    )
}
/// Recognise the checker-recorded `Result<(), CloseError>` shape that
/// `SendHalf`/`RecvHalf`/`Duplex` `.close()` produces. The half-close
/// materialises this from the runtime's i32 status, so the MIR producer
/// must consume the recorded type (`checker-authority`) rather than infer it.
pub(super) fn is_unit_close_error_result(ty: &ResolvedTy) -> bool {
    let ResolvedTy::Named {
        args,
        builtin: Some(hew_types::BuiltinType::Result),
        ..
    } = ty
    else {
        return false;
    };
    matches!(
        args.as_slice(),
        [
            ResolvedTy::Unit,
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::CloseError),
                ..
            }
        ]
    )
}
/// Extract the `Result<R, RecvError>` payload type the recv producers
/// materialise. Returns `None` for any other shape so the producer fails
/// closed on a checker contract drift rather than mis-sizing the payload
/// slot (`checker-authority`, `boundary-fail-closed`).
pub(super) fn recv_result_payload_ty(ty: &ResolvedTy) -> Option<&ResolvedTy> {
    let ResolvedTy::Named {
        args,
        builtin: Some(hew_types::BuiltinType::Result),
        ..
    } = ty
    else {
        return None;
    };
    match args.as_slice() {
        [payload, ResolvedTy::Named {
            builtin: Some(hew_types::BuiltinType::RecvError),
            ..
        }] => Some(payload),
        _ => None,
    }
}
pub(super) fn runtime_symbol_for_call_expr(
    expr: &HirExpr,
) -> Option<(String, &[hew_hir::HirExpr], hew_hir::SiteId)> {
    let HirExprKind::Call { target, args, .. } = &expr.kind else {
        return None;
    };
    // `CallTarget::Runtime` is the sole authority for this edge.  The
    // callee `BindingRef` is deliberately not inspected here: it is a linker
    // presentation carried alongside the semantic target, and may legally
    // have the same leaf spelling as a user declaration.  Re-selecting from
    // that spelling would turn a mutated/stale target into a silent
    // misdispatch.
    //
    // Pre-staged families whose C presentation is not a runtime-ABI symbol
    // intentionally return `None`; the ordinary Call lowerer consumes the
    // same typed family and emits its direct terminator path.
    if let hew_types::CallTarget::Runtime(family) = target {
        // F-string interpolation is the typed StringConcat family. Its MIR
        // producer lives in the ordinary Runtime-family branch so this helper
        // does not turn that semantic identity into a C-symbol dispatch key.
        if family == &hew_types::runtime_call::RuntimeCallFamily::StringConcat {
            return None;
        }
        let symbol = family.c_symbol();
        if crate::runtime_symbols::is_known_runtime_symbol(symbol) {
            return Some((symbol.to_string(), args, expr.site));
        }
    }
    None
}

#[cfg(test)]
mod lifecycle_layout_identity_tests {
    use super::*;

    /// One declaration publishes ONE layout key.
    ///
    /// The synthetic hook catalog used to publish each shape twice — under its
    /// declaration owner AND under its bare leaf — so the global layout tables
    /// held two identities for one declaration, and which one a lookup found
    /// depended on registration order against any user type sharing the leaf.
    #[test]
    fn synthetic_lifecycle_shapes_publish_only_their_declaration_owner() {
        let mut record_layouts = Vec::new();
        let mut record_field_orders = HashMap::new();
        let mut enum_layouts = Vec::new();
        register_lifecycle_hook_layouts(
            &mut record_layouts,
            &mut record_field_orders,
            &mut enum_layouts,
        );
        let owner = link_monitor_owner();
        let published: Vec<&str> = record_field_orders
            .keys()
            .map(String::as_str)
            .chain(enum_layouts.iter().map(|layout| layout.name.as_str()))
            .collect();
        assert!(
            !published.is_empty(),
            "the synthetic hook catalog must publish its shapes"
        );
        for name in published {
            assert!(
                name.strip_prefix(owner)
                    .and_then(|leaf| leaf.strip_prefix('.'))
                    .is_some_and(|leaf| !leaf.contains('.')),
                "`{name}` is not published under the declaration owner `{owner}`"
            );
        }
    }

    /// The MIR catalog's owner and the checker's owner are the same identity.
    #[test]
    fn the_synthetic_catalog_owner_is_the_checker_owner() {
        assert_eq!(
            hew_types::lookup_source_owned_lifecycle_type(&format!(
                "{}.MonitorId",
                link_monitor_owner()
            )),
            Some(hew_types::BuiltinType::MonitorId)
        );
    }
}

#[cfg(test)]
mod builtin_carrier_tests {
    use super::*;

    fn actor_ty() -> ResolvedTy {
        ResolvedTy::named_user("app.Worker", Vec::new())
    }

    #[test]
    fn local_pid_actor_name_uses_builtin_identity_not_presentation() {
        let renamed = ResolvedTy::named_builtin(
            "presentation.RenamedLocalPid",
            BuiltinType::LocalPid,
            vec![actor_ty()],
        );
        assert_eq!(actor_name_from_handle_ty(&renamed), Some("app.Worker"));

        let shadow = ResolvedTy::named_user("LocalPid", vec![actor_ty()]);
        assert_eq!(actor_name_from_handle_ty(&shadow), None);
    }

    #[test]
    fn remote_pid_actor_name_uses_builtin_identity_not_presentation() {
        let renamed = ResolvedTy::named_builtin(
            "presentation.RenamedRemotePid",
            BuiltinType::RemotePid,
            vec![actor_ty()],
        );
        assert_eq!(actor_name_from_remote_pid_ty(&renamed), Some("app.Worker"));

        let shadow = ResolvedTy::named_user("RemotePid", vec![actor_ty()]);
        assert_eq!(actor_name_from_remote_pid_ty(&shadow), None);
    }

    #[test]
    fn builtin_enum_layouts_use_exact_source_identity() {
        let mut layouts = Vec::new();
        let registered = register_builtin_monomorphic_enum_layouts(&mut layouts);

        for expected in [
            "std.builtins.LookupError",
            "std.builtins.LinkError",
            "std.link_monitor.MonitorError",
            "std.failure.CrashAction",
            "std.failure.CrashKind",
        ] {
            assert!(registered.iter().any(|name| name == expected));
            assert!(layouts.iter().any(|layout| layout.name == expected));
        }
        assert!(layouts.iter().all(|layout| layout.name.contains('.')));
    }

    #[test]
    fn same_leaf_user_layout_does_not_suppress_or_inherit_builtin_layout() {
        let mut layouts = vec![crate::model::EnumLayout {
            name: "LinkError".to_string(),
            tag_width: 1,
            variants: vec![crate::model::MachineVariantLayout {
                name: "UserOnly".to_string(),
                field_tys: Vec::new(),
                field_names: Vec::new(),
            }],
            is_indirect: false,
        }];

        register_builtin_monomorphic_enum_layouts(&mut layouts);

        let user = layouts
            .iter()
            .find(|layout| layout.name == "LinkError")
            .expect("same-leaf user layout");
        assert_eq!(user.variants.len(), 1);
        assert_eq!(user.variants[0].name, "UserOnly");
        let builtin = layouts
            .iter()
            .find(|layout| layout.name == "std.builtins.LinkError")
            .expect("canonical builtin layout must coexist");
        assert_ne!(builtin.variants.len(), user.variants.len());
    }

    #[test]
    fn user_monitor_ref_layout_coexists_with_builtin_registration() {
        let mut layouts = Vec::new();
        let mut fields: HashMap<String, Vec<(String, ResolvedTy)>> =
            HashMap::from([("MonitorRef".to_string(), Vec::new())]);

        register_builtin_record_layouts(&mut layouts, &mut fields);

        assert_eq!(fields.get("MonitorRef"), Some(&Vec::new()));
        let builtin_key = hew_hir::compiler_record_layout_key(BuiltinType::MonitorRef, &[])
            .expect("MonitorRef compiler layout key");
        assert!(fields.contains_key(&builtin_key));
        assert!(layouts.iter().any(|layout| layout.name == builtin_key));
    }

    #[test]
    fn result_error_shape_gates_reject_same_leaf_user_types() {
        let result = |error| {
            ResolvedTy::named_builtin(
                "RenamedResult",
                BuiltinType::Result,
                vec![ResolvedTy::Unit, error],
            )
        };

        let send = hew_types::builtin_enums::resolved_monomorphic_builtin_enum_ty("SendError")
            .expect("generated SendError");
        assert!(is_unit_send_error_result(&result(send)));
        assert!(!is_unit_send_error_result(&result(ResolvedTy::named_user(
            "SendError",
            Vec::new(),
        ))));

        let close =
            ResolvedTy::named_builtin("CompilerCloseError", BuiltinType::CloseError, Vec::new());
        assert!(is_unit_close_error_result(&result(close)));
        assert!(!is_unit_close_error_result(&result(
            ResolvedTy::named_user("CloseError", Vec::new(),)
        )));

        let recv =
            ResolvedTy::named_builtin("CompilerRecvError", BuiltinType::RecvError, Vec::new());
        assert_eq!(
            recv_result_payload_ty(&result(recv)),
            Some(&ResolvedTy::Unit)
        );
        assert!(
            recv_result_payload_ty(&result(ResolvedTy::named_user("RecvError", Vec::new())))
                .is_none()
        );
    }
}
