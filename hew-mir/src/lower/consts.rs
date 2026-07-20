use super::{
    dataflow, is_unsupported_user_record_value_class_ty, BasicBlock, BindingId, Builder, CmpPred,
    ExecutionContextReader, FloatWidth, HashMap, HashSet, HirBinding, HirBlock, HirConstValue,
    HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule, HirStmt, HirStmtKind, Instr,
    IntArithOp, IntSignedness, IntentKind, MirCheck, MirConst, MirConstValue, MirDiagnostic,
    MirDiagnosticKind, NumericMethodOp, NumericSignedness, PointerWidth, ResolvedRef, ResolvedTy,
    ResourceMarker, Strategy, UnaryOp, ValueClass, CRASH_KIND_VARIANTS, HEW_CTX_OFFSET_ACTOR_ID,
    HEW_CTX_OFFSET_PARENT_SUPERVISOR, HEW_CTX_OFFSET_TRACE_SPAN, SENTINEL_CRASH_CODE_NODE,
    SENTINEL_CRASH_CODE_SITE, SENTINEL_DOWN_CRASH_KIND_BINDING, SENTINEL_DOWN_LOCAL_SLOT_BINDING,
    SENTINEL_DOWN_LOCATION_BINDING, SENTINEL_DOWN_MONITOR_ID_BINDING,
    SENTINEL_DOWN_REASON_KIND_BINDING, SENTINEL_DOWN_TARGET_KIND_BINDING,
    SENTINEL_EXIT_ACTOR_ID_BINDING, SENTINEL_EXIT_KIND_TAG_BINDING,
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
        "CrashAction",
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
    let crash_kind_ty = ResolvedTy::named_user("CrashKind", Vec::new());

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
                    machine_name: "CrashKind".to_string(),
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
    let monitor_id_ty =
        ResolvedTy::named_builtin("MonitorId", hew_types::BuiltinType::MonitorId, Vec::new());
    let down_target_ty =
        ResolvedTy::named_builtin("DownTarget", hew_types::BuiltinType::DownTarget, Vec::new());
    let down_reason_ty =
        ResolvedTy::named_builtin("DownReason", hew_types::BuiltinType::DownReason, Vec::new());
    let crash_kind_ty =
        ResolvedTy::named_builtin("CrashKind", hew_types::BuiltinType::CrashKind, Vec::new());
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
            name: "MonitorId".to_string(),
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
                            machine_name: "DownTarget".to_string(),
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
                            machine_name: "DownTarget".to_string(),
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
                match_arm(Some(0), unit_variant("CrashKind", 0, crash_kind_ty.clone())),
                match_arm(Some(1), unit_variant("CrashKind", 1, crash_kind_ty.clone())),
                match_arm(None, unit_variant("CrashKind", 2, crash_kind_ty.clone())),
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
                    unit_variant("DownReason", 0, down_reason_ty.clone()),
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
                            machine_name: "DownReason".to_string(),
                            state_idx: 1,
                            payload: Some(vec![("0".to_string(), crash_kind)]),
                        },
                        span: span.clone(),
                    },
                ),
                match_arm(
                    Some(2),
                    unit_variant("DownReason", 2, down_reason_ty.clone()),
                ),
                match_arm(None, unit_variant("DownReason", 3, down_reason_ty.clone())),
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
            name: "DownNotification".to_string(),
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
        ResolvedTy::Named { name, args, .. } if name.as_str() == "LocalPid" && args.len() == 1 => {
            match &args[0] {
                ResolvedTy::Named { name, args, .. } if args.is_empty() => Some(name.as_str()),
                _ => None,
            }
        }
        _ => None,
    }
}
pub(super) fn actor_name_from_remote_pid_ty(ty: &ResolvedTy) -> Option<&str> {
    match ty {
        ResolvedTy::Named { name, args, .. } if name == "RemotePid" && args.len() == 1 => {
            match &args[0] {
                ResolvedTy::Named { name, args, .. } if args.is_empty() => Some(name.as_str()),
                _ => None,
            }
        }
        _ => None,
    }
}
pub(super) fn named_type_marker(
    ty: &ResolvedTy,
    type_classes: &hew_hir::TypeClassTable,
) -> Option<ResourceMarker> {
    hew_hir::lookup_type_marker_for_ty(ty, type_classes)
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
    let ResolvedTy::Named { name, args, .. } = ty else {
        return false;
    };
    // M-5: `CrashInfo` now carries an owned `message: string`, so it is no
    // longer marker-`BitCopy`. The authoritative discriminant is the
    // `CrashInfo` role on the builtin registration, not the marker.
    if !args.is_empty() {
        return false;
    }

    let Some(registration) = hew_hir::builtin_type_classes::builtin_type_registration(name) else {
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
    record_field_orders.get(name).is_some_and(|actual_fields| {
        builtin_registration_fields_match(actual_fields, expected_fields)
    })
}
pub(super) fn register_builtin_record_layouts(
    record_layouts: &mut Vec<crate::model::RecordLayout>,
    record_field_orders: &mut HashMap<String, Vec<(String, ResolvedTy)>>,
) {
    for registration in hew_hir::builtin_type_classes::builtin_type_registrations() {
        let hew_hir::builtin_type_classes::BuiltinTypeShape::Struct(fields) = registration.shape
        else {
            continue;
        };
        if let Some(existing_fields) = record_field_orders.get(registration.name) {
            debug_assert!(
                builtin_registration_fields_match(existing_fields, fields),
                "builtin record registration for `{}` disagrees with existing record layout",
                registration.name
            );
            continue;
        }

        let fields: Vec<(String, ResolvedTy)> = fields
            .iter()
            .map(|field| (field.name.to_string(), field.ty.to_resolved_ty()))
            .collect();
        record_layouts.push(crate::model::RecordLayout {
            name: registration.name.to_string(),
            field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
            field_names: fields.iter().map(|(name, _)| name.clone()).collect(),
        });
        record_field_orders.insert(registration.name.to_string(), fields);
    }
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
/// Returns the set of registered names so the caller can fold them into
/// `machine_layout_names`. Skips any name that already has a layout (e.g.
/// because a user enum coincidentally shadows the builtin name); the
/// user-source layout wins, mirroring the precedent in
/// `register_builtin_record_layouts`.
pub(super) fn register_builtin_monomorphic_enum_layouts(
    enum_layouts: &mut Vec<crate::model::EnumLayout>,
) -> Vec<String> {
    let existing: HashSet<String> = enum_layouts.iter().map(|el| el.name.clone()).collect();
    let mut registered = Vec::new();
    for spec in hew_types::builtin_enums::monomorphic_builtin_enums() {
        if existing.contains(spec.name) {
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
            name: spec.name.to_string(),
            tag_width,
            variants,
            is_indirect: false,
        });
        registered.push(spec.name.to_string());
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
                "unresolved Ty::AssocType `{}::{assoc_name}` on base `{}`",
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
    let mut result = dataflow::analyze(blocks, &builder.type_classes, &param_ids);
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
pub(super) fn is_string_const_ty(ty: &ResolvedTy) -> bool {
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
    let ResolvedTy::Named { name, args, .. } = ty else {
        return false;
    };
    if name != "Result" {
        return false;
    }
    matches!(
        args.as_slice(),
        [ResolvedTy::Unit, ResolvedTy::Named { name: err, .. }] if err == "SendError"
    )
}
/// Recognise the checker-recorded `Result<(), CloseError>` shape that
/// `SendHalf`/`RecvHalf`/`Duplex` `.close()` produces. The half-close
/// materialises this from the runtime's i32 status, so the MIR producer
/// must consume the recorded type (`checker-authority`) rather than infer it.
pub(super) fn is_unit_close_error_result(ty: &ResolvedTy) -> bool {
    let ResolvedTy::Named { name, args, .. } = ty else {
        return false;
    };
    if name != "Result" {
        return false;
    }
    matches!(
        args.as_slice(),
        [ResolvedTy::Unit, ResolvedTy::Named { name: err, .. }] if err == "CloseError"
    )
}
/// Extract the `Result<R, RecvError>` payload type the recv producers
/// materialise. Returns `None` for any other shape so the producer fails
/// closed on a checker contract drift rather than mis-sizing the payload
/// slot (`checker-authority`, `boundary-fail-closed`).
pub(super) fn recv_result_payload_ty(ty: &ResolvedTy) -> Option<&ResolvedTy> {
    let ResolvedTy::Named { name, args, .. } = ty else {
        return None;
    };
    if name != "Result" {
        return None;
    }
    match args.as_slice() {
        [payload, ResolvedTy::Named { name: err, .. }] if err == "RecvError" => Some(payload),
        _ => None,
    }
}
pub(super) fn runtime_symbol_for_call_expr(
    expr: &HirExpr,
) -> Option<(String, &[hew_hir::HirExpr], hew_hir::SiteId)> {
    let HirExprKind::Call { callee, args } = &expr.kind else {
        return None;
    };
    let HirExprKind::BindingRef { name, resolved } = &callee.kind else {
        return None;
    };
    // Typed path: HIR resolved a checker-known runtime builtin (the
    // no-AST-item builtins and every closed-set method-call rewrite) to
    // its catalog family. The C symbol is derived from the catalog
    // bijection — no user-name reverse-mapping. A real user function
    // that shadows a builtin name resolves to `ResolvedRef::Item` in
    // HIR and never reaches this arm.
    //
    // The allowlist gate preserves the producer routing split: families
    // whose symbol is in `known_runtime_symbols` lower to
    // `Instr::CallRuntimeAbi`; pre-staged families (codegen
    // `Terminator::Call` callee-name intercepts such as
    // `hew_remote_pid_send`) fall through to `module_fn_names` →
    // `lower_direct_call`, exactly as their name-resolved form did.
    if let ResolvedRef::Builtin(family) = resolved {
        let symbol = family.c_symbol();
        if crate::runtime_symbols::is_known_runtime_symbol(symbol) {
            return Some((symbol.to_string(), args, expr.site));
        }
        return None;
    }
    if crate::runtime_symbols::is_known_runtime_symbol(name) {
        return Some((name.clone(), args, expr.site));
    }
    None
}
