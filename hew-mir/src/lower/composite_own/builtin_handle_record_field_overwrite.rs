use super::{
    base_local, render_owned_handle_ty, user_record_layout_key, AggregateOwner, BasicBlock,
    BindingId, FieldOffset, HashMap, HashSet, Instr, MirCheck, MirStatement, Place, ResolvedTy,
};

/// Fail closed on an ordinary record-field overwrite whose destination is a
/// builtin close-bearing handle.
///
/// `RecordFieldStore` currently byte-copies its source into the destination
/// slot. Codegen can close the abandoned old field before that store, but MIR
/// carries no source-slot neutralisation for the replacement. The source and
/// destination would therefore both own one un-clonable runtime handle and
/// their eventual drops would close it twice. Until the store carries a proven
/// move/null protocol, refusing the operation is the only sound behaviour.
///
/// Resolve the destination field through the registered record layout rather
/// than trusting the source local's spelling. This is the same ownership
/// authority codegen uses for overwrite cleanup and keeps a malformed typed
/// store from escaping through a source/destination disagreement.
#[must_use]
pub(in crate::lower) fn detect_builtin_handle_record_field_overwrite(
    blocks: &[BasicBlock],
    local_tys: &[ResolvedTy],
    binding_locals: &HashMap<BindingId, Place>,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
) -> Vec<MirCheck> {
    let is_close_bearing_builtin = |ty: &ResolvedTy| {
        matches!(ty, ResolvedTy::CancellationToken)
            || matches!(
                ty,
                ResolvedTy::Named {
                    builtin: Some(builtin),
                    ..
                } if builtin.close_method().is_some()
                    || matches!(
                        builtin,
                        hew_types::BuiltinType::Generator
                            | hew_types::BuiltinType::AsyncGenerator
                    )
            )
    };
    let destination_field_ty = |record: Place, field_offset: FieldOffset| {
        let local = base_local(record)?;
        let record_ty = local_tys.get(local as usize)?;
        let key = user_record_layout_key(record_ty)?;
        let fields = record_field_orders.get(&key)?;
        fields
            .get(field_offset.0 as usize)
            .map(|(_, field_ty)| field_ty)
    };

    let mut local_to_binding: HashMap<u32, BindingId> = HashMap::new();
    for (binding, place) in binding_locals {
        if let Some(local) = base_local(*place) {
            local_to_binding.entry(local).or_insert(*binding);
        }
    }
    let mut bind_names: HashMap<BindingId, String> = HashMap::new();
    for block in blocks {
        for stmt in &block.statements {
            if let MirStatement::Bind { binding, name, .. } = stmt {
                bind_names.entry(*binding).or_insert_with(|| name.clone());
            }
        }
    }

    let mut findings = Vec::new();
    let mut seen: HashSet<(u32, u32)> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            let Instr::RecordFieldStore {
                record,
                field_offset,
                ..
            } = instr
            else {
                continue;
            };
            let Some(record_local) = base_local(*record) else {
                continue;
            };
            let Some(field_ty) = destination_field_ty(*record, *field_offset) else {
                continue;
            };
            if !is_close_bearing_builtin(field_ty) || !seen.insert((record_local, field_offset.0)) {
                continue;
            }
            let binding = local_to_binding
                .get(&record_local)
                .copied()
                .unwrap_or(BindingId(record_local));
            let name = bind_names
                .get(&binding)
                .cloned()
                .unwrap_or_else(|| format!("local{record_local}"));
            findings.push(MirCheck::OwnedHandleAggregateDoubleFree {
                binding,
                name,
                handle_ty: render_owned_handle_ty(field_ty),
                overwrite: true,
                owner: AggregateOwner::Record,
            });
        }
    }
    findings
}

#[cfg(test)]
mod tests {
    use super::super::Terminator;
    use super::{
        detect_builtin_handle_record_field_overwrite, render_owned_handle_ty, AggregateOwner,
        BasicBlock, BindingId, FieldOffset, HashMap, Instr, MirCheck, MirStatement, Place,
        ResolvedTy,
    };

    fn builtin_ty(builtin: hew_types::BuiltinType) -> ResolvedTy {
        let args = match builtin {
            hew_types::BuiltinType::Generator => vec![ResolvedTy::I64, ResolvedTy::Unit],
            hew_types::BuiltinType::AsyncGenerator
            | hew_types::BuiltinType::Stream
            | hew_types::BuiltinType::Sink
            | hew_types::BuiltinType::Sender
            | hew_types::BuiltinType::Receiver => vec![ResolvedTy::I64],
            _ => vec![],
        };
        ResolvedTy::named_builtin(builtin.canonical_name(), builtin, args)
    }

    fn findings(field_ty: ResolvedTy, src_ty: ResolvedTy) -> Vec<MirCheck> {
        let holder = BindingId(7);
        let blocks = vec![BasicBlock {
            id: 0,
            statements: vec![MirStatement::Bind {
                binding: holder,
                name: "holder".to_string(),
                site: hew_hir::SiteId(1),
                ty: ResolvedTy::named_user("Holder", vec![]),
            }],
            instructions: vec![Instr::RecordFieldStore {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                src: Place::Local(1),
            }],
            terminator: Terminator::Return,
        }];
        let local_tys = vec![ResolvedTy::named_user("Holder", vec![]), src_ty];
        let binding_locals = HashMap::from([(holder, Place::Local(0))]);
        let record_fields =
            HashMap::from([("Holder".to_string(), vec![("value".to_string(), field_ty)])]);
        detect_builtin_handle_record_field_overwrite(
            &blocks,
            &local_tys,
            &binding_locals,
            &record_fields,
        )
    }

    #[test]
    fn every_close_bearing_builtin_record_overwrite_is_refused() {
        let mut tys: Vec<_> = hew_types::builtin_types()
            .iter()
            .filter(|info| {
                info.close_method.is_some()
                    || matches!(
                        info.kind,
                        hew_types::BuiltinType::Generator | hew_types::BuiltinType::AsyncGenerator
                    )
            })
            .map(|info| builtin_ty(info.kind))
            .collect();
        tys.push(ResolvedTy::CancellationToken);
        for ty in tys {
            let checks = findings(ty.clone(), ty.clone());
            assert!(
                matches!(
                    checks.as_slice(),
                    [MirCheck::OwnedHandleAggregateDoubleFree {
                        name,
                        handle_ty,
                        overwrite: true,
                        owner: AggregateOwner::Record,
                        ..
                    }] if name == "holder" && handle_ty == &render_owned_handle_ty(&ty)
                ),
                "{ty:?} overwrite must be refused against the mutated record; got {checks:?}"
            );
        }
    }

    #[test]
    fn destination_layout_is_the_overwrite_ownership_authority() {
        let stream = builtin_ty(hew_types::BuiltinType::Stream);
        let checks = findings(stream.clone(), ResolvedTy::String);
        assert!(
            matches!(
                checks.as_slice(),
                [MirCheck::OwnedHandleAggregateDoubleFree { handle_ty, .. }]
                    if handle_ty == &render_owned_handle_ty(&stream)
            ),
            "a malformed source type must not hide the destination's live handle; got {checks:?}"
        );
    }

    #[test]
    fn ordinary_values_and_user_shadows_remain_admitted() {
        assert!(
            findings(ResolvedTy::String, ResolvedTy::String).is_empty(),
            "a string field has a sound retain/release overwrite protocol"
        );
        let shadow = ResolvedTy::named_user("Stream", vec![ResolvedTy::I64]);
        assert!(
            findings(shadow.clone(), shadow).is_empty(),
            "a user type named Stream must not acquire builtin-handle semantics"
        );
        let receiver_shadow = ResolvedTy::named_user("Receiver", vec![ResolvedTy::I64]);
        assert!(
            findings(receiver_shadow.clone(), receiver_shadow).is_empty(),
            "a user type named Receiver must not acquire channel-close semantics"
        );
        for name in ["Sender", "LambdaPid", "MonitorRef", "SendHalf", "RecvHalf"] {
            let shadow = ResolvedTy::named_user(name, vec![ResolvedTy::I64]);
            assert!(
                findings(shadow.clone(), shadow).is_empty(),
                "a user type named {name} must not acquire builtin close semantics"
            );
        }
    }

    #[test]
    fn channel_endpoint_diagnostics_recover_source_qualified_family() {
        for (builtin, expected) in [
            (hew_types::BuiltinType::Sender, "channel.Sender<i64>"),
            (hew_types::BuiltinType::Receiver, "channel.Receiver<i64>"),
        ] {
            assert_eq!(
                render_owned_handle_ty(&builtin_ty(builtin)),
                expected,
                "typed std endpoint diagnostics must retain their source family"
            );
        }
    }
}
