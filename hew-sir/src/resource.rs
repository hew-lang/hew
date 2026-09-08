//! Checked resource release identity; live ownership stays in the ordinary SSA/place analysis.

use hew_types::ffi_contracts::{
    extern_owned_resource_result, extern_ownership_contract, ExternParamOwnership,
};
use hew_types::{CloneKind, ResolvedTy, RuntimeCallFamily, TypeFacts, ValueClass};

/// One exact release authority transported from the checked source boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResourceRelease {
    /// Affine reference to scope-owned task execution and its eventual result.
    Task,
    /// An ephemeral actor invocation owns its request and eventual reply.
    ActorCall,
    /// Generator storage released only after checked cooperative close.
    Generator,
    /// HIR has validated the consuming close body's forwarding to this release.
    Nominal {
        lifecycle: Box<hew_hir::OpaqueResourceLifecycle>,
        release: ResourceExtern,
        producers: Vec<ResourceExtern>,
    },
    /// `Stream<T>`: closing the read half discards unread elements and wakes
    /// a parked producer.
    Stream,
    /// `Sink<T>`: closing the write half is the consumer's end of stream.
    Sink,
    /// `Sender<T>`: closing the last write half is the receiver's end of channel.
    Sender,
    /// `Receiver<T>`: closing the read half discards queued elements and wakes
    /// a parked producer.
    Receiver,
    /// A `#[resource]` record whose release is the user's consuming `close`.
    ///
    /// The lifecycle is the checker/HIR fact; `close` is the semantic callable
    /// that realizes it, resolved once when the module is published so no
    /// later stage joins a release to a body by symbol name.
    RecordClose {
        lifecycle: Box<hew_hir::ResourceRecordLifecycle>,
        close: crate::CallableId,
    },
}

/// One exact `extern` declaration: the C endpoint plus the ownership the
/// source declaration pins on each parameter and on the result.
///
/// This is the single argument-mode authority for an extern call. Downstream
/// stages read it; none of them re-derive a mode from the symbol spelling.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternSignature {
    pub declaration: hew_types::DefId,
    pub symbol: String,
    pub params: Vec<ResolvedTy>,
    pub consumes: Vec<bool>,
    pub result: ResolvedTy,
}

/// An exact HIR extern declaration retained as part of the release proof.
pub type ResourceExtern = ExternSignature;

/// Scalar ABI carrier selected by the checked release protocol.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResourceCarrier {
    Pointer,
    I32,
    /// A field-bearing record released by calling its own `close`.
    Record,
}

impl ResourceRelease {
    /// Return the scalar carrier of the admitted release protocol.
    ///
    /// # Errors
    /// Refuses a nominal release without one exact owner parameter.
    pub fn carrier(&self) -> Result<ResourceCarrier, String> {
        match self {
            Self::RecordClose { .. } => Ok(ResourceCarrier::Record),
            Self::Nominal { release, .. } => {
                let [owner] = release.params.as_slice() else {
                    return Err("nominal release must take one exact owner".into());
                };
                // Opaque extern values use the pointer ABI. Native I/O's
                // existing table-token contract is the exact i32 exception.
                Ok(
                    if hew_types::runtime_call::IoHandleKind::of_ty(owner).is_some() {
                        ResourceCarrier::I32
                    } else {
                        ResourceCarrier::Pointer
                    },
                )
            }
            _ => Ok(ResourceCarrier::Pointer),
        }
    }

    /// Return the release ABI result, discarded by ordinary destruction.
    #[must_use]
    pub fn release_result(&self) -> ResolvedTy {
        match self {
            Self::Nominal { release, .. } => release.result.clone(),
            _ => ResolvedTy::Unit,
        }
    }

    /// The exact C endpoint selected by the checked release authority.
    ///
    /// # Errors
    /// Record close executes a semantic callable rather than a C endpoint.
    pub fn release_symbol(&self) -> Result<&str, String> {
        let family = match self {
            Self::Nominal { release, .. } => return Ok(&release.symbol),
            Self::Task => RuntimeCallFamily::TaskFree,
            Self::ActorCall => RuntimeCallFamily::ActorCallFree,
            Self::Generator => RuntimeCallFamily::GeneratorFree,
            Self::Stream => RuntimeCallFamily::StreamClose,
            Self::Sink => RuntimeCallFamily::SinkClose,
            Self::Sender => RuntimeCallFamily::ChannelSenderClose,
            Self::Receiver => RuntimeCallFamily::ChannelReceiverClose,
            Self::RecordClose { .. } => {
                return Err(
                    "a record resource is released by its own close body, not an extern call"
                        .into(),
                )
            }
        };
        Ok(family.c_symbol())
    }
}

/// Preserve a HIR-admitted nominal lifecycle or the selected builtin descriptor.
pub(crate) fn resource_release_from_hir(
    module: &hew_hir::HirModule,
    ty: &ResolvedTy,
) -> Option<ResourceRelease> {
    if ty.is_builtin(hew_types::BuiltinType::ActorCall) {
        return Some(ResourceRelease::ActorCall);
    }
    if ty.is_builtin(hew_types::BuiltinType::Generator) {
        return Some(ResourceRelease::Generator);
    }
    if matches!(ty, ResolvedTy::Task(_)) {
        return Some(ResourceRelease::Task);
    }
    if ty.is_builtin(hew_types::BuiltinType::Stream) {
        Some(ResourceRelease::Stream)
    } else if ty.is_builtin(hew_types::BuiltinType::Sink) {
        Some(ResourceRelease::Sink)
    } else if ty.is_builtin(hew_types::BuiltinType::Sender) {
        Some(ResourceRelease::Sender)
    } else if ty.is_builtin(hew_types::BuiltinType::Receiver) {
        Some(ResourceRelease::Receiver)
    } else {
        if record_resource_lifecycle(module, ty).is_some() {
            // A record resource's release is a semantic callable, so its
            // authority is published where the callable table is resolved.
            return None;
        }
        let lifecycle = module
            .type_classes
            .lifecycle_registry()
            .opaque_resource_for_ty(ty)?
            .clone();
        let find = |declaration: &hew_types::DefId| {
            module.items.iter().find_map(|item| {
                let hew_hir::HirItem::ExternFn(function) = item else {
                    return None;
                };
                (&function.declaration == declaration).then(|| ResourceExtern {
                    declaration: function.declaration.clone(),
                    symbol: function.name.clone(),
                    params: function.param_tys.clone(),
                    consumes: function.param_consume.clone(),
                    result: function.return_ty.clone(),
                })
            })
        };
        let release = find(&lifecycle.release_declaration)?;
        let producers = lifecycle
            .producer_declarations
            .iter()
            .map(find)
            .collect::<Option<Vec<_>>>()?;
        Some(ResourceRelease::Nominal {
            lifecycle: Box::new(lifecycle),
            release,
            producers,
        })
    }
}

/// The checked `#[resource]` record lifecycle for one exact nominal type.
///
/// Keyed by the resource's qualified declaration path, which is the same
/// identity `ResolvedTy::Named` carries; no short or leaf name is retried.
pub(crate) fn record_resource_lifecycle<'a>(
    module: &'a hew_hir::HirModule,
    ty: &ResolvedTy,
) -> Option<&'a hew_hir::ResourceRecordLifecycle> {
    let ResolvedTy::Named {
        name,
        args,
        builtin: None,
        is_opaque: false,
    } = ty
    else {
        return None;
    };
    if !args.is_empty() {
        return None;
    }
    module
        .type_classes
        .lifecycle_registry()
        .resource_records()
        .find(|lifecycle| lifecycle.resource_declaration.full_path() == name)
}

/// Check the release contract independently before any target layout is selected.
///
/// # Errors
/// Refuses inconsistent declaration, ownership, signature or discharge facts.
pub fn verify_resource_release(
    ty: &ResolvedTy,
    release: &ResourceRelease,
    facts: &TypeFacts,
) -> Result<(), String> {
    if facts.class != ValueClass::AffineResource || facts.clone != CloneKind::None {
        return Err("resource release requires affine ownership without a copy recipe".into());
    }
    if *release == ResourceRelease::ActorCall {
        return if matches!(ty, ResolvedTy::Named { builtin: Some(hew_types::BuiltinType::ActorCall), args, .. }
            if matches!(args.as_slice(), [result] if result.is_builtin(hew_types::BuiltinType::Result)))
        {
            Ok(())
        } else {
            Err("actor completion release requires its exact Result protocol".into())
        };
    }
    if *release == ResourceRelease::Generator {
        return if ty.is_builtin(hew_types::BuiltinType::Generator) {
            Ok(())
        } else {
            Err("generator release requires an exact Generator type".into())
        };
    }
    if *release == ResourceRelease::Task {
        return if matches!(ty, ResolvedTy::Task(_)) {
            Ok(())
        } else {
            Err("task release requires an exact Task result type".into())
        };
    }
    // Both pipe and channel endpoints carry exactly one checked element type.
    if let Some((builtin, elements)) = match release {
        ResourceRelease::Stream => Some((hew_types::BuiltinType::Stream, 1..=1)),
        ResourceRelease::Sink => Some((hew_types::BuiltinType::Sink, 1..=1)),
        ResourceRelease::Sender => Some((hew_types::BuiltinType::Sender, 1..=1)),
        ResourceRelease::Receiver => Some((hew_types::BuiltinType::Receiver, 1..=1)),
        _ => None,
    } {
        return if matches!(ty, ResolvedTy::Named { builtin: Some(kind), args, .. } if *kind == builtin && elements.contains(&args.len()))
        {
            Ok(())
        } else {
            Err("a pipe or channel half release requires its exact handle type".into())
        };
    }
    if let ResourceRelease::RecordClose { lifecycle, .. } = release {
        let ResolvedTy::Named {
            name,
            args,
            builtin: None,
            is_opaque: false,
        } = ty
        else {
            return Err("a record release requires an exact non-opaque nominal type".into());
        };
        return if args.is_empty() && lifecycle.resource_declaration.full_path() == name {
            Ok(())
        } else {
            Err("record release declaration does not match its nominal owner".into())
        };
    }
    match release {
        ResourceRelease::Generator
        | ResourceRelease::ActorCall
        | ResourceRelease::Task
        | ResourceRelease::Stream
        | ResourceRelease::Sink
        | ResourceRelease::Sender
        | ResourceRelease::Receiver
        | ResourceRelease::RecordClose { .. } => {
            unreachable!("handled exact builtin and record releases above")
        }
        ResourceRelease::Nominal {
            lifecycle,
            release,
            producers,
        } => verify_nominal_release(ty, lifecycle, release, producers),
    }
}

fn verify_nominal_release(
    ty: &ResolvedTy,
    lifecycle: &hew_hir::OpaqueResourceLifecycle,
    release: &ResourceExtern,
    producers: &[ResourceExtern],
) -> Result<(), String> {
    if release.declaration != lifecycle.release_declaration
        || release.symbol != lifecycle.release_symbol
        || release.params != [ty.clone()]
        || release.consumes != [true]
        || !(release.result.is_integer()
            || matches!(
                release.result,
                ResolvedTy::Unit
                    | ResolvedTy::Bool
                    | ResolvedTy::F32
                    | ResolvedTy::F64
                    | ResolvedTy::Char
                    | ResolvedTy::Duration
            ))
    {
        return Err("release declaration or signature disagrees with checked lifecycle".into());
    }
    let declarations = producers
        .iter()
        .map(|producer| producer.declaration.clone())
        .collect::<std::collections::BTreeSet<_>>();
    let symbols = producers
        .iter()
        .map(|producer| producer.symbol.clone())
        .collect::<std::collections::BTreeSet<_>>();
    if declarations != lifecycle.producer_declarations
        || symbols != lifecycle.producer_symbols
        || producers.iter().any(|producer| {
            producer.result != *ty
                || producer.params.len() != producer.consumes.len()
                || !extern_ownership_contract(&producer.symbol)
                    .contract()
                    .is_some_and(|contract| {
                        contract.params.len() == producer.params.len()
                            && producer.consumes.iter().zip(contract.params).all(
                                |(consume, ownership)| {
                                    *consume == (*ownership == ExternParamOwnership::Consume)
                                },
                            )
                    })
        })
    {
        return Err("producer declarations or signatures disagree with checked lifecycle".into());
    }
    let ResolvedTy::Named {
        name,
        args,
        builtin: None,
        is_opaque: true,
    } = ty
    else {
        return Err("nominal release requires an exact opaque source type".into());
    };
    if !args.is_empty() || lifecycle.resource_declaration.full_path() != name {
        return Err("resource release declaration does not match its nominal owner".into());
    }
    if lifecycle.producer_declarations.is_empty() || lifecycle.producer_symbols.is_empty() {
        return Err("nominal resource release has no checked producer authority".into());
    }
    let release_row = extern_ownership_contract(&release.symbol)
        .contract()
        .ok_or("nominal resource release has no generated ownership row")?;
    if release_row.params != [ExternParamOwnership::Consume]
        || release_row.resource_param_types != [name.as_str()]
        || release_row.result != hew_types::ffi_contracts::ExternResultOwnership::None
    {
        return Err("nominal release row does not consume its exact declared resource".into());
    }
    for symbol in &lifecycle.producer_symbols {
        let producer = extern_owned_resource_result(symbol)
            .ok_or("nominal producer lacks an audited independent resource result")?;
        if producer.resource_type != name
            || producer.release_symbol != lifecycle.release_symbol
            || producer.discharge_depth != lifecycle.discharge_depth
        {
            return Err("nominal producer and release disagree on exact type or discharge".into());
        }
    }
    Ok(())
}
