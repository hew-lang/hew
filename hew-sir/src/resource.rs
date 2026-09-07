//! Checked resource release identity; live ownership stays in the ordinary SSA/place analysis.

use hew_types::ffi_contracts::{
    extern_owned_resource_result, extern_ownership_contract, ExternParamOwnership,
};
use hew_types::runtime_call::FileReadOp;
use hew_types::{
    CloneKind, ResolvedTy, RuntimeCallFamily, RuntimeResultEffect, TypeFacts, ValueClass,
};

/// One exact release authority transported from the checked source boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResourceRelease {
    /// Affine reference to scope-owned task execution and its eventual result.
    Task,
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

/// An exact HIR extern declaration retained as part of the release proof.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResourceExtern {
    pub declaration: hew_types::DefId,
    pub symbol: String,
    pub params: Vec<ResolvedTy>,
    pub consumes: Vec<bool>,
    pub result: ResolvedTy,
}

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
    /// Refuses a release without an executable runtime contract.
    pub fn carrier(&self) -> Result<ResourceCarrier, String> {
        if matches!(self, Self::RecordClose { .. }) {
            return Ok(ResourceCarrier::Record);
        }
        Ok(
            if matches!(self.runtime_family()?, RuntimeCallFamily::Tcp(_)) {
                ResourceCarrier::I32
            } else {
                ResourceCarrier::Pointer
            },
        )
    }

    /// Return the release ABI result, discarded by ordinary destruction.
    ///
    /// # Errors
    /// Refuses a release without an executable runtime contract.
    pub fn release_result(&self) -> Result<ResolvedTy, String> {
        if matches!(self, Self::RecordClose { .. }) {
            return Ok(ResolvedTy::Unit);
        }
        Ok(
            if matches!(self.runtime_family()?, RuntimeCallFamily::Tcp(_)) {
                ResolvedTy::I32
            } else {
                ResolvedTy::Unit
            },
        )
    }
    /// Select only an executable release from the retained semantic authority.
    ///
    /// # Errors
    /// Refuses releases outside the admitted synchronous resource contracts.
    pub fn runtime_family(&self) -> Result<RuntimeCallFamily, String> {
        match self {
            Self::Task => Ok(RuntimeCallFamily::TaskFree),
            Self::Generator => Ok(RuntimeCallFamily::GeneratorFree),
            Self::Nominal { lifecycle, .. } => {
                RuntimeCallFamily::from_c_symbol(&lifecycle.release_symbol)
                    .filter(|family| {
                        *family == RuntimeCallFamily::FileRead(FileReadOp::Close)
                            || matches!(family, RuntimeCallFamily::Tcp(op) if op.is_release())
                    })
                    .ok_or_else(|| {
                        "nominal resource release has no synchronous runtime contract".into()
                    })
            }
            Self::Stream => Ok(RuntimeCallFamily::StreamClose),
            Self::Sink => Ok(RuntimeCallFamily::SinkClose),
            Self::RecordClose { .. } => Err(
                "a record resource is released by its own close body, not a runtime call".into(),
            ),
        }
    }
}

/// Preserve a HIR-admitted nominal lifecycle or the selected builtin descriptor.
pub(crate) fn resource_release_from_hir(
    module: &hew_hir::HirModule,
    ty: &ResolvedTy,
) -> Option<ResourceRelease> {
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
    if matches!(release, ResourceRelease::Stream | ResourceRelease::Sink) {
        let builtin = if *release == ResourceRelease::Stream {
            hew_types::BuiltinType::Stream
        } else {
            hew_types::BuiltinType::Sink
        };
        return if matches!(ty, ResolvedTy::Named { builtin: Some(kind), args, .. } if *kind == builtin && args.len() == 1)
        {
            Ok(())
        } else {
            Err("pipe half release requires its exact Stream or Sink type".into())
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
    let family = release.runtime_family()?;
    let contract = family
        .semantic_contract()
        .ok_or("resource release lacks semantic argument effects")?;
    let release_result = release.release_result()?;
    if !matches!(
        contract.result,
        RuntimeResultEffect::Unit | RuntimeResultEffect::BitCopy(hew_types::RuntimeValueKind::I32)
    ) || !contract.matches_signature(std::slice::from_ref(ty), &release_result)
        || contract.arguments.len() != 1
        || contract.arguments[0].effect != hew_types::RuntimeArgumentEffect::Move
        || !contract.failures.is_empty()
    {
        return Err(
            "resource release must synchronously consume one exact owner and return its scalar status".into(),
        );
    }
    match release {
        ResourceRelease::Generator
        | ResourceRelease::Task
        | ResourceRelease::Stream
        | ResourceRelease::Sink
        | ResourceRelease::RecordClose { .. } => {
            unreachable!("handled exact builtin and record releases above")
        }
        ResourceRelease::Nominal {
            lifecycle,
            release,
            producers,
        } => verify_nominal_release(ty, lifecycle, release, producers, family),
    }
}

fn verify_nominal_release(
    ty: &ResolvedTy,
    lifecycle: &hew_hir::OpaqueResourceLifecycle,
    release: &ResourceExtern,
    producers: &[ResourceExtern],
    family: RuntimeCallFamily,
) -> Result<(), String> {
    if release.declaration != lifecycle.release_declaration
        || release.symbol != lifecycle.release_symbol
        || release.params != [ty.clone()]
        || release.consumes != [true]
        || !family
            .semantic_contract()
            .is_some_and(|contract| contract.matches_signature(&release.params, &release.result))
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
                || !RuntimeCallFamily::from_c_symbol(&producer.symbol)
                    .and_then(RuntimeCallFamily::semantic_contract)
                    .is_some_and(|contract| {
                        contract.matches_signature(&producer.params, &producer.result)
                            && producer.consumes.len() == contract.arguments.len()
                            && producer.consumes.iter().zip(contract.arguments).all(
                                |(consume, arg)| {
                                    *consume
                                        == (arg.effect == hew_types::RuntimeArgumentEffect::Move)
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
    let release_row = extern_ownership_contract(family.c_symbol())
        .contract()
        .ok_or("nominal resource release has no generated ownership row")?;
    if release_row.params != [ExternParamOwnership::Consume]
        || release_row.resource_param_types != [name.as_str()]
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
