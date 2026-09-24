//! Instance admission: type-fact and shape requests, monomorphization and closure/vtable requests.

use super::{
    callable_signature, callable_signature_with_substitution, declared_type_param_name,
    dyn_boundary_passing, dyn_passing_admits, dyn_receiver_passing, function_source_origin,
    is_supported_instance_type_arg, project_type_facts, require_aggregate_shape,
    require_signature_shapes, require_type_shapes, require_variant_shape, AggregateShapeRef,
    BTreeMap, BTreeSet, BodySource, Builder, BytesLiteralId, CallableId, CallableInstance,
    CallableState, CallableTable, Cow, DefId, EntryAdapter, EntryExitAction, FunctionSourceOrigin,
    HashMap, HirExpr, HirExprKind, HirFn, HirModule, InstanceService, LoweringInput, OwnKind,
    ResolvedTy, SemAbiParam, SemCallConv, SemCallable, SemCallableKind, SemFunction,
    SemGenericTemplate, SemModule, SemParamPassing, SemSignature, SirInstanceKey,
    SirLoweringStatus, SirRootSelectionError, StringLiteralId, TypeCheckOutput, TypeFactService,
    TypeInstanceKey, TypeSubstitution, VariantShapeId, VecDeque, SIR_GENERIC_INSTANCE_CAP,
};

impl<'a> InstanceService<'a> {
    pub(super) fn new(module: &'a HirModule, facts: &TypeCheckOutput) -> Self {
        let checked_facts =
            TypeFactService::new(facts.type_fact_context.clone(), facts.type_facts.clone());
        Self {
            module,
            checked_facts,
            table: CallableTable::from_hir(module),
            states: Vec::new(),
            statuses: Vec::new(),
            by_instance: HashMap::new(),
            closures: Vec::new(),
            actors: Vec::new(),
            actor_sources: HashMap::new(),
            supervisors: Vec::new(),
            synthetic_sources: HashMap::new(),
            closures_by_instance: HashMap::new(),
            closure_sources: Vec::new(),
            vtables: Vec::new(),
            vtables_by_erasure: HashMap::new(),
            entry_adapter: None,
            used_templates: std::collections::HashSet::new(),
            scanned_record_closes: 0,
            demanded_opaque_closes: std::collections::HashSet::new(),
            pending: VecDeque::new(),
            functions: Vec::new(),
            aggregate_shapes: Vec::new(),
            aggregate_shapes_by_type: HashMap::new(),
            variant_shapes: Vec::new(),
            variant_shapes_by_type: HashMap::new(),
            string_literals: BTreeMap::new(),
            bytes_literals: BTreeMap::new(),
            value_capabilities: BTreeMap::new(),
            structural_display: BTreeMap::new(),
            wire_plans: HashMap::new(),
        }
    }

    pub(super) fn callable(&self, id: CallableId) -> Option<&SemCallable> {
        self.table.callable(id)
    }

    pub(super) fn require_key_capabilities(&mut self, ty: &ResolvedTy) -> Result<(), String> {
        for capability in [
            hew_types::ValueCapability::Hash,
            hew_types::ValueCapability::Eq,
        ] {
            self.require_value_capability(ty, capability)?;
        }
        Ok(())
    }

    pub(super) fn require_value_capability(
        &mut self,
        ty: &ResolvedTy,
        capability: hew_types::ValueCapability,
    ) -> Result<(), String> {
        let key = (ty.clone(), capability);
        if self.value_capabilities.contains_key(&key) {
            return Ok(());
        }
        self.require_type_facts(ty)?;
        let selected = self
            .checked_facts
            .capability_plan(ty, capability)
            .map_err(|error| {
                format!(
                    "cannot select {capability:?} for `{}`: {error}",
                    ty.user_facing()
                )
            })?
            .ok_or_else(|| {
                format!(
                    "`{}` has no selected {capability:?} implementation",
                    ty.user_facing()
                )
            })?;
        let callable = match selected.plan() {
            hew_types::ValueMethodPlan::Derived => None,
            hew_types::ValueMethodPlan::User { method, type_args } => {
                let callable = if self.table.templates.contains_key(method) {
                    self.request_instance(method, type_args.clone())?
                } else {
                    if !type_args.is_empty() {
                        return Err("selected nongeneric capability has type arguments".to_string());
                    }
                    let id = self.admit_monomorphic(method).map_err(|reason| {
                        format!(
                            "selected capability `{}` has no admitted HIR callable: {reason}",
                            method.full_path()
                        )
                    })?;
                    self.request_body(id);
                    id
                };
                let metadata = self.callable(callable).ok_or_else(|| {
                    "selected capability callable disappeared from its table".to_string()
                })?;
                let facts = self
                    .checked_facts
                    .rows()
                    .get(&TypeInstanceKey(ty.clone()))
                    .ok_or_else(|| "selected capability type facts disappeared".to_string())?;
                crate::capability::verify_capability_signature(ty, capability, metadata, *facts)?;
                Some(callable)
            }
        };
        let derived = matches!(selected.plan(), hew_types::ValueMethodPlan::Derived);
        self.value_capabilities.insert(
            key.clone(),
            crate::SemValueMethodPlan {
                selection: selected,
                callable,
            },
        );
        if derived {
            let result = crate::derived_capability_components(
                ty,
                &self.aggregate_shapes,
                &self.variant_shapes,
            )
            .and_then(|components| {
                for component in components {
                    self.require_value_capability(&component, capability)?;
                }
                Ok(())
            });
            if let Err(reason) = result {
                self.value_capabilities.remove(&key);
                return Err(reason);
            }
        }
        Ok(())
    }

    pub(super) fn require_structural_rendering(
        &mut self,
        key: &crate::StructuralType,
    ) -> Result<(), String> {
        if self.structural_display.contains_key(key) {
            return Ok(());
        }
        let ty = &key.value;
        self.require_type_facts(ty)?;
        // Reserve the type before descending through recursive variants.
        self.structural_display
            .insert(key.clone(), crate::SemStructuralRender::default());
        if let Some((method, type_args)) = self
            .checked_facts
            .display_method_for_type(ty, &key.source)
            .map_err(|error| format!("cannot select Display for `{}`: {error}", ty.user_facing()))?
        {
            let instance = if type_args.is_empty() && !self.table.templates.contains_key(&method) {
                hew_types::EntryCallableInstance::Declared
            } else {
                hew_types::EntryCallableInstance::Generic { type_args }
            };
            let callable = self.resolve_entry_display(&method, &instance)?;
            if callable.signature.return_ty != ResolvedTy::String
                || callable.signature.params.len() != 1
                || callable.signature.params[0].ty != *ty
                || !matches!(
                    callable.signature.params[0].passing,
                    SemParamPassing::Borrow | SemParamPassing::ReadOnly
                )
            {
                return Err(
                    "selected Display has an incompatible borrowed formatter signature".into(),
                );
            }
            self.structural_display.insert(
                key.clone(),
                crate::SemStructuralRender {
                    display: Some(callable.id),
                    members: Vec::new(),
                },
            );
            return Ok(());
        }
        let members = self.structural_rendering_members(key)?;
        for member in &members {
            self.require_structural_rendering(member)?;
        }
        self.structural_display.insert(
            key.clone(),
            crate::SemStructuralRender {
                display: None,
                members,
            },
        );
        Ok(())
    }

    pub(super) fn structural_rendering_members(
        &self,
        key: &crate::StructuralType,
    ) -> Result<Vec<crate::StructuralType>, String> {
        let ty = &key.value;
        let source = self.checked_facts.rendering_source(&key.source)?;
        let source_args = match &source {
            ResolvedTy::Tuple(args) | ResolvedTy::Named { args, .. } => args.as_slice(),
            _ => &[],
        };
        Ok(match ty {
            ResolvedTy::Tuple(members) => members
                .iter()
                .enumerate()
                .map(|(index, value)| crate::StructuralType {
                    value: value.clone(),
                    source: source_args.get(index).unwrap_or(value).clone(),
                })
                .collect(),
            ResolvedTy::Named {
                args,
                builtin: Some(hew_types::BuiltinType::Vec | hew_types::BuiltinType::HashMap),
                ..
            } => args
                .iter()
                .enumerate()
                .map(|(index, value)| crate::StructuralType {
                    value: value.clone(),
                    source: source_args.get(index).unwrap_or(value).clone(),
                })
                .collect(),
            ResolvedTy::Named {
                is_opaque: true, ..
            } => Vec::new(),
            _ => {
                let mut members = Vec::new();
                if let Some(shape) = self
                    .aggregate_shapes
                    .iter()
                    .find(|shape| shape.aggregate_ty == *ty)
                {
                    for field in &shape.fields {
                        let source = self
                            .checked_facts
                            .rendering_field(&source, None, &field.name)?
                            .unwrap_or_else(|| field.ty.clone());
                        members.push(crate::StructuralType {
                            value: field.ty.clone(),
                            source,
                        });
                    }
                } else if let Some(shape) = self
                    .variant_shapes
                    .iter()
                    .find(|shape| shape.enum_ty == *ty)
                {
                    for (index, variant) in shape.variants.iter().enumerate() {
                        for (position, field) in variant.fields.iter().enumerate() {
                            let declared = self.checked_facts.rendering_field(
                                &source,
                                Some(&variant.name),
                                &field.name,
                            )?;
                            let source_builtin = match &source {
                                ResolvedTy::Named { builtin, .. } => *builtin,
                                _ => None,
                            };
                            let builtin_argument = source_builtin
                                .and_then(hew_types::BuiltinType::generic_enum)
                                .and_then(|decl| decl.variants.get(index))
                                .and_then(|variant| variant.payload_type_args.get(position))
                                .and_then(|argument| source_args.get(*argument));
                            let source = declared
                                .or_else(|| builtin_argument.cloned())
                                .unwrap_or_else(|| field.ty.clone());
                            members.push(crate::StructuralType {
                                value: field.ty.clone(),
                                source,
                            });
                        }
                    }
                }
                members
            }
        })
    }

    pub(super) fn require_type_facts(&mut self, ty: &ResolvedTy) -> Result<(), String> {
        require_type_shapes(
            self.module,
            &mut self.checked_facts,
            &mut self.aggregate_shapes,
            &mut self.aggregate_shapes_by_type,
            &mut self.variant_shapes,
            &mut self.variant_shapes_by_type,
            ty,
        )
    }

    /// Intern the exact checker-resolved shape of one concrete aggregate.
    ///
    /// Tuples are structural. Named records resolve through the checker type
    /// service and the declaration identity carried by `NominalInstance`.
    pub(super) fn require_aggregate_shape(
        &mut self,
        aggregate_ty: &ResolvedTy,
    ) -> Result<AggregateShapeRef, String> {
        require_aggregate_shape(
            self.module,
            &mut self.checked_facts,
            &mut self.aggregate_shapes,
            &mut self.aggregate_shapes_by_type,
            aggregate_ty,
        )
    }

    pub(super) fn require_variant_shape(
        &mut self,
        enum_ty: &ResolvedTy,
    ) -> Result<VariantShapeId, String> {
        require_variant_shape(
            self.module,
            &mut self.checked_facts,
            &mut self.variant_shapes,
            &mut self.variant_shapes_by_type,
            enum_ty,
        )
    }

    pub(super) fn require_runtime_variant_result_shapes(
        &mut self,
        kind: hew_types::RuntimeVariantResultKind,
        result_ty: &ResolvedTy,
    ) -> Result<(), String> {
        let (_, error_ty) = kind.payload_types(result_ty).ok_or_else(|| {
            format!(
                "runtime variant result contract does not admit `{}`",
                result_ty.user_facing()
            )
        })?;
        self.require_variant_shape(result_ty)?;
        let AggregateShapeRef::Record(error_shape) = self.require_aggregate_shape(error_ty)? else {
            return Err("runtime variant error must be an exact named record".to_string());
        };
        let error_len_ty = self
            .aggregate_shapes
            .get(usize::try_from(error_shape.0).map_err(|_| {
                format!(
                    "runtime variant error shape {} is out of range",
                    error_shape.0
                )
            })?)
            .and_then(|shape| shape.fields.iter().find(|field| field.name == "error_len"))
            .map(|field| field.ty.clone())
            .ok_or_else(|| "runtime variant error has no error_len field".to_string())?;
        self.require_variant_shape(&error_len_ty)?;
        crate::runtime_variant_shape_refs(
            kind,
            result_ty,
            &self.aggregate_shapes,
            &self.variant_shapes,
        )?;
        Ok(())
    }

    pub(super) fn require_signature_shapes(
        &mut self,
        signature: &SemSignature,
    ) -> Result<(), String> {
        let prior_aggregate_count = self.aggregate_shapes.len();
        let prior_variant_count = self.variant_shapes.len();
        let result = require_signature_shapes(
            self.module,
            &mut self.checked_facts,
            &mut self.aggregate_shapes,
            &mut self.aggregate_shapes_by_type,
            &mut self.variant_shapes,
            &mut self.variant_shapes_by_type,
            signature,
        );
        if result.is_err() {
            self.aggregate_shapes.truncate(prior_aggregate_count);
            self.aggregate_shapes_by_type
                .retain(|_, id| usize::try_from(id.0).is_ok_and(|id| id < prior_aggregate_count));
            self.variant_shapes.truncate(prior_variant_count);
            self.variant_shapes_by_type
                .retain(|_, id| usize::try_from(id.0).is_ok_and(|id| id < prior_variant_count));
        }
        result
    }

    pub(super) fn intern_string(&mut self, value: &str) -> StringLiteralId {
        if let Some((id, _)) = self
            .string_literals
            .iter()
            .find(|(_, existing)| existing.as_str() == value)
        {
            return *id;
        }
        let id = StringLiteralId(
            u32::try_from(self.string_literals.len())
                .expect("SIR string literal count exceeds u32"),
        );
        self.string_literals.insert(id, value.to_string());
        id
    }

    pub(super) fn intern_bytes(&mut self, value: &[u8]) -> BytesLiteralId {
        if let Some((id, _)) = self
            .bytes_literals
            .iter()
            .find(|(_, existing)| existing.as_slice() == value)
        {
            return *id;
        }
        let id = BytesLiteralId(
            u32::try_from(self.bytes_literals.len()).expect("SIR bytes literal count exceeds u32"),
        );
        self.bytes_literals.insert(id, value.to_vec());
        id
    }

    /// Seed the worklist with the module's resolved entry callable.
    ///
    /// A module without one is not an executable program, so it has no demand
    /// and lowers nothing.
    pub(super) fn request_entry(&mut self) {
        let Some(declaration) = self
            .table
            .entry_exit_plan
            .as_ref()
            .map(|plan| plan.entry.clone())
        else {
            return;
        };
        let Ok(entry) = self.admit_monomorphic(&declaration) else {
            return;
        };
        if let Err(reason) = self.request_actor_codec_roots() {
            self.record_callable_result(entry, Err(reason));
            return;
        }
        let result_plan = self
            .table
            .entry_exit_plan
            .as_ref()
            .is_some_and(|plan| matches!(plan.action, EntryExitAction::Result { .. }));
        if !result_plan {
            self.request_body(entry);
            return;
        }
        // A Result entry exits through a synthesized adapter. SIR consumes the
        // checker's action here; the module publishes the integer status the
        // adapter returns as the physical-facing exit action.
        let source = self
            .table
            .callable(entry)
            .cloned()
            .expect("entry callable exists in its table");
        let id = CallableId(
            u32::try_from(self.table.callables.len())
                .expect("SIR callable count exceeds the module-local ID range"),
        );
        self.table.callables.push(SemCallable {
            id,
            function: source.function,
            declaration: source.declaration,
            instance: CallableInstance::EntryAdapter,
            symbol: "__hew_entry".to_string(),
            source_origin: source.source_origin,
            signature: SemSignature {
                params: Vec::new(),
                return_ty: ResolvedTy::I64,
            },
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        });
        self.states.push(CallableState::Unreached);
        self.statuses.push(None);
        self.table.root_unit_callables.push(id);
        self.table.entry_callable = Some(id);
        let plan = self
            .table
            .entry_exit_plan
            .as_mut()
            .expect("a Result entry plan was just observed");
        let action = std::mem::replace(
            &mut plan.action,
            EntryExitAction::Integer(hew_types::EntryIntegerType::I64),
        );
        self.entry_adapter = Some(EntryAdapter {
            callable: id,
            entry,
            action,
        });
        self.request_body(id);
    }

    /// Intern the dispatch table for one `(dyn Trait, concrete type)` erasure.
    ///
    /// Each slot resolves the checker's implementer declaration to a demanded
    /// SIR callable, so no later stage joins a slot to a body by name. The
    /// slot order is the checker's, past the runtime's three-word prefix.
    pub(super) fn request_vtable(
        &mut self,
        dyn_ty: &ResolvedTy,
        concrete_ty: &ResolvedTy,
        entries: &[hew_types::DynVtableEntry],
    ) -> Result<crate::SemVtableId, String> {
        let key = (dyn_ty.clone(), concrete_ty.clone());
        if let Some(id) = self.vtables_by_erasure.get(&key) {
            return Ok(*id);
        }
        self.require_type_facts(dyn_ty)?;
        self.require_type_facts(concrete_ty)?;
        let mut slots = Vec::with_capacity(entries.len());
        for (index, entry) in entries.iter().enumerate() {
            let slot = 3 + u32::try_from(index)
                .map_err(|_| "trait-object method count exceeds u32".to_string())?;
            let declaration = entry.impl_method.as_ref().ok_or_else(|| {
                format!(
                    "`{}` fills slot {slot} of `{}` with `{}`, which has no source declaration",
                    concrete_ty.user_facing(),
                    dyn_ty.user_facing(),
                    entry.impl_fn_key
                )
            })?;
            let callee = self.admit_monomorphic(declaration).map_err(|reason| {
                format!(
                    "slot {slot} of `{}` names `{}`, which has no monomorphic SIR callable: {reason}",
                    dyn_ty.user_facing(),
                    declaration.full_path()
                )
            })?;
            // Erasure is what obliges the module to carry every slot body:
            // the dispatch edge cannot demand one, because it names an index
            // rather than a declaration.
            self.request_body(callee);
            let target = self
                .callable(callee)
                .cloned()
                .ok_or_else(|| format!("SIR callable {callee:?} is absent from its table"))?;
            let Some((receiver, arguments)) = target.signature.params.split_first() else {
                return Err(format!(
                    "slot {slot} implementation `{}` takes no receiver",
                    target.symbol
                ));
            };
            let receiver_passing = dyn_receiver_passing(&entry.signature);
            if receiver.ty != *concrete_ty
                || !dyn_passing_admits(receiver_passing, receiver.passing)
            {
                return Err(format!(
                    "slot {slot} implementation `{}` does not receive `{}` on the erased boundary",
                    target.symbol,
                    concrete_ty.user_facing()
                ));
            }
            let mut params = Vec::with_capacity(arguments.len());
            for argument in arguments {
                let passing =
                    dyn_boundary_passing(OwnKind::of_ty(&argument.ty, self.checked_facts.rows())?);
                if !dyn_passing_admits(passing, argument.passing) {
                    return Err(format!(
                        "slot {slot} implementation `{}` changes the erased transfer of `{}`",
                        target.symbol,
                        argument.ty.user_facing()
                    ));
                }
                params.push(SemAbiParam {
                    ty: argument.ty.clone(),
                    passing,
                    caller_visible_projection: false,
                });
            }
            slots.push(crate::SemVtableSlot {
                slot,
                trait_name: entry.trait_name.clone(),
                method_name: entry.method_name.clone(),
                method: entry.method.clone(),
                callee,
                receiver: receiver_passing,
                signature: SemSignature {
                    params,
                    return_ty: target.signature.return_ty.clone(),
                },
            });
        }
        let id = crate::SemVtableId(
            u32::try_from(self.vtables.len())
                .map_err(|_| "SIR vtable count exceeds u32".to_string())?,
        );
        self.vtables.push(crate::SemVtable {
            id,
            dyn_ty: dyn_ty.clone(),
            concrete_ty: concrete_ty.clone(),
            slots,
        });
        self.vtables_by_erasure.insert(key, id);
        Ok(id)
    }

    /// The checker-selected `Display::fmt` body for the entry error type.
    pub(super) fn resolve_entry_display(
        &mut self,
        declaration: &DefId,
        instance: &hew_types::EntryCallableInstance,
    ) -> Result<SemCallable, String> {
        let id = match instance {
            hew_types::EntryCallableInstance::Declared => {
                let id = self.admit_monomorphic(declaration).map_err(|reason| {
                    format!(
                        "entry Display target `{}` has no SIR callable: {reason}",
                        declaration.full_path()
                    )
                })?;
                self.request_body(id);
                id
            }
            hew_types::EntryCallableInstance::Generic { type_args } => {
                self.request_instance(declaration, type_args.clone())?
            }
        };
        self.callable(id)
            .cloned()
            .ok_or_else(|| format!("SIR callable {id:?} is absent from its deterministic table"))
    }

    /// Seed exact caller-selected declarations after validating the complete
    /// set. No body is queued until every root is admitted, and a failed
    /// request publishes no module at all, so one bad root cannot leave a
    /// partially selected lowering behind.
    pub(super) fn request_roots(
        &mut self,
        roots: &[DefId],
    ) -> Result<(), Vec<SirRootSelectionError>> {
        let mut callables = Vec::new();
        let mut errors = Vec::new();
        for declaration in roots.iter().collect::<BTreeSet<_>>() {
            if self.table.templates.contains_key(declaration) {
                errors.push(SirRootSelectionError {
                    declaration: (*declaration).clone(),
                    reason: "generic declarations require a concrete call-site specialization"
                        .to_string(),
                });
                continue;
            }
            match self.admit_monomorphic(declaration) {
                Ok(callable) => callables.push(callable),
                Err(reason) => errors.push(SirRootSelectionError {
                    declaration: (*declaration).clone(),
                    reason,
                }),
            }
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        callables.sort_unstable();
        callables.dedup();
        for callable in callables {
            self.request_body(callable);
        }
        Ok(())
    }

    /// Seed the worklist with every admitted header, in `CallableId` order.
    ///
    /// Generic templates have no header of their own; their instances are
    /// still minted only by resolved call edges, so an uncalled template stays
    /// unproven and its status says so.
    pub(super) fn request_every_callable(&mut self) {
        for declaration in self.table.admissible_order.clone() {
            if let Ok(id) = self.admit_monomorphic(&declaration) {
                self.request_body(id);
            }
        }
    }

    /// Mint the SIR header for one monomorphic declaration, once.
    ///
    /// A header is what a resolved call names, and publishing one obliges the
    /// module to carry its signature's aggregate shapes, variant shapes and
    /// type-fact rows. Admission is therefore demand-driven in the same way
    /// body lowering is: a prelude declaration nothing reachable calls never
    /// becomes a callable, so it puts no record shape, collection glue or row
    /// into a program that does not use it.
    ///
    /// A refusal is recorded once, keyed by the declaration a call would name,
    /// and reported at the call site that wanted it.
    pub(super) fn admit_monomorphic(&mut self, declaration: &DefId) -> Result<CallableId, String> {
        if let Some(id) = self.table.monomorphic_by_declaration.get(declaration) {
            return Ok(*id);
        }
        if let Some(reason) = self.table.ineligible.get(declaration) {
            return Err(reason.clone());
        }
        let Some(admissible) = self.table.admissible.get(declaration) else {
            return Err(
                "the declaration is not present as a HIR function in this module".to_string(),
            );
        };
        let function = admissible.function;
        let symbol = admissible.symbol.clone();
        let signature = callable_signature(self.module, function, &mut self.checked_facts)
            .and_then(|signature| {
                self.require_signature_shapes(&signature)?;
                Ok(signature)
            });
        let signature = match signature {
            Ok(signature) => signature,
            Err(reason) => {
                self.table
                    .ineligible
                    .insert(declaration.clone(), reason.clone());
                return Err(reason);
            }
        };
        let id = CallableId(
            u32::try_from(self.table.callables.len())
                .expect("SIR callable count exceeds the module-local ID range"),
        );
        let source_origin = function_source_origin(self.module, function);
        if source_origin == FunctionSourceOrigin::RootUnit {
            self.table.root_unit_callables.push(id);
        }
        // Entry selection joins on HIR's resolved entry declaration. SIR never
        // re-applies the language's entry rule, so it never compares a
        // declaration path or an emitted symbol against "main". A fact that
        // names a non-root declaration is admitted here and rejected by the
        // verifier's entry rule rather than silently dropped.
        if self.module.entry_exit_plan.as_ref().map(|plan| &plan.entry) == Some(declaration) {
            self.table.entry_callable = Some(id);
        }
        self.table
            .monomorphic_by_declaration
            .insert(declaration.clone(), id);
        self.table.callables.push(SemCallable {
            id,
            function: function.id,
            declaration: declaration.clone(),
            instance: CallableInstance::Monomorphic,
            symbol,
            source_origin,
            signature,
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        });
        self.states.push(CallableState::Unreached);
        self.statuses.push(None);
        Ok(id)
    }

    /// Record demand for one callable's body, once.
    pub(super) fn request_body(&mut self, callable: CallableId) {
        if self.state(callable) != Some(CallableState::Unreached) {
            return;
        }
        self.set_state(callable, CallableState::Queued);
        self.pending.push_back(callable);
    }

    /// Demand the `close` body of every newly admitted `#[resource]` record.
    ///
    /// Drop glue is the only caller of a record's `close`, so the demand
    /// cannot arrive through the call graph: admitting the type is what
    /// obliges the module to carry its release.
    pub(super) fn demand_record_closes(&mut self) {
        let mut closes = Vec::new();
        while self.scanned_record_closes < self.aggregate_shapes.len() {
            let index = self.scanned_record_closes;
            self.scanned_record_closes += 1;
            let shape = &self.aggregate_shapes[index];
            if shape.marker != hew_types::DeclarationMarker::Resource {
                continue;
            }
            let Some(lifecycle) =
                crate::resource::record_resource_lifecycle(self.module, &shape.aggregate_ty)
            else {
                continue;
            };
            closes.push(lifecycle.close_declaration.clone());
        }
        for declaration in closes {
            if let Ok(id) = self.admit_monomorphic(&declaration) {
                self.request_body(id);
            }
        }
    }

    /// Demand the `close` body of every authored opaque `#[resource]` the
    /// module names.
    ///
    /// Drop glue is the only caller of such a close, so like a record's the
    /// demand cannot arrive through the call graph. The projection that
    /// decides which types carry a published release is the same one consulted
    /// here, so a named handle and its executable release always agree. It
    /// runs only once the queue has drained, which bounds the scans to the
    /// number of authored handles the module reaches.
    pub(super) fn demand_opaque_closes(&mut self) {
        let templates: Vec<SemGenericTemplate> = self
            .table
            .generic_templates
            .iter()
            .filter(|template| self.used_templates.contains(&template.id))
            .cloned()
            .collect();
        let mentioned = project_type_facts(
            self.checked_facts.rows(),
            &self.table.callables,
            &templates,
            &self.functions,
            &self.aggregate_shapes,
            &self.variant_shapes,
            &self.vtables,
            &self.value_capabilities,
        );
        let mut closes = Vec::new();
        for key in mentioned.keys() {
            let Some(lifecycle) = crate::resource::authored_opaque_lifecycle(self.module, &key.0)
            else {
                continue;
            };
            let declaration = lifecycle.close_declaration.clone();
            if self.demanded_opaque_closes.insert(declaration.clone()) {
                closes.push(declaration);
            }
        }
        for declaration in closes {
            if let Ok(id) = self.admit_monomorphic(&declaration) {
                self.request_body(id);
            }
        }
    }

    pub(super) fn lower_pending(&mut self) {
        loop {
            self.demand_record_closes();
            if self.pending.is_empty() {
                self.demand_opaque_closes();
            }
            let Some(callable) = self.pending.pop_front() else {
                break;
            };
            if self.state(callable) != Some(CallableState::Queued) {
                continue;
            }
            self.set_state(callable, CallableState::Lowering);
            let result = self.lower_callable(callable);
            self.record_callable_result(callable, result);
        }
    }

    pub(super) fn lower_callable(&mut self, callable: CallableId) -> Result<SemFunction, String> {
        let input = self.input_for_callable(callable)?;
        Builder::new(
            input.function,
            input.callable,
            input.substitution,
            &input.source,
            self,
        )?
        .lower(input.source)
    }

    pub(super) fn record_callable_result(
        &mut self,
        callable: CallableId,
        result: Result<SemFunction, String>,
    ) {
        let index = usize::try_from(callable.0).expect("SIR callable id exceeds usize");
        match result {
            Ok(function) => {
                self.states[index] = CallableState::Lowered;
                self.statuses[index] = Some(SirLoweringStatus::Lowered);
                self.functions.push(function);
            }
            Err(reason) => {
                self.states[index] = CallableState::Failed;
                // Body lowering has no span of its own to report (#3384): the
                // refusal reason is a `String` built deep inside `Builder`,
                // with no source location threaded alongside it. The
                // declaring function's own span is the coarsest attribution
                // available without threading a span through every fallible
                // lowering step, so it stands in as the diagnostic's span.
                //
                // Restricted to the root compilation unit, matching the same
                // cross-module safety rule `hew-codegen-rs::CodegenError`
                // already uses: a byte range only ever indexes the file it
                // was parsed from, and the CLI renders it against the root
                // source. A foreign-module function's span would index the
                // wrong file, so it stays spanless rather than render a caret
                // against unrelated text.
                //
                // `SemCallable::source_origin` is the one authority for that
                // fact — every callable (direct, generic, closure, actor
                // member, entry adapter) sets it once at construction from
                // `function_source_origin`. Actor members register it against
                // the *actor's* HIR item id (their bodies live inside
                // `HirActorDecl`, not the free-function item table), so
                // re-deriving origin here through `functions_by_item` — which
                // only holds free functions and flattened impl methods — gave
                // every actor handler and actor-enclosed closure `None` even
                // when declared in the root file. Trust the stored fact
                // instead of reconstructing it from a table that does not
                // cover every callable shape.
                let span = self
                    .callable(callable)
                    .is_some_and(|meta| meta.source_origin == FunctionSourceOrigin::RootUnit)
                    .then(|| self.input_for_callable(callable).ok())
                    .flatten()
                    .map(|input| input.function.span.clone());
                self.statuses[index] = Some(SirLoweringStatus::Unsupported { reason, span });
            }
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "each callable instance kind selects its own source and substitution together"
    )]
    pub(super) fn input_for_callable(
        &self,
        callable: CallableId,
    ) -> Result<LoweringInput<'a>, String> {
        let callable_meta = self.callable(callable).cloned().ok_or_else(|| {
            format!(
                "SIR callable {} is absent from its deterministic table",
                callable.0
            )
        })?;
        if let CallableInstance::Closure(id) = callable_meta.instance {
            let closure = self
                .closures
                .get(id.0 as usize)
                .ok_or_else(|| "closure body has no enclosing callable".to_string())?;
            let (expression, substitution) = self
                .closure_sources
                .get(id.0 as usize)
                .ok_or_else(|| "closure body has no checked literal source".to_string())?;
            // A closure inherits the source of its exact enclosing callable.
            // Actor handlers have synthesized HIR functions outside the
            // ordinary item table; nested closures must retain that source too.
            let parent = self.input_for_callable(closure.instance.enclosing)?;
            return Ok(LoweringInput {
                function: parent.function,
                callable: callable_meta,
                substitution: substitution.clone(),
                source: BodySource::Closure(expression.clone()),
            });
        }
        if let SemCallableKind::HewActor(actor) = callable_meta.kind {
            let (function, state_bindings, substitution) = self
                .actor_sources
                .get(&callable)
                .ok_or_else(|| "actor body has no checked HIR source".to_string())?;
            return Ok(LoweringInput {
                function: Cow::Owned(function.clone()),
                callable: callable_meta,
                substitution: substitution.clone(),
                source: BodySource::Actor {
                    actor,
                    state_bindings: state_bindings.clone(),
                },
            });
        }
        if let Some((function, substitution)) = self.synthetic_sources.get(&callable) {
            return Ok(LoweringInput {
                function: Cow::Owned(function.clone()),
                callable: callable_meta,
                substitution: substitution.clone(),
                source: BodySource::Function,
            });
        }
        let function = *self
            .table
            .functions_by_item
            .get(&callable_meta.function)
            .ok_or_else(|| {
                format!(
                    "SIR callable `{}` has no HIR source template for its provenance item",
                    callable_meta.symbol
                )
            })?;
        if callable_meta.instance == CallableInstance::EntryAdapter {
            let adapter = self
                .entry_adapter
                .clone()
                .filter(|adapter| adapter.callable == callable)
                .ok_or("entry adapter has no exit plan")?;
            return Ok(LoweringInput {
                function: Cow::Borrowed(function),
                callable: callable_meta,
                substitution: TypeSubstitution::empty(),
                source: BodySource::EntryAdapter(adapter),
            });
        }
        let substitution = match &callable_meta.instance {
            CallableInstance::ActorMember
            | CallableInstance::Closure(_)
            | CallableInstance::EntryAdapter
            | CallableInstance::SupervisorChild { .. } => {
                unreachable!("closure, entry adapter and child spawn inputs are resolved above")
            }
            CallableInstance::Monomorphic => {
                if !function.type_params.is_empty() {
                    return Err(format!(
                        "generic HIR template `{}` was incorrectly admitted as a monomorphic SIR body",
                        function.declaration.full_path()
                    ));
                }
                TypeSubstitution::empty()
            }
            CallableInstance::Generic(key) => {
                if key.template.declaration != function.declaration
                    || callable_meta.declaration != function.declaration
                {
                    return Err(format!(
                        "SIR generic callable `{}` does not agree with its source template declaration",
                        callable_meta.symbol
                    ));
                }
                TypeSubstitution::for_instance(function, &key.type_args)?
            }
        };
        Ok(LoweringInput {
            function: Cow::Borrowed(function),
            callable: callable_meta,
            substitution,
            source: BodySource::Function,
        })
    }

    pub(super) fn resolve_direct_call(
        &mut self,
        declaration: &DefId,
        site: hew_hir::SiteId,
        substitution: &TypeSubstitution,
    ) -> Result<SemCallable, String> {
        if self.table.templates.contains_key(declaration) {
            let raw_args = self.module.call_site_type_args.get(&site).ok_or_else(|| {
                format!(
                    "generic direct call to `{}` is missing checker-resolved type arguments at SIR site {}",
                    declaration.full_path(),
                    site.0
                )
            })?;
            let type_args = raw_args
                .iter()
                .map(|argument| substitution.apply(argument))
                .collect::<Vec<_>>();
            let id = self.request_instance(declaration, type_args)?;
            return self.callable(id).cloned().ok_or_else(|| {
                format!(
                    "requested SIR generic callable {} disappeared from its table",
                    id.0
                )
            });
        }
        let id = self.admit_monomorphic(declaration).map_err(|reason| {
            format!(
                "direct callee `{}` has no scalar default-call SIR callable: {reason}",
                declaration.full_path()
            )
        })?;
        // Resolving a call edge is what makes the callee reachable, so this is
        // where its body becomes demanded. Generic callees go through
        // `request_instance`, which queues the instance it mints.
        self.request_body(id);
        self.callable(id)
            .cloned()
            .ok_or_else(|| format!("SIR callable {id:?} is absent from its deterministic table"))
    }

    /// Select the implementation a static trait call reaches, from the
    /// receiver type this instance's substitution produced.
    ///
    /// The generic template could not name it: `it.next()` under
    /// `I: Iterator<Item = A>` has no implementation until `I` is bound. The
    /// selection reads HIR's structured impl index by declaration identity —
    /// never a symbol spelling — and then enters the ordinary direct-call
    /// admission for the implementation it found.
    pub(super) fn resolve_static_trait_call(
        &mut self,
        declaring_trait: &DefId,
        method: &DefId,
        receiver_ty: &ResolvedTy,
        site: hew_hir::SiteId,
        substitution: &TypeSubstitution,
    ) -> Result<SemCallable, String> {
        let self_type = receiver_ty.impl_receiver_instance().ok_or_else(|| {
            format!(
                "static trait receiver `{}` cannot anchor an implementation",
                receiver_ty.user_facing()
            )
        })?;
        let entry = hew_hir::dispatch::lookup_trait_impl_entry_by_id(
            &self.table.trait_impls,
            declaring_trait,
            &self_type,
            method,
        )
        .cloned()
        .ok_or_else(|| {
            format!(
                "no implementation of `{}` for `{}` provides `{}`",
                declaring_trait.full_path(),
                receiver_ty.user_facing(),
                method.full_path()
            )
        })?;
        if !self.table.templates.contains_key(&entry.method) {
            let id = self.admit_monomorphic(&entry.method).map_err(|reason| {
                format!(
                    "static trait callee `{}` has no scalar default-call SIR callable: {reason}",
                    entry.method.full_path()
                )
            })?;
            self.request_body(id);
            return self.callable(id).cloned().ok_or_else(|| {
                format!("SIR callable {id:?} is absent from its deterministic table")
            });
        }
        let type_args = self.static_trait_instance_args(&entry, &self_type, site, substitution)?;
        let id = self.request_instance(&entry.method, type_args)?;
        self.callable(id).cloned().ok_or_else(|| {
            format!(
                "requested SIR generic callable {} disappeared from its table",
                id.0
            )
        })
    }

    /// Bind impl parameters from the concrete receiver and append the method
    /// parameters selected by the checker at this call site.
    ///
    /// `impl<A, B> Trait for Pair<B, A>` spells its self-type arguments in the
    /// opposite order to its parameter list, so the receiver's arguments are
    /// matched against the implementation's own receiver pattern rather than
    /// handed to the instance positionally.
    pub(super) fn static_trait_instance_args(
        &self,
        entry: &hew_hir::dispatch::TraitImplMethodEntry,
        self_type: &hew_types::NominalInstance,
        site: hew_hir::SiteId,
        substitution: &TypeSubstitution,
    ) -> Result<Vec<ResolvedTy>, String> {
        let method = &entry.method;
        let function = self
            .table
            .templates
            .get(method)
            .ok_or_else(|| {
                format!(
                    "generic implementation `{}` has no SIR template admission record",
                    method.full_path()
                )
            })?
            .function;
        let impl_param_count = entry.impl_type_params.len();
        if !function.type_params.starts_with(&entry.impl_type_params) {
            return Err(format!(
                "generic implementation `{}` has inconsistent impl parameter declarations",
                method.full_path()
            ));
        }
        let method_param_count = function.type_params.len() - impl_param_count;
        let method_args = self.module.call_site_type_args.get(&site);
        if method_args.map_or(0, Vec::len) != method_param_count {
            return Err(format!(
                "static trait call to `{}` requires {method_param_count} checker-resolved method type argument(s) at SIR site {}, found {}",
                method.full_path(),
                site.0,
                method_args.map_or(0, Vec::len),
            ));
        }
        let method_args = method_args
            .into_iter()
            .flatten()
            .map(|argument| substitution.apply(argument));
        if impl_param_count == 0 {
            return Ok(method_args.collect());
        }
        let Some(ResolvedTy::Named {
            args: pattern_args, ..
        }) = function.params.first().map(|param| &param.ty)
        else {
            return Err(format!(
                "generic implementation `{}` has no nominal receiver pattern",
                method.full_path()
            ));
        };
        if pattern_args.len() != self_type.args.len() {
            return Err(format!(
                "generic implementation `{}` declares {} receiver argument(s), the concrete receiver carries {}",
                method.full_path(),
                pattern_args.len(),
                self_type.args.len()
            ));
        }
        let mut bindings: HashMap<&str, &ResolvedTy> = HashMap::new();
        for (pattern, concrete) in pattern_args.iter().zip(&self_type.args) {
            let name = declared_type_param_name(pattern, &entry.impl_type_params).ok_or_else(|| {
                format!(
                    "generic implementation `{}` receives `{}` in a position SIR cannot bind to a type parameter",
                    method.full_path(),
                    pattern.user_facing()
                )
            })?;
            if bindings
                .insert(name, concrete)
                .is_some_and(|prior| prior != concrete)
            {
                return Err(format!(
                    "generic implementation `{}` binds type parameter `{name}` to two different types",
                    method.full_path()
                ));
            }
        }
        let mut arguments = entry
            .impl_type_params
            .iter()
            .map(|param| {
                bindings.get(param.as_str()).map_or_else(
                    || {
                        Err(format!(
                            "generic implementation `{}` leaves type parameter `{param}` unbound by its receiver",
                            method.full_path()
                        ))
                    },
                    |ty| Ok((*ty).clone()),
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        arguments.extend(method_args);
        Ok(arguments)
    }

    pub(super) fn request_closure(
        &mut self,
        enclosing: CallableId,
        expression: &HirExpr,
        substitution: &TypeSubstitution,
    ) -> Result<crate::ClosureId, String> {
        let instance = crate::ClosureInstanceKey {
            enclosing,
            literal: expression.node,
        };
        if let Some(id) = self.closures_by_instance.get(&instance) {
            return Ok(*id);
        }
        let HirExprKind::Closure { captures, .. } = &expression.kind else {
            return Err("closure demand requires a checked literal".to_string());
        };
        let parent = self
            .callable(enclosing)
            .cloned()
            .ok_or_else(|| "closure has no enclosing instance".to_string())?;
        let ty = substitution.apply(&expression.ty);
        self.require_type_facts(&ty)?;
        let (_, _, capabilities) = crate::callable_parts(&ty)?;
        let fields: Vec<_> = captures
            .iter()
            .map(|capture| crate::SemCaptureField {
                binding: capture.binding,
                ty: substitution.apply(&capture.ty),
                access: capture.access,
                consumption: capture.consumption,
            })
            .collect();
        for field in &fields {
            self.require_type_facts(&field.ty)?;
        }
        let mut signature = crate::callable_value_signature(&ty, self.checked_facts.rows())?;
        signature.params.insert(
            0,
            SemAbiParam {
                ty: ty.clone(),
                passing: match capabilities.call {
                    hew_types::CallableCallMode::Read => SemParamPassing::Borrow,
                    hew_types::CallableCallMode::Var => SemParamPassing::BorrowMut,
                    hew_types::CallableCallMode::Once => SemParamPassing::Consume,
                },
                caller_visible_projection: capabilities.call == hew_types::CallableCallMode::Var,
            },
        );
        self.require_signature_shapes(&signature)?;
        let id = crate::ClosureId(
            u32::try_from(self.closures.len())
                .map_err(|_| "closure count exceeds u32".to_string())?,
        );
        let body = CallableId(
            u32::try_from(self.table.callables.len())
                .map_err(|_| "callable count exceeds u32".to_string())?,
        );
        let symbol = format!("{}$closure${}", parent.symbol, expression.node.0);
        if self
            .table
            .callables
            .iter()
            .any(|callable| callable.symbol == symbol)
        {
            return Err("closure symbol conflicts with another exact callable".to_string());
        }
        self.closures.push(crate::SemClosure {
            generator_yield: None,
            id,
            instance,
            body,
            ty,
            fields,
        });
        self.closure_sources
            .push((Box::new(expression.clone()), substitution.clone()));
        self.closures_by_instance.insert(instance, id);
        self.table.callables.push(SemCallable {
            id: body,
            instance: CallableInstance::Closure(id),
            symbol,
            signature,
            kind: SemCallableKind::HewClosure,
            ..parent
        });
        self.states.push(CallableState::Queued);
        self.statuses.push(None);
        self.pending.push_back(body);
        Ok(id)
    }

    pub(super) fn request_instance(
        &mut self,
        declaration: &DefId,
        type_args: Vec<ResolvedTy>,
    ) -> Result<CallableId, String> {
        let template = self
            .table
            .templates
            .get(declaration)
            .cloned()
            .ok_or_else(|| {
                format!(
                    "generic direct callee `{}` has no SIR template admission record",
                    declaration.full_path()
                )
            })?;
        if type_args.len() != template.function.type_params.len() {
            return Err(format!(
                "generic direct callee `{}` expects {} type argument(s), HIR supplied {}",
                declaration.full_path(),
                template.function.type_params.len(),
                type_args.len()
            ));
        }
        for (index, argument) in type_args.iter().enumerate() {
            if !is_supported_instance_type_arg(self.module, &self.checked_facts, argument) {
                return Err(format!(
                    "generic direct callee `{}` type argument {index} is `{}`; SIR generic instances require a concrete semantic value contract",
                    declaration.full_path(),
                    argument.user_facing()
                ));
            }
        }
        for argument in &type_args {
            if !matches!(argument, ResolvedTy::Unit | ResolvedTy::Never) {
                self.require_type_facts(argument)?;
            }
        }
        let key = SirInstanceKey {
            template: template.id,
            type_args,
        };
        self.used_templates.insert(key.template.clone());
        if let Some(existing) = self.by_instance.get(&key).copied() {
            return Ok(existing);
        }
        if self.by_instance.len() >= SIR_GENERIC_INSTANCE_CAP {
            return Err(format!(
                "SIR generic instance cap ({SIR_GENERIC_INSTANCE_CAP}) exceeded while specializing `{}`; refuse unbounded semantic specialization",
                declaration.full_path()
            ));
        }
        let substitution = TypeSubstitution::for_instance(template.function, &key.type_args)?;
        let signature = callable_signature_with_substitution(
            self.module,
            template.function,
            &substitution,
            &mut self.checked_facts,
        )?;
        self.require_signature_shapes(&signature)?;
        let symbol =
            hew_hir::monomorph::function_monomorph_symbol(&template.symbol, &key.type_args);
        if let Some(existing) = self
            .table
            .callables
            .iter()
            .find(|callable| callable.symbol == symbol)
        {
            return Err(format!(
                "SIR generic instance `{}` would collide with callable {} despite a distinct semantic key",
                symbol, existing.id.0
            ));
        }
        let id = CallableId(
            u32::try_from(self.table.callables.len())
                .map_err(|_| "SIR callable count exceeds the module-local ID range".to_string())?,
        );
        self.table.callables.push(SemCallable {
            id,
            function: template.function.id,
            declaration: template.function.declaration.clone(),
            instance: CallableInstance::Generic(key.clone()),
            symbol,
            source_origin: template.source_origin,
            signature,
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        });
        self.by_instance.insert(key, id);
        self.states.push(CallableState::Queued);
        self.statuses.push(None);
        self.pending.push_back(id);
        Ok(id)
    }

    pub(super) fn source_status(&self, function: &HirFn) -> SirLoweringStatus {
        if self.table.templates.contains_key(&function.declaration) {
            let (instances, failed_instances) =
                self.template_instance_counts(&function.declaration);
            return SirLoweringStatus::GenericTemplate {
                instances,
                failed_instances,
            };
        }
        if let Some(callable) = self
            .table
            .monomorphic_by_declaration
            .get(&function.declaration)
            .copied()
        {
            return self.callable_status(callable);
        }
        // No minted header. A recorded refusal means demand did reach the
        // declaration and admission refused it; anything else means nothing
        // asked for it.
        self.table.ineligible.get(&function.declaration).map_or(
            SirLoweringStatus::NotReached,
            |reason| SirLoweringStatus::Unsupported {
                reason: reason.clone(),
                // Same root-unit-only rule as the body-lowering site above.
                span: matches!(
                    function_source_origin(self.module, function),
                    FunctionSourceOrigin::RootUnit
                )
                .then(|| function.span.clone()),
            },
        )
    }

    /// The recorded outcome for one admitted callable header.
    ///
    /// A header the entry closure never demanded has no recorded status; that
    /// is [`SirLoweringStatus::NotReached`], never a body failure.
    pub(super) fn callable_status(&self, callable: CallableId) -> SirLoweringStatus {
        let index = usize::try_from(callable.0).expect("SIR callable id exceeds usize");
        self.statuses
            .get(index)
            .cloned()
            .flatten()
            .unwrap_or(SirLoweringStatus::NotReached)
    }

    pub(super) fn template_instance_counts(&self, declaration: &DefId) -> (usize, usize) {
        let mut instances = 0;
        let mut failed = 0;
        for (key, callable) in &self.by_instance {
            if &key.template.declaration == declaration {
                instances += 1;
                if self.state(*callable) == Some(CallableState::Failed) {
                    failed += 1;
                }
            }
        }
        (instances, failed)
    }

    pub(super) fn state(&self, callable: CallableId) -> Option<CallableState> {
        self.states.get(usize::try_from(callable.0).ok()?).copied()
    }

    pub(super) fn set_state(&mut self, callable: CallableId, state: CallableState) {
        let index = usize::try_from(callable.0).expect("SIR callable id exceeds usize");
        self.states[index] = state;
    }

    pub(super) fn into_module(self) -> SemModule {
        let Self {
            module,
            table,
            checked_facts,
            used_templates,
            mut functions,
            closures,
            actors,
            supervisors,
            vtables,
            aggregate_shapes,
            variant_shapes,
            string_literals,
            bytes_literals,
            value_capabilities,
            structural_display,
            ..
        } = self;
        let debug = crate::SemDebugFacts::project(module);
        let regex_patterns: Vec<String> = module
            .regex_literals
            .iter()
            .map(|literal| literal.pattern.clone())
            .collect();
        let generic_templates: Vec<SemGenericTemplate> = table
            .generic_templates
            .into_iter()
            .filter(|template| used_templates.contains(&template.id))
            .collect();
        // Bodies are produced in demand order, which depends on the entry's
        // call graph. Publishing them in callable order instead keeps the
        // module — and every dump taken from it — a function of the program,
        // not of the traversal that discovered it.
        functions.sort_unstable_by_key(|function| function.callable);
        let type_facts = project_type_facts(
            checked_facts.rows(),
            &table.callables,
            &generic_templates,
            &functions,
            &aggregate_shapes,
            &variant_shapes,
            &vtables,
            &value_capabilities,
        );
        let mut resources: BTreeMap<ResolvedTy, crate::ResourceRelease> = type_facts
            .keys()
            .filter_map(|key| {
                crate::resource::resource_release_from_hir(module, &key.0)
                    .map(|release| (key.0.clone(), release))
            })
            .collect();
        // A release that runs an authored `close` - a `#[resource]` record's
        // or an authored opaque handle's - is a semantic callable, so it is
        // published here, where the resolved callable table is in hand. A
        // lifecycle whose close body never reached demand publishes no
        // release: the type then has no value contract at all rather than a
        // release nothing can execute.
        for key in type_facts.keys() {
            let close_of = |declaration| table.monomorphic_by_declaration.get(declaration).copied();
            let release = if let Some(lifecycle) =
                crate::resource::record_resource_lifecycle(module, &key.0)
            {
                close_of(&lifecycle.close_declaration).map(|close| {
                    crate::ResourceRelease::RecordClose {
                        lifecycle: Box::new(lifecycle.clone()),
                        close,
                    }
                })
            } else if let Some(lifecycle) =
                crate::resource::authored_opaque_lifecycle(module, &key.0)
            {
                close_of(&lifecycle.close_declaration).map(|close| {
                    crate::ResourceRelease::OpaqueClose {
                        lifecycle: Box::new(lifecycle.clone()),
                        close,
                    }
                })
            } else {
                continue;
            };
            if let Some(release) = release {
                resources.insert(key.0.clone(), release);
            }
        }
        SemModule {
            structural_display,
            debug,
            actors,
            supervisors,
            resources,
            closures,
            vtables,
            callables: table.callables,
            generic_templates,
            root_unit_callables: table.root_unit_callables,
            entry_exit_plan: table.entry_exit_plan,
            entry_callable: table.entry_callable,
            functions,
            aggregate_shapes,
            variant_shapes,
            type_facts,
            string_literals,
            bytes_literals,
            regex_patterns,
            value_capabilities,
        }
    }

    pub(super) fn callable_statuses(&self) -> Vec<(CallableId, SirLoweringStatus)> {
        self.table
            .callables
            .iter()
            .map(|callable| (callable.id, self.callable_status(callable.id)))
            .collect()
    }
}
