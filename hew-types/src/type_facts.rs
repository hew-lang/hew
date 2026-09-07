//! Capability facts on the class table (`docs/internal/ir-ladder.md` §6.3).
//!
//! The checker decides once per substituted type and publishes
//! `TypeCheckOutput.type_facts`, keyed by [`TypeInstanceKey`] (§6.2). SIR rules
//! 5 and 6, `TargetLayout`, glue emission and collection descriptors all read
//! this table; nothing downstream re-asks.

use crate::resolved_ty::ResolvedTy;
use std::collections::{BTreeMap, HashMap, HashSet};

use crate::check::TypeDef;
use crate::traits::{MarkerTrait, TraitRegistry};
use crate::value_class::{ClassContext, ClassError, DeclaredType, ValueClass};

/// What a `copy_value` of this type costs, per §1.1's `clone` column.
///
/// Class alone does not decide copy legality: `Rc<T>` and `Generator` are both
/// `AffineResource`, yet `Rc` has a retain path and a generator has none.
/// Rule 6b reads this, not the class.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CloneKind {
    /// Bit copy; no runtime call.
    Bits,
    /// Refcount increment.
    Retain,
    /// Fresh independent allocation through the type's semantic clone operation.
    /// Nested owners must be cloned, never duplicated with raw payload memcpy.
    DeepCopy,
    /// Fresh structure, members copied through their own glue.
    FieldWise,
    /// No copy path exists; a `copy_value` of this type is refused (rule 6b).
    None,
}

/// The `Send` verdict for one type instance.
///
/// **The wrong reading is made unrepresentable** (§6.3): this is not a `bool`,
/// and every `Closure`-keyed row carries [`Self::DeferredToClosureFacts`], so a
/// consumer asking `type_facts[closure_key].send` for a yes/no gets a value it
/// cannot use without going to `closure_facts`. Two closures with identical
/// capture types and opposite capture modes are one type instance and need
/// opposite facts, so the mode-agnostic answer is always wrong for a closure.
///
/// This type deliberately has no `Deref`, no `Into<bool>`, no `unwrap_or` and
/// no other accessor that would hand back a plain `bool`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SendFact {
    Known(bool),
    /// A `Closure` row: the fact lives in `closure_facts`, keyed per closure
    /// expression rather than per type.
    DeferredToClosureFacts,
}

/// The key of every per-type fact table.
///
/// **Structural, not nominal** (§6.2). The nominal spelling
/// `{ template: NominalId, type_args }` is withdrawn: `nominal_instance()`
/// represents source record instances, so `Tuple`, `Array`,
/// `Slice`, `Function`, `Closure`, `Pointer`, `Borrow`, `TraitObject` and
/// `Task` would have no key at all while §1.1 classes every one of them.
///
/// The key is a *type*, never a name: [`crate::mangle::mangle_resolved_ty`]
/// renders a symbol from it and is never a lookup key.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeInstanceKey(pub ResolvedTy);

impl TypeInstanceKey {
    #[must_use]
    pub const fn ty(&self) -> &ResolvedTy {
        &self.0
    }
}

impl From<ResolvedTy> for TypeInstanceKey {
    fn from(ty: ResolvedTy) -> Self {
        Self(ty)
    }
}

/// The authority for one substituted type's ownership and capability facts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeFacts {
    pub class: ValueClass,
    pub clone: CloneKind,
    pub send: SendFact,
    pub hash: bool,
    pub eq: bool,
}

/// Independently selected value operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueCapability {
    Hash,
    Eq,
}

/// The implementation chosen for one concrete value operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueMethodPlan {
    /// The checker permits its structural implementation.
    Derived,
    /// Exact declaration and complete impl-then-method binder arguments.
    User {
        method: crate::DefId,
        type_args: Vec<ResolvedTy>,
    },
}

/// A checker-selected implementation bound to its concrete type and capability.
///
/// Only [`TypeFactService::capability_plan`] creates selections. Consumers can
/// retain and inspect a selection but cannot replace its binding or plan.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueMethodSelection {
    ty: ResolvedTy,
    capability: ValueCapability,
    plan: ValueMethodPlan,
}

impl ValueMethodSelection {
    /// The concrete type for which the checker selected this implementation.
    #[must_use]
    pub fn ty(&self) -> &ResolvedTy {
        &self.ty
    }

    /// The operation authorized by this selection.
    #[must_use]
    pub fn capability(&self) -> ValueCapability {
        self.capability
    }

    /// The implementation selected by the checker.
    #[must_use]
    pub fn plan(&self) -> &ValueMethodPlan {
        &self.plan
    }
}

/// Registration-time receiver and binder order, keyed by the selected `DefId`.
#[derive(Debug, Clone)]
pub(crate) struct ImplMethodBinders {
    pub receiver: crate::Ty,
    pub impl_params: Vec<String>,
    pub method_params: Vec<String>,
    pub obligations: Option<Vec<(String, MarkerTrait)>>,
}

impl ImplMethodBinders {
    fn instantiate(
        &self,
        method: &crate::DefId,
        receiver: &ResolvedTy,
        registry: &TraitRegistry,
    ) -> Result<Vec<ResolvedTy>, ClassError> {
        if let Some(name) = self.method_params.first() {
            return Err(ClassError::TypeParam { name: name.clone() });
        }
        let variables: Vec<_> = self
            .impl_params
            .iter()
            .map(|_| crate::ty::TypeVar::fresh())
            .collect();
        let replacements: HashMap<_, _> = self
            .impl_params
            .iter()
            .cloned()
            .zip(variables.iter().copied().map(crate::Ty::Var))
            .collect();
        let pattern = self
            .receiver
            .substitute_named_params_parallel(&replacements);
        let mut subst = crate::ty::Substitution::new();
        let receiver_ty = receiver.to_ty();
        crate::unify::unify_exact(&mut subst, &pattern, &receiver_ty).map_err(|_| {
            ClassError::UnknownDeclaration {
                name: method.display_name().to_string(),
            }
        })?;
        let type_args: Vec<_> = self
            .impl_params
            .iter()
            .zip(variables)
            .map(|(name, var)| {
                let argument = subst.resolve(&crate::Ty::Var(var));
                // An impl binder is instantiated by a component of the
                // concrete receiver. Preserve that checked boundary type,
                // including opacity inside nested arguments, rather than
                // reconstructing it through the lossy Ty round trip.
                let mut components = vec![receiver.clone()];
                while let Some(component) = components.pop() {
                    if component.to_ty() == argument {
                        return Ok(component);
                    }
                    push_type_components(&component, &mut components);
                }
                Err(ClassError::TypeParam { name: name.clone() })
            })
            .collect::<Result<_, _>>()?;
        let refusal = || ClassError::UnknownDeclaration {
            name: method.display_name().to_string(),
        };
        for (param, marker) in self.obligations.as_ref().ok_or_else(refusal)? {
            let position = self
                .impl_params
                .iter()
                .position(|name| name == param)
                .ok_or_else(refusal)?;
            if !registry.implements_marker(&type_args[position].to_ty(), *marker) {
                return Err(refusal());
            }
        }
        Ok(type_args)
    }
}

fn require_concrete_capability_type(ty: &ResolvedTy) -> Result<(), ClassError> {
    if let ResolvedTy::TypeParam { name } = ty {
        return Err(ClassError::TypeParam { name: name.clone() });
    }
    let mut components = Vec::new();
    push_type_components(ty, &mut components);
    for component in components {
        require_concrete_capability_type(&component)?;
    }
    Ok(())
}

/// Shared exact-specialization-then-nominal lookup over checker registration.
pub(crate) fn selected_impl_method(
    ids: &HashMap<(String, String, String), crate::DefId>,
    owner: &str,
    args: &[ResolvedTy],
    trait_name: &str,
    method_name: &str,
) -> Option<(crate::DefId, String)> {
    crate::resolved_ty::mangle_impl_self_name(owner, args)
        .filter(|_| !args.is_empty())
        .into_iter()
        .chain(std::iter::once(owner.to_string()))
        .find_map(|owner| {
            ids.get(&(
                owner.clone(),
                trait_name.to_string(),
                method_name.to_string(),
            ))
            .cloned()
            .map(|id| (id, owner))
        })
}

/// Checker-owned declaration context for concrete fact expansion.
///
/// This is an immutable snapshot of the declarations and capability authority
/// the checker used. Keeping both here lets later concrete specialization ask
/// the same service instead of reconstructing marker or collection eligibility
/// rules in an IR crate.
#[derive(Debug, Clone, Default)]
pub struct TypeFactContext {
    declarations: BTreeMap<String, DeclaredType>,
    registry: TraitRegistry,
    type_defs: HashMap<String, TypeDef>,
    method_ids: HashMap<(String, String, String), crate::DefId>,
    method_binders: HashMap<crate::DefId, ImplMethodBinders>,
}

impl TypeFactContext {
    #[must_use]
    pub fn new(
        declarations: BTreeMap<String, DeclaredType>,
        registry: TraitRegistry,
        type_defs: HashMap<String, TypeDef>,
    ) -> Self {
        Self {
            declarations,
            registry,
            type_defs,
            method_ids: HashMap::new(),
            method_binders: HashMap::new(),
        }
    }

    pub(crate) fn with_impl_methods(
        mut self,
        ids: HashMap<(String, String, String), crate::DefId>,
        binders: HashMap<crate::DefId, ImplMethodBinders>,
    ) -> Self {
        self.method_ids = ids;
        self.method_binders = binders;
        self
    }

    #[must_use]
    pub fn declarations(&self) -> &BTreeMap<String, DeclaredType> {
        &self.declarations
    }
}

/// The one service that publishes and expands concrete type facts.
///
/// Checker-authored rows include independently decided capability columns.
/// When monomorphization synthesizes a new concrete instance, this service
/// computes the row from the retained checker authority. It never asks an IR
/// crate to classify the type or maintain a parallel capability table.
#[derive(Debug, Clone)]
pub struct TypeFactService {
    context: TypeFactContext,
    rows: BTreeMap<TypeInstanceKey, TypeFacts>,
}

impl TypeFactService {
    #[must_use]
    pub fn new(context: TypeFactContext, rows: BTreeMap<TypeInstanceKey, TypeFacts>) -> Self {
        Self { context, rows }
    }

    #[must_use]
    pub fn rows(&self) -> &BTreeMap<TypeInstanceKey, TypeFacts> {
        &self.rows
    }

    #[must_use]
    pub fn into_rows(self) -> BTreeMap<TypeInstanceKey, TypeFacts> {
        self.rows
    }

    /// Resolve an exact source record through the checker's declaration table.
    /// Field order, parameter substitution and opacity come from that same
    /// declaration for user records and source-defined builtin records.
    ///
    /// # Errors
    ///
    /// Refuses non-record types, absent declarations, opaque carriers and
    /// incomplete or incorrectly instantiated field definitions.
    pub fn record_fields(
        &self,
        ty: &ResolvedTy,
    ) -> Result<(crate::NominalInstance, Vec<(String, ResolvedTy)>), String> {
        let instance = ty.nominal_instance().ok_or_else(|| {
            format!(
                "`{}` is not a checker-resolved named record",
                ty.user_facing()
            )
        })?;
        let name = instance.nominal.full_path();
        let definition = self.context.type_defs.get(name).ok_or_else(|| {
            format!(
                "aggregate `{}` has no exact checker declaration",
                ty.user_facing()
            )
        })?;
        let declaration =
            self.context.declarations.get(name).ok_or_else(|| {
                format!("aggregate `{}` has no declaration facts", ty.user_facing())
            })?;
        if !matches!(
            definition.kind,
            crate::check::TypeDefKind::Struct | crate::check::TypeDefKind::Record
        ) || declaration.is_opaque
            || definition.field_order.len() != definition.fields.len()
            || definition.field_order.len() != declaration.members.len()
        {
            return Err(format!(
                "`{}` has no transparent named-field record contract",
                ty.user_facing()
            ));
        }
        if definition.type_params.len() != instance.args.len() {
            return Err(format!(
                "aggregate `{}` has incorrect type argument arity",
                ty.user_facing()
            ));
        }
        let fields = definition
            .field_order
            .iter()
            .zip(&declaration.members)
            .map(|(name, field)| {
                (
                    name.clone(),
                    crate::value_class::substitute(field, &definition.type_params, &instance.args),
                )
            })
            .collect();
        Ok((instance, fields))
    }

    /// Read a nominal type's own declaration marker. A plain record may
    /// contain resources without acquiring their whole-value cleanup boundary.
    ///
    /// # Errors
    ///
    /// Refuses non-nominal types, missing declarations and incorrect arity.
    /// This query does not grant transparent field access.
    pub fn declaration_marker(&self, ty: &ResolvedTy) -> Result<crate::DeclarationMarker, String> {
        let instance = ty
            .nominal_instance()
            .ok_or_else(|| format!("`{}` has no nominal declaration", ty.user_facing()))?;
        let declaration = self
            .context
            .declarations
            .get(instance.nominal.full_path())
            .ok_or_else(|| format!("`{}` has no declaration facts", ty.user_facing()))?;
        if declaration.type_params.len() != instance.args.len() {
            return Err(format!(
                "`{}` has incorrect type argument arity",
                ty.user_facing()
            ));
        }
        Ok(declaration.marker)
    }

    /// Ensure one concrete type and its components have rows.
    ///
    /// # Errors
    ///
    /// Refuses abstract types, unknown declarations, and recursive
    /// instantiations whose fact graph is not finite.
    pub fn require(&mut self, ty: &ResolvedTy) -> Result<TypeFacts, ClassError> {
        let key = TypeInstanceKey(ty.clone());
        if let Some(row) = self.rows.get(&key) {
            return Ok(*row);
        }

        let mut components = Vec::new();
        push_type_components(ty, &mut components);
        for component in &components {
            self.require(component)?;
        }

        let row = self.contextual_facts(ty)?;
        self.rows.insert(key, row);
        Ok(row)
    }

    /// Select one operation independently from the other capability.
    ///
    /// # Errors
    /// Refuses abstract receivers, absent declaration metadata, inconsistent
    /// receiver patterns and method binders that cannot be inferred from self.
    pub fn capability_plan(
        &mut self,
        ty: &ResolvedTy,
        capability: ValueCapability,
    ) -> Result<Option<ValueMethodSelection>, ClassError> {
        Ok(self
            .select_capability(ty, capability)?
            .map(|plan| ValueMethodSelection {
                ty: ty.clone(),
                capability,
                plan,
            }))
    }

    fn select_capability(
        &self,
        ty: &ResolvedTy,
        capability: ValueCapability,
    ) -> Result<Option<ValueMethodPlan>, ClassError> {
        self.select_capability_inner(ty, capability, &mut HashSet::new())
    }

    fn select_capability_inner(
        &self,
        ty: &ResolvedTy,
        capability: ValueCapability,
        visiting: &mut HashSet<(ResolvedTy, ValueCapability)>,
    ) -> Result<Option<ValueMethodPlan>, ClassError> {
        crate::value_class::classify_ty(ty, &ClassContext::new(&self.context.declarations))?;
        require_concrete_capability_type(ty)?;
        let as_ty = ty.to_ty();
        let (trait_name, method_name) = match capability {
            ValueCapability::Hash => ("Hash", "hash"),
            ValueCapability::Eq => ("Eq", "eq"),
        };
        let builtin_owner = crate::Checker::canonical_primitive_or_builtin_key(&as_ty);
        let (owner, args) = match ty {
            ResolvedTy::Named { name, args, .. } => {
                (builtin_owner.as_deref().unwrap_or(name), args.as_slice())
            }
            _ => (builtin_owner.as_deref().unwrap_or(""), &[][..]),
        };
        if let Some((method, _)) = selected_impl_method(
            &self.context.method_ids,
            owner,
            args,
            trait_name,
            method_name,
        ) {
            if capability == ValueCapability::Hash {
                let admitted_shape = match ty {
                    ResolvedTy::Named {
                        builtin: Some(builtin),
                        ..
                    } => matches!(
                        builtin,
                        crate::BuiltinType::NodeId
                            | crate::BuiltinType::Location
                            | crate::BuiltinType::RemotePid
                    ),
                    ResolvedTy::Named { name, .. } => {
                        self.context.type_defs.get(name).is_some_and(|definition| {
                            !definition.is_indirect
                                && matches!(
                                    definition.kind,
                                    crate::check::TypeDefKind::Struct
                                        | crate::check::TypeDefKind::Record
                                )
                        })
                    }
                    _ => true,
                };
                if !admitted_shape {
                    return Ok(None);
                }
            }
            let binders = self.context.method_binders.get(&method).ok_or_else(|| {
                ClassError::UnknownDeclaration {
                    name: method.display_name().to_string(),
                }
            })?;
            let type_args = binders.instantiate(&method, ty, &self.context.registry)?;
            return Ok(Some(ValueMethodPlan::User { method, type_args }));
        }
        if !crate::check::declaration_walk_terminates(ty, &self.context.type_defs) {
            return Ok(None);
        }
        let key = (ty.clone(), capability);
        if !visiting.insert(key.clone()) {
            // Recursive structural operations are outside the admitted surface.
            // Canonical member identities may expose a cycle hidden by an
            // import presentation in the source declaration walk.
            return Ok(None);
        }
        let derived = match capability {
            ValueCapability::Hash => self.derived_hash(ty, visiting),
            ValueCapability::Eq => self.derived_eq(ty, visiting),
        };
        visiting.remove(&key);
        derived.map(|allowed| allowed.then_some(ValueMethodPlan::Derived))
    }

    fn derived_eq(
        &self,
        ty: &ResolvedTy,
        visiting: &mut HashSet<(ResolvedTy, ValueCapability)>,
    ) -> Result<bool, ClassError> {
        use crate::BuiltinType;
        match ty {
            ResolvedTy::Tuple(members) => {
                self.members_have_capability(members, ValueCapability::Eq, visiting)
            }
            ResolvedTy::Named {
                builtin:
                    Some(builtin @ (BuiltinType::Option | BuiltinType::Result | BuiltinType::Vec)),
                args,
                ..
            } => {
                let arity = if *builtin == BuiltinType::Result {
                    2
                } else {
                    1
                };
                if args.len() != arity {
                    return Err(ClassError::UnknownDeclaration {
                        name: ty.user_facing().to_string(),
                    });
                }
                self.members_have_capability(args, ValueCapability::Eq, visiting)
            }
            ResolvedTy::Named {
                builtin: Some(BuiltinType::NodeId | BuiltinType::Location | BuiltinType::RemotePid),
                ..
            } => Ok(true),
            ResolvedTy::Named {
                builtin:
                    Some(
                        BuiltinType::HashMap
                        | BuiltinType::HashSet
                        | BuiltinType::Rc
                        | BuiltinType::Weak
                        | BuiltinType::JsonValue
                        | BuiltinType::YamlValue,
                    ),
                ..
            } => Ok(false),
            ResolvedTy::Named {
                name,
                args,
                builtin,
                ..
            } => {
                let owner = ty.nominal_instance().map_or_else(
                    || name.clone(),
                    |instance| instance.nominal.full_path().to_string(),
                );
                let Some(definition) = self.context.type_defs.get(&owner) else {
                    if builtin.is_some() {
                        return Ok(false);
                    }
                    return Err(ClassError::UnknownDeclaration { name: owner });
                };
                if definition.is_indirect
                    || (builtin.is_some() && definition.type_params.len() != args.len())
                {
                    return Ok(false);
                }
                self.members_have_capability(
                    &self.declared_capability_members(ty)?,
                    ValueCapability::Eq,
                    visiting,
                )
            }
            _ => Ok(matches!(
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
                    | ResolvedTy::F32
                    | ResolvedTy::F64
                    | ResolvedTy::Bool
                    | ResolvedTy::Char
                    | ResolvedTy::Duration
                    | ResolvedTy::Unit
                    | ResolvedTy::String
                    | ResolvedTy::Bytes
            )),
        }
    }

    fn members_have_capability(
        &self,
        members: &[ResolvedTy],
        capability: ValueCapability,
        visiting: &mut HashSet<(ResolvedTy, ValueCapability)>,
    ) -> Result<bool, ClassError> {
        for member in members {
            if self
                .select_capability_inner(member, capability, visiting)?
                .is_none()
            {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Instantiate declaration members, preserving the same source identities
    /// and parallel parameter substitution as the class and record services.
    fn declared_capability_members(&self, ty: &ResolvedTy) -> Result<Vec<ResolvedTy>, ClassError> {
        let ResolvedTy::Named { name, args, .. } = ty else {
            return Err(ClassError::UnknownDeclaration {
                name: ty.user_facing().to_string(),
            });
        };
        let nominal = ty.nominal_instance();
        let name = nominal
            .as_ref()
            .map_or(name.as_str(), |instance| instance.nominal.full_path());
        let declaration =
            self.context
                .declarations
                .get(name)
                .ok_or_else(|| ClassError::UnknownDeclaration {
                    name: name.to_string(),
                })?;
        if declaration.type_params.len() != args.len() {
            return Err(ClassError::UnknownDeclaration {
                name: ty.user_facing().to_string(),
            });
        }
        if declaration.marker == crate::DeclarationMarker::None {
            return Ok(declaration
                .members
                .iter()
                .map(|member| {
                    crate::value_class::substitute(member, &declaration.type_params, args)
                })
                .collect());
        }
        // A marked declaration keeps no members when one of them could not be
        // rendered at the boundary: its class came from the marker anyway.
        // Equality and hashing still inspect the declaration's own fields.
        let definition =
            self.context
                .type_defs
                .get(name)
                .ok_or_else(|| ClassError::UnknownDeclaration {
                    name: name.to_string(),
                })?;
        let mut members: Vec<_> = definition.fields.iter().collect();
        members.sort_by_key(|(name, _)| *name);
        let mut types: Vec<_> = members.into_iter().map(|(_, ty)| ty).collect();
        let mut variants: Vec<_> = definition.variants.iter().collect();
        variants.sort_by_key(|(name, _)| *name);
        for (_, variant) in variants {
            match variant {
                crate::check::VariantDef::Unit => {}
                crate::check::VariantDef::Tuple(payload) => types.extend(payload),
                crate::check::VariantDef::Struct(fields) => {
                    types.extend(fields.iter().map(|(_, ty)| ty));
                }
            }
        }
        types
            .into_iter()
            .map(|member| {
                let member =
                    ResolvedTy::from_ty(member).map_err(|_| ClassError::UnknownDeclaration {
                        name: name.to_string(),
                    })?;
                let member = crate::check::resolve_member_ty(
                    member,
                    name.rsplit_once('.').map(|(prefix, _)| prefix),
                    &self.context.type_defs,
                    &|name| {
                        self.context
                            .declarations
                            .get(name)
                            .is_some_and(|decl| decl.is_opaque)
                    },
                );
                Ok(crate::value_class::substitute(
                    &member,
                    &declaration.type_params,
                    args,
                ))
            })
            .collect()
    }

    fn derived_hash(
        &self,
        ty: &ResolvedTy,
        visiting: &mut HashSet<(ResolvedTy, ValueCapability)>,
    ) -> Result<bool, ClassError> {
        match ty {
            // Preserve the existing identity-aggregate exceptions exactly.
            ResolvedTy::Named {
                builtin: Some(builtin),
                ..
            } => Ok(matches!(
                builtin,
                crate::BuiltinType::NodeId
                    | crate::BuiltinType::Location
                    | crate::BuiltinType::RemotePid
            )),
            ResolvedTy::Named { name, args, .. } => {
                let definition = self
                    .context
                    .type_defs
                    .get(name)
                    .ok_or_else(|| ClassError::UnknownDeclaration { name: name.clone() })?;
                if definition.type_params.len() != args.len() {
                    return Err(ClassError::UnknownDeclaration {
                        name: ty.user_facing().to_string(),
                    });
                }
                if self
                    .context
                    .declarations
                    .get(name)
                    .is_some_and(|decl| decl.is_opaque)
                    || definition.is_indirect
                    || !matches!(
                        definition.kind,
                        crate::check::TypeDefKind::Struct | crate::check::TypeDefKind::Record
                    )
                    || self.context.registry.resource_type_names().contains(name)
                {
                    return Ok(false);
                }
                self.members_have_capability(
                    &self.declared_capability_members(ty)?,
                    ValueCapability::Hash,
                    visiting,
                )
            }
            _ => Ok(matches!(
                crate::hash_eligibility::ty_is_hash_eligible_with_resources(
                    &ty.to_ty(),
                    &self.context.type_defs,
                    self.context.registry.resource_type_names(),
                ),
                crate::hash_eligibility::HashEligibility::Eligible
            )),
        }
    }

    fn contextual_facts(&self, ty: &ResolvedTy) -> Result<TypeFacts, ClassError> {
        let declarations = ClassContext::new(&self.context.declarations);
        let as_ty = ty.to_ty();
        let send = self.send_fact(ty, &as_ty);
        let hash = self.select_capability(ty, ValueCapability::Hash)?.is_some();
        let eq = self.select_capability(ty, ValueCapability::Eq)?.is_some();
        TypeFacts::of_type(ty, &declarations, send, hash, eq)
    }

    fn send_fact(&self, ty: &ResolvedTy, as_ty: &crate::Ty) -> SendFact {
        if matches!(ty, ResolvedTy::Closure { .. }) {
            return SendFact::DeferredToClosureFacts;
        }
        let component_deferred = matches!(
            ty,
            ResolvedTy::Tuple(_)
                | ResolvedTy::Array(_, _)
                | ResolvedTy::Slice(_)
                | ResolvedTy::Task(_)
        ) && {
            let mut components = Vec::new();
            push_type_components(ty, &mut components);
            components.iter().any(|component| {
                self.rows
                    .get(&TypeInstanceKey(component.clone()))
                    .is_some_and(|row| row.send == SendFact::DeferredToClosureFacts)
            })
        };
        if component_deferred {
            SendFact::DeferredToClosureFacts
        } else {
            SendFact::Known(
                self.context
                    .registry
                    .implements_marker(as_ty, MarkerTrait::Send),
            )
        }
    }
}

impl TypeFacts {
    /// Build the facts for one substituted type.
    ///
    /// `send`, `hash` and `eq` are decided by their own checker authorities and
    /// handed in; `class` and `clone` come from the §1.1 table.
    ///
    /// # Errors
    ///
    /// Returns [`ClassError`] when §1.1 refuses the type. There is no default
    /// row: a type with no class gets no entry, and a consumer fails closed on
    /// the absence.
    pub fn of_type(
        ty: &ResolvedTy,
        decls: &ClassContext<'_>,
        send: SendFact,
        hash: bool,
        eq: bool,
    ) -> Result<Self, ClassError> {
        let (class, clone) = crate::value_class::classify_ty(ty, decls)?;
        let send = match ty {
            ResolvedTy::Closure { .. } => SendFact::DeferredToClosureFacts,
            // MARKED SHORTCUT - a trait object's send fact is `false` here.
            // WHY: §6.3 makes it "the bound list contains `Send`", and that is
            // sound *only* because §1.1's `CoerceToDynTrait` wall refuses a
            // coercion into a `+ Send` object whose concrete is not `Send`. The
            // wall does not exist yet, so the bound list would let a
            // `dyn ... + Send` over an `Rc`-holding concrete be shared between
            // actors and race a non-atomic count.
            // WHEN: the coercion wall lands with the closure send facts.
            // WHAT: this constructor takes the caller's decided fact, which is
            // then the bound list §6.3 names.
            ResolvedTy::TraitObject { .. } => SendFact::Known(false),
            _ => send,
        };
        Ok(Self {
            class,
            clone,
            send,
            hash,
            eq,
        })
    }
}

/// Push a type's immediate component types, so the fact table is closed under
/// the types its rows are built from.
pub fn push_type_components(ty: &ResolvedTy, out: &mut Vec<ResolvedTy>) {
    match ty {
        ResolvedTy::Tuple(elements) => out.extend(elements.iter().cloned()),
        ResolvedTy::Array(element, _) | ResolvedTy::Slice(element) => {
            out.push((**element).clone());
        }
        ResolvedTy::Named { args, .. } => out.extend(args.iter().cloned()),
        ResolvedTy::Function { params, ret, .. } => {
            out.extend(params.iter().cloned());
            out.push((**ret).clone());
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
            ..
        } => {
            out.extend(params.iter().cloned());
            out.push((**ret).clone());
            out.extend(captures.iter().cloned());
        }
        ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
            out.push((**pointee).clone());
        }
        ResolvedTy::Task(inner) => out.push((**inner).clone()),
        ResolvedTy::TraitObject { traits } => {
            for bound in traits {
                out.extend(bound.args.iter().cloned());
                out.extend(bound.assoc_bindings.iter().map(|(_, ty)| ty.clone()));
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::{
        CloneKind, SendFact, TypeFactContext, TypeFactService, TypeFacts, TypeInstanceKey,
    };
    use crate::builtin_type::{builtin_types, BuiltinType};
    use crate::resolved_ty::{ResolvedTraitBound, ResolvedTy};
    use crate::value_class::{
        ClassContext, ClassError, DeclarationMarker, DeclaredType, ValueClass,
    };

    fn named(name: &str, builtin: Option<BuiltinType>, args: Vec<ResolvedTy>) -> ResolvedTy {
        ResolvedTy::Named {
            name: name.to_string(),
            args,
            builtin,
            is_opaque: false,
        }
    }

    fn generic_impl_binders(receiver_name: &str) -> super::ImplMethodBinders {
        super::ImplMethodBinders {
            receiver: crate::Ty::named(receiver_name, vec![crate::Ty::named("T", vec![])]),
            impl_params: vec!["T".to_string()],
            method_params: vec![],
            obligations: Some(vec![]),
        }
    }

    #[test]
    fn impl_method_binders_reject_same_leaf_receiver_from_another_owner() {
        let binders = generic_impl_binders("local.Wrapper");
        let receiver = named("foreign.Wrapper", None, vec![ResolvedTy::I64]);

        assert!(matches!(
            binders.instantiate(
                &crate::DefId::for_test("local.Wrapper::eq"),
                &receiver,
                &crate::traits::TraitRegistry::new(),
            ),
            Err(ClassError::UnknownDeclaration { .. })
        ));
    }

    #[test]
    fn impl_method_binders_recover_the_exact_nested_opaque_argument() {
        let binders = generic_impl_binders("owner.Wrapper");
        let opaque = ResolvedTy::Named {
            name: "owner.Handle".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: true,
        };
        let argument =
            ResolvedTy::named_builtin("Option", BuiltinType::Option, vec![opaque.clone()]);
        let receiver = named("owner.Wrapper", None, vec![argument.clone()]);

        let inferred = binders
            .instantiate(
                &crate::DefId::for_test("owner.Wrapper::eq"),
                &receiver,
                &crate::traits::TraitRegistry::new(),
            )
            .unwrap();

        assert_eq!(inferred, vec![argument]);
        assert!(matches!(
            &inferred[0],
            ResolvedTy::Named { args, .. }
                if matches!(&args[0], ResolvedTy::Named { is_opaque: true, .. })
        ));
    }

    /// Declarations the §1.1 Aggregate rule needs for the cases below.
    fn declarations() -> BTreeMap<String, DeclaredType> {
        let mut decls = BTreeMap::new();
        decls.insert(
            "Conn".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::Resource,
                type_params: vec![],
                members: vec![ResolvedTy::I64],
            },
        );
        decls.insert(
            "Ticket".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::Linear,
                type_params: vec![],
                members: vec![ResolvedTy::I64],
            },
        );
        decls.insert(
            "Point".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![ResolvedTy::I64, ResolvedTy::I64],
            },
        );
        decls.insert(
            "Label".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![ResolvedTy::String, ResolvedTy::I64],
            },
        );
        // `std/failure.hew::CrashInfo { code: i64, message: string }`.
        decls.insert(
            "CrashInfo".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![ResolvedTy::I64, ResolvedTy::String],
            },
        );
        // `std/failure.hew::CrashNotification { actor_id: u64, kind: CrashKind }`.
        decls.insert(
            "CrashNotification".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![
                    ResolvedTy::U64,
                    named("CrashKind", Some(BuiltinType::CrashKind), vec![]),
                ],
            },
        );
        decls.insert(
            "std.builtins.VecIter".to_string(),
            DeclaredType {
                builtin: None,
                type_params: vec!["T".to_string()],
                members: vec![
                    ResolvedTy::named_builtin(
                        "Vec",
                        BuiltinType::Vec,
                        vec![ResolvedTy::TypeParam {
                            name: "T".to_string(),
                        }],
                    ),
                    ResolvedTy::I64,
                ],
                ..DeclaredType::default()
            },
        );
        decls
    }

    fn facts(ty: &ResolvedTy) -> (ValueClass, CloneKind) {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        crate::value_class::classify_ty(ty, &context)
            .unwrap_or_else(|error| panic!("§1.1 refused `{ty:?}`: {error}"))
    }

    fn conn() -> ResolvedTy {
        named("Conn", None, vec![])
    }

    fn rc(inner: ResolvedTy) -> ResolvedTy {
        named("Rc", Some(BuiltinType::Rc), vec![inner])
    }

    fn vec_of(element: ResolvedTy) -> ResolvedTy {
        named("Vec", Some(BuiltinType::Vec), vec![element])
    }

    fn closure_over(captures: Vec<ResolvedTy>) -> ResolvedTy {
        ResolvedTy::Closure {
            capabilities: crate::CallableCapabilities {
                suspends: false,
                clone: captures.iter().all(|capture| {
                    matches!(
                        facts(capture).0,
                        ValueClass::BitCopy
                            | ValueClass::CowValue
                            | ValueClass::PersistentShare
                            | ValueClass::View
                    )
                }),
                ..crate::CallableCapabilities::default()
            },
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
            captures,
        }
    }

    #[test]
    fn callable_clone_cannot_be_forged_over_resource_captures() {
        let declarations = declarations();
        let context = ClassContext::new(&declarations);
        let forged = ResolvedTy::Closure {
            capabilities: crate::CallableCapabilities::FUNCTION_ITEM,
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
            captures: vec![conn()],
        };
        assert_eq!(
            crate::value_class::classify_ty(&forged, &context),
            Err(ClassError::CallableCloneConflict)
        );
        let cloneable = ResolvedTy::Function {
            capabilities: crate::CallableCapabilities::FUNCTION_ITEM,
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
        };
        assert_eq!(
            facts(&cloneable),
            (ValueClass::CowValue, CloneKind::FieldWise)
        );
    }

    /// §1.1's own test sentence: every `ResolvedTy` arm, asserting
    /// `(class, clone)`.
    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "one case per ResolvedTy arm is the point of the table test"
    )]
    fn every_resolved_ty_arm_has_the_ladder_class_and_clone() {
        let cases: Vec<(ResolvedTy, ValueClass, CloneKind)> = vec![
            (ResolvedTy::I8, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::I16, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::I32, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::I64, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::U8, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::U16, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::U32, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::U64, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Isize, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Usize, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::F32, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::F64, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Bool, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Char, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Duration, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Unit, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Never, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::String, ValueClass::CowValue, CloneKind::Retain),
            (ResolvedTy::Bytes, ValueClass::CowValue, CloneKind::Retain),
            (
                ResolvedTy::CancellationToken,
                ValueClass::AffineResource,
                CloneKind::None,
            ),
            (
                ResolvedTy::Slice(Box::new(ResolvedTy::I64)),
                ValueClass::View,
                CloneKind::Bits,
            ),
            (
                ResolvedTy::Pointer {
                    is_mutable: false,
                    pointee: Box::new(ResolvedTy::I64),
                },
                ValueClass::View,
                CloneKind::Bits,
            ),
            (
                ResolvedTy::Borrow {
                    pointee: Box::new(ResolvedTy::I64),
                },
                ValueClass::View,
                CloneKind::Bits,
            ),
            (
                ResolvedTy::Function {
                    capabilities: crate::CallableCapabilities::default(),
                    params: vec![],
                    ret: Box::new(ResolvedTy::Unit),
                },
                ValueClass::AffineResource,
                CloneKind::None,
            ),
            (
                ResolvedTy::TraitObject {
                    traits: vec![ResolvedTraitBound {
                        trait_name: "Show".to_string(),
                        args: vec![],
                        assoc_bindings: vec![],
                    }],
                },
                ValueClass::PersistentShare,
                CloneKind::Retain,
            ),
            (
                closure_over(vec![ResolvedTy::I64]),
                ValueClass::CowValue,
                CloneKind::FieldWise,
            ),
            (
                ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]),
                ValueClass::BitCopy,
                CloneKind::Bits,
            ),
            (
                ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
                ValueClass::CowValue,
                CloneKind::FieldWise,
            ),
            (
                ResolvedTy::Array(Box::new(ResolvedTy::I64), 4),
                ValueClass::BitCopy,
                CloneKind::Bits,
            ),
            (
                ResolvedTy::Array(Box::new(conn()), 3),
                ValueClass::AffineResource,
                CloneKind::None,
            ),
            (
                ResolvedTy::Task(Box::new(ResolvedTy::I64)),
                ValueClass::Linear,
                CloneKind::None,
            ),
            (conn(), ValueClass::AffineResource, CloneKind::None),
            (
                named("Ticket", None, vec![]),
                ValueClass::Linear,
                CloneKind::None,
            ),
            (
                named("Point", None, vec![]),
                ValueClass::BitCopy,
                CloneKind::Bits,
            ),
            (
                named("Label", None, vec![]),
                ValueClass::CowValue,
                CloneKind::FieldWise,
            ),
        ];
        for (ty, class, clone) in cases {
            assert_eq!((class, clone), facts(&ty), "class table row for `{ty:?}`");
        }

        // `TypeParam` is an error, not a class: the instance service
        // substitutes before SIR sees it.
        let decls = declarations();
        let context = ClassContext::new(&decls);
        assert_eq!(
            ValueClass::of_ty(
                &ResolvedTy::TypeParam {
                    name: "T".to_string()
                },
                &context
            ),
            Err(ClassError::TypeParam {
                name: "T".to_string()
            })
        );
    }

    /// §1.1's own test sentence: **every `BuiltinType` variant**, in a `match`
    /// with no wildcard arm, so a new variant is a compile error rather than a
    /// silent `Unknown`.
    #[test]
    #[expect(
        clippy::match_same_arms,
        reason = "distinct §1.1 rows that agree for these arguments must stay distinct arms"
    )]
    fn every_builtin_type_variant_has_the_ladder_class_and_clone() {
        let decls = declarations();
        let context = ClassContext::new(&decls);

        for info in builtin_types() {
            // Two type arguments cover every builtin's arity in this table;
            // extra arguments are ignored by the rows that do not read them.
            let ty = named(
                info.canonical_name,
                Some(info.kind),
                match info.arity {
                    0 => vec![],
                    1 => vec![ResolvedTy::I64],
                    _ => vec![ResolvedTy::I64, ResolvedTy::String],
                },
            );
            let expected: Option<(ValueClass, CloneKind)> = match info.kind {
                BuiltinType::SupervisorPool
                | BuiltinType::ChildRef
                | BuiltinType::NodeId
                | BuiltinType::Location
                | BuiltinType::RemotePid
                | BuiltinType::MonitorId
                | BuiltinType::DownTarget
                | BuiltinType::DownReason
                | BuiltinType::DownNotification
                | BuiltinType::Instant
                | BuiltinType::Unit
                | BuiltinType::Duration
                | BuiltinType::Range
                | BuiltinType::Trap
                | BuiltinType::TimeoutError
                | BuiltinType::CrashAction
                | BuiltinType::CrashKind
                | BuiltinType::SendError
                | BuiltinType::AskError
                | BuiltinType::LookupError
                | BuiltinType::RecvError
                | BuiltinType::LinkError
                | BuiltinType::MonitorError
                | BuiltinType::CloseError
                | BuiltinType::LocalPid
                | BuiltinType::HewActor => Some((ValueClass::BitCopy, CloneKind::Bits)),
                // `Option<i64>` / `Result<i64, string>` join their payloads.
                BuiltinType::Option => Some((ValueClass::BitCopy, CloneKind::Bits)),
                BuiltinType::Result => Some((ValueClass::CowValue, CloneKind::FieldWise)),
                // A collection's buffer is heap, so it is never `BitCopy`.
                BuiltinType::Vec
                | BuiltinType::HashSet
                | BuiltinType::JsonValue
                | BuiltinType::YamlValue => Some((ValueClass::CowValue, CloneKind::DeepCopy)),
                BuiltinType::HashMap | BuiltinType::HashMapIter | BuiltinType::VecIter => {
                    Some((ValueClass::CowValue, CloneKind::FieldWise))
                }
                BuiltinType::CrashInfo => Some((ValueClass::CowValue, CloneKind::FieldWise)),
                BuiltinType::CrashNotification => Some((ValueClass::BitCopy, CloneKind::Bits)),
                BuiltinType::Rc | BuiltinType::Weak | BuiltinType::LambdaPid => {
                    Some((ValueClass::AffineResource, CloneKind::Retain))
                }
                BuiltinType::Generator
                | BuiltinType::AsyncGenerator
                | BuiltinType::StreamPair
                | BuiltinType::BoxedActor
                | BuiltinType::Duplex
                | BuiltinType::Sink
                | BuiltinType::Stream
                | BuiltinType::Sender
                | BuiltinType::Receiver
                | BuiltinType::HewDuplex
                | BuiltinType::HewSendHalf
                | BuiltinType::HewRecvHalf
                | BuiltinType::SendHalf
                | BuiltinType::RecvHalf
                | BuiltinType::LambdaActorHandle
                | BuiltinType::MonitorRef
                | BuiltinType::CancellationToken => {
                    Some((ValueClass::AffineResource, CloneKind::None))
                }
                BuiltinType::Task => Some((ValueClass::AffineResource, CloneKind::None)),
                // Never the type of a value.
                BuiltinType::Iterator | BuiltinType::ActorState | BuiltinType::MachineState => None,
            };
            match expected {
                Some(expected) => assert_eq!(
                    Ok(expected),
                    crate::value_class::classify_ty(&ty, &context),
                    "class table row for builtin `{}`",
                    info.canonical_name
                ),
                None => assert_eq!(
                    Err(ClassError::NotAValueType { builtin: info.kind }),
                    crate::value_class::classify_ty(&ty, &context).map(|facts| facts.0),
                    "`{}` must be unreachable for values",
                    info.canonical_name
                ),
            }
        }
    }

    /// §1.1's marker correction, stated as the property it buys: `marker()`
    /// and the class table agree by construction, so no consumer can read one
    /// and get the other's answer.
    #[test]
    fn builtin_marker_and_the_class_table_agree() {
        use crate::builtin_type::BuiltinTypeMarker;

        let decls = declarations();
        let context = ClassContext::new(&decls);
        for info in builtin_types() {
            // Probe every generic builtin at `string`, not at a scalar. A row
            // whose class is fixed answers `BitCopy` either way, while a row
            // whose class is the aggregate or collection rule over its
            // arguments answers `CowValue` here and `BitCopy` at `i64`. Probing
            // at a scalar makes the two indistinguishable, which is what let
            // the `None` arm below state nothing.
            let ty = named(
                info.canonical_name,
                Some(info.kind),
                match info.arity {
                    0 => vec![],
                    1 => vec![ResolvedTy::String],
                    _ => vec![ResolvedTy::String, ResolvedTy::String],
                },
            );
            let classed = ValueClass::of_ty(&ty, &context);
            // RECORDED EXCEPTIONS - `LocalPid`, `HewActor` and `BoxedActor`.
            //
            // §1.1 gives both the BitCopy row and the class table above records
            // that verdict. Neither `marker()` can follow in this change:
            // `marker()` is the legacy lowering's input and the legacy route is
            // the parity oracle. Flipping `LocalPid` routes a
            // `Vec<LocalPid<_>>` element off its pointer ABI
            // (`hew-cli::run_e2e run_generic_vec_element_methods_roundtrip_ptr_abi`
            // panics "Vec layout-aware operation is not implemented") and moves
            // an elaborated-MIR baseline
            // (`hew-cli::funcupdate_mir_baselines funcupdate_reassign_elab_mir_matches_committed_baselines`).
            // `HewActor` and `BoxedActor` additionally carry
            // `close_method() = Some("close")`, which
            // `hew-hir/src/builtin_type_classes.rs` refuses on a BitCopy
            // builtin. All three flip at P5 with the legacy carrier (§9).
            //
            // The list is closed and explicit: every other builtin's marker and
            // class agree, the `None` arm below included, so this is a named
            // exception rather than a hole the table can grow into.
            if matches!(
                info.kind,
                BuiltinType::LocalPid | BuiltinType::HewActor | BuiltinType::BoxedActor
            ) {
                assert_eq!(BuiltinTypeMarker::Resource, info.kind.marker());
                continue;
            }
            // The other two rows this loop cannot speak for: `CrashInfo` and
            // `CrashNotification` are the two builtins whose class is the
            // Aggregate rule over a std declaration rather than a row of their
            // own, so their verdict is the declaration's and their marker
            // states nothing about it. They are classed here against this
            // module's stand-in declarations, not against `std/failure.hew`.
            if matches!(
                info.kind,
                BuiltinType::CrashInfo | BuiltinType::CrashNotification
            ) {
                assert_eq!(BuiltinTypeMarker::None, info.kind.marker());
                continue;
            }
            match info.kind.marker() {
                BuiltinTypeMarker::BitCopy => assert_eq!(
                    Ok(ValueClass::BitCopy),
                    classed,
                    "`{}` carries marker BitCopy",
                    info.canonical_name
                ),
                BuiltinTypeMarker::Resource => assert_eq!(
                    Ok(ValueClass::AffineResource),
                    classed,
                    "`{}` carries marker Resource",
                    info.canonical_name
                ),
                // `Linear` is carried only by the two compiler-internal payload
                // carriers, which are never the type of a value.
                BuiltinTypeMarker::Linear => assert_eq!(
                    Err(ClassError::NotAValueType { builtin: info.kind }),
                    classed,
                    "`{}` carries marker Linear",
                    info.canonical_name
                ),
                // `None` states no obligation of its own, so the class rule
                // decides. It must not decide `BitCopy`: a builtin whose class
                // is `BitCopy` carries marker `BitCopy`, and a `None` row that
                // classed `BitCopy` would be a marker correction this table
                // silently skipped.
                BuiltinTypeMarker::None => assert_ne!(
                    Ok(ValueClass::BitCopy),
                    classed,
                    "`{}` carries marker None but classes BitCopy: correct its marker",
                    info.canonical_name
                ),
            }
        }
    }

    /// The four cases the §10 exit gate names by hand.
    #[test]
    fn exit_gate_aggregate_cases_join_through_their_elements() {
        assert_eq!(
            (ValueClass::AffineResource, CloneKind::None),
            facts(&vec_of(conn())),
            "Vec<Conn>"
        );
        assert_eq!(
            (ValueClass::AffineResource, CloneKind::FieldWise),
            facts(&vec_of(rc(ResolvedTy::I64))),
            "Vec<Rc<i64>>"
        );
        assert_eq!(
            (ValueClass::CowValue, CloneKind::FieldWise),
            facts(&closure_over(vec![ResolvedTy::I64])),
            "closure capturing an i64"
        );
        assert_eq!(
            (ValueClass::AffineResource, CloneKind::None),
            facts(&closure_over(vec![conn()])),
            "closure capturing a Conn"
        );
    }

    /// The counterfactual for the two cases above that fail if the Aggregate
    /// rule is not applied through the element and capture classes.
    #[test]
    fn a_flat_collection_or_closure_row_would_be_wrong() {
        assert_ne!(
            (ValueClass::CowValue, CloneKind::DeepCopy),
            facts(&vec_of(conn())),
            "a flat `Vec` row would class Vec<Conn> CowValue/DeepCopy"
        );
        assert_ne!(
            (ValueClass::PersistentShare, CloneKind::Retain),
            facts(&closure_over(vec![conn()])),
            "a flat `Closure` row would class a Conn-capturing closure PersistentShare"
        );
    }

    /// The class rule refuses rather than guessing when it has no declaration.
    #[test]
    fn an_unknown_declaration_is_refused_not_defaulted() {
        let context = ClassContext::empty();
        assert_eq!(
            Err(ClassError::UnknownDeclaration {
                name: "Conn".to_string()
            }),
            ValueClass::of_ty(&conn(), &context)
        );
    }

    /// §6.2's own test sentence: every one of these has no `NominalId`, so a
    /// nominal key could not express them; the structural key hits for all
    /// five.
    #[test]
    fn type_facts_lookups_hit_for_five_non_nominal_types() {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        let instances = vec![
            ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
            ResolvedTy::Array(Box::new(conn()), 3),
            ResolvedTy::TraitObject {
                traits: vec![ResolvedTraitBound {
                    trait_name: "Show".to_string(),
                    args: vec![],
                    assoc_bindings: vec![],
                }],
            },
            closure_over(vec![ResolvedTy::I64]),
            ResolvedTy::Task(Box::new(ResolvedTy::I64)),
        ];

        let mut table = std::collections::BTreeMap::new();
        for ty in &instances {
            // None of the five produces a nominal instance, which is the whole
            // argument for the structural key.
            assert!(ty.nominal_instance().is_none(), "`{ty:?}` has no NominalId");
            let facts = TypeFacts::of_type(ty, &context, SendFact::Known(true), false, false)
                .expect("§1.1 classes every one of these");
            table.insert(TypeInstanceKey(ty.clone()), facts);
        }

        for ty in &instances {
            assert!(
                table.contains_key(&TypeInstanceKey(ty.clone())),
                "structural lookup for `{ty:?}`"
            );
        }
    }

    /// §6.3: `send` is unrepresentable as a plain `bool` for a `Closure` key.
    #[test]
    fn a_closure_row_defers_its_send_fact() {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        let closure = closure_over(vec![ResolvedTy::I64]);
        // Even when the caller hands in a decided fact, the closure row refuses
        // it: the mode-agnostic answer is wrong for a `BorrowMut` capture.
        let facts = TypeFacts::of_type(&closure, &context, SendFact::Known(true), false, false)
            .expect("a closure has a class");
        assert_eq!(SendFact::DeferredToClosureFacts, facts.send);

        // A consumer cannot read a `bool` out of it: the only way to a yes/no
        // is an explicit match that must name the deferral arm.
        let read_as_bool = match facts.send {
            SendFact::Known(value) => Some(value),
            SendFact::DeferredToClosureFacts => None,
        };
        assert_eq!(None, read_as_bool);
    }

    /// An ordinary row keeps the decided fact, so the deferral is not a
    /// blanket refusal.
    #[test]
    fn an_ordinary_row_keeps_a_known_send_fact() {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        let facts = TypeFacts::of_type(
            &ResolvedTy::String,
            &context,
            SendFact::Known(true),
            false,
            true,
        )
        .expect("a string has a class");
        assert_eq!(SendFact::Known(true), facts.send);
    }

    #[test]
    fn authored_and_later_structural_rows_share_capability_construction() {
        let closure = closure_over(vec![ResolvedTy::I64]);
        let deferred_tuple = ResolvedTy::Tuple(vec![ResolvedTy::I64, closure.clone()]);
        let clone_none_tuple =
            ResolvedTy::Tuple(vec![ResolvedTy::CancellationToken, ResolvedTy::String]);

        let mut authored = TypeFactService::new(TypeFactContext::default(), BTreeMap::new());
        let authored_deferred = authored
            .require(&deferred_tuple)
            .expect("checker-order tuple facts");
        let authored_clone_none = authored
            .require(&clone_none_tuple)
            .expect("checker-order tuple facts");

        let mut specialized = TypeFactService::new(TypeFactContext::default(), BTreeMap::new());
        for component in [
            ResolvedTy::I64,
            closure,
            ResolvedTy::CancellationToken,
            ResolvedTy::String,
        ] {
            specialized
                .require(&component)
                .expect("specialization component facts");
        }
        let specialized_deferred = specialized
            .require(&deferred_tuple)
            .expect("specialized tuple facts");
        let specialized_clone_none = specialized
            .require(&clone_none_tuple)
            .expect("specialized tuple facts");

        assert_eq!(authored_deferred, specialized_deferred);
        assert_eq!(SendFact::DeferredToClosureFacts, authored_deferred.send);
        assert_eq!(authored_clone_none, specialized_clone_none);
        assert_eq!(CloneKind::None, authored_clone_none.clone);
    }

    /// A trait object's send fact is not the bound list yet: §6.3 makes that
    /// reading sound only through §1.1's `CoerceToDynTrait` wall, which has not
    /// landed, so the row publishes the fail-closed verdict rather than one a
    /// consumer could act on unsoundly.
    #[test]
    fn a_trait_object_row_does_not_publish_a_wall_less_send_fact() {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        let dyn_show = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "Show".to_string(),
                args: vec![],
                assoc_bindings: vec![],
            }],
        };
        let facts = TypeFacts::of_type(&dyn_show, &context, SendFact::Known(true), false, false)
            .expect("a trait object has a class");
        assert_eq!(SendFact::Known(false), facts.send);
    }

    /// Declarations that reach themselves, for the recursion cases below.
    ///
    /// `Tree` is the shape of the shipped `indirect enum` fixture and `ResTree`
    /// is the same shape over a resource payload. `Pair<T>` mentions one fixed
    /// instantiation of itself, so the walk reaches the declaration at an
    /// instantiation it did not enter.
    fn recursive_declarations() -> BTreeMap<String, DeclaredType> {
        let mut decls = declarations();
        decls.insert(
            "Tree".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![
                    ResolvedTy::I64,
                    named("Tree", None, vec![]),
                    named("Tree", None, vec![]),
                ],
            },
        );
        decls.insert(
            "ResTree".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![conn(), named("ResTree", None, vec![])],
            },
        );
        decls.insert(
            "Pair".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec!["T".to_string()],
                members: vec![
                    ResolvedTy::TypeParam {
                        name: "T".to_string(),
                    },
                    named("Pair", None, vec![conn()]),
                ],
            },
        );
        decls.insert(
            "Wrapper".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec!["T".to_string()],
                members: vec![ResolvedTy::TypeParam {
                    name: "T".to_string(),
                }],
            },
        );
        decls.insert(
            "Outer".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec!["T".to_string()],
                members: vec![named(
                    "Wrapper",
                    None,
                    vec![ResolvedTy::TypeParam {
                        name: "T".to_string(),
                    }],
                )],
            },
        );
        decls
    }

    /// §1.1's indirect-enum row: the recursive occurrence is an owning edge,
    /// so the declaration keeps its payload class and clones field-wise. The
    /// payload here is a scalar, and the answer is still `CowValue` because the
    /// recursion is only legal behind a heap box.
    #[test]
    fn a_self_recursive_declaration_keeps_its_payload_class_over_an_owning_edge() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        assert_eq!(
            Ok((ValueClass::CowValue, CloneKind::FieldWise)),
            crate::value_class::classify_ty(&named("Tree", None, vec![]), &context)
        );
    }

    /// The negative control for the row above, stated as the class the
    /// bottom-element cut used to publish: §1.2 gives `BitCopy` no owner, §1.3
    /// lets `copy_value` duplicate it at `clone == Bits`, and §2.1 bit-copies
    /// it across an actor heap, so a heap-boxed payload must never carry it.
    #[test]
    fn a_self_recursive_declaration_is_never_bit_copyable() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        let facts = crate::value_class::classify_ty(&named("Tree", None, vec![]), &context)
            .expect("an indirect enum has a class");
        assert_ne!((ValueClass::BitCopy, CloneKind::Bits), facts);
        assert_eq!(OwnershipObligation::Owned, obligation_of(facts.0));
    }

    /// The payload decides which owning class: a recursive declaration holding
    /// a resource is `AffineResource`, not the box's own `CowValue`, and its
    /// clone column collapses to `None` because the resource has no clone.
    #[test]
    fn a_recursive_declaration_over_a_resource_payload_is_affine() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        assert_eq!(
            Ok((ValueClass::AffineResource, CloneKind::None)),
            crate::value_class::classify_ty(&named("ResTree", None, vec![]), &context)
        );
    }

    /// The counterfactual for the rows above: a declaration whose members do
    /// not reach it takes no owning edge, so the heap floor is about the cycle
    /// and not about user declarations in general.
    #[test]
    fn a_non_recursive_declaration_over_the_same_members_still_classes() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        assert_eq!(
            Ok((ValueClass::AffineResource, CloneKind::None)),
            crate::value_class::classify_ty(&conn(), &context)
        );
        assert_eq!(
            Ok((ValueClass::BitCopy, CloneKind::Bits)),
            crate::value_class::classify_ty(&named("Point", None, vec![]), &context)
        );
    }

    /// A constant argument never grows the instantiation: `Pair<T>` mentions
    /// `Pair<Conn>`, and substituting `T` cannot change `Conn`, so every turn
    /// of that cycle reaches the same `Pair<Conn>` and the walk is finite. Both
    /// instantiations class, and both carry the `Conn`'s obligation - the
    /// entered `Pair<Conn>` directly, and `Pair<i64>` through the `Pair<Conn>`
    /// it holds.
    #[test]
    fn a_cycle_carrying_only_constant_arguments_classes_at_every_instantiation() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        for argument in [conn(), ResolvedTy::I64] {
            assert_eq!(
                Ok((ValueClass::AffineResource, CloneKind::None)),
                crate::value_class::classify_ty(&named("Pair", None, vec![argument]), &context)
            );
        }
    }

    /// The same rule one declaration apart: `Holder<T>` reaches itself through
    /// the non-generic `Expr`, which mentions `Holder<i64>`. Neither edge grows,
    /// because `Expr` has no parameters for `i64` to mention, so both class over
    /// the owning edge rather than refusing.
    #[test]
    fn a_cycle_through_a_constant_instantiation_of_another_declaration_classes() {
        let mut decls = recursive_declarations();
        decls.insert(
            "Holder".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec!["T".to_string()],
                members: vec![named("Expr", None, vec![])],
            },
        );
        decls.insert(
            "Expr".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![named("Holder", None, vec![ResolvedTy::I64])],
            },
        );
        let context = ClassContext::new(&decls);
        assert_eq!(
            Ok((ValueClass::CowValue, CloneKind::FieldWise)),
            crate::value_class::classify_ty(
                &named("Holder", None, vec![ResolvedTy::I64]),
                &context
            )
        );
        assert_eq!(
            Ok((ValueClass::CowValue, CloneKind::FieldWise)),
            crate::value_class::classify_ty(&named("Expr", None, vec![]), &context)
        );
    }

    /// The negative control the rule above must not swallow: `Deep<T>` wraps
    /// its own parameter on the way round, so each turn is a strictly larger
    /// type and the walk has no finite fixpoint.
    #[test]
    fn a_cycle_that_wraps_its_own_parameter_still_refuses() {
        let mut decls = recursive_declarations();
        decls.insert(
            "Deep".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec!["T".to_string()],
                members: vec![named(
                    "Deep",
                    None,
                    vec![ResolvedTy::Named {
                        name: "Vec".to_string(),
                        args: vec![ResolvedTy::TypeParam {
                            name: "T".to_string(),
                        }],
                        builtin: Some(BuiltinType::Vec),
                        is_opaque: false,
                    }],
                )],
            },
        );
        let context = ClassContext::new(&decls);
        assert_eq!(
            Err(ClassError::RecursiveInstantiation {
                name: "Deep".to_string()
            }),
            crate::value_class::classify_ty(&named("Deep", None, vec![ResolvedTy::I64]), &context)
        );
    }

    /// The counterfactual for the refusal above: a declaration whose members
    /// never mention it refuses nothing, however deeply a caller nests it in
    /// its own argument. `Wrapper<Wrapper<i64>>` is the aggregate over one
    /// `Wrapper<i64>` field, which is the aggregate over one `i64`.
    #[test]
    fn a_declaration_nested_in_its_own_argument_is_not_recursion() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        let inner = named("Wrapper", None, vec![ResolvedTy::I64]);
        assert_eq!(
            Ok((ValueClass::BitCopy, CloneKind::Bits)),
            crate::value_class::classify_ty(&named("Wrapper", None, vec![inner]), &context)
        );
    }

    /// The same rule one declaration deeper: `Wrapper<Outer<i64>>` reaches
    /// `Wrapper<i64>` through `Outer`'s member. Neither declaration mentions
    /// itself, so the walk is finite and no name on it refuses.
    #[test]
    fn a_declaration_reached_transitively_at_a_second_instantiation_is_not_recursion() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        let outer = named("Outer", None, vec![ResolvedTy::I64]);
        assert_eq!(
            Ok((ValueClass::BitCopy, CloneKind::Bits)),
            crate::value_class::classify_ty(&named("Wrapper", None, vec![outer]), &context)
        );
    }

    /// Mutual polymorphic recursion: neither `Grow` nor `Relay` mentions itself
    /// directly, but `Grow<T>` reaches itself as `Grow<Vec<T>>` through
    /// `Relay`, so the argument grows on every turn and both refuse. Without
    /// the reachability walk this pair has no finite member tree and no
    /// refusal, which is a stack overflow rather than a wrong answer.
    #[test]
    fn mutual_recursion_that_grows_its_argument_refuses() {
        let mut decls = recursive_declarations();
        decls.insert(
            "Grow".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec!["T".to_string()],
                members: vec![named(
                    "Relay",
                    None,
                    vec![ResolvedTy::Named {
                        name: "Vec".to_string(),
                        args: vec![ResolvedTy::TypeParam {
                            name: "T".to_string(),
                        }],
                        builtin: Some(BuiltinType::Vec),
                        is_opaque: false,
                    }],
                )],
            },
        );
        decls.insert(
            "Relay".to_string(),
            DeclaredType {
                builtin: None,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec!["U".to_string()],
                members: vec![named(
                    "Grow",
                    None,
                    vec![ResolvedTy::TypeParam {
                        name: "U".to_string(),
                    }],
                )],
            },
        );
        let context = ClassContext::new(&decls);
        assert_eq!(
            Err(ClassError::RecursiveInstantiation {
                name: "Grow".to_string()
            }),
            crate::value_class::classify_ty(&named("Grow", None, vec![ResolvedTy::I64]), &context)
        );
    }

    /// A classed recursive type publishes a row like any other aggregate: the
    /// missing-key contract is for types §1.1 refuses, and a legal `indirect`
    /// enum is not one of them.
    #[test]
    fn a_classed_recursive_type_publishes_its_row() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        let facts = TypeFacts::of_type(
            &named("Tree", None, vec![]),
            &context,
            SendFact::Known(true),
            false,
            false,
        )
        .expect("an indirect enum publishes a row");
        assert_eq!(ValueClass::CowValue, facts.class);
        assert_eq!(CloneKind::FieldWise, facts.clone);
    }

    /// `builtin` is the identity fact, and a `Named` carrying none that the
    /// context holds no declaration for is refused in every context. Reading
    /// the name against the builtin table instead gave a user declaration the
    /// builtin's class whenever the two names collided.
    #[test]
    fn a_named_type_with_no_discriminator_and_no_declaration_refuses() {
        let context = ClassContext::empty();
        for name in ["Location", "Handle", "Range", "Trap", "Sender"] {
            assert_eq!(
                Err(ClassError::UnknownDeclaration {
                    name: name.to_string()
                }),
                crate::value_class::classify_ty(&named(name, None, vec![]), &context),
                "`{name}` carries no builtin discriminator"
            );
        }
    }

    /// The refusal propagates through a container: `Vec<Location>` has no class
    /// in the empty context either, so a consumer cannot reach a guessed
    /// element class through the collection floor.
    #[test]
    fn a_container_over_an_undecidable_element_refuses() {
        let context = ClassContext::empty();
        let element = named("Location", None, vec![]);
        assert_eq!(
            Err(ClassError::UnknownDeclaration {
                name: "Location".to_string()
            }),
            crate::value_class::classify_ty(
                &ResolvedTy::Named {
                    name: "Vec".to_string(),
                    args: vec![element],
                    builtin: Some(BuiltinType::Vec),
                    is_opaque: false,
                },
                &context
            )
        );
    }

    /// The counterfactual: the same three builtin names decide immediately when
    /// they carry the discriminator, so the refusal above is about the missing
    /// identity fact and not about the names.
    #[test]
    fn the_same_builtin_names_decide_when_they_carry_the_discriminator() {
        let context = ClassContext::empty();
        for (name, builtin) in [("Range", BuiltinType::Range), ("Trap", BuiltinType::Trap)] {
            assert_eq!(
                Ok((ValueClass::BitCopy, CloneKind::Bits)),
                crate::value_class::classify_ty(&named(name, Some(builtin), vec![]), &context)
            );
        }
        assert_eq!(
            Ok((ValueClass::AffineResource, CloneKind::None)),
            crate::value_class::classify_ty(
                &named(
                    "Sender",
                    Some(BuiltinType::Sender),
                    vec![ResolvedTy::String]
                ),
                &context
            )
        );
    }

    #[test]
    fn selected_capability_never_derives_after_losing_impl_metadata() {
        let parsed = hew_parser::parse(
            "type Key { id: i64 } impl Hash for Key { fn hash(self) -> i64 { 1 } }",
        );
        let mut checker = crate::Checker::new(crate::module_registry::ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parsed.program);
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        let mut service = TypeFactService::new(output.type_fact_context, output.type_facts);
        let key = ResolvedTy::named_user("Key", vec![]);
        assert!(matches!(
            service
                .capability_plan(&key, super::ValueCapability::Hash)
                .unwrap()
                .map(|selection| selection.plan().clone()),
            Some(super::ValueMethodPlan::User { .. })
        ));
        service.context.method_binders.clear();
        assert!(service
            .capability_plan(&key, super::ValueCapability::Hash)
            .is_err());
        assert_eq!(
            service
                .capability_plan(&key, super::ValueCapability::Eq)
                .unwrap()
                .map(|selection| selection.plan().clone()),
            Some(super::ValueMethodPlan::Derived)
        );
    }

    /// What §1.2 owes for a class, as this module's tests read it.
    #[derive(Debug, PartialEq, Eq)]
    enum OwnershipObligation {
        None,
        Owned,
    }

    /// §1.2's kind table, restated locally so the assertion above is about the
    /// obligation and not about a spelling.
    fn obligation_of(class: ValueClass) -> OwnershipObligation {
        match class {
            ValueClass::BitCopy | ValueClass::View => OwnershipObligation::None,
            ValueClass::CowValue
            | ValueClass::PersistentShare
            | ValueClass::AffineResource
            | ValueClass::Linear => OwnershipObligation::Owned,
        }
    }
}
