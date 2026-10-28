//! Capability facts on the class table.
//!
//! The checker decides once per substituted type and publishes
//! `TypeCheckOutput.type_facts`, keyed by [`TypeInstanceKey`] (the structural key). SIR rules
//! 5 and 6, `TargetLayout`, glue emission and collection descriptors all read
//! this table; nothing downstream re-asks.

use crate::resolved_ty::ResolvedTy;
use std::collections::{BTreeMap, HashMap, HashSet};

use crate::check::TypeDef;
use crate::traits::{MarkerTrait, TraitRegistry};
use crate::value_class::{ClassContext, ClassError, DeclaredType, ValueClass};
use crate::BuiltinType;

/// What a `copy_value` of this type costs, per the class table's `clone` column.
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
/// **The wrong reading is made unrepresentable** (the type-fact contract): this is not a `bool`,
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
    /// An inferred generator requires a concrete producer-frame proof.
    DeferredToGeneratorFacts,
    /// A `Closure` row: the fact lives in `closure_facts`, keyed per closure
    /// expression rather than per type.
    DeferredToClosureFacts,
}

/// The key of every per-type fact table.
///
/// **Structural, not nominal** (the structural key). The nominal spelling
/// `{ template: NominalId, type_args }` is withdrawn: `nominal_instance()`
/// represents source record instances, so `Tuple`, `Array`,
/// `Slice`, `Function`, `Closure`, `Pointer`, `Borrow`, `TraitObject` and
/// `Task` would have no key at all while the class table classifies every one of them.
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

impl std::borrow::Borrow<ResolvedTy> for TypeInstanceKey {
    fn borrow(&self) -> &ResolvedTy {
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
    pub partial_ord: bool,
}

/// Independently selected value operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueCapability {
    Hash,
    Eq,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl ValueCapability {
    #[must_use]
    pub const fn is_ordering(self) -> bool {
        matches!(
            self,
            Self::Less | Self::LessEqual | Self::Greater | Self::GreaterEqual
        )
    }

    #[must_use]
    pub const fn component_capabilities(self) -> &'static [Self] {
        match self {
            Self::Hash => &[Self::Hash],
            Self::Eq => &[Self::Eq],
            Self::Less | Self::LessEqual | Self::Greater | Self::GreaterEqual => &[
                Self::Less,
                Self::LessEqual,
                Self::Greater,
                Self::GreaterEqual,
            ],
        }
    }
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

type CapabilityResult = Result<Option<ValueMethodPlan>, ClassError>;

/// A Display operation implemented directly by the compiler's runtime ABI.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinDisplayPlan {
    Identity,
    Runtime {
        family: crate::RuntimeCallFamily,
        argument_ty: ResolvedTy,
    },
}

impl BuiltinDisplayPlan {
    /// The scalar conversion also used when rendering assertion operands.
    #[must_use]
    pub fn scalar(ty: &ResolvedTy) -> Option<Self> {
        use crate::RuntimeCallFamily as F;
        let (family, argument_ty) = match ty {
            ResolvedTy::I8 | ResolvedTy::I16 | ResolvedTy::I32 => (F::I32ToString, ResolvedTy::I32),
            ResolvedTy::I64 | ResolvedTy::Isize => (F::I64ToString, ResolvedTy::I64),
            ResolvedTy::U8 => (F::U8ToString, ResolvedTy::U8),
            ResolvedTy::U16 | ResolvedTy::U32 => (F::U32ToString, ResolvedTy::U32),
            ResolvedTy::U64 | ResolvedTy::Usize => (F::U64ToString, ResolvedTy::U64),
            ResolvedTy::F32 | ResolvedTy::F64 => (F::F64ToString, ResolvedTy::F64),
            ResolvedTy::Bool => (F::BoolToString, ResolvedTy::Bool),
            ResolvedTy::Char => (F::CharToString, ResolvedTy::Char),
            _ => return None,
        };
        Some(Self::Runtime {
            family,
            argument_ty,
        })
    }

    fn for_type(ty: &ResolvedTy) -> Option<Self> {
        let family = match ty {
            ResolvedTy::String => return Some(Self::Identity),
            ResolvedTy::Named {
                head: crate::TypeHead::Builtin(BuiltinType::NodeId),
                ..
            } => crate::RuntimeCallFamily::NodeIdDisplay,
            ResolvedTy::Named {
                head: crate::TypeHead::Builtin(BuiltinType::Location),
                ..
            } => crate::RuntimeCallFamily::LocationDisplay,
            _ => return Self::scalar(ty),
        };
        Some(Self::Runtime {
            family,
            argument_ty: ty.clone(),
        })
    }

    #[must_use]
    pub fn can_fail(&self) -> bool {
        match self {
            Self::Identity => false,
            Self::Runtime { family, .. } => family
                .semantic_contract()
                .is_none_or(|contract| !contract.failures.is_empty()),
        }
    }
}

type CapabilityWalk = crate::capability_walk::CapabilityWalk<CapabilityResult>;

type DisplayResult = Result<Option<(crate::DefId, Vec<ResolvedTy>)>, ClassError>;

#[derive(Default)]
struct DisplayWalk {
    states: HashMap<(ResolvedTy, ResolvedTy), DisplayState>,
}

enum DisplayState {
    Visiting,
    Complete(DisplayResult),
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
    calls: Vec<(crate::DefId, Vec<ResolvedTy>)>,
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

    /// User bodies selected by this operation, including structural members.
    #[must_use]
    pub fn calls(&self) -> &[(crate::DefId, Vec<ResolvedTy>)] {
        &self.calls
    }
}

/// Registration-time receiver and binder order, keyed by the selected `DefId`.
#[derive(Debug, Clone)]
pub(crate) struct ImplMethodBinders {
    pub receiver: crate::Ty,
    pub impl_params: Vec<crate::ParamHead>,
    pub method_params: Vec<crate::ParamHead>,
    pub obligations: Vec<(crate::TypeParamId, ImplMethodObligation)>,
    pub checked_receivers: HashSet<ResolvedTy>,
    pub implemented_trait: Option<crate::check::TraitRef>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ImplMethodObligation {
    Marker(MarkerTrait),
    Display,
    Serializable,
    Checked,
}

impl ImplMethodBinders {
    pub(crate) fn source_parameters(&self) -> Vec<crate::ParamHead> {
        self.impl_params
            .iter()
            .chain(&self.method_params)
            .copied()
            .collect()
    }

    pub(crate) fn implementation_arguments(
        &self,
        defs: &crate::DefTable,
        method: crate::DefId,
        receiver: &ResolvedTy,
        trait_args: &[crate::Ty],
    ) -> Option<Vec<ResolvedTy>> {
        let arguments = self.receiver_arguments(defs, method, receiver).ok()?;
        let substitutions = self
            .impl_params
            .iter()
            .copied()
            .zip(arguments.iter().map(ResolvedTy::to_ty))
            .collect();
        if let Some(bound) = &self.implemented_trait {
            let implemented: Vec<_> = bound
                .args
                .iter()
                .map(|ty| ty.substitute_type_params_parallel(&substitutions))
                .collect();
            if implemented != trait_args {
                return None;
            }
        }
        Some(arguments)
    }

    #[cfg(test)]
    fn instantiate(
        &self,
        defs: &crate::DefTable,
        method: crate::DefId,
        receiver: &ResolvedTy,
        registry: &TraitRegistry,
    ) -> Result<Vec<ResolvedTy>, ClassError> {
        self.instantiate_with(defs, method, receiver, registry, &mut |_, _| Ok(false))
    }

    /// Instantiate the impl binders from `receiver`, deciding each bound the
    /// registry cannot answer (`Display`, `Serializable`) through `decide`.
    fn instantiate_with(
        &self,
        defs: &crate::DefTable,
        method: crate::DefId,
        receiver: &ResolvedTy,
        registry: &TraitRegistry,
        decide: &mut dyn FnMut(&ResolvedTy, ImplMethodObligation) -> Result<bool, ClassError>,
    ) -> Result<Vec<ResolvedTy>, ClassError> {
        let type_args = self.arguments(defs, method, receiver)?;
        if self.checked_receivers.contains(receiver) {
            return Ok(type_args);
        }
        let refusal = || ClassError::UnknownDeclaration {
            name: defs.display(method).to_string(),
        };
        for (param, obligation) in &self.obligations {
            let position = self
                .impl_params
                .iter()
                .position(|name| name.id == *param)
                .ok_or_else(refusal)?;
            let satisfied = match obligation {
                ImplMethodObligation::Marker(marker) => {
                    registry.implements_marker(&type_args[position].to_ty(), *marker)
                }
                ImplMethodObligation::Checked => false,
                other => decide(&type_args[position], *other)?,
            };
            if !satisfied {
                return Err(refusal());
            }
        }
        Ok(type_args)
    }
    pub(crate) fn arguments(
        &self,
        defs: &crate::DefTable,
        method: crate::DefId,
        receiver: &ResolvedTy,
    ) -> Result<Vec<ResolvedTy>, ClassError> {
        if let Some(name) = self.method_params.first() {
            return Err(ClassError::TypeParam { param: *name });
        }
        self.receiver_arguments(defs, method, receiver)
    }

    fn receiver_arguments(
        &self,
        defs: &crate::DefTable,
        method: crate::DefId,
        receiver: &ResolvedTy,
    ) -> Result<Vec<ResolvedTy>, ClassError> {
        self.bound_arguments(defs, method, receiver, None)
    }

    fn bound_arguments(
        &self,
        defs: &crate::DefTable,
        method: crate::DefId,
        receiver: &ResolvedTy,
        trait_arguments: Option<(&[crate::Ty], &[ResolvedTy])>,
    ) -> Result<Vec<ResolvedTy>, ClassError> {
        let variables: Vec<_> = self
            .impl_params
            .iter()
            .map(|_| crate::ty::TypeVar::fresh())
            .collect();
        let replacements: HashMap<_, _> = self
            .impl_params
            .iter()
            .copied()
            .zip(variables.iter().copied().map(crate::Ty::Var))
            .collect();
        let pattern = self.receiver.substitute_type_params_parallel(&replacements);
        let mut subst = crate::ty::Substitution::new();
        let receiver_ty = match receiver {
            ResolvedTy::Generator {
                yields, returns, ..
            } => crate::Ty::generator(yields.to_ty(), returns.to_ty()),
            other => other.to_ty(),
        };
        crate::unify::unify_exact(&mut subst, &pattern, &receiver_ty).map_err(|_| {
            ClassError::UnknownDeclaration {
                name: defs.display(method).to_string(),
            }
        })?;
        if let Some((patterns, arguments)) = trait_arguments {
            let refusal = || ClassError::UnknownDeclaration {
                name: defs.display(method).to_string(),
            };
            if patterns.len() != arguments.len() {
                return Err(refusal());
            }
            for (pattern, argument) in patterns.iter().zip(arguments) {
                let pattern = pattern.substitute_type_params_parallel(&replacements);
                crate::unify::unify_exact(&mut subst, &pattern, &argument.to_ty())
                    .map_err(|_| refusal())?;
            }
        }
        let type_args: Vec<_> = self
            .impl_params
            .iter()
            .zip(variables)
            .map(|(name, var)| {
                let argument = subst.resolve(&crate::Ty::Var(var));
                // An impl binder is instantiated by a component of the
                // concrete receiver or applied trait. Preserve that boundary type,
                // including opacity inside nested arguments, rather than
                // reconstructing it through the lossy Ty round trip.
                let mut components = vec![receiver.clone()];
                if let Some((_, arguments)) = trait_arguments {
                    components.extend_from_slice(arguments);
                }
                while let Some(component) = components.pop() {
                    if component.to_ty() == argument {
                        return Ok(component);
                    }
                    push_type_components(&component, &mut components);
                }
                Err(ClassError::TypeParam { param: *name })
            })
            .collect::<Result<_, _>>()?;
        Ok(type_args)
    }
}

fn require_concrete_capability_type(ty: &ResolvedTy) -> Result<(), ClassError> {
    if let ResolvedTy::TypeParam { name } = ty {
        return Err(ClassError::TypeParam { param: *name });
    }
    let mut components = Vec::new();
    push_type_components(ty, &mut components);
    for component in components {
        require_concrete_capability_type(&component)?;
    }
    Ok(())
}

/// Shared exact-specialization-then-nominal lookup over checker registration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum ImplMethodSlot {
    Declared(crate::DefId),
    Value(ValueCapability),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ImplMethodKey {
    receiver: ResolvedTy,
    arguments: Option<Vec<ResolvedTy>>,
    method: ImplMethodSlot,
}

impl ImplMethodKey {
    pub(crate) fn new(receiver: &ResolvedTy, method: ImplMethodSlot, generic: bool) -> Self {
        let (receiver, arguments) = match receiver {
            ResolvedTy::Named { head, args, .. } => (
                ResolvedTy::Named {
                    head: *head,
                    args: Vec::new(),
                    is_opaque: false,
                },
                (!generic).then(|| args.clone()),
            ),
            receiver => (receiver.clone(), (!generic).then(Vec::new)),
        };
        Self {
            receiver,
            arguments,
            method,
        }
    }
}

pub(crate) fn selected_impl_method(
    ids: &HashMap<ImplMethodKey, crate::DefId>,
    receiver: &ResolvedTy,
    method: ImplMethodSlot,
) -> Option<crate::DefId> {
    ids.get(&ImplMethodKey::new(receiver, method, false))
        .or_else(|| ids.get(&ImplMethodKey::new(receiver, method, true)))
        .copied()
}

fn is_authored_display_method(defs: &crate::DefTable, method: Option<crate::DefId>) -> bool {
    method
        .and_then(|method| defs.module(method))
        .is_some_and(|module| {
            defs.module_package(module) != &crate::def_table::PackageOrigin::StandardLibrary
        })
}

pub(crate) fn select_builtin_display_plan(
    ty: &ResolvedTy,
    defs: &crate::DefTable,
    methods: &HashMap<ImplMethodKey, crate::DefId>,
    display_method: Option<crate::DefId>,
) -> Option<BuiltinDisplayPlan> {
    let plan = BuiltinDisplayPlan::for_type(ty)?;
    let selected = display_method
        .and_then(|method| selected_impl_method(methods, ty, ImplMethodSlot::Declared(method)));
    (!is_authored_display_method(defs, selected)).then_some(plan)
}

/// Checker-owned declaration context for concrete fact expansion.
///
/// This is an immutable snapshot of the declarations and capability authority
/// the checker used. Keeping both here lets later concrete specialization ask
/// the same service instead of reconstructing marker or collection eligibility
/// rules in an IR crate.
#[derive(Debug, Clone, Default)]
pub struct TypeFactContext {
    declarations: BTreeMap<crate::NominalId, DeclaredType>,
    associated_types: crate::check::AssociatedTypeSchemas,
    registry: TraitRegistry,
    type_defs: HashMap<crate::NominalId, TypeDef>,
    method_ids: HashMap<ImplMethodKey, crate::DefId>,
    method_binders: HashMap<crate::DefId, ImplMethodBinders>,
    /// Applied supertrait patterns from the checker's substitution authority.
    method_trait_applications: HashMap<crate::DefId, Vec<crate::check::TraitRef>>,
    /// The declaration table every `DefId` above indexes.
    defs: std::sync::Arc<crate::DefTable>,
    display_method: Option<crate::DefId>,
    aliases: HashMap<crate::DefId, crate::check::TypeAliasDef>,
    rendering_members: HashMap<crate::NominalId, RenderingMembers>,
    /// Text keys, tags and flags of every data record and enum.
    serial_layouts: HashMap<crate::NominalId, crate::data_shape::SerialLayout>,
}

/// Source type identities before storage normalization expands aliases.
#[derive(Debug, Clone)]
pub(crate) struct RenderingMembers {
    pub type_params: Vec<crate::ParamHead>,
    pub fields: HashMap<String, crate::Ty>,
    pub variants: HashMap<String, crate::check::VariantDef>,
}

impl RenderingMembers {
    pub(crate) fn new(definition: &TypeDef, resolve: impl Fn(&crate::Ty) -> crate::Ty) -> Self {
        Self {
            type_params: definition.type_params.clone(),
            fields: definition
                .fields
                .iter()
                .map(|(name, ty)| (name.clone(), resolve(ty)))
                .collect(),
            variants: definition
                .variants
                .iter()
                .map(|(name, variant)| {
                    let variant = match variant {
                        crate::check::VariantDef::Unit => crate::check::VariantDef::Unit,
                        crate::check::VariantDef::Tuple(fields) => {
                            crate::check::VariantDef::Tuple(fields.iter().map(&resolve).collect())
                        }
                        crate::check::VariantDef::Struct(fields) => {
                            crate::check::VariantDef::Struct(
                                fields
                                    .iter()
                                    .map(|(name, ty)| (name.clone(), resolve(ty)))
                                    .collect(),
                            )
                        }
                    };
                    (name.clone(), variant)
                })
                .collect(),
        }
    }
}

impl TypeFactContext {
    /// Bind an implementation's receiver and match its checked trait arguments.
    #[must_use]
    pub fn implementation_arguments(
        &self,
        method: crate::DefId,
        receiver: &ResolvedTy,
        trait_args: &[crate::Ty],
    ) -> Option<Vec<ResolvedTy>> {
        self.method_binders
            .get(&method)?
            .implementation_arguments(&self.defs, method, receiver, trait_args)
    }

    /// Authored impl and method binders in the checked body's source order.
    /// Materialized defaults retain the original trait method's binder IDs.
    #[must_use]
    pub fn impl_method_type_parameters(
        &self,
        method: crate::DefId,
    ) -> Option<Vec<crate::ParamHead>> {
        self.method_binders
            .get(&method)
            .map(ImplMethodBinders::source_parameters)
    }

    /// Instantiate a selected trait implementation's body schema. Call-site
    /// arguments contain only authored method binders; associated projections
    /// are supplied by the checker's registered schema after receiver binding.
    ///
    /// # Errors
    /// Returns an error for absent binder metadata, incorrect method arity or
    /// receiver/projection arguments that cannot be resolved from checked facts.
    pub fn trait_method_instance_arguments(
        &self,
        method: crate::DefId,
        receiver: &ResolvedTy,
        trait_method: (crate::DefId, crate::DefId),
        trait_arguments: &[ResolvedTy],
        method_arguments: &[ResolvedTy],
        structural_witnesses: &[crate::check::StructuralWitness],
    ) -> Result<Vec<ResolvedTy>, String> {
        let binders = self.method_binders.get(&method).ok_or_else(|| {
            format!(
                "implementation `{}` has no checked binder schema",
                self.defs.path(method)
            )
        })?;
        if method_arguments.len() != binders.method_params.len() {
            return Err(format!(
                "requires {} checker-resolved method type argument(s), found {}",
                binders.method_params.len(),
                method_arguments.len()
            ));
        }
        let structurally_selected =
            receiver
                .impl_receiver_instance(&self.defs)
                .is_some_and(|receiver| {
                    structural_witnesses.iter().any(|witness| {
                        witness.self_type == receiver.nominal
                            && (witness.declaring_trait, witness.method) == trait_method
                            && witness.inherent == method
                    })
                });
        let mut arguments = if structurally_selected {
            binders
                .receiver_arguments(&self.defs, method, receiver)
                .map_err(|error| error.to_string())?
        } else {
            let mut candidates = self
                .method_trait_applications
                .get(&method)
                .into_iter()
                .flatten()
                .filter(|bound| bound.trait_id == trait_method.0)
                .filter_map(|bound| {
                    binders
                        .bound_arguments(
                            &self.defs,
                            method,
                            receiver,
                            Some((&bound.args, trait_arguments)),
                        )
                        .ok()
                });
            let arguments = candidates.next().ok_or_else(|| {
                format!(
                    "implementation `{}` cannot bind its receiver and applied trait arguments",
                    self.defs.path(method),
                )
            })?;
            if candidates.any(|candidate| candidate != arguments) {
                return Err(format!(
                    "implementation `{}` has ambiguous applied trait bindings",
                    self.defs.path(method)
                ));
            }
            arguments
        };
        arguments.extend_from_slice(method_arguments);
        let parameters = binders.source_parameters();
        self.associated_types
            .binders(&parameters)
            .into_iter()
            .map(|parameter| {
                self.instantiate_member(&ResolvedTy::param(parameter), &parameters, &arguments)
                    .map_err(|error| error.to_string())
            })
            .collect()
    }

    pub(crate) fn impl_method_arguments(
        &self,
        method: crate::DefId,
        receiver: &ResolvedTy,
    ) -> Option<Vec<ResolvedTy>> {
        self.method_binders
            .get(&method)?
            .arguments(&self.defs, method, receiver)
            .ok()
    }

    /// The type definitions, read by declaration.
    #[must_use]
    pub fn types(&self) -> crate::check::TypeDefView<'_> {
        crate::check::TypeDefView::new(&self.defs, &self.type_defs)
    }

    #[must_use]
    pub fn new(
        declarations: BTreeMap<crate::NominalId, DeclaredType>,
        registry: TraitRegistry,
        type_defs: HashMap<crate::NominalId, TypeDef>,
    ) -> Self {
        Self {
            declarations,
            associated_types: crate::check::AssociatedTypeSchemas::default(),
            registry,
            type_defs,
            method_ids: HashMap::new(),
            method_binders: HashMap::new(),
            method_trait_applications: HashMap::new(),
            defs: std::sync::Arc::default(),
            display_method: None,
            aliases: HashMap::new(),
            rendering_members: HashMap::new(),
            serial_layouts: HashMap::new(),
        }
    }

    pub(crate) fn with_associated_types(
        mut self,
        associated: crate::check::AssociatedTypeSchemas,
    ) -> Self {
        self.associated_types = associated;
        self
    }

    fn instantiate_member(
        &self,
        member: &ResolvedTy,
        parameters: &[crate::ParamHead],
        arguments: &[ResolvedTy],
    ) -> Result<ResolvedTy, ClassError> {
        let member = self
            .associated_types
            .instantiate_resolved_member(member, parameters, arguments, &self.defs)?;
        Ok(crate::check::restore_member_opacity(
            member,
            &self.defs,
            &|id| {
                self.declarations
                    .get(&id)
                    .is_some_and(|declaration| declaration.is_opaque)
            },
        ))
    }

    pub(crate) fn with_impl_methods(
        mut self,
        ids: HashMap<ImplMethodKey, crate::DefId>,
        binders: HashMap<crate::DefId, ImplMethodBinders>,
    ) -> Self {
        self.method_ids = ids;
        self.method_binders = binders;
        self
    }

    pub(crate) fn with_method_trait_applications(
        mut self,
        applications: HashMap<crate::DefId, Vec<crate::check::TraitRef>>,
    ) -> Self {
        self.method_trait_applications = applications;
        self
    }

    #[must_use]
    pub fn declarations(&self) -> &BTreeMap<crate::NominalId, DeclaredType> {
        &self.declarations
    }

    #[must_use]
    pub fn with_defs(mut self, defs: std::sync::Arc<crate::DefTable>) -> Self {
        self.defs = defs;
        self
    }

    /// The declaration table this context's identities index.
    #[must_use]
    pub fn defs(&self) -> &crate::DefTable {
        &self.defs
    }

    pub(crate) fn with_display_method(mut self, display: Option<crate::DefId>) -> Self {
        self.display_method = display;
        self
    }

    pub(crate) fn with_aliases(
        mut self,
        aliases: HashMap<crate::DefId, crate::check::TypeAliasDef>,
    ) -> Self {
        self.aliases = aliases;
        self
    }

    pub(crate) fn with_serial_layouts(
        mut self,
        layouts: HashMap<crate::NominalId, crate::data_shape::SerialLayout>,
    ) -> Self {
        self.serial_layouts = layouts;
        self
    }

    /// The serialization layout of a data record or enum.
    #[must_use]
    pub fn serial_layout(&self, id: crate::NominalId) -> Option<&crate::data_shape::SerialLayout> {
        self.serial_layouts.get(&id)
    }

    pub(crate) fn with_rendering_members(
        mut self,
        members: HashMap<crate::NominalId, RenderingMembers>,
    ) -> Self {
        self.rendering_members = members;
        self
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
    classes: std::cell::RefCell<crate::value_class::Walk>,
}

impl crate::value_class::ClassDeclarations for TypeFactContext {
    fn declared_type(&self, id: crate::NominalId) -> Option<std::borrow::Cow<'_, DeclaredType>> {
        self.declarations.get(&id).map(std::borrow::Cow::Borrowed)
    }

    fn builtin_declaration(&self, builtin: BuiltinType) -> Option<crate::NominalId> {
        crate::value_class::ClassDeclarations::builtin_declaration(&self.declarations, builtin)
    }

    fn parameter_source(&self, parameter: crate::ParamHead) -> crate::ParamHead {
        self.associated_types.parameter_source(parameter)
    }

    fn instantiate_members(
        &self,
        declaration: &DeclaredType,
        arguments: &[ResolvedTy],
    ) -> Result<Vec<ResolvedTy>, ClassError> {
        declaration
            .members
            .iter()
            .map(|member| self.instantiate_member(member, &declaration.type_params, arguments))
            .collect()
    }
}

impl TypeFactService {
    #[must_use]
    pub fn new(context: TypeFactContext, rows: BTreeMap<TypeInstanceKey, TypeFacts>) -> Self {
        Self {
            context,
            rows,
            classes: std::cell::RefCell::default(),
        }
    }

    /// Classify an exact type against this service's immutable declarations.
    ///
    /// # Errors
    /// Returns the class authority's refusal for unsupported types.
    pub fn classify(&self, ty: &ResolvedTy) -> Result<(ValueClass, CloneKind), ClassError> {
        if let Some(row) = self.rows.get(ty) {
            return Ok((row.class, row.clone));
        }
        self.classes
            .borrow_mut()
            .query(ty, &ClassContext::new(&self.context))
    }

    /// The declaration table the service's identities index.
    #[must_use]
    pub fn defs(&self) -> &crate::DefTable {
        &self.context.defs
    }

    #[must_use]
    pub fn rows(&self) -> &BTreeMap<TypeInstanceKey, TypeFacts> {
        &self.rows
    }

    #[must_use]
    pub fn into_rows(self) -> BTreeMap<TypeInstanceKey, TypeFacts> {
        self.rows
    }

    /// Return the unchanged declaration authority and the facts derived from it.
    ///
    /// The context's metadata is supplied at construction and stays immutable
    /// while queries retain derived facts. Returning both preserves that single
    /// owned snapshot without cloning its declarations and registries.
    #[must_use]
    pub fn into_parts(self) -> (TypeFactContext, BTreeMap<TypeInstanceKey, TypeFacts>) {
        (self.context, self.rows)
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
        let instance = ty.nominal_instance(&self.context.defs).ok_or_else(|| {
            format!(
                "`{}` is not a checker-resolved named record",
                ty.user_facing()
            )
        })?;
        if let Some(fields) = crate::range_value::fields(ty) {
            return Ok((instance, fields));
        }
        if let Some(fields) = crate::actor_protocol::interceptor_fields(&self.context.defs, ty) {
            return Ok((instance, fields));
        }
        let definition = self
            .context
            .type_defs
            .get(&instance.nominal)
            .ok_or_else(|| {
                format!(
                    "aggregate `{}` has no exact checker declaration",
                    ty.user_facing()
                )
            })?;
        let declaration = self
            .context
            .declarations
            .get(&instance.nominal)
            .ok_or_else(|| format!("aggregate `{}` has no declaration facts", ty.user_facing()))?;
        let positional =
            definition.kind == crate::check::TypeDefKind::Record && definition.fields.is_empty();
        if !matches!(
            definition.kind,
            crate::check::TypeDefKind::Struct | crate::check::TypeDefKind::Record
        ) || declaration.is_opaque
            || definition.field_order.len() != definition.fields.len()
            || (!positional && definition.field_order.len() != declaration.members.len())
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
        let field_names: Vec<String> = if positional {
            (0..declaration.members.len())
                .map(|index| index.to_string())
                .collect()
        } else {
            definition.field_order.clone()
        };
        let fields = field_names
            .into_iter()
            .zip(&declaration.members)
            .map(|(name, field)| {
                self.context
                    .instantiate_member(field, &definition.type_params, &instance.args)
                    .map(|field| (name, field))
                    .map_err(|error| error.to_string())
            })
            .collect::<Result<Vec<_>, _>>()?;
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
        if crate::range_value::fields(ty).is_some() {
            return Ok(crate::DeclarationMarker::None);
        }
        let instance = ty
            .nominal_instance(&self.context.defs)
            .ok_or_else(|| format!("`{}` has no nominal declaration", ty.user_facing()))?;
        let declaration = self
            .context
            .declarations
            .get(&instance.nominal)
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
        if let Some(row) = self.rows.get(ty) {
            return Ok(*row);
        }

        let mut components = Vec::new();
        push_type_components(ty, &mut components);
        for component in &components {
            self.require(component)?;
        }

        let row = self.contextual_facts(ty)?;
        self.rows.insert(TypeInstanceKey(ty.clone()), row);
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
        let mut calls = Vec::new();
        Ok(self
            .select_capability_inner(ty, capability, &mut CapabilityWalk::default(), &mut calls)?
            .map(|plan| ValueMethodSelection {
                ty: ty.clone(),
                capability,
                plan,
                calls,
            }))
    }

    /// Bind an exact inherent method's impl parameters from its receiver.
    ///
    /// # Errors
    /// Refuses missing registration facts or parameters not bound by self.
    pub fn inherent_method_type_args(
        &self,
        method: crate::DefId,
        receiver: &ResolvedTy,
    ) -> Result<Vec<ResolvedTy>, ClassError> {
        let mut display = DisplayWalk::default();
        self.context
            .method_binders
            .get(&method)
            .ok_or_else(|| ClassError::UnknownDeclaration {
                name: self.context.defs.display(method).to_string(),
            })?
            .instantiate_with(
                &self.context.defs,
                method,
                receiver,
                &self.context.registry,
                &mut |ty, bound| match bound {
                    ImplMethodObligation::Serializable => {
                        Ok(self.is_serializable(ty, &|_, _| false))
                    }
                    _ => self
                        .select_display_method(ty, ty, &mut display)
                        .map(|selected| selected.is_some()),
                },
            )
    }

    /// Select rendering by its source identity while binding the method to
    /// the normalized value type used by the callable ABI.
    ///
    /// # Errors
    /// Refuses abstract receivers or inconsistent implementation binders.
    pub fn display_method_for_type(
        &self,
        value: &ResolvedTy,
        source: &ResolvedTy,
    ) -> Result<Option<(crate::DefId, Vec<ResolvedTy>)>, ClassError> {
        self.select_display_method(value, source, &mut DisplayWalk::default())
    }

    /// Select a Display implementation whose bounds the live checker admitted.
    ///
    /// # Errors
    /// Refuses missing registration facts or inconsistent implementation binders.
    pub fn display_method_for_checked_type(
        &self,
        value: &ResolvedTy,
        source: &ResolvedTy,
    ) -> Result<Option<(crate::DefId, Vec<ResolvedTy>)>, ClassError> {
        let Some(method) = self.selected_display_method(source) else {
            return Ok(None);
        };
        let binders = self.context.method_binders.get(&method).ok_or_else(|| {
            ClassError::UnknownDeclaration {
                name: self.context.defs.display(method).to_string(),
            }
        })?;
        binders
            .arguments(&self.context.defs, method, value)
            .map(|args| Some((method, args)))
    }

    /// Checked Display bodies reached while rendering this source value.
    /// Structural members retain their declared source identities, including aliases.
    ///
    /// # Errors
    /// Refuses unresolved types or absent structural declaration metadata.
    pub fn rendering_calls(
        &self,
        source: &ResolvedTy,
    ) -> Result<Vec<(crate::DefId, Vec<ResolvedTy>)>, String> {
        let mut calls = Vec::new();
        let mut display = DisplayWalk::default();
        let mut pending = vec![source.clone()];
        let mut seen = HashSet::new();
        while let Some(source) = pending.pop() {
            if !seen.insert(source.clone()) {
                continue;
            }
            if let Some(call) = self
                .select_display_method(&source, &source, &mut display)
                .map_err(|error| error.to_string())?
            {
                calls.push(call);
                continue;
            }
            let source = self.rendering_source(&source)?;
            if !source.to_ty().renders_structurally() {
                return Err(format!(
                    "`{}` has no checked rendering",
                    source.user_facing()
                ));
            }
            match source {
                ResolvedTy::Tuple(members) => pending.extend(members),
                ResolvedTy::Named {
                    is_opaque: true, ..
                } => {}
                ResolvedTy::Named { head, args, .. } if head.builtin().is_some() => {
                    pending.extend(args);
                }
                ResolvedTy::Named { .. } => pending.extend(
                    self.declared_capability_members(&source)
                        .map_err(|error| error.to_string())?,
                ),
                _ => {}
            }
        }
        Ok(calls)
    }

    /// Whether rendering selects an explicit implementation outside the stdlib.
    #[must_use]
    pub fn has_explicit_display_impl(&self, source: &ResolvedTy) -> bool {
        is_authored_display_method(&self.context.defs, self.selected_display_method(source))
    }

    /// Select the intrinsic path, preserving an authored Display override.
    #[must_use]
    pub fn builtin_display_plan(&self, source: &ResolvedTy) -> Option<BuiltinDisplayPlan> {
        select_builtin_display_plan(
            source,
            &self.context.defs,
            &self.context.method_ids,
            self.context.display_method,
        )
    }

    fn selected_display_method(&self, source: &ResolvedTy) -> Option<crate::DefId> {
        let slot = ImplMethodSlot::Declared(self.context.display_method?);
        selected_impl_method(&self.context.method_ids, source, slot).or_else(|| {
            let expanded = self.rendering_source(source).ok()?;
            selected_impl_method(&self.context.method_ids, &expanded, slot)
        })
    }

    fn select_display_method(
        &self,
        ty: &ResolvedTy,
        source: &ResolvedTy,
        walk: &mut DisplayWalk,
    ) -> DisplayResult {
        let key = (ty.clone(), source.clone());
        match walk.states.get(&key) {
            Some(DisplayState::Complete(result)) => return result.clone(),
            Some(DisplayState::Visiting) => return Ok(None),
            None => {}
        }
        walk.states.insert(key.clone(), DisplayState::Visiting);
        let result = self.select_display_method_uncached(ty, source, walk);
        walk.states
            .insert(key, DisplayState::Complete(result.clone()));
        result
    }

    fn select_display_method_uncached(
        &self,
        ty: &ResolvedTy,
        source: &ResolvedTy,
        walk: &mut DisplayWalk,
    ) -> DisplayResult {
        require_concrete_capability_type(ty)?;
        let Some(method) = self.selected_display_method(source) else {
            return Ok(None);
        };
        let binders = self.context.method_binders.get(&method).ok_or_else(|| {
            ClassError::UnknownDeclaration {
                name: self.context.defs.display(method).to_string(),
            }
        })?;
        let args = binders.instantiate_with(
            &self.context.defs,
            method,
            ty,
            &self.context.registry,
            &mut |ty, bound| match bound {
                ImplMethodObligation::Serializable => Ok(self.is_serializable(ty, &|_, _| false)),
                _ => self
                    .select_display_method(ty, ty, walk)
                    .map(|selected| selected.is_some()),
            },
        );
        match args {
            Ok(args) => Ok(Some((method, args))),
            Err(ClassError::UnknownDeclaration { .. }) => Ok(None),
            Err(error) => Err(error),
        }
    }

    /// Expand only a source alias's head through its checked declaration.
    /// Nested alias identities stay intact for the child rendering selections.
    ///
    /// # Errors
    /// Refuses recursive aliases, incorrect arity or unresolved target types.
    pub fn rendering_source(&self, source: &ResolvedTy) -> Result<ResolvedTy, String> {
        let mut source = source.clone();
        let mut visited = HashSet::new();
        while let ResolvedTy::Named { head, args, .. } = &source {
            let Some(declaration) = head.declaration(&self.context.defs) else {
                break;
            };
            let Some(alias) = self.context.aliases.get(&declaration.declaration()) else {
                break;
            };
            if !visited.insert(declaration) {
                return Err("recursive rendering alias".into());
            }
            let target = alias
                .instantiate(&args.iter().map(ResolvedTy::to_ty).collect::<Vec<_>>())
                .ok_or_else(|| "rendering alias has inconsistent type arguments".to_string())?;
            source = ResolvedTy::from_ty(&target).map_err(|error| error.to_string())?;
        }
        Ok(source)
    }

    /// Preserve an explicitly authored field type for rendering, before alias
    /// expansion erases its distinct Display implementation.
    ///
    /// # Errors
    /// Refuses a field whose checked type still contains inference variables.
    pub fn rendering_field(
        &self,
        source: &ResolvedTy,
        variant: Option<&str>,
        field: &str,
    ) -> Result<Option<ResolvedTy>, String> {
        let ResolvedTy::Named { head, args, .. } = source else {
            return Ok(None);
        };
        let Some(definition) = head
            .declaration(&self.context.defs)
            .and_then(|id| self.context.rendering_members.get(&id))
        else {
            return Ok(None);
        };
        let ty = if let Some(variant) = variant {
            match definition.variants.get(variant) {
                Some(crate::check::VariantDef::Tuple(fields)) => field
                    .parse::<usize>()
                    .ok()
                    .and_then(|index| fields.get(index)),
                Some(crate::check::VariantDef::Struct(fields)) => fields
                    .iter()
                    .find_map(|(name, ty)| (name == field).then_some(ty)),
                _ => None,
            }
        } else {
            definition.fields.get(field)
        };
        ty.map(|ty| {
            let source = ResolvedTy::from_ty(ty).map_err(|error| error.to_string())?;
            Ok(crate::resolved_ty::substitute_type_params(
                &source,
                &definition.type_params,
                args,
            ))
        })
        .transpose()
    }

    fn select_capability(
        &self,
        ty: &ResolvedTy,
        capability: ValueCapability,
    ) -> Result<Option<ValueMethodPlan>, ClassError> {
        self.select_capability_inner(
            ty,
            capability,
            &mut CapabilityWalk::default(),
            &mut Vec::new(),
        )
    }

    fn select_capability_inner(
        &self,
        ty: &ResolvedTy,
        capability: ValueCapability,
        walk: &mut CapabilityWalk,
        calls: &mut Vec<(crate::DefId, Vec<ResolvedTy>)>,
    ) -> CapabilityResult {
        use crate::capability_walk::CapabilityVisit;
        let method = selected_impl_method(
            &self.context.method_ids,
            ty,
            ImplMethodSlot::Value(capability),
        );
        let entry = match walk.enter(ty, capability, method.is_none()) {
            CapabilityVisit::Complete(result) => return result,
            CapabilityVisit::Recursive
                if matches!(capability, ValueCapability::Eq | ValueCapability::Hash) =>
            {
                return Ok(Some(ValueMethodPlan::Derived))
            }
            CapabilityVisit::Recursive | CapabilityVisit::Growing => return Ok(None),
            CapabilityVisit::Entered(entry) => entry,
        };
        let call_mark = calls.len();
        let result = self.select_capability_uncached(ty, capability, method, walk, calls);
        let admitted = matches!(result, Ok(Some(_)));
        if !admitted {
            calls.truncate(call_mark);
        }
        walk.finish(entry, result.clone(), admitted);
        result
    }

    fn select_capability_uncached(
        &self,
        ty: &ResolvedTy,
        capability: ValueCapability,
        method: Option<crate::DefId>,
        walk: &mut CapabilityWalk,
        calls: &mut Vec<(crate::DefId, Vec<ResolvedTy>)>,
    ) -> Result<Option<ValueMethodPlan>, ClassError> {
        self.classify(ty)?;
        require_concrete_capability_type(ty)?;
        if let Some(method) = method {
            if capability == ValueCapability::Hash {
                let admitted_shape = match ty {
                    ResolvedTy::Named {
                        head: crate::TypeHead::Builtin(builtin),
                        ..
                    } => matches!(
                        builtin,
                        crate::BuiltinType::NodeId | crate::BuiltinType::Location
                    ),
                    ResolvedTy::Named { head, .. } => {
                        self.context.types().of(*head).is_some_and(|definition| {
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
                    name: self.context.defs.display(method).to_string(),
                }
            })?;
            let type_args = binders.instantiate_with(
                &self.context.defs,
                method,
                ty,
                &self.context.registry,
                &mut |ty, bound| {
                    Ok(matches!(bound, ImplMethodObligation::Serializable)
                        && self.is_serializable(ty, &|_, _| false))
                },
            )?;
            calls.push((method, type_args.clone()));
            return Ok(Some(ValueMethodPlan::User { method, type_args }));
        }
        let derived = match capability {
            ValueCapability::Hash => self.derived_hash(ty, walk, calls),
            ValueCapability::Eq => self.derived_eq(ty, walk, calls),
            ordering => self.derived_ordering(ty, ordering, walk, calls),
        };
        derived.map(|allowed| allowed.then_some(ValueMethodPlan::Derived))
    }

    fn derived_eq(
        &self,
        ty: &ResolvedTy,
        walk: &mut CapabilityWalk,
        calls: &mut Vec<(crate::DefId, Vec<ResolvedTy>)>,
    ) -> Result<bool, ClassError> {
        match self.derived_eq_members(ty)? {
            Some(members) => {
                self.members_have_capability(&members, ValueCapability::Eq, walk, calls)
            }
            None => Ok(false),
        }
    }

    /// The member demands of structural equality, also used before generic substitution.
    #[allow(
        clippy::too_many_lines,
        reason = "one exhaustive authority for structural equality member demands"
    )]
    pub(crate) fn derived_eq_members(
        &self,
        ty: &ResolvedTy,
    ) -> Result<Option<Vec<ResolvedTy>>, ClassError> {
        if selected_impl_method(
            &self.context.method_ids,
            ty,
            ImplMethodSlot::Value(ValueCapability::Eq),
        )
        .is_some()
        {
            return Ok(None);
        }
        match ty {
            ResolvedTy::Tuple(members) => Ok(Some(members.clone())),
            ResolvedTy::Named {
                head:
                    crate::TypeHead::Builtin(
                        builtin @ (BuiltinType::Option | BuiltinType::Result | BuiltinType::Vec),
                    ),
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
                Ok(Some(args.clone()))
            }
            // An actor reference compares by identity, exactly as `is` does;
            // its state is never compared.
            ResolvedTy::Named {
                head:
                    crate::TypeHead::Builtin(BuiltinType::NodeId | BuiltinType::Location)
                    | crate::TypeHead::Actor(_),
                ..
            } => Ok(Some(Vec::new())),
            ResolvedTy::Named {
                head:
                    crate::TypeHead::Builtin(
                        BuiltinType::HashMap
                        | BuiltinType::HashSet
                        | BuiltinType::Rc
                        | BuiltinType::Weak
                        | BuiltinType::JsonValue
                        | BuiltinType::YamlValue
                        | BuiltinType::TomlValue,
                    ),
                ..
            } => Ok(None),
            ResolvedTy::Named { head, args, .. } => {
                let builtin = head.builtin();
                let owner = ty.nominal_instance(&self.context.defs).map_or_else(
                    || head.registry_key().to_string(),
                    |instance| {
                        self.context
                            .defs
                            .path(instance.nominal.declaration())
                            .to_string()
                    },
                );
                let Some(definition) = self.context.types().of(*head) else {
                    // A builtin, or a memberless declaration such as a
                    // supervisor, derives nothing.
                    if builtin.is_some()
                        || head
                            .declaration(&self.context.defs)
                            .is_some_and(|id| self.context.declarations.contains_key(&id))
                    {
                        return Ok(None);
                    }
                    return Err(ClassError::UnknownDeclaration { name: owner });
                };
                if definition.is_indirect
                    || (builtin.is_some() && definition.type_params.len() != args.len())
                {
                    return Ok(None);
                }
                self.declared_capability_members(ty).map(Some)
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
            )
            .then(Vec::new)),
        }
    }

    /// Whether `ty` is data; see [`crate::data_shape`].
    pub(crate) fn is_serializable(
        &self,
        ty: &ResolvedTy,
        param: &dyn Fn(crate::ParamHead, MarkerTrait) -> bool,
    ) -> bool {
        self.data_error(ty, false, param).is_none()
    }

    /// The service's declaration context.
    #[must_use]
    pub fn context(&self) -> &TypeFactContext {
        &self.context
    }

    /// Map keys and set elements need the selected `Hash` and `Eq` the codec
    /// uses to rebuild the collection.
    pub(crate) fn is_codec_key(
        &self,
        ty: &ResolvedTy,
        param: &dyn Fn(crate::ParamHead, MarkerTrait) -> bool,
    ) -> bool {
        if let ResolvedTy::TypeParam { name } = ty {
            return param(*name, MarkerTrait::Hash) && param(*name, MarkerTrait::Eq);
        }
        [ValueCapability::Hash, ValueCapability::Eq]
            .into_iter()
            .all(|capability| matches!(self.select_capability(ty, capability), Ok(Some(_))))
    }

    fn members_have_capability(
        &self,
        members: &[ResolvedTy],
        capability: ValueCapability,
        walk: &mut CapabilityWalk,
        calls: &mut Vec<(crate::DefId, Vec<ResolvedTy>)>,
    ) -> Result<bool, ClassError> {
        for member in members {
            for capability in capability.component_capabilities() {
                if self
                    .select_capability_inner(member, *capability, walk, calls)?
                    .is_none()
                {
                    return Ok(false);
                }
            }
        }
        Ok(true)
    }

    /// Instantiate declaration members, preserving the same source identities
    /// and parallel parameter substitution as the class and record services.
    pub(crate) fn declared_capability_members(
        &self,
        ty: &ResolvedTy,
    ) -> Result<Vec<ResolvedTy>, ClassError> {
        let ResolvedTy::Named { head, args, .. } = ty else {
            return Err(ClassError::UnknownDeclaration {
                name: ty.user_facing().to_string(),
            });
        };
        let id =
            head.declaration(&self.context.defs)
                .ok_or_else(|| ClassError::UnknownDeclaration {
                    name: ty.user_facing().to_string(),
                })?;
        let name = head.registry_key();
        let declaration =
            self.context
                .declarations
                .get(&id)
                .ok_or_else(|| ClassError::UnknownDeclaration {
                    name: name.to_string(),
                })?;
        if declaration.type_params.len() != args.len() {
            return Err(ClassError::UnknownDeclaration {
                name: ty.user_facing().to_string(),
            });
        }
        if declaration.marker == crate::DeclarationMarker::None || !declaration.members.is_empty() {
            return declaration
                .members
                .iter()
                .map(|member| {
                    self.context
                        .instantiate_member(member, &declaration.type_params, args)
                })
                .collect();
        }
        // A marked declaration keeps no members when one of them could not be
        // rendered at the boundary: its class came from the marker anyway.
        // Equality and hashing still inspect the declaration's own fields.
        let definition =
            self.context
                .types()
                .of(*head)
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
                let member = self.context.associated_types.instantiate_member(
                    member,
                    &declaration.type_params,
                    args,
                    &self.context.defs,
                );
                let member =
                    ResolvedTy::from_ty(&member).map_err(|_| ClassError::UnknownDeclaration {
                        name: name.to_string(),
                    })?;
                let member =
                    crate::check::restore_member_opacity(member, &self.context.defs, &|name| {
                        self.context
                            .declarations
                            .get(&name)
                            .is_some_and(|decl| decl.is_opaque)
                    });
                Ok(member)
            })
            .collect()
    }

    fn derived_hash(
        &self,
        ty: &ResolvedTy,
        walk: &mut CapabilityWalk,
        calls: &mut Vec<(crate::DefId, Vec<ResolvedTy>)>,
    ) -> Result<bool, ClassError> {
        match ty {
            ResolvedTy::Tuple(members) => {
                self.members_have_capability(members, ValueCapability::Hash, walk, calls)
            }
            ResolvedTy::Named {
                head: crate::TypeHead::Builtin(builtin),
                args,
                ..
            } => match builtin {
                crate::BuiltinType::NodeId | crate::BuiltinType::Location => Ok(true),
                crate::BuiltinType::Timestamp => self.members_have_capability(
                    &self.declared_capability_members(ty)?,
                    ValueCapability::Hash,
                    walk,
                    calls,
                ),
                crate::BuiltinType::Vec => {
                    if args.len() != 1 {
                        return Err(ClassError::UnknownDeclaration {
                            name: ty.user_facing().to_string(),
                        });
                    }
                    self.members_have_capability(args, ValueCapability::Hash, walk, calls)
                }
                _ => Ok(false),
            },
            ResolvedTy::Named { head, args, .. } => {
                let name = head.registry_key();
                let Some(definition) = self.context.types().of(*head) else {
                    // A memberless declaration such as a supervisor derives nothing.
                    if head
                        .declaration(&self.context.defs)
                        .is_some_and(|id| self.context.declarations.contains_key(&id))
                    {
                        return Ok(false);
                    }
                    return Err(ClassError::UnknownDeclaration {
                        name: name.to_string(),
                    });
                };
                if definition.type_params.len() != args.len() {
                    return Err(ClassError::UnknownDeclaration {
                        name: ty.user_facing().to_string(),
                    });
                }
                if head
                    .declaration(&self.context.defs)
                    .and_then(|id| self.context.declarations.get(&id))
                    .is_some_and(|decl| decl.is_opaque)
                    || definition.is_indirect
                    || !matches!(
                        definition.kind,
                        crate::check::TypeDefKind::Struct | crate::check::TypeDefKind::Record
                    )
                    || head
                        .nominal()
                        .is_some_and(|id| self.context.registry.is_resource(id))
                {
                    return Ok(false);
                }
                self.members_have_capability(
                    &self.declared_capability_members(ty)?,
                    ValueCapability::Hash,
                    walk,
                    calls,
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
                    | ResolvedTy::F32
                    | ResolvedTy::F64
                    | ResolvedTy::Bool
                    | ResolvedTy::Char
                    | ResolvedTy::Duration
                    | ResolvedTy::String
                    | ResolvedTy::Bytes
            )),
        }
    }

    fn derived_ordering(
        &self,
        ty: &ResolvedTy,
        capability: ValueCapability,
        walk: &mut CapabilityWalk,
        calls: &mut Vec<(crate::DefId, Vec<ResolvedTy>)>,
    ) -> Result<bool, ClassError> {
        debug_assert!(capability.is_ordering());
        match ty {
            ResolvedTy::Tuple(members) => {
                self.members_have_capability(members, capability, walk, calls)
            }
            ResolvedTy::Named {
                head:
                    crate::TypeHead::Builtin(builtin @ (BuiltinType::Option | BuiltinType::Result)),
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
                self.members_have_capability(args, capability, walk, calls)
            }
            ResolvedTy::Named { head, .. } => {
                let Some(definition) = self.context.types().of(*head) else {
                    return Ok(false);
                };
                if definition.is_indirect
                    || !matches!(
                        definition.kind,
                        crate::check::TypeDefKind::Struct
                            | crate::check::TypeDefKind::Record
                            | crate::check::TypeDefKind::Enum
                    )
                {
                    return Ok(false);
                }
                self.members_have_capability(
                    &self.declared_capability_members(ty)?,
                    capability,
                    walk,
                    calls,
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
            )),
        }
    }

    fn contextual_facts(&self, ty: &ResolvedTy) -> Result<TypeFacts, ClassError> {
        let as_ty = ty.to_ty();
        let send = self.send_fact(ty, &as_ty);
        let hash = self.select_capability(ty, ValueCapability::Hash)?.is_some();
        let eq = self.select_capability(ty, ValueCapability::Eq)?.is_some();
        let partial_ord = [
            ValueCapability::Less,
            ValueCapability::LessEqual,
            ValueCapability::Greater,
            ValueCapability::GreaterEqual,
        ]
        .into_iter()
        .all(|capability| {
            self.select_capability(ty, capability)
                .is_ok_and(|plan| plan.is_some())
        });
        let (class, clone) = self.classify(ty)?;
        Ok(TypeFacts::with_class(
            ty,
            class,
            clone,
            send,
            hash,
            eq,
            partial_ord,
        ))
    }

    fn send_fact(&self, ty: &ResolvedTy, as_ty: &crate::Ty) -> SendFact {
        if ty.generator_parts().is_some() {
            return SendFact::DeferredToGeneratorFacts;
        }
        if matches!(ty.callable_body(), ResolvedTy::Closure { .. }) {
            return SendFact::DeferredToClosureFacts;
        }
        let components = match ty {
            ResolvedTy::Named { head, .. }
                if self
                    .context
                    .registry
                    .denies_marker(*head, MarkerTrait::Send) =>
            {
                return SendFact::Known(false)
            }
            ResolvedTy::Named { .. } => self
                .record_fields(ty)
                .ok()
                .map(|(_, fields)| fields.into_iter().map(|(_, ty)| ty).collect::<Vec<_>>()),
            ResolvedTy::Tuple(_)
            | ResolvedTy::Array(_, _)
            | ResolvedTy::Slice(_)
            | ResolvedTy::Task(_) => {
                let mut components = Vec::new();
                push_type_components(ty, &mut components);
                Some(components)
            }
            _ => None,
        };
        if let Some(components) = components {
            let facts: Vec<_> = components
                .iter()
                .filter_map(|component| {
                    self.rows
                        .get(&TypeInstanceKey(component.clone()))
                        .map(|row| row.send)
                })
                .collect();
            if facts.len() == components.len() && !facts.contains(&SendFact::Known(false)) {
                if facts.contains(&SendFact::DeferredToGeneratorFacts) {
                    return SendFact::DeferredToGeneratorFacts;
                }
                if facts.contains(&SendFact::DeferredToClosureFacts) {
                    return SendFact::DeferredToClosureFacts;
                }
            }
        }
        SendFact::Known(
            self.context
                .registry
                .implements_marker(as_ty, MarkerTrait::Send),
        )
    }
}

impl TypeFacts {
    /// Build the facts for one substituted type.
    ///
    /// `send`, `hash`, `eq` and ordering are decided by their own checker authorities and
    /// handed in; `class` and `clone` come from the class table.
    ///
    /// # Errors
    ///
    /// Returns [`ClassError`] when the class table refuses the type. There is no default
    /// row: a type with no class gets no entry, and a consumer fails closed on
    /// the absence.
    pub fn of_type(
        ty: &ResolvedTy,
        decls: &ClassContext<'_>,
        send: SendFact,
        hash: bool,
        eq: bool,
        partial_ord: bool,
    ) -> Result<Self, ClassError> {
        let (class, clone) = crate::value_class::classify_ty(ty, decls)?;
        Ok(Self::with_class(
            ty,
            class,
            clone,
            send,
            hash,
            eq,
            partial_ord,
        ))
    }

    fn with_class(
        ty: &ResolvedTy,
        class: ValueClass,
        clone: CloneKind,
        send: SendFact,
        hash: bool,
        eq: bool,
        partial_ord: bool,
    ) -> Self {
        let send = match ty.callable_body() {
            ty if ty.generator_parts().is_some() => SendFact::DeferredToGeneratorFacts,
            ResolvedTy::Closure { .. } => SendFact::DeferredToClosureFacts,
            // MARKED SHORTCUT - a trait object's send fact is `false` here.
            // WHY: the type-fact contract makes it "the bound list contains `Send`", and that is
            // sound *only* because the class table's `CoerceToDynTrait` wall refuses a
            // coercion into a `+ Send` object whose concrete is not `Send`. The
            // wall does not exist yet, so the bound list would let a
            // `dyn ... + Send` over an `Rc`-holding concrete be shared between
            // actors and race a non-atomic count.
            // WHEN: the coercion wall lands with the closure send facts.
            // WHAT: this constructor takes the caller's decided fact, which is
            // then the bound list the type-fact contract names.
            ResolvedTy::TraitObject { .. } => SendFact::Known(false),
            _ => send,
        };
        Self {
            class,
            clone,
            send,
            hash,
            eq,
            partial_ord,
        }
    }
}

/// Push a type's immediate component types, so the fact table is closed under
/// the types its rows are built from.
pub fn push_type_components(ty: &ResolvedTy, out: &mut Vec<ResolvedTy>) {
    match ty {
        ResolvedTy::Generator {
            yields,
            returns,
            producers,
        } => {
            out.push((**yields).clone());
            out.push((**returns).clone());
            out.extend(
                producers
                    .iter()
                    .flat_map(|producer| &producer.arguments)
                    .map(|(_, ty)| ty.clone()),
            );
        }
        ResolvedTy::GenericCallable { callable, .. } => {
            if let ResolvedTy::Closure { captures, .. } = callable.callable_body() {
                out.extend(captures.iter().cloned());
            }
        }
        ResolvedTy::Tuple(elements) => out.extend(elements.iter().cloned()),
        ResolvedTy::Array(element, _) | ResolvedTy::Slice(element) => {
            out.push((**element).clone());
        }
        ResolvedTy::Named { args, .. } => out.extend(args.iter().cloned()),
        ResolvedTy::Function { params, ret, .. } => {
            out.extend(params.iter().map(|parameter| parameter.ty.clone()));
            out.push((**ret).clone());
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
            ..
        } => {
            out.extend(params.iter().map(|parameter| parameter.ty.clone()));
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
    use super::{
        selected_impl_method, CapabilityWalk, ImplMethodKey, ValueCapability, ValueMethodPlan,
    };
    use std::collections::BTreeMap;
    use std::collections::{HashMap, HashSet};

    use super::{
        CloneKind, SendFact, TypeFactContext, TypeFactService, TypeFacts, TypeInstanceKey,
    };
    use crate::builtin_type::{builtin_types, BuiltinType};
    use crate::resolved_ty::{ResolvedTraitBound, ResolvedTy};
    use crate::value_class::{
        ClassContext, ClassError, DeclarationMarker, DeclaredType, ValueClass,
    };

    fn named(name: &str, builtin: Option<BuiltinType>, args: Vec<ResolvedTy>) -> ResolvedTy {
        match builtin {
            Some(BuiltinType::ActorHandle) => ResolvedTy::actor_for_test(name, args),
            Some(builtin) => ResolvedTy::named_builtin(builtin, args),
            None => ResolvedTy::user_for_test(name, args),
        }
    }

    fn generic_impl_binders(receiver_name: &str) -> super::ImplMethodBinders {
        super::ImplMethodBinders {
            receiver: crate::Ty::named_for_test(
                receiver_name,
                vec![crate::Ty::param(crate::ParamHead::for_test("T"))],
            ),
            impl_params: vec![crate::ParamHead::for_test("T")],
            method_params: vec![],
            obligations: vec![],
            checked_receivers: HashSet::new(),
            implemented_trait: None,
        }
    }

    #[test]
    fn impl_method_binders_reject_same_leaf_receiver_from_another_owner() {
        let binders = generic_impl_binders("local.Wrapper");
        let receiver = named("foreign.Wrapper", None, vec![ResolvedTy::I64]);

        let mut defs = crate::DefTable::new();
        let method = defs.mint_for_test("local.Wrapper::eq");
        assert!(matches!(
            binders.instantiate(
                &defs,
                method,
                &receiver,
                &crate::traits::TraitRegistry::new(),
            ),
            Err(ClassError::UnknownDeclaration { .. })
        ));
    }

    #[test]
    fn impl_method_binders_recover_the_exact_nested_opaque_argument() {
        let binders = generic_impl_binders("owner.Wrapper");
        let opaque = ResolvedTy::opaque_for_test("owner.Handle", vec![]);
        let argument = ResolvedTy::named_builtin(BuiltinType::Option, vec![opaque.clone()]);
        let receiver = named("owner.Wrapper", None, vec![argument.clone()]);

        let mut defs = crate::DefTable::new();
        let method = defs.mint_for_test("owner.Wrapper::eq");
        let inferred = binders
            .instantiate(
                &defs,
                method,
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

    #[test]
    fn method_selection_keeps_declarations_and_specializations_distinct() {
        let mut defs = crate::DefTable::new();
        let left = crate::NominalId::of_declaration(defs.mint_for_test("left.Box"));
        let right = crate::NominalId::of_declaration(defs.mint_for_test("right.Box"));
        let slot = super::ImplMethodSlot::Declared(defs.mint_for_test("Show::show"));
        let other_slot = super::ImplMethodSlot::Declared(defs.mint_for_test("OtherShow::show"));
        let generic = defs.mint_for_test("left.generic");
        let exact = defs.mint_for_test("left.i64");
        let receiver = |id, args| ResolvedTy::Named {
            head: crate::TypeHead::Nominal(crate::NominalHead::new(id, "Box")),
            args,
            is_opaque: false,
        };
        let concrete = receiver(left, vec![ResolvedTy::I64]);
        let methods = HashMap::from([
            (ImplMethodKey::new(&concrete, slot, true), generic),
            (ImplMethodKey::new(&concrete, slot, false), exact),
        ]);
        assert_eq!(selected_impl_method(&methods, &concrete, slot), Some(exact));
        assert_eq!(
            selected_impl_method(&methods, &receiver(left, vec![ResolvedTy::String]), slot),
            Some(generic)
        );
        assert_eq!(
            selected_impl_method(&methods, &receiver(right, vec![ResolvedTy::I64]), slot),
            None
        );
        assert_eq!(selected_impl_method(&methods, &concrete, other_slot), None);
    }

    #[test]
    fn shared_display_bounds_preserve_arguments_and_refusal() {
        let parsed = hew_parser::parse(
            r#"type Leaf {}
impl Display for Leaf { fn fmt(self) -> string { "leaf" } }
type Missing {}
type Wrapper<T> { value: T; }
impl<T: Display> Display for Wrapper<T> { fn fmt(self) -> string { "wrapper" } }
fn main() {}"#,
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let mut checker = crate::Checker::new(crate::module_registry::ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parsed.program);
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        let mut service = TypeFactService::new(output.type_fact_context, output.type_facts);
        // Repeated obligations model a shared proof edge without duplicating the owned type tree.
        for binders in service.context.method_binders.values_mut() {
            binders.obligations.extend(binders.obligations.clone());
        }
        let wrap = |leaf: ResolvedTy| {
            (0..28).fold(leaf, |ty, _| {
                ResolvedTy::named_path(service.defs(), "Wrapper", vec![ty])
            })
        };
        let ty = wrap(ResolvedTy::named_path(service.defs(), "Leaf", vec![]));
        let (_, args) = service.display_method_for_type(&ty, &ty).unwrap().unwrap();
        let ResolvedTy::Named { args: expected, .. } = &ty else {
            panic!("nominal receiver");
        };
        assert_eq!(&args, expected);
        let missing = wrap(ResolvedTy::named_path(service.defs(), "Missing", vec![]));
        assert_eq!(
            service.display_method_for_type(&missing, &missing),
            Ok(None)
        );
    }

    #[test]
    fn actor_equality_ignores_state_without_admitting_hash() {
        let parsed = hew_parser::parse(
            r"
            actor Worker {
                var state: HashMap<string, i64>;
                receive fn ping() {}
            }
            type Holder { worker: Worker; }
            fn main() {}
        ",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let mut checker = crate::Checker::new(crate::module_registry::ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parsed.program);
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        let service = TypeFactService::new(output.type_fact_context, output.type_facts);
        let actor = ResolvedTy::named_actor_path(service.defs(), "Worker", vec![]);
        let holder = ResolvedTy::named_path(service.defs(), "Holder", vec![]);
        let vector = ResolvedTy::named_builtin(BuiltinType::Vec, vec![actor.clone()]);
        for ty in [actor, holder, vector] {
            assert_eq!(
                service.select_capability(&ty, ValueCapability::Eq),
                Ok(Some(ValueMethodPlan::Derived))
            );
            assert_eq!(
                service.select_capability(&ty, ValueCapability::Hash),
                Ok(None)
            );
        }
    }

    #[test]
    fn recursive_capabilities_withdraw_failed_mutual_assumptions_in_either_order() {
        let parsed = hew_parser::parse(
            r"
            type Left { peers: Vec<Right>; unsupported: HashMap<string, i64>; }
            type Right { peers: Vec<Left>; }
            type Node { children: Vec<Node>; label: string; }
            fn main() {}
        ",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let mut checker = crate::Checker::new(crate::module_registry::ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parsed.program);
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        let service = TypeFactService::new(output.type_fact_context, output.type_facts);
        for capability in [ValueCapability::Eq, ValueCapability::Hash] {
            for order in [["Left", "Right"], ["Right", "Left"]] {
                let mut walk = CapabilityWalk::default();
                let mut calls = Vec::new();
                for name in order {
                    let ty = ResolvedTy::named_path(service.defs(), name, vec![]);
                    assert_eq!(
                        service.select_capability_inner(&ty, capability, &mut walk, &mut calls),
                        Ok(None)
                    );
                    assert!(calls.is_empty());
                }
                let node = ResolvedTy::named_path(service.defs(), "Node", vec![]);
                assert_eq!(
                    service.select_capability_inner(&node, capability, &mut walk, &mut calls),
                    Ok(Some(ValueMethodPlan::Derived))
                );
            }
        }
    }

    /// Declarations the aggregate rule needs for the cases below.
    fn declarations() -> BTreeMap<crate::NominalId, DeclaredType> {
        let mut decls = BTreeMap::new();
        decls.insert(
            crate::NominalId::for_test("Conn"),
            DeclaredType {
                builtin: None,
                is_indirect: false,
                is_opaque: false,
                marker: DeclarationMarker::Resource,
                type_params: vec![],
                members: vec![ResolvedTy::I64],
            },
        );
        decls.insert(
            crate::NominalId::for_test("Ticket"),
            DeclaredType {
                builtin: None,
                is_indirect: false,
                is_opaque: false,
                marker: DeclarationMarker::Linear,
                type_params: vec![],
                members: vec![ResolvedTy::I64],
            },
        );
        decls.insert(
            crate::NominalId::for_test("Point"),
            DeclaredType {
                builtin: None,
                is_indirect: false,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![ResolvedTy::I64, ResolvedTy::I64],
            },
        );
        decls.insert(
            crate::NominalId::for_test("Label"),
            DeclaredType {
                builtin: None,
                is_indirect: false,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![ResolvedTy::String, ResolvedTy::I64],
            },
        );
        // `std/failure.hew::CrashInfo { kind: CrashKind, message: string }`.
        decls.insert(
            crate::NominalId::for_test("std.failure.CrashInfo"),
            DeclaredType {
                builtin: Some(BuiltinType::CrashInfo),
                is_indirect: false,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![
                    named("CrashKind", Some(BuiltinType::CrashKind), vec![]),
                    ResolvedTy::String,
                ],
            },
        );
        // `std/failure.hew::CrashNotification { target: DownTarget, kind: CrashKind }`.
        decls.insert(
            crate::NominalId::for_test("std.failure.CrashNotification"),
            DeclaredType {
                builtin: Some(BuiltinType::CrashNotification),
                is_indirect: false,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![
                    named("DownTarget", Some(BuiltinType::DownTarget), vec![]),
                    named("CrashKind", Some(BuiltinType::CrashKind), vec![]),
                ],
            },
        );
        decls.insert(
            crate::NominalId::for_test("std.builtins.VecIter"),
            DeclaredType {
                builtin: Some(BuiltinType::VecIter),
                type_params: vec![crate::ParamHead::for_test("T")],
                members: vec![
                    ResolvedTy::named_builtin(
                        BuiltinType::Vec,
                        vec![ResolvedTy::TypeParam {
                            name: crate::ParamHead::for_test("T"),
                        }],
                    ),
                    ResolvedTy::I64,
                ],
                ..DeclaredType::default()
            },
        );
        decls.insert(
            crate::NominalId::for_test("std.builtins.HashMapIter"),
            hashmap_iter_declared_type(),
        );
        decls
    }

    /// `std/builtins.hew::HashMapIter<K, V> { ks: Vec<K>, vs: Vec<V>, idx: i64 }`.
    /// Split out of `declarations()` to keep that function under the line cap.
    fn hashmap_iter_declared_type() -> DeclaredType {
        let vec_of_param = |name: &str| {
            ResolvedTy::named_builtin(
                BuiltinType::Vec,
                vec![ResolvedTy::TypeParam {
                    name: crate::ParamHead::for_test(name),
                }],
            )
        };
        DeclaredType {
            builtin: Some(BuiltinType::HashMapIter),
            type_params: vec![
                crate::ParamHead::for_test("K"),
                crate::ParamHead::for_test("V"),
            ],
            members: vec![vec_of_param("K"), vec_of_param("V"), ResolvedTy::I64],
            ..DeclaredType::default()
        }
    }

    fn facts(ty: &ResolvedTy) -> (ValueClass, CloneKind) {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        crate::value_class::classify_ty(ty, &context)
            .unwrap_or_else(|error| panic!("the class table refused `{ty:?}`: {error}"))
    }

    #[test]
    fn nominal_ownership_ignores_display_spelling() {
        let owned = crate::NominalId::for_test("owned.Ticket");
        let plain = crate::NominalId::for_test("plain.Ticket");
        let declarations = BTreeMap::from([
            (
                owned,
                DeclaredType {
                    marker: DeclarationMarker::Resource,
                    ..DeclaredType::default()
                },
            ),
            (
                plain,
                DeclaredType {
                    members: vec![ResolvedTy::I64],
                    ..DeclaredType::default()
                },
            ),
        ]);
        let context = ClassContext::new(&declarations);
        let value =
            |id, spelling| ResolvedTy::named_user(crate::NominalHead::new(id, spelling), vec![]);
        assert_eq!(
            crate::value_class::classify_ty(&value(owned, "Ticket"), &context),
            Ok((ValueClass::AffineResource, CloneKind::None))
        );
        assert_eq!(
            crate::value_class::classify_ty(&value(plain, "Ticket"), &context),
            Ok((ValueClass::BitCopy, CloneKind::Bits))
        );
        assert_eq!(
            crate::value_class::classify_ty(&value(owned, "another.alias"), &context),
            Ok((ValueClass::AffineResource, CloneKind::None))
        );
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

    /// Every `ResolvedTy` arm, asserting
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
                        trait_id: None,
                        args: vec![],
                        assoc_bindings: vec![],
                    }],
                },
                ValueClass::PersistentShare,
                CloneKind::None,
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
                ValueClass::CowValue,
                CloneKind::DeepCopy,
            ),
            (
                ResolvedTy::Array(Box::new(conn()), 3),
                ValueClass::AffineResource,
                CloneKind::None,
            ),
            (
                ResolvedTy::Task(Box::new(ResolvedTy::I64)),
                ValueClass::AffineResource,
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
                    name: crate::ParamHead::for_test("T")
                },
                &context
            ),
            Err(ClassError::TypeParam {
                param: crate::ParamHead::for_test("T")
            })
        );
    }

    /// Every `BuiltinType` variant**, in a `match`
    /// with no wildcard arm, so a new variant is a compile error rather than a
    /// silent `Unknown`.
    #[test]
    #[expect(
        clippy::match_same_arms,
        reason = "distinct class table rows that agree for these arguments must stay distinct arms"
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
                // Expected class of ordinary retained actor descriptors.
                BuiltinType::ActorHandle => Some((ValueClass::PersistentShare, CloneKind::Retain)),
                BuiltinType::SupervisorPool
                | BuiltinType::NodeId
                | BuiltinType::Location
                | BuiltinType::MonitorId
                | BuiltinType::DownTarget
                | BuiltinType::DownReason
                | BuiltinType::DownNotification
                | BuiltinType::Instant
            | BuiltinType::Timestamp
                | BuiltinType::Unit
                | BuiltinType::Duration
                | BuiltinType::Range
                | BuiltinType::Trap
                | BuiltinType::CrashAction
                | BuiltinType::CrashKind
                | BuiltinType::SendError
                | BuiltinType::NodeError
                | BuiltinType::LookupError
                | BuiltinType::LinkError
                // A lambda actor's handle is a pid under another spelling.
                | BuiltinType::ActorFn
                | BuiltinType::HewActor => Some((ValueClass::BitCopy, CloneKind::Bits)),
                // `Option<i64>` / `Result<i64, string>` join their payloads.
                BuiltinType::Option => Some((ValueClass::BitCopy, CloneKind::Bits)),
                BuiltinType::Result => Some((ValueClass::CowValue, CloneKind::FieldWise)),
                // A collection's buffer is heap, so it is never `BitCopy`.
                BuiltinType::Vec
                | BuiltinType::HashSet
                | BuiltinType::JsonValue
                | BuiltinType::YamlValue
                | BuiltinType::TomlValue => Some((ValueClass::CowValue, CloneKind::DeepCopy)),
                BuiltinType::HashMap | BuiltinType::HashMapIter | BuiltinType::VecIter => {
                    Some((ValueClass::CowValue, CloneKind::FieldWise))
                }
                BuiltinType::CrashInfo => Some((ValueClass::CowValue, CloneKind::FieldWise)),
                BuiltinType::CrashNotification => Some((ValueClass::BitCopy, CloneKind::Bits)),
                BuiltinType::Rc | BuiltinType::Weak => {
                    Some((ValueClass::AffineResource, CloneKind::Retain))
                }
                BuiltinType::Generator
                | BuiltinType::BoxedActor
                | BuiltinType::Sink
                | BuiltinType::Stream
                | BuiltinType::MonitorRef
                | BuiltinType::CancellationToken => {
                    Some((ValueClass::AffineResource, CloneKind::None))
                }
                BuiltinType::Task | BuiltinType::ActorCall => Some((ValueClass::AffineResource, CloneKind::None)),
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

    /// Builtin markers and the class table agree on copyable values.
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
            // Actor handles carry a coarse Resource marker. Their exact class
            // distinguishes retained descriptors from affine boxed actors.
            if matches!(
                info.kind,
                BuiltinType::ActorHandle
                    | BuiltinType::ActorFn
                    | BuiltinType::HewActor
                    | BuiltinType::BoxedActor
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

    /// Four representative ownership classes.
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

    /// Each of these types has no `NominalId`, so a
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
                    trait_id: None,
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
            assert!(
                ty.nominal_instance(&crate::DefTable::new()).is_none(),
                "`{ty:?}` has no NominalId"
            );
            let facts =
                TypeFacts::of_type(ty, &context, SendFact::Known(true), false, false, false)
                    .expect("the class table classifies every one of these");
            table.insert(TypeInstanceKey(ty.clone()), facts);
        }

        for ty in &instances {
            assert!(
                table.contains_key(&TypeInstanceKey(ty.clone())),
                "structural lookup for `{ty:?}`"
            );
        }
    }

    /// the type-fact contract: `send` is unrepresentable as a plain `bool` for a `Closure` key.
    #[test]
    fn a_closure_row_defers_its_send_fact() {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        let closure = closure_over(vec![ResolvedTy::I64]);
        // Even when the caller hands in a decided fact, the closure row refuses
        // it: the mode-agnostic answer is wrong for a `BorrowMut` capture.
        let facts = TypeFacts::of_type(
            &closure,
            &context,
            SendFact::Known(true),
            false,
            false,
            false,
        )
        .expect("a closure has a class");
        assert_eq!(SendFact::DeferredToClosureFacts, facts.send);

        // A consumer cannot read a `bool` out of it: the only way to a yes/no
        // is an explicit match that must name the deferral arm.
        let read_as_bool = match facts.send {
            SendFact::Known(value) => Some(value),
            SendFact::DeferredToClosureFacts | SendFact::DeferredToGeneratorFacts => None,
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

    /// A trait object's send fact is not the bound list yet: the type-fact contract makes that
    /// reading sound only through the class table's `CoerceToDynTrait` wall, which has not
    /// landed, so the row publishes the fail-closed verdict rather than one a
    /// consumer could act on unsoundly.
    #[test]
    fn a_trait_object_row_does_not_publish_a_wall_less_send_fact() {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        let dyn_show = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "Show".to_string(),
                trait_id: None,
                args: vec![],
                assoc_bindings: vec![],
            }],
        };
        let facts = TypeFacts::of_type(
            &dyn_show,
            &context,
            SendFact::Known(true),
            false,
            false,
            false,
        )
        .expect("a trait object has a class");
        assert_eq!(SendFact::Known(false), facts.send);
    }

    /// Declarations that reach themselves, for the recursion cases below.
    ///
    /// `Tree` is the shape of the shipped `indirect enum` fixture and `ResTree`
    /// is the same shape over a resource payload. `Pair<T>` mentions one fixed
    /// instantiation of itself, so the walk reaches the declaration at an
    /// instantiation it did not enter.
    fn recursive_declarations() -> BTreeMap<crate::NominalId, DeclaredType> {
        let mut decls = declarations();
        decls.insert(
            crate::NominalId::for_test("Tree"),
            DeclaredType {
                builtin: None,
                is_indirect: true,
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
            crate::NominalId::for_test("ResTree"),
            DeclaredType {
                builtin: None,
                is_indirect: true,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![conn(), named("ResTree", None, vec![])],
            },
        );
        decls.insert(
            crate::NominalId::for_test("Pair"),
            DeclaredType {
                builtin: None,
                is_indirect: true,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![crate::ParamHead::for_test("T")],
                members: vec![
                    ResolvedTy::TypeParam {
                        name: crate::ParamHead::for_test("T"),
                    },
                    named("Pair", None, vec![conn()]),
                ],
            },
        );
        decls.insert(
            crate::NominalId::for_test("Wrapper"),
            DeclaredType {
                builtin: None,
                is_indirect: false,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![crate::ParamHead::for_test("T")],
                members: vec![ResolvedTy::TypeParam {
                    name: crate::ParamHead::for_test("T"),
                }],
            },
        );
        decls.insert(
            crate::NominalId::for_test("Outer"),
            DeclaredType {
                builtin: None,
                is_indirect: false,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![crate::ParamHead::for_test("T")],
                members: vec![named(
                    "Wrapper",
                    None,
                    vec![ResolvedTy::TypeParam {
                        name: crate::ParamHead::for_test("T"),
                    }],
                )],
            },
        );
        decls
    }

    #[test]
    fn indirect_boxes_are_owned_with_scalar_or_recursive_payloads() {
        let mut decls = recursive_declarations();
        let tree = named("Tree", None, vec![]);
        for members in [vec![ResolvedTy::I64], vec![ResolvedTy::I64, tree.clone()]] {
            decls
                .get_mut(&crate::NominalId::for_test("Tree"))
                .unwrap()
                .members = members;
            let facts = crate::value_class::classify_ty(&tree, &ClassContext::new(&decls))
                .expect("an indirect enum has a class");
            assert_eq!(facts, (ValueClass::CowValue, CloneKind::FieldWise));
            assert_eq!(OwnershipObligation::Owned, obligation_of(facts.0));
        }
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

    /// Inline declarations aggregate their members without acquiring an
    /// indirect box of their own.
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
            crate::NominalId::for_test("Holder"),
            DeclaredType {
                builtin: None,
                is_indirect: false,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![crate::ParamHead::for_test("T")],
                members: vec![named("Expr", None, vec![])],
            },
        );
        decls.insert(
            crate::NominalId::for_test("Expr"),
            DeclaredType {
                builtin: None,
                is_indirect: true,
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
            crate::NominalId::for_test("Deep"),
            DeclaredType {
                builtin: None,
                is_indirect: false,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![crate::ParamHead::for_test("T")],
                members: vec![named(
                    "Deep",
                    None,
                    vec![ResolvedTy::Named {
                        args: vec![ResolvedTy::TypeParam {
                            name: crate::ParamHead::for_test("T"),
                        }],
                        head: crate::TypeHead::Builtin(BuiltinType::Vec),
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
            crate::NominalId::for_test("Grow"),
            DeclaredType {
                builtin: None,
                is_indirect: false,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![crate::ParamHead::for_test("T")],
                members: vec![named(
                    "Relay",
                    None,
                    vec![ResolvedTy::Named {
                        args: vec![ResolvedTy::TypeParam {
                            name: crate::ParamHead::for_test("T"),
                        }],
                        head: crate::TypeHead::Builtin(BuiltinType::Vec),
                        is_opaque: false,
                    }],
                )],
            },
        );
        decls.insert(
            crate::NominalId::for_test("Relay"),
            DeclaredType {
                builtin: None,
                is_indirect: false,
                is_opaque: false,
                marker: DeclarationMarker::None,
                type_params: vec![crate::ParamHead::for_test("U")],
                members: vec![named(
                    "Grow",
                    None,
                    vec![ResolvedTy::TypeParam {
                        name: crate::ParamHead::for_test("U"),
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

    #[test]
    fn classification_consumes_published_rows_before_expanding_instances() {
        let ty = named("Published", None, vec![]);
        let row = TypeFacts {
            class: ValueClass::AffineResource,
            clone: CloneKind::None,
            send: SendFact::Known(false),
            hash: false,
            eq: false,
            partial_ord: false,
        };
        let service = TypeFactService::new(
            TypeFactContext::new(
                BTreeMap::new(),
                crate::traits::TraitRegistry::new(),
                HashMap::new(),
            ),
            BTreeMap::from([(TypeInstanceKey(ty.clone()), row)]),
        );
        assert_eq!(service.classify(&ty), Ok((row.class, row.clone)));
    }

    #[test]
    fn retained_classification_matches_fresh_queries_across_recursive_instances() {
        let declarations = recursive_declarations();
        let service = TypeFactService::new(
            TypeFactContext::new(
                declarations.clone(),
                crate::traits::TraitRegistry::new(),
                HashMap::new(),
            ),
            BTreeMap::new(),
        );
        let queries = [
            named("Tree", None, vec![]),
            named("ResTree", None, vec![]),
            named("Pair", None, vec![ResolvedTy::I64]),
            named("Pair", None, vec![conn()]),
            named("Missing", None, vec![]),
            closure_over(vec![conn()]),
            closure_over(vec![ResolvedTy::I64]),
        ];
        for query in queries.iter().chain(queries.iter().rev()) {
            assert_eq!(
                service.classify(query),
                crate::value_class::classify_ty(query, &ClassContext::new(&declarations))
            );
        }
        // A new declaration epoch must not retain a previous refusal.
        let mut updated = declarations;
        updated.insert(
            crate::NominalId::for_test("Missing"),
            DeclaredType::default(),
        );
        let updated = TypeFactService::new(
            TypeFactContext::new(updated, crate::traits::TraitRegistry::new(), HashMap::new()),
            BTreeMap::new(),
        );
        assert_eq!(
            updated.classify(&named("Missing", None, vec![])),
            Ok((ValueClass::BitCopy, CloneKind::Bits))
        );
    }

    /// A classed recursive type publishes a row like any other aggregate: the
    /// missing-key contract is for types the class table refuses, and a legal `indirect`
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
                    args: vec![element],
                    head: crate::TypeHead::Builtin(BuiltinType::Vec),
                    is_opaque: false
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
                &named("Sink", Some(BuiltinType::Sink), vec![ResolvedTy::String]),
                &context
            )
        );
    }

    #[test]
    fn selected_capability_never_derives_after_losing_impl_metadata() {
        let parsed = hew_parser::parse(
            "type Key {\n    id: i64;\n}\n\nimpl Hash for Key {\n    fn hash(self) -> i64 {\n        1\n    }\n}\n",
        );
        let mut checker = crate::Checker::new(crate::module_registry::ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parsed.program);
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        let declaration = output
            .defs
            .lookup_path("Key")
            .expect("checked Key declaration");
        let key = ResolvedTy::Named {
            head: crate::TypeHead::Nominal(crate::NominalHead::new(
                crate::NominalId::of_declaration(declaration),
                "Key",
            )),
            args: Vec::new(),
            is_opaque: false,
        };
        let mut service = TypeFactService::new(output.type_fact_context, output.type_facts);
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

    /// What the ownership-kind mapping owes for a class, as this module's tests read it.
    #[derive(Debug, PartialEq, Eq)]
    enum OwnershipObligation {
        None,
        Owned,
    }

    /// the ownership-kind mapping, restated locally so the assertion above is about the
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
