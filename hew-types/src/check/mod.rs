//! Bidirectional type checker for Hew programs.

use crate::builtin_names::{builtin_named_type, builtin_named_types, BuiltinMethodRuntime};
use crate::error::{SupervisorErrorKind, TypeError, TypeErrorKind};
use crate::module_registry::ModuleError;
use crate::resolved_ty::{BoundaryError, ResolvedTy};
use crate::traits::{MarkerTrait, TraitRegistry};
use crate::ty::{Ty, TypeVar};
use crate::type_facts::{TypeFactContext, TypeFactService, TypeFacts, TypeInstanceKey};
use crate::unify::unify;
use crate::{WasmFeatureDisposition, WasmUnsupportedFeature};
use hew_parser::ast::{
    ActorDecl, ActorInit, Attribute, AttributeArg, BinaryOp, Block, CallArg, ChildSpec, ConstDecl,
    Expr, ExternBlock, ExternFnDecl, FieldDecl, FnDecl, ImplDecl, ImportDecl, ImportSpec, Item,
    LambdaParam, Literal, MachineDecl, MatchArm, Param, Pattern, Program, ReceiveFnDecl,
    RecordDecl, RecordKind, RestartPolicy, Span, Spanned, Stmt, StringPart, SupervisorDecl,
    SupervisorStrategy, TraitBound, TraitDecl, TraitItem, TypeBodyItem, TypeDecl, TypeDeclKind,
    TypeExpr, TypeParam, UnaryOp, VariantKind, WhereClause,
};
use std::collections::{hash_map::Entry, BTreeMap, HashMap, HashSet};
use std::sync::OnceLock;

pub(crate) mod admissibility;
mod branch_join;
mod calls;
mod closure_inference;
mod coerce;
pub mod const_eval;
mod diagnostics;
pub mod dispatch;
pub use self::dispatch::{
    Bound, CallAbiHint, CallTarget, HashMapMethod, HashSetMethod, ImplDef, ImplId, ImplRegistry,
    LookupError, MethodTarget, MethodTargetFamily, ResolvedCall, RuntimeAbi, TyPattern, VecMethod,
};
mod expressions;
mod generics;
mod items;
mod lints;
pub use self::lints::{directive_suppresses, LintId, LintLevel, LintLevels, LintSources};
mod methods;
mod nominal_identity;
pub use self::methods::collection_dispatch_registry_for_tests;
use self::nominal_identity::NominalOrigin;
mod patterns;
mod registration;
pub use registration::intrinsic_floor_modules;
mod resolution;
mod statements;
#[cfg(test)]
mod tests;
mod type_members;
use self::type_members::DottedTypeMemberUse;
mod types;
mod util;
mod visibility;

use self::types::{
    ActorFieldInfo, ActorInitParamInfo, ConstValue, DeferredBoundCheck, DeferredCastCheck,
    DeferredChannelMethodRewrite, DeferredHashMapAdmission, DeferredHashSetAdmission,
    DeferredInferenceHole, DeferredMonomorphicSite, DeferredVecAdmission, ImplAliasEntry,
    ImplAliasScope, ImportKey, IndexContext, IntegerTypeInfo, PendingLoweringFact,
    SourceExternDeclaration, TraitAssociatedTypeInfo, TraitInfo, TypeAliasDef, TypeParamScope,
};
pub use self::types::{
    ActorMethodKind, ActorStateGuard, AllocationClass, ArmResolution, AssignTargetKind,
    AssignTargetShape, CaptureModeOrigin, Checker, ChildKind, ChildSlot, ClosureCaptureFact,
    ClosureCaptureMode, ClosureEscapeFact, ClosureEscapeKind, ClosureEscapeRule, DynAssocBinding,
    DynCoercion, DynMethodCall, DynVtableEntry, DynVtableKey, EntryCallableInstance,
    EntryDisplayTarget, EntryExitAction, EntryExitPlan, EntryIntegerType, ExecutionContextReader,
    ExternMethodCallIdentity, FnSig, MachineMethodKind, MathGenericOp, MethodCallReceiverKind,
    MethodCallRewrite, NumericMethodFamily, NumericMethodLowering, NumericMethodOp,
    NumericSignedness, NumericWidth, OpaqueResourceCandidateGraph,
    OpaqueResourceLifecycleCandidate, OpaqueResourceLifecycleConflict,
    OpaqueResourceLifecycleConflictKind, OptionResultMethod, PatternKind, PatternPlan,
    PayloadBinding, PayloadVariantPattern, PlanField, PlanSub, PoolAccessor, PoolAccessorKind,
    RcIntrinsicOp, SpanKey, StackHint, TryConversionKind, TryWidthCastLowering, TypeCheckOutput,
    TypeDef, TypeDefKind, UserComparisonDispatch, VariantDef, VariantMatch, VecHigherOrderOp,
    WidthCastKind, WidthCastLowering, WireCodecDirection, WireFieldLayout, WireFieldPresence,
    WireLayoutEntry, WireLayoutTable, WireTextFormat,
};
use self::util::{
    collect_unresolved_inference_vars, extract_float_literal_value, extract_integer_literal_value,
    first_infer_span_in_extern_fn, first_infer_span_in_type_expr, float_fits_type,
    integer_fits_type, integer_type_info, integer_type_range, is_float_literal, is_integer_literal,
    lookup_scoped_item, scoped_module_item_name,
};
use crate::lowering_facts::{LoweringFact, LoweringFactError};

static BUILTIN_FUNCTION_NAMES: OnceLock<HashSet<String>> = OnceLock::new();

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum TypeResolutionContext {
    Ordinary,
    ExternSignature,
}

/// How one module's nominal declarations are spelled in the identity table.
///
/// A nominal (type, trait, record, actor, supervisor, machine, const) has one
/// PRIMARY spelling — the one `ResolvedTy::Named` carries for it, and the one
/// every declaration-derived downstream key renders from — plus, in the two
/// flattened cases, a second spelling recorded on the same occurrence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum NominalNamespace {
    /// The root compilation unit: its nominals are BARE, and the root's own
    /// module path is the second spelling.
    RootBare,
    /// An ordinary graph module: its nominals are owner-qualified and no
    /// other spelling reaches them.
    Owned,
    /// A file import (`import "helper.hew";`) flattened into the root: its
    /// nominals keep their own file's qualification and are ALSO reachable
    /// bare, because the flattened items land in the root's namespace.
    FlattenedFile,
}

impl NominalNamespace {
    /// The namespace an import contributes into: a file import
    /// (empty module path) flattens, a module import does not.
    pub(super) fn for_import(file_import: bool) -> Self {
        if file_import {
            Self::FlattenedFile
        } else {
            Self::Owned
        }
    }
}

#[must_use]
pub fn builtin_function_names() -> &'static HashSet<String> {
    BUILTIN_FUNCTION_NAMES.get_or_init(|| {
        let mut checker = Checker::default();
        checker.register_builtins();
        let mut names: HashSet<String> = checker
            .fn_sigs
            .keys()
            .filter(|name| !name.contains('.') && !name.contains("::"))
            .cloned()
            .collect();
        for builtin in builtin_named_types() {
            for method in builtin.methods {
                match method.runtime {
                    BuiltinMethodRuntime::None => {}
                    BuiltinMethodRuntime::Fixed(symbol) => {
                        if !symbol.contains('.') && !symbol.contains("::") {
                            names.insert(symbol.to_string());
                        }
                    }
                    BuiltinMethodRuntime::IntegerOverload {
                        default_symbol,
                        integer_symbol,
                    } => {
                        for symbol in [default_symbol, integer_symbol] {
                            if !symbol.contains('.') && !symbol.contains("::") {
                                names.insert(symbol.to_string());
                            }
                        }
                    }
                    BuiltinMethodRuntime::ElementOverload {
                        string_symbol,
                        bytes_symbol,
                    } => {
                        for symbol in [string_symbol, bytes_symbol] {
                            if !symbol.contains('.') && !symbol.contains("::") {
                                names.insert(symbol.to_string());
                            }
                        }
                    }
                }
            }
        }
        names
    })
}

fn value_type_kind_label(kind: TypeDefKind) -> &'static str {
    match kind {
        TypeDefKind::Enum => "enum",
        TypeDefKind::Record => "record",
        TypeDefKind::Struct | TypeDefKind::Actor | TypeDefKind::Machine => "type",
    }
}

fn resolve_builtin_result_output_type_args(ok_ty: Ty, err_ty: Ty) -> Option<(Ty, Ty)> {
    let ok_unresolved = ok_ty.has_inference_var();
    let err_unresolved = err_ty.has_inference_var();
    match (ok_unresolved, err_unresolved) {
        (false, false) => Some((ok_ty, err_ty)),
        (false, true) => Some((ok_ty.clone(), ok_ty)),
        (true, false) => Some((err_ty.clone(), err_ty)),
        (true, true) => None,
    }
}

fn patch_builtin_result_output_type(_ty: Ty, ok_ty: &Ty, err_ty: &Ty) -> Ty {
    Ty::result(ok_ty.clone(), err_ty.clone())
}

/// The §1.1 declaration lookup, answered from the checker's own tables.
///
/// `is_resource` / `is_linear` carry the `#[resource]` / `#[linear]` marker and
/// `type_defs` carries the field and variant types the Aggregate rule joins.
/// Both live in this crate, so the class authority reads them directly rather
/// than through a second table.
pub(crate) struct CheckerClassDeclarations<'a> {
    registry: &'a TraitRegistry,
    type_defs: &'a HashMap<String, crate::check::types::TypeDef>,
    /// `#[opaque]` declarations written in this program.
    user_opaque_type_names: &'a HashSet<String>,
    /// Resolves an imported `#[opaque]` handle spelling, which the set above
    /// does not carry.
    module_registry: &'a crate::module_registry::ModuleRegistry,
}

impl crate::value_class::ClassDeclarations for CheckerClassDeclarations<'_> {
    fn declared_type(&self, name: &str) -> Option<crate::value_class::DeclaredType> {
        use crate::value_class::{DeclarationMarker, DeclaredType};

        // The `#[opaque]` attribute is a declaration fact, carried for a
        // program's own declarations by the checker's set and for an imported
        // handle by the module registry.
        let is_opaque = self.user_opaque_type_names.contains(name)
            || name
                .split_once('.')
                .is_some_and(|(_, leaf)| self.user_opaque_type_names.contains(leaf))
            || self.module_registry.is_handle_type(name);
        let marker = if self.registry.is_resource(name) {
            DeclarationMarker::Resource
        } else if self.registry.is_linear(name) {
            DeclarationMarker::Linear
        } else {
            DeclarationMarker::None
        };
        // `type_defs` is keyed by both the qualified path and its bare-name
        // twin; a resolved type may carry either spelling.
        let definition = self.type_defs.get(name).or_else(|| {
            name.split_once('.')
                .and_then(|(_, leaf)| self.type_defs.get(leaf))
        });
        let Some(definition) = definition else {
            // A marker with no field table still decides the class outright.
            return (marker != DeclarationMarker::None).then(|| DeclaredType {
                marker,
                is_opaque,
                type_params: Vec::new(),
                members: Vec::new(),
            });
        };
        if marker != DeclarationMarker::None {
            return Some(DeclaredType {
                marker,
                is_opaque,
                type_params: definition.type_params.clone(),
                members: Vec::new(),
            });
        }

        let mut member_tys: Vec<Ty> = Vec::new();
        if definition.field_order.is_empty() {
            let mut names: Vec<&String> = definition.fields.keys().collect();
            names.sort();
            member_tys.extend(
                names
                    .into_iter()
                    .filter_map(|n| definition.fields.get(n))
                    .cloned(),
            );
        } else {
            for field in &definition.field_order {
                if let Some(ty) = definition.fields.get(field) {
                    member_tys.push(ty.clone());
                }
            }
        }
        let mut variants: Vec<&String> = definition.variants.keys().collect();
        variants.sort();
        for variant in variants {
            match definition.variants.get(variant) {
                Some(crate::check::types::VariantDef::Unit) | None => {}
                Some(crate::check::types::VariantDef::Tuple(payload)) => {
                    member_tys.extend(payload.iter().cloned());
                }
                Some(crate::check::types::VariantDef::Struct(fields)) => {
                    member_tys.extend(fields.iter().map(|(_, ty)| ty.clone()));
                }
            }
        }

        // §1.1's recursion cut (`classify_declaration`'s `walk.on_path`) keys a
        // declaration by the exact name this lookup was asked for. A member
        // that refers back to its own declaration is spelled bare inside the
        // declaration's own source (name resolution never qualifies a
        // self-reference), while an importer reaches the declaration through
        // its qualified key — so the recursive occurrence and the declaration
        // it recurses into carry two different keys and the cut never fires.
        // `name` here is that qualified key whenever the lookup found one, so
        // canonicalizing every bare member reference that also has a
        // `{prefix}.{bare}` twin in `type_defs` gives the whole declaration
        // one spelling, matching `ir-ladder.md` §1.1: one canonical key per
        // declaration, the qualified one, with the bare twin staying a lookup
        // alias never used as a fact key.
        let module_prefix = name.rsplit_once('.').map(|(prefix, _)| prefix);
        let mut members = Vec::with_capacity(member_tys.len());
        for ty in member_tys {
            // A member the boundary cannot render leaves the whole declaration
            // unclassifiable: an aggregate over the members that happened to
            // convert would be a guess.
            let resolved = ResolvedTy::from_ty(&ty.materialize_literal_defaults()).ok()?;
            let resolved = match module_prefix {
                Some(prefix) => canonicalize_member_ty(resolved, prefix, self.type_defs),
                None => resolved,
            };
            members.push(resolved);
        }
        // A declaration with no fields and no variants is still a declaration:
        // the fieldless `#[opaque]` handle is the shape `std/path.hew` and
        // `std/semaphore.hew` ship, and §1.1 decides it by its marker - an
        // FFI pass-through id with no marker classes `BitCopy`, one with
        // `#[resource]`/`#[linear]` classes over that marker instead.
        // Answering `None` here sent the class rule to the builtin-name
        // fallback instead, so an `#[opaque] type Location {}` published
        // `BitCopy` while an identical `Handle` published nothing. The
        // builtin row is for a name the checker has no declaration of at
        // all.
        Some(DeclaredType {
            marker,
            is_opaque,
            type_params: definition.type_params.clone(),
            members,
        })
    }
}

/// Rewrite a member type's bare `Named` occurrences to the declaring module's
/// canonical (qualified) spelling, so a declaration's own recursive
/// occurrence carries the same key its declaration is looked up under.
///
/// Only rewrites a bare name that has a `{prefix}.{bare}` twin registered in
/// `type_defs` — an unqualified reference to a type outside this module (a
/// builtin, or a name `type_defs` never published under the prefix) is left
/// exactly as resolved.
fn canonicalize_member_ty(
    ty: ResolvedTy,
    prefix: &str,
    type_defs: &HashMap<String, crate::check::types::TypeDef>,
) -> ResolvedTy {
    let rewrite_name = |name: String| -> String {
        if name.starts_with(prefix) && name[prefix.len()..].starts_with('.') {
            return name;
        }
        let qualified = format!("{prefix}.{name}");
        if type_defs.contains_key(&qualified) {
            qualified
        } else {
            name
        }
    };
    match ty {
        ResolvedTy::Named {
            name,
            args,
            builtin,
            is_opaque,
        } => {
            let args = args
                .into_iter()
                .map(|a| canonicalize_member_ty(a, prefix, type_defs))
                .collect();
            // A builtin already carries its identity in `builtin`; the name
            // string is display-only there and rewriting it would be a
            // second, redundant identity authority.
            let name = if builtin.is_none() {
                rewrite_name(name)
            } else {
                name
            };
            ResolvedTy::Named {
                name,
                args,
                builtin,
                is_opaque,
            }
        }
        ResolvedTy::Tuple(elements) => ResolvedTy::Tuple(
            elements
                .into_iter()
                .map(|e| canonicalize_member_ty(e, prefix, type_defs))
                .collect(),
        ),
        ResolvedTy::Array(element, len) => ResolvedTy::Array(
            Box::new(canonicalize_member_ty(*element, prefix, type_defs)),
            len,
        ),
        ResolvedTy::Slice(element) => ResolvedTy::Slice(Box::new(canonicalize_member_ty(
            *element, prefix, type_defs,
        ))),
        ResolvedTy::Function { params, ret } => ResolvedTy::Function {
            params: params
                .into_iter()
                .map(|p| canonicalize_member_ty(p, prefix, type_defs))
                .collect(),
            ret: Box::new(canonicalize_member_ty(*ret, prefix, type_defs)),
        },
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => ResolvedTy::Closure {
            params: params
                .into_iter()
                .map(|p| canonicalize_member_ty(p, prefix, type_defs))
                .collect(),
            ret: Box::new(canonicalize_member_ty(*ret, prefix, type_defs)),
            captures: captures
                .into_iter()
                .map(|c| canonicalize_member_ty(c, prefix, type_defs))
                .collect(),
        },
        ResolvedTy::Pointer {
            is_mutable,
            pointee,
        } => ResolvedTy::Pointer {
            is_mutable,
            pointee: Box::new(canonicalize_member_ty(*pointee, prefix, type_defs)),
        },
        ResolvedTy::Borrow { pointee } => ResolvedTy::Borrow {
            pointee: Box::new(canonicalize_member_ty(*pointee, prefix, type_defs)),
        },
        ResolvedTy::TraitObject { traits } => ResolvedTy::TraitObject {
            traits: traits
                .into_iter()
                .map(|bound| crate::resolved_ty::ResolvedTraitBound {
                    trait_name: bound.trait_name,
                    args: bound
                        .args
                        .into_iter()
                        .map(|a| canonicalize_member_ty(a, prefix, type_defs))
                        .collect(),
                    assoc_bindings: bound.assoc_bindings,
                })
                .collect(),
        },
        ResolvedTy::Task(inner) => {
            ResolvedTy::Task(Box::new(canonicalize_member_ty(*inner, prefix, type_defs)))
        }
        other => other,
    }
}

/// The §6.3 replacement for `Ty::is_copy` and the `implements_marker(Copy)`
/// disjunct beside it: a value carries no ownership obligation exactly when its
/// §1.1 class is `BitCopy` or `View`.
///
/// `View` is included so an extern `Pointer` keeps the Copy verdict
/// `Ty::is_copy` gave it. A type the class rule refuses fails closed as owning.
pub(crate) fn class_is_non_owning(
    ty: &Ty,
    declarations: &dyn crate::value_class::ClassDeclarations,
) -> bool {
    // A type the boundary cannot render is a checker-internal state - a leaked
    // error, an unresolved inference variable, an unresolved associated
    // projection - and carries an obligation until something proves it does
    // not. There is no "unresolved therefore Copy" verdict.
    let Ok(resolved) = ResolvedTy::from_ty(&ty.materialize_literal_defaults()) else {
        return false;
    };
    let context = crate::value_class::ClassContext::new(declarations);
    matches!(
        crate::value_class::ValueClass::of_ty(&resolved, &context),
        Ok(crate::value_class::ValueClass::BitCopy | crate::value_class::ValueClass::View)
    )
}

/// Whether a declaration walk rooted at `ty` reaches no cycle.
///
/// The eligibility walks in `hash_eligibility` and `eq_eligibility` descend
/// through `type_defs` without a visited set, so a self-recursive nominal makes
/// them diverge. This answers, fail-closed, whether it is safe to ask them.
#[expect(
    clippy::too_many_lines,
    reason = "the walk enumerates every Ty and VariantDef shape it descends through"
)]
pub(crate) fn declaration_walk_terminates(
    ty: &ResolvedTy,
    type_defs: &HashMap<String, crate::check::types::TypeDef>,
) -> bool {
    fn definition<'a>(
        name: &str,
        type_defs: &'a HashMap<String, crate::check::types::TypeDef>,
    ) -> Option<&'a crate::check::types::TypeDef> {
        type_defs.get(name).or_else(|| {
            name.split_once('.')
                .and_then(|(_, local)| type_defs.get(local))
        })
    }

    fn nominal_names(ty: &ResolvedTy, out: &mut Vec<String>) {
        if let ResolvedTy::Named { name, .. } = ty {
            out.push(name.clone());
        }
        let mut components = Vec::new();
        crate::type_facts::push_type_components(ty, &mut components);
        for component in &components {
            nominal_names(component, out);
        }
    }

    fn member_names(ty: &Ty, out: &mut Vec<String>) {
        match ty {
            Ty::Named { name, args, .. } => {
                out.push(name.clone());
                for arg in args {
                    member_names(arg, out);
                }
            }
            Ty::Tuple(elements) => {
                for element in elements {
                    member_names(element, out);
                }
            }
            Ty::Array(inner, _)
            | Ty::Slice(inner)
            | Ty::Task(inner)
            | Ty::Pointer { pointee: inner, .. }
            | Ty::Borrow { pointee: inner } => member_names(inner, out),
            Ty::Function { params, ret } => {
                for param in params {
                    member_names(param, out);
                }
                member_names(ret, out);
            }
            Ty::Closure {
                params,
                ret,
                captures,
            } => {
                for param in params.iter().chain(captures) {
                    member_names(param, out);
                }
                member_names(ret, out);
            }
            Ty::AssocType { base, .. } => member_names(base, out),
            _ => {}
        }
    }

    fn visit(
        name: &str,
        type_defs: &HashMap<String, crate::check::types::TypeDef>,
        stack: &mut Vec<String>,
        done: &mut HashSet<String>,
    ) -> bool {
        if stack.iter().any(|entry| entry == name) {
            return false;
        }
        if done.contains(name) {
            return true;
        }
        let Some(definition) = definition(name, type_defs) else {
            done.insert(name.to_string());
            return true;
        };
        stack.push(name.to_string());
        let mut members: Vec<String> = Vec::new();
        for field in definition.fields.values() {
            member_names(field, &mut members);
        }
        for variant in definition.variants.values() {
            match variant {
                crate::check::types::VariantDef::Unit => {}
                crate::check::types::VariantDef::Tuple(payload) => {
                    for ty in payload {
                        member_names(ty, &mut members);
                    }
                }
                crate::check::types::VariantDef::Struct(fields) => {
                    for (_, ty) in fields {
                        member_names(ty, &mut members);
                    }
                }
            }
        }
        let terminates = members
            .iter()
            .all(|member| visit(member, type_defs, stack, done));
        stack.pop();
        if terminates {
            done.insert(name.to_string());
        }
        terminates
    }

    let mut roots = Vec::new();
    nominal_names(ty, &mut roots);
    let mut stack = Vec::new();
    let mut done = HashSet::new();
    roots
        .iter()
        .all(|root| visit(root, type_defs, &mut stack, &mut done))
}

impl Checker {
    /// Select one exact root declaration for the process entry plan.
    pub fn set_entry_selection(&mut self, selection: crate::DeclarationOccurrence) {
        self.entry_selection = Some(selection);
    }

    /// Build the §6.3 fact table over every concrete accepted expression type.
    ///
    /// The walk is closed under a type's own components so a `Vec<Conn>` row is
    /// always accompanied by its `Conn` row: consumers key on the exact type
    /// they hold, and glue emission descends into elements.
    /// Publish the §6.2 fact table, with the refusals §1.1 could not decide.
    ///
    /// A type §1.1 refuses gets no row, and for most refusals that is the whole
    /// statement: the type is a template parameter, or a declaration the
    /// boundary cannot render, and a consumer that misses the key fails closed.
    /// `RecursiveInstantiation` is different - it refuses a declaration the
    /// user wrote, whose members reach it at a growing instantiation - so it is
    /// stated as `E_LIMIT_CLASS_RECURSION` rather than left as an absence, once
    /// per declaration.
    fn build_type_facts(
        &self,
        resolved: &HashMap<SpanKey, ResolvedTy>,
    ) -> (
        TypeFactContext,
        BTreeMap<TypeInstanceKey, TypeFacts>,
        Vec<TypeError>,
    ) {
        let fact_context = self.type_fact_context();
        let mut service = TypeFactService::new(fact_context.clone(), BTreeMap::new());
        // Visited is tracked separately from the table: a type §1.1 refuses
        // gets no row, and keying the walk on the table alone would revisit it
        // for ever through a recursive declaration.
        let mut visited: std::collections::BTreeSet<TypeInstanceKey> =
            std::collections::BTreeSet::new();
        // Each pending type carries the span of the accepted expression it came
        // from, so a refusal names a place in the user's program rather than
        // the start of the file. A component inherits its container's span.
        let mut refusals: BTreeMap<String, TypeError> = BTreeMap::new();
        let mut pending: Vec<(ResolvedTy, Span)> = resolved
            .iter()
            .map(|(key, ty)| (ty.clone(), key.start..key.end))
            .collect();
        pending.sort_by(|left, right| {
            left.0
                .cmp(&right.0)
                .then_with(|| left.1.start.cmp(&right.1.start))
        });
        pending.dedup_by(|left, right| left.0 == right.0);
        while let Some((ty, span)) = pending.pop() {
            let key = TypeInstanceKey(ty.clone());
            if !visited.insert(key.clone()) {
                continue;
            }
            match service.require(&ty) {
                Err(crate::value_class::ClassError::RecursiveInstantiation { name }) => {
                    refusals.entry(format!("recursion:{name}")).or_insert_with(|| {
                        TypeError::new(
                            TypeErrorKind::ClassRecursion,
                            span.clone(),
                            format!(
                                "E_LIMIT_CLASS_RECURSION: `{name}` reaches itself through its own members at a growing instantiation, so it has no value class and no ownership obligation can be decided for it"
                            ),
                        )
                    });
                }
                // §1.1's `UnknownDeclaration` on a name `type_defs` actually
                // holds a definition for is a class-rule/registration
                // disagreement over a real declaration, not a legitimate
                // absence - `declared_type` found no answer for a name the
                // checker itself registered. D369 makes that never silent;
                // it fires loudly in debug/test/CI and is compiled out of
                // release, mirroring the W4.047 totality net above.
                Err(crate::value_class::ClassError::UnknownDeclaration { name })
                    if self.type_defs.contains_key(&name)
                        || name
                            .split_once('.')
                            .is_some_and(|(_, leaf)| self.type_defs.contains_key(leaf)) =>
                {
                    debug_assert!(
                        false,
                        "D369 totality gap: `{name}` is a declaration `type_defs` holds, \
                         but the §1.1 class rule refused it as `UnknownDeclaration` - a \
                         class-rule refusal on a user-written declaration must never be \
                         silent"
                    );
                }
                // Every other refusal is a legitimate absence a consumer
                // fails closed on: a template parameter the instance service
                // substitutes first, a name genuinely outside `type_defs`
                // (a checker-internal state), or a compiler-internal carrier
                // that is never the type of a value.
                Ok(_) | Err(_) => {}
            }
            let mut components = Vec::new();
            crate::type_facts::push_type_components(&ty, &mut components);
            pending.extend(
                components
                    .into_iter()
                    .map(|component| (component, span.clone())),
            );
        }
        (
            fact_context,
            service.into_rows(),
            refusals.into_values().collect(),
        )
    }

    /// Snapshot the declaration authority used to classify accepted types.
    fn type_fact_context(&self) -> TypeFactContext {
        let declarations = self.class_declarations();
        let mut names: std::collections::BTreeSet<String> =
            self.type_defs.keys().cloned().collect();
        names.extend(self.registry.resource_type_names().iter().cloned());
        names.extend(self.user_opaque_type_names.iter().cloned());
        names.extend(self.module_registry.all_handle_types());
        let rendered = names
            .into_iter()
            .filter_map(|name| {
                crate::value_class::ClassDeclarations::declared_type(&declarations, &name)
                    .map(|declaration| (name, declaration))
            })
            .collect();
        TypeFactContext::new(rendered, self.registry.clone(), self.type_defs.clone())
    }

    /// The §1.1 declaration lookup backed by this checker's tables.
    pub(crate) fn class_declarations(&self) -> CheckerClassDeclarations<'_> {
        CheckerClassDeclarations {
            registry: &self.registry,
            type_defs: &self.type_defs,
            user_opaque_type_names: &self.user_opaque_type_names,
            module_registry: &self.module_registry,
        }
    }

    /// Whether a checker-internal type carries no ownership obligation, per
    /// the §1.1 class table.
    pub(crate) fn ty_is_non_owning(&self, ty: &Ty) -> bool {
        class_is_non_owning(ty, &self.class_declarations())
    }

    /// Log function names that accept keyword arguments for structured fields.
    const LOG_KWARGS_FUNCTIONS: &'static [&'static str] = &[
        // C extern (used by codegen interception for legacy compatibility)
        "hew_log_emit",
        // Wrapper function (clean) names from log.hew
        "error",
        "warn",
        "info",
        "debug",
        "trace",
    ];

    pub(super) fn record_root_value_binding(&mut self, name: &str) {
        if self.current_module.is_none() {
            self.root_value_bindings.insert(name.to_string());
        }
    }

    /// The canonical module identity that owns items registered/checked in the
    /// current context (rc1-F1 stage A): the current graph module, or the ROOT
    /// unit's minted identity. This is the fn-sig mint chokepoint — root free
    /// functions key `{root_module}.{name}` exactly as they would when their
    /// module is imported (`scoped_module_item_name`'s bare-for-root behaviour
    /// is dead for the fn-sig family). `None` only for bare-by-design roots
    /// (REPL fragments, source-less roots).
    pub(super) fn canonical_fn_owner(&self) -> Option<&str> {
        self.current_module
            .as_deref()
            .or_else(|| self.identity.root_module_path())
    }

    /// Canonical-first `fn_sigs` key for a bare free-fn spelling written in
    /// ROOT context. Returns the root-canonical key only when it is actually
    /// registered, so bare builtin/extern registrations keep resolving
    /// unchanged (the bare rung is the builtin/extern floor, not a root
    /// fallback).
    pub(super) fn root_canonical_fn_sig_key(&self, name: &str) -> Option<String> {
        if self.current_module.is_some() {
            return None;
        }
        let scoped = scoped_module_item_name(self.identity.root_module_path(), name)?;
        self.fn_sigs.contains_key(&scoped).then_some(scoped)
    }

    /// Mint the declaration-table identity for a free function owned by a
    /// known lexical scope. A source-less root deliberately keeps its bare
    /// key; imported bindings are resolution facts and must never rename the
    /// declaration that shadows them.
    pub(super) fn declared_fn_identity(owner: Option<&str>, name: &str) -> String {
        owner
            .and_then(|owner| scoped_module_item_name(Some(owner), name))
            .unwrap_or_else(|| name.to_string())
    }

    /// Return the declaration-table identity for a free function.
    ///
    /// `owner` is supplied while minting a declaration or resolving a known
    /// lexical owner. Without an owner, an already-resolved signature key is
    /// re-anchored through named-import aliases or its declaration record.
    /// Actor and method identities use `::` and remain unchanged.
    pub(super) fn canonical_fn_identity(&self, owner: Option<&str>, name: &str) -> String {
        if name.contains("::") {
            return name.to_string();
        }
        if let Some(owner) = owner {
            return Self::declared_fn_identity(Some(owner), name);
        }
        if let Some((_, declaring_module)) = self.fn_def_spans.get(name) {
            return Self::declared_fn_identity(declaring_module.as_deref(), name);
        }
        if let Some(source) = self.import_fn_name_aliases.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            name.to_string(),
        )) {
            return source.clone();
        }
        name.to_string()
    }

    /// Record one call edge after canonicalizing both endpoints through the
    /// declaration identity authority.
    pub(super) fn record_call_edge(&mut self, target: &str) {
        let Some(source) = self.current_function.clone() else {
            return;
        };
        let source_identity = self.canonical_fn_identity(None, &source);
        let target_identity = self.canonical_fn_identity(None, target);
        self.call_graph
            .entry(source_identity)
            .or_default()
            .insert(target_identity);
    }

    /// When `key` is a ROOT-owned canonical free-fn key (`{root}.{leaf}`),
    /// return its bare leaf; with no minted root identity, a bare free-fn key
    /// IS root-owned and returns itself. `None` for module/method keys.
    ///
    /// Two consumers: root-fn detection where the legacy code tested "key has
    /// no dot" (dead-code entry points and warn set), and the LEGACY RENDER of
    /// root declarations at publication boundaries (published `CallTarget`
    /// `DefId`s, published `fn_sigs` keys, diagnostics), where downstream
    /// consumers still resolve root items by bare spelling.
    /// WHEN OBSOLETE: the rc2 identity continuation's render-canonicalization
    /// stage re-keys those consumers by `DefId`; the render half of this helper
    /// is deleted with them. The detection half stays until then.
    pub(super) fn root_owned_fn_leaf<'k>(&self, key: &'k str) -> Option<&'k str> {
        if key.contains("::") {
            return None;
        }
        match self.identity.root_module_path() {
            Some(root) => key
                .strip_prefix(root)
                .and_then(|rest| rest.strip_prefix('.'))
                .filter(|leaf| !leaf.contains('.')),
            None => (!key.contains('.')).then_some(key),
        }
    }

    /// Mint the compile's module identities before declaration inventory.
    ///
    /// One minting authority, run once at `check_program` entry: every graph
    /// module is interned under its canonical dotted identity (deduped by
    /// canonical source, so dual-import/peer-assembly spellings of one source
    /// resolve to one `ModuleId`), then the ROOT compilation unit is minted
    /// from its canonical source — reusing a graph module's identity when the
    /// root IS an importable source (root-vs-import provenance invariance).
    ///
    /// Source-less roots (synthetic stdlib floor roots from
    /// `rewrite_direct_stdlib_module_root`, unit-test programs without source
    /// paths) receive the reserved synthetic occurrence authority rather than
    /// borrowing a display-name namespace.
    fn mint_module_identities(&mut self, program: &Program) {
        self.identity = crate::identity::IdentityTable::new();
        self.canonical_module_spellings.clear();
        let Some(module_graph) = &program.module_graph else {
            self.identity.mint_synthetic_root();
            return;
        };
        // Deterministic mint order: the topo order, root last.
        for mod_id in &module_graph.topo_order {
            if *mod_id == module_graph.root {
                continue;
            }
            let Some(module) = module_graph.modules.get(mod_id) else {
                continue;
            };
            let dotted = mod_id.path.join(".");
            let canonical = crate::module_registry::canonical_source_module_identity(
                &dotted,
                &module.source_paths,
            );
            // `import std.channel.channel;` names the primary file of the
            // directory module `std.channel`, and registration keys that
            // module's items by the spelling the import used. Record the pair
            // now, while both are in hand, so declaration lookup can resolve
            // one through the other.
            if canonical != dotted {
                self.canonical_module_spellings
                    .insert(dotted.clone(), canonical.clone());
            }
            self.identity.mint_module(&canonical, &module.source_paths);
        }
        // Second pass — per-file identities for directory modules' peer
        // files (rc1-F1 stage C): a peer file's declarations carry the
        // FILE's identity, so one declaration reached through peer assembly
        // and through a direct submodule import mints one owner. This runs
        // AFTER every graph module minted its primary source, so a peer
        // mint can never claim (and mis-render) another module's primary.
        for mod_id in &module_graph.topo_order {
            if *mod_id == module_graph.root {
                continue;
            }
            let Some(module) = module_graph.modules.get(mod_id) else {
                continue;
            };
            let dotted = mod_id.path.join(".");
            let canonical = crate::module_registry::canonical_source_module_identity(
                &dotted,
                &module.source_paths,
            );
            for source in module.source_paths.iter().skip(1) {
                self.identity.mint_source_file_module(&canonical, source);
            }
        }
        if self.repl_fragment {
            self.identity.mint_synthetic_root();
        } else if let Some(root) = module_graph.modules.get(&module_graph.root) {
            if self.identity.mint_root_module(&root.source_paths).is_none() {
                self.identity.mint_synthetic_root();
            }
        }
    }

    /// Mint source declaration identities before semantic registration.
    ///
    /// This is the only source-declaration mint pass. It walks the resolved
    /// module graph (including per-item defining-file provenance) so a root,
    /// file import, module import, or alias route reaches the same occurrence.
    /// Impl methods are completed at their semantic registration chokepoint,
    /// where the resolved receiver and trait identities are available.
    fn mint_source_declaration_identities(&mut self, program: &Program) {
        if let Some(graph) = &program.module_graph {
            let flat_file_imports = registration::flat_file_import_module_ids(program);
            for module_id in &graph.topo_order {
                let Some(module) = graph.modules.get(module_id) else {
                    continue;
                };
                let dotted = module_id.path.join(".");
                let namespace = if *module_id == graph.root {
                    NominalNamespace::RootBare
                } else if flat_file_imports.contains(module_id) {
                    NominalNamespace::FlattenedFile
                } else {
                    NominalNamespace::Owned
                };
                let assembler = module
                    .source_paths
                    .first()
                    .and_then(|source| self.identity.module_for_source(source))
                    .or_else(|| self.identity.module_for_path(&dotted))
                    .or_else(|| {
                        (*module_id == graph.root)
                            .then(|| self.identity.root_module())
                            .flatten()
                    });
                for (item_index, (item, span)) in module.items.iter().enumerate() {
                    let source = graph
                        .item_source(module_id, item_index)
                        .or_else(|| module.source_paths.first());
                    // The file is the occurrence axis: two peer files whose
                    // items share a span must stay distinguishable.
                    let occurrence_module = source
                        .and_then(|source| self.identity.module_for_source(source))
                        .or(assembler);
                    // The render axis is the module being assembled, which is
                    // the module the checker keys by: `pkg/helpers.hew`'s
                    // `pub fn make` publishes as `pkg.make`. A peer file that
                    // is ALSO importable in its own right is walked a second
                    // time as its own module and records `pkg.helpers.make` as
                    // a second spelling of the same declaration.
                    self.mint_item_declaration_identities(
                        occurrence_module,
                        assembler,
                        namespace,
                        item_index,
                        item,
                        span,
                    );
                }
            }
            // `Program::items` is the checker root surface. Some callers keep
            // the graph root's `items` empty, so it is not interchangeable
            // with `modules[root].items`. Repeated graph-root claims are
            // idempotent. File imports are flattened into `Program::items`
            // only after type checking, so the checker never sees an imported
            // item on this surface.
            let root = self.identity.root_module();
            for (item_index, (item, span)) in program.items.iter().enumerate() {
                self.mint_item_declaration_identities(
                    root,
                    root,
                    NominalNamespace::RootBare,
                    item_index,
                    item,
                    span,
                );
            }
        } else {
            let root = self.identity.root_module();
            for (item_index, (item, span)) in program.items.iter().enumerate() {
                self.mint_item_declaration_identities(
                    root,
                    root,
                    NominalNamespace::RootBare,
                    item_index,
                    item,
                    span,
                );
            }
        }
    }

    pub(super) fn current_declaration_module(&self) -> Option<crate::ModuleId> {
        self.current_item_source
            .as_deref()
            .and_then(|source| self.identity.module_for_source(source))
            .or_else(|| {
                self.current_module
                    .as_deref()
                    .and_then(|module| self.identity.module_for_path(module))
            })
            .or_else(|| {
                // The module may be keyed by an import spelling that is not
                // its canonical owner; the identities were minted under the
                // owner.
                self.current_module
                    .as_deref()
                    .and_then(|module| self.canonical_module_spellings.get(module))
                    .and_then(|canonical| self.identity.module_for_path(canonical))
            })
            .or_else(|| self.identity.root_module())
    }

    /// Rewrite a declaration path whose module part is an import spelling of
    /// a differently-owned module into that module's canonical spelling.
    fn canonical_spelling_of(&self, path: &str) -> Option<String> {
        let (module, leaf) = path.rsplit_once('.')?;
        let canonical = self.canonical_module_spellings.get(module)?;
        Some(format!("{canonical}.{leaf}"))
    }

    pub(super) fn require_declaration_path(
        &mut self,
        path: &str,
        span: &std::ops::Range<usize>,
    ) -> Option<crate::DefId> {
        if let Some(declaration) = self.identity.declaration_by_path(path) {
            return Some(declaration.clone());
        }
        // A module reached under an import spelling that is not its canonical
        // owner keys its registrations by that spelling, while its
        // declarations were minted under the owner. Resolve the one through
        // the other rather than leaving the lookup to fail closed on a name
        // the checker itself produced.
        if let Some(declaration) = self
            .canonical_spelling_of(path)
            .and_then(|canonical| self.identity.declaration_by_path(&canonical))
        {
            return Some(declaration.clone());
        }
        self.errors.push(TypeError::new(
            TypeErrorKind::InvalidOperation,
            span.clone(),
            format!("checker identity table has no source declaration `{path}`"),
        ));
        None
    }

    pub(super) fn require_declaration_occurrence(
        &mut self,
        span: &std::ops::Range<usize>,
        kind: crate::DeclarationKind,
        ordinal: usize,
    ) -> Option<crate::DefId> {
        let occurrence = crate::DeclarationOccurrence::new_with_synthetic_ordinal(
            self.current_declaration_module(),
            span,
            self.current_item_ordinal,
            kind,
            ordinal,
        );
        if let Some(declaration) = self.identity.declaration(occurrence) {
            return Some(declaration.clone());
        }
        self.errors.push(TypeError::new(
            TypeErrorKind::InvalidOperation,
            span.clone(),
            format!(
                "checker identity table has no {kind:?} declaration at exact source occurrence"
            ),
        ));
        None
    }

    fn selected_entry_item<'a>(
        &mut self,
        program: &'a Program,
    ) -> Option<(crate::DeclarationOccurrence, &'a std::ops::Range<usize>)> {
        let selected_entry = self.entry_selection?;
        let selected_entry = if selected_entry.module().is_none() {
            selected_entry.with_module(self.identity.root_module())
        } else {
            selected_entry
        };
        let matched = program
            .items
            .iter()
            .enumerate()
            .find_map(|(item_index, (item, span))| {
                let Item::Function(_) = item else {
                    return None;
                };
                let occurrence = crate::DeclarationOccurrence::new_with_synthetic_ordinal(
                    self.identity.root_module(),
                    span,
                    item_index,
                    crate::DeclarationKind::Function,
                    0,
                );
                (selected_entry == occurrence).then_some((occurrence, span))
            });
        if matched.is_none() {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                selected_entry.span(),
                "selected process entry occurrence is not a root function in this compilation",
            ));
        }
        matched
    }

    fn classify_entry_exit_action(
        &mut self,
        return_type: Ty,
        span: &std::ops::Range<usize>,
        resolved_fn_sigs: &HashMap<String, FnSig>,
    ) -> Option<EntryExitAction> {
        let resolved_return_type = ResolvedTy::from_ty(&return_type).ok();
        match return_type {
            Ty::Unit => Some(EntryExitAction::Unit),
            integer if EntryIntegerType::from_ty(&integer).is_some() => Some(
                EntryExitAction::Integer(EntryIntegerType::from_ty(&integer)?),
            ),
            Ty::Named {
                builtin: Some(crate::BuiltinType::Result),
                args,
                ..
            } if matches!(args.as_slice(), [Ty::Unit, _]) => {
                let error_ty = args[1].clone();
                if !self.type_satisfies_trait_bound(&error_ty, "Error") {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::BoundsNotSatisfied,
                        span.clone(),
                        format!(
                            "process entry error type `{}` does not satisfy the bound `Error`",
                            error_ty.user_facing()
                        ),
                    ));
                    return None;
                }
                let Some((display_declaration, display_signature_key)) =
                    self.trait_impl_method_declaration(&error_ty, "Display", "fmt")
                else {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::BoundsNotSatisfied,
                        span.clone(),
                        format!(
                            "process entry error type `{}` has no resolved Display format target",
                            error_ty.user_facing()
                        ),
                    ));
                    return None;
                };
                let resolved_error_ty = ResolvedTy::from_ty(&error_ty).ok()?;
                let type_args = match &resolved_error_ty {
                    ResolvedTy::Named { args, .. } => args.clone(),
                    _ => Vec::new(),
                };
                let Some(display_signature) = resolved_fn_sigs.get(&display_signature_key) else {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        span.clone(),
                        format!(
                            "checker has no resolved signature for entry Display target `{}`",
                            display_declaration.display_name()
                        ),
                    ));
                    return None;
                };
                let instance = if display_signature.type_params.is_empty() {
                    EntryCallableInstance::Declared
                } else {
                    EntryCallableInstance::Generic { type_args }
                };
                Some(EntryExitAction::Result {
                    result_ty: resolved_return_type?,
                    error_ty: resolved_error_ty,
                    display: EntryDisplayTarget {
                        declaration: display_declaration,
                        instance,
                    },
                })
            }
            unsupported => {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span.clone(),
                    format!(
                        "process entry must return `()`, an integer, or `Result<(), E>`; found `{}`",
                        unsupported.user_facing()
                    ),
                ));
                None
            }
        }
    }

    fn classify_entry_exit_plan(
        &mut self,
        program: &Program,
        resolved_fn_sigs: &HashMap<String, FnSig>,
    ) -> Option<EntryExitPlan> {
        let (occurrence, span) =
            if self.entry_selection.is_some() {
                self.selected_entry_item(program)?
            } else {
                program.items.iter().enumerate().find_map(
                    |(item_index, (item, span))| match item {
                        Item::Function(declaration)
                            if declaration.name == "main"
                                && declaration.type_params.as_ref().is_none_or(Vec::is_empty) =>
                        {
                            Some((
                                crate::DeclarationOccurrence::new_with_synthetic_ordinal(
                                    self.identity.root_module(),
                                    span,
                                    item_index,
                                    crate::DeclarationKind::Function,
                                    0,
                                ),
                                span,
                            ))
                        }
                        _ => None,
                    },
                )?
            };
        let entry = self.identity.declaration(occurrence)?.clone();
        let Some(return_type) = resolved_fn_sigs
            .get(entry.full_path())
            .map(|signature| signature.return_type.clone())
        else {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                span.clone(),
                format!(
                    "checker has no resolved signature for process entry `{}`",
                    entry.display_name()
                ),
            ));
            return None;
        };
        let action = self.classify_entry_exit_action(return_type, span, resolved_fn_sigs)?;

        Some(EntryExitPlan { entry, action })
    }

    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive source-declaration inventory keeps the identity boundary closed"
    )]
    /// Inventory one parsed item's declarations under `module`.
    ///
    /// `module` is the OCCURRENCE axis: the file the item was parsed from, so
    /// two files whose items share a span stay distinguishable. `owner` is the
    /// PATH axis: the module the checker keys the declaration under. They
    /// differ for a directory module's peer files, whose items are reachable
    /// as `{assembler}.{name}` even though each file has its own identity;
    /// `owner` defaults to `module` everywhere else.
    ///
    /// The declaration path is the spelling the checker keys that declaration
    /// by, so later lookups resolve without a second render:
    /// * free and extern functions are always module-scoped
    ///   (`{module}.{name}`; the root unit's own path for root functions);
    /// * nominal declarations (types, traits, records, actors, supervisors,
    ///   machines, consts) render as [`NominalNamespace`] says, and record
    ///   the other reachable spelling as a second path on the same
    ///   occurrence.
    fn mint_item_declaration_identities(
        &mut self,
        module: Option<crate::ModuleId>,
        owner: Option<crate::ModuleId>,
        namespace: NominalNamespace,
        item_ordinal: usize,
        item: &Item,
        span: &std::ops::Range<usize>,
    ) {
        use crate::{DeclarationKind as Kind, DeclarationOccurrence as Occurrence};

        let module_path = owner.or(module).and_then(|owner| {
            let path = self.identity.module_path(owner);
            (path != "#synthetic-root").then(|| path.to_string())
        });
        let fn_path = |leaf: &str| {
            module_path
                .as_ref()
                .map_or_else(|| leaf.to_string(), |module| format!("{module}.{leaf}"))
        };
        // The primary render is the spelling `ResolvedTy::Named` carries for
        // the nominal downstream, because that is what every later
        // declaration-derived key (`HirTypeDecl::qualified_name`, the MIR
        // layout tables) renders from.
        let owner_path = |leaf: &str| match namespace {
            NominalNamespace::RootBare => leaf.to_string(),
            NominalNamespace::Owned | NominalNamespace::FlattenedFile => fn_path(leaf),
        };
        // The OTHER spelling the same declaration is reachable under. A root
        // nominal is bare but its module still owns it, and the checker's own
        // registries (`trait_defs`, `type_def_spans`, the visibility index)
        // key it `{module}.{leaf}`; a flat-imported nominal is qualified by
        // its file but its items were flattened into the root's namespace, so
        // it also answers bare. Recording the second spelling on the SAME
        // occurrence is what the identity table is for: one declaration,
        // several ways to name it. It never mints a second identity, and a
        // spelling another declaration already owns is refused rather than
        // merged.
        let nominal_alias = |leaf: &str| match namespace {
            NominalNamespace::RootBare => module_path
                .as_ref()
                .map(|module| format!("{module}.{leaf}")),
            NominalNamespace::FlattenedFile => Some(leaf.to_string()),
            NominalNamespace::Owned => None,
        };
        let mut declare = |kind: Kind, ordinal: usize, path: String| {
            let occurrence =
                Occurrence::new_with_synthetic_ordinal(module, span, item_ordinal, kind, ordinal);
            let Err(error) = self.identity.declare(occurrence, path.clone()) else {
                return;
            };
            let crate::identity::DeclarationIdentityError::PathAlreadyDeclared {
                established_occurrence,
                ..
            } = &error;
            // An `extern "C"` symbol is the one declaration form where two
            // occurrences under one path are genuinely one declaration: the
            // linker binds every call to a single implementation, the checker
            // keys every declaration of the symbol by the same fn-sig path,
            // and the extern table resolves the redeclaration against the
            // established ABI contract. Peer files of one directory module
            // routinely re-declare a runtime symbol, so binding the further
            // occurrence is what keeps their declarations resolvable.
            if kind == Kind::ExternFunction {
                self.identity.bind_redeclaration(occurrence, &path);
                return;
            }
            // Everything else is a redefinition. Registration reports the
            // ones it can see, which is one file at a time; a collision
            // ACROSS files that share one namespace — two peer files of a
            // directory module, or a root and the file imports flattened into
            // it — is invisible there, because each file registers on its own
            // and only the shared path collides. Report exactly that case
            // here, naming both files, so the user never sees the identity
            // table's internal "no declaration" wording instead of a
            // duplicate definition.
            let established_module = established_occurrence.module();
            if established_module == module
                || !self.reported_declaration_collisions.insert(path.clone())
            {
                return;
            }
            let leaf = path.rsplit(['.', ':']).next().unwrap_or(&path).to_string();
            let established_file = established_module
                .and_then(|module| self.identity.module_source(module))
                .map(|source| source.display().to_string());
            let conflicting_file = module
                .and_then(|module| self.identity.module_source(module))
                .map(|source| source.display().to_string());
            let mut error = TypeError::new(
                TypeErrorKind::DuplicateDefinition,
                span.clone(),
                format!("`{leaf}` is defined multiple times"),
            )
            .with_note_source(
                established_occurrence.span(),
                "previous definition here",
                established_file.clone(),
            );
            if let (Some(established_file), Some(conflicting_file)) =
                (&established_file, &conflicting_file)
            {
                error = error.with_suggestion(format!(
                    "`{established_file}` and `{conflicting_file}` contribute to one \
                     namespace: rename one `{leaf}`"
                ));
            }
            error.source_module = conflicting_file;
            self.errors.push(error);
        };
        match item {
            Item::Import(_) | Item::Impl(_) => {}
            Item::Const(decl) => {
                declare(Kind::Const, 0, owner_path(&decl.name));
                if let Some(alias) = nominal_alias(&decl.name) {
                    declare(Kind::Const, 0, alias);
                }
            }
            Item::Function(decl) => declare(Kind::Function, 0, fn_path(&decl.name)),
            Item::ExternBlock(block) => {
                for (index, decl) in block.functions.iter().enumerate() {
                    declare(Kind::ExternFunction, index, fn_path(&decl.name));
                }
            }
            Item::TypeDecl(decl) => {
                let owner = owner_path(&decl.name);
                declare(Kind::Type, 0, owner.clone());
                if let Some(alias) = nominal_alias(&decl.name) {
                    declare(Kind::Type, 0, alias);
                }
                for (index, method) in decl
                    .body
                    .iter()
                    .filter_map(|item| match item {
                        hew_parser::ast::TypeBodyItem::Method(method) => Some(method),
                        hew_parser::ast::TypeBodyItem::Field { .. }
                        | hew_parser::ast::TypeBodyItem::Variant(_) => None,
                    })
                    .enumerate()
                {
                    declare(Kind::TypeMethod, index, format!("{owner}::{}", method.name));
                }
            }
            Item::TypeAlias(decl) => {
                declare(Kind::TypeAlias, 0, owner_path(&decl.name));
                if let Some(alias) = nominal_alias(&decl.name) {
                    declare(Kind::TypeAlias, 0, alias);
                }
            }
            Item::Record(decl) => {
                declare(Kind::Record, 0, owner_path(&decl.name));
                if let Some(alias) = nominal_alias(&decl.name) {
                    declare(Kind::Record, 0, alias);
                }
            }
            Item::Trait(decl) => {
                let owner = owner_path(&decl.name);
                declare(Kind::Trait, 0, owner.clone());
                if let Some(alias) = nominal_alias(&decl.name) {
                    declare(Kind::Trait, 0, alias);
                }
                for (index, method) in decl
                    .items
                    .iter()
                    .filter_map(|item| match item {
                        hew_parser::ast::TraitItem::Method(method) => Some(method),
                        hew_parser::ast::TraitItem::AssociatedType { .. } => None,
                    })
                    .enumerate()
                {
                    declare(
                        Kind::TraitMethod,
                        index,
                        format!("{owner}::{}", method.name),
                    );
                }
            }
            Item::Actor(decl) => {
                let owner = owner_path(&decl.name);
                declare(Kind::Actor, 0, owner.clone());
                if let Some(alias) = nominal_alias(&decl.name) {
                    declare(Kind::Actor, 0, alias);
                }
                if decl.init.is_some() {
                    declare(Kind::ActorInit, 0, format!("{owner}::<init>"));
                }
                for (index, receive) in decl.receive_fns.iter().enumerate() {
                    declare(
                        Kind::ActorReceive,
                        index,
                        format!("{owner}::{}", receive.name),
                    );
                }
                for (index, method) in decl.methods.iter().enumerate() {
                    declare(
                        Kind::ActorMethod,
                        index,
                        format!("{owner}::{}", method.name),
                    );
                }
            }
            Item::Supervisor(decl) => {
                let owner = owner_path(&decl.name);
                declare(Kind::Supervisor, 0, owner.clone());
                if let Some(alias) = nominal_alias(&decl.name) {
                    declare(Kind::Supervisor, 0, alias);
                }
                declare(
                    Kind::SupervisorBootstrap,
                    0,
                    format!("{owner}::<bootstrap>"),
                );
            }
            Item::Machine(decl) => {
                let owner = owner_path(&decl.name);
                declare(Kind::Machine, 0, owner.clone());
                if let Some(alias) = nominal_alias(&decl.name) {
                    declare(Kind::Machine, 0, alias);
                }
                for (index, state) in decl.states.iter().enumerate() {
                    let state_owner = format!("{owner}::state {}", state.name);
                    declare(Kind::MachineState, index, state_owner.clone());
                    if state.entry.is_some() {
                        declare(
                            Kind::MachineStateEntry,
                            index,
                            format!("{state_owner}::<entry>"),
                        );
                    }
                    if state.exit.is_some() {
                        declare(
                            Kind::MachineStateExit,
                            index,
                            format!("{state_owner}::<exit>"),
                        );
                    }
                }
                for (index, event) in decl.events.iter().enumerate() {
                    declare(
                        Kind::MachineEvent,
                        index,
                        format!("{owner}::event {}", event.name),
                    );
                }
                for (index, _) in decl.transitions.iter().enumerate() {
                    declare(
                        Kind::MachineTransition,
                        index,
                        format!("{owner}::<transition#{index}>"),
                    );
                }
            }
        }
    }

    /// Check the compiler-embedded builtin source under its declaration
    /// authority. User programs must enter through [`Self::check_program`].
    pub fn check_embedded_builtins(&mut self, program: &Program) -> TypeCheckOutput {
        self.checking_embedded_builtins = true;
        let output = self.check_program(program);
        self.checking_embedded_builtins = false;
        output
    }

    /// Pass 3: Check all bodies
    #[expect(
        clippy::too_many_lines,
        reason = "orchestrates the full check pipeline: non-root body pass, root pass, \
                  type resolution, warning emission, and deferred-hole drain; \
                  each phase is a distinct step and extracting further helpers \
                  would only obscure the pipeline order"
    )]
    pub fn check_program(&mut self, program: &Program) -> TypeCheckOutput {
        if self.has_checked_program {
            self.reset_for_program();
        } else {
            self.has_checked_program = true;
        }
        // Mint the compile's module identities FIRST (rc1-F1 stage A): every
        // registration pass below resolves declaration identity through this
        // table, so it must be complete before any key is minted.
        self.mint_module_identities(program);
        // Fresh extern authority per compile (rc1-F1 stage B): contracts are
        // minted by the registration passes below; a stale table would leak
        // symbol ownership across `check_program` runs.
        self.extern_table = crate::extern_table::ExternTable::new();
        // Record concrete stdlib source provenance once, before registration
        // manufactures any compiler-recognised carrier signatures. Module
        // spelling is not authority: a user package may imitate the legacy
        // repeated-basename channel path, but only the source selected by the
        // stdlib search-path resolver may receive Sender/Receiver runtime
        // identity.
        self.canonical_std_module_sources.clear();
        self.canonical_std_root_sources.clear();
        self.module_source_paths.clear();
        self.module_item_sources.clear();
        self.source_file_span_indices.clear();
        self.current_item_source = None;
        self.current_item_ordinal = 0;
        self.file_type_decls.clear();
        if let Some(module_graph) = &program.module_graph {
            let span_indices = module_graph.file_span_indices();
            self.module_item_sources
                .clone_from(&module_graph.item_sources);
            // `hew check std/foo.hew` rewrites the graph root to a synthetic,
            // source-less floor module and promotes the shipped file to its
            // canonical `std.foo` identity. In a normal user program the root
            // always owns source paths, so this distinguishes direct shipped
            // compilation from merely importing a stdlib module.
            let directly_checked_stdlib = module_graph
                .modules
                .get(&module_graph.root)
                .is_some_and(|root| root.source_paths.is_empty());
            for (module_id, module) in &module_graph.modules {
                let module_full_path = module_id.path.join(".");
                if !module.source_paths.is_empty() {
                    self.module_source_paths
                        .insert(module_full_path.clone(), module.source_paths.clone());
                    for source in &module.source_paths {
                        if let Some(index) = span_indices.path_index(source) {
                            self.source_file_span_indices
                                .entry(source.clone())
                                .or_insert(index);
                        }
                    }
                }
                if module.source_paths.iter().any(|source| {
                    crate::module_registry::is_canonical_stdlib_module_source(
                        source,
                        &module_full_path,
                    )
                }) {
                    self.canonical_std_module_sources.insert(module_full_path);
                }
                // A shipped stdlib file can be checked directly as the graph
                // root (`hew check std/string.hew`). Its root module has no
                // dotted identity, so recover its owner from the canonical
                // source path. A user root cannot obtain this authority.
                if directly_checked_stdlib {
                    for owner in module.source_paths.iter().filter_map(|source| {
                        crate::module_registry::canonical_stdlib_module_for_source(source)
                    }) {
                        self.canonical_std_module_sources.insert(owner.clone());
                        self.canonical_std_root_sources.insert(owner);
                    }
                }
            }
        }
        self.mint_source_declaration_identities(program);
        self.register_builtins();
        self.capture_protected_prelude_bindings();
        self.reject_non_root_protected_prelude_declarations(program);
        // `register_builtins` parses the compiler-embedded `std/builtins.hew`
        // source outside the module graph.  Record that exact producer so
        // later trait/source identity normalization can relate prelude traits
        // (notably `Iterator`) to their canonical owner without treating a
        // user module named `builtins` as authoritative.
        self.canonical_std_module_sources
            .insert("std.builtins".to_string());
        // Compute the precise cross-module record-name collision set before any
        // registration runs, so the imported-actor registration can owner-qualify
        // a colliding receive-fn return record to its declaring module (#2208).
        self.cross_module_colliding_record_names =
            Self::compute_cross_module_colliding_record_names(program);
        self.collect_types(program);
        // Record every declared type-parameter name (across all modules) before
        // arming the guard, so it never false-positives on a generic parameter
        // the resolver leaves opaque and re-resolves without its scope active.
        self.collect_declared_type_param_names(program);
        // Every type declaration is now registered, so a named type that still
        // fails to resolve is genuinely undefined. Arm the undefined-named-type
        // guard in `resolve_type_expr_tracking_holes`; it stays disarmed during
        // `collect_types` because member resolution there can legally reference a
        // forward-declared sibling type that is not yet registered.
        self.type_decls_registered = true;
        self.collect_functions(program);

        // Pass 1.5 (#2202): re-resolve type-declaration MEMBER types now that
        // import-alias maps are live. `collect_types` resolves record/struct
        // fields, enum payloads, and machine state/event fields BEFORE
        // `collect_functions` processes imports, so a bare import alias in member
        // position froze unresolved while its construction (Pass 3) resolved to
        // the canonical qualified identity. This upgrade-only pass re-resolves
        // members under each owning module's context and re-runs the member-
        // derived marker/codec facts. It must run after `collect_functions` (so
        // alias maps exist) and before body checking + descriptor building (so
        // every member-derived consumer sees the corrected types).
        self.reresolve_member_types_after_imports(program);
        // `optional` is a wire-schema admission marker, not an implicit
        // default. Check it only after member re-resolution so aliases and
        // imports have reached their semantic `Ty` identity.
        self.validate_wire_optional_field_admission(program);

        // Build the actor protocol descriptor side-table BEFORE body checking.
        //
        // The descriptor maps each `receive fn` to its stable, hash-derived
        // `msg_id` (`SipHash-1-3("Actor::handler")`). Body checking needs this
        // map available because the active-mode `conn.attach(this)` coercion
        // (`LocalPid<Actor>` → `LocalPid<ConnectionHandler>`) consults
        // `actor_satisfies_handler_trait`, which reads
        // `self.actor_protocol_descriptors` to confirm an actor's `receive fn`s
        // structurally satisfy the handler trait. Building it after body
        // checking (the pre-Q90 placement) left the map empty during the
        // coercion, so the predicate always failed and the attach surface had
        // no working caller.
        //
        // `build_actor_protocol_descriptors` needs only `program` plus the
        // registered fn signatures, both of which `collect_functions` has
        // finalized by this point. Receive-fn parameter types are concrete
        // declared annotations (no inference vars to resolve), so the
        // descriptor built here is identical to one built from the post-
        // substitution `resolved_fn_sigs` snapshot later in the pipeline; the
        // published descriptor in `TypeCheckOutput` reads this same field
        // rather than rebuilding.
        //
        // `fn_sigs` is borrowed immutably while `errors` is borrowed mutably;
        // both are `self` fields, so swap `fn_sigs` out across the build to
        // keep the borrow checker happy without cloning the whole signature map.
        let fn_sigs_for_descriptors = std::mem::take(&mut self.fn_sigs);
        self.actor_protocol_descriptors =
            build_actor_protocol_descriptors(program, &fn_sigs_for_descriptors, &mut self.errors);
        self.fn_sigs = fn_sigs_for_descriptors;

        // Check non-root module_graph bodies first (dependencies before dependents).
        // Mirrors the traversal order in collect_functions so every registered
        // signature has its body validated, not just the root module.
        // Body-level deferred inference holes (e.g. `as _` cast targets, lambda
        // parameter `_` types) produced here accumulate in
        // `self.deferred_inference_holes` and are drained by
        // `report_unresolved_inference_holes` at the end of check_program.
        if let Some(ref mg) = program.module_graph {
            let span_indices = mg.file_span_indices();
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    let module_name = mod_id.path.join(".");
                    self.current_module = Some(module_name.clone());
                    // Index per SOURCE FILE, not per module: a directory
                    // module assembles its peer `.hew` files into one module
                    // whose items keep file-relative spans, so a per-module
                    // index lets two peers with same-offset expressions
                    // overwrite each other in `expr_types`.
                    self.current_module_idx = span_indices.module_base(mod_id).unwrap_or_default();
                    // Temporarily scope local_type_defs / local_trait_defs to
                    // this module so orphan-rule checks see module-local
                    // definitions and locally_non_generic works correctly.
                    let saved_local_type_defs = self.local_type_defs.clone();
                    let saved_source_type_defs = self.source_type_defs.clone();
                    let saved_local_trait_defs = self.local_trait_defs.clone();
                    for (item, _) in &module.items {
                        match item {
                            Item::TypeDecl(td) => {
                                self.local_type_defs.insert(td.name.clone());
                                self.source_type_defs.insert(td.name.clone());
                            }
                            Item::Machine(md) => {
                                // Parallel to the TypeDecl arm: seed the machine's
                                // name so orphan-rule and locally_non_generic checks
                                // inside the body pass see it as locally-defined.
                                // Also seed the synthesised `<Name>Event` companion
                                // type so event-typed parameters and bare event
                                // ctors in imported machine modules resolve as
                                // locally-non-generic.
                                self.local_type_defs.insert(md.name.clone());
                                self.source_type_defs.insert(md.name.clone());
                                let event_type_name = format!("{}Event", md.name);
                                self.local_type_defs.insert(event_type_name.clone());
                                self.source_type_defs.insert(event_type_name);
                            }
                            Item::Actor(ad) => {
                                // An actor declares a nominal type; an
                                // `impl ImportedTrait for ThisActor` is therefore
                                // local-typed, not an orphan. Seed both sets so the
                                // orphan rule treats it like any other local type.
                                self.local_type_defs.insert(ad.name.clone());
                                self.source_type_defs.insert(ad.name.clone());
                            }
                            Item::Trait(tr) => {
                                self.local_trait_defs.insert(tr.name.clone());
                            }
                            _ => {}
                        }
                    }

                    // Builtin/standard-library modules (`std::`, `hew::`,
                    // `ecosystem::`) ship with the compiler; scope-level lints
                    // inside them (UnusedVariable, UnusedMut) are implementation
                    // details the user cannot act on.  Set the flag transiently
                    // so `emit_scope_warnings` suppresses them for these bodies.
                    // Mirrors the stdlib-skip guard already in `run_lints`.
                    //
                    // Real stdlib modules are at least 2 path segments deep
                    // (e.g. ["std", "iter"]).  A single-segment module named
                    // ["std"] is a user file (std.hew) and must NOT be treated
                    // as stdlib — it still needs its scope warnings.  This
                    // matches `is_builtin_module` in hew-compile, which requires
                    // "std::" / "hew::" / "ecosystem::" (the double-colon
                    // guarantees 2+ segments).
                    let saved_is_stdlib_source = self.is_stdlib_source;
                    if mod_id.path.len() >= 2
                        && matches!(
                            mod_id.path.first().map(String::as_str),
                            Some("std" | "hew" | "ecosystem")
                        )
                    {
                        self.is_stdlib_source = true;
                    }

                    for (item_idx, (item, span)) in module.items.iter().enumerate() {
                        self.current_item_source = self
                            .module_item_sources
                            .get(&module_name)
                            .and_then(|sources| sources.get(item_idx))
                            .or_else(|| module.source_paths.first())
                            .cloned();
                        self.current_module_idx = span_indices
                            .item_index(mod_id, item_idx)
                            .unwrap_or(self.current_module_idx);
                        let diagnostic_source = self
                            .item_file_routing_token(
                                Some(&module_name),
                                self.current_item_source.as_ref(),
                            )
                            .unwrap_or_else(|| module_name.clone());
                        let err_before = self.errors.len();
                        let warn_before = self.warnings.len();
                        self.check_item(item, span);

                        // An assembled peer retains file-relative spans, so
                        // tag each item's diagnostics before advancing to the
                        // next file. Replace the aggregate module fallback as
                        // well as an absent source, while preserving an
                        // explicitly different diagnostic owner.
                        for error in &mut self.errors[err_before..] {
                            if error
                                .source_module
                                .as_deref()
                                .is_none_or(|source| source == module_name)
                            {
                                error.source_module = Some(diagnostic_source.clone());
                            }
                        }
                        for warning in &mut self.warnings[warn_before..] {
                            if warning
                                .source_module
                                .as_deref()
                                .is_none_or(|source| source == module_name)
                            {
                                warning.source_module = Some(diagnostic_source.clone());
                            }
                        }
                    }
                    self.current_item_source = None;
                    self.current_item_ordinal = 0;

                    self.is_stdlib_source = saved_is_stdlib_source;

                    self.local_type_defs = saved_local_type_defs;
                    self.source_type_defs = saved_source_type_defs;
                    self.local_trait_defs = saved_local_trait_defs;
                }
            }
            self.current_module = None;
            // Restore to 0 so subsequent root-level checks (program.items below)
            // still use module_idx = 0 for their span keys.
            self.current_module_idx = 0;
        }

        for (item, span) in &program.items {
            self.check_item(item, span);
        }

        // Closure escape classification — runs after all bodies have
        // been type-checked. Walks each fn body (root + modules) looking
        // for closure literal sites and computes per-closure
        // `ClosureEscapeFact`. Conservative default: `Escapes` unless
        // positively proven `Local` or `Forked`.
        self.classify_closure_escapes(program);

        // Fail-closed defense: every closure literal in the program
        // must have BOTH a `ClosureCaptureFact` ledger AND a
        // `ClosureEscapeFact` by the time the checker hands off to
        // MIR-lowering. A missing entry is a structural bug in this
        // checker (the lambda site was never visited by `check_lambda`
        // or `classify_closure_escapes`) — not a user-code shape — so
        // we emit a hard diagnostic rather than letting MIR observe a
        // silently-defaulted closure.
        self.validate_closure_facts_complete(program);

        // Apply final substitutions to all recorded types
        let mut expr_types: HashMap<SpanKey, Ty> = self
            .expr_types
            .iter()
            .map(|(k, v)| (k.clone(), self.normalize_for_use(v)))
            .collect();

        // Emit unused import warnings. A REPL fragment imports modules it will
        // reference on later inputs, so suppress this lint for eval fragments.
        if !self.repl_fragment {
            for (key, (import_span, stored_module)) in &self.import_spans {
                if self.is_canonical_prelude_manifest_import(stored_module.as_deref()) {
                    continue;
                }
                if !self.used_modules.borrow().contains(key) {
                    self.warnings.push(TypeError {
                        severity: crate::error::Severity::Warning,
                        kind: TypeErrorKind::UnusedImport,
                        span: import_span.clone(),
                        message: format!("unused import: `{}`", key.short_name),
                        notes: vec![],
                        suggestions: vec!["remove this import".to_string()],
                        source_module: stored_module.clone(),
                    });
                }
            }
        }

        self.emit_dead_code_warnings();

        self.default_unconstrained_range_types(&expr_types);
        // Re-record range bound spans with their concrete element types
        // (resolved after inference + defaulting) and validate fits.
        self.apply_deferred_range_bound_types(&mut expr_types);
        // Drain deferred trait-bound checks after inference/defaulting settles,
        // but before unresolved-hole reporting so concrete bound failures stay
        // specific and unresolved holes remain authoritative.
        self.drain_deferred_bound_checks();

        // Semantic lint sweep. Runs after inference + defaulting have settled
        // so each lint can trust fully-resolved expression types. Findings are
        // routed by their configured level: `Deny` lints become hard errors,
        // everything else a warning. Keeping `Deny` out of `self.warnings`
        // preserves the `lint_warnings_have_warning_severity` invariant (the
        // warnings channel is warning-only).
        let mut lint_out: Vec<TypeError> = Vec::new();
        self.run_lints(program, &self.lint_levels, &mut lint_out);
        for diag in lint_out {
            if diag.severity == crate::error::Severity::Error {
                self.errors.push(diag);
            } else {
                self.warnings.push(diag);
            }
        }
        let resolved_builtin_result_output_type_args: HashMap<SpanKey, (Ty, Ty)> =
            std::mem::take(&mut self.builtin_result_output_type_args)
                .into_iter()
                .filter_map(|(k, (ok_ty, err_ty))| {
                    let ok_ty = self.finalize_type_for_handoff(&ok_ty);
                    let err_ty = self.finalize_type_for_handoff(&err_ty);
                    resolve_builtin_result_output_type_args(ok_ty, err_ty).map(|args| (k, args))
                })
                .collect();

        // Also resolve inferred call type args so the enrichment layer can
        // fill in explicit type annotations for the codegen.
        let mut resolved_call_type_args: HashMap<SpanKey, Vec<Ty>> =
            std::mem::take(&mut self.call_type_args)
                .into_iter()
                .map(|(k, args)| {
                    let resolved: Vec<Ty> = args
                        .iter()
                        .map(|a| self.finalize_type_for_handoff(a))
                        .collect();
                    (k, resolved)
                })
                .collect();

        // Same boundary resolution for record-init type args.
        // Mirrors `resolved_call_type_args` so downstream consumers see
        // fully-resolved, literal-defaulted `Ty` values.
        let mut resolved_record_init_type_args: HashMap<SpanKey, Vec<Ty>> =
            std::mem::take(&mut self.record_init_type_args)
                .into_iter()
                .map(|(k, args)| {
                    let resolved: Vec<Ty> = args
                        .iter()
                        .map(|a| self.finalize_type_for_handoff(a))
                        .collect();
                    (k, resolved)
                })
                .collect();
        let resolved_closure_capture_facts = std::mem::take(&mut self.closure_capture_facts)
            .into_iter()
            .map(|(k, facts)| {
                let resolved = facts
                    .into_iter()
                    .map(|mut fact| {
                        fact.ty = self.finalize_type_for_handoff(&fact.ty);
                        fact
                    })
                    .collect();
                (k, resolved)
            })
            .collect();
        let resolved_actor_method_dispatch = std::mem::take(&mut self.actor_method_dispatch)
            .into_iter()
            .map(|(k, kind)| {
                let resolved_kind = match kind {
                    ActorMethodKind::Fire(method_id) => ActorMethodKind::Fire(method_id),
                    ActorMethodKind::BlockingFire(method_id) => {
                        ActorMethodKind::BlockingFire(method_id)
                    }
                    ActorMethodKind::CheckedFire(method_id) => {
                        ActorMethodKind::CheckedFire(method_id)
                    }
                    ActorMethodKind::Ask(method_id, reply_ty) => {
                        ActorMethodKind::Ask(method_id, self.finalize_type_for_handoff(&reply_ty))
                    }
                    ActorMethodKind::StreamProducer(method_id, elem_ty) => {
                        ActorMethodKind::StreamProducer(
                            method_id,
                            self.finalize_type_for_handoff(&elem_ty),
                        )
                    }
                };
                (k, resolved_kind)
            })
            .collect();
        self.actor_method_dispatch = resolved_actor_method_dispatch;

        // Move data out of Checker — it is not used after check_program.
        // Resolve any remaining type variables in expr_types via the
        // substitution so the enrichment layer sees concrete types, then
        // materialize surviving literal kinds at the checked-output boundary.
        let mut resolved_expr_types: HashMap<SpanKey, Ty> = expr_types
            .into_iter()
            .map(|(k, v)| {
                let mut resolved = self.finalize_type_for_handoff(&v);
                if let Some((ok_ty, err_ty)) = resolved_builtin_result_output_type_args.get(&k) {
                    resolved = patch_builtin_result_output_type(resolved, ok_ty, err_ty);
                }
                (k, resolved)
            })
            .collect();

        let mut resolved_type_defs: HashMap<String, TypeDef> = std::mem::take(&mut self.type_defs)
            .into_iter()
            .map(|(name, type_def)| {
                let resolved = self.resolve_type_def(&type_def);
                (name, resolved)
            })
            .collect();

        let mut resolved_fn_sigs: HashMap<String, FnSig> = std::mem::take(&mut self.fn_sigs)
            .into_iter()
            .map(|(name, sig)| {
                let resolved = self.resolve_fn_sig(&sig);
                (name, resolved)
            })
            .collect();

        self.validate_checker_output_contract(
            &mut resolved_expr_types,
            &mut resolved_type_defs,
            &mut resolved_fn_sigs,
            &mut resolved_call_type_args,
            &mut resolved_record_init_type_args,
        );
        // The output-contract validator may rebuild a surviving expression
        // type while pruning invalid siblings.  Re-run the same finalizer at
        // that mutation boundary so layout facts and
        // the published expression table observe one nominal identity.  In
        // particular, a source module's `CryptoError` match arms must not keep
        // their provisional bare spelling after the enclosing match has its
        // exact declaration owner.
        let saved_output_module = self.current_module.clone();
        for (key, ty) in &mut resolved_expr_types {
            self.current_module = self.expr_type_source_modules.get(key).cloned().flatten();
            *ty = self.finalize_type_for_handoff(ty);
        }
        self.current_module = saved_output_module;
        for sig in resolved_fn_sigs.values_mut() {
            *sig = self.resolve_fn_sig(sig);
        }
        let entry_exit_plan = self.classify_entry_exit_plan(program, &resolved_fn_sigs);
        for type_def in resolved_type_defs.values_mut() {
            *type_def = self.resolve_type_def(type_def);
        }
        let opaque_resource_candidates =
            self.derive_opaque_resource_candidate_graph(&resolved_fn_sigs);
        for cycle in crate::cycle::detect_recursive_value_type_cycles(&resolved_type_defs) {
            let span = self
                .type_def_spans
                .get(&cycle.edge.from)
                .cloned()
                .unwrap_or(0..0);
            let type_kind = resolved_type_defs
                .get(&cycle.edge.from)
                .map_or("type", |type_def| value_type_kind_label(type_def.kind));
            self.errors.push(TypeError::recursive_value_type(
                span,
                type_kind,
                &cycle.edge.from,
                &cycle.edge.member_desc,
                &cycle.edge.to,
            ));
        }
        // The layout-backed HashMap/HashSet admission finalizers below still
        // consult `self.type_defs` to prove named record hash-eligibility and
        // compute key/value ABI sizes. `resolved_type_defs` is the authoritative
        // post-substitution snapshot after the checked-output boundary pass, so
        // restore it into the checker before draining those deferred queues.
        self.type_defs = resolved_type_defs.clone();
        self.finalize_builtin_clone_admission();
        let mut resolved_lowering_facts = self.finalize_lowering_facts();
        admissibility::validate_lowering_facts_output_contract(
            &mut resolved_lowering_facts,
            &resolved_expr_types,
        );
        self.finalize_hashmap_admission();
        self.finalize_hashset_admission();
        self.finalize_vec_admission();
        self.finalize_channel_rewrites();
        self.finalize_generic_structural_eq();

        // Prune any layout facts whose span is not in the validated expr_types map.
        // This prevents orphaned layout facts (from expressions that were pruned
        // by validate_checker_output_contract) from reaching codegen.
        self.hashmap_layout_facts
            .retain(|key, _| resolved_expr_types.contains_key(key));
        self.hashset_layout_facts
            .retain(|key, _| resolved_expr_types.contains_key(key));

        self.report_unresolved_inference_holes(program);
        self.report_unresolved_monomorphic_sites();

        // Q87 slice 1: the actor protocol descriptor side-table is the only
        // msg_id authority downstream. Collisions (two `receive fn`s hashing
        // to the same msg_id) emit `ActorProtocolCollision` diagnostics and
        // the offending actor is **absent** from the map — MIR/codegen treat a
        // missing entry for an actor with handlers as fail-closed. There is no
        // fallback `enumerate()` path.
        //
        // The map was built once before body checking (so the active-mode
        // `conn.attach(this)` coercion could read it) and cached in
        // `self.actor_protocol_descriptors`. Take it for the typed output here
        // rather than rebuilding: a rebuild would re-run collision detection
        // and double-emit the diagnostics.
        let actor_protocol_descriptors = std::mem::take(&mut self.actor_protocol_descriptors);

        // Compute the set of monomorphic builtin enum names that landed in
        // `type_defs` via internal pre-registration (e.g.
        // `register_builtins_hew_impls`) without a matching user-source
        // TypeDecl. Sandbox-WASM emit consults this to suppress its eager
        // `type_defs` sweep for builtin shapes the user did not author.
        let internal_builtin_enum_names: HashSet<String> = {
            use crate::builtin_enums::monomorphic_builtin_enums;
            monomorphic_builtin_enums()
                .iter()
                .filter(|spec| {
                    spec.suppress_from_sandbox_emit
                        && resolved_type_defs.contains_key(spec.name)
                        && !self.source_type_defs.contains(spec.name)
                })
                .map(|spec| spec.name.to_string())
                .collect()
        };

        // W4.047 P1.1 — build the typed `resolved_expr_types` handoff map.
        //
        // Run the single authorised `Ty -> ResolvedTy` conversion over every
        // surviving (post-contract, post-prune) `expr_types` entry. A
        // successful conversion proves the type is concrete and admissible;
        // the entry is stored in the typed map. A conversion *failure* is only
        // legitimate for a *covered* generic inference var — a pre-monomorphi-
        // zation type-parameter position that `validate_expr_output_contract`
        // deliberately retained (its unresolved vars are a subset of the
        // tracked holes) and that monomorphization resolves downstream. Those
        // spans are legitimately absent from the typed map.
        //
        // ANY OTHER conversion failure (a leaked `Ty::Error`, an unmaterialized
        // numeric literal, or a var-free unresolved associated projection) is a
        // fail-open totality gap: an inadmissible type the output contract
        // should already have pruned + diagnosed *before* the checker→HIR
        // handoff. The `debug_assert!` below is the totality net — it fires
        // loudly in debug/test/CI if such a type survives, and is compiled out
        // of release so this remains a pure, zero-behaviour-change substrate
        // add (HIR still drives lowering off `expr_types` in Phase 1).
        let resolved_expr_types_typed: HashMap<SpanKey, ResolvedTy> = {
            // The totality invariant holds for *accepted* programs only: "every
            // accepted expression span has a concrete ResolvedTy, OR the program
            // was rejected." A program that emitted hard errors is rejected and
            // hands off no CheckedProgram, so error-recovery placeholders
            // legitimately survive in its `expr_types`. (Errors are still in
            // `self.errors` here — they are moved into the output below.)
            let program_accepted = self.errors.is_empty();
            let mut typed = HashMap::with_capacity(resolved_expr_types.len());
            for (key, ty) in &resolved_expr_types {
                match ResolvedTy::from_ty(ty) {
                    Ok(resolved) => {
                        typed.insert(key.clone(), resolved);
                    }
                    Err(boundary_err) => {
                        // `ResolvedTy` cannot represent the four checker-internal
                        // states, so a conversion failure means the span is not
                        // concrete and is (correctly) omitted from the typed map.
                        // We classify the omission to keep the totality net honest:
                        //
                        //  - `UnresolvedInference` / `UnresolvedAssocProjection`:
                        //    a *covered* generic position (a type-parameter var or
                        //    an associated-type projection over one) in a
                        //    pre-monomorphization body. `validate_expr_output_
                        //    contract` deliberately retains these (their unresolved
                        //    vars are a subset of the tracked holes); monomorphi-
                        //    zation resolves them. Legitimately absent — no gap.
                        //
                        //  - `TaintedError`: an error-recovery placeholder. In a
                        //    *rejected* program this is expected. In an *accepted*
                        //    program it is an upstream checker bug (the contract
                        //    gate prunes leaked inference vars but not `Ty::Error`):
                        //    a span typed `Ty::Error` with no diagnostic, masked
                        //    downstream by the fail-open `.unwrap_or(Unit)`. The
                        //    typed map omits it (fail-closed absence, not a Unit
                        //    guess); the finding is reported for a dedicated lane
                        //    and Phase 3 converts the downstream miss to a hard
                        //    `CheckerBoundaryViolation`. KNOWN INSTANCE (W4.047):
                        //    `ScopeError<i64>.cancelled_count` resolves to
                        //    `Ty::Error` though `cancelled_count: i64` is declared.
                        //
                        //  - `UnmaterializedLiteral`: must NEVER survive —
                        //    `materialize_literal_defaults` ran at the boundary.
                        //    A leak here is a genuine literal-defaulting totality
                        //    hole, so it stays a hard (debug-only) assert.
                        debug_assert!(
                            !program_accepted
                                || !matches!(
                                    boundary_err,
                                    BoundaryError::UnmaterializedLiteral { .. }
                                ),
                            "W4.047 totality gap: surviving expr_types entry at \
                             {key:?} has type {ty:?} that fails ResolvedTy::from_ty \
                             ({boundary_err}) — an unmaterialized numeric literal \
                             crossed the checker->HIR handoff (literal-defaulting \
                             totality hole)"
                        );
                    }
                }
            }
            typed
        };

        // #1929 Stage 1: classify every concrete generic type-argument's
        // `Vec<T>` element ABI now, while `self.registry` (the `Copy` marker
        // authority) and the resolved `type_defs` (the `is_indirect` authority)
        // are both still in scope. MIR re-resolution of an element-typed Vec
        // method under a type parameter consults this verdict per
        // monomorphisation; computing it here keeps the single element→ABI
        // authority on the checker side rather than re-deriving it downstream.
        let vec_generic_element_abi = self.build_vec_generic_element_abi(
            &resolved_call_type_args,
            &resolved_record_init_type_args,
            &resolved_type_defs,
        );

        // The §6.3 fact table describes an accepted program. A rejected one
        // hands off no CheckedProgram, and its declaration graph may be
        // ill-formed - `type Direct { next: Direct }` has no finite value
        // layout - so the hash and eq eligibility walks it would drive have no
        // bottom. Building nothing there is the same rule the typed
        // `resolved_expr_types` handoff above states for its own totality.
        let (type_fact_context, type_facts) = if self.errors.is_empty() {
            let (context, facts, refusals) = self.build_type_facts(&resolved_expr_types_typed);
            self.errors.extend(refusals);
            (context, facts)
        } else {
            (TypeFactContext::default(), BTreeMap::new())
        };
        let mut output = TypeCheckOutput {
            expr_types: resolved_expr_types,
            interpolation_display_types: std::mem::take(&mut self.interpolation_display_types),
            user_comparison_dispatch: std::mem::take(&mut self.user_comparison_dispatch),
            actor_self_state_fields: std::mem::take(&mut self.actor_self_state_fields),
            type_facts,
            type_fact_context,
            resolved_expr_types: resolved_expr_types_typed,
            is_type_patterns: std::mem::take(&mut self.is_type_patterns),
            method_call_receiver_kinds: std::mem::take(&mut self.method_call_receiver_kinds),
            method_call_consumes_receiver: std::mem::take(&mut self.method_call_consumes_receiver),
            method_call_discharges_receiver: std::mem::take(
                &mut self.method_call_discharges_receiver,
            ),
            method_call_preserves_receiver_identity: std::mem::take(
                &mut self.method_call_preserves_receiver_identity,
            ),
            opaque_resource_candidates,
            actor_handler_state_guards: std::mem::take(&mut self.actor_handler_state_guards),
            actor_max_heap: std::mem::take(&mut self.actor_max_heap),
            supervisor_child_slots: std::mem::take(&mut self.supervisor_child_slots),
            pool_accessor_sites: std::mem::take(&mut self.pool_accessor_sites),
            lowering_facts: resolved_lowering_facts,
            method_call_rewrites: std::mem::take(&mut self.method_call_rewrites),
            wire_layouts: std::mem::take(&mut self.wire_layouts),
            // W4.001 Stage A: substrate-only. Field is empty in Stage A
            // (no production populator); Stage B's resolver fills it.
            // See `check::dispatch` module docs and
            // `TypeCheckOutput::resolved_calls`.
            resolved_calls: std::mem::take(&mut self.resolved_calls),
            import_type_name_aliases: std::mem::take(&mut self.import_type_name_aliases),
            module_import_bindings: std::mem::take(&mut self.module_import_bindings),
            numeric_method_lowerings: std::mem::take(&mut self.numeric_method_lowerings),
            width_cast_lowerings: std::mem::take(&mut self.width_cast_lowerings),
            try_width_cast_lowerings: std::mem::take(&mut self.try_width_cast_lowerings),
            actor_method_dispatch: std::mem::take(&mut self.actor_method_dispatch),
            machine_method_dispatch: std::mem::take(&mut self.machine_method_dispatch),
            conn_await_reads: std::mem::take(&mut self.conn_await_reads),
            listener_await_accepts: std::mem::take(&mut self.listener_await_accepts),
            tail_ok_coercions: std::mem::take(&mut self.tail_ok_coercions),
            assign_target_kinds: std::mem::take(&mut self.assign_target_kinds),
            assign_target_shapes: std::mem::take(&mut self.assign_target_shapes),
            errors: std::mem::take(&mut self.errors),
            warnings: std::mem::take(&mut self.warnings),
            user_clone_record_seeds: std::mem::take(&mut self.user_clone_record_seeds),
            type_defs: resolved_type_defs,
            internal_builtin_enum_names,
            identity: std::mem::take(&mut self.identity).freeze(),
            entry_exit_plan,
            extern_contracts: std::mem::take(&mut self.extern_table),
            fn_sigs: resolved_fn_sigs,
            direct_call_targets: std::mem::take(&mut self.direct_call_targets),
            trait_method_ids: std::mem::take(&mut self.trait_method_ids),
            trait_method_ids_by_binding: std::mem::take(&mut self.trait_method_ids_by_binding),
            impl_method_declaration_ids: std::mem::take(&mut self.impl_method_declaration_ids),
            root_value_bindings: std::mem::take(&mut self.root_value_bindings),
            handle_bearing_structs: {
                // Flush any pending dirty registration before the set is moved
                // out — the output layer uses this set for codegen decisions.
                self.ensure_handle_bearing_fresh();
                std::mem::take(&mut self.handle_bearing_structs)
            },
            cycle_capable_actors: HashSet::new(),
            user_modules: std::mem::take(&mut self.user_modules),
            call_type_args: resolved_call_type_args,
            vec_generic_element_abi,
            record_init_type_args: resolved_record_init_type_args,
            stack_hints: std::mem::take(&mut self.stack_hints),
            dyn_trait_coercions: std::mem::take(&mut self.dyn_trait_coercions),
            dyn_trait_method_calls: std::mem::take(&mut self.dyn_trait_method_calls),
            closure_capture_facts: resolved_closure_capture_facts,
            closure_escape_facts: std::mem::take(&mut self.closure_escape_facts),
            actor_protocol_descriptors,
            intrinsic_declarations: std::mem::take(&mut self.intrinsic_declarations),
            pattern_resolutions: std::mem::take(&mut self.pending_pattern_resolutions)
                .into_iter()
                .map(|(k, mut arm)| {
                    // Resolve inference variables in every payload binding type.
                    for pb in &mut arm.payload_bindings {
                        pb.ty = self.finalize_type_for_handoff(&pb.ty);
                    }
                    (k, arm)
                })
                .collect(),
            pattern_plans: std::mem::take(&mut self.pending_pattern_plans)
                .into_iter()
                .map(|(key, mut plan)| {
                    for field in &mut plan.fields {
                        field.ty = self.finalize_type_for_handoff(&field.ty);
                    }
                    (key, plan)
                })
                .collect(),
            lang_items: std::mem::take(&mut self.lang_items),
            hashmap_layout_facts: std::mem::take(&mut self.hashmap_layout_facts),
            hashset_layout_facts: std::mem::take(&mut self.hashset_layout_facts),
            actor_spawn_type_args: {
                // Resolve any lingering inference variables in the type args
                // before publishing to the output table.
                std::mem::take(&mut self.actor_spawn_type_args)
                    .into_iter()
                    .map(|(k, (name, args))| {
                        let resolved_args = args
                            .into_iter()
                            .map(|ty| self.finalize_type_for_handoff(&ty))
                            .collect();
                        (k, (name, resolved_args))
                    })
                    .collect()
            },
        };

        // Detect actor reference cycles and emit warnings.
        let (cycle_capable, cycles) = crate::cycle::detect_actor_ref_cycles(&output.type_defs);
        for cycle_actors in &cycles {
            let desc = cycle_actors.join(" -> ");
            let span = cycle_actors
                .iter()
                .filter_map(|name| self.type_def_spans.get(name).cloned())
                .min_by_key(|span| span.start)
                .unwrap_or(0..0);
            output
                .warnings
                .push(TypeError::actor_ref_cycle(span, &desc));
        }
        output.cycle_capable_actors = cycle_capable;

        output
    }

    /// Restore the checker to the same program-owned state as a fresh instance.
    ///
    /// A checker may be reused by front ends, but every table populated while
    /// checking one program must be absent from the next program.  Rebuilding
    /// the checker is less error-prone than maintaining a second, incomplete
    /// list of registries whenever a new checker-side cache is added. The
    /// module registry preserves only parsed module data; its active modules
    /// and derived handle/drop metadata are structurally replaced.
    fn reset_for_program(&mut self) {
        let module_registry = std::mem::replace(
            &mut self.module_registry,
            crate::module_registry::ModuleRegistry::new(vec![]),
        )
        .for_new_program();
        let wasm_target = self.wasm_target;
        let repl_fragment = self.repl_fragment;
        let is_stdlib_source = self.is_stdlib_source;
        let checking_embedded_builtins = self.checking_embedded_builtins;
        let consume_receiver_methods = std::mem::take(&mut self.consume_receiver_methods);
        let lint_levels = self.lint_levels.clone();
        let lint_sources = self.lint_sources.clone();
        let entry_selection = self.entry_selection;

        *self = Self::new(module_registry);
        self.wasm_target = wasm_target;
        self.repl_fragment = repl_fragment;
        self.is_stdlib_source = is_stdlib_source;
        self.checking_embedded_builtins = checking_embedded_builtins;
        self.has_checked_program = true;
        self.consume_receiver_methods = consume_receiver_methods;
        self.lint_levels = lint_levels;
        self.lint_sources = lint_sources;
        self.entry_selection = entry_selection;
    }

    /// The canonical prelude is an import-only authority manifest: its imports
    /// are intentionally consumed by later user programs, not by its own body.
    fn is_canonical_prelude_manifest_import(&self, stored_module: Option<&str>) -> bool {
        stored_module == Some("std.prelude")
            || (stored_module.is_none() && self.canonical_std_root_sources.contains("std.prelude"))
    }

    /// Escape classifier. Walks the program AST after type-checking,
    /// identifies every closure literal, and records a
    /// `ClosureEscapeFact` keyed by the literal's span in
    /// `self.closure_escape_facts`. Conservative default: closures
    /// default to `Escapes` unless positively proven `Local` or
    /// `Forked`.
    fn classify_closure_escapes(&mut self, program: &Program) {
        for (item, _) in &program.items {
            self.classify_escapes_in_item(item);
        }
        // Read the SAME per-file index allocation body checking used, so each
        // `closure_escape_facts` entry is stamped with the `module_idx` the HIR
        // consumer reads back via `mk_key` and the fail-closed validator keys
        // on. `current_module_idx` was reset to 0 before this pass, so the root
        // items above used idx 0.
        let (module_order, span_indices) = match &program.module_graph {
            Some(mg) => (
                mg.topo_order
                    .iter()
                    .filter(|mod_id| **mod_id != mg.root)
                    .cloned()
                    .collect(),
                Some(mg.file_span_indices()),
            ),
            None => (Vec::new(), None),
        };
        for mod_id in &module_order {
            if let Some(module) = program
                .module_graph
                .as_ref()
                .and_then(|mg| mg.modules.get(mod_id))
            {
                for (item_idx, (item, _)) in module.items.iter().enumerate() {
                    self.current_module_idx = span_indices
                        .as_ref()
                        .and_then(|indices| indices.item_index(mod_id, item_idx))
                        .unwrap_or_default();
                    self.classify_escapes_in_item(item);
                }
            }
        }
        if program.module_graph.is_some() {
            self.current_module_idx = 0;
        }
    }

    /// Run the semantic lint sweep over every function/method body in the
    /// program and collect findings into `out`.
    ///
    /// Read-only over checker state (`&self`): it scans each source file
    /// once, walks each item's bodies, builds a [`lints::LintCtx`] carrying the
    /// resolved-type facts, and tags each finding with the right per-file
    /// `module_idx` / `source_module`. The module walk mirrors
    /// `classify_closure_escapes` and the body-check loop in
    /// [`Checker::check_program`] (root items at index 0, then each non-root
    /// module in topo order at a 1-based index, with the same dotted module
    /// name `record_type` stamped onto its spans) so [`SpanKey`] lookups hit
    /// and diagnostics route to the correct source file. Severity routing
    /// (`Deny` → error, otherwise warning) is left to the caller.
    pub fn run_lints(&self, program: &Program, levels: &LintLevels, out: &mut Vec<TypeError>) {
        if let Some(source) = self.lint_sources.source_for(None) {
            self.lint_source(source, 0, None, levels, out);
        }
        for (item, _) in &program.items {
            self.lint_item(item, 0, None, levels, out);
        }
        let (module_order, span_indices) = match &program.module_graph {
            Some(mg) => (
                mg.topo_order
                    .iter()
                    .filter(|mod_id| **mod_id != mg.root)
                    .cloned()
                    .collect(),
                Some(mg.file_span_indices()),
            ),
            None => (Vec::new(), None),
        };
        for mod_id in &module_order {
            if let Some(module) = program
                .module_graph
                .as_ref()
                .and_then(|mg| mg.modules.get(mod_id))
            {
                // Builtin/standard-library modules (`std::`, `hew::`,
                // `ecosystem::`) ship with the compiler rather than the user's
                // project, so lint findings inside them are noise the user
                // cannot act on. Skip them; the index allocation is shared with
                // body checking, so skipping cannot shift a later module's
                // span tagging. Mirrors `is_builtin_module` in `hew-compile`.
                //
                // Real stdlib modules are at least 2 path segments deep
                // (e.g. ["std", "iter"]).  A single-segment module named
                // ["std"] is a user file (std.hew) and must still be linted.
                if mod_id.path.len() >= 2
                    && matches!(
                        mod_id.path.first().map(String::as_str),
                        Some("std" | "hew" | "ecosystem")
                    )
                {
                    continue;
                }
                let module_name = mod_id.path.join(".");
                let module_base = span_indices
                    .as_ref()
                    .and_then(|indices| indices.module_base(mod_id))
                    .unwrap_or_default();
                for (source_idx, source_path) in module.source_paths.iter().enumerate() {
                    let source_module = if source_idx == 0 {
                        module_name.clone()
                    } else {
                        source_path.display().to_string()
                    };
                    let module_idx = span_indices
                        .as_ref()
                        .and_then(|indices| indices.path_index(source_path))
                        .unwrap_or(module_base);
                    if let Some(source) = self.lint_sources.source_for(Some(&source_module)) {
                        self.lint_source(source, module_idx, Some(&source_module), levels, out);
                    }
                }
                for (item_idx, (item, _)) in module.items.iter().enumerate() {
                    let module_idx = span_indices
                        .as_ref()
                        .and_then(|indices| indices.item_index(mod_id, item_idx))
                        .unwrap_or(module_base);
                    let item_source = program
                        .module_graph
                        .as_ref()
                        .and_then(|mg| mg.item_source(mod_id, item_idx))
                        .or_else(|| module.source_paths.first());
                    let source_module = self
                        .item_file_routing_token(Some(&module_name), item_source)
                        .unwrap_or_else(|| module_name.clone());
                    self.lint_item(item, module_idx, Some(&source_module), levels, out);
                }
            }
        }
    }

    /// Lint a module's raw source text once, for findings that do not live in
    /// the AST body structure (such as comments).
    fn lint_source(
        &self,
        source: &str,
        module_idx: u32,
        source_module: Option<&str>,
        levels: &LintLevels,
        out: &mut Vec<TypeError>,
    ) {
        let ctx = lints::LintCtx {
            checker: self,
            subst: &self.subst,
            expr_types: &self.expr_types,
            module_idx,
            source_module,
            source: Some(source),
            type_params: HashMap::new(),
        };
        lints::lint_source(&ctx, levels, source, out);
    }

    /// Lint every body carried by a single top-level item (mirrors
    /// `classify_escapes_in_item`).
    fn lint_item(
        &self,
        item: &Item,
        module_idx: u32,
        source_module: Option<&str>,
        levels: &LintLevels,
        out: &mut Vec<TypeError>,
    ) {
        let ctx = lints::LintCtx {
            checker: self,
            subst: &self.subst,
            expr_types: &self.expr_types,
            module_idx,
            source_module,
            source: self.lint_sources.source_for(source_module),
            type_params: HashMap::new(),
        };
        // Each body is linted under the type parameters actually in scope for
        // it — the enclosing item's, extended by the body's own. A lint that
        // proposes a rewrite reads bounds from here; a parameter missing from
        // the map is not treated as generic, and a rewrite over it is refused
        // by the unregistered-nominal path instead.
        match item {
            Item::Function(fn_decl) => {
                let ctx = ctx.with_type_params(fn_decl.type_params.as_ref());
                lints::lint_block(&ctx, levels, &fn_decl.body, out);
            }
            Item::Impl(impl_decl) => {
                let impl_ctx = ctx.with_type_params(impl_decl.type_params.as_ref());
                for method in &impl_decl.methods {
                    let ctx = impl_ctx.with_type_params(method.type_params.as_ref());
                    lints::lint_block(&ctx, levels, &method.body, out);
                }
            }
            Item::Actor(actor) => {
                let actor_ctx = ctx.with_type_params(Some(&actor.type_params));
                for method in &actor.methods {
                    let ctx = actor_ctx.with_type_params(method.type_params.as_ref());
                    lints::lint_block(&ctx, levels, &method.body, out);
                }
                for rec in &actor.receive_fns {
                    let ctx = actor_ctx.with_type_params(rec.type_params.as_ref());
                    lints::lint_receive_fn_definition(&ctx, levels, rec, out);
                    lints::lint_block(&ctx, levels, &rec.body, out);
                    lints::lint_receive_fn(&ctx, levels, &rec.body, out);
                }
            }
            Item::Trait(trait_decl) => {
                let trait_ctx = ctx.with_type_params(trait_decl.type_params.as_ref());
                for trait_item in &trait_decl.items {
                    if let TraitItem::Method(trait_method) = trait_item {
                        if let Some(body) = &trait_method.body {
                            let ctx = trait_ctx.with_type_params(trait_method.type_params.as_ref());
                            lints::lint_block(&ctx, levels, body, out);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    /// Emit a finding from a *main-pass* lint — one that runs inline during body
    /// checking rather than in the post-inference [`run_lints`] sweep — through
    /// the same level/suppression machinery the sweep uses.
    ///
    /// `module` is the module owning `span`; it locates the source text for
    /// `// hew:allow(...)` resolution and tags the diagnostic. An in-source
    /// directive is honoured first (it wins even over `Deny`), then the finding
    /// is routed by the configured [`LintLevel`]: `Allow` drops it, `Warn`
    /// pushes a warning, `Deny` pushes an error. This keeps migrated warnings
    /// (`clone_on_copy`, `dead_code`) configurable and suppressible without
    /// changing their default-`Warn` behaviour.
    fn emit_main_pass_lint(
        &mut self,
        id: LintId,
        span: &Span,
        module: Option<&str>,
        message: String,
        suggestion: String,
    ) {
        if let Some(source) = self.lint_sources.source_for(module) {
            if lints::directive_suppresses(source, span.start, id) {
                return;
            }
        }
        let severity = match self.lint_levels.level(id) {
            LintLevel::Allow => return,
            LintLevel::Warn => crate::error::Severity::Warning,
            LintLevel::Deny => crate::error::Severity::Error,
        };
        let diag = TypeError {
            severity,
            kind: TypeErrorKind::Lint(id),
            span: span.clone(),
            message,
            notes: Vec::new(),
            suggestions: vec![suggestion],
            source_module: module.map(str::to_string),
        };
        if severity == crate::error::Severity::Error {
            self.errors.push(diag);
        } else {
            self.warnings.push(diag);
        }
    }

    fn classify_escapes_in_item(&mut self, item: &Item) {
        match item {
            Item::Function(fn_decl) => {
                self.classify_escapes_in_block(&fn_decl.body, false);
            }
            Item::Impl(impl_decl) => {
                for method in &impl_decl.methods {
                    self.classify_escapes_in_block(&method.body, false);
                }
            }
            Item::Actor(actor) => {
                for method in &actor.methods {
                    self.classify_escapes_in_block(&method.body, false);
                }
                for rec in &actor.receive_fns {
                    self.classify_escapes_in_block(&rec.body, false);
                }
            }
            Item::Machine(machine) => {
                let _ = machine; // machine bodies traversed via transitions/entry/exit elsewhere
            }
            Item::Trait(trait_decl) => {
                for trait_item in &trait_decl.items {
                    if let TraitItem::Method(trait_method) = trait_item {
                        if let Some(body) = &trait_method.body {
                            self.classify_escapes_in_block(body, false);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn classify_escapes_in_block(&mut self, block: &Block, in_fork: bool) {
        // For every let-statement whose value is a closure literal
        // (lambda or lambda-actor), classify using the rest of the
        // block as the context.
        for (i, (stmt, _)) in block.stmts.iter().enumerate() {
            if let Stmt::Let {
                pattern,
                value: Some((value_expr, lambda_span)),
                ..
            } = stmt
            {
                if let Pattern::Identifier(binding_name) = &pattern.0 {
                    if matches!(
                        value_expr,
                        Expr::Lambda { .. } | Expr::SpawnLambdaActor { .. }
                    ) {
                        let fact = closure_inference::classify_closure_escape_in_block(
                            &block.stmts,
                            block.trailing_expr.as_deref(),
                            i,
                            binding_name,
                            in_fork,
                        );
                        self.closure_escape_facts.insert(
                            SpanKey::in_module(lambda_span, self.current_module_idx),
                            fact,
                        );
                        self.maybe_emit_escape_advisory(lambda_span, fact);
                    }
                }
            }
        }
        // Recurse into every statement for nested blocks / nested
        // closures (anonymous lambdas, lambdas inside expressions).
        for (stmt, _) in &block.stmts {
            self.classify_escapes_in_stmt(stmt, in_fork);
        }
        if let Some(tail) = &block.trailing_expr {
            self.classify_escapes_in_expr(&tail.0, &tail.1, in_fork, AnonContext::Tail);
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "Statement visitor traverses all stmt shapes; splitting \
                  by category would scatter related logic"
    )]
    fn classify_escapes_in_stmt(&mut self, stmt: &Stmt, in_fork: bool) {
        match stmt {
            Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
                if let Some((e, span)) = value {
                    // `let f = || ...` was handled at the block level.
                    // Other shapes (block expr, struct init, …) descend
                    // here. The expression-position context says "value
                    // flows into a binding" — anonymous lambdas bound
                    // to a name get `Escapes` since the binding stores
                    // the closure for later use.
                    self.classify_escapes_in_expr(e, span, in_fork, AnonContext::StoredInBinding);
                }
            }
            Stmt::Assign { target, value, .. } => {
                self.classify_escapes_in_expr(&target.0, &target.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_expr(
                    &value.0,
                    &value.1,
                    in_fork,
                    AnonContext::StoredInBinding,
                );
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.classify_escapes_in_expr(
                    &condition.0,
                    &condition.1,
                    in_fork,
                    AnonContext::Other,
                );
                self.classify_escapes_in_block(then_block, in_fork);
                if let Some(eb) = else_block {
                    if let Some(b) = &eb.block {
                        self.classify_escapes_in_block(b, in_fork);
                    }
                    if let Some(if_stmt) = &eb.if_stmt {
                        self.classify_escapes_in_stmt(&if_stmt.0, in_fork);
                    }
                }
            }
            Stmt::IfLet {
                expr,
                body,
                else_body,
                ..
            } => {
                self.classify_escapes_in_expr(&expr.0, &expr.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_block(body, in_fork);
                if let Some(b) = else_body {
                    self.classify_escapes_in_block(b, in_fork);
                }
            }
            Stmt::Match { scrutinee, arms } => {
                self.classify_escapes_in_expr(
                    &scrutinee.0,
                    &scrutinee.1,
                    in_fork,
                    AnonContext::Other,
                );
                for arm in arms {
                    if let Some((g, gs)) = &arm.guard {
                        self.classify_escapes_in_expr(g, gs, in_fork, AnonContext::Other);
                    }
                    self.classify_escapes_in_expr(
                        &arm.body.0,
                        &arm.body.1,
                        in_fork,
                        AnonContext::Other,
                    );
                }
            }
            Stmt::Loop { body, .. } => self.classify_escapes_in_block(body, in_fork),
            Stmt::For { iterable, body, .. } => {
                self.classify_escapes_in_expr(
                    &iterable.0,
                    &iterable.1,
                    in_fork,
                    AnonContext::Other,
                );
                self.classify_escapes_in_block(body, in_fork);
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.classify_escapes_in_expr(
                    &condition.0,
                    &condition.1,
                    in_fork,
                    AnonContext::Other,
                );
                self.classify_escapes_in_block(body, in_fork);
            }
            Stmt::WhileLet { expr, body, .. } => {
                self.classify_escapes_in_expr(&expr.0, &expr.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_block(body, in_fork);
            }
            Stmt::Break { value, .. } => {
                if let Some((e, s)) = value {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::Tail);
                }
            }
            Stmt::Continue { .. } => {}
            Stmt::Return(opt) => {
                if let Some((e, s)) = opt {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::Returned);
                }
            }
            Stmt::Defer(boxed) => {
                self.classify_escapes_in_expr(&boxed.0, &boxed.1, in_fork, AnonContext::Other);
            }
            Stmt::Expression((e, s)) => {
                self.classify_escapes_in_expr(e, s, in_fork, AnonContext::Other);
            }
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "Expression visitor traverses the full Expr surface; \
                  arm-per-variant is the clearest form"
    )]
    fn classify_escapes_in_expr(
        &mut self,
        expr: &Expr,
        expr_span: &hew_parser::ast::Span,
        in_fork: bool,
        ctx: AnonContext,
    ) {
        match expr {
            Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
                // Anonymous closure literal — classify by parent ctx.
                // When the literal is constructed *inside* a `fork { ... }`
                // body the closure's identity is tied to the forked task's
                // lifetime; this dominates the tail/stored/other contexts
                // the block walker would otherwise apply.
                let fact = if in_fork {
                    ClosureEscapeFact {
                        kind: ClosureEscapeKind::Forked,
                        rule: ClosureEscapeRule::InsideForkBlock,
                    }
                } else {
                    match ctx {
                        AnonContext::InForkBody => ClosureEscapeFact {
                            kind: ClosureEscapeKind::Forked,
                            rule: ClosureEscapeRule::InsideForkBlock,
                        },
                        AnonContext::Returned => ClosureEscapeFact {
                            kind: ClosureEscapeKind::Escapes,
                            rule: ClosureEscapeRule::Returned,
                        },
                        AnonContext::Tail => ClosureEscapeFact {
                            kind: ClosureEscapeKind::Escapes,
                            rule: ClosureEscapeRule::EscapesViaBlockValue,
                        },
                        AnonContext::PassedToHigherOrder => ClosureEscapeFact {
                            kind: ClosureEscapeKind::Escapes,
                            rule: ClosureEscapeRule::PassedToHigherOrder,
                        },
                        AnonContext::StoredInBinding => ClosureEscapeFact {
                            kind: ClosureEscapeKind::Escapes,
                            rule: ClosureEscapeRule::StoredOrSent,
                        },
                        AnonContext::Other => ClosureEscapeFact {
                            kind: ClosureEscapeKind::Escapes,
                            rule: ClosureEscapeRule::NoStaticBinding,
                        },
                    }
                };
                // Only insert if not already inserted by the let-bound
                // path (in which case the let-bound fact wins).
                self.closure_escape_facts
                    .entry(SpanKey::in_module(expr_span, self.current_module_idx))
                    .or_insert(fact);
                self.maybe_emit_escape_advisory(expr_span, fact);
                // Recurse into the body so nested closures inside this
                // lambda get classified too.
                self.classify_escapes_in_expr(
                    &body.0,
                    &body.1,
                    /* in_fork = */ false,
                    AnonContext::Other,
                );
            }
            Expr::ForkBlock { body } => {
                self.classify_escapes_in_block(body, /* in_fork = */ true);
            }
            Expr::ForkChild { expr, .. } => {
                self.classify_escapes_in_expr(
                    &expr.0,
                    &expr.1,
                    /* in_fork = */ true,
                    AnonContext::InForkBody,
                );
            }
            Expr::Scope { body } => {
                self.classify_escapes_in_block(body, /* in_fork = */ false);
            }
            Expr::ScopeDeadline { duration, body } => {
                self.classify_escapes_in_expr(
                    &duration.0,
                    &duration.1,
                    in_fork,
                    AnonContext::Other,
                );
                self.classify_escapes_in_block(body, in_fork);
            }
            Expr::Block(block) => self.classify_escapes_in_block(block, in_fork),
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.classify_escapes_in_expr(
                    &condition.0,
                    &condition.1,
                    in_fork,
                    AnonContext::Other,
                );
                self.classify_escapes_in_expr(&then_block.0, &then_block.1, in_fork, ctx);
                if let Some(eb) = else_block {
                    self.classify_escapes_in_expr(&eb.0, &eb.1, in_fork, ctx);
                }
            }
            Expr::IfLet {
                expr,
                body,
                else_body,
                ..
            } => {
                self.classify_escapes_in_expr(&expr.0, &expr.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_block(body, in_fork);
                if let Some(b) = else_body {
                    self.classify_escapes_in_block(b, in_fork);
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.classify_escapes_in_expr(
                    &scrutinee.0,
                    &scrutinee.1,
                    in_fork,
                    AnonContext::Other,
                );
                for arm in arms {
                    if let Some((g, gs)) = &arm.guard {
                        self.classify_escapes_in_expr(g, gs, in_fork, AnonContext::Other);
                    }
                    self.classify_escapes_in_expr(&arm.body.0, &arm.body.1, in_fork, ctx);
                }
            }
            Expr::Call { function, args, .. } => {
                self.classify_escapes_in_expr(
                    &function.0,
                    &function.1,
                    in_fork,
                    AnonContext::Other,
                );
                for arg in args {
                    let (e, s) = arg.expr();
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::PassedToHigherOrder);
                }
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.classify_escapes_in_expr(
                    &receiver.0,
                    &receiver.1,
                    in_fork,
                    AnonContext::Other,
                );
                for arg in args {
                    let (e, s) = arg.expr();
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::PassedToHigherOrder);
                }
            }
            Expr::Spawn { target, args, .. } => {
                self.classify_escapes_in_expr(&target.0, &target.1, in_fork, AnonContext::Other);
                for (_, (e, s)) in args {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::PassedToHigherOrder);
                }
            }
            Expr::StructInit { fields, base, .. } => {
                for (_, (e, s)) in fields {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::StoredInBinding);
                }
                if let Some(b) = base {
                    self.classify_escapes_in_expr(&b.0, &b.1, in_fork, AnonContext::Other);
                }
            }
            Expr::ContextVariant(context) => {
                if let Some(record) = &context.record {
                    for (_, (e, s)) in &record.fields {
                        self.classify_escapes_in_expr(e, s, in_fork, AnonContext::StoredInBinding);
                    }
                    if let Some(base) = &record.base {
                        self.classify_escapes_in_expr(
                            &base.0,
                            &base.1,
                            in_fork,
                            AnonContext::Other,
                        );
                    }
                }
            }
            Expr::GenericApplySuffix { target, .. } => {
                self.classify_escapes_in_expr(&target.0, &target.1, in_fork, ctx);
            }
            Expr::RecordInitSuffix {
                target,
                fields,
                base,
            } => {
                self.classify_escapes_in_expr(&target.0, &target.1, in_fork, AnonContext::Other);
                for (_, (e, s)) in fields {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::StoredInBinding);
                }
                if let Some(base) = base {
                    self.classify_escapes_in_expr(&base.0, &base.1, in_fork, AnonContext::Other);
                }
            }
            Expr::Tuple(items) | Expr::Array(items) => {
                for (e, s) in items {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::StoredInBinding);
                }
            }
            Expr::ArrayRepeat { value, count } => {
                self.classify_escapes_in_expr(
                    &value.0,
                    &value.1,
                    in_fork,
                    AnonContext::StoredInBinding,
                );
                self.classify_escapes_in_expr(&count.0, &count.1, in_fork, AnonContext::Other);
            }
            Expr::MapLiteral { entries } => {
                for ((k, ks), (v, vs)) in entries {
                    self.classify_escapes_in_expr(k, ks, in_fork, AnonContext::StoredInBinding);
                    self.classify_escapes_in_expr(v, vs, in_fork, AnonContext::StoredInBinding);
                }
            }
            Expr::Binary { left, right, .. } => {
                self.classify_escapes_in_expr(&left.0, &left.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_expr(&right.0, &right.1, in_fork, AnonContext::Other);
            }
            Expr::Unary { operand, .. } | Expr::Clone(operand) => {
                self.classify_escapes_in_expr(&operand.0, &operand.1, in_fork, AnonContext::Other);
            }
            Expr::FieldAccess { object, .. } => {
                self.classify_escapes_in_expr(&object.0, &object.1, in_fork, AnonContext::Other);
            }
            Expr::Index { object, index } => {
                self.classify_escapes_in_expr(&object.0, &object.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_expr(&index.0, &index.1, in_fork, AnonContext::Other);
            }
            Expr::Cast { expr, .. } => {
                self.classify_escapes_in_expr(&expr.0, &expr.1, in_fork, AnonContext::Other);
            }
            Expr::PostfixTry(inner) => {
                self.classify_escapes_in_expr(&inner.0, &inner.1, in_fork, ctx);
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start {
                    self.classify_escapes_in_expr(&s.0, &s.1, in_fork, AnonContext::Other);
                }
                if let Some(e) = end {
                    self.classify_escapes_in_expr(&e.0, &e.1, in_fork, AnonContext::Other);
                }
            }
            Expr::Is { lhs, rhs } => {
                self.classify_escapes_in_expr(&lhs.0, &lhs.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_expr(&rhs.0, &rhs.1, in_fork, AnonContext::Other);
            }
            Expr::Select { arms, timeout } => {
                for arm in arms {
                    self.classify_escapes_in_expr(
                        &arm.source.0,
                        &arm.source.1,
                        in_fork,
                        AnonContext::Other,
                    );
                    self.classify_escapes_in_expr(
                        &arm.body.0,
                        &arm.body.1,
                        in_fork,
                        AnonContext::Other,
                    );
                }
                if let Some(t) = timeout {
                    self.classify_escapes_in_expr(
                        &t.duration.0,
                        &t.duration.1,
                        in_fork,
                        AnonContext::Other,
                    );
                    self.classify_escapes_in_expr(
                        &t.body.0,
                        &t.body.1,
                        in_fork,
                        AnonContext::Other,
                    );
                }
            }
            Expr::Join(items) => {
                for (e, s) in items {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::PassedToHigherOrder);
                }
            }
            Expr::Timeout { expr, duration } => {
                self.classify_escapes_in_expr(&expr.0, &expr.1, in_fork, ctx);
                self.classify_escapes_in_expr(
                    &duration.0,
                    &duration.1,
                    in_fork,
                    AnonContext::Other,
                );
            }
            Expr::UnsafeBlock(block) => self.classify_escapes_in_block(block, in_fork),
            // `yield <expr>` and `return <expr>` both carry the operand out of
            // the closure to a higher-order boundary; same escape context.
            Expr::Yield(opt) | Expr::Return(opt) => {
                if let Some(boxed) = opt {
                    self.classify_escapes_in_expr(
                        &boxed.0,
                        &boxed.1,
                        in_fork,
                        AnonContext::PassedToHigherOrder,
                    );
                }
            }
            Expr::Await(inner) | Expr::AwaitRestart(inner) => {
                self.classify_escapes_in_expr(&inner.0, &inner.1, in_fork, AnonContext::Other);
            }
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let StringPart::Expr((e, s)) | StringPart::StructuralExpr((e, s)) = part {
                        self.classify_escapes_in_expr(e, s, in_fork, AnonContext::Other);
                    }
                }
            }
            Expr::MachineEmit { fields, .. } => {
                for (_, (e, s)) in fields {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::PassedToHigherOrder);
                }
            }
            Expr::GenBlock { body } => self.classify_escapes_in_block(body, in_fork),
            Expr::Literal(_)
            | Expr::Identifier(_)
            | Expr::QualifiedAssoc(_)
            | Expr::This
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_) => {}
        }
    }

    fn maybe_emit_escape_advisory(
        &mut self,
        lambda_span: &hew_parser::ast::Span,
        fact: ClosureEscapeFact,
    ) {
        // Advisory diagnostic when conservatively classified `Escapes`
        // AND the rule indicates restructuring could admit `Local`.
        // Emitted at warning severity (the diagnostic surface has no
        // Info level).
        if !matches!(fact.kind, ClosureEscapeKind::Escapes) {
            return;
        }
        // PassedToHigherOrder is intentionally excluded: inlining a let-bound
        // closure at its call site does not relieve the escape — an anonymous
        // closure in argument position is still classified PassedToHigherOrder
        // (via AnonContext::PassedToHigherOrder), so the advisory would fire
        // again.  Only rules where inlining genuinely admits Local are kept.
        let admit_local = matches!(
            fact.rule,
            ClosureEscapeRule::EscapesViaBlockValue | ClosureEscapeRule::NoStaticBinding
        );
        if !admit_local {
            return;
        }
        // One advisory per closure literal: the classifier visits the same
        // span more than once (let-bound block walk + anonymous-expression
        // walk; top-level item list + module graph for the entry module).
        // Gate on first-insert; distinct spans still warn independently.
        if !self
            .closure_escape_advisory_spans
            .insert(SpanKey::from(lambda_span))
        {
            return;
        }
        self.warnings.push(crate::error::TypeError {
            severity: crate::error::Severity::Warning,
            kind: TypeErrorKind::ClosureEscapeAdvisory {
                rule: format!("{:?}", fact.rule),
            },
            span: lambda_span.clone(),
            message: format!(
                "closure conservatively classified as escaping ({:?}); \
                 inlining the closure at its call site would admit `Local`",
                fact.rule
            ),
            notes: vec![],
            suggestions: vec![],
            source_module: None,
        });
    }

    /// Post-pass: walk the program once collecting every closure
    /// literal span, then verify `closure_capture_facts` and
    /// `closure_escape_facts` each carry an entry for that span. A
    /// missing entry trips the corresponding fail-closed diagnostic
    /// (`ClosureCaptureModeUnresolved` / `ClosureEscapeKindUnresolved`).
    ///
    /// The pass is a defensive contract enforcer for the
    /// checker→MIR-lowering boundary: it is intentionally noisy when
    /// the checker has a structural gap and silent when the contract
    /// holds. It does NOT classify or default; it only reports gaps.
    fn validate_closure_facts_complete(&mut self, program: &Program) {
        let mut sites: Vec<(Span, Option<String>, u32)> = Vec::new();
        collect_closure_literal_spans(program, &mut sites);
        let mut diagnostics = Vec::new();
        emit_unresolved_closure_diagnostics(
            &sites,
            &self.closure_capture_facts,
            &self.closure_escape_facts,
            &mut diagnostics,
        );
        for err in diagnostics {
            self.errors.push(err);
        }
    }
}

/// Walk a program's AST and append every closure literal span
/// (`Expr::Lambda` / `Expr::SpawnLambdaActor`) it finds, paired with
/// the capture-name when the literal is the value of a top-level
/// `let <name> = |...| ...` binding (None otherwise — for diagnostic
/// hint only) and the owning `module_idx` (0 for the root unit, N for
/// the N-th non-root module in topo order).
///
/// The `module_idx` MUST be assigned exactly as the checker assigns
/// `current_module_idx` while body-checking (`check_program`: iterate
/// `module_graph.topo_order`, skip the root, and bump a 1-based index
/// only when the module is actually present in `modules`). The
/// capture/escape facts for each closure are stamped with that same
/// index, so the fail-closed lookup below can only line up if this walk
/// reproduces the assignment site-for-site.
fn collect_closure_literal_spans(program: &Program, out: &mut Vec<(Span, Option<String>, u32)>) {
    let mut root_sites: Vec<(Span, Option<String>)> = Vec::new();
    for (item, _) in &program.items {
        collect_lambda_spans_in_item(item, &mut root_sites);
    }
    for (span, name) in root_sites {
        out.push((span, name, 0));
    }
    if let Some(mg) = &program.module_graph {
        let mut module_idx: u32 = 0;
        for mod_id in &mg.topo_order {
            if *mod_id == mg.root {
                continue;
            }
            if let Some(module) = mg.modules.get(mod_id) {
                module_idx += 1;
                let mut module_sites: Vec<(Span, Option<String>)> = Vec::new();
                for (item, _) in &module.items {
                    collect_lambda_spans_in_item(item, &mut module_sites);
                }
                for (span, name) in module_sites {
                    out.push((span, name, module_idx));
                }
            }
        }
    }
}

fn collect_lambda_spans_in_item(item: &Item, out: &mut Vec<(Span, Option<String>)>) {
    match item {
        Item::Function(fn_decl) => collect_lambda_spans_in_block(&fn_decl.body, out),
        Item::Impl(impl_decl) => {
            for method in &impl_decl.methods {
                collect_lambda_spans_in_block(&method.body, out);
            }
        }
        Item::Actor(actor) => {
            for method in &actor.methods {
                collect_lambda_spans_in_block(&method.body, out);
            }
            for rec in &actor.receive_fns {
                collect_lambda_spans_in_block(&rec.body, out);
            }
        }
        Item::Trait(trait_decl) => {
            for trait_item in &trait_decl.items {
                if let TraitItem::Method(trait_method) = trait_item {
                    if let Some(body) = &trait_method.body {
                        collect_lambda_spans_in_block(body, out);
                    }
                }
            }
        }
        _ => {}
    }
}

fn collect_lambda_spans_in_block(block: &Block, out: &mut Vec<(Span, Option<String>)>) {
    for (stmt, _) in &block.stmts {
        collect_lambda_spans_in_stmt(stmt, out);
    }
    if let Some(tail) = &block.trailing_expr {
        collect_lambda_spans_in_expr(&tail.0, &tail.1, out);
    }
}

#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "Statement visitor over the full Stmt surface; arm-per-variant \
              is the clearest form"
)]
fn collect_lambda_spans_in_stmt(stmt: &Stmt, out: &mut Vec<(Span, Option<String>)>) {
    match stmt {
        Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
            if let Some((e, s)) = value {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Stmt::Assign { target, value, .. } => {
            collect_lambda_spans_in_expr(&target.0, &target.1, out);
            collect_lambda_spans_in_expr(&value.0, &value.1, out);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_lambda_spans_in_expr(&condition.0, &condition.1, out);
            collect_lambda_spans_in_block(then_block, out);
            if let Some(eb) = else_block {
                if let Some(b) = &eb.block {
                    collect_lambda_spans_in_block(b, out);
                }
                if let Some(if_stmt) = &eb.if_stmt {
                    collect_lambda_spans_in_stmt(&if_stmt.0, out);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            collect_lambda_spans_in_expr(&expr.0, &expr.1, out);
            collect_lambda_spans_in_block(body, out);
            if let Some(b) = else_body {
                collect_lambda_spans_in_block(b, out);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            collect_lambda_spans_in_expr(&scrutinee.0, &scrutinee.1, out);
            for arm in arms {
                if let Some((g, gs)) = &arm.guard {
                    collect_lambda_spans_in_expr(g, gs, out);
                }
                collect_lambda_spans_in_expr(&arm.body.0, &arm.body.1, out);
            }
        }
        Stmt::Loop { body, .. } => collect_lambda_spans_in_block(body, out),
        Stmt::For { iterable, body, .. } => {
            collect_lambda_spans_in_expr(&iterable.0, &iterable.1, out);
            collect_lambda_spans_in_block(body, out);
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_lambda_spans_in_expr(&condition.0, &condition.1, out);
            collect_lambda_spans_in_block(body, out);
        }
        Stmt::WhileLet { expr, body, .. } => {
            collect_lambda_spans_in_expr(&expr.0, &expr.1, out);
            collect_lambda_spans_in_block(body, out);
        }
        Stmt::Break { value, .. } => {
            if let Some((e, s)) = value {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Stmt::Continue { .. } => {}
        Stmt::Return(opt) => {
            if let Some((e, s)) = opt {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Stmt::Defer(boxed) => {
            collect_lambda_spans_in_expr(&boxed.0, &boxed.1, out);
        }
        Stmt::Expression((e, s)) => collect_lambda_spans_in_expr(e, s, out),
    }
}

#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "Expression visitor over the full Expr surface; arm-per-variant \
              is the clearest form"
)]
fn collect_lambda_spans_in_expr(
    expr: &Expr,
    expr_span: &Span,
    out: &mut Vec<(Span, Option<String>)>,
) {
    match expr {
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            out.push((expr_span.clone(), None));
            collect_lambda_spans_in_expr(&body.0, &body.1, out);
        }
        Expr::Block(block) => collect_lambda_spans_in_block(block, out),
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_lambda_spans_in_expr(&condition.0, &condition.1, out);
            collect_lambda_spans_in_expr(&then_block.0, &then_block.1, out);
            if let Some(eb) = else_block {
                collect_lambda_spans_in_expr(&eb.0, &eb.1, out);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            collect_lambda_spans_in_expr(&expr.0, &expr.1, out);
            collect_lambda_spans_in_block(body, out);
            if let Some(b) = else_body {
                collect_lambda_spans_in_block(b, out);
            }
        }
        Expr::Match { scrutinee, arms } => {
            collect_lambda_spans_in_expr(&scrutinee.0, &scrutinee.1, out);
            for arm in arms {
                if let Some((g, gs)) = &arm.guard {
                    collect_lambda_spans_in_expr(g, gs, out);
                }
                collect_lambda_spans_in_expr(&arm.body.0, &arm.body.1, out);
            }
        }
        Expr::Call { function, args, .. } => {
            collect_lambda_spans_in_expr(&function.0, &function.1, out);
            for arg in args {
                let (e, s) = arg.expr();
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            collect_lambda_spans_in_expr(&receiver.0, &receiver.1, out);
            for arg in args {
                let (e, s) = arg.expr();
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Expr::Spawn { target, args, .. } => {
            collect_lambda_spans_in_expr(&target.0, &target.1, out);
            for (_, (e, s)) in args {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Expr::StructInit { fields, base, .. } => {
            for (_, (e, s)) in fields {
                collect_lambda_spans_in_expr(e, s, out);
            }
            if let Some(b) = base {
                collect_lambda_spans_in_expr(&b.0, &b.1, out);
            }
        }
        Expr::ContextVariant(context) => {
            if let Some(record) = &context.record {
                for (_, (e, s)) in &record.fields {
                    collect_lambda_spans_in_expr(e, s, out);
                }
                if let Some(base) = &record.base {
                    collect_lambda_spans_in_expr(&base.0, &base.1, out);
                }
            }
        }
        Expr::GenericApplySuffix { target, .. } => {
            collect_lambda_spans_in_expr(&target.0, &target.1, out);
        }
        Expr::RecordInitSuffix {
            target,
            fields,
            base,
        } => {
            collect_lambda_spans_in_expr(&target.0, &target.1, out);
            for (_, (e, s)) in fields {
                collect_lambda_spans_in_expr(e, s, out);
            }
            if let Some(base) = base {
                collect_lambda_spans_in_expr(&base.0, &base.1, out);
            }
        }
        Expr::Tuple(items) | Expr::Array(items) => {
            for (e, s) in items {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            collect_lambda_spans_in_expr(&value.0, &value.1, out);
            collect_lambda_spans_in_expr(&count.0, &count.1, out);
        }
        Expr::MapLiteral { entries } => {
            for ((k, ks), (v, vs)) in entries {
                collect_lambda_spans_in_expr(k, ks, out);
                collect_lambda_spans_in_expr(v, vs, out);
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_lambda_spans_in_expr(&left.0, &left.1, out);
            collect_lambda_spans_in_expr(&right.0, &right.1, out);
        }
        Expr::Unary { operand, .. } | Expr::Clone(operand) => {
            collect_lambda_spans_in_expr(&operand.0, &operand.1, out);
        }
        Expr::FieldAccess { object, .. } => {
            collect_lambda_spans_in_expr(&object.0, &object.1, out);
        }
        Expr::Index { object, index } => {
            collect_lambda_spans_in_expr(&object.0, &object.1, out);
            collect_lambda_spans_in_expr(&index.0, &index.1, out);
        }
        Expr::Cast { expr, .. } => collect_lambda_spans_in_expr(&expr.0, &expr.1, out),
        Expr::PostfixTry(inner) => collect_lambda_spans_in_expr(&inner.0, &inner.1, out),
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                collect_lambda_spans_in_expr(&s.0, &s.1, out);
            }
            if let Some(e) = end {
                collect_lambda_spans_in_expr(&e.0, &e.1, out);
            }
        }
        Expr::Is { lhs, rhs } => {
            collect_lambda_spans_in_expr(&lhs.0, &lhs.1, out);
            collect_lambda_spans_in_expr(&rhs.0, &rhs.1, out);
        }
        Expr::Scope { body } => collect_lambda_spans_in_block(body, out),
        Expr::ForkChild { expr, .. } => {
            collect_lambda_spans_in_expr(&expr.0, &expr.1, out);
        }
        Expr::ForkBlock { body } => collect_lambda_spans_in_block(body, out),
        Expr::ScopeDeadline { duration, body } => {
            collect_lambda_spans_in_expr(&duration.0, &duration.1, out);
            collect_lambda_spans_in_block(body, out);
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                collect_lambda_spans_in_expr(&arm.source.0, &arm.source.1, out);
                collect_lambda_spans_in_expr(&arm.body.0, &arm.body.1, out);
            }
            if let Some(t) = timeout {
                collect_lambda_spans_in_expr(&t.duration.0, &t.duration.1, out);
                collect_lambda_spans_in_expr(&t.body.0, &t.body.1, out);
            }
        }
        Expr::Join(items) => {
            for (e, s) in items {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Expr::Timeout { expr, duration } => {
            collect_lambda_spans_in_expr(&expr.0, &expr.1, out);
            collect_lambda_spans_in_expr(&duration.0, &duration.1, out);
        }
        Expr::UnsafeBlock(block) => collect_lambda_spans_in_block(block, out),
        Expr::Yield(opt) => {
            if let Some(boxed) = opt {
                collect_lambda_spans_in_expr(&boxed.0, &boxed.1, out);
            }
        }
        Expr::Await(inner) | Expr::AwaitRestart(inner) => {
            collect_lambda_spans_in_expr(&inner.0, &inner.1, out);
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr((e, s)) | StringPart::StructuralExpr((e, s)) = part {
                    collect_lambda_spans_in_expr(e, s, out);
                }
            }
        }
        Expr::MachineEmit { fields, .. } => {
            for (_, (e, s)) in fields {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Expr::GenBlock { body } => collect_lambda_spans_in_block(body, out),
        Expr::Return(opt) => {
            if let Some(boxed) = opt {
                collect_lambda_spans_in_expr(&boxed.0, &boxed.1, out);
            }
        }
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::QualifiedAssoc(_)
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_) => {}
    }
}

/// Fail-closed defense: for every collected closure literal span,
/// emit the corresponding `Unresolved` diagnostic if a fact is missing.
/// Pure function so unit tests can drive it with synthetic maps.
pub(crate) fn emit_unresolved_closure_diagnostics(
    sites: &[(Span, Option<String>, u32)],
    capture_facts: &HashMap<SpanKey, Vec<ClosureCaptureFact>>,
    escape_facts: &HashMap<SpanKey, ClosureEscapeFact>,
    out: &mut Vec<TypeError>,
) {
    for (span, hint_name, module_idx) in sites {
        // Key each site with its owning module index (assigned by
        // `collect_closure_literal_spans` to mirror the checker), so the
        // lookup matches the `SpanKey::in_module(span, current_module_idx)`
        // the capture/escape facts were stamped with.
        let key = SpanKey::in_module(span, *module_idx);
        if !capture_facts.contains_key(&key) {
            let name = hint_name.clone().unwrap_or_else(|| "<closure>".to_string());
            out.push(TypeError {
                severity: crate::error::Severity::Error,
                kind: TypeErrorKind::ClosureCaptureModeUnresolved { name: name.clone() },
                span: span.clone(),
                message: format!(
                    "internal: closure literal `{name}` reached checker output \
                     without a resolved capture-mode ledger; the \
                     checker→MIR contract requires every closure to \
                     have a `ClosureCaptureFact` set"
                ),
                notes: vec![],
                suggestions: vec![],
                source_module: None,
            });
        }
        if !escape_facts.contains_key(&key) {
            out.push(TypeError {
                severity: crate::error::Severity::Error,
                kind: TypeErrorKind::ClosureEscapeKindUnresolved,
                span: span.clone(),
                message: "internal: closure literal reached checker output without \
                     a resolved `ClosureEscapeKind`; the checker→MIR contract \
                     requires every closure literal to be classified"
                    .to_string(),
                notes: vec![],
                suggestions: vec![],
                source_module: None,
            });
        }
    }
}

/// Context label propagated to anonymous closure literals so they pick
/// up the right `ClosureEscapeFact` when no `let f = ...` binding
/// gives them a name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AnonContext {
    /// Inside a `fork { ... }` body or `fork name = ...` child binding.
    InForkBody,
    /// In a `return expr` position.
    Returned,
    /// Tail expression of a block — escapes via block value.
    Tail,
    /// Argument to a call / method-call / spawn / yield / join.
    PassedToHigherOrder,
    /// RHS of a let/var/assign — stored in another binding.
    StoredInBinding,
    /// Anything else.
    Other,
}

/// Collect every `actor` declaration in the program (root items + each
/// module-graph module).
///
/// Returns `(owner_identity, actor_decl)` per actor. `owner_identity` is the
/// full-path dotted declaration identity: module path `["a","b"]` yields
/// `"a.b.Alpha"`, while root actors remain bare (`"Alpha"`). It is the sole
/// semantic key used for signature lookup, descriptor publication, symbols,
/// and collision attribution. Import aliases and leaf-qualified spellings are
/// surface bindings and must not enter this declaration-identity walk.
///
/// The walk is read-only so it can run after the checker has frozen its
/// mutable state.
fn collect_program_actors(program: &Program) -> Vec<(String, &ActorDecl)> {
    let mut actors: Vec<(String, &ActorDecl)> = Vec::new();
    for (item, _) in &program.items {
        if let Item::Actor(ad) = item {
            actors.push((ad.name.clone(), ad));
        }
    }
    if let Some(mg) = &program.module_graph {
        for (mod_id, module) in &mg.modules {
            if *mod_id == mg.root {
                continue;
            }
            let module_owner = mod_id.path.join(".");
            for (item, _) in &module.items {
                if let Item::Actor(ad) = item {
                    let owner_identity = if module_owner.is_empty() {
                        ad.name.clone()
                    } else {
                        format!("{module_owner}.{}", ad.name)
                    };
                    actors.push((owner_identity, ad));
                }
            }
        }
    }
    actors
}

/// Build [`ActorProtocolDescriptor`]s for every actor in the program, using
/// each `receive fn`'s resolved type signature (param types + return type)
/// drawn from `fn_sigs` keyed `"Actor::handler"`.
///
/// On collision: emits a `TypeErrorKind::ActorProtocolCollision` diagnostic
/// against the second-colliding handler's span and **omits** the actor from
/// the returned map. Downstream consumers must treat a missing entry
/// fail-closed.
///
/// On unresolved signatures (e.g. an upstream type error left a handler
/// param without a concrete `ResolvedTy`): silently skips the actor. The
/// surfacing diagnostic is already emitted elsewhere; piling on with a
/// derivative error here would be noise.
fn build_actor_protocol_descriptors(
    program: &Program,
    fn_sigs: &HashMap<String, crate::check::types::FnSig>,
    errors: &mut Vec<TypeError>,
) -> HashMap<String, crate::actor_protocol::ActorProtocolDescriptor> {
    let mut descriptors: HashMap<String, crate::actor_protocol::ActorProtocolDescriptor> =
        HashMap::new();
    // Cross-actor collision tracking (defense-in-depth): every published
    // handler's `(msg_id, actor, handler, span)`. `msg_id` is the 32-bit
    // transport discriminant on every cross-node frame; two DISTINCT actors
    // sharing it leave the wire protocol ambiguous. The runtime keys its codec
    // registry by `(actor-type, msg_id)` so routing is correct in-process, but
    // refusing the collision at the source of truth keeps the wire unambiguous
    // for relays / mixed-binary peers (the `boundary-fail-closed` invariant).
    let mut cross_actor_seen: Vec<(u32, String, String, std::ops::Range<usize>)> = Vec::new();
    for (actor_identity, ad) in collect_program_actors(program) {
        if ad.receive_fns.is_empty() {
            continue;
        }
        let mut specs: Vec<crate::actor_protocol::ActorHandlerSpec> =
            Vec::with_capacity(ad.receive_fns.len());
        let mut all_signatures_resolved = true;
        for rf in &ad.receive_fns {
            let key = format!("{actor_identity}::{}", rf.name);
            let Some(sig) = fn_sigs.get(&key) else {
                all_signatures_resolved = false;
                break;
            };
            let mut param_tys: Vec<crate::ResolvedTy> = Vec::with_capacity(sig.params.len());
            let mut any_unresolved = false;
            for p in &sig.params {
                if let Ok(rt) = crate::ResolvedTy::from_ty(p) {
                    param_tys.push(rt);
                } else {
                    any_unresolved = true;
                    break;
                }
            }
            if any_unresolved {
                all_signatures_resolved = false;
                break;
            }
            let Ok(return_ty) = crate::ResolvedTy::from_ty(&sig.return_type) else {
                all_signatures_resolved = false;
                break;
            };
            // Symbol mangling is owned by MIR/codegen; for slice 1 we record
            // a stable surface-derived symbol string so the descriptor row
            // is self-describing. Downstream consumers may continue to
            // derive their own emit name today; subsequent Q87 slices route
            // codegen through this `symbol` field.
            let symbol = format!("{actor_identity}__{}", rf.name);
            specs.push(crate::actor_protocol::ActorHandlerSpec {
                name: rf.name.clone(),
                param_tys,
                return_ty,
                symbol,
            });
        }
        if !all_signatures_resolved {
            // A handler signature failed to resolve; the underlying type
            // error is already in `errors`. Skip publishing a partial
            // descriptor — fail-closed downstream is preferable to a
            // half-populated protocol.
            continue;
        }

        // The descriptor is a source declaration fact, so its identity is the
        // actor's full owner path. Surface aliases remain resolver bindings and
        // never become protocol identities.
        match crate::actor_protocol::ActorProtocolDescriptor::from_handlers(
            actor_identity.clone(),
            &specs,
        ) {
            Ok(descriptor) => {
                // Record each handler's msg_id for the cross-actor pass.
                for h in &descriptor.handlers {
                    let span = ad
                        .receive_fns
                        .iter()
                        .find(|rf| rf.name == h.name)
                        .map_or(0..0, |rf| rf.span.clone());
                    cross_actor_seen.push((h.msg_id, actor_identity.clone(), h.name.clone(), span));
                }
                descriptors.insert(actor_identity.clone(), descriptor);
            }
            Err(collision) => {
                // Pin the diagnostic to the second-colliding handler's span
                // so the user can jump straight to one of the two offenders.
                // The diagnostic message names both, and the hint mentions
                // the (not-yet-parseable) `#[msg_id(N)]` opt-in attribute
                // so the wording stays accurate when the later Q87 slice
                // lands the attribute.
                let span = ad
                    .receive_fns
                    .iter()
                    .find(|rf| rf.name == collision.handler_b)
                    .map_or(0..0, |rf| rf.span.clone());
                let message = format!(
                    "actor `{}` has two `receive fn`s with the same msg_id 0x{:08x}: `{}` and `{}`",
                    collision.actor_name,
                    collision.msg_id,
                    collision.handler_a,
                    collision.handler_b,
                );
                let mut err = TypeError::new(
                    TypeErrorKind::ActorProtocolCollision {
                        actor_name: collision.actor_name.clone(),
                        handler_a: collision.handler_a.clone(),
                        handler_b: collision.handler_b.clone(),
                        msg_id: collision.msg_id,
                    },
                    span,
                    message,
                );
                err = err.with_suggestion(format!(
                    "rename `{}` or `{}` so their fully-qualified names hash to distinct \
                     msg_ids; explicit `#[msg_id(N)]` pinning is reserved for a future \
                     release (not yet supported)",
                    collision.handler_a, collision.handler_b
                ));
                errors.push(err);
                // Fail-closed: descriptor is absent from the map.
            }
        }
    }

    report_cross_actor_msg_id_collisions(&cross_actor_seen, errors);

    descriptors
}

/// Defense-in-depth cross-actor pass: a `msg_id` shared by two DISTINCT actors
/// is a 32-bit cross-node wire-discriminant collision. Emits one
/// `CrossActorProtocolCollision` per colliding `msg_id` — the first
/// distinct-actor pair found — pinned to the later actor's handler span. `seen`
/// is every published handler's `(msg_id, actor, handler, span)` over the WHOLE
/// program's actor set (so cross-module collisions are caught); intra-actor dups
/// were already refused before this runs, so each entry is from a collision-free
/// (within-its-actor) descriptor.
fn report_cross_actor_msg_id_collisions(
    seen: &[(u32, String, String, std::ops::Range<usize>)],
    errors: &mut Vec<TypeError>,
) {
    let mut reported_msg_ids: Vec<u32> = Vec::new();
    for i in 0..seen.len() {
        let (msg_id_i, actor_i, handler_i, span_i) = &seen[i];
        if reported_msg_ids.contains(msg_id_i) {
            continue;
        }
        let Some((_, actor_j, handler_j, _)) = seen[..i]
            .iter()
            .find(|(mid, actor_j, _, _)| mid == msg_id_i && actor_j != actor_i)
        else {
            continue;
        };
        reported_msg_ids.push(*msg_id_i);
        let message = format!(
            "actors `{actor_j}` and `{actor_i}` have `receive fn`s with the same \
             cross-node msg_id 0x{msg_id_i:08x}: `{actor_j}.{handler_j}` and \
             `{actor_i}.{handler_i}` — the 32-bit wire discriminant is ambiguous"
        );
        let mut err = TypeError::new(
            TypeErrorKind::CrossActorProtocolCollision {
                actor_a: actor_j.clone(),
                handler_a: handler_j.clone(),
                actor_b: actor_i.clone(),
                handler_b: handler_i.clone(),
                msg_id: *msg_id_i,
            },
            span_i.clone(),
            message,
        );
        err = err.with_suggestion(format!(
            "rename `{actor_j}.{handler_j}` or `{actor_i}.{handler_i}` so their \
             fully-qualified names hash to distinct msg_ids; explicit `#[msg_id(N)]` \
             pinning is reserved for a future release (not yet supported)"
        ));
        errors.push(err);
    }
}
