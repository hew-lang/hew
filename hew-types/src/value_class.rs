//! The value-class authority.
//!
//! A type's *class* decides its SIR ownership kind and destructor shape;
//! its *clone kind* decides whether a `copy_value` exists and
//! what it costs. Both are computed here, from the resolved type plus the
//! declaration facts the class table needs beyond the type itself, and from nowhere else.
//!
//! The rule is **total** over [`ResolvedTy`]: there is no `Unknown`, no default
//! class and no "assume `BitCopy`". A type this module cannot class is an
//! [`Err`] the caller fails closed on.

use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};

use crate::builtin_type::BuiltinType;
use crate::resolved_ty::{substitute_type_params as substitute, ResolvedTy};
use crate::type_facts::CloneKind;

/// Ownership class of a value.
///
/// The variant order is the join order of the Aggregate rule: a later variant
/// wins over an earlier one, with `BitCopy` and `View` tied at the bottom and
/// `CowValue` and `PersistentShare` tied one step above.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueClass {
    /// Bit-copyable, carries no obligation.
    BitCopy,
    /// Non-owning extern-boundary view (`Slice`, `Pointer`, `Borrow`).
    View,
    /// Heap value with a retain path; one obligation per value.
    CowValue,
    /// Refcounted share (closures, function values, trait objects).
    PersistentShare,
    /// Single-owner value with an implicit destructor.
    AffineResource,
    /// Single-owner value with **no** implicit destructor; must be consumed.
    Linear,
}

impl ValueClass {
    /// Join order of the aggregate rule: any field `Linear` → `Linear`;
    /// else any field `AffineResource` → `AffineResource`; else any field
    /// `CowValue` or `PersistentShare` → `CowValue`; else `BitCopy`.
    const fn join_rank(self) -> u8 {
        match self {
            Self::BitCopy | Self::View => 0,
            Self::CowValue | Self::PersistentShare => 1,
            Self::AffineResource => 2,
            Self::Linear => 3,
        }
    }

    /// Join two member classes under the aggregate rule.
    ///
    /// A `View` or `PersistentShare` member collapses to the rank's canonical
    /// spelling because an aggregate holding one is not itself a view or a
    /// share: the class table names `BitCopy` and `CowValue` as the two joined
    /// outcomes below `AffineResource`.
    #[must_use]
    pub fn join(self, other: Self) -> Self {
        let rank = self.join_rank().max(other.join_rank());
        match rank {
            0 => Self::BitCopy,
            1 => Self::CowValue,
            2 => Self::AffineResource,
            _ => Self::Linear,
        }
    }
}

/// The user-written ownership attribute on a type declaration.
///
/// This mirrors `hew_parser::ast::ResourceMarker`, which is single-valued: a
/// type is never both `#[linear]` and `#[resource]`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum DeclarationMarker {
    #[default]
    None,
    Resource,
    Linear,
}

/// One type declaration's facts, as the class table needs them.
///
/// `members` are the record's field types or the enum's variant payload types,
/// in declaration order, before substitution. They may mention the declaration's
/// own `type_params`.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct DeclaredType {
    /// Source-owned encoding representation selected by the checker.
    /// A source spelling or opacity alone cannot grant this discriminator.
    pub builtin: Option<BuiltinType>,
    pub marker: DeclarationMarker,
    /// The declaration carries `#[opaque]`, so its members are not the whole
    /// value and the Aggregate rule cannot see through it.
    ///
    /// This checked declaration is the classification authority. Types may carry
    /// a copy for ABI/layout consumers; that annotation cannot change the class.
    pub is_opaque: bool,
    /// Every value owns an indirect enum box, independently of whether its
    /// payload is recursive or contains other managed values.
    pub is_indirect: bool,
    pub type_params: Vec<crate::ParamHead>,
    pub members: Vec<ResolvedTy>,
}

/// Where the aggregate rule gets a declaration's marker and members.
///
/// A borrowed lookup rather than an owned map so the checker can answer from
/// its own registry and `type_defs` without copying them, and a unit test can
/// answer from a hand-built map.
pub trait ClassDeclarations {
    /// The declaration's facts, or `None` when this lookup does not carry it.
    ///
    /// `None` is a refusal: the class rule turns it into
    /// [`ClassError::UnknownDeclaration`] and the caller fails closed. A
    /// lookup that cannot render one of a declaration's member types must
    /// answer `None` rather than an aggregate over the members it managed.
    fn declared_type(&self, id: crate::NominalId) -> Option<Cow<'_, DeclaredType>>;

    /// The exact source declaration implementing a compiler-owned aggregate.
    fn builtin_declaration(&self, builtin: BuiltinType) -> Option<crate::NominalId>;

    /// The source binder on which an implicit associated-type binder depends.
    /// Ordinary parameters are their own source.
    fn parameter_source(&self, parameter: crate::ParamHead) -> crate::ParamHead {
        parameter
    }

    /// Instantiate declaration members using the checked associated-type schema.
    ///
    /// # Errors
    /// Refuses a member whose projection cannot be resolved at this instance.
    fn instantiate_members(
        &self,
        declaration: &DeclaredType,
        arguments: &[ResolvedTy],
    ) -> Result<Vec<ResolvedTy>, ClassError> {
        Ok(declaration
            .members
            .iter()
            .map(|member| substitute(member, &declaration.type_params, arguments))
            .collect())
    }
}

impl ClassDeclarations for BTreeMap<crate::NominalId, DeclaredType> {
    fn declared_type(&self, id: crate::NominalId) -> Option<Cow<'_, DeclaredType>> {
        self.get(&id).map(Cow::Borrowed)
    }

    fn builtin_declaration(&self, builtin: BuiltinType) -> Option<crate::NominalId> {
        self.iter()
            .find_map(|(id, declaration)| (declaration.builtin == Some(builtin)).then_some(*id))
    }
}

/// The lookup [`ClassContext::empty`] answers from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NoDeclarations;

impl ClassDeclarations for NoDeclarations {
    fn declared_type(&self, _id: crate::NominalId) -> Option<Cow<'_, DeclaredType>> {
        None
    }

    fn builtin_declaration(&self, _builtin: BuiltinType) -> Option<crate::NominalId> {
        None
    }
}

static NO_DECLARATIONS: NoDeclarations = NoDeclarations;

/// The declaration facts the class table needs beyond the type itself: the
/// `#[resource]`/`#[linear]` marker and, for the Aggregate rule, the field and
/// variant types.
///
/// Built once from the checker's own tables (`TraitRegistry::is_resource` /
/// `is_linear` and `Checker.type_defs`). Both live in `hew-types`, so this is
/// not a cross-crate join.
///
/// Declaration and recursive-instance lookup use nominal identity. Display
/// spelling cannot change the ownership facts selected for a type.
///
/// Copying this borrowed view does not duplicate the declaration tables.
#[derive(Clone, Copy)]
pub struct ClassContext<'a> {
    declarations: &'a dyn ClassDeclarations,
}

impl std::fmt::Debug for ClassContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ClassContext")
    }
}

impl<'a> ClassContext<'a> {
    #[must_use]
    pub const fn new(declarations: &'a dyn ClassDeclarations) -> Self {
        Self { declarations }
    }

    /// A context with no declarations at all.
    ///
    /// Every `Named { builtin: None }` type then classes
    /// [`ClassError::UnknownDeclaration`], which callers fail closed on. This
    /// is the context a consumer uses when it has no `TypeCheckOutput` to build
    /// one from.
    #[must_use]
    pub const fn empty() -> Self {
        Self {
            declarations: &NO_DECLARATIONS,
        }
    }

    #[must_use]
    pub fn declaration(&self, id: crate::NominalId) -> Option<Cow<'_, DeclaredType>> {
        self.declarations.declared_type(id)
    }
}

/// Why a type has no class.
///
/// Every variant is a fail-closed refusal. There is no fallback class.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassError {
    /// A callable claims independent duplication over an owning resource field.
    CallableCloneConflict,
    /// The class table `TypeParam` row: the instance service substitutes first, so an
    /// abstract parameter never reaches SIR.
    TypeParam { param: crate::ParamHead },
    /// The declaration carrying the marker and the member types is not in the
    /// [`ClassContext`].
    UnknownDeclaration { name: String },
    /// The class table: `Iterator`, `ActorState` and `MachineState` are compiler-internal
    /// names, never the type of a value.
    NotAValueType { builtin: BuiltinType },
    /// The declaration's own members reach it at a *growing* instantiation
    /// (`type L<T> { n: L<Vec<T>> }`), directly or through another declaration.
    /// The argument grows on every turn of the cycle, so every instantiation on
    /// it is a distinct type, the walk has no finite fixpoint to join and there
    /// is no default class: this refuses, at every instantiation of `L`.
    ///
    /// This is a property of the declaration, not of one walk: a declaration
    /// whose members never reach it — `type Wrapper<T> { value: T }` — refuses
    /// nothing, however deeply a caller nests it in its own argument.
    RecursiveInstantiation { name: String },
}

impl std::fmt::Display for ClassError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CallableCloneConflict => f.write_str("callable Clone capability conflicts with captured ownership"),
            Self::TypeParam { param } => {
                write!(f, "abstract type parameter `{}` has no value class", param.spelling)
            }
            Self::UnknownDeclaration { name } => {
                write!(f, "no declaration facts for type `{name}`")
            }
            Self::NotAValueType { builtin } => write!(
                f,
                "`{}` is a compiler-internal name, never the type of a value",
                builtin.canonical_name()
            ),
            Self::RecursiveInstantiation { name } => write!(
                f,
                "`{name}`'s members form a growing generic cycle, so its member walk has no finite fixpoint"
            ),
        }
    }
}

impl ValueClass {
    /// The class of a resolved type, total over `ResolvedTy` given the
    /// declarations.
    ///
    /// # Errors
    ///
    /// Returns [`ClassError`] when the type is an abstract parameter, is a
    /// compiler-internal non-value name, or needs a declaration the context
    /// does not carry. There is no `Unknown` and no default.
    pub fn of_ty(ty: &ResolvedTy, decls: &ClassContext<'_>) -> Result<Self, ClassError> {
        classify_ty(ty, decls).map(|facts| facts.0)
    }
}

/// The class **and** clone kind of a resolved type.
///
/// Both columns come from one walk: the Aggregate rule joins classes and
/// clone kinds together, and computing them separately would walk the type
/// twice and invite the two answers to disagree.
///
/// # Errors
///
/// See [`ValueClass::of_ty`].
///
/// # Panics
///
/// Panics if the internal classification graph does not produce its root fact.
pub fn classify_ty(
    ty: &ResolvedTy,
    decls: &ClassContext<'_>,
) -> Result<(ValueClass, CloneKind), ClassError> {
    Walk::default().query(ty, decls)
}

type ClassFacts = Result<(ValueClass, CloneKind), ClassError>;

/// One exact instantiated graph owns both class and clone facts. Recursive
/// facts start at the bottom; declaration boxes and collections contribute
/// their own heap ownership. Dependents settle before a result is published.
#[derive(Debug, Clone, Default)]
pub(crate) struct Walk {
    identities: HashMap<ResolvedTy, usize>,
    nodes: Vec<ClassNode>,
    current: Option<usize>,
    cyclic: bool,
    polymorphic: BTreeMap<crate::NominalId, bool>,
    used_parameters: BTreeMap<crate::NominalId, Vec<bool>>,
}

#[derive(Debug, Clone)]
struct ClassNode {
    ty: ResolvedTy,
    facts: Option<ClassFacts>,
    computing: bool,
    users: HashSet<usize>,
    queued: bool,
}

impl Walk {
    pub(crate) fn query(&mut self, ty: &ResolvedTy, decls: &ClassContext<'_>) -> ClassFacts {
        let first_new = self.nodes.len();
        // Errors also leave graph nodes behind. Settle their dependencies before
        // publishing any result from this immutable declaration epoch.
        let _ = classify(ty, decls, self);
        self.settle(decls, first_new);
        self.nodes[self.identities[ty]]
            .facts
            .clone()
            .expect("classified root")
    }

    fn is_polymorphic(&mut self, id: crate::NominalId, decls: &ClassContext<'_>) -> bool {
        if let Some(answer) = self.polymorphic.get(&id) {
            return *answer;
        }
        self.discover_parameter_uses(id, decls);
        self.polymorphic
            .extend(instantiation_growth(id, decls, &self.used_parameters));
        self.polymorphic.get(&id).copied().unwrap_or(false)
    }

    /// Solve parameter use over declarations, without expanding instantiated
    /// members. A parameter matters only if a stored member can reach its value.
    /// Recursive declarations propagate those facts to a finite fixed point.
    fn discover_parameter_uses(&mut self, root: crate::NominalId, decls: &ClassContext<'_>) {
        if self.used_parameters.contains_key(&root) {
            return;
        }
        let mut pending = vec![root];
        let mut declarations = BTreeMap::new();
        while let Some(id) = pending.pop() {
            if self.used_parameters.contains_key(&id) {
                continue;
            }
            let Some(declaration) = decls.declaration(id) else {
                continue;
            };
            self.used_parameters
                .insert(id, vec![false; declaration.type_params.len()]);
            for member in &declaration.members {
                visit_class_inputs(member, decls, None, &mut |input| {
                    if let Some(id) = member_declaration(input, decls) {
                        pending.push(id);
                    }
                });
            }
            declarations.insert(id, declaration);
        }
        loop {
            let mut changed = false;
            for (id, declaration) in &declarations {
                let mut used = self.used_parameters[id].clone();
                for member in &declaration.members {
                    visit_class_inputs(member, decls, Some(&self.used_parameters), &mut |input| {
                        if let Some(parameter) = type_parameter(input) {
                            let source = if declaration.type_params.contains(&parameter) {
                                parameter
                            } else {
                                decls.declarations.parameter_source(parameter)
                            };
                            if let Some(index) =
                                declaration.type_params.iter().position(|p| *p == source)
                            {
                                used[index] = true;
                            }
                        }
                    });
                }
                if used != self.used_parameters[id] {
                    self.used_parameters.insert(*id, used);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
    }

    fn intern(&mut self, ty: &ResolvedTy) -> usize {
        let id = if let Some(id) = self.identities.get(ty) {
            *id
        } else {
            let id = self.nodes.len();
            self.identities.insert(ty.clone(), id);
            self.nodes.push(ClassNode {
                ty: ty.clone(),
                facts: None,
                computing: false,
                users: HashSet::new(),
                queued: false,
            });
            id
        };
        if let Some(current) = self.current {
            self.nodes[id].users.insert(current);
        }
        id
    }

    fn settle(&mut self, decls: &ClassContext<'_>, first_new: usize) {
        if !self.cyclic {
            return;
        }
        self.cyclic = false;
        let mut pending: VecDeque<_> = (first_new..self.nodes.len()).collect();
        for node in &mut self.nodes[first_new..] {
            node.queued = true;
        }
        while let Some(id) = pending.pop_front() {
            self.nodes[id].queued = false;
            self.current = Some(id);
            let ty = self.nodes[id].ty.clone();
            let facts = classify_uncached(&ty, decls, self);
            self.current = None;
            if self.nodes[id].facts.as_ref() == Some(&facts) {
                continue;
            }
            self.nodes[id].facts = Some(facts);
            let users: Vec<_> = self.nodes[id].users.iter().copied().collect();
            for user in users {
                if !self.nodes[user].queued {
                    self.nodes[user].queued = true;
                    pending.push_back(user);
                }
            }
        }
    }
}

/// Does `ty` mention one of `params`, the mentioning declaration's own type
/// parameters, anywhere inside it?
///
/// An argument that mentions none of them is a constant: substituting the
/// declaration's parameters cannot change it, so an edge carrying it reaches
/// one fixed instantiation however many times the cycle turns.
fn mentions_type_param(
    ty: &ResolvedTy,
    params: &[crate::ParamHead],
    decls: &ClassContext<'_>,
) -> bool {
    if is_own_parameter(ty, params, decls) {
        return true;
    }
    match ty {
        ResolvedTy::GenericCallable {
            parameters,
            callable,
        } => {
            let free: Vec<_> = params
                .iter()
                .copied()
                .filter(|parameter| !parameters.iter().any(|bound| bound.parameter == *parameter))
                .collect();
            mentions_type_param(callable, &free, decls)
        }
        ResolvedTy::Tuple(elements) => elements
            .iter()
            .any(|element| mentions_type_param(element, params, decls)),
        ResolvedTy::Array(element, _) | ResolvedTy::Slice(element) => {
            mentions_type_param(element, params, decls)
        }
        ResolvedTy::Task(inner) => mentions_type_param(inner, params, decls),
        ResolvedTy::Named { args, .. } => args
            .iter()
            .any(|arg| mentions_type_param(arg, params, decls)),
        ResolvedTy::Closure {
            params: p,
            ret,
            captures,
            ..
        } => {
            p.iter()
                .any(|parameter| mentions_type_param(&parameter.ty, params, decls))
                || mentions_type_param(ret, params, decls)
                || captures
                    .iter()
                    .any(|t| mentions_type_param(t, params, decls))
        }
        ResolvedTy::Function { params: p, ret, .. } => {
            p.iter()
                .any(|parameter| mentions_type_param(&parameter.ty, params, decls))
                || mentions_type_param(ret, params, decls)
        }
        _ => false,
    }
}

/// Is `arg` one of `params`, the mentioning declaration's own type parameters?
///
/// A parameter reaches here spelled either as an abstract `TypeParam` or, when
/// the declaration was resolved without a type-parameter scope, as a
/// zero-argument user `Named`. [`substitute`] reads both spellings and so does
/// this. Checked associated binders retain their dependence on the source
/// parameter through the declaration's schema metadata.
fn type_parameter(arg: &ResolvedTy) -> Option<crate::ParamHead> {
    match arg {
        ResolvedTy::TypeParam { name } => Some(*name),
        ResolvedTy::Named {
            head: crate::TypeHead::Param(param),
            args,
            ..
        } if args.is_empty() => Some(*param),
        _ => None,
    }
}

fn is_own_parameter(
    arg: &ResolvedTy,
    params: &[crate::ParamHead],
    decls: &ClassContext<'_>,
) -> bool {
    type_parameter(arg).is_some_and(|parameter| {
        params.contains(&parameter)
            || params.contains(&decls.declarations.parameter_source(parameter))
    })
}

/// Nominal declarations reached by the member walk. Other builtins have their
/// own rows, even when a source declaration also exists.
fn member_declaration(ty: &ResolvedTy, decls: &ClassContext<'_>) -> Option<crate::NominalId> {
    let ResolvedTy::Named { head, .. } = ty else {
        return None;
    };
    if head.is_actor() {
        return None;
    }
    match head.builtin() {
        Some(BuiltinType::CrashInfo | BuiltinType::CrashNotification) => head
            .builtin()
            .and_then(|builtin| decls.declarations.builtin_declaration(builtin)),
        None => head.nominal(),
        Some(_) => None,
    }
}

/// Visit dependencies within a member expression. Nominal arguments
/// contribute only through parameters used by that declaration's members.
/// Before the use summary is solved, `None` discovers all possible dependencies.
fn visit_class_inputs(
    ty: &ResolvedTy,
    decls: &ClassContext<'_>,
    used_parameters: Option<&BTreeMap<crate::NominalId, Vec<bool>>>,
    visit: &mut impl FnMut(&ResolvedTy),
) {
    visit(ty);
    if let ResolvedTy::Named { head, args, .. } = ty {
        // Actor protocols and pool roles are identities, not nested values.
        if head.is_actor() || head.builtin() == Some(BuiltinType::SupervisorPool) {
            return;
        }
        let declaration = member_declaration(ty, decls);
        for (index, arg) in args.iter().enumerate() {
            if used_parameters.is_none_or(|uses| {
                declaration
                    .and_then(|id| uses.get(&id))
                    .and_then(|positions| positions.get(index))
                    .copied()
                    .unwrap_or(true)
            }) {
                visit_class_inputs(arg, decls, used_parameters, visit);
            }
        }
    } else {
        // Flat ownership is not flat type structure: pointer pointees and
        // callable signatures still participate in concrete shape admission.
        let mut components = Vec::new();
        crate::type_facts::push_type_components(ty, &mut components);
        for component in components {
            visit_class_inputs(&component, decls, used_parameters, visit);
        }
    }
}

/// Solve argument growth over a finite graph of declaration parameter positions.
/// Refuse constructor-bearing parameter cycles. Constants cut the flow, while
/// selecting or permuting bare parameters has a finite orbit. Associated
/// binders retain dependence on their checked source parameter.
fn instantiation_growth(
    root: crate::NominalId,
    decls: &ClassContext<'_>,
    used_parameters: &BTreeMap<crate::NominalId, Vec<bool>>,
) -> BTreeMap<crate::NominalId, bool> {
    let mut declarations = BTreeMap::new();
    let mut pending = vec![root];
    while let Some(id) = pending.pop() {
        if declarations.contains_key(&id) {
            continue;
        }
        let Some(declaration) = decls.declaration(id) else {
            continue;
        };
        let mut mentions = Vec::new();
        for member in &declaration.members {
            visit_class_inputs(member, decls, Some(used_parameters), &mut |input| {
                if let ResolvedTy::Named { args, .. } = input {
                    if let Some(id) = member_declaration(input, decls) {
                        pending.push(id);
                        mentions.push((id, args.clone()));
                    }
                }
            });
        }
        declarations.insert(id, (declaration, mentions));
    }

    let mut nodes = Vec::new();
    let mut edges: HashMap<_, HashSet<_>> = HashMap::new();
    let mut growing = HashSet::new();
    for (&id, (declaration, mentions)) in &declarations {
        nodes.extend((0..declaration.type_params.len()).map(|index| (id, index)));
        for (mentioned, args) in mentions {
            let Some((target, _)) = declarations.get(mentioned) else {
                continue;
            };
            for (index, arg) in args.iter().take(target.type_params.len()).enumerate() {
                let to = (*mentioned, index);
                for (position, parameter) in declaration.type_params.iter().enumerate() {
                    if mentions_type_param(arg, &[*parameter], decls) {
                        let from = (id, position);
                        edges.entry(from).or_default().insert(to);
                        // A schema parameter preserves dependence on its base.
                        // A constructor around that dependency adds growth.
                        if type_parameter(arg).is_none() {
                            growing.insert((from, to));
                        }
                    }
                }
            }
        }
    }
    let mut answers: BTreeMap<_, _> = declarations.keys().map(|id| (*id, false)).collect();
    for component in crate::cycle::tarjan_scc(&nodes, &edges) {
        let members: HashSet<_> = component.iter().copied().collect();
        if component.iter().any(|from| {
            edges
                .get(from)
                .into_iter()
                .flatten()
                .any(|to| members.contains(to) && growing.contains(&(*from, *to)))
        }) {
            for (id, _) in component {
                answers.insert(id, true);
            }
        }
    }
    answers
}

/// Class and clone of a heap collection over its element facts (the class table
/// `Vec`/`HashMap`/`HashSet` and fixed-array rows).
///
/// The collection is never `BitCopy`: its buffer is heap, so the class floor
/// is `CowValue`.
fn collection_facts(elements: &[(ValueClass, CloneKind)]) -> (ValueClass, CloneKind) {
    let class = elements
        .iter()
        .fold(ValueClass::CowValue, |acc, (class, _)| acc.join(*class));
    let clone = if elements.iter().any(|(_, clone)| *clone == CloneKind::None) {
        CloneKind::None
    } else if elements
        .iter()
        .all(|(class, _)| *class == ValueClass::BitCopy)
    {
        CloneKind::DeepCopy
    } else {
        CloneKind::FieldWise
    };
    (class, clone)
}

/// Class and clone of an ordinary aggregate over its member facts (the class table
/// Aggregate rule): records, enums, tuples, `Option`/`Result`.
fn aggregate_facts(members: &[(ValueClass, CloneKind)]) -> (ValueClass, CloneKind) {
    let class = members
        .iter()
        .fold(ValueClass::BitCopy, |acc, (class, _)| acc.join(*class));
    let clone = if class == ValueClass::BitCopy {
        CloneKind::Bits
    } else if members.iter().any(|(_, clone)| *clone == CloneKind::None) {
        CloneKind::None
    } else {
        CloneKind::FieldWise
    };
    (class, clone)
}

fn classify_all(
    tys: &[ResolvedTy],
    decls: &ClassContext<'_>,
    walk: &mut Walk,
) -> Result<Vec<(ValueClass, CloneKind)>, ClassError> {
    tys.iter().map(|ty| classify(ty, decls, walk)).collect()
}

fn classify(ty: &ResolvedTy, decls: &ClassContext<'_>, walk: &mut Walk) -> ClassFacts {
    let id = walk.intern(ty);
    if let Some(facts) = &walk.nodes[id].facts {
        return facts.clone();
    }
    if walk.nodes[id].computing {
        walk.cyclic = true;
        return Ok((ValueClass::BitCopy, CloneKind::Bits));
    }
    walk.nodes[id].computing = true;
    let parent = walk.current.replace(id);
    let facts = classify_uncached(ty, decls, walk);
    walk.current = parent;
    walk.nodes[id].computing = false;
    walk.nodes[id].facts = Some(facts.clone());
    facts
}

#[expect(
    clippy::too_many_lines,
    reason = "the class table is one closed match with a row per ResolvedTy arm and BuiltinType variant"
)]
fn classify_uncached(
    ty: &ResolvedTy,
    decls: &ClassContext<'_>,
    walk: &mut Walk,
) -> Result<(ValueClass, CloneKind), ClassError> {
    let bits = (ValueClass::BitCopy, CloneKind::Bits);
    let view = (ValueClass::View, CloneKind::Bits);
    let cow_retain = (ValueClass::CowValue, CloneKind::Retain);
    let affine_none = (ValueClass::AffineResource, CloneKind::None);
    let affine_retain = (ValueClass::AffineResource, CloneKind::Retain);
    // A trait object erases its concrete type, and its vtable carries a drop
    // slot but no clone slot, so there is nothing a copy could call. The
    // checker already refuses `dyn T.clone()`; recording `CloneKind::None`
    // keeps the physical stages from selecting a clone action that has no
    // implementation behind it.
    let share_none = (ValueClass::PersistentShare, CloneKind::None);
    let linear_none = (ValueClass::Linear, CloneKind::None);

    Ok(match ty {
        ResolvedTy::GenericCallable { callable, .. } => classify(callable, decls, walk)?,
        // Actor values share their descriptor, never the actor allocation or
        // the issuer's export authority. Local descriptor retain/release is a
        // no-op; an imported descriptor is reclaimed after its last local use.
        ResolvedTy::Named {
            head: crate::TypeHead::Actor(_),
            ..
        } => (ValueClass::PersistentShare, CloneKind::Retain),
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
        | ResolvedTy::Never => bits,
        ResolvedTy::String | ResolvedTy::Bytes => cow_retain,
        ResolvedTy::Generator { .. } | ResolvedTy::CancellationToken | ResolvedTy::Task(_) => {
            affine_none
        }
        ResolvedTy::Slice(_) | ResolvedTy::Pointer { .. } | ResolvedTy::Borrow { .. } => view,
        ResolvedTy::TraitObject { .. } => share_none,
        ResolvedTy::Function { capabilities, .. } => {
            if capabilities.clone {
                (ValueClass::CowValue, CloneKind::FieldWise)
            } else {
                affine_none
            }
        }
        ResolvedTy::Closure {
            capabilities,
            captures,
            ..
        } => {
            let capture_facts = classify_all(captures, decls, walk)?;
            let class = if capture_facts
                .iter()
                .any(|(class, _)| *class == ValueClass::Linear)
            {
                ValueClass::Linear
            } else if !capabilities.clone {
                ValueClass::AffineResource
            } else if capture_facts.iter().any(|(class, clone)| {
                *class == ValueClass::AffineResource || *clone == CloneKind::None
            }) {
                return Err(ClassError::CallableCloneConflict);
            } else {
                ValueClass::CowValue
            };
            if capabilities.clone && class == ValueClass::Linear {
                return Err(ClassError::CallableCloneConflict);
            }
            (
                class,
                if capabilities.clone {
                    CloneKind::FieldWise
                } else {
                    CloneKind::None
                },
            )
        }
        ResolvedTy::Tuple(elements) => aggregate_facts(&classify_all(elements, decls, walk)?),
        // Arrays own their element storage; copying uses the element recipe.
        ResolvedTy::Array(element, _) => collection_facts(&[classify(element, decls, walk)?]),
        ResolvedTy::TypeParam { name } => return Err(ClassError::TypeParam { param: *name }),
        ResolvedTy::Named {
            head: head @ crate::TypeHead::Builtin(builtin),
            args,
            ..
        } => match builtin {
            // marker BitCopy today
            BuiltinType::SupervisorPool
            | BuiltinType::NodeId
            | BuiltinType::Location
            | BuiltinType::MonitorId
            | BuiltinType::DownTarget
            | BuiltinType::DownReason
            | BuiltinType::DownNotification
            // These builtins are bit-copy values.
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
            // the class table decision, overrides `marker() = Resource`: a pid never owns
            // the actor, so its drop frees nothing.
            | BuiltinType::ActorHandle
            // A lambda actor's handle is a pid under another spelling, and
            // owns the actor no more than `ActorHandle` does.
            | BuiltinType::ActorFn
            | BuiltinType::HewActor => bits,
            // Enums whose class is the join over their payload arguments.
            BuiltinType::Option | BuiltinType::Result => {
                aggregate_facts(&classify_all(args, decls, walk)?)
            }
            // Heap collections: aggregate over the element (key, value) classes
            // with a `CowValue` floor for the buffer.
            BuiltinType::Vec | BuiltinType::HashMap | BuiltinType::HashSet => {
                collection_facts(&classify_all(args, decls, walk)?)
            }
            // Both cursor records take their std declaration's own fields.
            // `HashMapIter<K, V>` is a record of two snapshot `Vec`s and a
            // `BitCopy` index, not a collection of `K` and `V`: reading it as
            // one gives `HashMapIter<i64, i64>` a `DeepCopy` clone that physical
            // MIR cannot realize, because the record's heap fields must be
            // cloned field-wise however cheap their elements are.
            BuiltinType::VecIter | BuiltinType::HashMapIter
            | BuiltinType::CrashInfo | BuiltinType::CrashNotification => {
                classify_declaration(head, args, decls, walk)?
            }
            BuiltinType::JsonValue | BuiltinType::YamlValue | BuiltinType::TomlValue => {
                (ValueClass::CowValue, CloneKind::DeepCopy)
            }
            BuiltinType::Rc | BuiltinType::Weak => affine_retain,
            BuiltinType::Generator
            | BuiltinType::BoxedActor
            | BuiltinType::Sink
            | BuiltinType::Stream
            | BuiltinType::MonitorRef
            | BuiltinType::CancellationToken | BuiltinType::Task | BuiltinType::ActorCall => affine_none,
            // Never the type of a value: `Iterator` is the std trait name, and
            // `ActorState`/`MachineState` are compiler-internal payload carriers.
            BuiltinType::Iterator | BuiltinType::ActorState | BuiltinType::MachineState => {
                return Err(ClassError::NotAValueType { builtin: *builtin })
            }
        },
        ResolvedTy::Named {
            head:
                head @ (crate::TypeHead::Nominal(_)
                | crate::TypeHead::Param(_)
                | crate::TypeHead::Unresolved(_)),
            args,
            ..
        } => {
            let name = head.registry_key();
            // `builtin` is the identity fact. A `Named` that carries none and
            // that the context holds no declaration for is refused in every
            // context, the empty one included: reading the name against the
            // builtin table here would be a second identity authority.
            let Some(declared) = head.nominal().and_then(|id| decls.declaration(id)) else {
                return Err(ClassError::UnknownDeclaration {
                    name: name.to_string(),
                });
            };
            match declared.marker {
                DeclarationMarker::Resource => affine_none,
                DeclarationMarker::Linear => linear_none,
                // A fieldless `#[opaque]` declaration with no ownership
                // marker is an FFI pass-through id whose lifecycle is owned
                // elsewhere: std's convention is a `#[resource]` wrapper
                // around it that owns the close (`Deque` around
                // `DequeHandle`, `Response` around `ResponseHandle`, …). The
                // id itself carries no obligation - it round-trips through
                // `extern "C"` as a bare pointer-width value - so it classes
                // `BitCopy` exactly like any other opaque handle with no
                // marker, whether or not its name happens to collide with a
                // compiler builtin (`Location`, `Handle`).
                DeclarationMarker::None if declared.is_opaque => bits,
                DeclarationMarker::None => classify_declaration(head, args, decls, walk)?,
            }
        }
    })
}

/// Aggregate rule over one declaration's substituted member types.
fn classify_declaration(
    head: &crate::TypeHead,
    args: &[ResolvedTy],
    decls: &ClassContext<'_>,
    walk: &mut Walk,
) -> Result<(ValueClass, CloneKind), ClassError> {
    let name = head.registry_key();
    let id = head
        .nominal()
        .or_else(|| {
            head.builtin()
                .and_then(|builtin| decls.declarations.builtin_declaration(builtin))
        })
        .ok_or_else(|| ClassError::UnknownDeclaration {
            name: name.to_string(),
        })?;
    let declared = decls
        .declaration(id)
        .ok_or_else(|| ClassError::UnknownDeclaration {
            name: name.to_string(),
        })?;
    if walk.is_polymorphic(id, decls) {
        return Err(ClassError::RecursiveInstantiation {
            name: name.to_string(),
        });
    }
    let members = decls.declarations.instantiate_members(&declared, args)?;
    let (class, clone) = aggregate_facts(&classify_all(&members, decls, walk)?);
    if declared.is_indirect {
        // Copying a box allocates independent storage and copies its payload.
        // Resource and linear payloads retain their stronger obligations.
        Ok((
            class.join(ValueClass::CowValue),
            if clone == CloneKind::None {
                clone
            } else {
                CloneKind::FieldWise
            },
        ))
    } else {
        Ok((class, clone))
    }
}

#[cfg(test)]
mod graph_tests {
    use super::*;

    fn nominal(name: &str) -> ResolvedTy {
        ResolvedTy::user_for_test(name, vec![])
    }

    #[test]
    fn nominal_classification_uses_the_checked_declaration_opacity() {
        let id = crate::NominalId::for_test("Record");
        let mut declarations = BTreeMap::from([(
            id,
            DeclaredType {
                members: vec![ResolvedTy::String],
                ..DeclaredType::default()
            },
        )]);
        let ordinary = nominal("Record");
        let mut annotated = ordinary.clone();
        let ResolvedTy::Named { is_opaque, .. } = &mut annotated else {
            unreachable!();
        };
        *is_opaque = true;
        for ty in [&ordinary, &annotated] {
            assert_eq!(
                classify_ty(ty, &ClassContext::new(&declarations)),
                Ok((ValueClass::CowValue, CloneKind::FieldWise))
            );
        }
        declarations.get_mut(&id).unwrap().is_opaque = true;
        for ty in [&ordinary, &annotated] {
            assert_eq!(
                classify_ty(ty, &ClassContext::new(&declarations)),
                Ok((ValueClass::BitCopy, CloneKind::Bits))
            );
        }
    }

    #[test]
    fn recursive_arguments_follow_transitive_parameter_use() {
        let parameter = crate::ParamHead::for_test("T");
        let parameter_ty = ResolvedTy::TypeParam { name: parameter };
        let root = |argument| ResolvedTy::user_for_test("Tree", vec![argument]);
        let intermediary = |argument| ResolvedTy::user_for_test("Wrapper", vec![argument]);
        let mut declarations = BTreeMap::from([
            (
                crate::NominalId::for_test("Tree"),
                DeclaredType {
                    type_params: vec![parameter],
                    members: vec![
                        parameter_ty.clone(),
                        intermediary(root(ResolvedTy::Named {
                            head: crate::TypeHead::Builtin(BuiltinType::Vec),
                            args: vec![parameter_ty.clone()],
                            is_opaque: false,
                        })),
                    ],
                    ..DeclaredType::default()
                },
            ),
            (
                crate::NominalId::for_test("Wrapper"),
                DeclaredType {
                    type_params: vec![parameter],
                    members: vec![ResolvedTy::I64],
                    ..DeclaredType::default()
                },
            ),
        ]);
        assert_eq!(
            classify_ty(&root(ResolvedTy::String), &ClassContext::new(&declarations)),
            Ok((ValueClass::CowValue, CloneKind::FieldWise))
        );

        // A stored parameter makes the growing argument reachable, including
        // when another declaration forwards it before it becomes a value.
        for member in [
            parameter_ty.clone(),
            ResolvedTy::Pointer {
                is_mutable: true,
                pointee: Box::new(parameter_ty.clone()),
            },
            ResolvedTy::user_for_test("Forward", vec![parameter_ty.clone()]),
        ] {
            declarations
                .get_mut(&crate::NominalId::for_test("Wrapper"))
                .unwrap()
                .members = vec![member];
            declarations.insert(
                crate::NominalId::for_test("Forward"),
                DeclaredType {
                    type_params: vec![parameter],
                    members: vec![parameter_ty.clone()],
                    ..DeclaredType::default()
                },
            );
            for marker in [DeclarationMarker::None, DeclarationMarker::Resource] {
                declarations
                    .get_mut(&crate::NominalId::for_test("Wrapper"))
                    .unwrap()
                    .marker = marker;
                assert!(matches!(
                    classify_ty(&root(ResolvedTy::String), &ClassContext::new(&declarations)),
                    Err(ClassError::RecursiveInstantiation { .. })
                ));
            }
        }
    }

    #[test]
    fn argument_growth_requires_a_parameter_cycle() {
        let first = crate::ParamHead::for_test("T");
        let second = crate::ParamHead::for_test("U");
        let first_ty = ResolvedTy::TypeParam { name: first };
        let second_ty = ResolvedTy::TypeParam { name: second };
        let wrapped = ResolvedTy::Named {
            head: crate::TypeHead::Builtin(BuiltinType::Vec),
            args: vec![second_ty.clone()],
            is_opaque: false,
        };
        let pair = |args| ResolvedTy::user_for_test("Pair", args);
        for (arguments, grows) in [
            (vec![second_ty, first_ty.clone()], false),
            (vec![wrapped.clone(), ResolvedTy::I64], false),
            (vec![wrapped, first_ty.clone()], true),
        ] {
            let declarations = BTreeMap::from([(
                crate::NominalId::for_test("Pair"),
                DeclaredType {
                    type_params: vec![first, second],
                    members: vec![first_ty.clone(), pair(arguments)],
                    ..DeclaredType::default()
                },
            )]);
            let facts = classify_ty(
                &pair(vec![ResolvedTy::String, ResolvedTy::I64]),
                &ClassContext::new(&declarations),
            );
            if grows {
                assert!(matches!(
                    facts,
                    Err(ClassError::RecursiveInstantiation { .. })
                ));
            } else {
                assert_eq!(facts, Ok((ValueClass::CowValue, CloneKind::FieldWise)));
            }
        }
    }

    #[test]
    fn shared_member_graph_retains_complete_class_and_clone_facts() {
        let mut declarations = BTreeMap::new();
        let leaf = crate::NominalId::for_test("Node0");
        declarations.insert(
            leaf,
            DeclaredType {
                members: vec![ResolvedTy::I64],
                ..DeclaredType::default()
            },
        );
        let mut root = nominal("Node0");
        for level in 1..=28 {
            let name = format!("Node{level}");
            declarations.insert(
                crate::NominalId::for_test(&name),
                DeclaredType {
                    members: vec![root.clone(), root],
                    ..DeclaredType::default()
                },
            );
            root = nominal(&name);
        }
        assert_eq!(
            classify_ty(&root, &ClassContext::new(&declarations)),
            Ok((ValueClass::BitCopy, CloneKind::Bits))
        );
        declarations.get_mut(&leaf).unwrap().marker = DeclarationMarker::Resource;
        assert_eq!(
            classify_ty(&root, &ClassContext::new(&declarations)),
            Ok((ValueClass::AffineResource, CloneKind::None))
        );
    }

    #[test]
    fn cycle_members_settle_before_a_sibling_callable_claims_clone() {
        let resource = crate::NominalId::for_test("Resource");
        let mut declarations = BTreeMap::from([
            (
                crate::NominalId::for_test("A"),
                DeclaredType {
                    members: vec![nominal("B"), nominal("Resource")],
                    ..DeclaredType::default()
                },
            ),
            (
                crate::NominalId::for_test("B"),
                DeclaredType {
                    members: vec![nominal("A")],
                    ..DeclaredType::default()
                },
            ),
            (resource, DeclaredType::default()),
        ]);
        let closure = ResolvedTy::Closure {
            capabilities: crate::CallableCapabilities {
                clone: true,
                ..crate::CallableCapabilities::default()
            },
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
            captures: vec![nominal("B")],
        };
        let root = ResolvedTy::Tuple(vec![nominal("A"), closure]);
        assert_eq!(
            classify_ty(&root, &ClassContext::new(&declarations)),
            Ok((ValueClass::CowValue, CloneKind::FieldWise))
        );
        for marker in [DeclarationMarker::Resource, DeclarationMarker::Linear] {
            declarations.get_mut(&resource).unwrap().marker = marker;
            assert_eq!(
                classify_ty(&root, &ClassContext::new(&declarations)),
                Err(ClassError::CallableCloneConflict),
                "an owning-cycle floor must not hide {marker:?} in a sibling's capture"
            );
        }
    }

    #[test]
    fn completed_class_states_keep_generic_arguments_distinct() {
        let parameter = crate::ParamHead::for_test("T");
        let declarations = BTreeMap::from([(
            crate::NominalId::for_test("Wrap"),
            DeclaredType {
                type_params: vec![parameter],
                members: vec![ResolvedTy::TypeParam { name: parameter }],
                ..DeclaredType::default()
            },
        )]);
        let root = ResolvedTy::Tuple(vec![
            ResolvedTy::user_for_test("Wrap", vec![ResolvedTy::I64]),
            ResolvedTy::user_for_test("Wrap", vec![ResolvedTy::String]),
        ]);
        assert_eq!(
            classify_ty(&root, &ClassContext::new(&declarations)),
            Ok((ValueClass::CowValue, CloneKind::FieldWise))
        );
    }
    #[test]
    fn retained_walk_does_not_revisit_settled_unrelated_declarations() {
        struct Counted {
            declarations: BTreeMap<crate::NominalId, DeclaredType>,
            reads: std::cell::RefCell<BTreeMap<crate::NominalId, usize>>,
        }
        impl ClassDeclarations for Counted {
            fn declared_type(&self, id: crate::NominalId) -> Option<Cow<'_, DeclaredType>> {
                *self.reads.borrow_mut().entry(id).or_default() += 1;
                self.declarations.get(&id).map(Cow::Borrowed)
            }
            fn builtin_declaration(&self, _: BuiltinType) -> Option<crate::NominalId> {
                None
            }
        }
        let plain = crate::NominalId::for_test("Plain");
        let cycle = crate::NominalId::for_test("Cycle");
        let plain_ty = ResolvedTy::user_for_test("Plain", vec![]);
        let cycle_ty = ResolvedTy::user_for_test("Cycle", vec![]);
        let declarations = Counted {
            declarations: BTreeMap::from([
                (
                    plain,
                    DeclaredType {
                        members: vec![ResolvedTy::I64],
                        ..DeclaredType::default()
                    },
                ),
                (
                    cycle,
                    DeclaredType {
                        is_indirect: true,
                        members: vec![cycle_ty.clone()],
                        ..DeclaredType::default()
                    },
                ),
            ]),
            reads: std::cell::RefCell::default(),
        };
        let context = ClassContext::new(&declarations);
        let mut walk = Walk::default();
        assert_eq!(
            walk.query(&plain_ty, &context),
            Ok((ValueClass::BitCopy, CloneKind::Bits))
        );
        let plain_reads = declarations.reads.borrow()[&plain];
        assert_eq!(
            walk.query(&cycle_ty, &context),
            Ok((ValueClass::CowValue, CloneKind::FieldWise))
        );
        assert_eq!(declarations.reads.borrow()[&plain], plain_reads);
        let reads = declarations.reads.borrow().clone();
        assert_eq!(
            walk.query(&cycle_ty, &context),
            Ok((ValueClass::CowValue, CloneKind::FieldWise))
        );
        assert_eq!(*declarations.reads.borrow(), reads);
    }
}
