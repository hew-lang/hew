//! The one value-class authority (`docs/internal/ir-ladder.md` §1.1).
//!
//! A type's *class* decides its SIR ownership kind (§1.2) and its destructor
//! shape (§5.2); its *clone kind* decides whether a `copy_value` exists and
//! what it costs. Both are computed here, from the resolved type plus the
//! declaration facts §1.1 needs beyond the type itself, and from nowhere else.
//!
//! The rule is **total** over [`ResolvedTy`]: there is no `Unknown`, no default
//! class and no "assume `BitCopy`". A type this module cannot class is an
//! [`Err`] the caller fails closed on.

use std::collections::{BTreeMap, BTreeSet};

use crate::builtin_type::BuiltinType;
use crate::resolved_ty::ResolvedTy;
use crate::type_facts::CloneKind;

/// Ownership class of a value, per `ir-ladder.md` §1.1.
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
    /// Join order of the §1.1 Aggregate rule: any field `Linear` → `Linear`;
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

    /// Join two member classes under the §1.1 Aggregate rule.
    ///
    /// A `View` or `PersistentShare` member collapses to the rank's canonical
    /// spelling because an aggregate holding one is not itself a view or a
    /// share: the §1.1 rule names `BitCopy` and `CowValue` as the two joined
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

/// One type declaration's facts, as §1.1 needs them.
///
/// `members` are the record's field types or the enum's variant payload types,
/// in declaration order, before substitution. They may mention the declaration's
/// own `type_params`.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct DeclaredType {
    pub marker: DeclarationMarker,
    /// The declaration carries `#[opaque]`, so its members are not the whole
    /// value and the Aggregate rule cannot see through it.
    ///
    /// This is the declaration's own fact. `ResolvedTy::Named.is_opaque` is the
    /// same fact spelled on a type, and only some producers stamp it, so §1.1
    /// reads whichever of the two says yes.
    pub is_opaque: bool,
    pub type_params: Vec<String>,
    pub members: Vec<ResolvedTy>,
}

/// Where the §1.1 Aggregate rule gets a declaration's marker and members.
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
    fn declared_type(&self, name: &str) -> Option<DeclaredType>;
}

impl ClassDeclarations for BTreeMap<String, DeclaredType> {
    fn declared_type(&self, name: &str) -> Option<DeclaredType> {
        self.get(name).cloned()
    }
}

/// The lookup [`ClassContext::empty`] answers from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NoDeclarations;

impl ClassDeclarations for NoDeclarations {
    fn declared_type(&self, _name: &str) -> Option<DeclaredType> {
        None
    }
}

static NO_DECLARATIONS: NoDeclarations = NoDeclarations;

/// The declaration facts §1.1 needs beyond the type itself: the
/// `#[resource]`/`#[linear]` marker and, for the Aggregate rule, the field and
/// variant types.
///
/// Built once from the checker's own tables (`TraitRegistry::is_resource` /
/// `is_linear` and `Checker.type_defs`). Both live in `hew-types`, so this is
/// not a cross-crate join.
///
/// MARKED SHORTCUT — declaration lookup is keyed by the declaration's canonical
/// path spelling, inherited from `main`.
/// WHY: `ResolvedTy::Named` carries no `DefId`, so a name is the only key a
/// resolved type offers.
/// WHEN: `IdentityTable::declare` (#3210) gives declarations an identity that a
/// later lane threads onto the type.
/// WHAT: this context keys on that `DefId` and the name map is deleted.
/// This is not a fact-table join: `type_facts` is keyed structurally by
/// [`crate::type_facts::TypeInstanceKey`], never by a name.
///
/// Deliberately not `Copy`: every §1.1 entry point takes it by reference, so
/// the class rule reads one context rather than silently duplicating it.
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
    pub fn declaration(&self, name: &str) -> Option<DeclaredType> {
        self.declarations.declared_type(name)
    }
}

/// Why a type has no class.
///
/// Every variant is a fail-closed refusal. There is no fallback class.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassError {
    /// §1.1 `TypeParam` row: the instance service substitutes first, so an
    /// abstract parameter never reaches SIR.
    TypeParam { name: String },
    /// The declaration carrying the marker and the member types is not in the
    /// [`ClassContext`].
    UnknownDeclaration { name: String },
    /// §1.1: `Iterator`, `ActorState` and `MachineState` are compiler-internal
    /// names, never the type of a value.
    NotAValueType { builtin: BuiltinType },
    /// The declaration's own members reach it at a *different* instantiation
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
            Self::TypeParam { name } => {
                write!(f, "abstract type parameter `{name}` has no value class")
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
                "`{name}`'s own members reach it at a different instantiation, so its member walk has no finite fixpoint"
            ),
        }
    }
}

impl ValueClass {
    /// The §1.1 class of a resolved type, total over `ResolvedTy` given the
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

/// The §1.1 class **and** clone kind of a resolved type.
///
/// Both columns come from one walk: the Aggregate rule joins classes and
/// clone kinds together, and computing them separately would walk the type
/// twice and invite the two answers to disagree.
///
/// # Errors
///
/// See [`ValueClass::of_ty`].
pub fn classify_ty(
    ty: &ResolvedTy,
    decls: &ClassContext<'_>,
) -> Result<(ValueClass, CloneKind), ClassError> {
    let mut walk = Walk::default();
    classify(ty, decls, &mut walk)
}

/// The state one [`classify_ty`] carries across its member walk.
///
/// `on_path` is the §1.1 owning-edge cut, keyed by the instantiation the walk
/// entered. `polymorphic` memoizes [`is_polymorphically_recursive`], which is a
/// property of a declaration and not of any one walk, so it is answered once
/// per declaration rather than once per member occurrence.
#[derive(Default)]
struct Walk {
    on_path: Vec<(String, Vec<ResolvedTy>)>,
    polymorphic: BTreeMap<String, bool>,
}

impl Walk {
    fn is_polymorphic(&mut self, name: &str, decls: &ClassContext<'_>) -> bool {
        if let Some(answer) = self.polymorphic.get(name) {
            return *answer;
        }
        let answer = is_polymorphically_recursive(name, decls);
        self.polymorphic.insert(name.to_string(), answer);
        answer
    }
}

/// Does `ty` mention one of `params`, the mentioning declaration's own type
/// parameters, anywhere inside it?
///
/// An argument that mentions none of them is a constant: substituting the
/// declaration's parameters cannot change it, so an edge carrying it reaches
/// one fixed instantiation however many times the cycle turns.
fn mentions_type_param(ty: &ResolvedTy, params: &[String]) -> bool {
    if is_own_parameter(ty, params) {
        return true;
    }
    match ty {
        ResolvedTy::Tuple(elements) => elements
            .iter()
            .any(|element| mentions_type_param(element, params)),
        ResolvedTy::Array(element, _) | ResolvedTy::Slice(element) => {
            mentions_type_param(element, params)
        }
        ResolvedTy::Task(inner) => mentions_type_param(inner, params),
        ResolvedTy::Named { args, .. } => args.iter().any(|arg| mentions_type_param(arg, params)),
        ResolvedTy::Closure {
            params: p,
            ret,
            captures,
        } => {
            p.iter().any(|t| mentions_type_param(t, params))
                || mentions_type_param(ret, params)
                || captures.iter().any(|t| mentions_type_param(t, params))
        }
        ResolvedTy::Function { params: p, ret } => {
            p.iter().any(|t| mentions_type_param(t, params)) || mentions_type_param(ret, params)
        }
        _ => false,
    }
}

/// Is `arg` one of `params`, the mentioning declaration's own type parameters?
///
/// A parameter reaches here spelled either as an abstract `TypeParam` or, when
/// the declaration was resolved without a type-parameter scope, as a
/// zero-argument user `Named`. [`substitute`] reads both spellings and so does
/// this.
fn is_own_parameter(arg: &ResolvedTy, params: &[String]) -> bool {
    let name = match arg {
        ResolvedTy::TypeParam { name } => name,
        ResolvedTy::Named {
            name,
            args,
            builtin: None,
            ..
        } if args.is_empty() => name,
        _ => return false,
    };
    params.iter().any(|param| param == name)
}

/// Collect every declaration `ty` mentions, with the arguments it mentions it
/// at, descending exactly where [`classify`] descends.
///
/// MARKED SHORTCUT — a declaration mention's own arguments are descended into
/// even though [`classify_declaration`] only reaches an argument through a
/// member that uses the parameter.
/// WHY: a declaration that passes a growing argument through an intermediary
/// (`L<T> { x: M<L<Vec<T>>> }`, `M<U> { u: U }`) has no edge at all without it,
/// and the member walk then diverges instead of refusing. Over-approximating
/// refuses a declaration whose growing argument lands on a phantom parameter no
/// member reads; under-approximating overflows the stack.
/// WHEN: `DeclaredType` records which parameters its members actually read.
/// WHAT: descend into a mention's arguments only at the positions the mentioned
/// declaration's members use.
fn collect_mentions(
    ty: &ResolvedTy,
    decls: &ClassContext<'_>,
    out: &mut Vec<(String, Vec<ResolvedTy>)>,
) {
    match ty {
        ResolvedTy::Tuple(elements) => {
            for element in elements {
                collect_mentions(element, decls, out);
            }
        }
        ResolvedTy::Array(element, _) => collect_mentions(element, decls, out),
        ResolvedTy::Closure { captures, .. } => {
            for capture in captures {
                collect_mentions(capture, decls, out);
            }
        }
        ResolvedTy::Named {
            name,
            args,
            builtin,
            ..
        } => {
            let builtin = builtin.or_else(|| {
                if decls.declaration(name).is_some() {
                    None
                } else {
                    crate::builtin_type::lookup_builtin_type(name)
                }
            });
            match builtin {
                // The two builtins whose class is the Aggregate rule over a
                // declaration, so they are declaration mentions like any other.
                Some(BuiltinType::CrashInfo | BuiltinType::CrashNotification) | None => {
                    if decls.declaration(name).is_some() {
                        out.push((name.clone(), args.clone()));
                    }
                    for arg in args {
                        collect_mentions(arg, decls, out);
                    }
                }
                // The builtins whose class is the join over their arguments.
                Some(
                    BuiltinType::Option
                    | BuiltinType::Result
                    | BuiltinType::Vec
                    | BuiltinType::HashMap
                    | BuiltinType::HashSet
                    | BuiltinType::VecIter
                    | BuiltinType::HashMapIter,
                ) => {
                    for arg in args {
                        collect_mentions(arg, decls, out);
                    }
                }
                // Every other builtin has a flat row: `classify` never looks at
                // its arguments, so neither does the reachability walk.
                Some(_) => {}
            }
        }
        // `Slice`, `Pointer`, `Borrow`, `Function`, `TraitObject` and `Task`
        // all have flat rows in §1.1: their payloads are never classified, so a
        // mention inside one is not an edge the member walk can take.
        _ => {}
    }
}

/// Does `name`'s declaration reach itself through its own members at an
/// instantiation other than its own parameters?
///
/// This is §1.1's polymorphic-recursion question, and it is asked of the
/// declaration's **pre-substitution** members. `L<T>` whose body mentions
/// `L<T>` reaches finitely many instantiations from any starting argument and
/// takes the owning edge, and so does `Tree<T>` whose body mentions a constant
/// `Tree<i64>`: substitution cannot change a constant, so that cycle reaches
/// one fixed instantiation. `L<T>` whose body mentions `L<Vec<T>>` — alone or
/// through an intermediary declaration — wraps the parameter and grows its
/// argument on every turn, so it has no finite fixpoint to join. A declaration
/// that never reaches itself refuses nothing, whatever arguments a caller
/// supplies: a nested `Wrapper<Wrapper<i64>>` is an ordinary aggregate.
fn is_polymorphically_recursive(name: &str, decls: &ClassContext<'_>) -> bool {
    let mut seen: BTreeSet<(String, bool)> = BTreeSet::new();
    let mut stack = vec![(name.to_string(), false)];
    while let Some((current, grew)) = stack.pop() {
        if !seen.insert((current.clone(), grew)) {
            continue;
        }
        let Some(declared) = decls.declaration(&current) else {
            continue;
        };
        let mut mentions = Vec::new();
        for member in &declared.members {
            collect_mentions(member, decls, &mut mentions);
        }
        for (mentioned, args) in mentions {
            // An argument grows the instantiation only when substitution can
            // make it bigger: it must mention one of the mentioning
            // declaration's parameters and must not be exactly one of them.
            // `Tree<T> { Node(Tree<i64>) }` carries a constant, so every turn
            // of that cycle reaches the same `Tree<i64>` and the walk is
            // finite; `L<T> { n: L<Vec<T>> }` wraps the parameter and grows.
            //
            // MARKED SHORTCUT - a permutation of parameters counts as no
            // growth, which is right, but a substitution that merely reorders
            // or duplicates them is admitted by the same test rather than
            // proved finite.
            // WHY: the exact question is whether the substitution composed
            // around the cycle is idempotent, which needs the composed map and
            // not one edge at a time.
            // WHEN: the walk carries the composed substitution rather than a
            // per-edge boolean.
            // WHAT: refuse exactly when the composition around the cycle is not
            // idempotent, which admits `Pair<T, U> { Pair<U, T> }` by proof
            // instead of by this argument-shape test.
            let grows = args.iter().any(|arg| {
                mentions_type_param(arg, &declared.type_params)
                    && !is_own_parameter(arg, &declared.type_params)
            });
            let grew = grew || grows;
            if grew && mentioned == name {
                return true;
            }
            stack.push((mentioned, grew));
        }
    }
    false
}

/// Class and clone of a heap collection over its element facts (§1.1
/// `Vec`/`HashMap`/`HashSet` row).
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

/// Class and clone of an ordinary aggregate over its member facts (§1.1
/// Aggregate rule): records, enums, tuples, arrays, `Option`/`Result`.
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

/// Substitute a declaration's own type parameters out of a member type.
fn substitute(ty: &ResolvedTy, params: &[String], args: &[ResolvedTy]) -> ResolvedTy {
    // A declaration's own parameter reaches here spelled either as an abstract
    // `TypeParam` or, when the declaration was resolved without a type-parameter
    // scope, as a zero-argument user `Named`. Both are the same parameter.
    let parameter_name = match ty {
        ResolvedTy::TypeParam { name } => Some(name),
        ResolvedTy::Named {
            name,
            args,
            builtin: None,
            ..
        } if args.is_empty() => Some(name),
        _ => None,
    };
    if let Some(index) =
        parameter_name.and_then(|name| params.iter().position(|param| param == name))
    {
        if let Some(argument) = args.get(index) {
            return argument.clone();
        }
    }
    match ty {
        ResolvedTy::Tuple(elements) => ResolvedTy::Tuple(
            elements
                .iter()
                .map(|element| substitute(element, params, args))
                .collect(),
        ),
        ResolvedTy::Array(element, len) => {
            ResolvedTy::Array(Box::new(substitute(element, params, args)), *len)
        }
        ResolvedTy::Slice(element) => {
            ResolvedTy::Slice(Box::new(substitute(element, params, args)))
        }
        ResolvedTy::Named {
            name,
            args: named_args,
            builtin,
            is_opaque,
        } => ResolvedTy::Named {
            name: name.clone(),
            args: named_args
                .iter()
                .map(|arg| substitute(arg, params, args))
                .collect(),
            builtin: *builtin,
            is_opaque: *is_opaque,
        },
        ResolvedTy::Task(inner) => ResolvedTy::Task(Box::new(substitute(inner, params, args))),
        other => other.clone(),
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "the §1.1 table is one closed match with a row per ResolvedTy arm and BuiltinType variant"
)]
fn classify(
    ty: &ResolvedTy,
    decls: &ClassContext<'_>,
    walk: &mut Walk,
) -> Result<(ValueClass, CloneKind), ClassError> {
    let bits = (ValueClass::BitCopy, CloneKind::Bits);
    let view = (ValueClass::View, CloneKind::Bits);
    let cow_retain = (ValueClass::CowValue, CloneKind::Retain);
    let affine_none = (ValueClass::AffineResource, CloneKind::None);
    let affine_retain = (ValueClass::AffineResource, CloneKind::Retain);
    let share_retain = (ValueClass::PersistentShare, CloneKind::Retain);
    let linear_none = (ValueClass::Linear, CloneKind::None);

    Ok(match ty {
        // integers, floats, Bool, Char, Unit, Never, Duration
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
        ResolvedTy::CancellationToken => affine_none,
        ResolvedTy::Slice(_) | ResolvedTy::Pointer { .. } | ResolvedTy::Borrow { .. } => view,
        ResolvedTy::Function { .. } | ResolvedTy::TraitObject { .. } => share_retain,
        // §1.1 Closure row: PersistentShare joined with the capture classes;
        // `clone` stays `Retain` in every case, because retaining a closure is
        // an env refcount bump that duplicates no capture.
        ResolvedTy::Closure { captures, .. } => {
            let capture_facts = classify_all(captures, decls, walk)?;
            let class = if capture_facts
                .iter()
                .any(|(class, _)| *class == ValueClass::Linear)
            {
                ValueClass::Linear
            } else if capture_facts
                .iter()
                .any(|(class, _)| *class == ValueClass::AffineResource)
            {
                ValueClass::AffineResource
            } else {
                ValueClass::PersistentShare
            };
            (class, CloneKind::Retain)
        }
        ResolvedTy::Tuple(elements) => aggregate_facts(&classify_all(elements, decls, walk)?),
        ResolvedTy::Array(element, _) => aggregate_facts(&[classify(element, decls, walk)?]),
        ResolvedTy::Task(_) => linear_none,
        ResolvedTy::TypeParam { name } => return Err(ClassError::TypeParam { name: name.clone() }),
        ResolvedTy::Named {
            name,
            args,
            builtin: Some(builtin),
            ..
        } => match builtin {
            // marker BitCopy today
            BuiltinType::SupervisorPool
            | BuiltinType::ChildRef
            | BuiltinType::NodeId
            | BuiltinType::Location
            | BuiltinType::RemotePid
            | BuiltinType::MonitorId
            | BuiltinType::DownTarget
            | BuiltinType::DownReason
            | BuiltinType::DownNotification
            // §1.1 decision: marker becomes BitCopy at P1 so `marker()` and
            // this table agree by construction.
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
            // §1.1 decision, overrides `marker() = Resource`: a pid never owns
            // the actor, so its drop frees nothing.
            | BuiltinType::LocalPid
            | BuiltinType::HewActor => bits,
            // Enums whose class is the join over their payload arguments.
            BuiltinType::Option | BuiltinType::Result => {
                aggregate_facts(&classify_all(args, decls, walk)?)
            }
            // Heap collections: aggregate over the element (key, value) classes
            // with a `CowValue` floor for the buffer.
            BuiltinType::Vec
            | BuiltinType::HashMap
            | BuiltinType::HashSet
            // A `VecIter`/`HashMapIter` is a collection field plus BitCopy
            // cursor fields, so it takes the collection's own facts.
            | BuiltinType::VecIter
            | BuiltinType::HashMapIter => {
                collection_facts(&classify_all(args, decls, walk)?)
            }
            // Aggregate rule over the std declaration's fields.
            BuiltinType::CrashInfo
            | BuiltinType::CrashNotification
            | BuiltinType::NodeConfig => {
                classify_declaration(name, args, decls, walk)?
            }
            BuiltinType::Rc | BuiltinType::Weak | BuiltinType::LambdaPid => affine_retain,
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
            | BuiltinType::CancellationToken => affine_none,
            BuiltinType::Task => linear_none,
            // Never the type of a value: `Iterator` is the std trait name, and
            // `ActorState`/`MachineState` are compiler-internal payload carriers.
            BuiltinType::Iterator | BuiltinType::ActorState | BuiltinType::MachineState => {
                return Err(ClassError::NotAValueType { builtin: *builtin })
            }
        },
        ResolvedTy::Named {
            name,
            args,
            builtin: None,
            is_opaque,
        } => {
            // `builtin` is the identity fact. A `Named` that carries none and
            // that the context holds no declaration for is refused in every
            // context, the empty one included: reading the name against the
            // builtin table here would be a second identity authority.
            let Some(declared) = decls.declaration(name) else {
                return Err(ClassError::UnknownDeclaration { name: name.clone() });
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
                DeclarationMarker::None if *is_opaque || declared.is_opaque => bits,
                DeclarationMarker::None => classify_declaration(name, args, decls, walk)?,
            }
        }
    })
}

/// Aggregate rule over one declaration's substituted member types.
fn classify_declaration(
    name: &str,
    args: &[ResolvedTy],
    decls: &ClassContext<'_>,
    walk: &mut Walk,
) -> Result<(ValueClass, CloneKind), ClassError> {
    let declared = decls
        .declaration(name)
        .ok_or_else(|| ClassError::UnknownDeclaration {
            name: name.to_string(),
        })?;
    // §1.1's indirect-enum row: a recursive occurrence is an **owning edge**,
    // not a base case. The recursion is legal only because the payload sits
    // behind a heap box, so the occurrence contributes the same heap floor a
    // collection's buffer does — `CowValue` with a `FieldWise` clone. The
    // aggregate join then keeps the declaration's payload class: a `BitCopy`
    // payload gives `(CowValue, FieldWise)`, a payload holding an
    // `AffineResource` gives `AffineResource`. It is never `BitCopy`, which is
    // what a bottom-element cut published and which §1.2 maps to no ownership
    // obligation, §1.3 lets `copy_value` duplicate and §2.1 bit-copies across
    // an actor heap.
    //
    // Whether the cycle has a finite fixpoint at all is a property of the
    // declaration's own pre-substitution members, not of the walk stack: a body
    // that mentions itself at its own instantiation reaches finitely many
    // instantiations from any argument, and a body that mentions itself at a
    // different one grows its argument on every turn. The second case refuses,
    // at every instantiation, rather than under-approximating the growing type
    // as the box's own `CowValue`. A declaration whose members never reach it
    // refuses nothing, so a nested `Wrapper<Wrapper<i64>>` is an ordinary
    // aggregate.
    //
    // MARKED SHORTCUT — the owning edge is the collection heap floor rather
    // than the box's own retain path.
    // WHY: `DeclaredType` carries the members, not the box, so the walk has no
    // term for the allocation itself and borrows `collection_facts`' floor.
    // The floor is sound for §1.2 (the class is `Owned` either way) and only
    // approximates the clone column for a box whose payload is shareable.
    // WHEN: `DeclaredType` carries `is_indirect` (`HirTypeDecl.is_indirect`)
    // and the box's own class row lands (P2, §5.3).
    // WHAT: the recursive occurrence reads the box's row instead of the
    // collection floor.
    if walk.is_polymorphic(name, decls) {
        return Err(ClassError::RecursiveInstantiation {
            name: name.to_string(),
        });
    }
    let instance = (name.to_string(), args.to_vec());
    if walk.on_path.contains(&instance) {
        return Ok((ValueClass::CowValue, CloneKind::FieldWise));
    }
    walk.on_path.push(instance);
    let members: Vec<ResolvedTy> = declared
        .members
        .iter()
        .map(|member| substitute(member, &declared.type_params, args))
        .collect();
    let facts = classify_all(&members, decls, walk);
    walk.on_path.pop();
    Ok(aggregate_facts(&facts?))
}
