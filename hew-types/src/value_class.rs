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

use std::collections::BTreeMap;

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
    /// An `#[opaque]` handle declaration with no ownership marker: the
    /// Aggregate rule cannot see through it and there is no default.
    OpaqueWithoutMarker { name: String },
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
            Self::OpaqueWithoutMarker { name } => write!(
                f,
                "opaque handle `{name}` carries no ownership marker and has no visible fields"
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
    let mut in_progress = Vec::new();
    classify(ty, decls, &mut in_progress)
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
    in_progress: &mut Vec<String>,
) -> Result<Vec<(ValueClass, CloneKind)>, ClassError> {
    tys.iter()
        .map(|ty| classify(ty, decls, in_progress))
        .collect()
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
    in_progress: &mut Vec<String>,
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
            let capture_facts = classify_all(captures, decls, in_progress)?;
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
        ResolvedTy::Tuple(elements) => {
            aggregate_facts(&classify_all(elements, decls, in_progress)?)
        }
        ResolvedTy::Array(element, _) => aggregate_facts(&[classify(element, decls, in_progress)?]),
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
                aggregate_facts(&classify_all(args, decls, in_progress)?)
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
                collection_facts(&classify_all(args, decls, in_progress)?)
            }
            // Aggregate rule over the std declaration's fields.
            BuiltinType::CrashInfo | BuiltinType::CrashNotification => {
                classify_declaration(name, args, decls, in_progress)?
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
            let declared = decls
                .declaration(name)
                .ok_or_else(|| ClassError::UnknownDeclaration { name: name.clone() })?;
            match declared.marker {
                DeclarationMarker::Resource => affine_none,
                DeclarationMarker::Linear => linear_none,
                DeclarationMarker::None if *is_opaque => {
                    return Err(ClassError::OpaqueWithoutMarker { name: name.clone() })
                }
                DeclarationMarker::None => classify_declaration(name, args, decls, in_progress)?,
            }
        }
    })
}

/// Aggregate rule over one declaration's substituted member types.
fn classify_declaration(
    name: &str,
    args: &[ResolvedTy],
    decls: &ClassContext<'_>,
    in_progress: &mut Vec<String>,
) -> Result<(ValueClass, CloneKind), ClassError> {
    let declared = decls
        .declaration(name)
        .ok_or_else(|| ClassError::UnknownDeclaration {
            name: name.to_string(),
        })?;
    // MARKED SHORTCUT — a recursive occurrence contributes the join's bottom.
    // WHY: an `indirect` enum (`type List { Cons(i64, List) }`) is a legal
    // declaration whose member walk does not terminate; the join is monotone,
    // so the bottom element is its fixpoint and no legal type is refused.
    // WHEN: §1.1's indirect-enum row lands with its own boxed-payload clone
    // kind (P2).
    // WHAT: the walk carries a per-declaration class variable and iterates the
    // join to a fixpoint instead of cutting the cycle at the bottom.
    if in_progress.iter().any(|entry| entry == name) {
        return Ok((ValueClass::BitCopy, CloneKind::Bits));
    }
    in_progress.push(name.to_string());
    let members: Vec<ResolvedTy> = declared
        .members
        .iter()
        .map(|member| substitute(member, &declared.type_params, args))
        .collect();
    let facts = classify_all(&members, decls, in_progress);
    in_progress.pop();
    Ok(aggregate_facts(&facts?))
}
