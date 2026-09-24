//! Value-kind and argument/result contract types for runtime calls.

use super::{FileReadHandleKind, IoHandleKind};
use crate::{BuiltinType, ResolvedTy};
use serde::{Deserialize, Serialize};
use strum::EnumIter;

// =============================================================================
// Ownership verdict
// =============================================================================

/// Three-valued consume/borrow verdict for one call argument.
///
/// Refines the historical `bool` consume flag (`true` = consume, `false` =
/// borrow) by splitting the consume case on WHY the value escapes:
///
/// * [`ProvenBorrow`](Self::ProvenBorrow) — the argument never escapes the
///   callee (returned/stored/sent/captured nowhere, forwarded to no consuming
///   sink). The caller keeps ownership and drops it at its own scope exit. This
///   is exactly the old `false`.
/// * [`ProvenConsume`](Self::ProvenConsume) — a POSITIVE proof of escape: the
///   argument is returned, stored (`let`/assign/struct/tuple), sent, or
///   captured. The callee owns it.
/// * [`ConservativeConsume`](Self::ConservativeConsume) — flipped to consume
///   ONLY because it was forwarded to an unproven or consuming parameter (or a
///   synthesized runtime edge whose per-arg contract is not individually
///   proven). No positive escape proof exists; the callee is ASSUMED to consume
///   it. This is the fail-closed half of the old `true`.
///
/// Safety invariant: [`ProvenConsume`](Self::ProvenConsume) and
/// [`ConservativeConsume`](Self::ConservativeConsume) are byte-identical to the
/// old `true` at every consumer — both mean "callee owns / drops, caller must
/// not" (`boundary-fail-closed`, no double-free). Only the label is finer. The
/// precision this unlocks lives entirely on [`ProvenBorrow`](Self::ProvenBorrow),
/// which is unchanged from the old `false`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConsumeVerdict {
    /// Never escapes — caller keeps and drops (old `false`).
    ProvenBorrow,
    /// Positively proven escape — callee owns (proven half of old `true`).
    ProvenConsume,
    /// Assumed-consume fail-closed default — callee owns (fail-closed half of
    /// old `true`).
    ConservativeConsume,
}

/// Ownership of a runtime call's returned value when it carries a scalar
/// result whose representation alone cannot express the acquisition fact.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeResultOwnership {
    /// No owned-result admission is required for this family.
    Untracked,
    /// The ABI returns a freshly allocated string that the caller owns.
    FreshOwnedString,
    /// The ABI returns a fresh vector handle that the caller owns.
    FreshOwnedVec,
    /// The ABI returns a fresh bytes allocation that the caller owns.
    FreshOwnedBytes,
}

/// Receiver-relative authority of a runtime result. This is deliberately
/// orthogonal to the concrete retain/allocation ritual above: consumers that
/// decide whether a result can outlive or be released independently from its
/// receiver must use this closed classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeResultAuthority {
    /// The call returns or moves out one independently-owned value.
    IndependentOwned,
    /// Independent semantic copy; concrete type facts decide owning versus bit-copy.
    IndependentValue,
    /// The result points into storage owned by argument zero.
    InteriorAliasOfReceiver,
    /// The returned bits contain no receiver-owned storage.
    IndependentBitCopy,
    /// No positive authority is published; ownership-sensitive uses reject.
    FailClosed,
}

/// Target-independent value kinds used by the first executable ownership-SIR
/// runtime surface. These are semantic types, not ABI storage classes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeValueKind {
    /// A file-read owner, with exact source or builtin identity supplied by the signature.
    FileReadHandle(FileReadHandleKind),
    IoHandle(IoHandleKind),
    /// One generic pipe half, resolved to the receiver's checked type.
    PipeHalf(PipeHalfKind),
    /// A freshly extracted pipe half named by the operation, not by the
    /// receiver: `hew_stream_pair_sink` borrows a pair and returns a half.
    PipeHalfResult(PipeHalfKind),
    /// The paired pipe allocation `stream.pipe` splits. Its nominal identity
    /// comes from the generated `hew_stream_channel` ownership row.
    StreamPair,
    ActorRequestOwner,
    ActorRequestAdmission,
    /// A concrete actor identity, preserved through semantic lowering.
    ActorHandle,
    /// An immutable raw pointer, including compiler-generated ingress adapters.
    ConstBytePointer,
    Unit,
    Bool,
    I8,
    I16,
    U8,
    U16,
    U32,
    I32,
    I64,
    U64,
    Isize,
    Usize,
    F64,
    /// A Unicode scalar. The C ABI carries it as `i32`.
    Char,
    String,
    Bytes,
    /// A nanosecond span. Distinct from `I64` at the semantic boundary; the C
    /// ABI carries it as `i64`. `instant` has no kind of its own - it resolves
    /// to `I64` before SIR sees it (`ResolvedTy::from_ty`).
    Duration,
    /// A concrete source-owned prelude nominal with no type parameters.
    Named(&'static str),
    /// A concrete `#[opaque]` runtime-handle nominal with no type parameters.
    NamedOpaque(&'static str),
    /// A monomorphic enum whose source declaration supplies its exact owner.
    MonomorphicBuiltin(BuiltinType),
    /// The signature's receiver, constrained by canonical builtin identity.
    Receiver(BuiltinType),
    /// The exact fixed-size array receiver, including its length.
    FixedArray,
    /// The element of that exact fixed-size array receiver.
    ArrayElement,
    /// The exact `SupervisorPool<S, T>` receiver.
    PoolView,
    /// One member's role, `ChildRef<T>`, from that pool receiver.
    PoolMember,
    /// One type argument from the signature's canonical collection receiver.
    TypeArgument(usize),
    /// The payload of the signature's `Rc<T>`/`Weak<T>` receiver. A shared
    /// handle is not a collection, so its argument does not resolve through
    /// [`Self::TypeArgument`].
    SharedPayload,
    /// The exact checked result type for a compiler-owned operation whose
    /// generic payload is not carried by an argument (Node.lookup).
    NodeLookupResult,
    /// A builtin nominal with no type parameters, named by its own identity
    /// rather than by a source spelling (`NodeId`).
    BuiltinNominal(BuiltinType),
    /// The argument's own checked type, required to carry this builtin
    /// identity. A declared accessor names its value as an ordinary parameter
    /// rather than a method receiver (`hew_remote_pid_slot(pid)`), so identity
    /// is checked on the argument itself. One authority for that identity: the
    /// type's own `builtin` discriminator, never the receiver table.
    BuiltinArgument(BuiltinType),
    /// Ordinary type construction, shared by optional results and projections.
    Applied(BuiltinType, &'static [Self]),
    /// Ordinary product results, including receiver replacement with a value.
    Tuple(&'static [Self]),
    /// The rendered operand of `f"{v:?}"`. Structural rendering is defined for
    /// every type the checker admits under `:?`, so this kind matches whatever
    /// concrete type the call site carries; the rendering recipe comes from
    /// that type's physical layout, not from this kind.
    StructuralOperand,
}

impl RuntimeValueKind {
    #[must_use]
    pub const fn matches(self, ty: &ResolvedTy) -> bool {
        matches!(
            (self, ty),
            (Self::StructuralOperand, _)
                | (Self::Unit, ResolvedTy::Unit)
                | (Self::Bool, ResolvedTy::Bool)
                | (Self::I8, ResolvedTy::I8)
                | (Self::I16, ResolvedTy::I16)
                | (Self::U8, ResolvedTy::U8)
                | (Self::U16, ResolvedTy::U16)
                | (Self::U32, ResolvedTy::U32)
                | (Self::I32, ResolvedTy::I32)
                | (Self::I64, ResolvedTy::I64)
                | (Self::U64, ResolvedTy::U64)
                | (Self::Isize, ResolvedTy::Isize)
                | (Self::Usize, ResolvedTy::Usize)
                | (Self::F64, ResolvedTy::F64)
                | (Self::Char, ResolvedTy::Char)
                | (Self::String, ResolvedTy::String)
                | (Self::Bytes, ResolvedTy::Bytes)
                | (Self::Duration, ResolvedTy::Duration)
        )
    }

    /// Resolve a type expression using the signature's one receiver binding.
    #[must_use]
    #[expect(
        clippy::too_many_lines,
        reason = "one arm per value kind; splitting the dispatch hides the binding rules"
    )]
    pub fn resolve(self, receiver: Option<&ResolvedTy>) -> Option<ResolvedTy> {
        Some(match self {
            Self::IoHandle(kind) => {
                let receiver = receiver?;
                if !kind.matches(receiver) {
                    return None;
                }
                receiver.clone()
            }
            Self::FileReadHandle(kind) => {
                let receiver = receiver?;
                if !kind.matches(receiver) {
                    return None;
                }
                receiver.clone()
            }
            Self::PipeHalf(kind) => {
                let receiver = receiver?;
                if !kind.matches(receiver) {
                    return None;
                }
                receiver.clone()
            }
            Self::PipeHalfResult(_) | Self::NodeLookupResult => return None,
            Self::StreamPair => stream_pair_ty()?,
            Self::ActorRequestOwner => actor_request_owner_ty(),
            Self::ActorHandle => {
                let actor = receiver?;
                actor.actor_handle_instance()?;
                actor.clone()
            }
            Self::ActorRequestAdmission => {
                ResolvedTy::named_opaque("std.builtins.ActorRequestAdmission", Vec::new())
            }
            Self::StructuralOperand => receiver?.clone(),
            Self::Named(name) => ResolvedTy::named_user(name, Vec::new()),
            Self::NamedOpaque(name) => ResolvedTy::named_opaque(name, Vec::new()),
            Self::BuiltinNominal(builtin) => {
                ResolvedTy::named_builtin(builtin.canonical_name(), builtin, Vec::new())
            }
            Self::MonomorphicBuiltin(builtin) => {
                let fact =
                    crate::builtin_enums::monomorphic_builtin_enum(builtin.canonical_name())?;
                ResolvedTy::named_builtin(fact.canonical_name, builtin, Vec::new())
            }
            Self::Receiver(expected) => {
                let receiver = receiver?;
                let actual = runtime_receiver_builtin(receiver)?;
                if actual != expected {
                    return None;
                }
                receiver.clone()
            }
            Self::BuiltinArgument(expected) => {
                let argument = receiver?;
                if !carries_builtin_identity(argument, expected) {
                    return None;
                }
                argument.clone()
            }
            Self::FixedArray => {
                let array @ ResolvedTy::Array(_, _) = receiver? else {
                    return None;
                };
                array.clone()
            }
            Self::ArrayElement => {
                let ResolvedTy::Array(element, _) = receiver? else {
                    return None;
                };
                (**element).clone()
            }
            Self::TypeArgument(index) => {
                collection_type_arguments(receiver?)?.1.get(index)?.clone()
            }
            Self::SharedPayload => shared_handle_payload(receiver?)?.clone(),
            Self::PoolView => {
                let view = receiver?;
                supervisor_pool_member_type(view)?;
                view.clone()
            }
            Self::PoolMember => ResolvedTy::named_builtin(
                BuiltinType::ChildRef.canonical_name(),
                BuiltinType::ChildRef,
                vec![supervisor_pool_member_type(receiver?)?.clone()],
            ),
            Self::Applied(builtin, arguments) => ResolvedTy::named_builtin(
                builtin.canonical_name(),
                builtin,
                arguments
                    .iter()
                    .map(|ty| ty.resolve(receiver))
                    .collect::<Option<Vec<_>>>()?,
            ),
            Self::Tuple(fields) => ResolvedTy::Tuple(
                fields
                    .iter()
                    .map(|ty| ty.resolve(receiver))
                    .collect::<Option<Vec<_>>>()?,
            ),
            // The remaining kinds have fixed types independent of the receiver.
            kind => resolve_fixed_type(kind),
        })
    }
}

/// Resolve primitive value kinds whose types do not depend on a receiver.
fn resolve_fixed_type(kind: RuntimeValueKind) -> ResolvedTy {
    match kind {
        RuntimeValueKind::ConstBytePointer => ResolvedTy::Pointer {
            is_mutable: false,
            pointee: Box::new(ResolvedTy::U8),
        },
        RuntimeValueKind::Unit => ResolvedTy::Unit,
        RuntimeValueKind::Bool => ResolvedTy::Bool,
        RuntimeValueKind::F64 => ResolvedTy::F64,
        RuntimeValueKind::Char => ResolvedTy::Char,
        RuntimeValueKind::String => ResolvedTy::String,
        RuntimeValueKind::Bytes => ResolvedTy::Bytes,
        RuntimeValueKind::Duration => ResolvedTy::Duration,
        RuntimeValueKind::I8 => ResolvedTy::I8,
        RuntimeValueKind::I16 => ResolvedTy::I16,
        RuntimeValueKind::U8 => ResolvedTy::U8,
        RuntimeValueKind::U16 => ResolvedTy::U16,
        RuntimeValueKind::U32 => ResolvedTy::U32,
        RuntimeValueKind::I32 => ResolvedTy::I32,
        RuntimeValueKind::I64 => ResolvedTy::I64,
        RuntimeValueKind::U64 => ResolvedTy::U64,
        RuntimeValueKind::Isize => ResolvedTy::Isize,
        RuntimeValueKind::Usize => ResolvedTy::Usize,
        _ => unreachable!("resolve_fixed_type requires a primitive value kind"),
    }
}

/// Ownership action required for one semantic runtime operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeArgumentEffect {
    Borrow,
    Copy,
    Move,
    /// Adopt the operand: the operation owns it once the call returns normally.
    Value,
}

impl RuntimeArgumentEffect {
    /// Resolve one operand's ownership action from the checker-authored class.
    ///
    /// Value ingress means the operation adopts its operand, so an owned
    /// operand transfers. Lowering reads a copyable binding as an independent
    /// owner first, which leaves the caller's own value intact and hands the
    /// operation the copy it would otherwise clone a second time and destroy
    /// on the call's normal edge. A bit-copied operand owns nothing to
    /// transfer and is read in place.
    #[must_use]
    pub const fn resolve_operand(self, class: crate::ValueClass) -> Self {
        match self {
            Self::Value => match class {
                crate::ValueClass::BitCopy | crate::ValueClass::View => Self::Borrow,
                crate::ValueClass::CowValue
                | crate::ValueClass::PersistentShare
                | crate::ValueClass::AffineResource
                | crate::ValueClass::Linear => Self::Move,
            },
            effect => effect,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RuntimeArgumentContract {
    pub ty: RuntimeValueKind,
    pub effect: RuntimeArgumentEffect,
}

/// Closed semantic identities for runtime results whose value is a concrete
/// builtin enum instance. The corresponding [`hew_sir::SemVariantShape`]
/// remains the authority for tag order and payload layout; this discriminator
/// only constrains the exact language type produced by the runtime operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeVariantResultKind {
    /// A validating byte decode returns ordinary data:
    /// `Result<string, std.encoding.utf8.Utf8Error>`.
    Utf8Decode,
}

impl RuntimeVariantResultKind {
    /// Return the exact Ok/Err payload types when `ty` is this contract's
    /// canonical result. Same-leaf user errors and user-defined `Result`
    /// records are rejected.
    #[must_use]
    pub fn payload_types(self, ty: &ResolvedTy) -> Option<(&ResolvedTy, &ResolvedTy)> {
        match (self, ty) {
            (
                Self::Utf8Decode,
                ResolvedTy::Named {
                    args,
                    builtin: Some(crate::BuiltinType::Result),
                    ..
                },
            ) if args.len() == 2
                && args[0] == ResolvedTy::String
                && args[1].nominal_instance().is_some_and(|instance| {
                    instance.args.is_empty()
                        && instance.nominal.declaration().full_path()
                            == "std.encoding.utf8.Utf8Error"
                }) =>
            {
                Some((&args[0], &args[1]))
            }
            _ => None,
        }
    }

    #[must_use]
    pub fn matches(self, ty: &ResolvedTy) -> bool {
        self.payload_types(ty).is_some()
    }
}

/// Semantic result relation of one runtime operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeResultEffect {
    Unit,
    /// The operation never returns; the call has no normal continuation.
    Never,
    BitCopy(RuntimeValueKind),
    FreshOwned(RuntimeValueKind),
    /// An independent language value. Owning elements are semantically copied;
    /// scalar elements are bit-copied, as decided by the concrete type facts.
    /// Runtime interior borrows never escape through this result.
    IndependentValue(RuntimeValueKind),
    /// A loan of a value the receiver still owns. The result carries no
    /// obligation of its own and must not outlive the receiver's loan, so it
    /// needs no clone recipe: a clone-free element is readable this way and
    /// no other.
    Borrowed(RuntimeValueKind),
    /// A transform yields `(receiver, value)`. SIR destructures the sole
    /// result and writes the receiver back to its source binding.
    UpdatedReceiverAndValue(RuntimeValueKind),
    /// A runtime operation constructs one exact owned enum value on its normal
    /// edge. Logical alternatives such as `Ok`/`Err` are payload data, not SIR
    /// fault edges.
    FreshOwnedVariant(RuntimeVariantResultKind),
    /// A successful transform consumes argument zero and yields its sole
    /// updated owner as a new SSA value. The physical ABI may use an out
    /// pointer or return the same owner after a C-void in-place mutation, but
    /// may not hide the semantic owner replacement from SIR.
    UpdatedReceiver(RuntimeValueKind),
}

/// Closed language-visible failures of the initial runtime-operation surface.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeLogicalFailure {
    IndexOutOfBounds,
    IntegerOverflow,
    /// A selected user callback returns an existing logical fault owner.
    CallbackFault,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RuntimeSemanticContract {
    pub arguments: &'static [RuntimeArgumentContract],
    pub result: RuntimeResultEffect,
    pub failures: &'static [RuntimeLogicalFailure],
}

impl RuntimeSemanticContract {
    /// A bounds failure happens before a receiver transform mutates or takes
    /// any input. Its cleanup edge retains those owners for ordinary cleanup.
    #[must_use]
    pub fn preserves_inputs_on_failure(self) -> bool {
        self.failures == [RuntimeLogicalFailure::IndexOutOfBounds]
            && matches!(
                self.result,
                RuntimeResultEffect::UpdatedReceiver(_)
                    | RuntimeResultEffect::UpdatedReceiverAndValue(_)
            )
    }

    /// All failures on this call's cleanup edge carry an active fault owner.
    /// Static failures sharing a callback edge are materialized by the runtime
    /// boundary before cleanup, preserving a single fault transfer path.
    #[must_use]
    pub fn propagates_fault(self) -> bool {
        self.failures
            .contains(&RuntimeLogicalFailure::CallbackFault)
    }

    /// Check a concrete language signature against this runtime operation.
    ///
    /// The checker calls this only after proving the declaration is canonical
    /// compiler-owned source.  Keeping the type comparison here makes the
    /// runtime contract the single authority for both call admission and SIR
    /// verification, including exact nominal variant results.
    #[must_use]
    pub fn matches_signature(self, params: &[ResolvedTy], result: &ResolvedTy) -> bool {
        self.instantiate(params, result)
            .is_ok_and(|resolved| resolved.result_ty == *result)
    }

    /// Bind the receiver, element and result through one checked relationship.
    /// `result_hint` supplies a constructor's receiver binding, including an
    /// encoding constructor with scalar arguments. It never overrides an
    /// existing receiver's identity or collection element type.
    ///
    /// # Errors
    /// Rejects wrong arity, noncanonical receivers and mismatched type arguments.
    pub fn instantiate(
        self,
        params: &[ResolvedTy],
        result_hint: &ResolvedTy,
    ) -> Result<RuntimeInstantiatedContract, String> {
        let resolved = self.resolve_types(params, result_hint)?;
        for (index, (actual, expected)) in params.iter().zip(&resolved.arguments).enumerate() {
            if actual != expected {
                return Err(format!(
                    "runtime argument {index} has `{}`, expected `{}`",
                    actual.user_facing(),
                    expected.user_facing()
                ));
            }
        }
        Ok(resolved)
    }

    /// Resolve the operation's expected types before lowering argument coercions.
    /// This does not admit operands: `instantiate` checks their exact types after
    /// the producer has applied any checked source-language conversion.
    ///
    /// # Errors
    /// Rejects wrong arity or a missing canonical receiver/result binding.
    pub fn resolve_types(
        self,
        params: &[ResolvedTy],
        result_hint: &ResolvedTy,
    ) -> Result<RuntimeInstantiatedContract, String> {
        if params.len() != self.arguments.len() {
            return Err(format!(
                "runtime signature has {} arguments, expected {}",
                params.len(),
                self.arguments.len()
            ));
        }
        // `Rc.new(v)`'s only argument is the payload, and a payload can itself
        // be a canonical receiver (`Rc.new(vec)`). A shared constructor's
        // identity therefore comes from its result before any argument is
        // scanned; every other operation names its receiver in an argument or,
        // failing that, in its result.
        let shared_constructor =
            (runtime_receiver_builtin(result_hint) == Some(BuiltinType::Rc)).then_some(result_hint);
        let receiver = shared_constructor
            .or_else(|| {
                params.iter().find(|ty| {
                    runtime_receiver_builtin(ty).is_some()
                        || matches!(ty, ResolvedTy::Array(_, _))
                        || FileReadHandleKind::of_ty(ty).is_some()
                        || IoHandleKind::of_ty(ty).is_some()
                        || PipeHalfKind::of_ty(ty).is_some()
                })
            })
            .or_else(|| {
                (params.is_empty()
                    || runtime_receiver_builtin(result_hint)
                        .is_some_and(BuiltinType::is_encoding_value)
                    || FileReadHandleKind::of_ty(result_hint).is_some())
                .then_some(result_hint)
                .filter(|ty| {
                    runtime_receiver_builtin(ty).is_some()
                        || FileReadHandleKind::of_ty(ty).is_some()
                })
            });
        let arguments = self
            .arguments
            .iter()
            .zip(params)
            .map(|(expected, actual)| {
                // An actor handle and a pipe half bind to their own argument:
                // `stream.forward(from, to)` names both halves in one contract.
                // A rendered operand binds to itself the same way: `:?` has no
                // canonical receiver, only the value the call site carries.
                let binding = if expected.ty == RuntimeValueKind::ActorHandle
                    || matches!(
                        expected.ty,
                        RuntimeValueKind::PipeHalf(_)
                            | RuntimeValueKind::BuiltinArgument(_)
                            | RuntimeValueKind::StructuralOperand
                    ) {
                    Some(actual)
                } else {
                    receiver
                };
                expected.ty.resolve(binding).ok_or_else(|| {
                    "runtime signature has no matching canonical receiver binding".to_string()
                })
            })
            .collect::<Result<Vec<_>, String>>()?;
        let result_ty = match self.result {
            RuntimeResultEffect::Unit => ResolvedTy::Unit,
            RuntimeResultEffect::Never => ResolvedTy::Never,
            RuntimeResultEffect::BitCopy(kind)
            | RuntimeResultEffect::FreshOwned(kind)
            | RuntimeResultEffect::IndependentValue(kind)
            | RuntimeResultEffect::Borrowed(kind)
            | RuntimeResultEffect::UpdatedReceiver(kind)
            | RuntimeResultEffect::UpdatedReceiverAndValue(kind) => {
                let resolved = if (matches!(kind, RuntimeValueKind::NodeLookupResult)
                    && is_node_lookup_result(result_hint))
                    || matches!(kind, RuntimeValueKind::IoHandle(handle) if handle.matches(result_hint))
                    || matches!(kind, RuntimeValueKind::PipeHalfResult(half) if half.matches(result_hint))
                {
                    Some(result_hint.clone())
                } else {
                    kind.resolve(receiver)
                };
                resolved.ok_or_else(|| {
                    "runtime result has no matching canonical receiver binding".to_string()
                })?
            }
            RuntimeResultEffect::FreshOwnedVariant(kind) if kind.matches(result_hint) => {
                result_hint.clone()
            }
            RuntimeResultEffect::FreshOwnedVariant(_) => {
                return Err("exact runtime variant result does not admit this type".to_string());
            }
        };
        Ok(RuntimeInstantiatedContract {
            arguments,
            result_ty,
        })
    }
}

/// Concrete types checked by the semantic operation's parametric authority.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeInstantiatedContract {
    pub arguments: Vec<ResolvedTy>,
    pub result_ty: ResolvedTy,
}

/// Bind the existing canonical receiver itself, preserving representation facts
/// such as opacity. Encoding values have no collection type arguments.
/// Whether `ty` is the named builtin `expected`, by its own identity.
///
/// A checked type reaches here spelled either with its `builtin` discriminator
/// filled in or as the module-qualified nominal the resolver minted
/// (`std.builtins.NodeId`), depending on which producer built it. Both are the
/// same type, so this reads the nominal and accepts either spelling rather
/// than routing identity through the receiver table, which knows only
/// collections and handles.
fn carries_builtin_identity(ty: &ResolvedTy, expected: BuiltinType) -> bool {
    let ResolvedTy::Named { name, builtin, .. } = ty else {
        return false;
    };
    if *builtin == Some(expected) {
        return true;
    }
    let leaf = name.rsplit('.').next().unwrap_or(name.as_str());
    leaf == expected.canonical_name()
}

fn runtime_receiver_builtin(ty: &ResolvedTy) -> Option<BuiltinType> {
    if let Some((builtin, _)) = collection_type_arguments(ty) {
        return Some(builtin);
    }
    if supervisor_pool_member_type(ty).is_some() {
        return Some(BuiltinType::SupervisorPool);
    }
    // An actor is the type of its handle, so the handle's arguments are the
    // actor declaration's own and may be empty.
    if ty.actor_handle_instance().is_some() {
        return Some(BuiltinType::ActorHandle);
    }
    match ty {
        ResolvedTy::Named {
            builtin:
                Some(
                    kind @ (BuiltinType::Stream
                    | BuiltinType::Sink
                    | BuiltinType::Rc
                    | BuiltinType::Weak),
                ),
            args,
            ..
        } if args.len() == 1 => Some(*kind),
        ResolvedTy::Named {
            name,
            builtin: Some(builtin),
            args,
            ..
        } if builtin.is_encoding_value() && name == builtin.canonical_name() && args.is_empty() => {
            Some(*builtin)
        }
        _ => None,
    }
}

/// The payload of a canonical `Rc<T>`/`Weak<T>` handle, and the one place a
/// shared handle is recognised from its type.
#[must_use]
pub fn shared_handle_payload(ty: &ResolvedTy) -> Option<&ResolvedTy> {
    match ty {
        ResolvedTy::Named {
            builtin: Some(BuiltinType::Rc | BuiltinType::Weak),
            args,
            ..
        } if args.len() == 1 => args.first(),
        _ => None,
    }
}

fn is_node_lookup_result(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Named { builtin: Some(BuiltinType::Result), args, .. }
        if matches!(args.as_slice(), [ResolvedTy::Named { builtin: Some(BuiltinType::RemotePid), args: remote_args, .. }, error]
            if remote_args.len() == 1
                && error.is_builtin(BuiltinType::LookupError)))
}

/// Which half of a pipe a runtime contract's receiver is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PipeHalfKind {
    Sink,
    Stream,
}

impl PipeHalfKind {
    /// Match a checked generic endpoint with exactly one element type.
    #[must_use]
    pub fn matches(self, ty: &ResolvedTy) -> bool {
        let expected = match self {
            Self::Sink => BuiltinType::Sink,
            Self::Stream => BuiltinType::Stream,
        };
        matches!(ty, ResolvedTy::Named { builtin: Some(builtin), args, .. }
            if *builtin == expected && args.len() == 1)
    }

    /// The pipe half one checked type is, if it is one.
    #[must_use]
    pub fn of_ty(ty: &ResolvedTy) -> Option<Self> {
        [Self::Sink, Self::Stream]
            .into_iter()
            .find(|kind| kind.matches(ty))
    }
}

/// The nominal the generated `hew_stream_channel` row names as its owned
/// result. Reading the row rather than the spelling keeps one ownership
/// authority.
#[must_use]
pub fn stream_pair_ty() -> Option<ResolvedTy> {
    let contract = crate::ffi_contracts::extern_owned_resource_result("hew_stream_channel")?;
    Some(ResolvedTy::named_opaque(contract.resource_type, Vec::new()))
}

/// The sealed completion request's source-declared runtime owner.
#[must_use]
pub fn actor_request_owner_ty() -> ResolvedTy {
    ResolvedTy::named_opaque("std.builtins.ActorRequestOwner", Vec::new())
}

/// Whether one checked type is the paired pipe allocation.
#[must_use]
pub fn is_stream_pair_ty(ty: &ResolvedTy) -> bool {
    stream_pair_ty().is_some_and(|pair| pair == *ty)
}

/// Recognize supported canonical collection instances and their exact arity.
#[must_use]
pub fn collection_type_arguments(ty: &ResolvedTy) -> Option<(BuiltinType, &[ResolvedTy])> {
    match ty {
        ResolvedTy::Named {
            builtin: Some(builtin),
            args,
            ..
        } if matches!(
            (builtin, args.len()),
            (BuiltinType::Vec | BuiltinType::HashSet, 1) | (BuiltinType::HashMap, 2)
        ) =>
        {
            Some((*builtin, args))
        }
        _ => None,
    }
}

/// The member type of a `SupervisorPool<S, T>` receiver.
#[must_use]
pub fn supervisor_pool_member_type(ty: &ResolvedTy) -> Option<&ResolvedTy> {
    match ty {
        ResolvedTy::Named {
            builtin: Some(BuiltinType::SupervisorPool),
            args,
            ..
        } if args.len() == 2 => args.get(1),
        _ => None,
    }
}

/// Recognize canonical Vec identity without inspecting a source leaf name.
#[must_use]
pub fn vector_element_type(ty: &ResolvedTy) -> Option<&ResolvedTy> {
    match ty {
        ResolvedTy::Named {
            builtin: Some(crate::BuiltinType::Vec),
            args,
            ..
        } if args.len() == 1 => args.first(),
        _ => None,
    }
}

/// Element of a contiguous owning sequence. Physical allocation and value glue
/// are shared by vectors and fixed arrays; their semantic operations stay distinct.
#[must_use]
pub fn sequence_element_type(ty: &ResolvedTy) -> Option<&ResolvedTy> {
    match ty {
        ResolvedTy::Array(element, _) => Some(element),
        _ => vector_element_type(ty),
    }
}

/// Ordinary vector value operations, independent of element representation.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
pub enum VecValueOp {
    #[default]
    New,
    Len,
    Contains,
    Index,
    Get,
    Push,
    Set,
    Pop,
    Remove,
    Clear,
    /// `v[i]` where the element has no clone: a loan of the element the vector
    /// still owns, readable for the length of the receiver's loan.
    IndexBorrow,
    /// `v.get(i)` where the element has no clone: `Some` carries a loan of the
    /// slot the vector still owns, readable for the length of the receiver's
    /// loan, and a past-end index still reads `None`.
    GetBorrow,
    /// Remove the first element and hand its ownership to the caller. This is
    /// the consuming iterator's step for an element with no clone: the vector
    /// shrinks by one and ends empty when the drain runs to completion.
    TakeFirst,
    /// Move the whole buffer out and leave the receiver a valid empty vector
    /// with its element representation intact. This is how a `Vec` living in
    /// persistent storage - an actor state field - is drained by a consuming
    /// iteration: the cursor owns the elements and the seat keeps a usable
    /// vector a later dispatch can refill.
    TakeAll,
    /// `v[a..b]` - a fresh independent `Vec<T>` over the selected range.
    Slice,
    /// `v[a..]` - the open-ended form; the runtime supplies the end bound so
    /// the receiver expression is evaluated once.
    SliceFrom,
    /// `dst.append(src)` - every element of `src` is added to `dst`. The
    /// element authority admits this only for the shared-copy families
    /// (bit-copyable and refcounted-string elements), so `src` keeps its own
    /// elements and stays usable afterwards.
    Append,
    /// `v.join(sep)` - concatenate a `Vec<string>` with `sep` between
    /// elements into one fresh string.
    Join,
}

impl VecValueOp {
    #[must_use]
    pub const fn from_method(method: crate::VecMethod) -> Option<Self> {
        Some(match method {
            crate::VecMethod::Len => Self::Len,
            crate::VecMethod::Contains => Self::Contains,
            crate::VecMethod::Get => Self::Get,
            crate::VecMethod::Push => Self::Push,
            crate::VecMethod::Set => Self::Set,
            crate::VecMethod::Pop => Self::Pop,
            crate::VecMethod::Remove => Self::Remove,
            crate::VecMethod::Clear => Self::Clear,
            crate::VecMethod::Append => Self::Append,
            crate::VecMethod::Join => Self::Join,
            _ => return None,
        })
    }
}

/// Ordinary map operations with independently owned keys and values.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
pub enum MapValueOp {
    #[default]
    New,
    Len,
    Index,
    Get,
    /// `m.get(k)` where the value has no clone: `Some` carries a loan of the
    /// slot the map still owns, readable for the length of the receiver's
    /// loan, and an absent key still reads `None`.
    GetBorrow,
    ContainsKey,
    Insert,
    Remove,
    Clear,
    Keys,
    Values,
    Entries,
}

impl MapValueOp {
    #[must_use]
    pub const fn from_method(method: crate::HashMapMethod) -> Option<Self> {
        Some(match method {
            crate::HashMapMethod::Len => Self::Len,
            crate::HashMapMethod::Get => Self::Get,
            crate::HashMapMethod::ContainsKey => Self::ContainsKey,
            crate::HashMapMethod::Insert => Self::Insert,
            crate::HashMapMethod::Remove => Self::Remove,
            crate::HashMapMethod::Clear => Self::Clear,
            crate::HashMapMethod::Keys => Self::Keys,
            crate::HashMapMethod::Values => Self::Values,
            crate::HashMapMethod::Entries => Self::Entries,
            crate::HashMapMethod::Clone | crate::HashMapMethod::IsEmpty => return None,
        })
    }
}

/// Ordinary set operations using the same element and receiver type templates.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
pub enum SetValueOp {
    #[default]
    New,
    Len,
    Contains,
    Insert,
    Remove,
    Clear,
    Elements,
}

impl SetValueOp {
    #[must_use]
    pub const fn from_method(method: crate::HashSetMethod) -> Option<Self> {
        Some(match method {
            crate::HashSetMethod::Len => Self::Len,
            crate::HashSetMethod::Contains => Self::Contains,
            crate::HashSetMethod::Insert => Self::Insert,
            crate::HashSetMethod::Remove => Self::Remove,
            crate::HashSetMethod::Clear => Self::Clear,
            crate::HashSetMethod::ToVec => Self::Elements,
            crate::HashSetMethod::Clone | crate::HashSetMethod::IsEmpty => return None,
        })
    }
}

/// Distinct owned serde representations; no normalization between formats.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
pub enum EncodingFormat {
    #[default]
    Json,
    Yaml,
}

impl EncodingFormat {
    #[must_use]
    pub const fn builtin(self) -> BuiltinType {
        match self {
            Self::Json => BuiltinType::JsonValue,
            Self::Yaml => BuiltinType::YamlValue,
        }
    }

    #[must_use]
    pub const fn module(self) -> &'static str {
        match self {
            Self::Json => "std.encoding.json",
            Self::Yaml => "std.encoding.yaml",
        }
    }
}

// One closed list owns the common operation vocabulary and exact C endpoints.
macro_rules! encoding_operations {
    ($($(#[$attr:meta])* $op:ident => $suffix:literal),+ $(,)?) => {
        #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
        pub enum EncodingOp { $($(#[$attr])* $op),+ }

        impl EncodingOp {
            #[must_use]
            pub const fn c_symbol(self, format: EncodingFormat) -> &'static str {
                match (format, self) {
                    $((EncodingFormat::Json, Self::$op) => concat!("hew_json_", $suffix),
                      (EncodingFormat::Yaml, Self::$op) => concat!("hew_yaml_", $suffix)),+
                }
            }

        }
    };
}

encoding_operations! {
    #[default]
    Parse => "parse",
    LastError => "last_error",
    Stringify => "stringify",
    Type => "type",
    IntStatus => "int_status",
    GetBool => "get_bool",
    GetInt => "get_int",
    GetU64 => "get_u64",
    GetFloat => "get_float",
    GetString => "get_string",
    GetField => "get_field",
    ArrayLen => "array_len",
    ArrayGet => "array_get",
    ObjectKeys => "object_keys",
    ObjectNew => "object_new",
    ArrayNew => "array_new",
    FromBool => "from_bool",
    FromInt => "from_int",
    FromU64 => "from_u64",
    FromFloat => "from_float",
    FromString => "from_string",
    FromNull => "from_null",
    Eq => "eq",
    ObjectSet => "object_set",
    ArrayPush => "array_push",
    Clone => "clone",
    Free => "free",
}

// Instantiate one semantic table for each canonical format. The operand slices
// are static descriptor data, not a second registry of managed values.
macro_rules! encoding_contract {
    ($builtin:ident, $op:expr) => {{
        use RuntimeArgumentEffect::{Borrow, Copy, Move};
        use RuntimeResultEffect::{BitCopy, FreshOwned, Unit, UpdatedReceiver};
        use RuntimeValueKind::{Receiver, String, F64, I32, I64, U64};
        const VALUE: RuntimeValueKind = Receiver(BuiltinType::$builtin);
        const READ: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: VALUE,
            effect: Borrow,
        };
        const WRITE: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: VALUE,
            effect: Move,
        };
        const TEXT: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: String,
            effect: Borrow,
        };
        const INT: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: I32,
            effect: Copy,
        };
        match $op {
            EncodingOp::Parse | EncodingOp::FromString => {
                runtime_semantic_contract(&[TEXT], FreshOwned(VALUE), &[])
            }
            EncodingOp::LastError => runtime_semantic_contract(&[], FreshOwned(String), &[]),
            EncodingOp::Stringify | EncodingOp::GetString => {
                runtime_semantic_contract(&[READ], FreshOwned(String), &[])
            }
            EncodingOp::Type
            | EncodingOp::IntStatus
            | EncodingOp::GetBool
            | EncodingOp::ArrayLen => runtime_semantic_contract(&[READ], BitCopy(I32), &[]),
            EncodingOp::GetInt => runtime_semantic_contract(&[READ], BitCopy(I64), &[]),
            EncodingOp::GetU64 => runtime_semantic_contract(&[READ], BitCopy(U64), &[]),
            EncodingOp::GetFloat => runtime_semantic_contract(&[READ], BitCopy(F64), &[]),
            EncodingOp::GetField => {
                runtime_semantic_contract(&[READ, TEXT], FreshOwned(VALUE), &[])
            }
            EncodingOp::ArrayGet => runtime_semantic_contract(&[READ, INT], FreshOwned(VALUE), &[]),
            EncodingOp::ObjectNew | EncodingOp::ArrayNew | EncodingOp::FromNull => {
                runtime_semantic_contract(&[], FreshOwned(VALUE), &[])
            }
            EncodingOp::FromBool => runtime_semantic_contract(&[INT], FreshOwned(VALUE), &[]),
            EncodingOp::FromInt => runtime_semantic_contract(
                &[RuntimeArgumentContract {
                    ty: I64,
                    effect: Copy,
                }],
                FreshOwned(VALUE),
                &[],
            ),
            EncodingOp::FromU64 => runtime_semantic_contract(
                &[RuntimeArgumentContract {
                    ty: U64,
                    effect: Copy,
                }],
                FreshOwned(VALUE),
                &[],
            ),
            EncodingOp::FromFloat => runtime_semantic_contract(
                &[RuntimeArgumentContract {
                    ty: F64,
                    effect: Copy,
                }],
                FreshOwned(VALUE),
                &[],
            ),
            EncodingOp::Eq => runtime_semantic_contract(&[READ, READ], BitCopy(I32), &[]),
            EncodingOp::ObjectSet => {
                runtime_semantic_contract(&[WRITE, TEXT, WRITE], UpdatedReceiver(VALUE), &[])
            }
            EncodingOp::ArrayPush => {
                runtime_semantic_contract(&[WRITE, WRITE], UpdatedReceiver(VALUE), &[])
            }
            EncodingOp::Clone | EncodingOp::ObjectKeys => {
                runtime_semantic_contract(&[READ], FreshOwned(VALUE), &[])
            }
            EncodingOp::Free => runtime_semantic_contract(&[WRITE], Unit, &[]),
        }
    }};
}

impl EncodingOp {
    pub(super) const fn contract(self, format: EncodingFormat) -> RuntimeSemanticContract {
        match format {
            EncodingFormat::Json => encoding_contract!(JsonValue, self),
            EncodingFormat::Yaml => encoding_contract!(YamlValue, self),
        }
    }
}

pub(crate) const fn runtime_semantic_contract(
    arguments: &'static [RuntimeArgumentContract],
    result: RuntimeResultEffect,
    failures: &'static [RuntimeLogicalFailure],
) -> RuntimeSemanticContract {
    RuntimeSemanticContract {
        arguments,
        result,
        failures,
    }
}

impl ConsumeVerdict {
    /// `true` iff the verdict directs the callee to own/drop the argument —
    /// the projection back onto the historical `bool` (both consume flavours
    /// collapse to `true`). Consumers that only need the safety-relevant
    /// borrow-vs-consume bit call this; the finer label drives precision only.
    #[must_use]
    pub const fn is_consume(self) -> bool {
        !matches!(self, Self::ProvenBorrow)
    }
}

// =============================================================================
// Element-type discriminators
// =============================================================================

/// Element-type discriminator for `Vec<T>::get` runtime entries
/// (`hew_vec_get_*`). One variant per monomorphic-element C-ABI symbol
/// emitted today by `hew-runtime/src/vec.rs`.
///
/// `Layout` covers the layout-descriptor path for `BitCopy` Named records
/// and tuples; `Owned` is the W5.016 borrow-getter for non-Copy owned
/// elements. The pending Vec genericisation work will collapse most of
/// these onto a single `Generic` variant that reads the descriptor's
/// `elem` field — out of scope here.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter, Serialize, Deserialize)]
pub enum VecGetElem {
    #[default]
    Bool,
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    /// Descriptor-backed deep clone into a caller-provided output slot.
    ///
    /// Unlike the scalar getters, this family rides the intercepted
    /// `Terminator::Call` path because its concrete element layout determines
    /// the hidden output-pointer ABI.
    Clone,
    /// Descriptor-backed move into a caller-provided output slot. Used by
    /// consuming iteration over drop-only elements.
    Take,
    Layout,
    Owned,
    Ptr,
    Str,
    U8,
    U16,
}

/// Scalar element ABI discriminator shared by the direct Vec operation
/// matrix. `bool`, layout-managed, owned, and closure-pair paths remain
/// distinct families because their ABI/ownership contracts differ.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter, Serialize, Deserialize)]
pub enum VecScalarElem {
    #[default]
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    Ptr,
    Str,
    U8,
    U16,
}

/// The scalar Vec operations that share the same element discriminator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter, Serialize, Deserialize)]
pub enum VecScalarOp {
    #[default]
    Push,
    Pop,
    Set,
    RemoveAt,
}

/// Element discriminators for the scalar `Vec::contains` ABI entries that
/// exist in the runtime. Other element representations use their dedicated
/// layout/owned families.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter, Serialize, Deserialize)]
pub enum VecContainsScalarElem {
    F64,
    #[default]
    I32,
    I64,
    Str,
}

/// Element-type discriminator for `Vec<T>::slice_range` runtime entries
/// (`hew_vec_slice_range_*`). Narrower than [`VecGetElem`] because slice
/// does not have a bool path today; descriptor-backed record/tuple elements
/// route through the `Layout`/`Owned` substrate variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter, Serialize, Deserialize)]
pub enum VecSliceElem {
    #[default]
    Bytesize,
    F64,
    I32,
    I64,
    Layout,
    Owned,
    Ptr,
    Str,
}

/// Math-intrinsic family discriminator. HIR resolves the user-visible
/// identifiers (`"sqrt"`, `"sin"`, …) onto
/// `RuntimeCallFamily::MathIntrinsic(...)`; MIR carries that family on the
/// call so codegen never re-derives the intrinsic from the callee string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter, Serialize, Deserialize)]
pub enum MathIntrinsic {
    #[default]
    Sqrt,
    Exp,
    Log,
    Sin,
    Cos,
    AbsI64,
    MinI64,
    MaxI64,
    AbsF64,
    MinF64,
    MaxF64,
    Pow,
    Floor,
    Ceil,
    Round,
    Tan,
    Asin,
    Acos,
    Atan,
    Atan2,
    Sinh,
    Cosh,
    Tanh,
    Exp2,
    Log2,
    Log10,
    /// Not an LLVM intrinsic on this target's LLVM version; codegen declares
    /// and calls the libm symbol `log1p` directly (see `emit_math_intrinsic`).
    Log1p,
    /// Libm-only, like `Log1p`: codegen calls `expm1` directly.
    Expm1,
    /// Libm-only, like `Log1p`: codegen calls `cbrt` directly.
    Cbrt,
    /// Libm-only, like `Log1p`: codegen calls `hypot` directly.
    Hypot,
    Fma,
    Trunc,
    Copysign,
    /// `x.powi(n)`: `llvm.powi.f64.i32`. `n` is `i32` — LLVM's `powi` overload
    /// is parameterized on a fixed integer width and does not generalize to
    /// `i64`; widening `n` to `i64` in source would silently truncate at the
    /// call site instead of at the declared signature.
    Powi,
    /// `f64.from_bits(u64) -> f64`: reinterpret the bit pattern (`bitcast`,
    /// not a real LLVM intrinsic call — codegen special-cases it like the
    /// libm symbols above). Spelled `math.from_bits`, not the primitive
    /// type-path call `f64.from_bits` the brief for this looked for: the
    /// checker does not admit a bare builtin type name (`f64`, `i32`, …) as
    /// a call receiver today (`error: type f64 cannot be used as a value`),
    /// and adding that surface is out of scope for one function.
    FromBits,
}

impl MathIntrinsic {}

/// Bit-manipulation method discriminator for `x.count_ones()` and its
/// siblings. Every op maps to one LLVM intrinsic (`ctpop`, `ctlz`, `cttz`,
/// `bswap`, `bitreverse`, `fshl`/`fshr`) parameterized on the receiver's
/// width, carried separately as [`IntMethodWidth`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter, Serialize, Deserialize)]
pub enum IntBitOp {
    #[default]
    CountOnes,
    CountZeros,
    LeadingZeros,
    TrailingZeros,
    SwapBytes,
    ReverseBits,
    RotateLeft,
    RotateRight,
}

/// Receiver width for [`IntBitOp`]/[`IntArithKind`] methods, covering every
/// integer width Hew has. Narrow widths (`I8`/`I16`/`U8`/`U16`) and the
/// pointer-sized widths (`Isize`/`Usize`) share codegen's C ABI convention
/// for `RuntimeValueKind`: they carry their own LLVM width end to end (no
/// widening at a call boundary), so codegen picks the LLVM intrinsic at the
/// value's own bit width (`llvm.ctpop.i8`, `llvm.sadd.sat.i16`, …) and needs
/// no runtime symbol per width.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter, Serialize, Deserialize)]
pub enum IntMethodWidth {
    I8,
    I16,
    #[default]
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
}

/// `f64` bit/classification methods (`x.to_bits()`, `x.is_nan()`, …).
/// Scoped to `f64`: `RuntimeValueKind` has no `F32` kind yet, matching the
/// `IntMethodWidth` gap for the narrower integer widths.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter, Serialize, Deserialize)]
pub enum FloatMethodOp {
    #[default]
    ToBits,
    IsNan,
    IsFinite,
    IsInfinite,
    IsSignNegative,
}

/// Non-trapping and overflow-checked integer arithmetic: `x.wrapping_add(y)`
/// and its siblings. `SaturatingMul` has no direct LLVM saturating-multiply
/// intrinsic; codegen builds it from `llvm.{s,u}mul.with.overflow` plus a
/// select onto the saturated bound. `Checked*` results are `Option<T>`
/// through the ordinary `VariantResult` physical form (see the row table).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter, Serialize, Deserialize)]
pub enum IntArithKind {
    #[default]
    WrappingAdd,
    WrappingSub,
    WrappingMul,
    SaturatingAdd,
    SaturatingSub,
    SaturatingMul,
    CheckedAdd,
    CheckedSub,
    CheckedMul,
}

// =============================================================================
// RuntimeCallFamily — closed-set typed catalog
// =============================================================================

/// Selected primitive print representation. Physical lowering carries this
/// choice and the newline flag to the runtime's tagged print ABI.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
pub enum PrintKind {
    I32,
    #[default]
    I64,
    U8,
    U32,
    U64,
    F64,
    Bool,
    Str,
}

impl PrintKind {}
