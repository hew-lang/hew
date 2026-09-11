//! Typed contracts for compiler-known runtime operations.
//!
//! The checker selects runtime families and argument/result semantics. HIR and
//! SIR preserve those facts; physical lowering chooses their storage and ABI.
//! Adding an exported symbol does not by itself implement a source operation:
//! its selected family still needs a complete lowering and execution contract.
//!
//! This module lives in `hew-types` so source checking and downstream stages
//! consume the same family definitions without a dependency cycle. Foreign
//! symbol ownership is described separately by `crate::ffi_contracts`.

mod array;
pub use array::ArrayValueOp;

mod supervisor_pool;
pub use supervisor_pool::SupervisorPoolOp;

mod async_io;
pub use async_io::{AsyncIoLoan, AsyncIoOp, AsyncIoResume, IoHandleKind};
mod tcp;
pub use tcp::TcpOp;
mod file_resources;
pub use file_resources::{FileReadHandleKind, FileReadOp};

use crate::{BuiltinType, ResolvedTy};
use serde::{Deserialize, Serialize};
use strum::{EnumIter, IntoEnumIterator};

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
    /// One generic channel half, resolved to the receiver's checked type.
    ChannelHalf(ChannelHalfKind),
    /// A freshly extracted channel half named by the operation, not by the
    /// receiver: `hew_channel_pair_sender` borrows a pair and returns a half.
    ChannelHalfResult(ChannelHalfKind),
    /// The paired channel allocation `channel.new` splits. Its nominal
    /// identity comes from the generated `hew_channel_new` ownership row.
    ChannelPair,
    ActorRequestOwner,
    ActorRequestAdmission,
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
    /// Ordinary type construction, shared by optional results and projections.
    Applied(BuiltinType, &'static [Self]),
    /// Ordinary product results, including receiver replacement with a value.
    Tuple(&'static [Self]),
}

impl RuntimeValueKind {
    #[must_use]
    pub const fn matches(self, ty: &ResolvedTy) -> bool {
        matches!(
            (self, ty),
            (Self::Unit, ResolvedTy::Unit)
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
            Self::ChannelHalf(kind) => {
                let receiver = receiver?;
                if !kind.matches(receiver) {
                    return None;
                }
                receiver.clone()
            }
            Self::ChannelHalfResult(_) | Self::NodeLookupResult => return None,
            Self::ChannelPair => channel_pair_ty()?,
            Self::ActorRequestOwner => actor_request_owner_ty(),
            Self::ActorRequestAdmission => {
                ResolvedTy::named_opaque("std.builtins.ActorRequestAdmission", Vec::new())
            }
            Self::Unit => ResolvedTy::Unit,
            Self::Bool => ResolvedTy::Bool,
            Self::F64 => ResolvedTy::F64,
            Self::Char => ResolvedTy::Char,
            Self::String => ResolvedTy::String,
            Self::Bytes => ResolvedTy::Bytes,
            Self::Duration => ResolvedTy::Duration,
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
            // Only the ten integer widths reach this catch-all; every other
            // variant is matched above. See `resolve_integer_width`.
            kind => resolve_integer_width(kind),
        })
    }
}

/// Resolve one of the ten integer-width `RuntimeValueKind` variants to its
/// `ResolvedTy`. Split out of `resolve` to keep that match under clippy's
/// line limit; the only caller is `resolve`'s catch-all arm, reached only
/// after every other variant has already matched.
fn resolve_integer_width(kind: RuntimeValueKind) -> ResolvedTy {
    match kind {
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
        _ => unreachable!("resolve_integer_width is only called with an integer-width kind"),
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
                        || ChannelHalfKind::of_ty(ty).is_some()
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
            .map(|expected| {
                expected.ty.resolve(receiver).ok_or_else(|| {
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
                    || matches!(kind, RuntimeValueKind::ChannelHalfResult(half) if half.matches(result_hint))
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

/// Which half of a channel a runtime contract's receiver is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChannelHalfKind {
    Sender,
    Receiver,
}

impl ChannelHalfKind {
    /// Match a checked generic endpoint with exactly one message type.
    #[must_use]
    pub fn matches(self, ty: &ResolvedTy) -> bool {
        let expected = match self {
            Self::Sender => BuiltinType::Sender,
            Self::Receiver => BuiltinType::Receiver,
        };
        matches!(ty, ResolvedTy::Named { builtin: Some(builtin), args, .. }
            if *builtin == expected && args.len() == 1)
    }

    /// The channel half one checked type is, if it is one.
    #[must_use]
    pub fn of_ty(ty: &ResolvedTy) -> Option<Self> {
        [Self::Sender, Self::Receiver]
            .into_iter()
            .find(|kind| kind.matches(ty))
    }
}

/// The nominal the generated `hew_channel_new` row names as its owned result.
/// Reading the row rather than the spelling keeps one ownership authority.
#[must_use]
pub fn channel_pair_ty() -> Option<ResolvedTy> {
    let contract = crate::ffi_contracts::extern_owned_resource_result("hew_channel_new")?;
    Some(ResolvedTy::named_opaque(contract.resource_type, Vec::new()))
}

/// The sealed completion request's source-declared runtime owner.
#[must_use]
pub fn actor_request_owner_ty() -> ResolvedTy {
    ResolvedTy::named_opaque("std.builtins.ActorRequestOwner", Vec::new())
}

/// Whether one checked type is the paired channel allocation.
#[must_use]
pub fn is_channel_pair_ty(ty: &ResolvedTy) -> bool {
    channel_pair_ty().is_some_and(|pair| pair == *ty)
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
    const fn contract(self, format: EncodingFormat) -> RuntimeSemanticContract {
        match format {
            EncodingFormat::Json => encoding_contract!(JsonValue, self),
            EncodingFormat::Yaml => encoding_contract!(YamlValue, self),
        }
    }
}

const fn runtime_semantic_contract(
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

/// Element-kind discriminator for the Sink write families
/// (`hew_sink_write_bytes` vs `hew_sink_write_string`). Stream/channel
/// recv retired their per-element symbols in favour of the
/// element-layout-witness `*_layout` entries, which bypass
/// `RuntimeCallFamily` entirely (codegen `Terminator::Call` intercept).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter, Serialize, Deserialize)]
pub enum StreamElementKind {
    #[default]
    Bytes,
    String,
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

/// Closed-set discriminator for every compiler-known runtime / builtin
/// call. One variant per `(method, generic-arity)` tuple. Adding a new
/// runtime operation means adding a variant here and classifying its emitter
/// route. Symbol-derived operations round-trip through `from_c_symbol`;
/// print operations also require their selected type and newline flag.
///
/// Variants are grouped by surface family; ordering within a group is
/// alphabetical by C-symbol leaf to ease diffing against the allowlist.
///
/// `non_exhaustive` is INTENTIONALLY OMITTED — this enum's whole purpose
/// is fail-closed exhaustiveness across consumer match sites (LESSONS P0
/// `match-fail-closed`). A future contributor cannot silently extend it
/// behind a wildcard arm; every consumer must add the new arm at the
/// next slice.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
pub enum RuntimeCallFamily {
    AsyncIo(AsyncIoOp),
    FileRead(FileReadOp),
    Tcp(TcpOp),
    /// Semantic operations over a distinct, owning encoding value.
    Encoding {
        format: EncodingFormat,
        op: EncodingOp,
    },
    // --- Actor cooperate/link/monitor/unlink/spawn surface ------------------
    ActorAsk,
    ActorAskWithChannel,
    ActorCooperate,
    /// `MonitorRef::close` → `hew_actor_demonitor(ref_id: u64) -> void`.
    /// In the drop path, codegen extracts `ref_id` from the struct alloca
    /// via `build_struct_gep` + `build_load` and passes it directly.
    /// Present as a `RuntimeCallFamily` variant for allowlist parity only;
    /// the canonical path is `RuntimeDropDescriptor::MonitorRefClose`.
    ActorDemonitor,
    /// `hew_actor_gen_sink_complete(actor, sink) -> void` — a `receive gen
    /// fn` stream-producer pump's clean (generator-exhausted) exit:
    /// deregisters the actor's gen-sink slot and frees the sink (decision
    /// 7). Emitted only by `build_stream_producer_pump`
    /// (`hew-mir/src/lower.rs`); no user-facing Hew syntax reaches it.
    /// Pre-staged like `SinkClose`, which it replaces in the pump.
    ActorGenSinkComplete,
    /// `hew_actor_gen_sink_register(actor, sink) -> void` — a `receive gen
    /// fn` pump's prologue registration of its own producer sink, so a
    /// terminal actor teardown can find and fault-close it (decision
    /// 7). Emitted only by `build_stream_producer_pump`.
    ActorGenSinkRegister,
    ActorLink,
    /// `link_remote(RemotePid<T>, PartitionPolicy)` →
    /// `hew_node_link_remote_location(target, policy_tag) -> i64`. Establishes a
    /// cross-node link: a local actor links a remote actor so the remote's
    /// death (exit / crash / partition) fires the per-link `PartitionPolicy`
    /// (`CrashLinked` crashes the local actor). Distinct from `ActorLink` (the
    /// process-local pointer-keyed link); the cross-node form has no local
    /// `HewActor*` for the remote peer.
    LinkRemote,
    ActorMonitor,
    ActorSelf,
    ActorSendById,
    ActorSpawn,
    ActorUnlink,

    // --- Auto-injected mutex substrate (closure-env / generator-state) ------
    AutoMutexAlloc,
    AutoMutexFree,
    AutoMutexLock,
    AutoMutexUnlock,

    // --- Bytes value collection ops -----------------------------------------
    BytesAppend,
    BytesClear,
    BytesContains,
    BytesDecodeUtf8,
    BytesDecodeUtf8Lossy,
    BytesGet,
    BytesIndex,
    BytesIsEmpty,
    BytesLen,
    BytesPop,
    BytesPush,
    BytesSet,
    BytesSlice,
    /// `b[a..]` - the open-ended byte slice. The end bound is the receiver's
    /// own length, so the receiver expression is evaluated once.
    BytesSliceFrom,
    /// `bytes::new` constructor callee identity. Codegen materialises the
    /// runtime `hew_bytes_new` call from the destination type.
    BytesNew,

    // --- CancellationToken retain/release/poll ------------------------------
    CancelTokenIsRequested,
    CancelTokenRelease,
    CancelTokenRetain,

    // --- Channel<T> (std::channel) ------------------------------------------
    // recv/try_recv/send ride the element-layout-witness `*_layout`
    // entries (one symbol per operation for every describable element
    // type; the elem identity travels on the checker-resolved
    // `Option<T>` / value type, never on the symbol). They are
    // pre-staged: codegen intercepts the `Terminator::Call` by callee
    // identity, so they are not in `known_runtime_symbols`.
    ChannelRecvLayout,
    ChannelSendLayout,
    ChannelTryRecvLayout,
    ChannelSenderClone,
    ChannelSenderClose,
    ChannelReceiverClose,
    // The paired allocation `channel.new` splits into its two halves. It never
    // escapes that function: `new` extracts both halves and frees the pair.
    ChannelPairNew,
    ChannelPairFree,
    ActorRequestRelease,
    ActorCallFree,
    ActorRequestTake,
    ChannelPairIsValid,
    ChannelPairSender,
    ChannelPairReceiver,

    // --- Duplex<S, R> dual-queue substrate ----------------------------------
    DuplexClone,
    DuplexClose,
    DuplexCloseHalf,
    DuplexPair,
    DuplexPayloadFree,
    DuplexRecv,
    DuplexRecvHalf,
    DuplexSend,
    DuplexSendHalf,
    DuplexTryRecv,
    DuplexTrySend,

    // --- Duration accessors (monomorphic time canaries) ---------------------
    DurationAbs,
    DurationHours,
    DurationIsZero,
    DurationMicros,
    DurationMillis,
    DurationMins,
    DurationNanos,
    DurationSecs,

    // --- Trait-object heap-box storage --------------------------------------
    DynBoxAlloc,
    DynBoxFree,

    // --- Layout-backed HashMap ---------------------------------------------
    HashMapContainsKeyLayout,
    HashMapClearLayout,
    HashMapCloneLayout,
    HashMapEntriesLayout,
    HashMapFreeLayout,
    HashMapGetLayout,
    HashMapInsertLayout,
    /// `m.keys()` / `m.values()` projection ops. Pre-staged: they ride
    /// the `Terminator::Call` route (checker `MethodTarget.symbol_name`),
    /// not `Instr::CallRuntimeAbi`, so their symbols are not in
    /// `known_runtime_symbols`. Catalogued so the codegen
    /// layout-fact walker classifies them by family, not symbol prefix.
    HashMapKeysLayout,
    HashMapLenLayout,
    /// The `HashMap::new` constructor surface form. A distinct callee
    /// identity from [`Self::HashMapNewWithLayout`]: the catalog row
    /// `"HashMap::new"` (`BuiltinLinkage::CalleeNameDispatchOnly`)
    /// survives to codegen as the literal callee name when the checker
    /// did not rewrite the construction to the synthesized
    /// `hew_hashmap_new_with_layout` form. Both identities are real at
    /// the `Terminator::Call` intercept; the bijection demands one
    /// variant per callee identity.
    HashMapNew,
    HashMapNewWithLayout,
    HashMapRemoveLayout,
    HashMapValuesLayout,

    // --- Layout-backed HashSet ---------------------------------------------
    HashSetContainsLayout,
    HashSetClearLayout,
    HashSetCloneLayout,
    HashSetFreeLayout,
    HashSetInsertLayout,
    HashSetIsEmptyLayout,
    HashSetLenLayout,
    /// The `HashSet::new` constructor surface form; see
    /// [`Self::HashMapNew`] for the two-identity rationale.
    HashSetNew,
    HashSetNewWithLayout,
    HashSetRemoveLayout,
    HashSetToVecLayout,

    // --- Instant accessors --------------------------------------------------
    InstantDurationSince,
    InstantElapsed,
    InstantNow,

    // --- Math intrinsics ---------------------------------------------------
    // User-visible callee identities carried on MIR `Terminator::Call`; not
    // runtime C-ABI symbols and therefore absent from `known_runtime_symbols`.
    MathIntrinsic(MathIntrinsic),
    // Integer bit-manipulation methods (`x.count_ones()`, …). Same shape as
    // `MathIntrinsic`: a user-visible callee identity carried on MIR
    // `Terminator::Call`, not a runtime C-ABI symbol.
    IntMethod(IntBitOp, IntMethodWidth),
    // Non-trapping integer arithmetic (`x.wrapping_add(y)`, `x.saturating_sub(y)`).
    // Same shape as `IntMethod`.
    IntArith(IntArithKind, IntMethodWidth),
    // `f64` bit/classification methods (`x.to_bits()`, `x.is_nan()`, …).
    // Same shape as `IntMethod`.
    FloatMethod(FloatMethodOp),

    // --- Node operations ---------------------------------------------------
    // The `Node::*` variants are pre-staged Terminator::Call callee identities.
    // Their runtime FFI symbols are carried separately by the stdlib catalog;
    // the family records the source-level builtin identity used by checker,
    // MIR, and codegen dispatch.
    NodeAllowPeer,
    NodeConnect,
    NodeId,
    NodeIdentityKey,
    NodeLoadKeys,
    NodeLookup,
    /// `monitor(RemotePid<T>)` →
    /// `hew_node_monitor_location(target, out_monitor_id) -> i32`.
    /// Zero returns success and writes the distributed-monitor id; non-zero is
    /// one plus the `MonitorError` discriminant. Codegen assembles
    /// `Result<MonitorRef, MonitorError>`. The current node is resolved
    /// internally, so the single runtime argument is a pointer to the carried
    /// full `Location`; non-consuming.
    NodeMonitor,
    NodeRegister,
    NodeSetTransport,
    NodeShutdown,
    NodeStart,

    // --- User metrics (#1862) -----------------------------------------------
    // `std::metrics` emit path: register-or-get + mutate developer-defined
    // counters/gauges/histograms (and their labelled `*Vec` forms). Non-
    // suspending, non-consuming (handles are Copy index IDs).
    MetricCounterRegister,
    MetricCounterInc,
    MetricCounterAdd,
    MetricGaugeRegister,
    MetricGaugeSet,
    MetricGaugeInc,
    MetricGaugeDec,
    MetricGaugeAdd,
    MetricHistogramRegister,
    /// Bucketless histogram register (name-only ABI). The bucketed
    /// `MetricHistogramRegister` takes a raw `(*const i64, len)` array that a
    /// Hew `extern "C"` declaration cannot express, so the stdlib reaches this
    /// scalar entry point instead; it registers a histogram with no buckets
    /// (just the running observation count).
    MetricHistogramRegisterSimple,
    MetricHistogramRecord,
    MetricVecRegister,
    MetricVecWith,

    // --- Observe ------------------------------------------------------------
    ObserveReadU64,
    ObserveScrape,
    ObserveSeries,
    ObserveBarrier,

    // --- Rc/Weak ownership --------------------------------------------------
    RcClone,
    RcDowngrade,
    RcDrop,
    RcGet,
    RcIsUnique,
    RcNew,
    RcSet,
    RcStrongCount,
    RcWeakCount,
    WeakCloneRc,
    WeakDropRc,
    WeakUpgradeRc,

    // --- RecvHalf<T> --------------------------------------------------------
    RecvHalfRecv,
    RecvHalfTryRecv,

    // --- Regex runtime ABI --------------------------------------------------
    RegexCapture,
    RegexCompile,
    RegexFreeCapture,
    /// Value-position regex literal materialisation (`let pat = re"..."`).
    /// Operand zero is the literal's slot index; the result is the handle SIR
    /// wraps in a `std.text.regex.Pattern`, exactly as `regex.new` wraps the
    /// handle `hew_regex_new` returns. Codegen GEP-loads the compiled
    /// `*HewRegex` from `@hew_regex_handles[literal_id]` (the same load
    /// `RegexMatch` performs) and clones it, because that pattern's scope exit
    /// frees its handle and the module slot outlives every value built from it.
    /// The C-symbol spelling `hew_regex_handle` names the synthetic family for
    /// the round-trip bijection only; the call it emits is `hew_regex_clone`.
    RegexHandle,
    RegexMatch,

    // --- RemotePid<T>::send intercept --------------------------------------
    // Pre-staged for codegen-intercept consumers: `pid.send(msg)` on a
    // `RemotePid<T>` receiver dispatches via the `hew_remote_pid_send`
    // callee-name intercept (`hew-codegen-rs/src/llvm.rs:25649`); it
    // does NOT call that symbol directly (codegen emits the
    // `hew_actor_send_by_id` sequence + `Result<(), SendError>` wrapping
    // in-place). The catalog declares `hew_remote_pid_send` with linkage
    // `BuiltinLinkage::CalleeNameDispatchOnly` so the symbol is a real
    // callee identity but has no extern body.
    //
    // `RemoteActorAsk` is INTENTIONALLY ABSENT: the checker's
    // `MethodCallRewrite::RemoteActorAsk` is a fieldless structured
    // marker that HIR lowers to `HirExprKind::RemoteActorAsk` and MIR
    // lowers to `Terminator::RemoteAsk` — there is no callee-name string
    // anywhere in that path, so it does not belong in a runtime-call
    // descriptor catalog.
    RemotePidSend,

    // --- Reply channel surface (select{} actor-ask arm) ---------------------
    ReplyChannelCancel,
    ReplyChannelFree,
    ReplyChannelNew,
    /// Waiter-side payload free — frees the libc-allocated reply
    /// buffer that the runtime publishes back to the ask call site.
    /// Distinct from `LambdaBodyAllocReplyBuf` (body-side allocator)
    /// and from `ReplyChannelFree` (handle-level cleanup).
    ReplyPayloadFree,
    ReplyWait,

    // --- Select winner-picker ----------------------------------------------
    SelectFirst,

    // --- SendHalf<T> -------------------------------------------------------
    SendHalfSend,
    SendHalfTrySend,

    // --- Sink<T> -----------------------------------------------------------
    // Pre-staged consumers: today the bytes path is producer-emitted via
    // `Terminator::Call` intercept; symbols flow through the codegen
    // callee-name match, not `Instr::CallRuntimeAbi`. Both element kinds
    // exist in the runtime (`hew-runtime/src/sink.rs`) and in the checker
    // `BuiltinMethodRuntime::ElementOverload` table at
    // `hew-types/src/builtin_names.rs:253-265`. `SinkTryWrite` mirrors
    // `Sink::try_send` from the same table.
    SinkClose,
    /// `hew_sink_peer_closed(sink) -> i32` — a `receive gen fn` pump's
    /// per-iteration peer-closed check (decision 6): 1 once the
    /// consumer stream has closed/detached, so the pump breaks its loop
    /// WITHOUT resuming the generator further (cancellation; an infinite
    /// generator plus a consumer `break` must not livelock the actor).
    /// Emitted only by `build_stream_producer_pump`; pre-staged like
    /// `SinkClose`.
    SinkPeerClosed,
    SinkWrite(StreamElementKind),
    SinkTryWrite(StreamElementKind),

    // --- Stream<T> ---------------------------------------------------------
    // recv/try_recv/send ride the element-layout-witness `*_layout`
    // entries (see the Channel note above). `consumes_receiver()` is
    // `true` for `StreamClose`/`SinkClose` to mirror
    // `runtime_symbol_consumes_receiver`.
    StreamClose,
    /// The three lazy adaptors. Each consumes its source stream and returns a
    /// fresh one that closes the source cooperatively when the consumer stops.
    StreamChunks,
    StreamLines,
    StreamNextLayout,
    StreamSendLayout,
    StreamTake,
    StreamTryNextLayout,

    // --- String runtime helpers --------------------------------------------
    StringCharAt,
    StringCharAtUtf8,
    StringCharCount,
    StringByteLen,
    StringConcat,
    StringEquals,
    StringCompare,
    StringStartsWith,
    StringEndsWith,
    StringContains,
    StringIsEmpty,
    StringIsDigit,
    StringIsAlpha,
    StringIsAlphanumeric,
    StructuralFormat,
    StringFind,
    StringGet,
    StringIndex,
    StringLen,
    /// `s.repeat(n)` - a fresh string of `n` concatenated copies.
    StringRepeat,
    StringReplace,
    StringClone,
    /// `s.split(sep)` - a fresh `Vec<string>` of the separated parts.
    StringSplit,
    /// `s.lines()` - a fresh `Vec<string>` split on line boundaries.
    StringLines,
    /// `s.chars()` - a fresh `Vec<char>` of the string's Unicode scalars.
    StringChars,
    StringSliceCodepoints,
    StringSliceCodepointsFrom,
    StringSlice,
    StringToLowercase,
    StringToBytes,
    StringToUppercase,
    StringTrim,
    U8ToString,
    I32ToString,
    I64ToString,
    U32ToString,
    U64ToString,
    F64ToString,
    CharToString,
    /// The selected type and newline choice are required in addition to the
    /// shared C symbol; they cannot be reconstructed from that symbol alone.
    Print {
        kind: PrintKind,
        newline: bool,
    },
    /// Flush the standard streams and terminate the process with a Hew exit
    /// code. The call never returns.
    ProcessExit,
    /// Write one borrowed string to the standard error stream.
    StderrWrite,
    BoolToString,

    // --- Supervisor --------------------------------------------------------
    /// Capture the stable direct identity for a live supervisor binding.
    SupervisorDirectId,
    SupervisorChildGet,
    /// Resolve a static child role through a stable supervisor identity and
    /// return the current incarnation's stable `LocalPid` token.
    LocalPidSupervisorChildGet,
    SupervisorNestedGet,
    /// `hew_supervisor_pool_child_get(sup, pool_key, index) -> ChildLookupResult`
    /// — resolve a static-pool member through its live static slot. Emitted by
    /// the MIR static-pool accessor (`sup.pool[i]` / `.get(i)`).
    SupervisorPoolChildGet,
    /// Resolve a static-pool index to its stable static-child slot without
    /// consulting the member incarnation or liveness state.
    LocalPidSupervisorPoolChildRefGet,
    /// `hew_supervisor_pool_len(sup, pool_key) -> i64` — the static-pool member
    /// count (`sup.pool.len()`).
    SupervisorPoolLen,
    SupervisorStop,
    /// `hew_supervisor_restart_await_blocking(sup, key) -> void` — the
    /// contextless `await_restart` path (`main` / free fn). Blocks the calling
    /// thread until the child slot is Live or permanently Dead.
    SupervisorRestartAwaitBlocking,

    // --- Active transport attach (network actor binding) -------------------
    // Pre-staged method calls dispatch via callee-name intercepts that
    // synthesize concrete actor protocol IDs at codegen time.
    TcpAttachLocal,
    TlsAttachLocal,
    WebSocketAttachLocal,

    // --- Task ABI (scope{}/spawn/await) ------------------------------------
    TaskAwaitBlocking,
    TaskCompleteThreaded,
    TaskCompletionObserve,
    TaskCompletionUnobserve,
    TaskFree,
    GeneratorFree,
    TaskGetEnv,
    TaskGetError,
    TaskGetResult,
    TaskNew,
    TaskScopeCancelAfterNs,
    TaskScopeDestroy,
    TaskScopeJoinAll,
    TaskScopeNew,
    TaskScopeSetCurrent,
    TaskScopeSpawn,
    TaskSetEnv,
    TaskSetResult,
    TaskSpawnThread,

    // --- Vec<T> ------------------------------------------------------------
    /// Final semantic values; target lowering chooses layout-backed runtime entry points.
    Vector(VecValueOp),
    Array(ArrayValueOp),
    /// `sup.pool[i]`, `sup.pool.get(i)` and `await_restart sup.pool[i]`.
    SupervisorPool(SupervisorPoolOp),
    Map(MapValueOp),
    Set(SetValueOp),
    VecAppend,
    VecClear,
    VecClone,
    VecCloneLayout,
    VecCloneOwned,
    VecContainsLayout,
    /// Whole-buffer move: `hew_vec_take_all(v)` returns a fresh vec owning
    /// every element (no clone/drop thunk runs) and leaves `v` a valid empty
    /// vec with its element representation intact. The consuming-iteration
    /// choke for a Vec living in persistent storage (an actor state field).
    VecTakeAll,
    VecContainsOwned,
    VecContainsScalar(VecContainsScalarElem),
    VecGet(VecGetElem),
    VecIsEmpty,
    VecJoinStr,
    VecLen,
    VecNew,
    VecPopBool,
    VecPopLayout,
    VecPopOwned,
    VecPushBool,
    VecPushLayout,
    VecPushOwned,
    VecPushOwnedMove,
    /// Closed scalar Vec ABI matrix (`hew_vec_{push,pop,set,remove_at}_$elem`).
    VecScalar {
        op: VecScalarOp,
        elem: VecScalarElem,
    },
    VecRemoveAtBool,
    VecRemoveAtLayout,
    VecRemoveAtOwned,
    VecSetBool,
    VecSetLayout,
    VecSetOwned,
    VecSetOwnedMove,
    VecSliceRange(VecSliceElem),

    // --- Trait-object dispatch diagnostics ---------------------------------
    VtableDispatchPanicOnOob,
}

/// Module-level runtime authorities implied by typed runtime-call families.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeCapability {
    BlockingOffload,
    Metrics,
    Node,
}

/// Checker-owned surface type shapes for canonical stdlib extern methods.
///
/// This is intentionally narrower than the runtime's physical C ABI.  It
/// describes the checked Hew declaration that may mint a typed
/// [`RuntimeCallFamily`], so a linker spelling plus matching arity is never
/// enough to select a special lowering path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalExternTy {
    Bytes,
    Char,
    U8,
    I64,
    Bool,
    String,
    Unit,
    OptionU8,
    OptionI64,
    OptionChar,
    VecString,
    VecChar,
    Duration,
    Instant,
}

impl CanonicalExternTy {
    const fn matches(self, ty: &crate::Ty) -> bool {
        match self {
            Self::Bytes => matches!(ty, crate::Ty::Bytes),
            Self::Char => matches!(ty, crate::Ty::Char),
            Self::U8 => matches!(ty, crate::Ty::U8),
            Self::I64 => matches!(ty, crate::Ty::I64),
            Self::Bool => matches!(ty, crate::Ty::Bool),
            Self::String => matches!(ty, crate::Ty::String),
            Self::Unit => matches!(ty, crate::Ty::Unit),
            Self::Duration => matches!(ty, crate::Ty::Duration),
            Self::Instant => matches!(
                ty,
                crate::Ty::Named {
                    builtin: Some(crate::BuiltinType::Instant),
                    args,
                    ..
                } if args.is_empty()
            ),
            Self::VecString => matches!(
                ty,
                crate::Ty::Named {
                    builtin: Some(crate::BuiltinType::Vec),
                    args,
                    ..
                } if matches!(args.as_slice(), [crate::Ty::String])
            ),
            Self::VecChar => matches!(
                ty,
                crate::Ty::Named {
                    builtin: Some(crate::BuiltinType::Vec),
                    args,
                    ..
                } if matches!(args.as_slice(), [crate::Ty::Char])
            ),
            Self::OptionU8 => matches!(
                ty,
                crate::Ty::Named {
                    builtin: Some(crate::BuiltinType::Option),
                    args,
                    ..
                } if matches!(args.as_slice(), [crate::Ty::U8])
            ),
            Self::OptionI64 => matches!(
                ty,
                crate::Ty::Named {
                    builtin: Some(crate::BuiltinType::Option),
                    args,
                    ..
                } if matches!(args.as_slice(), [crate::Ty::I64])
            ),
            Self::OptionChar => matches!(
                ty,
                crate::Ty::Named {
                    builtin: Some(crate::BuiltinType::Option),
                    args,
                    ..
                } if matches!(args.as_slice(), [crate::Ty::Char])
            ),
        }
    }
}

/// One exact stdlib extern declaration that is eligible for a typed runtime
/// carrier. `family: None` records a real canonical source method whose
/// dedicated codegen path remains an open-set extern call; it prevents that
/// row from being silently treated as an omitted declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CanonicalStdlibExternSignature {
    pub module: &'static str,
    pub signature_key: &'static str,
    pub symbol: &'static str,
    pub family: Option<RuntimeCallFamily>,
    pub params: &'static [CanonicalExternTy],
    pub result: CanonicalExternTy,
}

// `FnSig` omits an inherent method's receiver; `signature_key` provides that
// exact `bytes` receiver identity, so these are the remaining source args.
const EMPTY: &[CanonicalExternTy] = &[];
const U8: &[CanonicalExternTy] = &[CanonicalExternTy::U8];
const I64: &[CanonicalExternTy] = &[CanonicalExternTy::I64];
const I64_U8: &[CanonicalExternTy] = &[CanonicalExternTy::I64, CanonicalExternTy::U8];
const BYTES: &[CanonicalExternTy] = &[CanonicalExternTy::Bytes];
const STRING: &[CanonicalExternTy] = &[CanonicalExternTy::String];
const INSTANT: &[CanonicalExternTy] = &[CanonicalExternTy::Instant];

/// Complete source-declaration authority for compiler-lowered stdlib extern
/// bridges. A new method fails closed until it is added here with an exact Hew
/// signature, trusted source module, and admitted runtime family.
const CANONICAL_STD_IO_EXTERN_SIGNATURES: &[CanonicalStdlibExternSignature] = &[
    // The `impl duration` / `impl instant` methods in `std/builtins.hew`. Both
    // receivers are i64-backed scalars, so every one of these is a bit-copied
    // scalar operation with no ownership consequence.
    CanonicalStdlibExternSignature {
        module: "std.builtins",
        signature_key: "duration::nanos",
        symbol: "hew_duration_nanos",
        family: Some(RuntimeCallFamily::DurationNanos),
        params: EMPTY,
        result: CanonicalExternTy::I64,
    },
    CanonicalStdlibExternSignature {
        module: "std.builtins",
        signature_key: "duration::micros",
        symbol: "hew_duration_micros",
        family: Some(RuntimeCallFamily::DurationMicros),
        params: EMPTY,
        result: CanonicalExternTy::I64,
    },
    CanonicalStdlibExternSignature {
        module: "std.builtins",
        signature_key: "duration::millis",
        symbol: "hew_duration_millis",
        family: Some(RuntimeCallFamily::DurationMillis),
        params: EMPTY,
        result: CanonicalExternTy::I64,
    },
    CanonicalStdlibExternSignature {
        module: "std.builtins",
        signature_key: "duration::secs",
        symbol: "hew_duration_secs",
        family: Some(RuntimeCallFamily::DurationSecs),
        params: EMPTY,
        result: CanonicalExternTy::I64,
    },
    CanonicalStdlibExternSignature {
        module: "std.builtins",
        signature_key: "duration::mins",
        symbol: "hew_duration_mins",
        family: Some(RuntimeCallFamily::DurationMins),
        params: EMPTY,
        result: CanonicalExternTy::I64,
    },
    CanonicalStdlibExternSignature {
        module: "std.builtins",
        signature_key: "duration::hours",
        symbol: "hew_duration_hours",
        family: Some(RuntimeCallFamily::DurationHours),
        params: EMPTY,
        result: CanonicalExternTy::I64,
    },
    CanonicalStdlibExternSignature {
        module: "std.builtins",
        signature_key: "duration::abs",
        symbol: "hew_duration_abs",
        family: Some(RuntimeCallFamily::DurationAbs),
        params: EMPTY,
        result: CanonicalExternTy::Duration,
    },
    CanonicalStdlibExternSignature {
        module: "std.builtins",
        signature_key: "duration::is_zero",
        symbol: "hew_duration_is_zero",
        family: Some(RuntimeCallFamily::DurationIsZero),
        params: EMPTY,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.builtins",
        signature_key: "instant::elapsed",
        symbol: "hew_instant_elapsed",
        family: Some(RuntimeCallFamily::InstantElapsed),
        params: EMPTY,
        result: CanonicalExternTy::Duration,
    },
    CanonicalStdlibExternSignature {
        module: "std.builtins",
        signature_key: "instant::duration_since",
        symbol: "hew_instant_duration_since",
        family: Some(RuntimeCallFamily::InstantDurationSince),
        params: INSTANT,
        result: CanonicalExternTy::Duration,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::append",
        symbol: "hew_bytes_append",
        family: Some(RuntimeCallFamily::BytesAppend),
        params: BYTES,
        result: CanonicalExternTy::Unit,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::clear",
        symbol: "hew_bytes_clear",
        family: Some(RuntimeCallFamily::BytesClear),
        params: EMPTY,
        result: CanonicalExternTy::Unit,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::contains",
        symbol: "hew_bytes_contains",
        family: Some(RuntimeCallFamily::BytesContains),
        params: U8,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::get",
        symbol: "hew_bytes_get",
        family: Some(RuntimeCallFamily::BytesGet),
        params: I64,
        result: CanonicalExternTy::OptionU8,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::is_empty",
        symbol: "hew_bytes_is_empty",
        family: Some(RuntimeCallFamily::BytesIsEmpty),
        params: EMPTY,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::len",
        symbol: "hew_bytes_len",
        family: Some(RuntimeCallFamily::BytesLen),
        params: EMPTY,
        result: CanonicalExternTy::I64,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::pop",
        symbol: "hew_bytes_pop",
        family: Some(RuntimeCallFamily::BytesPop),
        params: EMPTY,
        result: CanonicalExternTy::OptionU8,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::push",
        symbol: "hew_bytes_push",
        family: Some(RuntimeCallFamily::BytesPush),
        params: U8,
        result: CanonicalExternTy::Unit,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::set",
        symbol: "hew_bytes_set",
        family: Some(RuntimeCallFamily::BytesSet),
        params: I64_U8,
        result: CanonicalExternTy::Unit,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::byte_len",
        symbol: "hew_string_byte_length",
        family: Some(RuntimeCallFamily::StringByteLen),
        params: EMPTY,
        result: CanonicalExternTy::I64,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::contains",
        symbol: "hew_string_contains",
        family: Some(RuntimeCallFamily::StringContains),
        params: STRING,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::starts_with",
        symbol: "hew_string_starts_with",
        family: Some(RuntimeCallFamily::StringStartsWith),
        params: STRING,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::ends_with",
        symbol: "hew_string_ends_with",
        family: Some(RuntimeCallFamily::StringEndsWith),
        params: STRING,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::is_empty",
        symbol: "hew_string_is_empty",
        family: Some(RuntimeCallFamily::StringIsEmpty),
        params: EMPTY,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::is_digit",
        symbol: "hew_string_is_digit",
        family: Some(RuntimeCallFamily::StringIsDigit),
        params: EMPTY,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::is_alpha",
        symbol: "hew_string_is_alpha",
        family: Some(RuntimeCallFamily::StringIsAlpha),
        params: EMPTY,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::is_alphanumeric",
        symbol: "hew_string_is_alphanumeric",
        family: Some(RuntimeCallFamily::StringIsAlphanumeric),
        params: EMPTY,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::find",
        symbol: "hew_string_find",
        family: Some(RuntimeCallFamily::StringFind),
        params: STRING,
        result: CanonicalExternTy::OptionI64,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::char_at",
        symbol: "hew_string_char_at",
        family: Some(RuntimeCallFamily::StringCharAt),
        params: I64,
        result: CanonicalExternTy::OptionChar,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::get",
        symbol: "hew_string_get",
        family: Some(RuntimeCallFamily::StringGet),
        params: I64,
        result: CanonicalExternTy::OptionChar,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::codepoint_at_utf8",
        symbol: "hew_string_char_at_utf8",
        family: Some(RuntimeCallFamily::StringCharAtUtf8),
        params: I64,
        result: CanonicalExternTy::OptionI64,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::len",
        symbol: "hew_string_length",
        family: Some(RuntimeCallFamily::StringLen),
        params: EMPTY,
        result: CanonicalExternTy::I64,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::to_bytes",
        symbol: "hew_string_to_bytes",
        family: Some(RuntimeCallFamily::StringToBytes),
        params: EMPTY,
        result: CanonicalExternTy::Bytes,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::to_upper",
        symbol: "hew_string_to_uppercase",
        family: Some(RuntimeCallFamily::StringToUppercase),
        params: EMPTY,
        result: CanonicalExternTy::String,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::slice",
        symbol: "hew_string_slice",
        family: Some(RuntimeCallFamily::StringSlice),
        params: &[CanonicalExternTy::I64, CanonicalExternTy::I64],
        result: CanonicalExternTy::String,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::to_lower",
        symbol: "hew_string_to_lowercase",
        family: Some(RuntimeCallFamily::StringToLowercase),
        params: EMPTY,
        result: CanonicalExternTy::String,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::repeat",
        symbol: "hew_string_repeat",
        family: Some(RuntimeCallFamily::StringRepeat),
        params: &[CanonicalExternTy::I64],
        result: CanonicalExternTy::String,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::split",
        symbol: "hew_string_split",
        family: Some(RuntimeCallFamily::StringSplit),
        params: &[CanonicalExternTy::String],
        result: CanonicalExternTy::VecString,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::lines",
        symbol: "hew_string_lines",
        family: Some(RuntimeCallFamily::StringLines),
        params: EMPTY,
        result: CanonicalExternTy::VecString,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::chars",
        symbol: "hew_string_chars",
        family: Some(RuntimeCallFamily::StringChars),
        params: EMPTY,
        result: CanonicalExternTy::VecChar,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::replace",
        symbol: "hew_string_replace",
        family: Some(RuntimeCallFamily::StringReplace),
        params: &[CanonicalExternTy::String, CanonicalExternTy::String],
        result: CanonicalExternTy::String,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::clone",
        symbol: "hew_string_clone",
        family: Some(RuntimeCallFamily::StringClone),
        params: EMPTY,
        result: CanonicalExternTy::String,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::trim",
        symbol: "hew_string_trim",
        family: Some(RuntimeCallFamily::StringTrim),
        params: EMPTY,
        result: CanonicalExternTy::String,
    },
];

/// Return a canonical stdlib extern declaration when its source identity,
/// endpoint, parameter sequence, and result type all agree.
#[must_use]
pub fn canonical_std_io_extern_signature(
    signature_key: &str,
    symbol: &str,
    params: &[crate::Ty],
    result: &crate::Ty,
) -> Option<&'static CanonicalStdlibExternSignature> {
    CANONICAL_STD_IO_EXTERN_SIGNATURES.iter().find(|entry| {
        let params_match = entry.params.len() == params.len()
            && entry
                .params
                .iter()
                .zip(params)
                .all(|(expected, actual)| expected.matches(actual));
        // Most method signatures have their receiver removed during
        // registration. Directly checked shipped stdlib roots retain the
        // explicit first receiver parameter in a few registration paths; the
        // declaration remains exact after accounting for that source form.
        let explicit_receiver = entry
            .signature_key
            .split_once("::")
            .and_then(|(receiver, _)| match receiver {
                "bytes" => Some(CanonicalExternTy::Bytes),
                "string" => Some(CanonicalExternTy::String),
                _ => None,
            });
        let params_with_receiver_match = explicit_receiver.is_some_and(|receiver| {
            params.len() == entry.params.len() + 1
                && receiver.matches(&params[0])
                && entry
                    .params
                    .iter()
                    .zip(&params[1..])
                    .all(|(expected, actual)| expected.matches(actual))
        });
        entry.signature_key == signature_key
            && entry.symbol == symbol
            && (params_match || params_with_receiver_match)
            && entry.result.matches(result)
    })
}

#[must_use]
pub const fn canonical_std_io_extern_signatures() -> &'static [CanonicalStdlibExternSignature] {
    CANONICAL_STD_IO_EXTERN_SIGNATURES
}

impl RuntimeCallFamily {
    #[must_use]
    pub const fn encoding_format(self) -> Option<EncodingFormat> {
        match self {
            Self::Encoding { format, .. } => Some(format),
            _ => None,
        }
    }

    /// Admit an encoding extern only at its exact declaring identity and ABI.
    /// The caller must additionally prove the module is the shipped source.
    /// A C-void mutation retains its semantic updated-owner result; only the
    /// child is consumed by the source ABI, while SIR also moves the receiver.
    #[must_use]
    pub fn matches_encoding_extern(
        self,
        module: &str,
        declaration: &str,
        symbol: &str,
        params: &[ResolvedTy],
        result: &ResolvedTy,
        consuming: &[bool],
    ) -> bool {
        let Some(format) = self.encoding_format() else {
            return false;
        };
        if module != format.module()
            || symbol != self.c_symbol()
            || declaration != format!("{module}.{symbol}")
        {
            return false;
        }
        let Some(contract) = self.semantic_contract() else {
            return false;
        };
        let updates_receiver = matches!(contract.result, RuntimeResultEffect::UpdatedReceiver(_));
        if consuming.len() != contract.arguments.len()
            || !consuming.iter().zip(contract.arguments).enumerate().all(
                |(index, (actual, argument))| {
                    *actual
                        == (argument.effect == RuntimeArgumentEffect::Move
                            && !(updates_receiver && index == 0))
                },
            )
        {
            return false;
        }
        if updates_receiver {
            *result == ResolvedTy::Unit
                && params
                    .first()
                    .is_some_and(|receiver| contract.matches_signature(params, receiver))
        } else {
            contract.matches_signature(params, result)
        }
    }

    /// Canonical checker signature for compiler-registered builtins whose
    /// source identity differs from their runtime ABI symbol.
    #[must_use]
    pub const fn checker_signature_key(self) -> Option<&'static str> {
        match self {
            Self::RcNew => Some("Rc::new"),
            _ => None,
        }
    }

    /// Resolve a checker-selected builtin signature to its typed identity.
    /// Dotted and namespaced syntax have already converged on this key.
    #[must_use]
    pub fn from_checker_signature(signature_key: &str) -> Option<Self> {
        if signature_key == Self::RcNew.checker_signature_key()? {
            return Some(Self::RcNew);
        }
        Self::from_c_symbol(signature_key)
    }

    /// Exact source declaration allowed to publish this floor operation.
    /// Import aliases retain this owner; a same-named function cannot claim it.
    #[must_use]
    pub fn source_intrinsic_declaration(self) -> Option<&'static str> {
        match self {
            Self::ChannelPairSender => Some("std.channel.pair_sender"),
            Self::ChannelPairReceiver => Some("std.channel.pair_receiver"),
            Self::BytesDecodeUtf8 => Some("std.encoding.utf8.decode"),
            Self::BytesDecodeUtf8Lossy => Some("std.encoding.utf8.decode_lossy"),
            _ => None,
        }
    }

    /// Generic parameters admitted by a source-owned floor operation.
    #[must_use]
    pub const fn source_intrinsic_type_params(self) -> &'static [&'static str] {
        match self {
            Self::ChannelPairSender | Self::ChannelPairReceiver => &["T"],
            _ => &[],
        }
    }

    /// Resolve an exact compiler-owned stdlib catalogue endpoint to its
    /// runtime family. Catalogue endpoint identity is established before this
    /// call; arbitrary source names never reach it.
    #[must_use]
    pub fn from_catalog_endpoint(endpoint: &str) -> Option<Self> {
        match endpoint {
            "channel.pair_sender" => Some(Self::ChannelPairSender),
            "channel.pair_receiver" => Some(Self::ChannelPairReceiver),
            "println_i32" => Some(Self::Print {
                kind: PrintKind::I32,
                newline: true,
            }),
            "println_i64" => Some(Self::Print {
                kind: PrintKind::I64,
                newline: true,
            }),
            "println_u8" => Some(Self::Print {
                kind: PrintKind::U8,
                newline: true,
            }),
            "println_u32" => Some(Self::Print {
                kind: PrintKind::U32,
                newline: true,
            }),
            "println_u64" => Some(Self::Print {
                kind: PrintKind::U64,
                newline: true,
            }),
            "println_f64" => Some(Self::Print {
                kind: PrintKind::F64,
                newline: true,
            }),
            "println_bool" => Some(Self::Print {
                kind: PrintKind::Bool,
                newline: true,
            }),
            "println_str" => Some(Self::Print {
                kind: PrintKind::Str,
                newline: true,
            }),
            "print_i32" => Some(Self::Print {
                kind: PrintKind::I32,
                newline: false,
            }),
            "print_i64" => Some(Self::Print {
                kind: PrintKind::I64,
                newline: false,
            }),
            "print_u8" => Some(Self::Print {
                kind: PrintKind::U8,
                newline: false,
            }),
            "print_u32" => Some(Self::Print {
                kind: PrintKind::U32,
                newline: false,
            }),
            "print_u64" => Some(Self::Print {
                kind: PrintKind::U64,
                newline: false,
            }),
            "print_f64" => Some(Self::Print {
                kind: PrintKind::F64,
                newline: false,
            }),
            "print_bool" => Some(Self::Print {
                kind: PrintKind::Bool,
                newline: false,
            }),
            "print_str" => Some(Self::Print {
                kind: PrintKind::Str,
                newline: false,
            }),
            "to_string_u8" => Some(Self::U8ToString),
            "to_string_i32" => Some(Self::I32ToString),
            "to_string_i64" => Some(Self::I64ToString),
            "to_string_u32" => Some(Self::U32ToString),
            "to_string_u64" => Some(Self::U64ToString),
            "to_string_f64" => Some(Self::F64ToString),
            "to_string_char" => Some(Self::CharToString),
            "to_string_bool" => Some(Self::BoolToString),
            "string_concat" => Some(Self::StringConcat),
            "bytes::new" => Some(Self::BytesNew),
            "exit" => Some(Self::ProcessExit),
            "utf8.decode" => Some(Self::BytesDecodeUtf8),
            "utf8.decode_lossy" => Some(Self::BytesDecodeUtf8Lossy),
            "Node::start" => Some(Self::NodeStart),
            "Node::connect" => Some(Self::NodeConnect),
            "Node::register" => Some(Self::NodeRegister),
            "Node::lookup" => Some(Self::NodeLookup),
            "Node::shutdown" => Some(Self::NodeShutdown),
            "Node::identity_key" => Some(Self::NodeIdentityKey),
            "Node::id" => Some(Self::NodeId),
            _ => None,
        }
    }

    /// The one row describing this runtime operation.
    #[must_use]
    #[expect(
        clippy::too_many_lines,
        clippy::match_same_arms,
        reason = "one declarative row per runtime operation is the authority, and \
                  two operations that happen to agree today still state their own facts"
    )]
    pub const fn row(self) -> RuntimeOpRow {
        use RuntimeArgumentContract as A;
        use RuntimeArgumentEffect as E;
        use RuntimeResultEffect as R;
        use RuntimeValueKind as K;
        match self {
            // The async IO, file and TCP operation sets and the encoding
            // matrix each carry their row in their own operation enum.
            Self::AsyncIo(op) => RuntimeOpRow {
                symbol: op.c_symbol(),
                contract: Some(op.contract()),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                c_return: RuntimeCReturn::Storage,
                physical: RuntimePhysicalForm::NotAnAction,
            },
            Self::FileRead(op) => RuntimeOpRow {
                symbol: op.c_symbol(),
                contract: Some(op.contract()),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                c_return: match op {
                    FileReadOp::IsValid | FileReadOp::StreamIsValid => RuntimeCReturn::TruthI32,
                    FileReadOp::HasError => RuntimeCReturn::TruthBool,
                    _ => RuntimeCReturn::Storage,
                },
                physical: RuntimePhysicalForm::Direct,
            },
            Self::Tcp(op) => RuntimeOpRow {
                symbol: op.c_symbol(),
                contract: Some(op.contract()),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                c_return: RuntimeCReturn::Storage,
                physical: RuntimePhysicalForm::Direct,
            },
            Self::Encoding { format, op } => RuntimeOpRow {
                symbol: op.c_symbol(format),
                contract: Some(op.contract(format)),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                c_return: RuntimeCReturn::Storage,
                physical: RuntimePhysicalForm::Direct,
            },
            Self::SupervisorPool(op) => RuntimeOpRow {
                symbol: op.symbol(),
                contract: Some(op.contract()),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                // Only `get` builds an `Option<ChildRef<T>>` descriptor; the
                // trapping and awaiting forms hand back the member itself.
                physical: match op {
                    SupervisorPoolOp::Get => RuntimePhysicalForm::VariantResult,
                    SupervisorPoolOp::Member | SupervisorPoolOp::AwaitRestartMember => {
                        RuntimePhysicalForm::Direct
                    }
                },
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorAsk => RuntimeOpRow {
                symbol: "hew_actor_ask",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorAskWithChannel => RuntimeOpRow {
                symbol: "hew_actor_ask_with_channel",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorCooperate => RuntimeOpRow {
                symbol: "hew_actor_cooperate",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorDemonitor => RuntimeOpRow {
                symbol: "hew_actor_demonitor",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorGenSinkComplete => RuntimeOpRow {
                symbol: "hew_actor_gen_sink_complete",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorGenSinkRegister => RuntimeOpRow {
                symbol: "hew_actor_gen_sink_register",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorLink => RuntimeOpRow {
                symbol: "hew_actor_link",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::LinkRemote => RuntimeOpRow {
                symbol: "hew_node_link_remote_location",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorMonitor => RuntimeOpRow {
                symbol: "hew_actor_monitor",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorSelf => RuntimeOpRow {
                symbol: "hew_actor_self",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorSendById => RuntimeOpRow {
                symbol: "hew_actor_send_by_id",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorSpawn => RuntimeOpRow {
                symbol: "hew_actor_spawn",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorUnlink => RuntimeOpRow {
                symbol: "hew_actor_unlink",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::AutoMutexAlloc => RuntimeOpRow {
                symbol: "hew_auto_mutex_alloc",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::AutoMutexFree => RuntimeOpRow {
                symbol: "hew_auto_mutex_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::AutoMutexLock => RuntimeOpRow {
                symbol: "hew_auto_mutex_lock",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::AutoMutexUnlock => RuntimeOpRow {
                symbol: "hew_auto_mutex_unlock",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesAppend => RuntimeOpRow {
                symbol: "hew_bytes_append",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Move,
                        },
                        A {
                            ty: K::Bytes,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Bytes),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesClear => RuntimeOpRow {
                symbol: "hew_bytes_clear",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bytes,
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiver(K::Bytes),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesContains => RuntimeOpRow {
                symbol: "hew_bytes_contains",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesDecodeUtf8 => RuntimeOpRow {
                symbol: "hew_bytes_decode_utf8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bytes,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwnedVariant(RuntimeVariantResultKind::Utf8Decode),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Utf8Decode,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesDecodeUtf8Lossy => RuntimeOpRow {
                symbol: "hew_bytes_decode_utf8_lossy",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bytes,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesGet => RuntimeOpRow {
                symbol: "hew_bytes_get",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesIndex => RuntimeOpRow {
                symbol: "hew_bytes_index",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesIsEmpty => RuntimeOpRow {
                symbol: "hew_bytes_is_empty",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bytes,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesLen => RuntimeOpRow {
                symbol: "hew_bytes_len",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bytes,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesPop => RuntimeOpRow {
                symbol: "hew_bytes_pop",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bytes,
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Bytes,
                        K::Applied(BuiltinType::Option, &[K::U8]),
                    ])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::PairWithOption,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesPush => RuntimeOpRow {
                symbol: "hew_bytes_push",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Move,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Bytes),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesSet => RuntimeOpRow {
                symbol: "hew_bytes_set",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Move,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Bytes),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesSlice => RuntimeOpRow {
                symbol: "hew_bytes_slice",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::Bytes),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesSliceFrom => RuntimeOpRow {
                symbol: "hew_bytes_slice_from",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::Bytes),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesNew => RuntimeOpRow {
                symbol: "bytes::new",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::FreshOwned(K::Bytes),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::BytesConstructor,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::CancelTokenIsRequested => RuntimeOpRow {
                symbol: "hew_cancel_token_is_requested",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::CancelTokenRelease => RuntimeOpRow {
                symbol: "hew_cancel_token_release",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::CancelTokenRetain => RuntimeOpRow {
                symbol: "hew_cancel_token_retain",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ChannelRecvLayout => RuntimeOpRow {
                symbol: "hew_channel_recv_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ChannelSendLayout => RuntimeOpRow {
                symbol: "hew_channel_send_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ChannelTryRecvLayout => RuntimeOpRow {
                symbol: "hew_channel_try_recv_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ChannelSenderClone => RuntimeOpRow {
                symbol: "hew_channel_sender_clone",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::ChannelHalf(ChannelHalfKind::Sender),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::ChannelHalf(ChannelHalfKind::Sender)),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ChannelSenderClose => RuntimeOpRow {
                symbol: "hew_channel_sender_close",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::ChannelHalf(ChannelHalfKind::Sender),
                        effect: E::Move,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ChannelReceiverClose => RuntimeOpRow {
                symbol: "hew_channel_receiver_close",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::ChannelHalf(ChannelHalfKind::Receiver),
                        effect: E::Move,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ChannelPairNew => RuntimeOpRow {
                symbol: "hew_channel_new",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::ChannelPair),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ChannelPairFree => RuntimeOpRow {
                symbol: "hew_channel_pair_free",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::ChannelPair,
                        effect: E::Move,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorRequestRelease => RuntimeOpRow {
                symbol: "hew_msg_envelope_release",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::ActorRequestOwner,
                        effect: E::Move,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorCallFree => RuntimeOpRow {
                symbol: "hew_actor_call_free",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::ActorCall),
                        effect: E::Move,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorRequestTake => RuntimeOpRow {
                symbol: "hew_actor_ask_wait_take_request",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::ActorRequestAdmission,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::ActorRequestOwner),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ChannelPairIsValid => RuntimeOpRow {
                symbol: "hew_channel_pair_is_valid",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::ChannelPair,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ChannelPairSender => RuntimeOpRow {
                symbol: "hew_channel_pair_sender",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::ChannelPair,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::ChannelHalfResult(ChannelHalfKind::Sender)),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ChannelPairReceiver => RuntimeOpRow {
                symbol: "hew_channel_pair_receiver",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::ChannelPair,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::ChannelHalfResult(ChannelHalfKind::Receiver)),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DuplexClone => RuntimeOpRow {
                symbol: "hew_duplex_clone",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DuplexClose => RuntimeOpRow {
                symbol: "hew_duplex_close",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DuplexCloseHalf => RuntimeOpRow {
                symbol: "hew_duplex_close_half",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DuplexPair => RuntimeOpRow {
                symbol: "hew_duplex_pair",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DuplexPayloadFree => RuntimeOpRow {
                symbol: "hew_duplex_payload_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DuplexRecv => RuntimeOpRow {
                symbol: "hew_duplex_recv",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DuplexRecvHalf => RuntimeOpRow {
                symbol: "hew_duplex_recv_half",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DuplexSend => RuntimeOpRow {
                symbol: "hew_duplex_send",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DuplexSendHalf => RuntimeOpRow {
                symbol: "hew_duplex_send_half",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DuplexTryRecv => RuntimeOpRow {
                symbol: "hew_duplex_try_recv",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DuplexTrySend => RuntimeOpRow {
                symbol: "hew_duplex_try_send",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DurationAbs => RuntimeOpRow {
                symbol: "hew_duration_abs",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Duration,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Duration),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DurationHours => RuntimeOpRow {
                symbol: "hew_duration_hours",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Duration,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DurationIsZero => RuntimeOpRow {
                symbol: "hew_duration_is_zero",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Duration,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DurationMicros => RuntimeOpRow {
                symbol: "hew_duration_micros",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Duration,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DurationMillis => RuntimeOpRow {
                symbol: "hew_duration_millis",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Duration,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DurationMins => RuntimeOpRow {
                symbol: "hew_duration_mins",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Duration,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DurationNanos => RuntimeOpRow {
                symbol: "hew_duration_nanos",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Duration,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DurationSecs => RuntimeOpRow {
                symbol: "hew_duration_secs",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Duration,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DynBoxAlloc => RuntimeOpRow {
                symbol: "hew_dyn_box_alloc",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DynBoxFree => RuntimeOpRow {
                symbol: "hew_dyn_box_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapContainsKeyLayout => RuntimeOpRow {
                symbol: "hew_hashmap_contains_key_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapClearLayout => RuntimeOpRow {
                symbol: "hew_hashmap_clear_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapCloneLayout => RuntimeOpRow {
                symbol: "hew_hashmap_clone_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapEntriesLayout => RuntimeOpRow {
                symbol: "hew_hashmap_entries_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapFreeLayout => RuntimeOpRow {
                symbol: "hew_hashmap_free_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapGetLayout => RuntimeOpRow {
                symbol: "hew_hashmap_get_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashMapLayoutGet,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapInsertLayout => RuntimeOpRow {
                symbol: "hew_hashmap_insert_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapKeysLayout => RuntimeOpRow {
                symbol: "hew_hashmap_keys_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapLenLayout => RuntimeOpRow {
                symbol: "hew_hashmap_len_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapNew => RuntimeOpRow {
                symbol: "HashMap::new",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionConstructor,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapNewWithLayout => RuntimeOpRow {
                symbol: "hew_hashmap_new_with_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionConstructor,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapRemoveLayout => RuntimeOpRow {
                symbol: "hew_hashmap_remove_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapValuesLayout => RuntimeOpRow {
                symbol: "hew_hashmap_values_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetContainsLayout => RuntimeOpRow {
                symbol: "hew_hashset_contains_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetClearLayout => RuntimeOpRow {
                symbol: "hew_hashset_clear_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetCloneLayout => RuntimeOpRow {
                symbol: "hew_hashset_clone_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetFreeLayout => RuntimeOpRow {
                symbol: "hew_hashset_free_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetInsertLayout => RuntimeOpRow {
                symbol: "hew_hashset_insert_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetIsEmptyLayout => RuntimeOpRow {
                symbol: "hew_hashset_is_empty_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetLenLayout => RuntimeOpRow {
                symbol: "hew_hashset_len_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetNew => RuntimeOpRow {
                symbol: "HashSet::new",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionConstructor,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetNewWithLayout => RuntimeOpRow {
                symbol: "hew_hashset_new_with_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionConstructor,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetRemoveLayout => RuntimeOpRow {
                symbol: "hew_hashset_remove_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetToVecLayout => RuntimeOpRow {
                symbol: "hew_hashset_to_vec_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::InstantDurationSince => RuntimeOpRow {
                symbol: "hew_instant_duration_since",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Duration),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::InstantElapsed => RuntimeOpRow {
                symbol: "hew_instant_elapsed",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Duration),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::InstantNow => RuntimeOpRow {
                symbol: "hew_instant_now",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Sqrt) => RuntimeOpRow {
                symbol: "sqrt",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Exp) => RuntimeOpRow {
                symbol: "exp",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Log) => RuntimeOpRow {
                symbol: "log",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Sin) => RuntimeOpRow {
                symbol: "sin",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Cos) => RuntimeOpRow {
                symbol: "cos",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::AbsI64) => RuntimeOpRow {
                symbol: "abs",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[RuntimeLogicalFailure::IntegerOverflow],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::MinI64) => RuntimeOpRow {
                symbol: "min",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::MaxI64) => RuntimeOpRow {
                symbol: "max",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::AbsF64) => RuntimeOpRow {
                symbol: "abs_f",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::MinF64) => RuntimeOpRow {
                symbol: "min_f",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::MaxF64) => RuntimeOpRow {
                symbol: "max_f",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Pow) => RuntimeOpRow {
                symbol: "pow",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Floor) => RuntimeOpRow {
                symbol: "floor",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Ceil) => RuntimeOpRow {
                symbol: "ceil",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Round) => RuntimeOpRow {
                symbol: "round",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Tan) => RuntimeOpRow {
                symbol: "tan",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Asin) => RuntimeOpRow {
                symbol: "asin",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Acos) => RuntimeOpRow {
                symbol: "acos",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Atan) => RuntimeOpRow {
                symbol: "atan",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Atan2) => RuntimeOpRow {
                symbol: "atan2",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Sinh) => RuntimeOpRow {
                symbol: "sinh",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Cosh) => RuntimeOpRow {
                symbol: "cosh",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Tanh) => RuntimeOpRow {
                symbol: "tanh",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Exp2) => RuntimeOpRow {
                symbol: "exp2",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Log2) => RuntimeOpRow {
                symbol: "log2",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Log10) => RuntimeOpRow {
                symbol: "log10",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Log1p) => RuntimeOpRow {
                symbol: "log1p",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Expm1) => RuntimeOpRow {
                symbol: "expm1",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Cbrt) => RuntimeOpRow {
                symbol: "cbrt",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Hypot) => RuntimeOpRow {
                symbol: "hypot",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Fma) => RuntimeOpRow {
                symbol: "fma",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Trunc) => RuntimeOpRow {
                symbol: "trunc",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Copysign) => RuntimeOpRow {
                symbol: "copysign",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Powi) => RuntimeOpRow {
                symbol: "powi",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::FromBits) => RuntimeOpRow {
                symbol: "from_bits",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::FloatMethod(FloatMethodOp::ToBits) => RuntimeOpRow {
                symbol: "to_bits",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::FloatMethod(FloatMethodOp::IsNan) => RuntimeOpRow {
                symbol: "is_nan",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::FloatMethod(FloatMethodOp::IsFinite) => RuntimeOpRow {
                symbol: "is_finite",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::FloatMethod(FloatMethodOp::IsInfinite) => RuntimeOpRow {
                symbol: "is_infinite",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::FloatMethod(FloatMethodOp::IsSignNegative) => RuntimeOpRow {
                symbol: "is_sign_negative",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "count_ones.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "count_ones.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "count_ones.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "count_ones.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "count_ones.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Isize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "count_ones.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "count_ones.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "count_ones.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "count_ones.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "count_ones.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Usize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "count_zeros.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "count_zeros.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "count_zeros.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "count_zeros.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "count_zeros.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Isize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "count_zeros.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "count_zeros.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "count_zeros.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "count_zeros.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "count_zeros.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Usize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "leading_zeros.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "leading_zeros.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "leading_zeros.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "leading_zeros.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "leading_zeros.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Isize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "leading_zeros.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "leading_zeros.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "leading_zeros.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "leading_zeros.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "leading_zeros.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Usize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "trailing_zeros.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "trailing_zeros.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "trailing_zeros.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "trailing_zeros.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "trailing_zeros.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Isize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "trailing_zeros.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "trailing_zeros.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "trailing_zeros.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "trailing_zeros.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "trailing_zeros.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Usize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "swap_bytes.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "swap_bytes.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "swap_bytes.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "swap_bytes.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "swap_bytes.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Isize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "swap_bytes.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "swap_bytes.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "swap_bytes.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "swap_bytes.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "swap_bytes.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Usize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "reverse_bits.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "reverse_bits.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "reverse_bits.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "reverse_bits.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "reverse_bits.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Isize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "reverse_bits.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "reverse_bits.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "reverse_bits.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "reverse_bits.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "reverse_bits.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Usize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "rotate_left.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "rotate_left.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "rotate_left.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "rotate_left.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "rotate_left.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "rotate_left.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "rotate_left.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "rotate_left.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "rotate_left.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "rotate_left.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "rotate_right.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "rotate_right.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "rotate_right.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "rotate_right.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "rotate_right.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "rotate_right.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "rotate_right.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "rotate_right.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "rotate_right.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "rotate_right.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "wrapping_add.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "wrapping_add.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "wrapping_add.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "wrapping_add.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "wrapping_add.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "wrapping_add.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "wrapping_add.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "wrapping_add.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "wrapping_add.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "wrapping_add.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "wrapping_sub.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "wrapping_sub.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "wrapping_sub.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "wrapping_sub.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "wrapping_sub.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "wrapping_sub.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "wrapping_sub.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "wrapping_sub.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "wrapping_sub.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "wrapping_sub.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "wrapping_mul.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "wrapping_mul.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "wrapping_mul.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "wrapping_mul.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "wrapping_mul.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "wrapping_mul.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "wrapping_mul.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "wrapping_mul.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "wrapping_mul.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "wrapping_mul.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "saturating_add.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "saturating_add.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "saturating_add.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "saturating_add.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "saturating_add.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "saturating_add.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "saturating_add.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "saturating_add.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "saturating_add.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "saturating_add.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "saturating_sub.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "saturating_sub.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "saturating_sub.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "saturating_sub.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "saturating_sub.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "saturating_sub.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "saturating_sub.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "saturating_sub.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "saturating_sub.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "saturating_sub.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "saturating_mul.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "saturating_mul.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "saturating_mul.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "saturating_mul.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "saturating_mul.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "saturating_mul.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "saturating_mul.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "saturating_mul.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "saturating_mul.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "saturating_mul.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "checked_add.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "checked_add.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I16])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "checked_add.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I32])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "checked_add.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "checked_add.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Isize])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "checked_add.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "checked_add.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U16])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "checked_add.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U32])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "checked_add.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "checked_add.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Usize])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "checked_sub.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "checked_sub.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I16])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "checked_sub.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I32])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "checked_sub.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "checked_sub.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Isize])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "checked_sub.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "checked_sub.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U16])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "checked_sub.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U32])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "checked_sub.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "checked_sub.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Usize])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "checked_mul.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "checked_mul.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I16])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "checked_mul.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I32])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "checked_mul.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "checked_mul.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Isize])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "checked_mul.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "checked_mul.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U16])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "checked_mul.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U32])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "checked_mul.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "checked_mul.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Usize])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeAllowPeer => RuntimeOpRow {
                symbol: "Node::allow_peer",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeConnect => RuntimeOpRow {
                symbol: "Node::connect",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Result,
                        &[K::Unit, K::MonomorphicBuiltin(BuiltinType::NodeError)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NodeResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeId => RuntimeOpRow {
                symbol: "Node::id",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Option,
                        &[K::BuiltinNominal(BuiltinType::NodeId)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeIdentityKey => RuntimeOpRow {
                symbol: "Node::identity_key",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeLoadKeys => RuntimeOpRow {
                symbol: "Node::load_keys",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeLookup => RuntimeOpRow {
                symbol: "Node::lookup",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::NodeLookupResult),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NodeResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeMonitor => RuntimeOpRow {
                symbol: "hew_node_monitor_location",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeRegister => RuntimeOpRow {
                symbol: "Node::register",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::Receiver(BuiltinType::ActorHandle),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeSetTransport => RuntimeOpRow {
                symbol: "Node::set_transport",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeShutdown => RuntimeOpRow {
                symbol: "Node::shutdown",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeStart => RuntimeOpRow {
                symbol: "Node::start",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Named("std.builtins.NodeConfig"),
                        effect: E::Move,
                    }],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Result,
                        &[K::Unit, K::MonomorphicBuiltin(BuiltinType::NodeError)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NodeResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricCounterRegister => RuntimeOpRow {
                symbol: "hew_metric_counter_register",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricCounterInc => RuntimeOpRow {
                symbol: "hew_metric_counter_inc",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricCounterAdd => RuntimeOpRow {
                symbol: "hew_metric_counter_add",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricGaugeRegister => RuntimeOpRow {
                symbol: "hew_metric_gauge_register",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricGaugeSet => RuntimeOpRow {
                symbol: "hew_metric_gauge_set",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricGaugeInc => RuntimeOpRow {
                symbol: "hew_metric_gauge_inc",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricGaugeDec => RuntimeOpRow {
                symbol: "hew_metric_gauge_dec",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricGaugeAdd => RuntimeOpRow {
                symbol: "hew_metric_gauge_add",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricHistogramRegister => RuntimeOpRow {
                symbol: "hew_metric_histogram_register",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricHistogramRegisterSimple => RuntimeOpRow {
                symbol: "hew_metric_histogram_register_simple",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricHistogramRecord => RuntimeOpRow {
                symbol: "hew_metric_histogram_record",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricVecRegister => RuntimeOpRow {
                symbol: "hew_metric_vec_register",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricVecWith => RuntimeOpRow {
                symbol: "hew_metric_vec_with",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ObserveReadU64 => RuntimeOpRow {
                symbol: "hew_observe_read_u64",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ObserveScrape => RuntimeOpRow {
                symbol: "hew_observe_scrape",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ObserveSeries => RuntimeOpRow {
                symbol: "hew_observe_series",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ObserveBarrier => RuntimeOpRow {
                symbol: "hew_observe_barrier",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            // --- Rc/Weak ownership -------------------------------------
            // A strong handle is the payload pointer; a weak handle is the
            // allocation header pointer. `Rc.new` hands the runtime the
            // payload's release recipe, so every later release of the last
            // strong reference runs the payload's own destructor. `Rc.drop`
            // and `Weak.drop` are destroy actions rather than call sites:
            // physical MIR reaches their symbols through `DestroyAction`.
            Self::RcNew => RuntimeOpRow {
                symbol: "hew_rc_new",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::SharedPayload,
                        effect: E::Value,
                    }],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Rc)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::SharedHandle,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcClone => RuntimeOpRow {
                symbol: "hew_rc_clone",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Rc),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Rc)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcDowngrade => RuntimeOpRow {
                symbol: "hew_rc_downgrade",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Rc),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Applied(BuiltinType::Weak, &[K::SharedPayload])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcDrop => RuntimeOpRow {
                symbol: "hew_rc_drop",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcGet => RuntimeOpRow {
                symbol: "hew_rc_get",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Rc),
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::SharedPayload),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::SharedHandle,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcIsUnique => RuntimeOpRow {
                symbol: "hew_rc_is_unique",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Rc),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthI32,
            },
            Self::RcSet => RuntimeOpRow {
                symbol: "hew_rc_set",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        RuntimeArgumentContract {
                            ty: K::Receiver(BuiltinType::Rc),
                            effect: E::Borrow,
                        },
                        RuntimeArgumentContract {
                            ty: K::SharedPayload,
                            effect: E::Value,
                        },
                    ],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::SharedHandle,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcStrongCount => RuntimeOpRow {
                symbol: "hew_rc_strong_count",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Rc),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcWeakCount => RuntimeOpRow {
                symbol: "hew_rc_weak_count",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Rc),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::WeakCloneRc => RuntimeOpRow {
                symbol: "hew_weak_clone_rc",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Weak),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Weak)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::WeakDropRc => RuntimeOpRow {
                symbol: "hew_weak_drop_rc",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::WeakUpgradeRc => RuntimeOpRow {
                symbol: "hew_weak_upgrade_rc",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Weak),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Applied(
                        BuiltinType::Option,
                        &[K::Applied(BuiltinType::Rc, &[K::SharedPayload])],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RecvHalfRecv => RuntimeOpRow {
                symbol: "hew_recv_half_recv",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RecvHalfTryRecv => RuntimeOpRow {
                symbol: "hew_recv_half_try_recv",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RegexCapture => RuntimeOpRow {
                symbol: "hew_regex_capture",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RegexCompile => RuntimeOpRow {
                symbol: "hew_regex_compile",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RegexFreeCapture => RuntimeOpRow {
                symbol: "hew_regex_free_capture",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RegexHandle => RuntimeOpRow {
                symbol: "hew_regex_handle",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::NamedOpaque("std.text.regex.PatternHandle")),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RegexMatch => RuntimeOpRow {
                symbol: "hew_regex_match",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RemotePidSend => RuntimeOpRow {
                symbol: "hew_remote_pid_send",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ReplyChannelCancel => RuntimeOpRow {
                symbol: "hew_reply_channel_cancel",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ReplyChannelFree => RuntimeOpRow {
                symbol: "hew_reply_channel_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ReplyChannelNew => RuntimeOpRow {
                symbol: "hew_reply_channel_new",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ReplyPayloadFree => RuntimeOpRow {
                symbol: "hew_reply_payload_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ReplyWait => RuntimeOpRow {
                symbol: "hew_reply_wait",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SelectFirst => RuntimeOpRow {
                symbol: "hew_select_first",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SendHalfSend => RuntimeOpRow {
                symbol: "hew_send_half_send",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SendHalfTrySend => RuntimeOpRow {
                symbol: "hew_send_half_try_send",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SinkClose => RuntimeOpRow {
                symbol: "hew_sink_close",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Sink),
                        effect: E::Move,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SinkPeerClosed => RuntimeOpRow {
                symbol: "hew_sink_peer_closed",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SinkWrite(StreamElementKind::Bytes) => RuntimeOpRow {
                symbol: "hew_sink_write_bytes",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SinkWrite(StreamElementKind::String) => RuntimeOpRow {
                symbol: "hew_sink_write_string",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SinkTryWrite(StreamElementKind::Bytes) => RuntimeOpRow {
                symbol: "hew_sink_try_write_bytes",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SinkTryWrite(StreamElementKind::String) => RuntimeOpRow {
                symbol: "hew_sink_try_write_string",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamClose => RuntimeOpRow {
                symbol: "hew_stream_close",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Stream),
                        effect: E::Move,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamChunks => RuntimeOpRow {
                symbol: "hew_stream_chunks",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Stream),
                            effect: E::Move,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Stream)),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamLines => RuntimeOpRow {
                symbol: "hew_stream_lines",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Stream),
                        effect: E::Move,
                    }],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Stream)),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamTake => RuntimeOpRow {
                symbol: "hew_stream_take",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Stream),
                            effect: E::Move,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Stream)),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamNextLayout => RuntimeOpRow {
                symbol: "hew_stream_next_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamSendLayout => RuntimeOpRow {
                symbol: "hew_stream_send_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamTryNextLayout => RuntimeOpRow {
                symbol: "hew_stream_try_next_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringCharAt => RuntimeOpRow {
                symbol: "hew_string_char_at",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Char])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringCharAtUtf8 => RuntimeOpRow {
                symbol: "hew_string_char_at_utf8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringCharCount => RuntimeOpRow {
                symbol: "hew_string_char_count",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringByteLen => RuntimeOpRow {
                symbol: "hew_string_byte_length",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringConcat => RuntimeOpRow {
                symbol: "hew_string_concat",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringEquals => RuntimeOpRow {
                symbol: "hew_string_equals",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthI32,
            },
            Self::StringCompare => RuntimeOpRow {
                symbol: "hew_string_compare",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringStartsWith => RuntimeOpRow {
                symbol: "hew_string_starts_with",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StringEndsWith => RuntimeOpRow {
                symbol: "hew_string_ends_with",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StringContains => RuntimeOpRow {
                symbol: "hew_string_contains",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StringIsEmpty => RuntimeOpRow {
                symbol: "hew_string_is_empty",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StringIsDigit => RuntimeOpRow {
                symbol: "hew_string_is_digit",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StringIsAlpha => RuntimeOpRow {
                symbol: "hew_string_is_alpha",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StringIsAlphanumeric => RuntimeOpRow {
                symbol: "hew_string_is_alphanumeric",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StructuralFormat => RuntimeOpRow {
                symbol: "hew_structural_format",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringFind => RuntimeOpRow {
                symbol: "hew_string_find",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringGet => RuntimeOpRow {
                symbol: "hew_string_get",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringIndex => RuntimeOpRow {
                symbol: "hew_string_index",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Char),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringLen => RuntimeOpRow {
                symbol: "hew_string_length",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringRepeat => RuntimeOpRow {
                symbol: "hew_string_repeat",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringReplace => RuntimeOpRow {
                symbol: "hew_string_replace",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringClone => RuntimeOpRow {
                symbol: "hew_string_clone",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringSplit => RuntimeOpRow {
                symbol: "hew_string_split",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::FreshOwned(K::Applied(BuiltinType::Vec, &[K::String])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringLines => RuntimeOpRow {
                symbol: "hew_string_lines",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Applied(BuiltinType::Vec, &[K::String])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringChars => RuntimeOpRow {
                symbol: "hew_string_chars",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Applied(BuiltinType::Vec, &[K::Char])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringSliceCodepoints => RuntimeOpRow {
                symbol: "hew_string_slice_codepoints",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringSliceCodepointsFrom => RuntimeOpRow {
                symbol: "hew_string_slice_codepoints_from",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringSlice => RuntimeOpRow {
                symbol: "hew_string_slice",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringToLowercase => RuntimeOpRow {
                symbol: "hew_string_to_lowercase",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringToBytes => RuntimeOpRow {
                symbol: "hew_string_to_bytes",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Bytes),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringToUppercase => RuntimeOpRow {
                symbol: "hew_string_to_uppercase",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringTrim => RuntimeOpRow {
                symbol: "hew_string_trim",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::U8ToString => RuntimeOpRow {
                symbol: "hew_u8_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::I32ToString => RuntimeOpRow {
                symbol: "hew_int_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::I64ToString => RuntimeOpRow {
                symbol: "hew_i64_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::U32ToString => RuntimeOpRow {
                symbol: "hew_uint_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::U64ToString => RuntimeOpRow {
                symbol: "hew_u64_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::F64ToString => RuntimeOpRow {
                symbol: "hew_float_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::CharToString => RuntimeOpRow {
                symbol: "hew_char_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Char,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::I32,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::I32,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::I64,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::I64,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::U8,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::U8,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::U32,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::U32,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::U64,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::U64,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::F64,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::F64,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::Bool,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bool,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::Bool,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bool,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::Str,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::Str,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ProcessExit => RuntimeOpRow {
                symbol: "hew_exit",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::Never,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StderrWrite => RuntimeOpRow {
                symbol: "hew_io_write_err",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BoolToString => RuntimeOpRow {
                symbol: "hew_bool_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bool,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorDirectId => RuntimeOpRow {
                symbol: "hew_supervisor_direct_id",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorChildGet => RuntimeOpRow {
                symbol: "hew_supervisor_child_get",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::LocalPidSupervisorChildGet => RuntimeOpRow {
                symbol: "hew_local_pid_supervisor_child_get",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorNestedGet => RuntimeOpRow {
                symbol: "hew_supervisor_nested_get",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorPoolChildGet => RuntimeOpRow {
                symbol: "hew_supervisor_pool_child_get",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::LocalPidSupervisorPoolChildRefGet => RuntimeOpRow {
                symbol: "hew_local_pid_supervisor_pool_child_ref_get",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorPoolLen => RuntimeOpRow {
                symbol: "hew_supervisor_pool_len",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorStop => RuntimeOpRow {
                symbol: "hew_supervisor_stop",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorRestartAwaitBlocking => RuntimeOpRow {
                symbol: "hew_supervisor_restart_await_blocking",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TcpAttachLocal => RuntimeOpRow {
                symbol: "hew_tcp_attach_local",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TlsAttachLocal => RuntimeOpRow {
                symbol: "hew_tls_attach_local",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::WebSocketAttachLocal => RuntimeOpRow {
                symbol: "hew_ws_attach_local",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskAwaitBlocking => RuntimeOpRow {
                symbol: "hew_task_await_blocking",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskCompleteThreaded => RuntimeOpRow {
                symbol: "hew_task_complete_threaded",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskCompletionObserve => RuntimeOpRow {
                symbol: "hew_task_completion_observe",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskCompletionUnobserve => RuntimeOpRow {
                symbol: "hew_task_completion_unobserve",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskFree => RuntimeOpRow {
                symbol: "hew_task_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::GeneratorFree => RuntimeOpRow {
                symbol: "hew_checked_generator_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskGetEnv => RuntimeOpRow {
                symbol: "hew_task_get_env",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskGetError => RuntimeOpRow {
                symbol: "hew_task_get_error",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskGetResult => RuntimeOpRow {
                symbol: "hew_task_get_result",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskNew => RuntimeOpRow {
                symbol: "hew_task_new",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskScopeCancelAfterNs => RuntimeOpRow {
                symbol: "hew_task_scope_cancel_after_ns",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskScopeDestroy => RuntimeOpRow {
                symbol: "hew_task_scope_destroy",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskScopeJoinAll => RuntimeOpRow {
                symbol: "hew_task_scope_join_all",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskScopeNew => RuntimeOpRow {
                symbol: "hew_task_scope_new",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskScopeSetCurrent => RuntimeOpRow {
                symbol: "hew_task_scope_set_current",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskScopeSpawn => RuntimeOpRow {
                symbol: "hew_task_scope_spawn",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskSetEnv => RuntimeOpRow {
                symbol: "hew_task_set_env",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskSetResult => RuntimeOpRow {
                symbol: "hew_task_set_result",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TaskSpawnThread => RuntimeOpRow {
                symbol: "hew_task_spawn_thread",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::New) => RuntimeOpRow {
                symbol: "vec.value.new",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Vec)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Len) => RuntimeOpRow {
                symbol: "vec.value.len",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Vec),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Contains) => RuntimeOpRow {
                symbol: "vec.value.contains",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Index) => RuntimeOpRow {
                symbol: "vec.value.index",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::TypeArgument(0)),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Get) => RuntimeOpRow {
                symbol: "vec.value.get",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Option,
                        &[K::TypeArgument(0)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Push) => RuntimeOpRow {
                symbol: "vec.value.push",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Move,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Value,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::Vec)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Set) => RuntimeOpRow {
                symbol: "vec.value.set",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Move,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Value,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::Vec)),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Pop) => RuntimeOpRow {
                symbol: "vec.value.pop",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Vec),
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::Vec),
                        K::TypeArgument(0),
                    ])),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Remove) => RuntimeOpRow {
                symbol: "vec.value.remove",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Move,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::Vec),
                        K::TypeArgument(0),
                    ])),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Clear) => RuntimeOpRow {
                symbol: "vec.value.clear",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Vec),
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::Vec)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::IndexBorrow) => RuntimeOpRow {
                symbol: "vec.value.index_borrow",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::Borrowed(K::TypeArgument(0)),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::GetBorrow) => RuntimeOpRow {
                symbol: "vec.value.get_borrow",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::Borrowed(K::Applied(BuiltinType::Option, &[K::TypeArgument(0)])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::TakeFirst) => RuntimeOpRow {
                symbol: "vec.value.take_first",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Vec),
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::Vec),
                        K::TypeArgument(0),
                    ])),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Slice) => RuntimeOpRow {
                symbol: "vec.value.slice",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Receiver(BuiltinType::Vec)),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::SliceFrom) => RuntimeOpRow {
                symbol: "vec.value.slice_from",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Receiver(BuiltinType::Vec)),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Append) => RuntimeOpRow {
                symbol: "vec.value.append",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Move,
                        },
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::Vec)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Join) => RuntimeOpRow {
                symbol: "vec.value.join",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Array(ArrayValueOp::Len) => RuntimeOpRow {
                symbol: "array.value.len",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::FixedArray,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Array(ArrayValueOp::Index) => RuntimeOpRow {
                symbol: "array.value.index",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::FixedArray,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::ArrayElement),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Array(ArrayValueOp::IndexBorrow) => RuntimeOpRow {
                symbol: "array.value.index_borrow",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::FixedArray,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::Borrowed(K::ArrayElement),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Array(ArrayValueOp::Set) => RuntimeOpRow {
                symbol: "array.value.set",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::FixedArray,
                            effect: E::Move,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::ArrayElement,
                            effect: E::Value,
                        },
                    ],
                    result: R::UpdatedReceiver(K::FixedArray),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::New) => RuntimeOpRow {
                symbol: "map.value.new",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::FreshOwned(K::Receiver(BuiltinType::HashMap)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Len) => RuntimeOpRow {
                symbol: "map.value.len",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashMap),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Index) => RuntimeOpRow {
                symbol: "map.value.index",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashMap),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::IndependentValue(K::TypeArgument(1)),
                    failures: &[
                        RuntimeLogicalFailure::CallbackFault,
                        RuntimeLogicalFailure::IndexOutOfBounds,
                    ],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Get) => RuntimeOpRow {
                symbol: "map.value.get",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashMap),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Option,
                        &[K::TypeArgument(1)],
                    )),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::GetBorrow) => RuntimeOpRow {
                symbol: "map.value.get_borrow",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashMap),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::Borrowed(K::Applied(BuiltinType::Option, &[K::TypeArgument(1)])),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::ContainsKey) => RuntimeOpRow {
                symbol: "map.value.contains_key",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashMap),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Insert) => RuntimeOpRow {
                symbol: "map.value.insert",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashMap),
                            effect: E::Move,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(1),
                            effect: E::Value,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::HashMap)),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Remove) => RuntimeOpRow {
                symbol: "map.value.remove",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashMap),
                            effect: E::Move,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::HashMap),
                        K::Applied(BuiltinType::Option, &[K::TypeArgument(1)]),
                    ])),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Clear) => RuntimeOpRow {
                symbol: "map.value.clear",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashMap),
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::HashMap)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Keys) => RuntimeOpRow {
                symbol: "map.value.keys",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashMap),
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Vec,
                        &[K::TypeArgument(0)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Values) => RuntimeOpRow {
                symbol: "map.value.values",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashMap),
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Vec,
                        &[K::TypeArgument(1)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Entries) => RuntimeOpRow {
                symbol: "map.value.entries",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashMap),
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Vec,
                        &[K::Tuple(&[K::TypeArgument(0), K::TypeArgument(1)])],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::New) => RuntimeOpRow {
                symbol: "set.value.new",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::FreshOwned(K::Receiver(BuiltinType::HashSet)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::Len) => RuntimeOpRow {
                symbol: "set.value.len",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashSet),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::Contains) => RuntimeOpRow {
                symbol: "set.value.contains",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashSet),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::Insert) => RuntimeOpRow {
                symbol: "set.value.insert",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashSet),
                            effect: E::Move,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Value,
                        },
                    ],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::HashSet),
                        K::Bool,
                    ])),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::Remove) => RuntimeOpRow {
                symbol: "set.value.remove",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashSet),
                            effect: E::Move,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::HashSet),
                        K::Bool,
                    ])),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::Clear) => RuntimeOpRow {
                symbol: "set.value.clear",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashSet),
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::HashSet)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::Elements) => RuntimeOpRow {
                symbol: "set.value.elements",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashSet),
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Vec,
                        &[K::TypeArgument(0)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecAppend => RuntimeOpRow {
                symbol: "hew_vec_append",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecClear => RuntimeOpRow {
                symbol: "hew_vec_clear",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecClone => RuntimeOpRow {
                symbol: "hew_vec_clone",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecCloneLayout => RuntimeOpRow {
                symbol: "hew_vec_clone_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecCloneOwned => RuntimeOpRow {
                symbol: "hew_vec_clone_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecContainsLayout => RuntimeOpRow {
                symbol: "hew_vec_contains_thunk",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecTakeAll => RuntimeOpRow {
                symbol: "hew_vec_take_all",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecContainsOwned => RuntimeOpRow {
                symbol: "hew_vec_contains_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecContainsScalar(VecContainsScalarElem::F64) => RuntimeOpRow {
                symbol: "hew_vec_contains_f64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecContainsScalar(VecContainsScalarElem::I32) => RuntimeOpRow {
                symbol: "hew_vec_contains_i32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecContainsScalar(VecContainsScalarElem::I64) => RuntimeOpRow {
                symbol: "hew_vec_contains_i64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecContainsScalar(VecContainsScalarElem::Str) => RuntimeOpRow {
                symbol: "hew_vec_contains_str",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Bool) => RuntimeOpRow {
                symbol: "hew_vec_get_bool",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::VecBool,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::F32) => RuntimeOpRow {
                symbol: "hew_vec_get_f32",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::F64) => RuntimeOpRow {
                symbol: "hew_vec_get_f64",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::I8) => RuntimeOpRow {
                symbol: "hew_vec_get_i8",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::I16) => RuntimeOpRow {
                symbol: "hew_vec_get_i16",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::I32) => RuntimeOpRow {
                symbol: "hew_vec_get_i32",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::VecI32GetSet,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::I64) => RuntimeOpRow {
                symbol: "hew_vec_get_i64",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Clone) => RuntimeOpRow {
                symbol: "hew_vec_get_clone",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Take) => RuntimeOpRow {
                symbol: "hew_vec_take_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Layout) => RuntimeOpRow {
                symbol: "hew_vec_get_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Owned) => RuntimeOpRow {
                symbol: "hew_vec_get_owned",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Ptr) => RuntimeOpRow {
                symbol: "hew_vec_get_ptr",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Str) => RuntimeOpRow {
                symbol: "hew_vec_get_str",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::U8) => RuntimeOpRow {
                symbol: "hew_vec_get_u8",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::U16) => RuntimeOpRow {
                symbol: "hew_vec_get_u16",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecIsEmpty => RuntimeOpRow {
                symbol: "hew_vec_is_empty",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecJoinStr => RuntimeOpRow {
                symbol: "hew_vec_join_str",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecLen => RuntimeOpRow {
                symbol: "hew_vec_len",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecNew => RuntimeOpRow {
                symbol: "Vec::new",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecConstructor,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPopBool => RuntimeOpRow {
                symbol: "hew_vec_pop_bool",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecBool,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPopLayout => RuntimeOpRow {
                symbol: "hew_vec_pop_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPopOwned => RuntimeOpRow {
                symbol: "hew_vec_pop_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPushBool => RuntimeOpRow {
                symbol: "hew_vec_push_bool",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecBool,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPushLayout => RuntimeOpRow {
                symbol: "hew_vec_push_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPushOwned => RuntimeOpRow {
                symbol: "hew_vec_push_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPushOwnedMove => RuntimeOpRow {
                symbol: "hew_vec_push_owned_move",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::F32,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_f32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::F64,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_f64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::I8,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_i8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::I16,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_i16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::I32,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_i32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::I64,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_i64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::Ptr,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_ptr",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::Str,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_str",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::U8,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_u8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::U16,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_u16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::F32,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_f32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::F64,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_f64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::I8,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_i8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::I16,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_i16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::I32,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_i32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::I64,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_i64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::Ptr,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_ptr",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::Str,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_str",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::U8,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_u8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::U16,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_u16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::F32,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_f32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::F64,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_f64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::I8,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_i8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::I16,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_i16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::I32,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_i32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecI32GetSet,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::I64,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_i64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::Ptr,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_ptr",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::Str,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_str",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::U8,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_u8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::U16,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_u16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::F32,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_f32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::F64,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_f64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::I8,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_i8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::I16,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_i16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::I32,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_i32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::I64,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_i64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::Ptr,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_ptr",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::Str,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_str",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::U8,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_u8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::U16,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_u16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecRemoveAtBool => RuntimeOpRow {
                symbol: "hew_vec_remove_at_bool",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecBool,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecRemoveAtLayout => RuntimeOpRow {
                symbol: "hew_vec_remove_at_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecRemoveAtOwned => RuntimeOpRow {
                symbol: "hew_vec_remove_at_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSetBool => RuntimeOpRow {
                symbol: "hew_vec_set_bool",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecBool,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSetLayout => RuntimeOpRow {
                symbol: "hew_vec_set_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSetOwned => RuntimeOpRow {
                symbol: "hew_vec_set_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSetOwnedMove => RuntimeOpRow {
                symbol: "hew_vec_set_owned_move",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::Bytesize) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_bytesize",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::F64) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_f64",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::I32) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_i32",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::I64) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_i64",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::Layout) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::Owned) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_owned",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::Ptr) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_ptr",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::Str) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_str",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VtableDispatchPanicOnOob => RuntimeOpRow {
                symbol: "hew_vtable_dispatch_panic_on_oob",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
        }
    }

    /// The C-ABI symbol this operation lowers to, from its row.
    ///
    /// A `Declared` row's symbol is the linker symbol. A `PreStaged` row's is
    /// the codegen `Terminator::Call` intercept callee name, which the linker
    /// never has to resolve at the call site.
    #[must_use]
    pub const fn c_symbol(self) -> &'static str {
        self.row().symbol
    }

    /// The operation that owns a C symbol, when the symbol names exactly one.
    ///
    /// This is the row table's `symbol` column read backwards, so a new
    /// operation needs no reverse arm. A symbol several rows share names no
    /// single operation: `hew_print_value` carries its element type and its
    /// newline flag beside the symbol, so it does not identify a row.
    #[must_use]
    pub fn from_c_symbol(sym: &str) -> Option<Self> {
        static BY_SYMBOL: std::sync::OnceLock<
            std::collections::HashMap<&'static str, Option<RuntimeCallFamily>>,
        > = std::sync::OnceLock::new();
        BY_SYMBOL
            .get_or_init(|| {
                let mut symbols = std::collections::HashMap::new();
                for family in all_runtime_call_families() {
                    symbols
                        .entry(family.row().symbol)
                        .and_modify(|owner: &mut Option<RuntimeCallFamily>| *owner = None)
                        .or_insert(Some(family));
                }
                symbols
            })
            .get(sym)
            .copied()
            .flatten()
    }

    /// Recover a family that is intentionally carried on MIR
    /// `Terminator::Call`.
    ///
    /// Codegen-only collection partition variants remain classifiable through
    /// [`Self::from_c_symbol`] but do not widen the MIR carrier merely because
    /// codegen gained a typed spelling for an existing direct-call helper.
    #[must_use]
    pub fn from_mir_builtin_symbol(sym: &str) -> Option<Self> {
        Self::from_c_symbol(sym).filter(|family| !family.is_codegen_partition_only())
    }

    const fn is_codegen_partition_only(self) -> bool {
        matches!(
            self,
            Self::BytesNew
                | Self::VecAppend
                | Self::VecClear
                | Self::VecClone
                | Self::HashMapClearLayout
                | Self::HashMapCloneLayout
                | Self::HashSetClearLayout
                | Self::HashSetCloneLayout
                | Self::HashSetToVecLayout
                | Self::VecCloneLayout
                | Self::VecCloneOwned
                | Self::VecTakeAll
                | Self::VecContainsLayout
                | Self::VecContainsOwned
                | Self::VecContainsScalar(_)
                | Self::VecNew
                | Self::VecPopBool
                | Self::VecPopLayout
                | Self::VecPopOwned
                | Self::VecPushBool
                | Self::VecPushLayout
                | Self::VecPushOwned
                | Self::VecPushOwnedMove
                | Self::VecScalar { .. }
                | Self::VecRemoveAtBool
                | Self::VecRemoveAtLayout
                | Self::VecRemoveAtOwned
                | Self::VecSetBool
                | Self::VecSetLayout
                | Self::VecSetOwned
                | Self::VecSetOwnedMove
                | Self::VecIsEmpty
                | Self::VecJoinStr
        )
    }

    /// Broad collection family partition used by codegen routing.
    #[must_use]
    pub const fn is_vec_op(self) -> bool {
        matches!(
            self,
            Self::VecAppend
                | Self::VecClear
                | Self::VecClone
                | Self::VecCloneLayout
                | Self::VecCloneOwned
                | Self::VecTakeAll
                | Self::VecContainsLayout
                | Self::VecContainsOwned
                | Self::VecContainsScalar(_)
                | Self::VecGet(_)
                | Self::VecIsEmpty
                | Self::VecJoinStr
                | Self::VecLen
                | Self::VecNew
                | Self::VecPopBool
                | Self::VecPopLayout
                | Self::VecPopOwned
                | Self::VecPushBool
                | Self::VecPushLayout
                | Self::VecPushOwned
                | Self::VecPushOwnedMove
                | Self::VecScalar { .. }
                | Self::VecRemoveAtBool
                | Self::VecRemoveAtLayout
                | Self::VecRemoveAtOwned
                | Self::VecSetBool
                | Self::VecSetLayout
                | Self::VecSetOwned
                | Self::VecSetOwnedMove
                | Self::VecSliceRange(_)
        )
    }

    /// Broad `HashMap` operation partition used by codegen routing.
    #[must_use]
    pub const fn is_hashmap_op(self) -> bool {
        matches!(
            self,
            Self::HashMapContainsKeyLayout
                | Self::HashMapClearLayout
                | Self::HashMapCloneLayout
                | Self::HashMapEntriesLayout
                | Self::HashMapFreeLayout
                | Self::HashMapGetLayout
                | Self::HashMapInsertLayout
                | Self::HashMapKeysLayout
                | Self::HashMapLenLayout
                | Self::HashMapNew
                | Self::HashMapNewWithLayout
                | Self::HashMapRemoveLayout
                | Self::HashMapValuesLayout
        )
    }

    /// Broad `HashSet` operation partition used by codegen routing.
    #[must_use]
    pub const fn is_hashset_op(self) -> bool {
        matches!(
            self,
            Self::HashSetContainsLayout
                | Self::HashSetClearLayout
                | Self::HashSetCloneLayout
                | Self::HashSetFreeLayout
                | Self::HashSetInsertLayout
                | Self::HashSetIsEmptyLayout
                | Self::HashSetLenLayout
                | Self::HashSetNew
                | Self::HashSetNewWithLayout
                | Self::HashSetRemoveLayout
                | Self::HashSetToVecLayout
        )
    }

    /// Broad bytes operation partition used by codegen routing.
    #[must_use]
    pub const fn is_bytes_op(self) -> bool {
        matches!(
            self,
            Self::BytesAppend
                | Self::BytesClear
                | Self::BytesContains
                | Self::BytesDecodeUtf8
                | Self::BytesDecodeUtf8Lossy
                | Self::BytesGet
                | Self::BytesIndex
                | Self::BytesIsEmpty
                | Self::BytesLen
                | Self::BytesNew
                | Self::BytesPop
                | Self::BytesPush
                | Self::BytesSet
                | Self::BytesSlice
                | Self::BytesSliceFrom
        )
    }

    /// True for source-level `Node::*` builtin identities.
    #[must_use]
    pub const fn is_node_builtin(self) -> bool {
        matches!(
            self,
            Self::NodeAllowPeer
                | Self::NodeConnect
                | Self::NodeId
                | Self::NodeIdentityKey
                | Self::NodeLoadKeys
                | Self::NodeLookup
                | Self::NodeRegister
                | Self::NodeSetTransport
                | Self::NodeShutdown
                | Self::NodeStart
        )
    }

    /// Runtime authority required by this call family, when one is carried.
    #[must_use]
    pub const fn runtime_capability(self) -> Option<RuntimeCapability> {
        match self {
            Self::MetricCounterRegister
            | Self::MetricCounterInc
            | Self::MetricCounterAdd
            | Self::MetricGaugeRegister
            | Self::MetricGaugeSet
            | Self::MetricGaugeInc
            | Self::MetricGaugeDec
            | Self::MetricGaugeAdd
            | Self::MetricHistogramRegister
            | Self::MetricHistogramRegisterSimple
            | Self::MetricHistogramRecord
            | Self::MetricVecRegister
            | Self::MetricVecWith => Some(RuntimeCapability::Metrics),
            family if family.is_node_builtin() => Some(RuntimeCapability::Node),
            _ => None,
        }
    }

    /// True for compiler-recognised MIR spellings with no runtime export.
    #[must_use]
    pub const fn is_synthetic_mir_symbol(self) -> bool {
        matches!(
            self,
            Self::Array(_)
                | Self::Vector(_)
                | Self::Map(_)
                | Self::Set(_)
                | Self::BytesGet
                | Self::RegexCapture
                | Self::RegexCompile
                | Self::RegexFreeCapture
                | Self::RegexHandle
                | Self::RegexMatch
                | Self::StringGet
        )
    }

    /// True when `Instr::CallRuntimeAbi` may carry this family.
    #[must_use]
    pub const fn is_mir_emitter_family(self) -> bool {
        !is_pre_staged_family(self)
    }

    /// True when the MIR emitter family names a real runtime export.
    #[must_use]
    pub const fn is_runtime_backed_mir_family(self) -> bool {
        self.is_mir_emitter_family() && !self.is_synthetic_mir_symbol()
    }

    /// ABI-routing shape for collection calls that need bespoke marshalling.
    #[must_use]
    pub const fn abi_shape(self) -> RuntimeCallAbiShape {
        self.row().abi_shape
    }

    /// The collection and encoding contracts, which the two consume-verdict
    /// readers below trust for every argument position.
    ///
    /// WHY the filter: every operation now publishes its row, but only these
    /// families' contracts have been audited position by position. Reading the
    /// rest would change lowering, not just labels. WHEN obsolete: once each
    /// remaining row's argument effects are audited against its runtime entry.
    /// WHAT the real fix is: delete this filter and read `row().contract` for
    /// every operation, leaving the fail-closed default only for a row that
    /// publishes no contract.
    const fn collection_semantic_contract(self) -> Option<RuntimeSemanticContract> {
        match self {
            Self::Array(_)
            | Self::SupervisorPool(_)
            | Self::Vector(_)
            | Self::Map(_)
            | Self::Set(_) => self.row().contract,
            _ => None,
        }
    }

    /// True iff calling this family consumes the receiver handle (the
    /// runtime entry takes ownership; the caller MUST NOT drop the
    /// handle after the call). Mirrors `runtime_symbol_consumes_receiver`
    /// in `hew-types/src/builtin_names.rs` for the 7-symbol set:
    /// `hew_stream_close`, `hew_sink_close`, `hew_channel_sender_close`,
    /// `hew_channel_receiver_close`, `hew_duplex_close`,
    /// `hew_duplex_close_half`, plus the TCP
    /// active-mode handoff. The latter is not a close call: its consume fact
    /// comes from the generated FFI contract for `hew_tcp_attach_local`,
    /// because the reactor becomes the connection's sole close authority.
    ///
    /// LESSONS P0 `boundary-fail-closed`: a missed consume-mark leaks
    /// the handle (drop fires once on a still-live handle) — it never
    /// double-frees. So the safe default for unrecognised cases is
    /// `false` (borrowing), preserved here by the explicit closed-set
    /// listing.
    #[must_use]
    pub fn consumes_receiver(self) -> bool {
        if let Some(contract) = self.collection_semantic_contract().or_else(|| {
            self.encoding_format()
                .and_then(|_| self.semantic_contract())
        }) {
            return matches!(
                contract.arguments.first(),
                Some(RuntimeArgumentContract {
                    effect: RuntimeArgumentEffect::Move,
                    ..
                })
            );
        }
        if self == Self::TcpAttachLocal {
            // The TCP handoff carries a scalar connection token at ABI level,
            // so this must not be a spelling-only ownership inference. An
            // absent/wrong/short row is deliberately non-consuming; the
            // generated contract is the sole positive authority.
            return crate::ffi_contracts::extern_param_ownership(self.c_symbol(), 0)
                == Some(crate::ffi_contracts::ExternParamOwnership::Consume);
        }
        matches!(
            self,
            Self::StreamClose
                | Self::SinkClose
                | Self::ChannelSenderClose
                | Self::ChannelReceiverClose
                | Self::ChannelPairFree
                | Self::ActorRequestRelease
                | Self::ActorCallFree
                | Self::DuplexClose
                | Self::DuplexCloseHalf
                // The half-extract methods move the unified `Duplex` handle out:
                // after `.send_half()` / `.recv_half()` the source `Duplex`
                // binding is dead and only the extracted half drops. Without the
                // consume mark the parent `Duplex` stays in the scope-exit drop
                // set and closes a direction the half now owns — a double-close.
                | Self::DuplexSendHalf
                | Self::DuplexRecvHalf
        )
    }

    /// Per-argument consume/borrow verdict for a synthesized runtime-call
    /// edge. This is the runtime-side carrier of the same fact the user-body
    /// fixpoint (`compute_call_param_consumption`) carries for user functions:
    /// together they close the census's "synthesized-spelling UNKNOWN" edge
    /// class — every compiler-emitted call argument now has a KNOWN verdict
    /// from a CLOSED compiler-owned vocabulary, with no open-world unknown.
    ///
    /// # Authority and fail-closed shape
    ///
    /// * **Receiver (`index == 0`)** — derived from the single existing
    ///   authority [`consumes_receiver`](Self::consumes_receiver): a consuming
    ///   receiver (the close family + the `Duplex` half-extracts) is
    ///   [`ProvenConsume`](ConsumeVerdict::ProvenConsume); every other receiver
    ///   is [`ProvenBorrow`](ConsumeVerdict::ProvenBorrow). Defaulting a
    ///   receiver to BORROW is the double-free-safe direction (a missed consume
    ///   leaks, never double-frees — see `consumes_receiver`'s note).
    /// * **Non-receiver (`index >= 1`)** —
    ///   [`ConservativeConsume`](ConsumeVerdict::ConservativeConsume) unless
    ///   a closed family-specific ABI table proves a borrow. The scalar and
    ///   plain Vec matrix below is exact: its index/value/secondary-vector and
    ///   separator inputs are borrowed.
    ///   WHY — the family carries no per-argument type/ownership detail, so an
    ///   individually-proven verdict is not derivable here;
    ///   `ConservativeConsume` is the fail-closed default (callee assumed to
    ///   own → no double-free if
    ///   a future consumer acts on it). WHEN obsolete — when a consumer wires
    ///   onto this axis and a genuine borrow-vs-consume precision win on a
    ///   non-receiver runtime arg is wanted. WHAT the real solution is — an
    ///   exhaustive per-family, per-index classification (the `*Owned`/`*Move`
    ///   collection sinks moving their payload, the scalar-index args
    ///   borrowing), audited against each family's MIR emit intent. Nothing
    ///   consumes this axis in the current lane (fact-carriage + pin only;
    ///   codegen byte-identical), so the default is a carried label, never a
    ///   lowering decision.
    #[must_use]
    pub fn arg_consume_verdict(self, index: usize) -> ConsumeVerdict {
        if let Some(contract) = self.collection_semantic_contract().or_else(|| {
            self.encoding_format()
                .and_then(|_| self.semantic_contract())
        }) {
            return match contract.arguments.get(index) {
                Some(RuntimeArgumentContract {
                    effect: RuntimeArgumentEffect::Move,
                    ..
                }) => ConsumeVerdict::ProvenConsume,
                Some(RuntimeArgumentContract {
                    effect: RuntimeArgumentEffect::Value,
                    ..
                })
                | None => ConsumeVerdict::ConservativeConsume,
                Some(_) => ConsumeVerdict::ProvenBorrow,
            };
        }
        if index == 0 {
            return if self.consumes_receiver() {
                ConsumeVerdict::ProvenConsume
            } else {
                ConsumeVerdict::ProvenBorrow
            };
        }
        match self {
            // Layout insert takes ownership of both heap-owning key/value
            // arguments on its normal return. The receiver stays borrowed;
            // contains/get/remove keys remain borrowed lookup probes.
            Self::HashMapInsertLayout if matches!(index, 1 | 2) => {
                ConsumeVerdict::ProvenConsume
            }
            // Set insertion likewise adopts its element on success. The
            // owned-move Vec ABI copies the element bytes into
            // descriptor-owned storage and adopts every nested allocation on
            // normal return.  The caller retains cleanup only on the unwind
            // edge; MIR publishes the Transfer in the call's normal
            // successor.  Keep this fact here, beside the typed family, so
            // lowering and balance validation cannot disagree through a
            // second symbol-name table.
            Self::HashSetInsertLayout | Self::VecPushOwnedMove if index == 1 => {
                ConsumeVerdict::ProvenConsume
            }
            Self::VecSetOwnedMove if index == 2 => ConsumeVerdict::ProvenConsume,
            // Scalar values have no ownership transfer. For `Str`, the
            // runtime reads the input string during push/set and never adopts
            // it. Pop/remove ownership is a result fact, not an argument fact.
            Self::VecScalar { .. }
            // Scalar contains arguments are borrowed, including C-string
            // comparison for `Vec<string>::contains`.
            | Self::VecContainsScalar(_)
            // `append` borrows the source vector, and `join` borrows its
            // separator string. The remaining plain Vec entries carry no
            // payload beyond their borrowed receiver, so any accidental extra
            // argument is still fail-closed as borrowed only within this
            // closed, audited ABI family.
            | Self::VecAppend
            | Self::VecClear
            | Self::VecClone
            | Self::VecIsEmpty
            | Self::VecJoinStr => ConsumeVerdict::ProvenBorrow,
            _ => ConsumeVerdict::ConservativeConsume,
        }
    }

    /// Exact semantic operation contract ownership SIR admits. An operation
    /// whose row publishes no contract fails closed.
    #[must_use]
    pub const fn semantic_contract(self) -> Option<RuntimeSemanticContract> {
        self.row().contract
    }

    /// Return-value ownership for the closed scalar Vec ABI surface.
    #[must_use]
    pub const fn result_ownership(self) -> RuntimeResultOwnership {
        if let Some(contract) = self.semantic_contract() {
            return match contract.result {
                RuntimeResultEffect::FreshOwned(RuntimeValueKind::String) => {
                    RuntimeResultOwnership::FreshOwnedString
                }
                RuntimeResultEffect::FreshOwned(RuntimeValueKind::Bytes) => {
                    RuntimeResultOwnership::FreshOwnedBytes
                }
                RuntimeResultEffect::Unit
                | RuntimeResultEffect::Never
                | RuntimeResultEffect::Borrowed(_)
                | RuntimeResultEffect::IndependentValue(_)
                | RuntimeResultEffect::UpdatedReceiverAndValue(_)
                | RuntimeResultEffect::BitCopy(_)
                | RuntimeResultEffect::UpdatedReceiver(_)
                | RuntimeResultEffect::FreshOwnedVariant(_)
                | RuntimeResultEffect::FreshOwned(
                    RuntimeValueKind::Unit
                    | RuntimeValueKind::Bool
                    | RuntimeValueKind::I8
                    | RuntimeValueKind::I16
                    | RuntimeValueKind::U8
                    | RuntimeValueKind::U16
                    | RuntimeValueKind::U32
                    | RuntimeValueKind::I32
                    | RuntimeValueKind::I64
                    | RuntimeValueKind::U64
                    | RuntimeValueKind::Isize
                    | RuntimeValueKind::Usize
                    | RuntimeValueKind::F64
                    | RuntimeValueKind::Char
                    | RuntimeValueKind::Duration
                    | RuntimeValueKind::Receiver(_)
                    | RuntimeValueKind::FixedArray
                    | RuntimeValueKind::ArrayElement
                    | RuntimeValueKind::PoolView
                    | RuntimeValueKind::PoolMember
                    | RuntimeValueKind::ChannelHalf(_)
                    | RuntimeValueKind::ChannelHalfResult(_)
                    | RuntimeValueKind::ChannelPair
                    | RuntimeValueKind::ActorRequestOwner
                    | RuntimeValueKind::ActorRequestAdmission
                    | RuntimeValueKind::TypeArgument(_)
                    | RuntimeValueKind::SharedPayload
                    | RuntimeValueKind::NodeLookupResult
                    | RuntimeValueKind::BuiltinNominal(_)
                    | RuntimeValueKind::Applied(_, _)
                    | RuntimeValueKind::Tuple(_)
                    | RuntimeValueKind::IoHandle(_)
                    | RuntimeValueKind::FileReadHandle(_)
                    | RuntimeValueKind::Named(_)
                    | RuntimeValueKind::NamedOpaque(_)
                    | RuntimeValueKind::MonomorphicBuiltin(_),
                ) => RuntimeResultOwnership::Untracked,
            };
        }
        match self {
            Self::VecScalar {
                op: VecScalarOp::Pop | VecScalarOp::RemoveAt,
                elem: VecScalarElem::Str,
            }
            | Self::VecJoinStr => RuntimeResultOwnership::FreshOwnedString,
            Self::VecClone | Self::VecNew => RuntimeResultOwnership::FreshOwnedVec,
            Self::BytesNew => RuntimeResultOwnership::FreshOwnedBytes,
            _ => RuntimeResultOwnership::Untracked,
        }
    }

    /// Classify whether the returned value is independent of argument zero.
    /// Getter families are exhaustive here so adding an ABI variant cannot
    /// silently inherit owner or borrow semantics from its C spelling.
    #[must_use]
    pub const fn result_authority(self) -> RuntimeResultAuthority {
        if let Some(contract) = self.semantic_contract() {
            return match contract.result {
                RuntimeResultEffect::IndependentValue(_) => {
                    RuntimeResultAuthority::IndependentValue
                }
                RuntimeResultEffect::UpdatedReceiverAndValue(_)
                | RuntimeResultEffect::FreshOwned(_)
                | RuntimeResultEffect::FreshOwnedVariant(_)
                | RuntimeResultEffect::UpdatedReceiver(_) => {
                    RuntimeResultAuthority::IndependentOwned
                }
                RuntimeResultEffect::BitCopy(_) => RuntimeResultAuthority::IndependentBitCopy,
                RuntimeResultEffect::Borrowed(_) => RuntimeResultAuthority::InteriorAliasOfReceiver,
                RuntimeResultEffect::Unit | RuntimeResultEffect::Never => {
                    RuntimeResultAuthority::FailClosed
                }
            };
        }
        match self {
            Self::VecGet(VecGetElem::Str | VecGetElem::Clone | VecGetElem::Take)
            | Self::VecClone
            | Self::VecNew
            | Self::VecJoinStr
            | Self::BytesNew
            | Self::VecScalar {
                op: VecScalarOp::Pop | VecScalarOp::RemoveAt,
                ..
            } => RuntimeResultAuthority::IndependentOwned,
            Self::VecGet(VecGetElem::Owned | VecGetElem::Ptr) | Self::HashMapGetLayout => {
                RuntimeResultAuthority::InteriorAliasOfReceiver
            }
            Self::VecGet(
                VecGetElem::Bool
                | VecGetElem::F32
                | VecGetElem::F64
                | VecGetElem::I8
                | VecGetElem::I16
                | VecGetElem::I32
                | VecGetElem::I64
                | VecGetElem::Layout
                | VecGetElem::U8
                | VecGetElem::U16,
            ) => RuntimeResultAuthority::IndependentBitCopy,
            _ => RuntimeResultAuthority::FailClosed,
        }
    }

    /// Whether a successful call can invalidate a pointer into argument
    /// zero's element storage. The answer is intentionally broader than
    /// reallocation: clear/set/remove and whole-receiver teardown invalidate
    /// an alias even when the backing allocation address happens not to move.
    #[must_use]
    pub const fn invalidates_collection_element_aliases(self) -> bool {
        if let Some(contract) = self.collection_semantic_contract() {
            return matches!(
                contract.arguments,
                [
                    RuntimeArgumentContract {
                        effect: RuntimeArgumentEffect::Move,
                        ..
                    },
                    ..
                ]
            );
        }
        matches!(
            self,
            Self::VecAppend
                | Self::VecClear
                | Self::VecTakeAll
                | Self::VecPopBool
                | Self::VecPopLayout
                | Self::VecPopOwned
                | Self::VecPushBool
                | Self::VecPushLayout
                | Self::VecPushOwned
                | Self::VecPushOwnedMove
                | Self::VecScalar {
                    op: VecScalarOp::Push
                        | VecScalarOp::Pop
                        | VecScalarOp::Set
                        | VecScalarOp::RemoveAt,
                    ..
                }
                | Self::VecRemoveAtBool
                | Self::VecRemoveAtLayout
                | Self::VecRemoveAtOwned
                | Self::VecSetBool
                | Self::VecSetLayout
                | Self::VecSetOwned
                | Self::VecSetOwnedMove
                | Self::HashMapClearLayout
                | Self::HashMapFreeLayout
                | Self::HashMapInsertLayout
                | Self::HashMapRemoveLayout
                | Self::HashSetClearLayout
                | Self::HashSetFreeLayout
                | Self::HashSetInsertLayout
                | Self::HashSetRemoveLayout
        )
    }

    /// Classify the family's async-suspending behaviour, if any.
    ///
    /// Source of truth: the HIR await-classifier at
    /// `hew-hir/src/lower.rs` — `is_stream_send_await`
    /// (`c_symbol == "hew_sink_write_bytes"`) and the duplex-close
    /// await arm (`c_symbol == "hew_duplex_close"`). The suspending
    /// channel/stream recv path rides the element-layout-witness
    /// `*_layout` symbols, which bypass `RuntimeCallFamily` entirely
    /// (HIR string-matches `hew_channel_recv_layout` /
    /// `hew_stream_next_layout` directly).
    ///
    /// The match is exhaustive over [`RuntimeCallFamily`] — there is
    /// no `_ =>` wildcard arm — so a future family variant cannot be
    /// added without explicitly declaring its suspension behaviour
    /// (LESSONS P0 `match-fail-closed` + `exhaustive-traversal-and-lowering`).
    #[must_use]
    #[allow(
        clippy::too_many_lines,
        reason = "exhaustive closed-enum match by design; no `_ =>` arm"
    )]
    pub fn is_async_suspending(self) -> Option<AsyncSuspendKind> {
        use RuntimeCallFamily as F;
        match self {
            F::AsyncIo(op) => Some(AsyncSuspendKind::NativeIo(op)),
            // The suspending symbols (HIR await-classifier source of truth).
            // Every describable sink-send element suspends: the byte and
            // string sink writes and the layout-witness stream send all
            // share the backpressure-aware `SuspendKind::StreamSend` ramp.
            // Codegen discriminates the runtime entry on the value's
            // `ResolvedTy` (bytes → native `hew_stream_await_send`, else
            // layout `hew_stream_await_send_layout`), so one kind suffices.
            F::SinkWrite(StreamElementKind::Bytes | StreamElementKind::String)
            | F::StreamSendLayout => Some(AsyncSuspendKind::SinkSend),
            F::DuplexClose => Some(AsyncSuspendKind::DuplexClose),
            F::ChannelRecvLayout => Some(AsyncSuspendKind::ChannelRecv),
            F::StreamNextLayout => Some(AsyncSuspendKind::StreamRecv),

            // Everything else: NOT suspending today. Exhaustively listed
            // so adding a new variant requires an explicit decision.
            F::FileRead(_)
            | F::Tcp(_)
            | F::Array(_)
            // The pool member barrier blocks its calling thread; SIR refuses
            // it inside an actor, so no accessor suspends.
            | F::SupervisorPool(_)
            | F::Vector(_)
            | F::Map(_)
            | F::Set(_)
            | F::Encoding { .. }
            | F::StreamClose
            // Wrapping a source stream in an adaptor is a pointer handoff.
            | F::StreamChunks
            | F::StreamLines
            | F::StreamTake
            | F::StreamTryNextLayout
            | F::SinkTryWrite(_)
            | F::SinkClose
            | F::SinkPeerClosed
            | F::ActorGenSinkComplete
            | F::ActorGenSinkRegister
            | F::ChannelSendLayout
            | F::ChannelTryRecvLayout
            | F::ChannelSenderClone
            | F::ChannelSenderClose
            | F::ChannelReceiverClose
            | F::ChannelPairNew
            | F::ChannelPairFree
            | F::ActorRequestRelease
            | F::ActorCallFree
            | F::ActorRequestTake
            | F::ChannelPairIsValid
            | F::ChannelPairSender
            | F::ChannelPairReceiver
            | F::DuplexClone
            | F::DuplexCloseHalf
            | F::DuplexPair
            | F::DuplexPayloadFree
            | F::DuplexRecv
            | F::DuplexRecvHalf
            | F::DuplexSend
            | F::DuplexSendHalf
            | F::DuplexTryRecv
            | F::DuplexTrySend
            | F::ActorAsk
            | F::ActorAskWithChannel
            | F::ActorCooperate
            | F::ActorDemonitor
            | F::ActorLink
            | F::LinkRemote
            | F::ActorMonitor
            | F::ActorSelf
            | F::ActorSendById
            | F::ActorSpawn
            | F::ActorUnlink
            | F::AutoMutexAlloc
            | F::AutoMutexFree
            | F::AutoMutexLock
            | F::AutoMutexUnlock
            | F::BytesAppend
            | F::BytesClear
            | F::BytesContains
            | F::BytesDecodeUtf8
            | F::BytesDecodeUtf8Lossy
            | F::BytesGet
            | F::BytesIndex
            | F::BytesIsEmpty
            | F::BytesLen
            | F::BytesPop
            | F::BytesPush
            | F::BytesSet
            | F::BytesSlice
            | F::BytesSliceFrom
            | F::BytesNew
            | F::CancelTokenIsRequested
            | F::CancelTokenRelease
            | F::CancelTokenRetain
            | F::DurationAbs
            | F::DurationHours
            | F::DurationIsZero
            | F::DurationMicros
            | F::DurationMillis
            | F::DurationMins
            | F::DurationNanos
            | F::DurationSecs
            | F::DynBoxAlloc
            | F::DynBoxFree
            | F::HashMapContainsKeyLayout
            | F::HashMapClearLayout
            | F::HashMapCloneLayout
            | F::HashMapEntriesLayout
            | F::HashMapFreeLayout
            | F::HashMapGetLayout
            | F::HashMapInsertLayout
            | F::HashMapKeysLayout
            | F::HashMapLenLayout
            | F::HashMapNew
            | F::HashMapNewWithLayout
            | F::HashMapRemoveLayout
            | F::HashMapValuesLayout
            | F::HashSetContainsLayout
            | F::HashSetClearLayout
            | F::HashSetCloneLayout
            | F::HashSetFreeLayout
            | F::HashSetInsertLayout
            | F::HashSetIsEmptyLayout
            | F::HashSetLenLayout
            | F::HashSetNew
            | F::HashSetNewWithLayout
            | F::HashSetRemoveLayout
            | F::HashSetToVecLayout
            | F::InstantDurationSince
            | F::InstantElapsed
            | F::InstantNow
            | F::MathIntrinsic(_)
            | F::IntMethod(_, _)
            | F::IntArith(_, _)
            | F::FloatMethod(_)
            | F::MetricCounterRegister
            | F::MetricCounterInc
            | F::MetricCounterAdd
            | F::MetricGaugeRegister
            | F::MetricGaugeSet
            | F::MetricGaugeInc
            | F::MetricGaugeDec
            | F::MetricGaugeAdd
            | F::MetricHistogramRegister
            | F::MetricHistogramRegisterSimple
            | F::MetricHistogramRecord
            | F::MetricVecRegister
            | F::MetricVecWith
            | F::NodeAllowPeer
            | F::NodeConnect
            | F::NodeId
            | F::NodeIdentityKey
            | F::NodeLoadKeys
            | F::NodeLookup
            | F::NodeMonitor
            | F::NodeRegister
            | F::NodeSetTransport
            | F::NodeShutdown
            | F::NodeStart
            | F::ObserveReadU64
            | F::ObserveScrape
            | F::ObserveSeries
            | F::ObserveBarrier
            | F::RcClone
            | F::RcDowngrade
            | F::RcDrop
            | F::RcGet
            | F::RcIsUnique
            | F::RcNew
            | F::RcSet
            | F::RcStrongCount
            | F::RcWeakCount
            | F::WeakCloneRc
            | F::WeakDropRc
            | F::WeakUpgradeRc
            | F::RecvHalfRecv
            | F::RecvHalfTryRecv
            | F::RegexCapture
            | F::RegexCompile
            | F::RegexFreeCapture
            | F::RegexHandle
            | F::RegexMatch
            | F::RemotePidSend
            | F::ReplyChannelCancel
            | F::ReplyChannelFree
            | F::ReplyChannelNew
            | F::ReplyPayloadFree
            | F::ReplyWait
            | F::SelectFirst
            | F::SendHalfSend
            | F::SendHalfTrySend
            | F::StringCharAt
            | F::StringCharAtUtf8
            | F::StringCharCount
            | F::StringByteLen
            | F::StringConcat
            | F::ProcessExit
            | F::StderrWrite
            | F::BoolToString
            | F::StringEquals
            | F::StringCompare
            | F::StringContains
            | F::StringStartsWith
            | F::StringEndsWith
            | F::StringIsEmpty
            | F::StringIsDigit
            | F::StringIsAlpha
            | F::StringIsAlphanumeric
            | F::StructuralFormat
            | F::StringFind
            | F::StringGet
            | F::StringIndex
            | F::StringLen
            | F::StringSlice
            | F::StringRepeat
            | F::StringReplace
            | F::StringClone
            | F::StringSplit
            | F::StringLines
            | F::StringChars
            | F::StringToLowercase
            | F::StringSliceCodepoints
            | F::StringSliceCodepointsFrom
            | F::StringToBytes
            | F::StringToUppercase
            | F::StringTrim
            | F::U8ToString
            | F::I32ToString
            | F::I64ToString
            | F::U32ToString
            | F::U64ToString
            | F::F64ToString
            | F::CharToString
            | F::Print { .. }
            | F::SupervisorDirectId
            | F::SupervisorChildGet
            | F::LocalPidSupervisorChildGet
            | F::SupervisorNestedGet
            | F::SupervisorPoolChildGet
            | F::LocalPidSupervisorPoolChildRefGet
            | F::SupervisorPoolLen
            | F::SupervisorStop
            | F::SupervisorRestartAwaitBlocking
            | F::TcpAttachLocal
            | F::TlsAttachLocal
            | F::WebSocketAttachLocal
            | F::TaskAwaitBlocking
            | F::TaskCompleteThreaded
            | F::TaskCompletionObserve
            | F::TaskCompletionUnobserve
            | F::GeneratorFree
            | F::TaskFree
            | F::TaskGetEnv
            | F::TaskGetError
            | F::TaskGetResult
            | F::TaskNew
            | F::TaskScopeCancelAfterNs
            | F::TaskScopeDestroy
            | F::TaskScopeJoinAll
            | F::TaskScopeNew
            | F::TaskScopeSetCurrent
            | F::TaskScopeSpawn
            | F::TaskSetEnv
            | F::TaskSetResult
            | F::TaskSpawnThread
            | F::VecCloneLayout
            | F::VecCloneOwned
            | F::VecTakeAll
            | F::VecAppend
            | F::VecClear
            | F::VecClone
            | F::VecContainsLayout
            | F::VecContainsOwned
            | F::VecContainsScalar(_)
            | F::VecGet(_)
            | F::VecIsEmpty
            | F::VecJoinStr
            | F::VecLen
            | F::VecNew
            | F::VecPopBool
            | F::VecPopLayout
            | F::VecPopOwned
            | F::VecPushBool
            | F::VecPushLayout
            | F::VecPushOwned
            | F::VecPushOwnedMove
            | F::VecScalar { .. }
            | F::VecRemoveAtBool
            | F::VecRemoveAtLayout
            | F::VecRemoveAtOwned
            | F::VecSetBool
            | F::VecSetLayout
            | F::VecSetOwned
            | F::VecSetOwnedMove
            | F::VecSliceRange(_)
            | F::VtableDispatchPanicOnOob => None,
        }
    }

    /// True iff the family's variant carries no element-type degree of
    /// freedom beyond the variant itself (i.e. the descriptor's `elem`
    /// field MUST be `None` for this family). Used by
    /// [`RuntimeCallDescriptor::new`] to enforce fail-closed
    /// construction.
    ///
    /// Today, EVERY variant returns `false`-meaning-"forbids-elem"
    /// because every element-type discriminant is embedded in the
    /// variant payload (the descriptor's optional `elem` field is
    /// pre-staged for the pending Vec genericisation work). When a
    /// `VecGetGeneric` variant lands, that one variant returns `true`
    /// here.
    #[must_use]
    #[allow(
        clippy::unused_self,
        reason = "substrate: every variant currently forbids a separate \
                                                `ResolvedTy` elem. When the pending genericisation work's first generic variant \
                                                lands (e.g. `VecGetGeneric`), this becomes a real per-variant match."
    )]
    fn accepts_elem(self) -> bool {
        // Closed-set fail-closed: no current variant accepts a separate
        // `ResolvedTy` elem; the pending genericisation work is where
        // this opens up. The future genericisation is additive:
        // a new `VecGetGeneric` variant returns `true` here and the
        // constructor admits `Some(elem)` for it without changing the
        // bijection invariant for the closed-set variants.
        false
    }
}

/// The C entry's return type, when it is not simply the result storage's own
/// representation.
///
/// A runtime entry that answers a question returns a C truth whose width is
/// its own business; the language's `bool` storage is one byte. The backend
/// narrows and widens from this field rather than from a per-operation arm.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeCReturn {
    /// The C return is the result storage's representation, or void.
    Storage,
    /// A 32-bit status: nonzero is true.
    TruthI32,
    /// A 1-bit truth.
    TruthBool,
}

/// How physical MIR realizes one runtime operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimePhysicalForm {
    /// Some other physical construct realizes the operation - a suspend or
    /// actor terminator, a drop descriptor, or a backend intercept - so it
    /// never becomes a runtime action.
    NotAnAction,
    /// A runtime action carrying nothing beyond the call's own storage.
    Direct,
    /// A runtime action whose result is one tagged variant built in place.
    VariantResult,
    /// A runtime action returning a `(receiver, Option<element>)` pair.
    PairWithOption,
    /// `Result<string, Utf8Error>`, with the error aggregate and the variant
    /// carrying its length.
    Utf8Decode,
    /// A `Result` over two variant glues.
    NodeResult,
    /// Vector glue, shared by the fixed-size array operations.
    Vector,
    Map,
    Set,
    /// `Rc` glue: the payload's own layout and release recipe. `Rc.new` hands
    /// that recipe to the runtime as the allocation's destructor, `Rc.get`
    /// loads the payload back out of the shared allocation and `Rc.set`
    /// stages a replacement for the runtime to swap in.
    SharedHandle,
}

/// How the backend reaches one runtime operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeStaging {
    /// An ordinary declared call to the row's linker symbol.
    Declared,
    /// Codegen intercepts the call by callee identity and materializes the ABI
    /// itself, so the row's symbol is not a name the linker resolves at the
    /// call site.
    PreStaged,
}

/// Everything the compiler knows about one runtime operation, in one place.
///
/// The linker symbol and the reverse lookup from it, the semantic contract
/// ownership SIR verifies, whether codegen pre-stages the call, and the
/// collection marshalling shape are all fields here rather than separate
/// per-operation tables that can disagree. Adding an operation is a row and a
/// runtime function; a row missing a field does not compile.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RuntimeOpRow {
    /// The C linker symbol, or the catalog identity for a constructor whose
    /// ABI the backend materializes from the destination type.
    pub symbol: &'static str,
    /// The exact operation contract ownership SIR admits. `None` fails closed:
    /// the operation publishes no verified ownership surface.
    pub contract: Option<RuntimeSemanticContract>,
    pub staging: RuntimeStaging,
    pub abi_shape: RuntimeCallAbiShape,
    pub physical: RuntimePhysicalForm,
    pub c_return: RuntimeCReturn,
}

/// ABI-routing shape for collection calls that require bespoke codegen.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeCallAbiShape {
    VecBool,
    /// An ordinary scalar Vec entry that uses the typed function declaration
    /// path rather than bespoke ABI marshalling.
    VecScalarDirect,
    VecI32GetSet,
    VecConstructor,
    VecLayout,
    VecOwned,
    HashCollectionLayoutOp,
    HashMapLayoutGet,
    HashCollectionConstructor,
    BytesConstructor,
    Other,
}

/// Async-suspending behaviour classification. One variant per HIR
/// await-classifier flavour in `hew-hir/src/lower.rs` that still flows
/// through `RuntimeCallFamily`: `is_stream_send_await` and the
/// duplex-close await arm. The channel/stream recv awaits ride the
/// element-layout-witness `*_layout` symbols outside this enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AsyncSuspendKind {
    NativeIo(AsyncIoOp),
    /// `await sink.send(x)` over any describable `Sink<T>` element —
    /// `hew_sink_write_bytes`, `hew_sink_write_string`, or the
    /// layout-witness `hew_stream_send_layout`. All three share the
    /// backpressure-aware suspend ramp; codegen picks the concrete
    /// runtime entry from the value's `ResolvedTy`.
    SinkSend,
    /// `actor.close()` over a lambda-actor `Duplex` →
    /// `hew_duplex_close`.
    DuplexClose,
    /// `await rx.recv()` over a `std::channel` `Receiver<T>` →
    /// `hew_channel_recv_layout`. Suspends only in execution-context
    /// callers; a context-free caller keeps the blocking call.
    ChannelRecv,
    /// `await stream.recv()` over a `Stream<T>` →
    /// `hew_stream_next_layout`. Same context gating as `ChannelRecv`.
    StreamRecv,
}

// =============================================================================
// RuntimeCallDescriptor — typed cross-layer carrier
// =============================================================================

/// Typed cross-layer carrier for a compiler-known runtime / builtin call.
///
/// Fields are private and access is mediated by accessor methods so
/// construction is only possible via [`RuntimeCallDescriptor::new`],
/// which enforces the family↔elem consistency invariant. LESSONS P0
/// `boundary-fail-closed`: invalid combinations refuse to construct in
/// every build profile.
///
/// The `elem` field is pre-staged for the pending Vec/HashMap
/// genericisation work; today every legal `(family, elem)`
/// has `elem = None` and the embedded discriminator in the variant
/// payload (e.g. [`VecGetElem`]) carries the element identity.
///
/// Equality is value-equality (derive); hashing is fine because all
/// fields are hashable. Cloning is cheap (`Copy` on the family enum;
/// `ResolvedTy::Clone` for the elem if populated).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeCallDescriptor {
    family: RuntimeCallFamily,
    elem: Option<ResolvedTy>,
}

impl RuntimeCallDescriptor {
    /// Construct a validated descriptor.
    ///
    /// # Errors
    ///
    /// Returns [`DescriptorError::UnexpectedElem`] when `elem` is `Some`
    /// for a family whose variant payload already encodes the element
    /// identity. This is the substrate's fail-closed guarantee: a future
    /// producer that incorrectly threads a `ResolvedTy` through to a
    /// non-generic family will hard-fail at construction rather than
    /// silently store the unused elem.
    ///
    /// When the pending genericisation work lands its first
    /// element-type-generic variant (e.g. `VecGetGeneric`), that
    /// variant will return `true` from
    /// `RuntimeCallFamily::accepts_elem` and the constructor will
    /// require `Some(elem)` for it (symmetric fail-closed).
    pub fn new(
        family: RuntimeCallFamily,
        elem: Option<ResolvedTy>,
    ) -> Result<Self, DescriptorError> {
        match (family.accepts_elem(), &elem) {
            (false, Some(_)) => Err(DescriptorError::UnexpectedElem { family }),
            (true, None) => Err(DescriptorError::MissingElem { family }),
            _ => Ok(Self { family, elem }),
        }
    }

    /// The family discriminator.
    #[must_use]
    pub fn family(&self) -> RuntimeCallFamily {
        self.family
    }

    /// The checker-resolved element type, if the family accepts one.
    #[must_use]
    pub fn elem(&self) -> Option<&ResolvedTy> {
        self.elem.as_ref()
    }

    /// The C-ABI symbol the family lowers to. Delegates to
    /// [`RuntimeCallFamily::c_symbol`].
    #[must_use]
    pub fn c_symbol(&self) -> &'static str {
        self.family.c_symbol()
    }

    /// True iff calling this descriptor consumes the receiver handle.
    /// Delegates to [`RuntimeCallFamily::consumes_receiver`].
    #[must_use]
    pub fn consumes_receiver(&self) -> bool {
        self.family.consumes_receiver()
    }

    /// Async-suspending classification. Delegates to
    /// [`RuntimeCallFamily::is_async_suspending`].
    #[must_use]
    pub fn is_async_suspending(&self) -> Option<AsyncSuspendKind> {
        self.family.is_async_suspending()
    }
}

/// Construction error for [`RuntimeCallDescriptor::new`].
///
/// Carries the offending family so callers can emit diagnostics naming
/// the exact misuse (`MirDiagnosticKind::NotYetImplemented`, codegen
/// assertions, unit-test assertions).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DescriptorError {
    /// `elem` is `Some` but the family's variant payload already encodes
    /// the element identity (the elem field is meaningless / a fail-
    /// closed signal of misuse).
    UnexpectedElem { family: RuntimeCallFamily },
    /// `elem` is `None` but the family is element-type-generic and the
    /// elem MUST be supplied. Reserved for the pending genericisation
    /// work; no current family triggers this arm.
    MissingElem { family: RuntimeCallFamily },
}

impl std::fmt::Display for DescriptorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedElem { family } => write!(
                f,
                "RuntimeCallDescriptor: family {family:?} does not accept an `elem`; \
                 the variant payload already encodes the element identity",
            ),
            Self::MissingElem { family } => write!(
                f,
                "RuntimeCallDescriptor: family {family:?} is element-type-generic and \
                 requires `Some(elem)`",
            ),
        }
    }
}

impl std::error::Error for DescriptorError {}

// =============================================================================
// RuntimeDropDescriptor — typed mirror of runtime_drop_symbol's table
// =============================================================================

/// Closed-set descriptor for compiler-known runtime drop entries. Mirrors
/// the `runtime_drop_symbol` table in `hew-codegen-rs/src/llvm.rs:18352`
/// (today: `Duplex::close`, `Stream::close`, `Sink::close`,
/// `Sender::close`, `Receiver::close`,
/// `SendHalf::close | RecvHalf::close`, `CancellationToken::release`).
///
/// `non_exhaustive` is INTENTIONALLY OMITTED — same exhaustiveness
/// argument as [`RuntimeCallFamily`]. The codegen-internal "literal
/// C-ABI symbol pass-through" arms in `runtime_drop_symbol` (for hand-
/// built test MIR that pre-dates elaborated-drop-plan consumption)
/// are NOT mirrored here; a follow-up migrates the test MIR sites to typed
/// variants and the pass-through dies with the string field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeDropDescriptor {
    /// `Duplex::close` → `hew_duplex_close`. Two-way close.
    DuplexClose,
    /// `Stream::close` → `hew_stream_close`. Element-type-independent
    /// at the ABI level; the type checker's builtin-method table emits
    /// the same `drop_fn` regardless of element type.
    StreamClose,
    /// `Sink::close` → `hew_sink_close`.
    SinkClose,
    /// `Sender::close` → `hew_channel_sender_close`.
    SenderClose,
    /// `Receiver::close` → `hew_channel_receiver_close`.
    ReceiverClose,
    /// `SendHalf::close` → `hew_duplex_close_half`. Direction
    /// discriminant materialised at the call site from the Place
    /// variant (`SendHalf` vs `RecvHalf`), not encoded in the symbol.
    SendHalfClose,
    /// `RecvHalf::close` → `hew_duplex_close_half`. Shares C-symbol with
    /// `SendHalfClose`; the two descriptor variants exist to preserve
    /// the typed direction information cross-layer.
    RecvHalfClose,
    /// `CancellationToken::release` → `hew_cancel_token_release`.
    CancellationTokenRelease,
    /// `MonitorRef::close` → `hew_actor_demonitor`. Extracts `ref_id: i64`
    /// from the struct and passes it directly to the runtime.
    MonitorRefClose,
}

/// The exact operand shape consumed by a runtime resource-close descriptor.
///
/// This stays coupled to [`RuntimeDropDescriptor`], rather than inferred from
/// its C symbol: two descriptors may share a symbol while carrying different
/// typed operands (`SendHalf` versus `RecvHalf`), and `MonitorRef` owns an
/// inline record slot rather than a pointer handle.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeDropOperandShape {
    /// One opaque heap-handle pointer.
    HandlePtr,
    /// One opaque half-handle pointer plus the typed duplex direction.
    DuplexHalf { direction: DuplexHalfDropDirection },
    /// The `ref_id: i64` field in an inline `MonitorRef` record.
    MonitorRefId,
}

/// Direction materialised for the shared `hew_duplex_close_half` ABI.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DuplexHalfDropDirection {
    Send,
    Recv,
}

impl DuplexHalfDropDirection {
    /// Runtime ABI discriminant for `hew_duplex_close_half`.
    #[must_use]
    pub const fn runtime_discriminant(self) -> u64 {
        match self {
            Self::Send => 0,
            Self::Recv => 1,
        }
    }
}

impl RuntimeDropDescriptor {
    /// The typed builtin identity whose scope-exit close this descriptor
    /// represents. Internal ABI aliases share their public handle family's
    /// descriptor, so snapshot/drop planning never has to recover lifecycle
    /// from a presentation name.
    #[must_use]
    pub const fn for_builtin(builtin: BuiltinType) -> Option<Self> {
        match builtin {
            BuiltinType::Duplex | BuiltinType::HewDuplex => Some(Self::DuplexClose),
            BuiltinType::Stream => Some(Self::StreamClose),
            BuiltinType::Sink => Some(Self::SinkClose),
            BuiltinType::Sender => Some(Self::SenderClose),
            BuiltinType::Receiver => Some(Self::ReceiverClose),
            BuiltinType::SendHalf | BuiltinType::HewSendHalf => Some(Self::SendHalfClose),
            BuiltinType::RecvHalf | BuiltinType::HewRecvHalf => Some(Self::RecvHalfClose),
            BuiltinType::CancellationToken => Some(Self::CancellationTokenRelease),
            BuiltinType::MonitorRef => Some(Self::MonitorRefClose),
            _ => None,
        }
    }

    /// The slot/operand ABI required by this close ritual.
    #[must_use]
    pub const fn operand_shape(self) -> RuntimeDropOperandShape {
        match self {
            Self::SendHalfClose => RuntimeDropOperandShape::DuplexHalf {
                direction: DuplexHalfDropDirection::Send,
            },
            Self::RecvHalfClose => RuntimeDropOperandShape::DuplexHalf {
                direction: DuplexHalfDropDirection::Recv,
            },
            Self::MonitorRefClose => RuntimeDropOperandShape::MonitorRefId,
            Self::DuplexClose
            | Self::StreamClose
            | Self::SinkClose
            | Self::SenderClose
            | Self::ReceiverClose
            | Self::CancellationTokenRelease => RuntimeDropOperandShape::HandlePtr,
        }
    }

    /// The C-ABI runtime symbol the drop lowers to. NB
    /// [`RuntimeDropDescriptor::SendHalfClose`] and
    /// [`RuntimeDropDescriptor::RecvHalfClose`] share
    /// `hew_duplex_close_half`; this is intentional — the bijection is
    /// over (drop descriptor → symbol), not (symbol → descriptor).
    #[must_use]
    pub fn c_symbol(self) -> &'static str {
        match self {
            Self::DuplexClose => "hew_duplex_close",
            Self::StreamClose => "hew_stream_close",
            Self::SinkClose => "hew_sink_close",
            Self::SenderClose => "hew_channel_sender_close",
            Self::ReceiverClose => "hew_channel_receiver_close",
            Self::SendHalfClose | Self::RecvHalfClose => "hew_duplex_close_half",
            Self::CancellationTokenRelease => "hew_cancel_token_release",
            Self::MonitorRefClose => "hew_actor_demonitor",
        }
    }

    /// The producer-side method-name spelling (`<Type>::<method>`), the
    /// round-trip key of the descriptor: unlike `c_symbol()` (where the
    /// two half-close variants share a symbol), every variant has a
    /// unique name, so [`RuntimeDropDescriptor::from_drop_fn_name`] is a
    /// true inverse.
    #[must_use]
    pub fn drop_fn_name(self) -> &'static str {
        match self {
            Self::DuplexClose => "Duplex::close",
            Self::StreamClose => "Stream::close",
            Self::SinkClose => "Sink::close",
            Self::SenderClose => "Sender::close",
            Self::ReceiverClose => "Receiver::close",
            Self::SendHalfClose => "SendHalf::close",
            Self::RecvHalfClose => "RecvHalf::close",
            Self::CancellationTokenRelease => "CancellationToken::release",
            Self::MonitorRefClose => "MonitorRef::close",
        }
    }

    /// Inverse of [`RuntimeDropDescriptor::drop_fn_name`]: lift a
    /// type-class-derived `<Type>::<method>` close name into the typed
    /// descriptor. Returns `None` for user `#[resource]` close methods
    /// (`MyType::close`) — the open-set arm of the drop-dispatch split.
    /// MIR's drop elaboration uses this lift to classify every close
    /// ritual at production; the `c_symbol` is then born at codegen from
    /// the descriptor.
    #[must_use]
    pub fn from_drop_fn_name(name: &str) -> Option<Self> {
        match name {
            "Duplex::close" => Some(Self::DuplexClose),
            "Stream::close" => Some(Self::StreamClose),
            "Sink::close" => Some(Self::SinkClose),
            "Sender::close" => Some(Self::SenderClose),
            "Receiver::close" => Some(Self::ReceiverClose),
            "SendHalf::close" => Some(Self::SendHalfClose),
            "RecvHalf::close" => Some(Self::RecvHalfClose),
            "CancellationToken::release" => Some(Self::CancellationTokenRelease),
            "MonitorRef::close" => Some(Self::MonitorRefClose),
            _ => None,
        }
    }
}

// =============================================================================
// Test-only enumeration helpers
// =============================================================================

/// Enumerate every legal [`RuntimeCallFamily`] value — compiler-complete.
///
/// The outer variant set is sourced from the `EnumIter` derive, so a new
/// `RuntimeCallFamily` variant is enumerated automatically and cannot be
/// silently dropped from the catalog. The no-wildcard `match` below expands
/// each payload-bearing variant to its full cross-product via the inner
/// enum's own `EnumIter`; adding a payload variant makes that match
/// non-exhaustive (a compile error) until it is expanded here.
///
/// This is what closes focal-7's pre-staged-family gap: a family that rides
/// `Terminator::Call` (its `c_symbol()` absent from
/// `known_runtime_symbols`, so untouched by
/// `every_allowlist_symbol_has_a_family`) is still counted by this catalog,
/// hence by the corpus-coverage and bijection tests below. No hand-maintained
/// row to forget.
///
/// The bijection / round-trip tests in `tests` below and in
/// `hew-mir/tests/runtime_call_allowlist.rs` iterate this list to assert
/// every variant has a unique `c_symbol()` and (where applicable) lies in
/// `known_runtime_symbols`.
#[must_use]
pub fn all_runtime_call_families() -> Vec<RuntimeCallFamily> {
    use RuntimeCallFamily as F;
    let mut out = Vec::new();
    for repr in F::iter() {
        match repr {
            F::Print { .. } => {
                for kind in PrintKind::iter() {
                    out.extend([false, true].map(|newline| F::Print { kind, newline }));
                }
            }
            F::AsyncIo(_) => out.extend(AsyncIoOp::iter().map(F::AsyncIo)),
            F::Encoding { .. } => {
                for format in EncodingFormat::iter() {
                    out.extend(EncodingOp::iter().map(|op| F::Encoding { format, op }));
                }
            }
            F::FileRead(_) => out.extend(FileReadOp::iter().map(F::FileRead)),
            F::Tcp(_) => out.extend(TcpOp::iter().map(F::Tcp)),
            F::Array(_) => out.extend(ArrayValueOp::iter().map(F::Array)),
            F::SupervisorPool(_) => {
                out.extend(SupervisorPoolOp::iter().map(F::SupervisorPool));
            }
            F::Vector(_) => out.extend(VecValueOp::iter().map(F::Vector)),
            F::Map(_) => out.extend(MapValueOp::iter().map(F::Map)),
            F::Set(_) => out.extend(SetValueOp::iter().map(F::Set)),
            F::MathIntrinsic(_) => out.extend(MathIntrinsic::iter().map(F::MathIntrinsic)),
            F::IntMethod(_, _) => {
                out.extend(IntBitOp::iter().flat_map(|op| {
                    IntMethodWidth::iter().map(move |width| F::IntMethod(op, width))
                }));
            }
            F::IntArith(_, _) => out.extend(IntArithKind::iter().flat_map(|kind| {
                IntMethodWidth::iter().map(move |width| F::IntArith(kind, width))
            })),
            F::FloatMethod(_) => out.extend(FloatMethodOp::iter().map(F::FloatMethod)),
            F::SinkWrite(_) => out.extend(StreamElementKind::iter().map(F::SinkWrite)),
            F::SinkTryWrite(_) => out.extend(StreamElementKind::iter().map(F::SinkTryWrite)),
            F::VecContainsScalar(_) => out.extend(all_vec_contains_scalar_families()),
            F::VecGet(_) => out.extend(VecGetElem::iter().map(F::VecGet)),
            F::VecScalar { .. } => out.extend(all_vec_scalar_families()),
            F::VecSliceRange(_) => out.extend(VecSliceElem::iter().map(F::VecSliceRange)),
            // Nullary variants carry no payload: emit `EnumIter`'s single
            // representative as-is.
            nullary => out.push(nullary),
        }
    }
    out
}

/// Enumerate the complete closed scalar Vec operation matrix.
///
/// This is exported for producer tests in downstream compiler crates: those
/// tests must derive their cases from the catalog instead of duplicating an
/// element list that can drift when the matrix grows.
#[must_use]
pub fn all_vec_scalar_families() -> Vec<RuntimeCallFamily> {
    VecScalarOp::iter()
        .flat_map(|op| {
            VecScalarElem::iter().map(move |elem| RuntimeCallFamily::VecScalar { op, elem })
        })
        .collect()
}

/// Enumerate the closed scalar `Vec::contains` entries.
#[must_use]
pub fn all_vec_contains_scalar_families() -> Vec<RuntimeCallFamily> {
    VecContainsScalarElem::iter()
        .map(RuntimeCallFamily::VecContainsScalar)
        .collect()
}

/// Enumerate every legal [`RuntimeDropDescriptor`] variant.
///
/// See the bijection / parity tests; same coverage discipline as
/// [`all_runtime_call_families`].
#[must_use]
pub fn all_runtime_drop_descriptors() -> [RuntimeDropDescriptor; 9] {
    [
        RuntimeDropDescriptor::DuplexClose,
        RuntimeDropDescriptor::StreamClose,
        RuntimeDropDescriptor::SinkClose,
        RuntimeDropDescriptor::SenderClose,
        RuntimeDropDescriptor::ReceiverClose,
        RuntimeDropDescriptor::SendHalfClose,
        RuntimeDropDescriptor::RecvHalfClose,
        RuntimeDropDescriptor::CancellationTokenRelease,
        RuntimeDropDescriptor::MonitorRefClose,
    ]
}

/// True when codegen intercepts the call by callee identity instead of
/// declaring the row's symbol at the call site.
#[must_use]
pub const fn is_pre_staged_family(family: RuntimeCallFamily) -> bool {
    matches!(family.row().staging, RuntimeStaging::PreStaged)
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn named_type(name: &str, builtin: Option<crate::BuiltinType>) -> ResolvedTy {
        ResolvedTy::Named {
            name: name.to_string(),
            args: Vec::new(),
            builtin,
            is_opaque: false,
        }
    }

    #[test]
    fn utf8_decode_result_contract_requires_exact_nominal_error_identity() {
        let exact_error = named_type("std.encoding.utf8.Utf8Error", None);
        let exact_result = ResolvedTy::Named {
            name: "Result".to_string(),
            args: vec![ResolvedTy::String, exact_error.clone()],
            builtin: Some(crate::BuiltinType::Result),
            is_opaque: false,
        };
        assert!(RuntimeVariantResultKind::Utf8Decode.matches(&exact_result));

        let lookalike_error = named_type("application.Utf8Error", None);
        let lookalike_result = ResolvedTy::Named {
            name: "Result".to_string(),
            args: vec![ResolvedTy::String, lookalike_error],
            builtin: Some(crate::BuiltinType::Result),
            is_opaque: false,
        };
        assert!(!RuntimeVariantResultKind::Utf8Decode.matches(&lookalike_result));

        let user_result = ResolvedTy::Named {
            name: "application.Result".to_string(),
            args: vec![ResolvedTy::String, exact_error],
            builtin: None,
            is_opaque: false,
        };
        assert!(!RuntimeVariantResultKind::Utf8Decode.matches(&user_result));
    }

    #[test]
    fn new_text_runtime_families_publish_closed_semantic_effects() {
        let decode = RuntimeCallFamily::BytesDecodeUtf8
            .semantic_contract()
            .expect("validating decode must have an ownership-SIR contract");
        assert_eq!(
            decode.arguments,
            &[RuntimeArgumentContract {
                ty: RuntimeValueKind::Bytes,
                effect: RuntimeArgumentEffect::Borrow,
            }]
        );
        assert_eq!(
            decode.result,
            RuntimeResultEffect::FreshOwnedVariant(RuntimeVariantResultKind::Utf8Decode)
        );
        assert!(decode.failures.is_empty());

        let lossy = RuntimeCallFamily::BytesDecodeUtf8Lossy
            .semantic_contract()
            .expect("lossy decode must have an ownership-SIR contract");
        assert_eq!(lossy.arguments, decode.arguments);
        assert_eq!(
            lossy.result,
            RuntimeResultEffect::FreshOwned(RuntimeValueKind::String)
        );
        assert!(lossy.failures.is_empty());

        assert!(decode.matches_signature(
            &[ResolvedTy::Bytes],
            &ResolvedTy::Named {
                name: "Result".to_string(),
                args: vec![
                    ResolvedTy::String,
                    named_type("std.encoding.utf8.Utf8Error", None),
                ],
                builtin: Some(crate::BuiltinType::Result),
                is_opaque: false,
            },
        ));
        assert!(!decode.matches_signature(&[ResolvedTy::String], &ResolvedTy::String));
        assert!(lossy.matches_signature(&[ResolvedTy::Bytes], &ResolvedTy::String));

        let byte_len = RuntimeCallFamily::StringByteLen
            .semantic_contract()
            .expect("string byte length must have an ownership-SIR contract");
        assert_eq!(
            byte_len.arguments,
            &[RuntimeArgumentContract {
                ty: RuntimeValueKind::String,
                effect: RuntimeArgumentEffect::Borrow,
            }]
        );
        assert_eq!(
            byte_len.result,
            RuntimeResultEffect::BitCopy(RuntimeValueKind::I64)
        );
        assert!(byte_len.failures.is_empty());
    }

    use std::collections::{HashMap, HashSet};

    #[test]
    fn fixed_array_update_preserves_length_and_element_ownership() {
        let array = ResolvedTy::Array(Box::new(ResolvedTy::String), 2);
        let family = RuntimeCallFamily::Array(ArrayValueOp::Set);
        let contract = family.semantic_contract().unwrap();
        let arguments = [array.clone(), ResolvedTy::I64, ResolvedTy::String];
        assert!(contract.matches_signature(&arguments, &array));
        assert!(!contract.matches_signature(
            &arguments,
            &ResolvedTy::Array(Box::new(ResolvedTy::String), 3)
        ));
        assert!(
            !contract.matches_signature(&[array.clone(), ResolvedTy::I64, ResolvedTy::I64], &array)
        );
        assert!(!contract.matches_signature(
            &[array.clone(), ResolvedTy::U64, ResolvedTy::String],
            &array
        ));
        assert!(!contract.matches_signature(&arguments, &ResolvedTy::Unit));

        // Updating replaces the array owner; the index is copied and an
        // owning element follows its copy-or-move value boundary.
        assert!(family.consumes_receiver());
        assert_eq!(family.arg_consume_verdict(0), ConsumeVerdict::ProvenConsume);
        assert_eq!(family.arg_consume_verdict(1), ConsumeVerdict::ProvenBorrow);
        assert_eq!(
            family.arg_consume_verdict(2),
            ConsumeVerdict::ConservativeConsume
        );
    }

    #[test]
    fn scalar_vec_result_ownership_is_family_keyed_and_total() {
        for family in all_vec_scalar_families() {
            let RuntimeCallFamily::VecScalar { op, elem } = family else {
                unreachable!("the scalar Vec matrix only yields VecScalar families");
            };
            let expected = if matches!(op, VecScalarOp::Pop | VecScalarOp::RemoveAt)
                && elem == VecScalarElem::Str
            {
                RuntimeResultOwnership::FreshOwnedString
            } else {
                RuntimeResultOwnership::Untracked
            };
            assert_eq!(family.result_ownership(), expected, "{family:?}");
        }
        for family in all_vec_contains_scalar_families() {
            assert_eq!(family.result_ownership(), RuntimeResultOwnership::Untracked);
        }
        assert_eq!(
            RuntimeCallFamily::VecClone.result_ownership(),
            RuntimeResultOwnership::FreshOwnedVec
        );
        assert_eq!(
            RuntimeCallFamily::VecNew.result_ownership(),
            RuntimeResultOwnership::FreshOwnedVec
        );
        assert_eq!(
            RuntimeCallFamily::VecJoinStr.result_ownership(),
            RuntimeResultOwnership::FreshOwnedString
        );
        assert_eq!(
            RuntimeCallFamily::BytesNew.result_ownership(),
            RuntimeResultOwnership::FreshOwnedBytes
        );
    }

    /// `from_c_symbol` reads the row table's symbol column backwards, so a
    /// shared symbol silently costs an operation its reverse lookup. The print
    /// operations are the one sanctioned sharing: their element type and
    /// newline flag ride beside the symbol. Anything else sharing a symbol is
    /// a table defect.
    #[test]
    fn only_the_print_operations_share_a_c_symbol() {
        let mut seen: HashMap<&'static str, RuntimeCallFamily> = HashMap::new();
        for family in all_runtime_call_families() {
            let symbol = family.row().symbol;
            if let Some(previous) = seen.insert(symbol, family) {
                assert!(
                    matches!(family, RuntimeCallFamily::Print { .. })
                        && matches!(previous, RuntimeCallFamily::Print { .. }),
                    "{previous:?} and {family:?} both claim {symbol:?}, so neither \
                     lifts back out of a C symbol"
                );
            }
        }
    }

    #[test]
    fn canonical_stdlib_extern_descriptors_agree_with_runtime_symbols() {
        for entry in canonical_std_io_extern_signatures() {
            if let Some(family) = entry.family {
                assert_eq!(family.c_symbol(), entry.symbol, "{entry:?}");
            }
        }
    }

    /// Every operation lifts back out of its own symbol.
    /// `record_runtime_method_call_rewrite` turns a checker-resolved C symbol
    /// into a typed descriptor this way, so an operation the reverse lookup
    /// drops is unreachable from that producer.
    #[test]
    fn runtime_call_family_round_trips_through_from_c_symbol() {
        for family in all_runtime_call_families() {
            let sym = family.c_symbol();
            let back = RuntimeCallFamily::from_c_symbol(sym);
            if matches!(family, RuntimeCallFamily::Print { .. }) {
                assert_eq!(
                    back, None,
                    "a print ABI symbol cannot infer its type or newline flag"
                );
                continue;
            }
            assert_eq!(
                back,
                Some(family),
                "from_c_symbol({sym:?}) returned {back:?}, expected Some({family:?}) \
                 — the inverse arm is missing or wrong",
            );
        }
    }

    #[test]
    fn mir_builtin_lift_excludes_codegen_only_partitions() {
        assert_eq!(
            RuntimeCallFamily::from_mir_builtin_symbol("hew_vec_push_layout"),
            None
        );
        assert_eq!(
            RuntimeCallFamily::from_mir_builtin_symbol("Node::start"),
            Some(RuntimeCallFamily::NodeStart)
        );
        assert_eq!(
            RuntimeCallFamily::from_mir_builtin_symbol("hew_hashmap_insert_layout"),
            Some(RuntimeCallFamily::HashMapInsertLayout)
        );
    }

    #[test]
    fn runtime_capabilities_are_classified_by_family() {
        assert_eq!(
            RuntimeCallFamily::MetricCounterInc.runtime_capability(),
            Some(RuntimeCapability::Metrics)
        );
        assert_eq!(
            RuntimeCallFamily::NodeStart.runtime_capability(),
            Some(RuntimeCapability::Node)
        );
        assert_eq!(RuntimeCallFamily::DuplexSend.runtime_capability(), None);
    }

    #[test]
    fn symbol_lifting_respects_codegen_partitions_and_required_attributes() {
        use RuntimeCallFamily as F;

        let mut expected: HashSet<RuntimeCallFamily> = [
            F::BytesNew,
            F::HashMapClearLayout,
            F::HashMapCloneLayout,
            F::HashSetClearLayout,
            F::HashSetCloneLayout,
            F::HashSetToVecLayout,
            F::VecAppend,
            F::VecClear,
            F::VecClone,
            F::VecCloneLayout,
            F::VecCloneOwned,
            F::VecTakeAll,
            F::VecContainsLayout,
            F::VecContainsOwned,
            F::VecNew,
            F::VecPopBool,
            F::VecPopLayout,
            F::VecPopOwned,
            F::VecPushBool,
            F::VecPushLayout,
            F::VecPushOwned,
            F::VecPushOwnedMove,
            F::VecRemoveAtBool,
            F::VecRemoveAtLayout,
            F::VecRemoveAtOwned,
            F::VecSetBool,
            F::VecSetLayout,
            F::VecSetOwned,
            F::VecSetOwnedMove,
            F::VecIsEmpty,
            F::VecJoinStr,
        ]
        .into_iter()
        .collect();
        expected.extend(
            VecScalarOp::iter()
                .flat_map(|op| VecScalarElem::iter().map(move |elem| F::VecScalar { op, elem })),
        );
        expected.extend(VecContainsScalarElem::iter().map(F::VecContainsScalar));
        let actual: HashSet<RuntimeCallFamily> = all_runtime_call_families()
            .into_iter()
            .filter(|family| family.is_codegen_partition_only())
            .collect();
        assert_eq!(
            actual, expected,
            "the codegen-only collection partition changed; review whether each \
             affected family should remain absent from MIR calls"
        );

        for family in all_runtime_call_families() {
            assert_eq!(
                RuntimeCallFamily::from_mir_builtin_symbol(family.c_symbol()).is_none(),
                expected.contains(&family) || matches!(family, F::Print { .. }),
                "MIR carrier classification drifted for {family:?}"
            );
        }
    }

    /// `from_c_symbol` returns `None` for strings the catalog does
    /// not enumerate (open-set extern FFI symbols, user-trait method
    /// keys like `i64::fmt`, garbage).
    #[test]
    fn from_c_symbol_rejects_unknown_strings() {
        assert!(RuntimeCallFamily::from_c_symbol("not_a_runtime_symbol").is_none());
        assert!(RuntimeCallFamily::from_c_symbol("").is_none());
        // User-trait method keys live in `RewriteToFunction.c_symbol` as
        // `Type::method` strings; they are open-set and MUST be rejected
        // so the typed-descriptor path leaves them alone.
        assert!(RuntimeCallFamily::from_c_symbol("i64::fmt").is_none());
        assert!(RuntimeCallFamily::from_c_symbol("MyType::greet").is_none());
    }

    /// `is_async_suspending` returns `Some(_)` for EXACTLY the symbols
    /// the HIR await-classifier discriminates through `RuntimeCallFamily`
    /// today: the three sink-send families (`hew_sink_write_bytes`,
    /// `hew_sink_write_string`, `hew_stream_send_layout`) plus
    /// `hew_duplex_close`, `hew_channel_recv_layout`, and
    /// `hew_stream_next_layout`. All three sink-send families share the
    /// single `SinkSend` kind — codegen picks the concrete runtime entry
    /// from the value's `ResolvedTy`.
    /// Locks the consumer contract for the eventual migration.
    ///
    /// Positive: the symbols listed map to the matching
    /// `AsyncSuspendKind`. Negative: every other family returns `None`,
    /// pinned by enumeration via `all_runtime_call_families`. ESP. the
    /// `try_*` peers (`SinkTryWrite`, `ChannelSendLayout`, `DuplexSend`,
    /// …) MUST stay non-suspending: those never touch the backpressure
    /// ramp.
    #[test]
    fn async_suspension_classification_preserves_operation_identity() {
        use RuntimeCallFamily as F;

        // Positive: exactly these (family, expected kind) tuples. All
        // three sink-send families share the single `SinkSend` kind.
        let positives: &[(RuntimeCallFamily, AsyncSuspendKind)] = &[
            (
                F::SinkWrite(StreamElementKind::Bytes),
                AsyncSuspendKind::SinkSend,
            ),
            (
                F::SinkWrite(StreamElementKind::String),
                AsyncSuspendKind::SinkSend,
            ),
            (F::StreamSendLayout, AsyncSuspendKind::SinkSend),
            (F::DuplexClose, AsyncSuspendKind::DuplexClose),
            (F::ChannelRecvLayout, AsyncSuspendKind::ChannelRecv),
            (F::StreamNextLayout, AsyncSuspendKind::StreamRecv),
        ];
        for (family, kind) in positives {
            assert_eq!(
                family.is_async_suspending(),
                Some(*kind),
                "{family:?} must suspend with {kind:?}",
            );
        }

        // Explicit negative regression set: the non-suspending channel
        // send, the recv/send try_* peers, and the close families the
        // classifier never touches. `SinkWrite(String)` and
        // `StreamSendLayout` are NO LONGER here — they suspend now.
        let must_not_suspend: &[RuntimeCallFamily] = &[
            F::ChannelTryRecvLayout,
            F::ChannelSendLayout,
            F::StreamTryNextLayout,
            F::DuplexRecv,
            F::DuplexSend,
            F::DuplexTryRecv,
            F::DuplexTrySend,
            F::SinkTryWrite(StreamElementKind::Bytes),
            F::SinkTryWrite(StreamElementKind::String),
            F::StreamClose,
            F::SinkClose,
            F::DuplexCloseHalf,
        ];
        for family in must_not_suspend {
            assert_eq!(
                family.is_async_suspending(),
                None,
                "{family:?} must NOT suspend per the current HIR await-classifier",
            );
        }

        for op in AsyncIoOp::iter() {
            assert_eq!(
                F::AsyncIo(op).is_async_suspending(),
                Some(AsyncSuspendKind::NativeIo(op))
            );
        }
    }

    /// Fail-closed constructor: passing `Some(elem)` to a family that
    /// does not accept an element-type degree of freedom returns
    /// `Err(UnexpectedElem)`. Pins the substrate's "no silent default"
    /// guarantee.
    #[test]
    fn descriptor_new_rejects_unexpected_elem() {
        let err = RuntimeCallDescriptor::new(RuntimeCallFamily::VecLen, Some(ResolvedTy::I64))
            .expect_err("VecLen must reject Some(elem)");
        assert!(matches!(err, DescriptorError::UnexpectedElem { .. }));

        let err =
            RuntimeCallDescriptor::new(RuntimeCallFamily::DuplexClose, Some(ResolvedTy::Bool))
                .expect_err("DuplexClose must reject Some(elem)");
        assert!(matches!(err, DescriptorError::UnexpectedElem { .. }));

        // Symmetric: every variant accepts `None`.
        for family in all_runtime_call_families() {
            RuntimeCallDescriptor::new(family, None).unwrap_or_else(|e| {
                panic!("descriptor with elem=None must construct for {family:?}: {e}")
            });
        }
    }

    /// The descriptor's accessor methods delegate to the family;
    /// confirm the wire works for one representative variant from
    /// each axis.
    #[test]
    fn descriptor_accessors_delegate_to_family() {
        let d = RuntimeCallDescriptor::new(RuntimeCallFamily::DuplexClose, None).unwrap();
        assert_eq!(d.family(), RuntimeCallFamily::DuplexClose);
        assert_eq!(d.elem(), None);
        assert_eq!(d.c_symbol(), "hew_duplex_close");
        assert!(d.consumes_receiver());
        // DuplexClose is one of the suspending classifier symbols.
        assert_eq!(d.is_async_suspending(), Some(AsyncSuspendKind::DuplexClose));

        // Non-suspending close peer: StreamClose / SinkClose are NOT
        // in the await-classifier set.
        let d = RuntimeCallDescriptor::new(RuntimeCallFamily::StreamClose, None).unwrap();
        assert_eq!(d.c_symbol(), "hew_stream_close");
        assert!(d.consumes_receiver());
        assert_eq!(d.is_async_suspending(), None);

        let d = RuntimeCallDescriptor::new(RuntimeCallFamily::VecLen, None).unwrap();
        assert_eq!(d.c_symbol(), "hew_vec_len");
        assert!(!d.consumes_receiver());
        assert_eq!(d.is_async_suspending(), None);
    }

    // -------------------------------------------------------------------------
    // RuntimeDropDescriptor
    // -------------------------------------------------------------------------

    /// Round-trip parity with `runtime_drop_symbol`'s table in
    /// `hew-codegen-rs/src/llvm.rs:18352`. Hard-coded mirror; a follow-up
    /// migration deletes the string-keyed table and reads
    /// `RuntimeDropDescriptor::c_symbol()` directly.
    #[test]
    fn drop_descriptor_c_symbols_match_codegen_table() {
        use std::collections::HashSet;
        // Mirror of the runtime_drop_symbol table (drop_fn_name → C symbol).
        // Listed here so a future change to either side fails this test
        // loudly (substrate-tests-the-substrate).
        let expected: &[(&str, &str)] = &[
            ("Duplex::close", "hew_duplex_close"),
            ("Stream::close", "hew_stream_close"),
            ("Sink::close", "hew_sink_close"),
            ("Sender::close", "hew_channel_sender_close"),
            ("Receiver::close", "hew_channel_receiver_close"),
            ("SendHalf::close", "hew_duplex_close_half"),
            ("RecvHalf::close", "hew_duplex_close_half"),
            ("CancellationToken::release", "hew_cancel_token_release"),
            ("MonitorRef::close", "hew_actor_demonitor"),
        ];
        let mut by_name: HashMap<&'static str, RuntimeDropDescriptor> = HashMap::new();
        for d in all_runtime_drop_descriptors() {
            by_name.insert(d.drop_fn_name(), d);
        }
        for (name, sym) in expected {
            let d = by_name
                .get(name)
                .unwrap_or_else(|| panic!("missing RuntimeDropDescriptor for {name}"));
            assert_eq!(
                d.c_symbol(),
                *sym,
                "drop descriptor {d:?} c_symbol mismatch"
            );
        }
        // No extra descriptors (the inverse direction): every variant we
        // enumerated must be in the expected table. If a future
        // contributor adds a variant without updating the expected table
        // (and the codegen `runtime_drop_symbol` table), this fires.
        let expected_names: HashSet<&'static str> = expected.iter().map(|(n, _)| *n).collect();
        for d in all_runtime_drop_descriptors() {
            assert!(
                expected_names.contains(d.drop_fn_name()),
                "RuntimeDropDescriptor {d:?} has no entry in the \
                 codegen `runtime_drop_symbol` parity table; add it to \
                 both or remove the variant"
            );
        }
    }

    #[test]
    fn builtin_resource_close_inventory_has_one_typed_descriptor_authority() {
        use BuiltinType::*;

        let inventory = [
            (Duplex, Some(RuntimeDropDescriptor::DuplexClose)),
            (HewDuplex, Some(RuntimeDropDescriptor::DuplexClose)),
            (Stream, Some(RuntimeDropDescriptor::StreamClose)),
            (Sink, Some(RuntimeDropDescriptor::SinkClose)),
            (Sender, Some(RuntimeDropDescriptor::SenderClose)),
            (Receiver, Some(RuntimeDropDescriptor::ReceiverClose)),
            (SendHalf, Some(RuntimeDropDescriptor::SendHalfClose)),
            (HewSendHalf, Some(RuntimeDropDescriptor::SendHalfClose)),
            (RecvHalf, Some(RuntimeDropDescriptor::RecvHalfClose)),
            (HewRecvHalf, Some(RuntimeDropDescriptor::RecvHalfClose)),
            (ActorFn, None),
            (
                CancellationToken,
                Some(RuntimeDropDescriptor::CancellationTokenRelease),
            ),
            (MonitorRef, Some(RuntimeDropDescriptor::MonitorRefClose)),
            // These marker-bearing internal carriers have no executable
            // runtime close contract. They must remain unsupported rather
            // than acquiring one by spelling or marker alone.
            (ActorHandle, None),
            (HewActor, None),
            (BoxedActor, None),
        ];

        for (builtin, expected) in inventory {
            assert_eq!(
                RuntimeDropDescriptor::for_builtin(builtin),
                expected,
                "typed lifecycle inventory drifted for {builtin:?}",
            );
        }
    }

    #[test]
    fn runtime_resource_drop_operands_are_exhaustively_typed() {
        use DuplexHalfDropDirection::{Recv, Send};
        use RuntimeDropDescriptor::*;
        use RuntimeDropOperandShape::{DuplexHalf, HandlePtr, MonitorRefId};

        let expected = [
            (DuplexClose, HandlePtr),
            (StreamClose, HandlePtr),
            (SinkClose, HandlePtr),
            (SenderClose, HandlePtr),
            (ReceiverClose, HandlePtr),
            (SendHalfClose, DuplexHalf { direction: Send }),
            (RecvHalfClose, DuplexHalf { direction: Recv }),
            (CancellationTokenRelease, HandlePtr),
            (MonitorRefClose, MonitorRefId),
        ];

        assert_eq!(all_runtime_drop_descriptors().len(), expected.len());
        for (descriptor, shape) in expected {
            assert_eq!(
                descriptor.operand_shape(),
                shape,
                "operand ABI drifted for {descriptor:?}"
            );
        }
    }

    /// `from_drop_fn_name` is a true inverse of `drop_fn_name` (every
    /// variant has a unique name), and rejects user `<Type>::close`
    /// spellings so the open-set arm stays open.
    #[test]
    fn drop_descriptor_name_round_trips() {
        for d in all_runtime_drop_descriptors() {
            assert_eq!(
                RuntimeDropDescriptor::from_drop_fn_name(d.drop_fn_name()),
                Some(d),
                "drop_fn_name round-trip failed for {d:?}"
            );
        }
        assert!(RuntimeDropDescriptor::from_drop_fn_name("MyType::close").is_none());
        assert!(RuntimeDropDescriptor::from_drop_fn_name("hew_duplex_close").is_none());
        assert!(RuntimeDropDescriptor::from_drop_fn_name("").is_none());
    }

    // The allowlist-coverage parity tests
    // (`allowlist_subset_round_trips`, `every_allowlist_symbol_has_a_family`,
    // `drop_descriptor_symbols_in_allowlist_or_pre_staged`, and
    // `every_c_symbol_resolves_to_a_real_symbol`) require
    // `hew_mir::runtime_symbols::is_known_runtime_symbol`, which lives
    // in `hew-mir`. They moved to `hew-mir/tests/runtime_call_allowlist.rs`
    // alongside the re-export shim and run as integration tests against
    // the same substrate.
}

#[cfg(test)]
mod encoding_tests;

#[cfg(test)]
mod map_set_semantic_contract_tests {
    use super::*;

    fn builtin(kind: BuiltinType, arguments: Vec<ResolvedTy>) -> ResolvedTy {
        ResolvedTy::named_builtin(kind.canonical_name(), kind, arguments)
    }

    #[test]
    fn map_selection_and_projection_keep_exact_nested_value_types() {
        let value = builtin(
            BuiltinType::Vec,
            vec![builtin(
                BuiltinType::Result,
                vec![ResolvedTy::I64, ResolvedTy::String],
            )],
        );
        let map = builtin(
            BuiltinType::HashMap,
            vec![ResolvedTy::String, value.clone()],
        );
        let optional = builtin(BuiltinType::Option, vec![value.clone()]);
        let get = RuntimeCallFamily::Map(MapValueOp::Get)
            .semantic_contract()
            .unwrap();
        assert!(get.matches_signature(&[map.clone(), ResolvedTy::String], &optional));
        assert!(!get.matches_signature(&[map.clone(), ResolvedTy::I64], &optional));
        assert!(!get.matches_signature(
            &[map.clone(), ResolvedTy::String],
            &builtin(BuiltinType::Option, vec![ResolvedTy::String])
        ));
        assert!(!get.matches_signature(
            &[map.clone(), ResolvedTy::String],
            &ResolvedTy::named_user("Option", vec![value.clone()])
        ));

        let entries = RuntimeCallFamily::Map(MapValueOp::Entries)
            .semantic_contract()
            .unwrap();
        let pairs = builtin(
            BuiltinType::Vec,
            vec![ResolvedTy::Tuple(vec![ResolvedTy::String, value.clone()])],
        );
        assert!(entries.matches_signature(std::slice::from_ref(&map), &pairs));
        assert!(!entries.matches_signature(
            std::slice::from_ref(&map),
            &builtin(
                BuiltinType::Vec,
                vec![ResolvedTy::Tuple(vec![value, ResolvedTy::String])]
            )
        ));
        assert_eq!(
            RuntimeCallFamily::Map(MapValueOp::Get).result_authority(),
            RuntimeResultAuthority::IndependentValue
        );
        assert_eq!(
            RuntimeCallFamily::Map(MapValueOp::Entries).result_authority(),
            RuntimeResultAuthority::IndependentValue
        );
    }

    #[test]
    fn collection_constructors_require_canonical_identity_and_arity() {
        for (family, kind, arguments) in [
            (
                RuntimeCallFamily::Map(MapValueOp::New),
                BuiltinType::HashMap,
                vec![ResolvedTy::String, ResolvedTy::I64],
            ),
            (
                RuntimeCallFamily::Set(SetValueOp::New),
                BuiltinType::HashSet,
                vec![ResolvedTy::String],
            ),
        ] {
            let contract = family.semantic_contract().unwrap();
            let receiver = builtin(kind, arguments.clone());
            assert!(contract.matches_signature(&[], &receiver));
            assert!(contract.matches_signature(
                &[],
                &ResolvedTy::named_builtin("renamed.Collection", kind, arguments.clone())
            ));
            assert!(!contract.matches_signature(
                &[],
                &ResolvedTy::named_user(kind.canonical_name(), arguments)
            ));
            assert!(!contract.matches_signature(&[], &builtin(kind, vec![])));
            assert!(!contract
                .matches_signature(&[], &builtin(BuiltinType::Vec, vec![ResolvedTy::String])));
            assert!(!contract.matches_signature(std::slice::from_ref(&receiver), &receiver));
        }
    }

    #[test]
    fn map_updates_replace_the_receiver_and_preserve_input_owners() {
        let map = builtin(
            BuiltinType::HashMap,
            vec![ResolvedTy::String, ResolvedTy::Bytes],
        );
        let insert = RuntimeCallFamily::Map(MapValueOp::Insert);
        assert!(insert
            .semantic_contract()
            .unwrap()
            .matches_signature(&[map.clone(), ResolvedTy::String, ResolvedTy::Bytes], &map));
        assert!(!insert.semantic_contract().unwrap().matches_signature(
            &[map.clone(), ResolvedTy::String, ResolvedTy::Bytes],
            &ResolvedTy::Unit
        ));
        assert_eq!(insert.arg_consume_verdict(0), ConsumeVerdict::ProvenConsume);
        assert_eq!(insert.arg_consume_verdict(1), ConsumeVerdict::ProvenBorrow);
        // The value's ingress follows its clone fact - copied in when it has
        // one, moved in when it has none - so the per-argument table cannot
        // prove a borrow for it.
        assert_eq!(
            insert.arg_consume_verdict(2),
            ConsumeVerdict::ConservativeConsume
        );

        let removed = builtin(BuiltinType::Option, vec![ResolvedTy::Bytes]);
        let remove = RuntimeCallFamily::Map(MapValueOp::Remove)
            .semantic_contract()
            .unwrap();
        assert!(remove.matches_signature(
            &[map.clone(), ResolvedTy::String],
            &ResolvedTy::Tuple(vec![map.clone(), removed.clone()])
        ));
        assert!(!remove.matches_signature(&[map.clone(), ResolvedTy::String], &removed));
        assert!(!remove.matches_signature(
            &[map, ResolvedTy::String],
            &ResolvedTy::Tuple(vec![
                builtin(
                    BuiltinType::HashMap,
                    vec![ResolvedTy::I64, ResolvedTy::Bytes]
                ),
                removed
            ])
        ));
    }

    #[test]
    fn set_updates_return_presence_and_adopt_an_owned_element() {
        let set = builtin(BuiltinType::HashSet, vec![ResolvedTy::String]);
        let vector = builtin(BuiltinType::Vec, vec![ResolvedTy::String]);
        for operation in [SetValueOp::Insert, SetValueOp::Remove] {
            let family = RuntimeCallFamily::Set(operation);
            let contract = family.semantic_contract().unwrap();
            assert!(contract.matches_signature(
                &[set.clone(), ResolvedTy::String],
                &ResolvedTy::Tuple(vec![set.clone(), ResolvedTy::Bool])
            ));
            assert!(!contract.matches_signature(
                &[vector.clone(), ResolvedTy::String],
                &ResolvedTy::Tuple(vec![vector.clone(), ResolvedTy::Bool])
            ));
            assert!(
                !contract.matches_signature(&[set.clone(), ResolvedTy::String], &ResolvedTy::Bool)
            );
            // The element's ingress follows its clone fact - copied in when
            // it has one, moved in when it has none - so insertion cannot
            // prove a borrow for it, while removal only probes.
            let expected = if operation == SetValueOp::Insert {
                ConsumeVerdict::ConservativeConsume
            } else {
                ConsumeVerdict::ProvenBorrow
            };
            assert_eq!(family.arg_consume_verdict(1), expected);
            assert!(family.invalidates_collection_element_aliases());
        }
        let elements = RuntimeCallFamily::Set(SetValueOp::Elements);
        assert!(elements
            .semantic_contract()
            .unwrap()
            .matches_signature(std::slice::from_ref(&set), &vector));
        assert!(!elements.invalidates_collection_element_aliases());
    }
}

#[cfg(test)]
mod vector_semantic_contract_tests {
    use super::*;

    fn vector(element: ResolvedTy) -> ResolvedTy {
        ResolvedTy::named_builtin("Vec", crate::BuiltinType::Vec, vec![element])
    }

    #[test]
    fn vector_contract_binds_receiver_element_and_result_together() {
        let values = vector(ResolvedTy::String);
        let other = vector(ResolvedTy::I64);
        let contract = RuntimeCallFamily::Vector(VecValueOp::Push)
            .semantic_contract()
            .unwrap();
        assert!(contract.matches_signature(&[values.clone(), ResolvedTy::String], &values));
        assert!(!contract.matches_signature(&[values.clone(), ResolvedTy::I64], &values));
        assert!(!contract.matches_signature(&[values.clone(), ResolvedTy::String], &other));
        assert!(!contract.matches_signature(
            &[
                ResolvedTy::named_user("Vec", vec![ResolvedTy::String]),
                ResolvedTy::String
            ],
            &values
        ));
        assert!(!contract.matches_signature(std::slice::from_ref(&values), &values));
        let renamed = ResolvedTy::named_builtin(
            "std.collections.Sequence",
            crate::BuiltinType::Vec,
            vec![ResolvedTy::String],
        );
        assert!(contract.matches_signature(&[renamed.clone(), ResolvedTy::String], &renamed));
    }

    #[test]
    fn vector_results_are_exact_for_every_operation() {
        let values = vector(vector(ResolvedTy::String));
        let element = vector(ResolvedTy::String);
        let optional =
            ResolvedTy::named_builtin("Option", crate::BuiltinType::Option, vec![element.clone()]);
        let cases = [
            (VecValueOp::New, vec![], values.clone()),
            (VecValueOp::Len, vec![values.clone()], ResolvedTy::I64),
            (
                VecValueOp::Index,
                vec![values.clone(), ResolvedTy::I64],
                element.clone(),
            ),
            (
                VecValueOp::Get,
                vec![values.clone(), ResolvedTy::I64],
                optional.clone(),
            ),
            (
                VecValueOp::Set,
                vec![values.clone(), ResolvedTy::I64, element],
                values.clone(),
            ),
            (
                VecValueOp::Pop,
                vec![values.clone()],
                ResolvedTy::Tuple(vec![values.clone(), vector(ResolvedTy::String)]),
            ),
            (VecValueOp::Clear, vec![values.clone()], values),
        ];
        for (op, args, result) in cases {
            let contract = RuntimeCallFamily::Vector(op).semantic_contract().unwrap();
            assert!(contract.matches_signature(&args, &result), "{op:?}");
            assert!(
                !contract.matches_signature(&args, &ResolvedTy::Bool),
                "{op:?} must reject an unrelated result"
            );
        }
    }

    #[test]
    fn vector_read_contracts_never_publish_an_interior_owner() {
        assert_eq!(
            RuntimeCallFamily::Vector(VecValueOp::Index)
                .semantic_contract()
                .unwrap()
                .result,
            RuntimeResultEffect::IndependentValue(RuntimeValueKind::TypeArgument(0))
        );
        assert_eq!(
            RuntimeCallFamily::Vector(VecValueOp::Get)
                .semantic_contract()
                .unwrap()
                .result,
            RuntimeResultEffect::IndependentValue(RuntimeValueKind::Applied(
                BuiltinType::Option,
                &[RuntimeValueKind::TypeArgument(0)],
            ))
        );
        assert_eq!(
            RuntimeCallFamily::Vector(VecValueOp::Index)
                .semantic_contract()
                .unwrap()
                .failures,
            &[RuntimeLogicalFailure::IndexOutOfBounds]
        );
        assert!(RuntimeCallFamily::Vector(VecValueOp::Get)
            .semantic_contract()
            .unwrap()
            .failures
            .is_empty());
    }
}
