//! Query methods on `RuntimeCallFamily`: symbol lookup, consume verdicts, suspension.

#![allow(
    clippy::wildcard_imports,
    reason = "sibling split of one module; shares its item set"
)]

use super::descriptors::*;
use super::family::RuntimeCallFamily;
use super::family::RuntimeCapability;
use super::value_kinds::*;
use super::{declared, ArrayValueOp};

impl RuntimeCallFamily {
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

    pub(super) const fn is_codegen_partition_only(self) -> bool {
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
        if let Some(method) = declared::DECLARED_RUNTIME_METHODS
            .iter()
            .find(|row| row.family == self)
        {
            return method.consumes_receiver;
        }
        matches!(
            self,
            Self::StreamClose
                | Self::SinkClose
                | Self::StreamForward
                | Self::ActorRequestRelease
                | Self::ActorCallFree
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

    /// Whether this operation releases receiver-typed contents inside its call.
    ///
    /// Replacement, removal and clearing can run an authored `close`, as can
    /// discarding an incoming set duplicate of the receiver's element type.
    /// SIR uses the receiver's contained-release effects, and physical MIR uses
    /// the same carrier's release recipe, to require a fault dispatch after the
    /// call. Callback failures have their own failure edge; internal releases
    /// join that fault without replacing it (HEW-SPEC-2026 §3.7.8.5).
    #[must_use]
    pub const fn releases_receiver_contents(self) -> bool {
        matches!(
            self,
            Self::RcSet
                | Self::Vector(VecValueOp::Set | VecValueOp::Clear)
                | Self::Array(ArrayValueOp::Set)
                | Self::Map(MapValueOp::Insert | MapValueOp::Remove | MapValueOp::Clear)
                | Self::Set(SetValueOp::Insert | SetValueOp::Remove | SetValueOp::Clear)
        )
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
                    | RuntimeValueKind::PipeHalf(_)
                    | RuntimeValueKind::PipeHalfResult(_)
                    | RuntimeValueKind::StreamPair
                    | RuntimeValueKind::ActorRequestOwner
                    | RuntimeValueKind::ActorRequestAdmission
                    | RuntimeValueKind::ActorHandle
                    | RuntimeValueKind::ConstBytePointer
                    | RuntimeValueKind::TypeArgument(_)
                    | RuntimeValueKind::SharedPayload
                    | RuntimeValueKind::NodeLookupResult
                    | RuntimeValueKind::BuiltinNominal(_)
                    | RuntimeValueKind::BuiltinArgument(_)
                    | RuntimeValueKind::Applied(_, _)
                    | RuntimeValueKind::Tuple(_)
                    | RuntimeValueKind::IoHandle(_)
                    | RuntimeValueKind::FileReadHandle(_)
                    | RuntimeValueKind::Named(_)
                    | RuntimeValueKind::NamedOpaque(_)
                    | RuntimeValueKind::MonomorphicBuiltin(_)
                    | RuntimeValueKind::StructuralOperand,
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
            // Every describable sink-send element rides the layout-witness
            // stream send on the backpressure-aware `SuspendKind::StreamSend`
            // ramp.
            F::StreamSendLayout => Some(AsyncSuspendKind::SinkSend),
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
            | F::StreamTrySendLayout
            | F::StreamForward
            | F::StreamPairSink
            | F::StreamPairStream
            | F::SinkClone
            | F::SinkFinish
            | F::SinkClose
            | F::SinkPeerClosed
            | F::ActorGenSinkComplete
            | F::ActorGenSinkRegister
            | F::ActorRequestRelease
            | F::ActorCallFree
            | F::ActorRequestTake
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
            | F::NodeIdDisplay
            | F::LocationNodeId
            | F::LocationSlot
            | F::LocationIncarnation
            | F::LocationDisplay
            | F::RemotePidLocation
            | F::RemotePidNodeId
            | F::RemotePidSlot
            | F::RemotePidIncarnation
            | F::RemotePidDisplay
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
            | F::RegexCapture
            | F::RegexCompile
            | F::RegexFreeCapture
            | F::RegexHandle
            | F::RegexMatch
            | F::ReplyChannelCancel
            | F::ReplyChannelFree
            | F::ReplyChannelNew
            | F::ReplyPayloadFree
            | F::ReplyWait
            | F::SelectFirst
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
            | F::GeneratorFree
            | F::TaskFree
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
    pub(super) fn accepts_elem(self) -> bool {
        // Closed-set fail-closed: no current variant accepts a separate
        // `ResolvedTy` elem; the pending genericisation work is where
        // this opens up. The future genericisation is additive:
        // a new `VecGetGeneric` variant returns `true` here and the
        // constructor admits `Some(elem)` for it without changing the
        // bijection invariant for the closed-set variants.
        false
    }
}
