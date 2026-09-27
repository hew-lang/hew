//! Runtime call/drop descriptors and row-table support types.

#![allow(
    clippy::wildcard_imports,
    reason = "sibling split of one module; shares its item set"
)]

use super::family::RuntimeCallFamily;
use super::value_kinds::*;
use super::{ArrayValueOp, AsyncIoOp, FileReadOp, SupervisorPoolOp, TcpOp};
use crate::{BuiltinType, ResolvedTy};
use strum::IntoEnumIterator;

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
    /// Structural rendering glue: the operand's recipe tree, which codegen
    /// realizes as one borrow-only formatter thunk per participating type.
    StructuralFormat,
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
    /// `sink.send(x)` over any describable `Sink<T>` element: the
    /// layout-witness `hew_stream_send_layout` on the backpressure-aware
    /// suspend ramp.
    SinkSend,
    /// `stream.recv()` over a `Stream<T>` → `hew_stream_next_layout`.
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
/// the `runtime_drop_symbol` table in `hew-codegen-rs/src/llvm.rs`
/// (today: `Stream::close`, `Sink::close`, `CancellationToken::release`,
/// `MonitorRef::close`).
///
/// `non_exhaustive` is INTENTIONALLY OMITTED — same exhaustiveness
/// argument as [`RuntimeCallFamily`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeDropDescriptor {
    /// `Stream::close` → `hew_stream_close`. Element-type-independent
    /// at the ABI level; the type checker's builtin-method table emits
    /// the same `drop_fn` regardless of element type.
    StreamClose,
    /// `Sink::close` → `hew_sink_close`.
    SinkClose,
    /// `CancellationToken::release` → `hew_cancel_token_release`.
    CancellationTokenRelease,
    /// `MonitorRef::close` → `hew_actor_demonitor`. Extracts `ref_id: i64`
    /// from the struct and passes it directly to the runtime.
    MonitorRefClose,
}

/// The exact operand shape consumed by a runtime resource-close descriptor.
///
/// This stays coupled to [`RuntimeDropDescriptor`] rather than inferred from
/// its C symbol: `MonitorRef` owns an inline record slot rather than a
/// pointer handle.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeDropOperandShape {
    /// One opaque heap-handle pointer.
    HandlePtr,
    /// The `ref_id: i64` field in an inline `MonitorRef` record.
    MonitorRefId,
}

impl RuntimeDropDescriptor {
    /// The typed builtin identity whose scope-exit close this descriptor
    /// represents.
    #[must_use]
    pub const fn for_builtin(builtin: BuiltinType) -> Option<Self> {
        match builtin {
            BuiltinType::Stream => Some(Self::StreamClose),
            BuiltinType::Sink => Some(Self::SinkClose),
            BuiltinType::CancellationToken => Some(Self::CancellationTokenRelease),
            BuiltinType::MonitorRef => Some(Self::MonitorRefClose),
            _ => None,
        }
    }

    /// The slot/operand ABI required by this close ritual.
    #[must_use]
    pub const fn operand_shape(self) -> RuntimeDropOperandShape {
        match self {
            Self::MonitorRefClose => RuntimeDropOperandShape::MonitorRefId,
            Self::StreamClose | Self::SinkClose | Self::CancellationTokenRelease => {
                RuntimeDropOperandShape::HandlePtr
            }
        }
    }

    /// The C-ABI runtime symbol the drop lowers to.
    #[must_use]
    pub fn c_symbol(self) -> &'static str {
        match self {
            Self::StreamClose => "hew_stream_close",
            Self::SinkClose => "hew_sink_close",
            Self::CancellationTokenRelease => "hew_cancel_token_release",
            Self::MonitorRefClose => "hew_actor_demonitor",
        }
    }

    /// The producer-side method-name spelling (`<Type>::<method>`), the
    /// round-trip key of the descriptor: every variant has a unique name, so
    /// [`RuntimeDropDescriptor::from_drop_fn_name`] is a true inverse.
    #[must_use]
    pub fn drop_fn_name(self) -> &'static str {
        match self {
            Self::StreamClose => "Stream::close",
            Self::SinkClose => "Sink::close",
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
            "Stream::close" => Some(Self::StreamClose),
            "Sink::close" => Some(Self::SinkClose),
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
pub fn all_runtime_drop_descriptors() -> [RuntimeDropDescriptor; 4] {
    [
        RuntimeDropDescriptor::StreamClose,
        RuntimeDropDescriptor::SinkClose,
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
