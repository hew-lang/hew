//! Call-scrutinee return provenance (#2648) — the sound, precise authority for
//! *what a called function's by-value return may alias*.
//!
//! # Why this module exists
//!
//! The match/while-let/let-else/if-let/discarded call-scrutinee owner mint
//! (`call_scrutinee_owned_ty`, #2429) and #2523's projected-payload move-out
//! neutralize both rest on one premise: *a `Call` scrutinee's by-value return is
//! a fresh sole owner*. That premise is FALSE for an identity-forwarding callee
//! (`fn passthru(x: Box) -> Box { x }`) — by-value heap params are `Read`
//! borrows (`by-value-heap-params-are-borrows`), so the return aliases storage
//! the caller still owns; minting a second owner over it double-frees (#2648).
//!
//! This module replaces the fail-**open** admission with a **three-state
//! may-alias lattice** `AliasBits = { PARAM, OPAQUE }` (Fresh = ∅), computed by a
//! monotone least-fixpoint that starts EMPTY and grows by union. The lattice
//! distinguishes an arg-rescuable forward (`ParamsOnly`, `{PARAM}`) from a
//! never-rescuable alias (`Opaque`, `⊇{OPAQUE}` — a capture, a global, an
//! interior borrow, an indirect callee), which a boolean cannot.
//!
//! # Status: UNWIRED (S1)
//!
//! Every item here is analysis machinery with NO behaviour change: the sole
//! live edge is [`return_value_may_alias_borrow`] delegating to
//! [`return_alias_bits`] under [`CoarsePolicy`], whose output is byte-identical
//! to the pre-refactor boolean walk (proven by the `coarse_verdict_differential`
//! frozen-reference test). The Precise driver, the interprocedural mutation
//! summary, the preflight classifier, and the extern contract table are all
//! authored here but consumed by no lowering path until S2+.
//!
//! # The one-authority discipline (`vec-element-width-symmetric-abi`)
//!
//! The leaf walk is written ONCE as [`return_alias_bits`], parameterized by a
//! [`LeafPolicy`]. `CoarsePolicy` reproduces today's leaves exactly so the
//! shared funcupdate/reassign gates stay byte-identical; `PrecisePolicy` (S2)
//! consumes the three-state verdict. Two parallel walkers were the drift that
//! produced the #2523 twin — there is only one here.

use std::collections::HashMap;

use hew_hir::{HirExpr, HirExprKind, HirFn, ResolvedRef};
use hew_types::ResolvedTy;

// ---------------------------------------------------------------------------
// The three-state may-alias lattice
// ---------------------------------------------------------------------------

/// May-alias provenance bits for a value used as a function's by-value return.
///
/// - Empty (`∅`) is `Fresh`: aliases nothing caller-visible.
/// - `PARAM` is `ParamsOnly`: may alias one of the callee's by-value heap
///   params and nothing else (arg-rescuable — the forwarded param binds to some
///   caller argument).
/// - `OPAQUE` is `Opaque`: may alias a capture, a global, an interior borrow,
///   or an indirect/unknown callee. Never rescuable by an argument scan.
///
/// The set is finite (2 bits), and the module fixpoint only ever unions bits in,
/// so it terminates. This is a sound may-analysis: every real alias source is
/// injected by a concrete (non-recursive) transfer and propagated to stability.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct AliasBits(u8);

impl AliasBits {
    /// Fresh — the value aliases nothing caller-visible. **ADMIT regardless of
    /// args.**
    pub const EMPTY: Self = Self(0);
    /// The value may alias a by-value heap parameter (and nothing else). **May
    /// scan caller args** — ADMIT iff every heap-owning argument is itself fresh.
    pub const PARAM: Self = Self(0b01);
    /// The value may alias something the analysis cannot see through. **REJECT
    /// always; the arg scan cannot rescue it.**
    pub const OPAQUE: Self = Self(0b10);

    /// True when the value is provably a fresh sole owner (`∅`).
    #[must_use]
    pub const fn is_fresh(self) -> bool {
        self.0 == 0
    }

    /// True when every set bit is `PARAM` (i.e. `ParamsOnly`, not `Opaque`) and
    /// at least one bit is set. Only a `ParamsOnly` verdict licenses the caller
    /// arg-scan — this is the load-bearing rule (a boolean + arg-scan is unsound
    /// because it admits a zero-arg opaque return).
    #[must_use]
    pub const fn is_params_only(self) -> bool {
        self.0 == Self::PARAM.0
    }

    /// True when the `OPAQUE` bit is set — a never-rescuable alias.
    #[must_use]
    pub const fn is_opaque(self) -> bool {
        self.0 & Self::OPAQUE.0 != 0
    }

    /// True when `other`'s bits are all present in `self`.
    #[must_use]
    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }
}

impl std::ops::BitOr for AliasBits {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}

impl std::ops::BitOrAssign for AliasBits {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl std::fmt::Debug for AliasBits {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (self.contains(Self::PARAM), self.contains(Self::OPAQUE)) {
            (false, false) => f.write_str("Fresh(∅)"),
            (true, false) => f.write_str("ParamsOnly({PARAM})"),
            (false, true) => f.write_str("Opaque({OPAQUE})"),
            (true, true) => f.write_str("Opaque({PARAM|OPAQUE})"),
        }
    }
}

/// Whole-function return provenance = the union of every value-bearing return
/// path's `AliasBits`. This is the summary the module fixpoint computes per
/// `ItemId`.
pub type ReturnProvenance = AliasBits;

// ---------------------------------------------------------------------------
// The parameterized leaf walk — ONE authority, two policies
// ---------------------------------------------------------------------------

/// The interprocedural resolution of a `Call` callee, shared shape for both
/// policies. The shared walk owns the recursion; the policy only classifies the
/// callee.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CallClass {
    /// The callee is proven to hand back a fresh owner regardless of arguments
    /// → contributes `∅`.
    Fresh,
    /// The callee may forward a by-value param → the call contributes the union
    /// of its heap arguments' bits (argument substitution; a non-heap arg is `∅`
    /// so unioning ALL args is a sound superset of "heap args only").
    ParamSubst,
    /// The callee may alias something unknowable, OR the callee is not a
    /// statically-resolved item (a closure value, a fn-pointer param, an
    /// indirect/dynamic dispatch that can return a captured heap param through
    /// its environment) → contributes `{OPAQUE}` unconditionally. Fail-closed.
    Opaque,
}

/// The leaf/callee decisions that differ between the Coarse (byte-identical to
/// today) and Precise (#2648) walks. The structural arms (wrappers, aggregates,
/// projections, the fresh `Index`/`Slice`/`Literal`/`RecordCloneCall` leaves)
/// are policy-independent and live in [`return_alias_bits`].
pub trait LeafPolicy {
    /// Resolve a `Call`'s callee to its [`CallClass`].
    fn classify_call(&self, callee: &HirExpr) -> CallClass;

    /// Classify an expression that reaches the walk's fail-closed leaf (a
    /// `BindingRef`, a `Binary`, a method call, or any unmodelled form). Coarse
    /// returns `{OPAQUE}` unconditionally (today's `_ => true`); Precise applies
    /// the delta leaf rules.
    fn leaf_bits(&self, expr: &HirExpr) -> AliasBits;
}

/// The single structural walk. Structural arms are identical for every policy;
/// the `Call` and fail-closed-leaf decisions are delegated to `policy`.
///
/// A `None` sub-position (a tail-less block, an else-less `if`, a value-less
/// `return`, an empty `match`) contributes `{OPAQUE}` — fail-closed, exactly
/// reproducing the pre-refactor boolean walk's `is_none_or(..)`/`arms.is_empty()`
/// semantics.
pub fn return_alias_bits<P: LeafPolicy>(expr: &HirExpr, policy: &P) -> AliasBits {
    match &expr.kind {
        // Value-passthrough wrappers: the value flows from the tail / both
        // branches / every arm — aliases iff ANY reachable value aliases.
        HirExprKind::Block(block) => match &block.tail {
            None => AliasBits::OPAQUE,
            Some(tail) => return_alias_bits(tail, policy),
        },
        HirExprKind::If {
            then_expr,
            else_expr,
            ..
        } => {
            let mut bits = return_alias_bits(then_expr, policy);
            bits |= match else_expr.as_deref() {
                None => AliasBits::OPAQUE,
                Some(e) => return_alias_bits(e, policy),
            };
            bits
        }
        HirExprKind::Match { arms, .. } => {
            if arms.is_empty() {
                AliasBits::OPAQUE
            } else {
                arms.iter().fold(AliasBits::EMPTY, |acc, arm| {
                    acc | return_alias_bits(&arm.body, policy)
                })
            }
        }
        HirExprKind::Return { value } => match value.as_deref() {
            None => AliasBits::OPAQUE,
            Some(v) => return_alias_bits(v, policy),
        },
        // Fresh leaves — never a caller-owned alias. A `.clone()` is a deep copy;
        // a `Vec<T>` element load / slice is an independent heap element; a
        // literal owns nothing borrowed.
        HirExprKind::RecordCloneCall { .. }
        | HirExprKind::Index { .. }
        | HirExprKind::Slice { .. }
        | HirExprKind::Literal(_) => AliasBits::EMPTY,
        // A construction aliases a parameter iff one of its owned operands does.
        HirExprKind::StructInit { fields, base, .. } => {
            let mut bits = fields.iter().fold(AliasBits::EMPTY, |acc, (_, v)| {
                acc | return_alias_bits(v, policy)
            });
            if let Some(base) = base.as_deref() {
                bits |= return_alias_bits(base, policy);
            }
            bits
        }
        HirExprKind::TupleLiteral { elements } => {
            elements.iter().fold(AliasBits::EMPTY, |acc, e| {
                acc | return_alias_bits(e, policy)
            })
        }
        HirExprKind::MachineVariantCtor { payload, .. } => match payload {
            None => AliasBits::EMPTY,
            Some(fields) => fields.iter().fold(AliasBits::EMPTY, |acc, (_, v)| {
                acc | return_alias_bits(v, policy)
            }),
        },
        HirExprKind::Call { callee, args } => match policy.classify_call(callee) {
            CallClass::Opaque => AliasBits::OPAQUE,
            CallClass::Fresh => AliasBits::EMPTY,
            CallClass::ParamSubst => args.iter().fold(AliasBits::EMPTY, |acc, a| {
                acc | return_alias_bits(a, policy)
            }),
        },
        // A projection aliases a parameter iff its object chain does.
        HirExprKind::FieldAccess { object, .. } => return_alias_bits(object, policy),
        HirExprKind::TupleIndex { tuple, .. } => return_alias_bits(tuple, policy),
        // Every other form (a bare `BindingRef`, a `Binary`, a method call, a
        // deref, any unmodelled shape) is not provably fresh → the policy's leaf.
        _ => policy.leaf_bits(expr),
    }
}

// ---------------------------------------------------------------------------
// Coarse policy — byte-identical to the pre-refactor boolean walk
// ---------------------------------------------------------------------------

/// Reproduces today's `return_value_may_alias_borrow` leaves EXACTLY. The only
/// bit it ever produces is `OPAQUE` (it collapses `ParamsOnly`/`Opaque`, exactly
/// as the boolean did), so `return_alias_bits(e, &CoarsePolicy) != ∅` is
/// bit-for-bit the old boolean. Consumed only by the pinned funcupdate/reassign
/// gates via the [`return_value_may_alias_borrow`] wrapper.
#[derive(Debug)]
pub struct CoarsePolicy<'a> {
    /// The module freshness summary — `compute_fn_returns_fresh_owner`'s output.
    pub fresh: &'a HashMap<hew_hir::ItemId, bool>,
}

impl LeafPolicy for CoarsePolicy<'_> {
    fn classify_call(&self, callee: &HirExpr) -> CallClass {
        // `!callee_is_resolved_item(callee)` → OPAQUE (an indirect/closure callee
        // can hand back a captured heap param through a hidden argument).
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Item(item_id),
            ..
        } = &callee.kind
        else {
            return CallClass::Opaque;
        };
        // A resolved item: `Some(f)` reads the analyzed body's verdict; `None`
        // (an extern/runtime primitive/constructor) is fresh by the owned-return
        // ABI — exactly today's `unwrap_or(true)`.
        if self.fresh.get(item_id).copied().unwrap_or(true) {
            CallClass::Fresh
        } else {
            CallClass::ParamSubst
        }
    }

    fn leaf_bits(&self, _expr: &HirExpr) -> AliasBits {
        // Today's `_ => true` — every unmodelled form fails closed.
        AliasBits::OPAQUE
    }
}

/// The byte-identical Coarse wrapper. `return_value_may_alias_borrow` in
/// `lower.rs` delegates here so the funcupdate/reassign gates keep the exact
/// pre-refactor verdict while the one leaf walk is shared with the Precise
/// driver.
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "only ever called with the pipeline's default-hasher freshness summary map (compute_fn_returns_fresh_owner's output); a generic hasher param buys nothing"
)]
pub fn coarse_may_alias_borrow(expr: &HirExpr, fresh: &HashMap<hew_hir::ItemId, bool>) -> bool {
    !return_alias_bits(expr, &CoarsePolicy { fresh }).is_fresh()
}

// ---------------------------------------------------------------------------
// Type short-circuit — the scalar non-heap leaf (needs no layout registry)
// ---------------------------------------------------------------------------

/// True for a resolved type that is a scalar (or `unit`/`never`) leaf — a value
/// that provably owns no heap and therefore cannot alias any heap parameter.
///
/// Conservative on purpose: it fires ONLY for the primitive-scalar leaves the
/// type short-circuit needs without a layout registry (`semver`'s `maj/min/pat`
/// are `i64`). A composite whose fields are all scalar is NOT short-circuited
/// here — that needs the `ty_owns_heap` layout authority, threaded in at the
/// wiring site (S2); leaving it to the structural aggregate recursion is sound
/// (less precise, never unsound).
#[must_use]
pub fn ty_is_scalar_non_heap(ty: &ResolvedTy) -> bool {
    matches!(
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
            | ResolvedTy::Never
    )
}

// ---------------------------------------------------------------------------
// Method-call return contract — keyed on the EMITTED runtime symbol [F1]
// ---------------------------------------------------------------------------

/// EMITTED runtime symbols proved (by reading the runtime implementation) to
/// hand back a NEW `+1` owner, so a method call lowering to one of them is a
/// fresh sole owner (`∅`).
///
/// These are the descriptor-clone / retain / move-out getters — NOT the borrowed
/// getters. The distinction is load-bearing and is the F1 correction: the HIR
/// `ResolvedImplCall.target_symbol` is a *placeholder* (`hew_hashmap_get_layout`,
/// `hew_vec_get_owned`/`_ptr`), and lowering picks the actual owned callee at
/// emission time (`hew_vec_get_clone` for owned-value elements,
/// `hew_hashmap_get_clone_layout` always for `HashMap` get). Keying on the HIR
/// symbol/family would admit the receiver-alias class this lane rejects, so the
/// contract keys on the EMITTED symbol the site will actually lower to.
const PROVED_OWNER_METHOD_SYMBOLS: &[&str] = &[
    "hew_vec_get_clone",
    "hew_vec_get_str",
    "hew_vec_pop_str",
    "hew_vec_remove_at_str",
    "hew_hashmap_get_clone_layout",
    "hew_hashmap_remove_take_layout",
];

/// Return-provenance of a method call, given the EMITTED runtime symbol the site
/// lowers to. Fresh (`∅`) ONLY for a proved-owner clone/retain/take symbol or an
/// owned-return string/bytes producer; every borrowed getter
/// (`hew_vec_get_owned`/`_ptr`/`_layout`, `hew_hashmap_get_layout`), interior
/// getter, unknown, or family-only placeholder → `{OPAQUE}` (fail-closed).
///
/// The caller resolves which symbol the site emits by reproducing lowering's
/// owned-element-class decision (`Builder::is_owned_vec_element`) at the wiring
/// site (S2); this function is the sound EMITTED-symbol → provenance contract it
/// consults.
#[must_use]
pub fn method_return_provenance(emitted_symbol: &str) -> AliasBits {
    use crate::runtime_symbols::{callee_ownership_contract, ResultOwnership};
    if PROVED_OWNER_METHOD_SYMBOLS.contains(&emitted_symbol) {
        return AliasBits::EMPTY;
    }
    match callee_ownership_contract(emitted_symbol).result {
        ResultOwnership::FreshOwnedString | ResultOwnership::FreshOwnedBytes => AliasBits::EMPTY,
        ResultOwnership::Borrowed
        | ResultOwnership::InteriorAliasOfReceiver
        | ResultOwnership::Untracked => AliasBits::OPAQUE,
    }
}

// ---------------------------------------------------------------------------
// Audited ExternFn owned-return contract table [F3] — EMPTY/fail-closed interim
// ---------------------------------------------------------------------------

/// The audited positive allowlist of externs whose by-value return is a fresh
/// `+1` owner, keyed by `ItemId`.
///
/// # Interim (S1–S4): EMPTY / fail-closed [Rev-8, round-6 item 2]
///
/// `StdlibOrigin` / `TrustedStdlibRoot` / `HirModule.stdlib_origins` do NOT exist
/// at this base, so NO marker-backed row can be built yet. The interim table
/// therefore admits ONLY scalar-return externs (a scalar owns nothing and aliases
/// nothing — no trusted-root marker needed) and treats EVERY heap-returning
/// extern as `{OPAQUE}` (absent from the table → fail-closed lookup). The
/// marker-backed jwt/encrypt rows land at S4b once the trusted-root precursor
/// (`stdlib-root-canonical-resolution`, U194) exposes the non-forgeable marker.
///
/// A user `extern "C" fn evil() -> string` returning an interior pointer is
/// therefore `{OPAQUE}` here — never auto-trusted from `return_ty` heap-ness or
/// the arbitrary `abi` string.
#[derive(Debug, Default, Clone)]
pub struct ExternContractTable {
    rows: HashMap<hew_hir::ItemId, ReturnProvenance>,
}

impl ExternContractTable {
    /// Return-provenance of a resolved extern `ItemId`. An extern absent from the
    /// table (every heap-returning extern in the interim) is `{OPAQUE}` —
    /// fail-closed.
    #[must_use]
    pub fn provenance_of(&self, id: hew_hir::ItemId) -> AliasBits {
        self.rows.get(&id).copied().unwrap_or(AliasBits::OPAQUE)
    }

    /// Number of marker-backed / scalar rows. Zero marker-backed rows in the
    /// interim; the value is the count of scalar-return externs admitted.
    #[must_use]
    pub fn len(&self) -> usize {
        self.rows.len()
    }

    /// True when no extern is admitted.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }
}

/// Build the interim (empty/fail-closed) extern contract table over a module's
/// `extern "C"` declarations: scalar-return externs → Fresh; every
/// heap-returning extern is omitted (→ `{OPAQUE}` on lookup). Zero marker-backed
/// rows — the trusted-root precursor is required for those (S4b).
#[must_use]
pub fn build_extern_contract_table(module: &hew_hir::HirModule) -> ExternContractTable {
    let mut rows: HashMap<hew_hir::ItemId, ReturnProvenance> = HashMap::new();
    for item in &module.items {
        if let hew_hir::HirItem::ExternFn(ef) = item {
            if ty_is_scalar_non_heap(&ef.return_ty) {
                rows.insert(ef.id, AliasBits::EMPTY);
            }
        }
    }
    ExternContractTable { rows }
}

// ---------------------------------------------------------------------------
// Preflight carve-out detectors — pure HIR, keyed on TYPED identity [F4-new]
// ---------------------------------------------------------------------------

/// True when `callee` carries the compiler-minted typed runtime identity of a
/// receive family (`ResolvedRef::Builtin(RuntimeCallFamily::{ChannelRecv* |
/// StreamNext* | DuplexRecv*})`).
///
/// The carve-out keys on this TYPED identity, NOT the display name: a genuine
/// recv callee resolves to `ResolvedRef::Builtin(fam)` and carries its own
/// `BodyEndReleased` per-iteration release discipline (no synthetic owner must be
/// minted), whereas a user-declared `extern "C" fn hew_channel_recv_layout(..)`
/// resolves to `ResolvedRef::Item` → does NOT match → falls through to the
/// three-way `Call` resolution → `{OPAQUE}` → REJECT (fail-closed; closes the
/// name-forgeable bypass this lane fixes).
#[must_use]
pub fn is_typed_recv_callee(callee: &HirExpr) -> bool {
    use hew_types::runtime_call::RuntimeCallFamily as F;
    let HirExprKind::BindingRef {
        resolved: ResolvedRef::Builtin(family),
        ..
    } = &callee.kind
    else {
        return false;
    };
    matches!(
        family,
        F::ChannelRecvLayout
            | F::ChannelTryRecvLayout
            | F::StreamNextLayout
            | F::StreamTryNextLayout
            | F::DuplexRecv
            | F::DuplexRecvHalf
            | F::DuplexTryRecv
    )
}

/// True when `scrutinee` is a `Call` — the ONLY kind the from-call owner mint
/// (`call_scrutinee_owned_ty`) engages on. A non-`Call` scrutinee (a `Block`/`If`
/// synthetic `Vec<_>`-iteration desugar, a `GeneratorNext`, a bare place) is
/// structurally `NotApplicable` — it can never reach the from-call owner mint, so
/// its own release discipline runs unchanged. This is the `let HirExprKind::Call
/// { .. } = &scrutinee.kind else { return None }` gate the preflight reproduces
/// FIRST, before any runtime-identity or three-way `Call` resolution.
#[must_use]
pub fn scrutinee_is_call_kind(scrutinee: &HirExpr) -> bool {
    matches!(&scrutinee.kind, HirExprKind::Call { .. })
}

/// The admission verdict a call/method/aggregate scrutinee consumer acts on
/// (#2648 preflight). Pure-analysis shape; the wiring site (S2) maps `Admit` onto
/// the `ProjectedPayloadOrigin` the #2523 classifier + the #2429 owner mint
/// consume, and a reject onto a `MirDiagnostic` returned as `Err`.
///
/// `Reject` is NOT a variant here: the preflight returns `Result<_, MirDiagnostic>`
/// at the wiring site, so a reject is `Err` (one diagnostic, early return, no
/// partial MIR). This enum is the `Ok(..)` payload.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CallScrutineeAdmission {
    /// Not a from-call owner shape (a non-`Call` scrutinee, a typed-recv/iter-next
    /// carve-out, a builtin callee) → behave as today's `None`: no owner minted,
    /// no reject, the scrutinee's own release discipline runs unchanged.
    NotApplicable,
    /// A `Fresh` (or `ParamsOnly`-with-all-fresh-args) scrutinee → mint the #2429
    /// owner and classify #2523's move-out as `EphemeralTemp`.
    Admit,
    /// INTERIM ONLY (S2–S4; DELETED at S4b) [Rev-8, round-6 item 2]: a resolved
    /// module-fn callee whose precise summary carries NO `PARAM` bit → today's
    /// admission EXACTLY (the existing owner gate mints `__hew_call_scrutinee`;
    /// #2523 keeps its legacy `EphemeralTemp`). The precise module summary is
    /// consulted ONLY for the `PARAM`-present early reject, never for the
    /// admission shape — so opaque-hidden forwarding stays legacy fail-open until
    /// the trusted-root precursor merges.
    LegacyModuleCall,
}

// ---------------------------------------------------------------------------
// Module-map helpers
// ---------------------------------------------------------------------------

/// The set of `ItemId`s currently proven `false` (not fresh) under a coarse
/// bool table. Small helper used by tests and the differential harness to build
/// the same `origin_fns` map the live pipeline builds.
#[must_use]
pub fn origin_fns_of(module: &hew_hir::HirModule) -> HashMap<hew_hir::ItemId, &HirFn> {
    let mut origin_fns: HashMap<hew_hir::ItemId, &HirFn> = HashMap::new();
    for item in &module.items {
        if let hew_hir::HirItem::Function(f) = item {
            origin_fns.insert(f.id, f);
        }
    }
    origin_fns
}

/// A [`hew_mir::model::HeapOwnershipLayouts`]-shaped adapter that reports NO
/// record/enum layouts. Under it, `ty_owns_heap` still classifies the scalar and
/// collection-handle leaves correctly (they need no layout), and a composite of
/// unknown layout conservatively reads as non-heap — so this adapter is for
/// UNIT TESTS of the scalar/collection leaves only, never the wiring site (which
/// supplies the Builder's real registries).
#[derive(Debug)]
pub struct EmptyLayouts;

impl crate::model::HeapOwnershipLayouts for EmptyLayouts {
    fn record_field_tys(&self, _name: &str, _args: &[ResolvedTy]) -> Option<Vec<ResolvedTy>> {
        None
    }

    fn enum_variant_field_tys(
        &self,
        _name: &str,
        _args: &[ResolvedTy],
    ) -> Option<Vec<Vec<ResolvedTy>>> {
        None
    }
}

#[cfg(test)]
#[path = "return_provenance_ref.rs"]
mod frozen_reference;

#[cfg(test)]
mod tests {
    use super::frozen_reference::compute_fn_returns_fresh_owner_ref;
    use super::*;
    use crate::lower::compute_fn_returns_fresh_owner;

    /// Front-end-lower a `.hew` source string to a `HirModule`.
    fn lower_source(source: &str) -> hew_hir::HirModule {
        let parsed = hew_parser::parse(source);
        assert!(
            parsed.errors.is_empty(),
            "parse errors: {:#?}",
            parsed.errors
        );
        let mut checker =
            hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let tc_output = checker.check_program(&parsed.program);
        let output = hew_hir::lower_program(
            &parsed.program,
            &tc_output,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        output.module
    }

    /// The F5 interface pin (inline half): for every function in `source`, the
    /// LIVE coarse fixpoint (now routed through the shared `return_alias_bits`
    /// walk under `CoarsePolicy`) must produce the byte-identical `(ItemId, bool)`
    /// verdict the FROZEN pre-refactor transfer produces. Any divergence is a
    /// silent-UAF-regression signal in the funcupdate/reassign consumers.
    fn assert_coarse_byte_identical(source: &str) {
        let module = lower_source(source);
        let origin_fns = origin_fns_of(&module);
        let live = compute_fn_returns_fresh_owner(&origin_fns);
        let frozen = compute_fn_returns_fresh_owner_ref(&origin_fns);
        assert_eq!(
            live, frozen,
            "coarse verdict drift between shared walk and frozen reference:\nsource:\n{source}"
        );
    }

    #[test]
    fn coarse_differential_fresh_producers() {
        assert_coarse_byte_identical(
            r#"
            fn make() -> string { "hello" }
            fn concat(a: string) -> string { a + "!" }
            fn wrap() -> string { make() }
            "#,
        );
    }

    #[test]
    fn coarse_differential_forwarder_and_projection() {
        assert_coarse_byte_identical(
            r"
            record Box { data: string }
            fn passthru(x: string) -> string { x }
            fn project(b: Box) -> string { b.data }
            fn ctor(s: string) -> Box { Box { data: s } }
            ",
        );
    }

    #[test]
    fn coarse_differential_control_flow_and_match() {
        assert_coarse_byte_identical(
            r"
            fn choose(flag: bool, a: string, b: string) -> string {
                if flag { a } else { b }
            }
            fn viamatch(r: Result<string, string>) -> string {
                match r { Ok(v) => v, Err(e) => e }
            }
            fn nested(a: string) -> string {
                let x = a;
                x
            }
            ",
        );
    }

    #[test]
    fn coarse_differential_recursive_scc() {
        assert_coarse_byte_identical(
            r"
            fn a(flag: bool, x: string) -> string { if flag { x } else { b(x) } }
            fn b(x: string) -> string { a(true, x) }
            ",
        );
    }

    // -- Method-call return contract [F1] --

    #[test]
    fn owned_value_vec_get_emits_clone_and_is_fresh() {
        // The owned-value `Vec::get` lowers to `hew_vec_get_clone` (a descriptor
        // clone → a fresh independent owner), even though its runtime contract is
        // `Untracked`; the proved-owner set is what admits it.
        assert!(method_return_provenance("hew_vec_get_clone").is_fresh());
        assert!(method_return_provenance("hew_hashmap_get_clone_layout").is_fresh());
        assert!(method_return_provenance("hew_hashmap_remove_take_layout").is_fresh());
    }

    #[test]
    fn borrowed_vec_getters_are_opaque() {
        // Collection-handle `Vec::get` lowers to `hew_vec_get_owned` — a slot
        // borrow into the receiver storage; it MUST reject.
        assert!(method_return_provenance("hew_vec_get_owned").is_opaque());
        assert!(method_return_provenance("hew_vec_get_ptr").is_opaque());
        assert!(method_return_provenance("hew_vec_get_layout").is_opaque());
        assert!(method_return_provenance("hew_hashmap_get_layout").is_opaque());
    }

    #[test]
    fn owned_return_string_method_is_fresh_and_unknown_symbol_is_opaque() {
        // `s.slice(..)` → `hew_string_slice` → FreshOwnedString feeds semver.
        assert!(method_return_provenance("hew_string_slice").is_fresh());
        // An unknown / family-only placeholder fails closed.
        assert!(method_return_provenance("hew_totally_unknown_symbol").is_opaque());
    }

    // -- Extern owned-return contract table (interim empty/fail-closed) [F3] --

    #[test]
    fn extern_table_admits_scalar_returns_and_rejects_heap_returns() {
        let module = lower_source(
            r#"
            extern "C" {
                fn scalar_ext() -> i64;
                fn heap_ext() -> string;
            }
            "#,
        );
        let table = build_extern_contract_table(&module);
        // Zero marker-backed rows in the interim: only the scalar extern is
        // admitted; the heap-returning extern is absent → {OPAQUE} on lookup.
        let mut scalar_id = None;
        let mut heap_id = None;
        for item in &module.items {
            if let hew_hir::HirItem::ExternFn(ef) = item {
                match ef.name.as_str() {
                    "scalar_ext" => scalar_id = Some(ef.id),
                    "heap_ext" => heap_id = Some(ef.id),
                    _ => {}
                }
            }
        }
        let scalar_id = scalar_id.expect("scalar_ext must lower to an ExternFn");
        let heap_id = heap_id.expect("heap_ext must lower to an ExternFn");
        assert!(
            table.provenance_of(scalar_id).is_fresh(),
            "a scalar-return extern owns nothing and must be Fresh"
        );
        assert!(
            table.provenance_of(heap_id).is_opaque(),
            "a heap-return extern has no trusted-root marker in the interim → OPAQUE"
        );
        assert_eq!(table.len(), 1, "only the scalar extern is a row");
    }

    // -- Preflight structural carve-out --

    #[test]
    fn only_call_scrutinees_engage_the_owner_mint() {
        let module = lower_source(
            r#"
            fn producer() -> Result<string, string> { Ok("x") }
            fn use_call(r: Result<string, string>) -> i64 {
                match producer() { Ok(_) => 1, Err(_) => 0 }
            }
            "#,
        );
        // Find the `match producer()` scrutinee inside `use_call` and confirm it
        // is a Call kind; a bare-place / block scrutinee would not be.
        let mut saw_call_scrutinee = false;
        for item in &module.items {
            if let hew_hir::HirItem::Function(f) = item {
                if f.name == "use_call" {
                    for stmt in &f.body.statements {
                        collect_call_scrutinee(stmt, &mut saw_call_scrutinee);
                    }
                    if let Some(tail) = &f.body.tail {
                        if let hew_hir::HirExprKind::Match { scrutinee, .. } = &tail.kind {
                            saw_call_scrutinee |= scrutinee_is_call_kind(scrutinee);
                        }
                    }
                }
            }
        }
        assert!(
            saw_call_scrutinee,
            "the `match producer()` scrutinee must be recognised as a Call kind"
        );
    }

    fn collect_call_scrutinee(stmt: &hew_hir::HirStmt, out: &mut bool) {
        if let hew_hir::HirStmtKind::Expr(e) = &stmt.kind {
            if let hew_hir::HirExprKind::Match { scrutinee, .. } = &e.kind {
                *out |= scrutinee_is_call_kind(scrutinee);
            }
        }
    }

    #[test]
    fn alias_bits_lattice_states_are_distinct() {
        assert!(AliasBits::EMPTY.is_fresh());
        assert!(!AliasBits::EMPTY.is_params_only());
        assert!(!AliasBits::EMPTY.is_opaque());

        assert!(!AliasBits::PARAM.is_fresh());
        assert!(AliasBits::PARAM.is_params_only());
        assert!(!AliasBits::PARAM.is_opaque());

        assert!(!AliasBits::OPAQUE.is_fresh());
        assert!(!AliasBits::OPAQUE.is_params_only());
        assert!(AliasBits::OPAQUE.is_opaque());
    }

    #[test]
    fn union_of_param_and_opaque_is_not_params_only() {
        let both = AliasBits::PARAM | AliasBits::OPAQUE;
        assert!(!both.is_fresh());
        assert!(
            !both.is_params_only(),
            "PARAM|OPAQUE must not license the arg-scan"
        );
        assert!(both.is_opaque());
    }

    #[test]
    fn union_is_monotone_and_idempotent() {
        let mut bits = AliasBits::EMPTY;
        bits |= AliasBits::PARAM;
        assert!(bits.is_params_only());
        bits |= AliasBits::PARAM;
        assert!(bits.is_params_only(), "union is idempotent");
        bits |= AliasBits::OPAQUE;
        assert!(bits.is_opaque());
    }

    #[test]
    fn scalar_types_short_circuit_but_heap_types_do_not() {
        assert!(ty_is_scalar_non_heap(&ResolvedTy::I64));
        assert!(ty_is_scalar_non_heap(&ResolvedTy::Bool));
        assert!(ty_is_scalar_non_heap(&ResolvedTy::Duration));
        assert!(ty_is_scalar_non_heap(&ResolvedTy::Unit));
        assert!(!ty_is_scalar_non_heap(&ResolvedTy::String));
        assert!(!ty_is_scalar_non_heap(&ResolvedTy::Bytes));
        assert!(!ty_is_scalar_non_heap(&ResolvedTy::CancellationToken));
    }
}
