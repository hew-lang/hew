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

#![allow(
    deprecated,
    reason = "the reachability + mutation visitors visit the legacy CallTraitMethodStatic \
              variant exhaustively (fail-closed as may-mutate); it is allowlist-gated at \
              construction, matching the same allow in lower.rs"
)]

use std::collections::HashMap;

use std::collections::HashSet;

use hew_hir::{BindingId, HirBlock, HirExpr, HirExprKind, HirFn, ResolvedRef};
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

    /// Bits contributed by an ABSENT value position inside `enclosing` — a
    /// tail-less block (a diverging `{ return …; }` match arm), an else-less
    /// `if`, a value-less `return`, an empty `match`. Coarse keeps the
    /// pre-refactor `{OPAQUE}` (byte-identical `is_none_or` semantics); Precise
    /// applies the type short-circuit — a `Unit`/`Never`/scalar-typed enclosing
    /// expression carries no heap value, so the absent position contributes `∅`
    /// (a diverging arm must not poison a `ParamsOnly` summary to
    /// `PARAM|OPAQUE`), while any heap-typed enclosing form stays fail-closed.
    fn missing_position_bits(&self, enclosing: &HirExpr) -> AliasBits {
        let _ = enclosing;
        AliasBits::OPAQUE
    }
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
            None => policy.missing_position_bits(expr),
            Some(tail) => return_alias_bits(tail, policy),
        },
        HirExprKind::If {
            then_expr,
            else_expr,
            ..
        } => {
            let mut bits = return_alias_bits(then_expr, policy);
            bits |= match else_expr.as_deref() {
                None => policy.missing_position_bits(expr),
                Some(e) => return_alias_bits(e, policy),
            };
            bits
        }
        HirExprKind::Match { arms, .. } => {
            if arms.is_empty() {
                policy.missing_position_bits(expr)
            } else {
                arms.iter().fold(AliasBits::EMPTY, |acc, arm| {
                    acc | return_alias_bits(&arm.body, policy)
                })
            }
        }
        HirExprKind::Return { value } => match value.as_deref() {
            None => policy.missing_position_bits(expr),
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
/// symbol/family would admit the receiver-alias class this check rejects, so the
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
    /// Every declared `extern "C"` fn NAME. An extern CALL dispatches by name —
    /// its call-site `ResolvedRef::Item` carries the PLACEHOLDER `ItemId(0)`,
    /// NOT the declaration's id — so any id-keyed lookup for an extern callee
    /// is an id COLLISION with the module-fn summary space (a real fn with the
    /// colliding id could leak its `PARAM` bits into an extern caller's
    /// summary, the jwt/encrypt contamination). The Precise walk therefore
    /// checks the callee NAME here BEFORE any id lookup.
    names: HashSet<String>,
}

impl ExternContractTable {
    /// True when `name` is a declared `extern "C"` fn — the callee must be
    /// classified by the extern contract (interim: `{OPAQUE}` for every
    /// heap-or-unknown return), never by an id lookup.
    #[must_use]
    pub fn is_extern_name(&self, name: &str) -> bool {
        self.names.contains(name)
    }

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
    let mut names: HashSet<String> = HashSet::new();
    for item in &module.items {
        if let hew_hir::HirItem::ExternFn(ef) = item {
            names.insert(ef.name.clone());
            if ty_is_scalar_non_heap(&ef.return_ty) {
                rows.insert(ef.id, AliasBits::EMPTY);
            }
        }
    }
    ExternContractTable { rows, names }
}

// ---------------------------------------------------------------------------
// Module-global preflight context — built once, threaded into every builder
// ---------------------------------------------------------------------------

/// The module-global call-scrutinee return-provenance context the #2648 preflight
/// admission classifier consults at every scrutinee consumer.
///
/// Built ONCE per module (beside the coarse `compute_fn_returns_fresh_owner`
/// summary): the precise three-state provenance summary over every module fn, the
/// set of declared extern `ItemId`s, and the audited owned-return extern table.
///
/// `Default` (empty) fails SAFE: an empty summary classifies every module-fn
/// callee as an unknown item → interim `LegacyModuleCall` (today's fail-open
/// mint), never a wrongly-Fresh admit and never a spurious reject. The live
/// pipeline always threads the fully-built context; the empty default only backs
/// `Builder::default()` in unit tests that do not exercise a forwarder scrutinee.
#[derive(Debug, Default, Clone)]
pub struct CallScrutineeProvenance {
    /// Per-module-fn `ItemId` → precise three-state return provenance.
    pub provenance: HashMap<hew_hir::ItemId, ReturnProvenance>,
    /// Every declared `extern "C"` fn NAME. A call to an extern dispatches by
    /// name (its call-site `ResolvedRef::Item` carries a placeholder id, NOT the
    /// declaration's `ItemId`), so extern detection at the preflight keys on the
    /// name. A user extern whose name spoofs a runtime symbol is therefore caught
    /// here (heap-extern reject) BEFORE the name-based runtime-symbol carve-out;
    /// module-fn names are disjoint from extern names, so this cannot shadow a
    /// module fn.
    pub extern_names: HashSet<String>,
    /// The audited owned-return extern contract table (interim: scalar → Fresh,
    /// every heap extern → `{OPAQUE}`). Consumed by the precise fixpoint.
    pub extern_table: ExternContractTable,
    /// The interprocedural may-mutate-heap-param summary [F2], retained so the
    /// per-function local binding-provenance (the S2b caller arg-scan's
    /// fresh-local resolver) can be recomputed at the lowering seam under the
    /// SAME mutation taint the module fixpoint used. Empty default fails
    /// closed via `callee_is_proven_pure_item`'s `unwrap_or(false)` — but the
    /// arg-scan's fresh-local admit additionally requires an entry in the
    /// freshness map, so an empty context never widens an admit.
    pub may_mutate: HashMap<hew_hir::ItemId, bool>,
}

/// Build the module-global preflight context: the precise return-provenance
/// fixpoint (via the interprocedural mutation summary), the declared-extern id
/// set, and the audited extern contract table.
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "built once over the pipeline's default-hasher origin_fns map"
)]
pub fn build_call_scrutinee_provenance(
    module: &hew_hir::HirModule,
    origin_fns: &HashMap<hew_hir::ItemId, &HirFn>,
) -> CallScrutineeProvenance {
    let extern_table = build_extern_contract_table(module);
    let extern_names: HashSet<String> = module
        .items
        .iter()
        .filter_map(|item| match item {
            hew_hir::HirItem::ExternFn(ef) => Some(ef.name.clone()),
            _ => None,
        })
        .collect();
    let may_mutate = compute_may_mutate_heap_param(origin_fns);
    let provenance =
        compute_call_scrutinee_return_provenance(origin_fns, &extern_table, &may_mutate);
    CallScrutineeProvenance {
        provenance,
        extern_names,
        extern_table,
        may_mutate,
    }
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
/// name-forgeable bypass this admission check fixes).
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
// Total HIR reachability visitor + intra-procedural alias partition [F2-Rev6]
// ---------------------------------------------------------------------------

/// The set of tracked local/param bindings a value expression may carry an alias
/// of, plus an `unknown` flag.
///
/// `unknown` is set when the visitor hits an unmodelled heap-bearing form — a
/// fail-closed marker: an `unknown` reachability taints as if every tracked class
/// were reached. This is what makes the mutation-side extraction TOTAL: a form
/// the visitor cannot see through never silently reads as "reaches nothing".
#[derive(Debug, Default, Clone)]
pub struct Reachable {
    /// Bindings the value may embed an alias of.
    pub bindings: std::collections::HashSet<hew_hir::BindingId>,
    /// True when an unmodelled heap-bearing sub-form was encountered.
    pub unknown: bool,
}

/// Resolve the root binding of a place expression, walking through
/// field/tuple/index/slice projections. `None` when the root is not a binding
/// reference (a call result, a literal, an aggregate, …).
#[must_use]
#[allow(
    clippy::match_same_arms,
    reason = "projection arms are kept distinct to mirror the sealed HirExprKind surface"
)]
pub fn place_root_binding(expr: &HirExpr) -> Option<hew_hir::BindingId> {
    match &expr.kind {
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } => Some(*id),
        HirExprKind::FieldAccess { object, .. } => place_root_binding(object),
        HirExprKind::TupleIndex { tuple, .. } => place_root_binding(tuple),
        HirExprKind::Index { container, .. } => place_root_binding(container),
        HirExprKind::Slice { container, .. } => place_root_binding(container),
        _ => None,
    }
}

/// The TOTAL reachability visitor: descend EVERY expression AND statement form
/// reachable from `expr` — aggregate operands, projections, wrappers (`Block`
/// with ALL statements and the tail, `If`, `Match` arms), the array-literal
/// desugar's non-tail push statements, `Closure`/`GenBlock` capture ledgers,
/// call/method arguments and receivers, and every nested sub-expression —
/// accumulating every tracked binding alias into `out`.
///
/// SEPARATE from the admission-side value-flow [`return_alias_bits`] (which stays
/// tail-only, sound for the returned VALUE). Reusing the tail-only walk for
/// REACHABILITY was the round-4 bug: `helper([h], p)` hides `h` in a non-tail
/// push, and a `Closure` capturing `h` stores it in a ledger field an operand
/// visitor never reaches.
#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "the reachability visitor mirrors the sealed HirExprKind surface exhaustively;               structurally-similar arms are kept separate for auditability"
)]
pub fn reachable_bindings(expr: &HirExpr, out: &mut Reachable) {
    match &expr.kind {
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } => {
            out.bindings.insert(*id);
        }
        // Aggregates — an operand embedded in a struct/tuple/variant carries its
        // alias into the constructed value.
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, v) in fields {
                reachable_bindings(v, out);
            }
            if let Some(base) = base.as_deref() {
                reachable_bindings(base, out);
            }
        }
        HirExprKind::TupleLiteral { elements } => {
            for e in elements {
                reachable_bindings(e, out);
            }
        }
        HirExprKind::MachineVariantCtor { payload, .. } => {
            if let Some(fields) = payload {
                for (_, v) in fields {
                    reachable_bindings(v, out);
                }
            }
        }
        // Projections and casts pass the alias through.
        HirExprKind::FieldAccess { object, .. } => reachable_bindings(object, out),
        HirExprKind::TupleIndex { tuple, .. } => reachable_bindings(tuple, out),
        HirExprKind::Index { container, index } => {
            reachable_bindings(container, out);
            reachable_bindings(index, out);
        }
        HirExprKind::Slice { container, .. } => reachable_bindings(container, out),
        HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. }
        | HirExprKind::TryWidthCast { value, .. }
        | HirExprKind::CoerceToDynTrait { value, .. } => reachable_bindings(value, out),
        // Wrappers — visit ALL statements (the array-literal desugar hides its
        // push in a NON-tail statement) and the tail.
        HirExprKind::Block(block) => reachable_bindings_in_block(block, out),
        HirExprKind::If {
            then_expr,
            else_expr,
            ..
        } => {
            reachable_bindings(then_expr, out);
            if let Some(e) = else_expr.as_deref() {
                reachable_bindings(e, out);
            }
        }
        HirExprKind::Match {
            scrutinee, arms, ..
        } => {
            reachable_bindings(scrutinee, out);
            for arm in arms {
                // Guards read (and can escape) tracked bindings before any
                // arm body runs — a guard-only reference must still reach the
                // caller-side taint.
                if let Some(guard) = &arm.guard {
                    reachable_bindings(guard, out);
                }
                reachable_bindings(&arm.body, out);
            }
        }
        // Calls / methods — an argument (or receiver) embedding a tracked local
        // carries it to the call boundary.
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            reachable_bindings(callee, out);
            for a in args {
                reachable_bindings(a, out);
            }
        }
        HirExprKind::CallDynMethod { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            reachable_bindings(receiver, out);
            for a in args {
                reachable_bindings(a, out);
            }
        }
        HirExprKind::NumericMethod { receiver, arg, .. } => {
            reachable_bindings(receiver, out);
            reachable_bindings(arg, out);
        }
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            reachable_bindings(left, out);
            reachable_bindings(right, out);
        }
        HirExprKind::Unary { operand, .. } => reachable_bindings(operand, out),
        // Capture ledgers — a closure/generator capturing a tracked local carries
        // it across the callable boundary (an operand visitor cannot see these).
        HirExprKind::Closure { captures, .. } => {
            for cap in captures {
                out.bindings.insert(cap.binding);
            }
        }
        HirExprKind::GenBlock { captures, .. } => {
            for cap in captures {
                out.bindings.insert(cap.binding);
            }
        }
        // Fresh-by-construction / non-heap leaves carry no caller local.
        HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::RecordCloneCall { .. }
        | HirExprKind::ActorSelf
        | HirExprKind::ContextReader { .. } => {}
        HirExprKind::BindingRef { .. } => {
            // A non-local binding reference (Item / Const / Builtin) — a global or
            // module item, carries no tracked local.
        }
        // Any other form: fail closed if it could carry heap.
        other => {
            let _ = other;
            if !ty_is_scalar_non_heap(&expr.ty) {
                out.unknown = true;
            }
        }
    }
}

/// Reachability over a block: EVERY statement (initializers, assignments,
/// discarded expressions, returns, defers, let-else scrutinees/preludes) and the
/// tail. The non-tail statements are what the tail-only value-flow walk misses.
#[allow(
    clippy::match_same_arms,
    reason = "statement arms mirror the sealed HirStmtKind surface exhaustively"
)]
fn reachable_bindings_in_block(block: &HirBlock, out: &mut Reachable) {
    for stmt in &block.statements {
        match &stmt.kind {
            hew_hir::HirStmtKind::Let(_, Some(init)) => reachable_bindings(init, out),
            hew_hir::HirStmtKind::Let(_, None) => {}
            hew_hir::HirStmtKind::Assign { target, value } => {
                reachable_bindings(target, out);
                reachable_bindings(value, out);
            }
            hew_hir::HirStmtKind::Expr(e) => reachable_bindings(e, out),
            hew_hir::HirStmtKind::Return(Some(e)) => reachable_bindings(e, out),
            hew_hir::HirStmtKind::Return(None) => {}
            hew_hir::HirStmtKind::Defer { body, .. } => reachable_bindings(body, out),
            hew_hir::HirStmtKind::LetElse {
                scrutinee,
                success_prelude,
                else_body,
                ..
            } => {
                reachable_bindings(scrutinee, out);
                for s in success_prelude {
                    if let hew_hir::HirStmtKind::Let(_, Some(v)) = &s.kind {
                        reachable_bindings(v, out);
                    }
                }
                reachable_bindings_in_block(else_body, out);
            }
        }
    }
    if let Some(tail) = &block.tail {
        reachable_bindings(tail, out);
    }
}

/// The by-value heap parameters of `f` (the borrows a caller still owns). A
/// scalar param owns nothing → excluded; every non-scalar param is conservatively
/// included (a precise `ty_owns_heap` refinement is a wiring-site concern; PARAM
/// over-inclusion is sound).
#[must_use]
pub fn by_value_heap_param_bindings(f: &HirFn) -> std::collections::HashSet<hew_hir::BindingId> {
    f.params
        .iter()
        .filter(|p| !ty_is_scalar_non_heap(&p.ty))
        .map(|p| p.id)
        .collect()
}

// ---------------------------------------------------------------------------
// Interprocedural may-mutate-heap-param summary [F2]
// ---------------------------------------------------------------------------

/// Whole-function conservative summary: does `f` MUTATE (or store into) any of
/// its by-value heap parameters — the channel by which a returned param-borrow
/// silently gains an alias?
///
/// A second monotone boolean fixpoint (init `false`), built beside the provenance
/// fixpoint. `f` is may-mutate if its body:
/// - projection-stores (`p.f = …` / `p[i] = …`) into a heap param;
/// - calls a mutating / storing method on a heap param (anything NOT proven
///   `BorrowsReceiver` + non-escaping string args);
/// - passes a heap param (reachable through the total visitor) as an argument to
///   a callee NOT proven `!may_mutate` under the current table;
/// - invokes a callable parameter (an fn-pointer/closure param — conservatively
///   may-mutate).
///
/// Audited pure externs are absent from `fns`, so they never set the bit; an
/// unknown/indirect callee is treated as may-mutate (fail-closed).
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "built once over the pipeline's default-hasher origin_fns map"
)]
pub fn compute_may_mutate_heap_param(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
) -> HashMap<hew_hir::ItemId, bool> {
    let mut summary: HashMap<hew_hir::ItemId, bool> = fns.keys().map(|&id| (id, false)).collect();
    loop {
        let mut changed = false;
        for (&id, &f) in fns {
            if summary[&id] {
                continue;
            }
            if fn_mutates_heap_param(f, &summary) {
                summary.insert(id, true);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    summary
}

/// True when `f`'s body mutates one of its by-value heap params (or their alias
/// class) under the current `summary`.
fn fn_mutates_heap_param(f: &HirFn, summary: &HashMap<hew_hir::ItemId, bool>) -> bool {
    let param_class = by_value_heap_param_bindings(f);
    if param_class.is_empty() {
        return false;
    }
    let mut ctx = MutationScan {
        param_class: &param_class,
        summary,
        callable_params: f
            .params
            .iter()
            .filter(|p| {
                matches!(
                    p.ty,
                    ResolvedTy::Function { .. } | ResolvedTy::Closure { .. }
                )
            })
            .map(|p| p.id)
            .collect(),
    };
    ctx.block_mutates(&f.body)
}

struct MutationScan<'a> {
    param_class: &'a std::collections::HashSet<hew_hir::BindingId>,
    summary: &'a HashMap<hew_hir::ItemId, bool>,
    callable_params: std::collections::HashSet<hew_hir::BindingId>,
}

impl MutationScan<'_> {
    /// True when any tracked heap param is reachable from `expr` as an argument
    /// value (via the total reachability visitor, including the `unknown`
    /// fail-closed marker).
    fn arg_reaches_param(&self, expr: &HirExpr) -> bool {
        let mut r = Reachable::default();
        reachable_bindings(expr, &mut r);
        r.unknown || r.bindings.iter().any(|b| self.param_class.contains(b))
    }

    #[allow(
        clippy::match_same_arms,
        reason = "statement arms mirror the sealed HirStmtKind surface exhaustively"
    )]
    fn block_mutates(&mut self, block: &HirBlock) -> bool {
        for stmt in &block.statements {
            match &stmt.kind {
                hew_hir::HirStmtKind::Let(_, Some(init)) => {
                    if self.expr_mutates(init) {
                        return true;
                    }
                }
                hew_hir::HirStmtKind::Let(_, None) => {}
                hew_hir::HirStmtKind::Assign { target, value } => {
                    // A projection-store into a heap-param place is a mutation.
                    if is_projection_place(target)
                        && place_root_binding(target).is_some_and(|b| self.param_class.contains(&b))
                    {
                        return true;
                    }
                    if self.expr_mutates(target) || self.expr_mutates(value) {
                        return true;
                    }
                }
                hew_hir::HirStmtKind::Expr(e) => {
                    if self.expr_mutates(e) {
                        return true;
                    }
                }
                hew_hir::HirStmtKind::Return(Some(e)) => {
                    if self.expr_mutates(e) {
                        return true;
                    }
                }
                hew_hir::HirStmtKind::Return(None) => {}
                hew_hir::HirStmtKind::Defer { body, .. } => {
                    if self.expr_mutates(body) {
                        return true;
                    }
                }
                hew_hir::HirStmtKind::LetElse {
                    scrutinee,
                    else_body,
                    ..
                } => {
                    if self.expr_mutates(scrutinee) || self.block_mutates(else_body) {
                        return true;
                    }
                }
            }
        }
        block.tail.as_deref().is_some_and(|t| self.expr_mutates(t))
    }

    #[allow(
        clippy::match_same_arms,
        clippy::too_many_lines,
        reason = "mutation-scan arms mirror the sealed HirExprKind surface exhaustively"
    )]
    fn expr_mutates(&mut self, expr: &HirExpr) -> bool {
        match &expr.kind {
            // A mutating / storing method on a heap-param receiver.
            HirExprKind::ResolvedImplCall {
                receiver,
                target_symbol,
                args,
                ..
            } => {
                if place_root_binding(receiver).is_some_and(|b| self.param_class.contains(&b))
                    && !method_is_non_mutating(target_symbol)
                {
                    return true;
                }
                self.expr_mutates(receiver) || args.iter().any(|a| self.expr_mutates(a))
            }
            HirExprKind::VarSelfMethodCall { receiver, args, .. }
            | HirExprKind::CallDynMethod { receiver, args, .. }
            | HirExprKind::CallTraitMethodStatic { receiver, args, .. } => {
                // No emitted-symbol contract available for these forms here →
                // fail-closed: a mutating method on a heap-param receiver taints.
                if place_root_binding(receiver).is_some_and(|b| self.param_class.contains(&b)) {
                    return true;
                }
                self.expr_mutates(receiver) || args.iter().any(|a| self.expr_mutates(a))
            }
            HirExprKind::NumericMethod { receiver, arg, .. } => {
                self.expr_mutates(receiver) || self.expr_mutates(arg)
            }
            // A direct call: may-mutate if the callee is not proven pure AND an
            // argument reaches a heap-param class; a callable-param invocation is
            // may-mutate unconditionally when an arg reaches (or when the invoked
            // callable itself captures — conservatively any-arg).
            HirExprKind::Call { callee, args } => {
                let callee_pure = self.callee_is_proven_pure(callee);
                if !callee_pure && args.iter().any(|a| self.arg_reaches_param(a)) {
                    return true;
                }
                // A callable-parameter invocation with no explicit heap arg still
                // may mutate through the callable's captures — fail-closed.
                if self.callee_is_callable_param(callee) {
                    return true;
                }
                args.iter().any(|a| self.expr_mutates(a))
            }
            HirExprKind::Block(block) => self.block_mutates(block),
            HirExprKind::If {
                then_expr,
                else_expr,
                ..
            } => {
                self.expr_mutates(then_expr)
                    || else_expr.as_deref().is_some_and(|e| self.expr_mutates(e))
            }
            HirExprKind::Match {
                scrutinee, arms, ..
            } => {
                // Guards run before arm bodies and can mutate a heap param
                // (`0 if { p.push(x); true } => …`) — omit them and the
                // may-mutate summary reads a guard-mutating callee as pure.
                self.expr_mutates(scrutinee)
                    || arms.iter().any(|a| {
                        a.guard.as_ref().is_some_and(|g| self.expr_mutates(g))
                            || self.expr_mutates(&a.body)
                    })
            }
            HirExprKind::StructInit { fields, base, .. } => {
                fields.iter().any(|(_, v)| self.expr_mutates(v))
                    || base.as_deref().is_some_and(|b| self.expr_mutates(b))
            }
            HirExprKind::TupleLiteral { elements } => elements.iter().any(|e| self.expr_mutates(e)),
            HirExprKind::FieldAccess { object, .. } => self.expr_mutates(object),
            HirExprKind::TupleIndex { tuple, .. } => self.expr_mutates(tuple),
            HirExprKind::Index { container, index } => {
                self.expr_mutates(container) || self.expr_mutates(index)
            }
            HirExprKind::Binary { left, right, .. } => {
                self.expr_mutates(left) || self.expr_mutates(right)
            }
            HirExprKind::Unary { operand, .. } => self.expr_mutates(operand),
            HirExprKind::Return { value } => value.as_deref().is_some_and(|v| self.expr_mutates(v)),
            // Loop / scope bodies — a mutation inside a loop mutates on every
            // back-edge; missing these arms silently read a loop-mutating
            // callee as pure.
            HirExprKind::While {
                condition, body, ..
            } => self.expr_mutates(condition) || self.block_mutates(body),
            HirExprKind::ForRange {
                start,
                end,
                step,
                body,
                ..
            } => {
                self.expr_mutates(start)
                    || self.expr_mutates(end)
                    || self.expr_mutates(step)
                    || self.block_mutates(body)
            }
            HirExprKind::Loop { body, .. } => self.block_mutates(body),
            HirExprKind::Scope { body } => self.block_mutates(body),
            HirExprKind::Break { value, .. } => {
                value.as_deref().is_some_and(|v| self.expr_mutates(v))
            }
            HirExprKind::IfLet {
                scrutinee,
                body,
                else_body,
                ..
            } => {
                self.expr_mutates(scrutinee)
                    || self.block_mutates(body)
                    || else_body.as_ref().is_some_and(|b| self.block_mutates(b))
            }
            HirExprKind::WhileLet {
                scrutinee, body, ..
            } => self.expr_mutates(scrutinee) || self.block_mutates(body),
            _ => false,
        }
    }

    fn callee_is_callable_param(&self, callee: &HirExpr) -> bool {
        matches!(
            &callee.kind,
            HirExprKind::BindingRef { resolved: ResolvedRef::Binding(id), .. }
            if self.callable_params.contains(id)
        )
    }

    fn callee_is_proven_pure(&self, callee: &HirExpr) -> bool {
        // A resolved module item proven `!may_mutate` under the current summary is
        // pure. `None` in the summary = an extern / constructor with no analysable
        // body → pure by the owned-return ABI (matches the freshness gate's trust
        // of owned-return externs). Everything else (closure/indirect/unresolved)
        // is NOT proven pure → fail-closed.
        if let HirExprKind::BindingRef {
            resolved: ResolvedRef::Item(id),
            ..
        } = &callee.kind
        {
            !self.summary.get(id).copied().unwrap_or(false)
        } else {
            false
        }
    }
}

/// True when `place` is a projection (not a bare binding) — a `p.f` / `p[i]` /
/// `p.0` place whose store mutates interior storage.
fn is_projection_place(place: &HirExpr) -> bool {
    matches!(
        &place.kind,
        HirExprKind::FieldAccess { .. }
            | HirExprKind::TupleIndex { .. }
            | HirExprKind::Index { .. }
            | HirExprKind::Slice { .. }
    )
}

/// True when an EMITTED method symbol is proven non-mutating AND non-storing —
/// the ONLY exempt contract (`BorrowsReceiver` receiver + non-escaping string
/// args). Everything else (a storing element write, an escaping arg, an unknown
/// symbol's `FAIL_CLOSED` default) counts as mutating.
fn method_is_non_mutating(emitted_symbol: &str) -> bool {
    use crate::runtime_symbols::{
        callee_ownership_contract, ReceiverOwnership, StringArgsOwnership,
    };
    let contract = callee_ownership_contract(emitted_symbol);
    matches!(contract.receiver, ReceiverOwnership::BorrowsReceiver { .. })
        && matches!(
            contract.string_args,
            StringArgsOwnership::BorrowingUse | StringArgsOwnership::PrintSink
        )
}

// ---------------------------------------------------------------------------
// Local binding-provenance sub-analysis [F2] — the BindingRef-to-local resolver
// ---------------------------------------------------------------------------

/// One source contributing bits to a local binding.
enum DefSource<'f> {
    /// A whole-value definition (a `let`/`var` init, a `var` whole-assign RHS, or
    /// a pattern binder's scrutinee) — bits = `return_alias_bits(expr)`.
    Value(&'f HirExpr),
    /// A fail-closed definition (a projection-store `var`, an unmodelled binding
    /// form) → `{OPAQUE}`.
    Opaque,
}

#[derive(Default)]
struct LocalDefs<'f> {
    /// param heap-ness: id → true when the param owns heap (a `PARAM` alias root).
    params: HashMap<BindingId, bool>,
    /// per-binding contributing sources.
    defs: HashMap<BindingId, Vec<DefSource<'f>>>,
    /// alias edges (`let y = x` / `var y = x` whole) to union into one class.
    alias_edges: Vec<(BindingId, BindingId)>,
    /// bindings tainted `{OPAQUE}` by a mutation channel (their whole alias class
    /// is poisoned).
    tainted: HashSet<BindingId>,
    /// bindings introduced by a PATTERN (match arm / if-let / while-let /
    /// let-else destructure). A pattern binder aliases a payload slot of its
    /// scrutinee — a value another owner (a minted scrutinee owner, an
    /// `OwnedBinding` move) may also release — so the S2b arg-scan never
    /// treats one as an independently-owned fresh value.
    pattern_binders: HashSet<BindingId>,
}

/// Per-function local binding-provenance with MANDATORY alias closure [F2].
///
/// A by-value heap param is a `PARAM` alias root; a local's bits flow from its
/// definition(s); a binding whose alias class is mutated (a projection-store, a
/// mutating method, or a value passed to a not-proven-pure callee) is poisoned to
/// `{OPAQUE}` across the WHOLE class — `let y = x; y.f = p; return x` must reject
/// through `x` even though the store names `y`.
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "consumed with the pipeline's default-hasher summary maps"
)]
pub fn compute_local_binding_provenance(
    f: &HirFn,
    provenance: &HashMap<hew_hir::ItemId, AliasBits>,
    extern_table: &ExternContractTable,
    may_mutate: &HashMap<hew_hir::ItemId, bool>,
) -> HashMap<BindingId, AliasBits> {
    local_binding_provenance_impl(f, provenance, extern_table, may_mutate).0
}

/// The CURRENT function's local-binding freshness facts for the S2b caller
/// arg-scan — the S1 binding-provenance bits PLUS the shape facts the
/// fresh-local admit requires (`local_is_provably_fresh`).
///
/// The empty `Default` fails closed: a binding absent from `bits` is never
/// provably fresh, so a `Builder` that never computed the facts (a synthetic
/// machine-step / test builder) admits no local argument.
#[derive(Debug, Default, Clone)]
pub struct LocalBindingFreshness {
    /// S1 local binding-provenance bits (alias-closed, mutation-tainted).
    pub bits: HashMap<BindingId, AliasBits>,
    /// Bindings participating in ANY whole-value alias edge (`let y = x`).
    /// An aliased binding has a second in-scope release authority over the
    /// same value, so it is never admitted as a fresh argument.
    pub aliased: HashSet<BindingId>,
    /// Bindings introduced by a match/if-let/while-let/let-else pattern.
    pub pattern_binders: HashSet<BindingId>,
    /// TOTAL `BindingRef` occurrence count per binding across the whole body
    /// (initializer positions do not count; every read does, including match
    /// guards and closure/generator capture ledgers).
    pub ref_counts: HashMap<BindingId, u32>,
    /// True when the body contains a non-scalar expression form the counter
    /// does not model — the count may be an undercount, so NO local is
    /// admitted (fail-closed, mirroring `Reachable::unknown`).
    pub saw_unknown_form: bool,
}

impl LocalBindingFreshness {
    /// True when `id` is provably a solely-owned FRESH local at the call
    /// site: its S1 bits are `∅` (no `PARAM`, no `OPAQUE`, no mutation
    /// taint), it is a plain `let`/`var` local (not a pattern binder), it is
    /// not whole-value aliased, and this argument position is its ONLY read
    /// in the whole body — so the minted scrutinee owner is the single
    /// release authority over the value it carries (the exactly-once
    /// invariant; a second read would re-derive the buffer after the owner
    /// released it).
    #[must_use]
    pub fn local_is_provably_fresh(&self, id: BindingId) -> bool {
        !self.saw_unknown_form
            && self.bits.get(&id).copied().is_some_and(AliasBits::is_fresh)
            && !self.aliased.contains(&id)
            && !self.pattern_binders.contains(&id)
            && self.ref_counts.get(&id).copied() == Some(1)
    }
}

/// Compute the [`LocalBindingFreshness`] facts for one function — the S2b
/// arg-scan seam, run once per lowered function beside the funcupdate base
/// provenance. Uses the SAME module tables the S1 fixpoint used so the local
/// bits agree with the module summary.
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "consumed with the pipeline's default-hasher summary maps"
)]
pub fn compute_local_binding_freshness(
    f: &HirFn,
    provenance: &HashMap<hew_hir::ItemId, AliasBits>,
    extern_table: &ExternContractTable,
    may_mutate: &HashMap<hew_hir::ItemId, bool>,
) -> LocalBindingFreshness {
    let (bits, aliased, pattern_binders) =
        local_binding_provenance_impl(f, provenance, extern_table, may_mutate);
    let mut ref_counts: HashMap<BindingId, u32> = HashMap::new();
    let mut saw_unknown_form = false;
    count_binding_refs_in_block(&f.body, &mut ref_counts, &mut saw_unknown_form);
    LocalBindingFreshness {
        bits,
        aliased,
        pattern_binders,
        ref_counts,
        saw_unknown_form,
    }
}

fn local_binding_provenance_impl(
    f: &HirFn,
    provenance: &HashMap<hew_hir::ItemId, AliasBits>,
    extern_table: &ExternContractTable,
    may_mutate: &HashMap<hew_hir::ItemId, bool>,
) -> (
    HashMap<BindingId, AliasBits>,
    HashSet<BindingId>,
    HashSet<BindingId>,
) {
    let mut collector = LocalDefs::default();
    for p in &f.params {
        collector.params.insert(p.id, !ty_is_scalar_non_heap(&p.ty));
    }
    let mut ctx = DefCollector {
        defs: &mut collector,
        may_mutate,
    };
    ctx.collect_block(&f.body);

    // Union-find over alias classes.
    let mut uf = UnionFind::default();
    for &id in collector.params.keys() {
        uf.make(id);
    }
    for id in collector.defs.keys() {
        uf.make(*id);
    }
    for &(a, b) in &collector.alias_edges {
        uf.make(a);
        uf.make(b);
        uf.union(a, b);
    }
    // A class is poisoned if ANY member is tainted.
    let mut poisoned_roots: HashSet<BindingId> = HashSet::new();
    for &t in &collector.tainted {
        uf.make(t);
        poisoned_roots.insert(uf.find(t));
    }

    // Fixpoint over binding bits (monotone union from the optimistic ∅ / PARAM
    // seeds). Terminates: bits only grow over a finite 2-bit set.
    let mut bits: HashMap<BindingId, AliasBits> = HashMap::new();
    for (&id, &heap) in &collector.params {
        bits.insert(
            id,
            if heap {
                AliasBits::PARAM
            } else {
                AliasBits::EMPTY
            },
        );
    }
    for id in collector.defs.keys() {
        bits.entry(*id).or_insert(AliasBits::EMPTY);
    }
    loop {
        let mut changed = false;
        for (&id, sources) in &collector.defs {
            let policy = PrecisePolicy {
                provenance,
                extern_table,
                local_bits: &bits,
            };
            let mut new_bits = *bits.get(&id).unwrap_or(&AliasBits::EMPTY);
            for src in sources {
                new_bits |= match src {
                    DefSource::Value(e) => return_alias_bits(e, &policy),
                    DefSource::Opaque => AliasBits::OPAQUE,
                };
            }
            if new_bits != bits[&id] {
                bits.insert(id, new_bits);
                changed = true;
            }
        }
        // Poison whole classes and propagate class unions.
        let ids: Vec<BindingId> = bits.keys().copied().collect();
        for id in ids {
            let root = uf.find(id);
            if poisoned_roots.contains(&root) && !bits[&id].is_opaque() {
                let v = bits[&id] | AliasBits::OPAQUE;
                bits.insert(id, v);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    let aliased: HashSet<BindingId> = collector
        .alias_edges
        .iter()
        .flat_map(|&(a, b)| [a, b])
        .collect();
    (bits, aliased, collector.pattern_binders)
}

/// TOTAL `BindingRef` occurrence counter over one function body — the S2b
/// single-read fact. Mirrors the [`reachable_bindings`] surface (aggregates,
/// projections, wrappers with ALL block statements, calls/methods, capture
/// ledgers) plus `Scope` bodies and match-arm guards; any non-scalar form it
/// does not model sets `unknown` so the caller fails closed (an undercount
/// must never manufacture a "single use").
#[allow(
    clippy::match_same_arms,
    clippy::too_many_lines,
    reason = "the counter mirrors the sealed HirExprKind surface; structurally-similar arms stay separate for auditability"
)]
fn count_binding_refs(expr: &HirExpr, counts: &mut HashMap<BindingId, u32>, unknown: &mut bool) {
    match &expr.kind {
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } => {
            *counts.entry(*id).or_insert(0) += 1;
        }
        HirExprKind::BindingRef { .. } => {}
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, v) in fields {
                count_binding_refs(v, counts, unknown);
            }
            if let Some(base) = base.as_deref() {
                count_binding_refs(base, counts, unknown);
            }
        }
        HirExprKind::TupleLiteral { elements } => {
            for e in elements {
                count_binding_refs(e, counts, unknown);
            }
        }
        HirExprKind::MachineVariantCtor { payload, .. } => {
            if let Some(fields) = payload {
                for (_, v) in fields {
                    count_binding_refs(v, counts, unknown);
                }
            }
        }
        HirExprKind::FieldAccess { object, .. } => count_binding_refs(object, counts, unknown),
        HirExprKind::TupleIndex { tuple, .. } => count_binding_refs(tuple, counts, unknown),
        HirExprKind::Index { container, index } => {
            count_binding_refs(container, counts, unknown);
            count_binding_refs(index, counts, unknown);
        }
        HirExprKind::Slice { container, .. } => count_binding_refs(container, counts, unknown),
        HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. }
        | HirExprKind::TryWidthCast { value, .. }
        | HirExprKind::CoerceToDynTrait { value, .. } => {
            count_binding_refs(value, counts, unknown);
        }
        HirExprKind::Block(block) => count_binding_refs_in_block(block, counts, unknown),
        HirExprKind::Scope { body } => count_binding_refs_in_block(body, counts, unknown),
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            count_binding_refs(condition, counts, unknown);
            count_binding_refs(then_expr, counts, unknown);
            if let Some(e) = else_expr.as_deref() {
                count_binding_refs(e, counts, unknown);
            }
        }
        HirExprKind::Match {
            scrutinee, arms, ..
        } => {
            count_binding_refs(scrutinee, counts, unknown);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    count_binding_refs(guard, counts, unknown);
                }
                count_binding_refs(&arm.body, counts, unknown);
            }
        }
        HirExprKind::IfLet {
            scrutinee,
            body,
            else_body,
            ..
        } => {
            count_binding_refs(scrutinee, counts, unknown);
            count_binding_refs_in_block(body, counts, unknown);
            if let Some(else_body) = else_body {
                count_binding_refs_in_block(else_body, counts, unknown);
            }
        }
        HirExprKind::WhileLet {
            scrutinee, body, ..
        } => {
            count_binding_refs(scrutinee, counts, unknown);
            count_binding_refs_in_block(body, counts, unknown);
        }
        // Loop bodies: every read inside a loop counts (and a loop-carried read
        // can execute more than once per textual occurrence, so a binding read
        // inside a loop body is counted TWICE — a loop-body arg can never
        // qualify as single-read).
        HirExprKind::While {
            condition, body, ..
        } => {
            count_binding_refs(condition, counts, unknown);
            let mut inner: HashMap<BindingId, u32> = HashMap::new();
            count_binding_refs_in_block(body, &mut inner, unknown);
            for (id, n) in inner {
                *counts.entry(id).or_insert(0) += n.saturating_mul(2);
            }
        }
        HirExprKind::ForRange {
            start,
            end,
            step,
            body,
            ..
        } => {
            count_binding_refs(start, counts, unknown);
            count_binding_refs(end, counts, unknown);
            count_binding_refs(step, counts, unknown);
            let mut inner: HashMap<BindingId, u32> = HashMap::new();
            count_binding_refs_in_block(body, &mut inner, unknown);
            for (id, n) in inner {
                *counts.entry(id).or_insert(0) += n.saturating_mul(2);
            }
        }
        HirExprKind::Loop { body, .. } => {
            let mut inner: HashMap<BindingId, u32> = HashMap::new();
            count_binding_refs_in_block(body, &mut inner, unknown);
            for (id, n) in inner {
                *counts.entry(id).or_insert(0) += n.saturating_mul(2);
            }
        }
        HirExprKind::Break { value, .. } => {
            if let Some(v) = value.as_deref() {
                count_binding_refs(v, counts, unknown);
            }
        }
        HirExprKind::Continue { .. } => {}
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            count_binding_refs(callee, counts, unknown);
            for a in args {
                count_binding_refs(a, counts, unknown);
            }
        }
        HirExprKind::CallDynMethod { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            count_binding_refs(receiver, counts, unknown);
            for a in args {
                count_binding_refs(a, counts, unknown);
            }
        }
        HirExprKind::NumericMethod { receiver, arg, .. } => {
            count_binding_refs(receiver, counts, unknown);
            count_binding_refs(arg, counts, unknown);
        }
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            count_binding_refs(left, counts, unknown);
            count_binding_refs(right, counts, unknown);
        }
        HirExprKind::Unary { operand, .. } => count_binding_refs(operand, counts, unknown),
        HirExprKind::Return { value } => {
            if let Some(v) = value.as_deref() {
                count_binding_refs(v, counts, unknown);
            }
        }
        // A capture is a read that survives into the callable — count it so a
        // captured local can never look single-use at an argument position.
        HirExprKind::Closure { captures, .. } => {
            for cap in captures {
                *counts.entry(cap.binding).or_insert(0) += 1;
            }
        }
        HirExprKind::GenBlock { captures, .. } => {
            for cap in captures {
                *counts.entry(cap.binding).or_insert(0) += 1;
            }
        }
        HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::RecordCloneCall { .. }
        | HirExprKind::ActorSelf
        | HirExprKind::ContextReader { .. } => {}
        // Any other form: fail closed if it could carry heap (an unmodelled
        // read would undercount).
        other => {
            let _ = other;
            if !ty_is_scalar_non_heap(&expr.ty) {
                *unknown = true;
            }
        }
    }
}

/// Block-level counting: every statement form and the tail (mirrors
/// [`reachable_bindings_in_block`]).
#[allow(
    clippy::match_same_arms,
    reason = "statement arms mirror the sealed HirStmtKind surface exhaustively"
)]
fn count_binding_refs_in_block(
    block: &HirBlock,
    counts: &mut HashMap<BindingId, u32>,
    unknown: &mut bool,
) {
    for stmt in &block.statements {
        match &stmt.kind {
            hew_hir::HirStmtKind::Let(_, Some(init)) => count_binding_refs(init, counts, unknown),
            hew_hir::HirStmtKind::Let(_, None) => {}
            hew_hir::HirStmtKind::Assign { target, value } => {
                count_binding_refs(target, counts, unknown);
                count_binding_refs(value, counts, unknown);
            }
            hew_hir::HirStmtKind::Expr(e) => count_binding_refs(e, counts, unknown),
            hew_hir::HirStmtKind::Return(Some(e)) => count_binding_refs(e, counts, unknown),
            hew_hir::HirStmtKind::Return(None) => {}
            hew_hir::HirStmtKind::Defer { body, .. } => count_binding_refs(body, counts, unknown),
            hew_hir::HirStmtKind::LetElse {
                scrutinee,
                success_prelude,
                else_body,
                ..
            } => {
                count_binding_refs(scrutinee, counts, unknown);
                for s in success_prelude {
                    if let hew_hir::HirStmtKind::Let(_, Some(v)) = &s.kind {
                        count_binding_refs(v, counts, unknown);
                    }
                }
                count_binding_refs_in_block(else_body, counts, unknown);
            }
        }
    }
    if let Some(tail) = &block.tail {
        count_binding_refs(tail, counts, unknown);
    }
}

/// A minimal union-find over `BindingId`s for the alias closure.
#[derive(Default)]
struct UnionFind {
    parent: HashMap<BindingId, BindingId>,
}

impl UnionFind {
    fn make(&mut self, id: BindingId) {
        self.parent.entry(id).or_insert(id);
    }

    fn find(&mut self, id: BindingId) -> BindingId {
        let p = *self.parent.get(&id).unwrap_or(&id);
        if p == id {
            return id;
        }
        let root = self.find(p);
        self.parent.insert(id, root);
        root
    }

    fn union(&mut self, a: BindingId, b: BindingId) {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra != rb {
            self.parent.insert(ra, rb);
        }
    }
}

struct DefCollector<'a, 'f> {
    defs: &'a mut LocalDefs<'f>,
    may_mutate: &'a HashMap<hew_hir::ItemId, bool>,
}

impl<'f> DefCollector<'_, 'f> {
    #[allow(
        clippy::match_same_arms,
        reason = "statement arms mirror the sealed HirStmtKind surface exhaustively"
    )]
    fn collect_block(&mut self, block: &'f HirBlock) {
        for stmt in &block.statements {
            match &stmt.kind {
                hew_hir::HirStmtKind::Let(binding, Some(init)) => {
                    // `let y = <BindingRef x>` unions y and x (an alias).
                    if let Some(root) = binding_ref_local(init) {
                        self.defs.alias_edges.push((binding.id, root));
                    }
                    self.defs
                        .defs
                        .entry(binding.id)
                        .or_default()
                        .push(DefSource::Value(init));
                    self.collect_expr(init);
                }
                hew_hir::HirStmtKind::Let(binding, None) => {
                    self.defs.defs.entry(binding.id).or_default();
                }
                hew_hir::HirStmtKind::Assign { target, value } => {
                    if is_projection_place(target) {
                        // A projection-store into a local `x.f = …` fail-closes x
                        // AND taints its class (an alias smuggle the whole-value
                        // walk cannot see).
                        if let Some(root) = place_root_binding(target) {
                            self.defs
                                .defs
                                .entry(root)
                                .or_default()
                                .push(DefSource::Opaque);
                            self.defs.tainted.insert(root);
                        }
                    } else if let Some(root) = binding_ref_local(target) {
                        // A whole `var x = value` contributes the value's bits.
                        if let Some(rhs_root) = binding_ref_local(value) {
                            self.defs.alias_edges.push((root, rhs_root));
                        }
                        self.defs
                            .defs
                            .entry(root)
                            .or_default()
                            .push(DefSource::Value(value));
                    }
                    self.collect_expr(value);
                }
                hew_hir::HirStmtKind::Expr(e) => self.collect_expr(e),
                hew_hir::HirStmtKind::Return(Some(e)) => self.collect_expr(e),
                hew_hir::HirStmtKind::Return(None) => {}
                hew_hir::HirStmtKind::Defer { body, .. } => self.collect_expr(body),
                hew_hir::HirStmtKind::LetElse {
                    scrutinee,
                    bindings,
                    success_prelude,
                    else_body,
                    ..
                } => {
                    for b in bindings {
                        self.defs.pattern_binders.insert(b.binding);
                        self.defs
                            .defs
                            .entry(b.binding)
                            .or_default()
                            .push(DefSource::Value(scrutinee));
                    }
                    self.collect_expr(scrutinee);
                    for s in success_prelude {
                        if let hew_hir::HirStmtKind::Let(binding, Some(v)) = &s.kind {
                            self.defs
                                .defs
                                .entry(binding.id)
                                .or_default()
                                .push(DefSource::Value(v));
                            self.collect_expr(v);
                        }
                    }
                    self.collect_block(else_body);
                }
            }
        }
        if let Some(tail) = &block.tail {
            self.collect_expr(tail);
        }
    }

    /// Walk an expression, recording pattern binders (Match/IfLet/WhileLet) and
    /// the taint channels (mutating methods, may-mutate call args), and recursing.
    #[allow(
        clippy::too_many_lines,
        clippy::match_same_arms,
        reason = "the collector mirrors the sealed HirExprKind surface exhaustively"
    )]
    fn collect_expr(&mut self, expr: &'f HirExpr) {
        match &expr.kind {
            HirExprKind::Match {
                scrutinee, arms, ..
            } => {
                for arm in arms {
                    // A catch-all binding arm (`err => …`) carries its binder in
                    // the PREDICATE, not `arm.bindings` — missing it left the
                    // binder out of the local map, whose fail-closed leaf reads
                    // an absent local as `{PARAM}` (the jwt/encrypt `err =>
                    // Err(err)` PARAM contamination).
                    if let hew_hir::HirMatchArmPredicate::Binding { binding_id, .. } =
                        &arm.predicate
                    {
                        self.defs.pattern_binders.insert(*binding_id);
                        self.defs
                            .defs
                            .entry(*binding_id)
                            .or_default()
                            .push(DefSource::Value(scrutinee));
                    }
                    for b in &arm.bindings {
                        self.defs.pattern_binders.insert(b.binding);
                        self.defs
                            .defs
                            .entry(b.binding)
                            .or_default()
                            .push(DefSource::Value(scrutinee));
                    }
                    self.collect_expr(&arm.body);
                    if let Some(guard) = &arm.guard {
                        self.collect_expr(guard);
                    }
                }
                self.collect_expr(scrutinee);
            }
            HirExprKind::IfLet {
                scrutinee,
                bindings,
                body,
                else_body,
                ..
            } => {
                for b in bindings {
                    self.defs.pattern_binders.insert(b.binding);
                    self.defs
                        .defs
                        .entry(b.binding)
                        .or_default()
                        .push(DefSource::Value(scrutinee));
                }
                self.collect_expr(scrutinee);
                self.collect_block(body);
                if let Some(else_body) = else_body {
                    self.collect_block(else_body);
                }
            }
            HirExprKind::WhileLet {
                scrutinee,
                bindings,
                body,
                ..
            } => {
                for b in bindings {
                    self.defs.pattern_binders.insert(b.binding);
                    self.defs
                        .defs
                        .entry(b.binding)
                        .or_default()
                        .push(DefSource::Value(scrutinee));
                }
                self.collect_expr(scrutinee);
                self.collect_block(body);
            }
            // Mutating-method taint: a mutating/storing method on a local
            // receiver poisons its class.
            HirExprKind::ResolvedImplCall {
                receiver,
                target_symbol,
                args,
                ..
            } => {
                if let Some(root) = place_root_binding(receiver) {
                    if !method_is_non_mutating(target_symbol) {
                        self.defs.tainted.insert(root);
                    }
                }
                self.collect_expr(receiver);
                for a in args {
                    self.collect_expr(a);
                }
            }
            HirExprKind::VarSelfMethodCall { receiver, args, .. }
            | HirExprKind::CallDynMethod { receiver, args, .. }
            | HirExprKind::CallTraitMethodStatic { receiver, args, .. } => {
                if let Some(root) = place_root_binding(receiver) {
                    self.defs.tainted.insert(root);
                }
                self.collect_expr(receiver);
                for a in args {
                    self.collect_expr(a);
                }
            }
            HirExprKind::NumericMethod { receiver, arg, .. } => {
                self.collect_expr(receiver);
                self.collect_expr(arg);
            }
            // Caller-side call-argument taint: an argument reaching a heap local,
            // passed to a not-proven-pure direct callee, poisons that local's
            // class.
            HirExprKind::Call { callee, args } => {
                let pure = callee_is_proven_pure_item(callee, self.may_mutate);
                for a in args {
                    if !pure {
                        let mut r = Reachable::default();
                        reachable_bindings(a, &mut r);
                        for b in r.bindings {
                            self.defs.tainted.insert(b);
                        }
                    }
                    self.collect_expr(a);
                }
                self.collect_expr(callee);
            }
            HirExprKind::Block(block) => self.collect_block(block),
            HirExprKind::If {
                then_expr,
                else_expr,
                ..
            } => {
                self.collect_expr(then_expr);
                if let Some(e) = else_expr.as_deref() {
                    self.collect_expr(e);
                }
            }
            HirExprKind::StructInit { fields, base, .. } => {
                for (_, v) in fields {
                    self.collect_expr(v);
                }
                if let Some(base) = base.as_deref() {
                    self.collect_expr(base);
                }
            }
            HirExprKind::TupleLiteral { elements } => {
                for e in elements {
                    self.collect_expr(e);
                }
            }
            HirExprKind::Binary { left, right, .. } => {
                self.collect_expr(left);
                self.collect_expr(right);
            }
            HirExprKind::Unary { operand, .. } => self.collect_expr(operand),
            HirExprKind::FieldAccess { object, .. } => self.collect_expr(object),
            HirExprKind::TupleIndex { tuple, .. } => self.collect_expr(tuple),
            HirExprKind::Index { container, index } => {
                self.collect_expr(container);
                self.collect_expr(index);
            }
            HirExprKind::Return { value } => {
                if let Some(v) = value.as_deref() {
                    self.collect_expr(v);
                }
            }
            // Loop / scope bodies — a `let` or a mutating call inside a loop
            // body defines/taints exactly like straight-line code (the union
            // over defs is flow-insensitive already). Missing these arms left
            // every loop-body local OUT of the map, and the fail-closed local
            // leaf then read each one as `{PARAM}` — the injection that poisoned
            // the whole template render SCC to ParamsOnly.
            HirExprKind::While {
                condition, body, ..
            } => {
                self.collect_expr(condition);
                self.collect_block(body);
            }
            HirExprKind::ForRange {
                binding,
                start,
                end,
                step,
                body,
                ..
            } => {
                // The loop counter is a compiler-stepped integer — register it
                // so its `BindingRef`s resolve (∅ via the scalar short-circuit).
                self.defs.defs.entry(binding.id).or_default();
                self.collect_expr(start);
                self.collect_expr(end);
                self.collect_expr(step);
                self.collect_block(body);
            }
            HirExprKind::Loop { body, .. } => self.collect_block(body),
            HirExprKind::Scope { body } => self.collect_block(body),
            HirExprKind::Break { value, .. } => {
                if let Some(v) = value.as_deref() {
                    self.collect_expr(v);
                }
            }
            // A deep clone borrows its source non-mutatingly; descend for
            // nested defs but do NOT taint the source.
            HirExprKind::RecordCloneCall { src, .. } => self.collect_expr(src),
            // Enum/machine variant construction embeds its operands by value —
            // recurse them (the value-flow walk models the embedding); no taint.
            HirExprKind::MachineVariantCtor { payload, .. } => {
                if let Some(fields) = payload {
                    for (_, v) in fields {
                        self.collect_expr(v);
                    }
                }
            }
            // Pointer-identity comparison reads its operands without mutating
            // or escaping them.
            HirExprKind::IdentityCompare { left, right } => {
                self.collect_expr(left);
                self.collect_expr(right);
            }
            // Value-passthrough casts.
            HirExprKind::NumericCast { value, .. }
            | HirExprKind::SaturatingWidthCast { value, .. }
            | HirExprKind::TryWidthCast { value, .. }
            | HirExprKind::CoerceToDynTrait { value, .. } => self.collect_expr(value),
            HirExprKind::Slice { container, .. } => self.collect_expr(container),
            // Benign leaves: no binder, no taint, no sub-expression.
            HirExprKind::Literal(_)
            | HirExprKind::RegexLiteralRef { .. }
            | HirExprKind::BindingRef { .. }
            | HirExprKind::ActorSelf
            | HirExprKind::ContextReader { .. }
            | HirExprKind::Continue { .. } => {}
            // Any OTHER form is unmodelled here: it may hide a mutation or an
            // escape of a tracked binding (an actor send, a spawn capture, an
            // await), so every binding reachable from it is tainted fail-closed
            // rather than silently skipped.
            other => {
                let _ = other;
                if !ty_is_scalar_non_heap(&expr.ty) || expr_has_substructure(expr) {
                    let mut r = Reachable::default();
                    reachable_bindings(expr, &mut r);
                    for b in r.bindings {
                        self.defs.tainted.insert(b);
                    }
                }
            }
        }
    }
}

/// True when an expression form carries sub-expressions the def collector does
/// not model — used to route scalar-RESULT composites (an await returning i64,
/// an actor ask) through the fail-closed taint rather than skipping the
/// bindings their operands may escape.
fn expr_has_substructure(expr: &HirExpr) -> bool {
    !matches!(
        expr.kind,
        HirExprKind::Literal(_)
            | HirExprKind::RegexLiteralRef { .. }
            | HirExprKind::BindingRef { .. }
            | HirExprKind::ActorSelf
            | HirExprKind::ContextReader { .. }
            | HirExprKind::Continue { .. }
    )
}

/// The local binding id a value expression refers to directly (a bare
/// `BindingRef` to a `Binding`), or `None`.
fn binding_ref_local(expr: &HirExpr) -> Option<BindingId> {
    match &expr.kind {
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } => Some(*id),
        _ => None,
    }
}

/// Whether a direct-call callee is a resolved module item proven
/// `!may_mutate_heap_param` (or an owned-return extern/constructor with no
/// analysable body). Everything else (indirect/closure/unresolved) is NOT proven
/// pure.
fn callee_is_proven_pure_item(
    callee: &HirExpr,
    may_mutate: &HashMap<hew_hir::ItemId, bool>,
) -> bool {
    if let HirExprKind::BindingRef {
        resolved: ResolvedRef::Item(id),
        ..
    } = &callee.kind
    {
        !may_mutate.get(id).copied().unwrap_or(false)
    } else {
        false
    }
}

// ---------------------------------------------------------------------------
// Precise policy + the module return-provenance fixpoint [Sol-3]
// ---------------------------------------------------------------------------

/// The audited builtin collection constructors — checker-resolved static calls
/// (`Vec::new()` / `HashMap::new()` / `HashSet::new()`) that lower to a fresh
/// empty allocation (`hew_vec_new_*` / `hew_hashmap_new` / `hew_hashset_new`).
///
/// These reach HIR as a `Call` whose callee `BindingRef` carries the qualified
/// static name and a SYNTHETIC `ItemId` (no analysable body), so they miss the
/// module summary and would otherwise fail closed to `{OPAQUE}` — which turned
/// every `Ctx`-style stdlib producer (`template.new_ctx()`) opaque and broke the
/// S2b fresh-local arg-scan for its consumers. Name-keying is sound here: `::`
/// is not a declarable module-fn or extern identifier, and a user impl method
/// emitted under a qualified name carries a REAL `ItemId` that the module
/// summary (consulted FIRST) resolves.
#[must_use]
pub fn is_builtin_fresh_ctor(name: &str) -> bool {
    matches!(name, "Vec::new" | "HashMap::new" | "HashSet::new")
}

/// The Precise `LeafPolicy`: consumes the module provenance table (for the
/// three-way `Call` resolution), the audited extern table, and the CURRENT
/// function's local binding-provenance.
///
/// # Method-leaf note (S1)
///
/// The method leaf reads `method_return_provenance(target_symbol)` — the HIR
/// PLACEHOLDER symbol. This is SOUND (a placeholder for a borrowed getter is
/// `{OPAQUE}`, a placeholder for an owned getter is ALSO `{OPAQUE}`, never
/// wrongly Fresh), but conservative: the owned-value `Vec::get` (emitted
/// `hew_vec_get_clone`) reads `{OPAQUE}` until the wiring site (S2) supplies the
/// emitted-symbol resolver that reproduces lowering's owned-element-class
/// decision. It never admits a receiver alias.
#[derive(Debug)]
pub struct PrecisePolicy<'a> {
    /// The module return-provenance summary (being computed — read for the
    /// three-way `Call` resolution).
    pub provenance: &'a HashMap<hew_hir::ItemId, AliasBits>,
    /// The audited extern owned-return contract table.
    pub extern_table: &'a ExternContractTable,
    /// The CURRENT function's local binding-provenance.
    pub local_bits: &'a HashMap<BindingId, AliasBits>,
}

impl LeafPolicy for PrecisePolicy<'_> {
    fn classify_call(&self, callee: &HirExpr) -> CallClass {
        // A non-item callee (closure value, fn-pointer param, dynamic dispatch,
        // const, builtin) can hand back a captured heap param → Opaque.
        let HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Item(id),
        } = &callee.kind
        else {
            return CallClass::Opaque;
        };
        // Clause 0: an extern call dispatches by NAME — its call-site id is the
        // PLACEHOLDER `ItemId(0)`, so an id lookup would collide with a real
        // module fn's summary (leaking that fn's `PARAM` bits into the extern
        // caller — the jwt/encrypt false-reject contamination). No
        // heap-returning extern is trusted in the interim → `{OPAQUE}`; a
        // scalar-returning extern also lands here (sound: over-approximation
        // only widens toward Opaque, and its consumers' scalar results are
        // short-circuited by type at the leaves).
        if self.extern_table.is_extern_name(name) {
            return CallClass::Opaque;
        }
        // Clause 1: a resolved module fn → its summary (with arg substitution).
        if let Some(bits) = self.provenance.get(id) {
            if bits.is_fresh() {
                CallClass::Fresh
            } else if bits.is_params_only() {
                CallClass::ParamSubst
            } else {
                CallClass::Opaque
            }
        // Clause 2: an extern (scalar row → Fresh; heap/omitted → Opaque), or
        // Clause 3: an audited builtin collection constructor (`Vec::new()`
        // inside `new_ctx()`-style producers) — a fresh empty allocation.
        } else if self.extern_table.provenance_of(*id).is_fresh() || is_builtin_fresh_ctor(name) {
            CallClass::Fresh
        // Clause 4: an unknown/missing item (absent from every table → Opaque).
        // Never `unwrap_or(true)`.
        } else {
            CallClass::Opaque
        }
    }

    fn leaf_bits(&self, expr: &HirExpr) -> AliasBits {
        // Type short-circuit: a value owning no heap cannot alias a heap param.
        if ty_is_scalar_non_heap(&expr.ty) {
            return AliasBits::EMPTY;
        }
        match &expr.kind {
            // `a + b` on strings lowers to a fresh-allocating `hew_string_concat`
            // whose result aliases neither operand → ∅. Any other heap `Binary`
            // fails closed.
            HirExprKind::Binary { .. } => {
                if matches!(expr.ty, ResolvedTy::String) {
                    AliasBits::EMPTY
                } else {
                    AliasBits::OPAQUE
                }
            }
            // A method call → the emitted-symbol contract (S1: keyed on the
            // placeholder `target_symbol`, sound-but-conservative — see the type
            // doc).
            HirExprKind::ResolvedImplCall { target_symbol, .. } => {
                method_return_provenance(target_symbol)
            }
            // A binding reference to a tracked local reads its computed bits; a
            // by-value param not in the local map is `{PARAM}`.
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } => self.local_bits.get(id).copied().unwrap_or(AliasBits::PARAM),
            // Other method calls (no emitted-symbol contract here), a non-local
            // BindingRef (a module item/const/builtin), and every unmodelled form
            // fail closed.
            _ => AliasBits::OPAQUE,
        }
    }

    fn missing_position_bits(&self, enclosing: &HirExpr) -> AliasBits {
        // A diverging `{ return …; }` arm / else-less `if` / value-less
        // `return` in a `Unit`/`Never`/scalar position carries no heap value —
        // it must not poison a `ParamsOnly` summary to `PARAM|OPAQUE`.
        if ty_is_scalar_non_heap(&enclosing.ty) {
            AliasBits::EMPTY
        } else {
            AliasBits::OPAQUE
        }
    }
}

/// The module return-provenance summary: `ItemId → ReturnProvenance`, a monotone
/// least-fixpoint over the three-state lattice that starts every function at `∅`
/// and grows by union to stability.
///
/// Each pass, for every function, recomputes its local binding-provenance under
/// the current module table, then unions the bits of every value-bearing return
/// path (`return_alias_bits` under [`PrecisePolicy`]). Bits only grow over a
/// finite 2-bit set → terminates; start-empty is sound because every real alias
/// source is injected by a non-recursive transfer (a bare param → `{PARAM}`, an
/// opaque leaf → `{OPAQUE}`) and propagated by union.
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "built once over the pipeline's default-hasher origin_fns map"
)]
pub fn compute_call_scrutinee_return_provenance(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    extern_table: &ExternContractTable,
    may_mutate: &HashMap<hew_hir::ItemId, bool>,
) -> HashMap<hew_hir::ItemId, ReturnProvenance> {
    let mut provenance: HashMap<hew_hir::ItemId, AliasBits> =
        fns.keys().map(|&id| (id, AliasBits::EMPTY)).collect();
    loop {
        let mut changed = false;
        for (&id, &f) in fns {
            let local_bits =
                compute_local_binding_provenance(f, &provenance, extern_table, may_mutate);
            let policy = PrecisePolicy {
                provenance: &provenance,
                extern_table,
                local_bits: &local_bits,
            };
            let mut return_values: Vec<&HirExpr> = Vec::new();
            crate::lower::collect_return_values_in_block(&f.body, &mut return_values);
            if let Some(tail) = &f.body.tail {
                if !matches!(tail.ty, ResolvedTy::Unit | ResolvedTy::Never) {
                    return_values.push(tail);
                }
            }
            let mut bits = provenance[&id];
            for e in &return_values {
                bits |= return_alias_bits(e, &policy);
            }
            if bits != provenance[&id] {
                provenance.insert(id, bits);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    provenance
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

    #[test]
    fn coarse_differential_aggregate_and_method_shapes() {
        assert_coarse_byte_identical(
            r"
            record Box { data: string }
            fn embed(p: string) -> Box { Box { data: p } }
            fn tuple_embed(p: string) -> (string, i64) { (p, 0) }
            fn via_method(v: Vec<i64>) -> i64 { v.len() }
            ",
        );
    }

    /// The four mandated Coarse negative pins [Sol-5 + F5]: the Coarse authority
    /// (which the funcupdate/reassign gates consume) MUST still fail closed — a
    /// forwarder, an aggregate embedding a param, and a mutation channel are all
    /// NOT proven fresh. If any silently flipped to fresh, the shared UAF gates
    /// would regress.
    #[test]
    fn coarse_still_fails_closed_on_the_unsafe_shapes() {
        let module = lower_source(
            r"
            record Box { data: Vec<i64> }
            fn recursive_forwarder(flag: bool, x: Vec<i64>) -> Vec<i64> {
                if flag { x } else { recursive_forwarder(true, x) }
            }
            fn aggregate_embeds_param(p: Vec<i64>) -> Box { Box { data: p } }
            fn mutation_channel(x: Vec<i64>, v: i64) -> Vec<i64> {
                let y = x;
                y.push(v);
                x
            }
            ",
        );
        let origin_fns = origin_fns_of(&module);
        let coarse = compute_fn_returns_fresh_owner(&origin_fns);
        for name in [
            "recursive_forwarder",
            "aggregate_embeds_param",
            "mutation_channel",
        ] {
            assert!(
                !coarse[&fn_id(&module, name)],
                "Coarse must fail closed (not-fresh) on `{name}` so the shared gates never regress"
            );
        }
        // And the shared walk stays byte-identical to the frozen reference on the
        // same unsafe shapes.
        let frozen = compute_fn_returns_fresh_owner_ref(&origin_fns);
        assert_eq!(coarse, frozen);
    }

    /// Recursively collect every `.hew` file under `dir` into `out`.
    fn collect_hew_files(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                collect_hew_files(&path, out);
            } else if path.extension().is_some_and(|e| e == "hew") {
                out.push(path);
            }
        }
    }

    /// The F5 interface pin (corpus half) [F5/Rev-4]: iterate the named corpus
    /// roots, standalone-lower every `.hew` to HIR, and for EVERY function assert
    /// the LIVE Coarse fixpoint (routed through the shared `return_alias_bits`
    /// walk) equals the FROZEN pre-refactor transfer. Divergence on any function
    /// is a silent-UAF-regression signal in the funcupdate (#2420 base) / reassign
    /// consumers that share the Coarse authority.
    ///
    /// An input that fails BEFORE HIR (parse / resolve error, or a standalone
    /// lowering that panics without the full module registry) is skipped and
    /// counted; the `compared` floor guards against silent corpus shrinkage
    /// turning the differential vacuous. The floor is a lower bound (new inputs
    /// only raise `compared`), so adding fixtures never breaks it while a
    /// disappearing corpus does.
    /// Floor on the number of `.hew` files discovered under the named roots.
    const CORPUS_FILE_FLOOR: usize = 840;
    /// Floor on the number of inputs that lower standalone and are compared.
    const COMPARED_FLOOR: usize = 800;

    #[test]
    fn coarse_verdict_differential() {
        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-mir crate dir has a repo-root parent")
            .to_path_buf();
        let roots = [
            "std",
            "tests/hew",
            "tests/vertical-slice/accept",
            "tests/vertical-slice/reject",
            "examples/v05/checked-mir",
            "examples/v05",
        ];
        let mut files: Vec<std::path::PathBuf> = Vec::new();
        for r in roots {
            collect_hew_files(&repo_root.join(r), &mut files);
        }
        files.sort();
        files.dedup();
        assert!(
            files.len() >= CORPUS_FILE_FLOOR,
            "corpus enumeration collapsed: found only {} `.hew` files under the named roots \
             (repo_root={}); expected >= {CORPUS_FILE_FLOOR}",
            files.len(),
            repo_root.display(),
        );

        // The comparison runs on a worker thread with a large stack: a corpus
        // input's standalone lowering can recurse deeply enough to overflow the
        // default test stack (an abort `catch_unwind` cannot trap), so the big
        // stack keeps the differential robust over the whole corpus.
        let worker = std::thread::Builder::new()
            .name("coarse-verdict-differential".into())
            .stack_size(256 * 1024 * 1024)
            .spawn(move || {
                // Standalone lowering of a corpus file that expects the full
                // module registry can panic; treat a panic as a skip, not a
                // differential failure.
                let prev_hook = std::panic::take_hook();
                std::panic::set_hook(Box::new(|_| {}));

                let mut compared = 0usize;
                let mut skipped = 0usize;
                let mut drift: Vec<String> = Vec::new();
                for f in &files {
                    let Ok(src) = std::fs::read_to_string(f) else {
                        skipped += 1;
                        continue;
                    };
                    let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        let parsed = hew_parser::parse(&src);
                        if !parsed.errors.is_empty() {
                            return None;
                        }
                        let mut checker = hew_types::Checker::new(
                            hew_types::module_registry::ModuleRegistry::new(vec![]),
                        );
                        let tc_output = checker.check_program(&parsed.program);
                        let output = hew_hir::lower_program(
                            &parsed.program,
                            &tc_output,
                            &hew_hir::ResolutionCtx,
                            hew_hir::TargetArch::host(),
                        );
                        let origin_fns = origin_fns_of(&output.module);
                        let live = compute_fn_returns_fresh_owner(&origin_fns);
                        let frozen = compute_fn_returns_fresh_owner_ref(&origin_fns);
                        Some(live == frozen)
                    }));
                    match outcome {
                        Ok(Some(true)) => compared += 1,
                        Ok(Some(false)) => {
                            compared += 1;
                            drift.push(f.display().to_string());
                        }
                        Ok(None) | Err(_) => skipped += 1,
                    }
                }

                std::panic::set_hook(prev_hook);
                (compared, skipped, drift)
            })
            .expect("spawn coarse-verdict-differential worker");
        let (compared, skipped, drift) = worker.join().expect("worker thread panicked");

        assert!(
            drift.is_empty(),
            "Coarse verdict drift between the shared walk and the frozen pre-refactor \
             reference on {} corpus input(s): {drift:#?}",
            drift.len(),
        );
        assert!(
            compared >= COMPARED_FLOOR,
            "the coarse differential went vacuous: only {compared} corpus input(s) lowered \
             standalone ({skipped} skipped); silent corpus shrinkage below the {COMPARED_FLOOR} floor",
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

    // -- Interprocedural may-mutate-heap-param summary [F2] --

    fn fn_id(module: &hew_hir::HirModule, name: &str) -> hew_hir::ItemId {
        for item in &module.items {
            if let hew_hir::HirItem::Function(f) = item {
                if f.name == name {
                    return f.id;
                }
            }
        }
        panic!("function {name} not found");
    }

    #[test]
    fn method_mutation_on_heap_param_is_may_mutate() {
        let module = lower_source(
            r"
            fn mutate(x: Vec<i64>, v: i64) { x.push(v); }
            fn reader(x: Vec<i64>) -> i64 { 0 }
            ",
        );
        let origin_fns = origin_fns_of(&module);
        let summary = compute_may_mutate_heap_param(&origin_fns);
        assert!(
            summary[&fn_id(&module, "mutate")],
            "x.push(v) stores into the heap param x → may-mutate"
        );
        assert!(
            !summary[&fn_id(&module, "reader")],
            "a body that never touches the heap param is not may-mutate"
        );
    }

    #[test]
    fn guard_buried_return_contributes_param_bits() {
        // A `return p` inside a match-arm GUARD exits the function: its value
        // is a return path. Missing it read this forwarder as Fresh(∅) — the
        // preflight then admitted `match evil(p, 0)` and minted a second owner
        // over the caller-owned borrow (the codegen-review exploit).
        let (m, prov) = provenance_of_source(
            r"
            fn evil(p: Vec<i64>, k: i64) -> Vec<i64> {
                let d = match k {
                    0 if { return p; } => 0,
                    _ => 1,
                };
                let out: Vec<i64> = Vec::new();
                out.push(d);
                out
            }
            ",
        );
        assert!(
            prov[&fn_id(&m, "evil")].contains(AliasBits::PARAM),
            "the guard-buried `return p` path must union {{PARAM}}: {:?}",
            prov[&fn_id(&m, "evil")]
        );
    }

    #[test]
    fn guard_mutation_of_heap_param_is_may_mutate() {
        // A mutation inside a match-arm guard runs before any body — the
        // may-mutate summary must see it.
        let module = lower_source(
            r"
            fn guard_mut(x: Vec<i64>, k: i64) -> i64 {
                match k {
                    0 if { x.push(1); true } => 0,
                    _ => 1,
                }
            }
            ",
        );
        let origin_fns = origin_fns_of(&module);
        let summary = compute_may_mutate_heap_param(&origin_fns);
        assert!(
            summary[&fn_id(&module, "guard_mut")],
            "x.push(1) inside a guard stores into the heap param → may-mutate"
        );
    }

    #[test]
    fn guard_only_binding_reference_is_reachable() {
        // The total reachability visitor must see a binding referenced ONLY in
        // a match-arm guard (the caller-side taint channel).
        let module = lower_source(
            r"
            fn probe(h: Vec<i64>, k: i64) -> i64 {
                match k {
                    0 if h.len() > 0 => 0,
                    _ => 1,
                }
            }
            ",
        );
        for item in &module.items {
            if let hew_hir::HirItem::Function(f) = item {
                if f.name == "probe" {
                    let h_id = f.params[0].id;
                    let mut r = Reachable::default();
                    if let Some(tail) = &f.body.tail {
                        reachable_bindings(tail, &mut r);
                    }
                    assert!(
                        r.bindings.contains(&h_id),
                        "a guard-only reference to `h` must be reachable: {r:?}"
                    );
                    return;
                }
            }
        }
        panic!("probe not found");
    }

    #[test]
    fn interprocedural_mutation_propagates_to_the_caller() {
        let module = lower_source(
            r"
            fn mutate(x: Vec<i64>, v: i64) { x.push(v); }
            fn caller(h: Vec<i64>, v: i64) { mutate(h, v); }
            fn pure_target(x: Vec<i64>) -> i64 { 0 }
            fn caller_pure(h: Vec<i64>) -> i64 { pure_target(h) }
            ",
        );
        let origin_fns = origin_fns_of(&module);
        let summary = compute_may_mutate_heap_param(&origin_fns);
        assert!(
            summary[&fn_id(&module, "caller")],
            "passing a heap param to a may-mutate callee taints the caller"
        );
        assert!(
            !summary[&fn_id(&module, "caller_pure")],
            "passing a heap param to a proven-pure callee does not taint the caller"
        );
    }

    #[test]
    fn reachability_sees_a_heap_param_through_a_direct_ref() {
        let module = lower_source(r"fn f(a: Vec<i64>) -> Vec<i64> { a }");
        let f = module.items.iter().find_map(|it| match it {
            hew_hir::HirItem::Function(f) if f.name == "f" => Some(f),
            _ => None,
        });
        let f = f.expect("f present");
        let params = by_value_heap_param_bindings(f);
        assert_eq!(
            params.len(),
            1,
            "the Vec<i64> param is a by-value heap param"
        );
        let tail = f.body.tail.as_deref().expect("f has a tail expr");
        let mut r = Reachable::default();
        reachable_bindings(tail, &mut r);
        assert!(!r.unknown);
        assert!(
            params.iter().all(|p| r.bindings.contains(p)),
            "the returned `a` reaches the heap param binding"
        );
    }

    // -- The module return-provenance fixpoint [Sol-3] --

    fn provenance_of_source(
        source: &str,
    ) -> (hew_hir::HirModule, HashMap<hew_hir::ItemId, AliasBits>) {
        let module = lower_source(source);
        let origin_fns = origin_fns_of(&module);
        let extern_table = build_extern_contract_table(&module);
        let may_mutate = compute_may_mutate_heap_param(&origin_fns);
        let prov =
            compute_call_scrutinee_return_provenance(&origin_fns, &extern_table, &may_mutate);
        (module, prov)
    }

    #[test]
    fn fresh_producer_scc_converges_to_fresh() {
        let (m, prov) = provenance_of_source(
            r#"
            fn make() -> string { "hello" }
            fn wrap() -> string { make() }
            "#,
        );
        assert!(prov[&fn_id(&m, "make")].is_fresh());
        assert!(
            prov[&fn_id(&m, "wrap")].is_fresh(),
            "a chain of fresh producers is Fresh"
        );
    }

    #[test]
    fn diverging_return_arm_does_not_poison_params_only() {
        // A `{ return …; }` arm's body is a Unit-typed tail-less block; it
        // carries no value, so it must contribute `∅` under the Precise
        // policy — a param-embedding producer with a diverging arm stays
        // `ParamsOnly` (arg-rescuable), not `PARAM|OPAQUE`
        // (`match_diverging_arm_result_type` regression).
        let (m, prov) = provenance_of_source(
            r"
            enum Status {
                Good(string);
                Bad;
            }
            fn parse(input: string, fail: bool) -> Status {
                let result = match fail {
                    true => {
                        return Status::Bad;
                    },
                    false => Status::Good(input),
                };
                result
            }
            ",
        );
        assert!(
            prov[&fn_id(&m, "parse")].is_params_only(),
            "a diverging return-only arm contributes no bits: {:?}",
            prov[&fn_id(&m, "parse")]
        );
    }

    #[test]
    fn forwarder_scc_converges_to_params_only() {
        let (m, prov) = provenance_of_source(
            r"
            fn a(flag: bool, x: Vec<i64>) -> Vec<i64> { if flag { x } else { b(x) } }
            fn b(x: Vec<i64>) -> Vec<i64> { a(true, x) }
            fn passthru(x: Vec<i64>) -> Vec<i64> { x }
            ",
        );
        assert!(
            prov[&fn_id(&m, "passthru")].is_params_only(),
            "an identity forwarder returns a param borrow → ParamsOnly, not Fresh"
        );
        assert!(
            prov[&fn_id(&m, "a")].is_params_only(),
            "the mutually-recursive forwarder SCC converges to ParamsOnly"
        );
        assert!(prov[&fn_id(&m, "b")].is_params_only());
    }

    #[test]
    fn var_string_concat_composition_is_fresh() {
        // Models template's `var out` + `out = out + seg` — every whole-assign is
        // a fresh string concat, so `out` stays ∅ and the return is Fresh.
        let (m, prov) = provenance_of_source(
            r#"
            fn build(seg: string) -> string {
                var out = "";
                out = out + seg;
                out
            }
            "#,
        );
        assert!(
            prov[&fn_id(&m, "build")].is_fresh(),
            "string-concat var composition returns a fresh owner"
        );
    }

    #[test]
    fn returned_match_binder_over_fresh_scrutinee_is_fresh() {
        let (m, prov) = provenance_of_source(
            r#"
            fn produce() -> Result<string, string> { Ok("x") }
            fn unwrap_or_default() -> string {
                match produce() { Ok(v) => v, Err(e) => e }
            }
            "#,
        );
        assert!(
            prov[&fn_id(&m, "unwrap_or_default")].is_fresh(),
            "a binder over a fresh call scrutinee is Fresh"
        );
    }

    #[test]
    fn helper_mediated_mutation_makes_the_return_opaque() {
        // caller returns a heap param it passed to a param-mutating helper → the
        // returned value now holds a smuggled alias → NOT Fresh (rejects).
        let (m, prov) = provenance_of_source(
            r"
            fn helper(x: Vec<i64>, v: i64) { x.push(v); }
            fn caller(h: Vec<i64>, v: i64) -> Vec<i64> { helper(h, v); h }
            ",
        );
        assert!(
            !prov[&fn_id(&m, "caller")].is_fresh(),
            "a heap param mutated via a helper then returned must not be Fresh"
        );
        assert!(prov[&fn_id(&m, "caller")].is_opaque());
    }

    #[test]
    fn aliased_mutation_return_is_opaque() {
        // `let y = x; y.push(v); return x` — the store names y but x aliases it;
        // alias closure must poison x too.
        let (m, prov) = provenance_of_source(
            r"
            fn f(x: Vec<i64>, v: i64) -> Vec<i64> {
                let y = x;
                y.push(v);
                x
            }
            ",
        );
        assert!(
            prov[&fn_id(&m, "f")].is_opaque(),
            "a mutation through an alias must poison the whole alias class"
        );
    }

    #[test]
    fn global_const_return_is_opaque_not_wrongly_fresh() {
        let (m, prov) = provenance_of_source(
            r#"
            const GLOBAL: string = "g";
            fn leak() -> string { GLOBAL }
            "#,
        );
        assert!(
            prov[&fn_id(&m, "leak")].is_opaque(),
            "returning a module global is Opaque, never wrongly Fresh (the boolean+arg-scan hole)"
        );
    }

    // -----------------------------------------------------------------------
    // [F2/Rev-6] Interprocedural mutation reachability — the three channels the
    // tail-only value-flow recursion misses: an alias hidden inside an aggregate
    // argument, inside an array-literal desugar's non-tail push, and inside a
    // closure capture ledger reached via a callable-parameter invocation. Each
    // caller returns a heap param it smuggled through a may-mutate helper, so the
    // return is OPAQUE and its `match caller()` scrutinee rejects at S4b. These
    // pin the ANALYSIS verdict (the interim compile verdict is `LegacyModuleCall`
    // fail-open; the precise reject lands at S4b).
    // -----------------------------------------------------------------------

    #[test]
    fn helper_mutates_aggregate_arg_is_opaque() {
        // `helper(Wrapper { v: h }, ..); return h` — h is reachable through the
        // StructInit operand of a may-mutate call argument.
        let (m, prov) = provenance_of_source(
            r"
            type Wrapper { v: Vec<i64>; }
            fn helper(w: Wrapper, x: i64) { w.v.push(x); }
            fn caller(h: Vec<i64>, x: i64) -> Vec<i64> {
                helper(Wrapper { v: h }, x);
                h
            }
            ",
        );
        assert!(
            prov[&fn_id(&m, "caller")].is_opaque(),
            "a heap param smuggled inside an aggregate arg to a may-mutate helper is Opaque"
        );
    }

    #[test]
    fn helper_mutates_array_arg_is_opaque() {
        // `helper([h], ..); return h` — h is reachable only through the array
        // literal's non-tail push statement, which a tail-only walk misses.
        let (m, prov) = provenance_of_source(
            r"
            fn helper(xs: Vec<Vec<i64>>, x: i64) { xs.push(Vec::new()); }
            fn caller(h: Vec<i64>, x: i64) -> Vec<i64> {
                helper([h], x);
                h
            }
            ",
        );
        assert!(
            prov[&fn_id(&m, "caller")].is_opaque(),
            "a heap param inside an array-literal arg (non-tail push) to a may-mutate helper is Opaque"
        );
    }

    #[test]
    fn helper_invokes_capturing_closure_is_opaque() {
        // `helper(|| { h.len(); }, ..); return h` — helper invokes its callable
        // parameter (may-mutate, callable-param invocation), and h lives in the
        // closure's capture ledger, invisible to an operand-only visitor.
        let (m, prov) = provenance_of_source(
            r"
            fn helper(f: fn() -> i64, x: i64) -> i64 { f() }
            fn caller(h: Vec<i64>, x: i64) -> Vec<i64> {
                helper(|| { h.len() }, x);
                h
            }
            ",
        );
        assert!(
            prov[&fn_id(&m, "caller")].is_opaque(),
            "a heap param captured by a closure arg to a callable-invoking helper is Opaque"
        );
    }

    #[test]
    fn generator_capture_of_heap_param_is_opaque() {
        // The generator-capture analogue: `h` lives in a `gen { .. }` block's
        // capture ledger (`GenBlock.captures`), passed to a may-mutate helper.
        // The total reachability visitor must descend the generator capture
        // ledger — an operand-only walk cannot see it.
        let (m, prov) = provenance_of_source(
            r"
            fn drain<I>(g: I, sink: Vec<i64>) where I: Iterator<Item = i64> {
                sink.push(0);
            }
            fn caller(h: Vec<i64>, sink: Vec<i64>) -> Vec<i64> {
                drain(gen { yield h.len(); }, sink);
                h
            }
            ",
        );
        assert!(
            prov[&fn_id(&m, "caller")].is_opaque(),
            "a heap param captured by a generator-block arg to a may-mutate helper is Opaque"
        );
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
