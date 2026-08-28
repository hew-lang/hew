//! Return provenance (#2648) — analysis of what a called function's by-value
//! return may alias.
//!
//! # Why this module exists
//!
//! Legacy call-scrutinee owner minting and projected-payload neutralization once
//! rested on one premise: *a `Call` scrutinee's by-value return is a fresh sole
//! owner*. Those paths now consume HIR's typed produced-value fact instead. This
//! analysis remains for foreign-provenance and alias-safety consumers that are
//! outside produced-value owner publication. The old premise is FALSE for an identity-forwarding callee
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

use hew_hir::{
    BindingId, HirBlock, HirExpr, HirExprKind, HirFn, HirStmt, HirStmtKind, ResolvedRef,
};
use hew_types::ResolvedTy;

// ---------------------------------------------------------------------------
// The DECLARED-RELEASE authority — the third provenance class
// ---------------------------------------------------------------------------

/// The named types whose release this program DECLARES, rather than the
/// compiler deriving it from the type's layout.
///
/// # The provenance class the two taint policies could not express
///
/// Both policies below sort a value into one of two classes: domestic (mintable)
/// or ownership-opaque foreign (never mintable). Neither can express the class
/// in between — a handle whose ORIGIN is foreign but whose RELEASE this program
/// has taken responsibility for. That class already exists in the language and
/// was already carved out once, ad hoc: a root `extern "C" -> string` is
/// ADOPTED at the call edge (`emit_extern_malloc_string_adoption` copies the
/// foreign C string into a refcounted Hew buffer and `free`s the raw pointer),
/// and the `let` binder exempts `ResolvedTy::String` for exactly that reason.
///
/// `#[resource]` is the same class, spelled by the user instead of by the ABI:
///
/// ```hew
/// #[opaque] type Dq {}
/// #[resource] type Handle { raw: Dq; }
/// impl Handle {
///     fn close(self) { unsafe { hew_deque_free(self.raw) }; }
/// }
/// Handle { raw: unsafe { hew_deque_new() } }   // <- an ADOPTION
/// ```
///
/// Constructing that record is the program taking delivery of the host's handle
/// and naming `close` as its release. Reading the construction as "a container
/// embedding a foreign value" withholds the owner and the handle is never
/// closed at all.
///
/// # Why the composite-ownership rule does not reach these types
///
/// The rule the container mints enforce —
/// [`FreshOwnerVerdicts::value_is_free_of_opaque_foreign_provenance`] — rests on
/// a stated premise: *every composite release in this compiler is recursive and
/// generated from the container's LAYOUT, so there is no drop plan that frees
/// the container's spine while sparing a field.* For a `#[resource]` record that
/// premise is false. Its drop plan is
/// [`IrPipeline::lifecycle_registry`](crate::model::IrPipeline::lifecycle_registry):
/// codegen's `__hew_record_drop_inplace_<R>` thunk calls the user's
/// `<R>::close(self)` as the FIRST step, and only then tears the fields down
/// field-wise. The declared destructor IS the per-value drop plan the rule
/// assumed did not exist.
///
/// # Membership, and the clause that keeps it sound
///
/// A type is admitted when all three hold:
///
/// 1. it carries `ResourceMarker::Resource` in the module's
///    [`TypeClassTable`](hew_hir::TypeClassTable);
/// 2. that same table entry names its close method — the identical
///    `(marker, close)` entry lowering admits into the typed lifecycle registry,
///    so this authority and codegen cannot disagree about which types have a
///    declared release;
/// 3. **every declared field is one the post-close field-wise teardown cannot
///    free** — a scalar leaf, or an `#[opaque]` handle declared in this module
///    (an `#[opaque]` decl is a pointer-width slot with no fields and no
///    structural drop).
///
/// Clause 3 is the whole soundness argument and it is why this is not simply
/// "`#[resource]` types are exempt". The thunk runs `close(self)` and THEN the
/// field-wise teardown. For a type that satisfies clause 3 the second half frees
/// nothing, so the type's entire release is the one declared call and no
/// compiler-generated free can reach an operand. A `#[resource]` type with a
/// heap-owning field — `#[resource] type Conn { raw: Sock; log: string; }` — is
/// NOT admitted: its `log` really is torn down field-wise after `close`, so a
/// foreign value in that position would be freed by a plan the program never
/// declared, and its operands' provenance must keep flowing. That is the
/// fail-closed direction, and it costs a leak rather than a double release.
///
/// An EMPTY table admits nothing, so every default/unbuilt authority keeps the
/// pre-existing two-class behaviour.
#[derive(Debug, Clone, Default)]
pub struct DeclaredReleaseTypes {
    /// Admitted declaration identities. A semantic adoption proof is nominal:
    /// a same-leaf declaration from another owner must never inherit it.
    names: HashSet<String>,
}

impl DeclaredReleaseTypes {
    /// Build from the module's type declarations and its `#[resource]` close
    /// registry. See the type docs for the three admission clauses. The
    /// computation is hosted in [`hew_hir::declared_release`] so HIR's
    /// produced-value fact resolution reads the IDENTICAL admission — one
    /// authority, two IR consumers.
    #[must_use]
    pub fn from_module(module: &hew_hir::HirModule) -> Self {
        Self {
            names: hew_hir::declared_release::declared_release_type_names(module),
        }
    }

    /// True when a construction of `name` is an adoption: the constructed
    /// value's whole release is the type's declared close.
    #[must_use]
    pub fn release_is_declared(&self, name: &str) -> bool {
        self.names.contains(name)
    }

    /// True when this authority admits no type at all — the state every
    /// unbuilt/default authority is in, and the state that reproduces the
    /// pre-existing two-class behaviour exactly.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.names.is_empty()
    }
}

// The clause-3 field predicate (`field_is_released_only_by_the_declared_close`)
// is hosted beside the admission loop in `hew_hir::declared_release`; the
// re-export keeps it the one authority every MIR consumer names (the
// shell-drop cap's declared-release carve-out reads it through this path).
pub(crate) use hew_hir::declared_release::field_is_released_only_by_the_declared_close;

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
// The parameterized leaf walk — one structural authority
// ---------------------------------------------------------------------------

/// The interprocedural resolution of a `Call` callee, shared by every leaf
/// policy. The shared walk owns the recursion; the policy only classifies the
/// callee and leaf kinds whose meaning depends on the question being asked.
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

/// The leaf/callee decisions that differ among consumers of the shared
/// structural walk. Wrappers, aggregates, projections, and control-flow unions
/// live in [`return_alias_bits`]; the hooks below classify leaves whose meaning
/// depends on whether the consumer is proving non-aliasing or a releasable
/// producer.
pub trait LeafPolicy {
    /// Resolve a `Call`'s callee to its [`CallClass`].
    fn classify_call(&self, callee: &HirExpr) -> CallClass;

    /// Classify an expression that reaches the walk's fail-closed leaf (a
    /// `BindingRef`, a `Binary`, a method call, or any unmodelled form). Coarse
    /// returns `{OPAQUE}` unconditionally (today's `_ => true`); Precise applies
    /// the delta leaf rules.
    fn leaf_bits(&self, expr: &HirExpr) -> AliasBits;

    /// Classify a leaf that materializes its own value (`Literal`, `Index`,
    /// `Slice`, or `RecordCloneCall`). Return-provenance policies treat these as
    /// non-aliasing by default. Producer-provenance consumers may override this:
    /// for example, a static string literal does not materialize a caller-owned
    /// allocation and therefore must remain opaque to an owner mint.
    fn materialized_leaf_bits(&self, expr: &HirExpr) -> AliasBits {
        let _ = expr;
        AliasBits::EMPTY
    }

    /// Whether the scoped walk may see through an immutable `let` binding to
    /// its initializer. Return-provenance policies need this to follow values
    /// across local aliases. A consumer minting an anonymous temporary owner
    /// may disable it because the named binding already carries its own owner.
    fn see_through_immutable_binding(&self) -> bool {
        true
    }

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

    /// True when constructing the record named `name` ADOPTS its operands: the
    /// constructed value's whole release is the type's declared close, so no
    /// compiler-generated free can reach an operand and the operands'
    /// provenance does not flow into the construction. See
    /// [`DeclaredReleaseTypes`].
    ///
    /// Defaults to `false`, which is what keeps this confined to the two
    /// OWNERSHIP-provenance policies. [`CoarsePolicy`] and [`PrecisePolicy`]
    /// answer a different question — may this return value alias a by-value
    /// heap PARAMETER of the returning function — and for that question an
    /// adoption proves nothing: `Handle { raw: p }` over a borrowed parameter
    /// `p` still aliases `p`. They keep the default and stay byte-identical.
    fn construction_release_is_declared(&self, name: &str) -> bool {
        let _ = name;
        false
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
    // The public entry has no enclosing-block scope, so a bare `let`-bound-local
    // return leaf falls to the policy leaf (fail closed). The freshness fixpoint
    // uses [`return_alias_bits_in_block`] to seed the scope with the function
    // body, enabling the fix-(i) see-through.
    return_alias_bits_scoped(expr, policy, None)
}

/// [`return_alias_bits`] seeded with `body` as the enclosing scope, so a
/// return-value expression that is (or reaches) a `let`-bound-local reference is
/// seen through to the values that flow into that local (fix (i)). This is the
/// entry the module freshness fixpoint uses for every return path of a function.
pub fn return_alias_bits_in_block<P: LeafPolicy>(
    expr: &HirExpr,
    body: &HirBlock,
    policy: &P,
) -> AliasBits {
    let root = SeeThroughScope {
        block: body,
        parent: None,
    };
    return_alias_bits_scoped(expr, policy, Some(&root))
}

/// A chain of enclosing blocks whose `let`-bound locals the see-through may
/// resolve. Each `Block` the walk descends through pushes a frame; a
/// `BindingRef` leaf resolves against the chain (innermost first).
///
/// `pub(crate)` so the frozen reference threads the IDENTICAL scope plumbing
/// (only the may-alias transfer is reimplemented there).
pub(crate) struct SeeThroughScope<'a, 'p> {
    pub(crate) block: &'a HirBlock,
    pub(crate) parent: Option<&'p SeeThroughScope<'a, 'p>>,
}

impl<'a> SeeThroughScope<'a, '_> {
    /// The enclosing block that defines `id` as a single-assignment `let` local
    /// (never a `var`), searching innermost-first. `None` when `id` is a
    /// parameter, a `var`, or defined outside every tracked block.
    pub(crate) fn resolve(&self, id: BindingId) -> Option<&'a HirBlock> {
        if block_defines_immutable_let(self.block, id) {
            return Some(self.block);
        }
        self.parent.and_then(|p| p.resolve(id))
    }
}

/// True when `block` has exactly one `let <id> = ..` statement for `id` and that
/// binding is immutable (a `let`, not a `var`). A `var` is never seen through —
/// a later store could clobber its proven-fresh init with a param alias.
fn block_defines_immutable_let(block: &HirBlock, id: BindingId) -> bool {
    block.statements.iter().any(|stmt| {
        matches!(&stmt.kind, HirStmtKind::Let(binding, _) if binding.id == id && !binding.mutable)
    })
}

fn return_alias_bits_scoped<P: LeafPolicy>(
    expr: &HirExpr,
    policy: &P,
    scope: Option<&SeeThroughScope>,
) -> AliasBits {
    match &expr.kind {
        // Value-passthrough wrappers: the value flows from the tail / both
        // branches / every arm — aliases iff ANY reachable value aliases. A block
        // pushes a see-through scope frame for its own `let` locals.
        HirExprKind::Block(block) => match &block.tail {
            None => policy.missing_position_bits(expr),
            Some(tail) => {
                let child = SeeThroughScope {
                    block,
                    parent: scope,
                };
                return_alias_bits_scoped(tail, policy, Some(&child))
            }
        },
        HirExprKind::If {
            then_expr,
            else_expr,
            ..
        } => {
            let mut bits = return_alias_bits_scoped(then_expr, policy, scope);
            bits |= match else_expr.as_deref() {
                None => policy.missing_position_bits(expr),
                Some(e) => return_alias_bits_scoped(e, policy, scope),
            };
            bits
        }
        HirExprKind::Match { arms, .. } => {
            if arms.is_empty() {
                policy.missing_position_bits(expr)
            } else {
                arms.iter().fold(AliasBits::EMPTY, |acc, arm| {
                    acc | return_alias_bits_scoped(&arm.body, policy, scope)
                })
            }
        }
        HirExprKind::Return { value } => match value.as_deref() {
            None => policy.missing_position_bits(expr),
            Some(v) => return_alias_bits_scoped(v, policy, scope),
        },
        // Materialized leaves. Return-alias policies classify these as fresh:
        // a `.clone()` is a deep copy, a `Vec<T>` element load / slice is an
        // independent heap element, and a literal aliases no parameter.
        // Producer-provenance consumers may be stricter (a static string
        // literal, for example, carries no caller-owned allocation to release).
        HirExprKind::RecordCloneCall { .. }
        | HirExprKind::Index { .. }
        | HirExprKind::Slice { .. }
        | HirExprKind::Literal(_) => policy.materialized_leaf_bits(expr),
        // A construction aliases a parameter iff one of its owned operands does
        // — unless the construction is an ADOPTION, in which case the value's
        // whole release is the type's declared close and no compiler-generated
        // free reaches an operand. A functional-update base is excluded: it
        // re-wraps an already-constructed owner rather than taking delivery of
        // fresh operands, so it keeps the union.
        HirExprKind::StructInit {
            name, fields, base, ..
        } => {
            if base.is_none() && policy.construction_release_is_declared(name) {
                return AliasBits::EMPTY;
            }
            let mut bits = fields.iter().fold(AliasBits::EMPTY, |acc, (_, v)| {
                acc | return_alias_bits_scoped(v, policy, scope)
            });
            if let Some(base) = base.as_deref() {
                bits |= return_alias_bits_scoped(base, policy, scope);
            }
            bits
        }
        HirExprKind::TupleLiteral { elements } => {
            elements.iter().fold(AliasBits::EMPTY, |acc, e| {
                acc | return_alias_bits_scoped(e, policy, scope)
            })
        }
        HirExprKind::MachineVariantCtor { payload, .. } => match payload {
            None => AliasBits::EMPTY,
            Some(fields) => fields.iter().fold(AliasBits::EMPTY, |acc, (_, v)| {
                acc | return_alias_bits_scoped(v, policy, scope)
            }),
        },
        HirExprKind::Call { callee, args, .. } => match policy.classify_call(callee) {
            CallClass::Opaque => AliasBits::OPAQUE,
            CallClass::Fresh => AliasBits::EMPTY,
            CallClass::ParamSubst => args.iter().fold(AliasBits::EMPTY, |acc, a| {
                acc | return_alias_bits_scoped(a, policy, scope)
            }),
        },
        // A projection aliases a parameter iff its object chain does.
        HirExprKind::FieldAccess { object, .. } => return_alias_bits_scoped(object, policy, scope),
        HirExprKind::TupleIndex { tuple, .. } => return_alias_bits_scoped(tuple, policy, scope),
        // A reference to a `let`-bound local in scope: see THROUGH it to the
        // values that flow into that local (fix (i)) — the `let x = <fresh>; x`
        // idiom and the `[..]` array-literal desugar. Fails closed (the policy
        // leaf) for a `var`, a reassignment, a param root, or any use of the
        // local that could inject an unmeasured alias.
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } => {
            if policy.see_through_immutable_binding() {
                scope
                    .and_then(|s| s.resolve(*id))
                    .and_then(|block| see_through_let_binding_bits(block, *id, policy, scope, None))
                    .unwrap_or_else(|| policy.leaf_bits(expr))
            } else {
                policy.leaf_bits(expr)
            }
        }
        // Every other form (a `Binary`, a method call, a deref, any unmodelled
        // shape) is not provably fresh → the policy's leaf.
        _ => policy.leaf_bits(expr),
    }
}

/// Fresh-owner see-through (fix (i)): the alias bits contributed by a reference
/// to a `let`-bound local `id` defined in `block`, computed by seeing THROUGH the
/// binding to the values that actually flow into it.
///
/// Returns `None` — leaving the caller to keep the fail-closed `OPAQUE` leaf —
/// unless `id` is a single-assignment `let` local (never a `var`, never a param,
/// never reassigned) whose value is provably the union of its `let` initializer
/// and a set of interior container appends (`id.push(e)` — the array-literal
/// desugar). A direct consuming initializer may recursively see through one
/// earlier immutable binding (`let next = id`) when that move is the earlier
/// binding's only use. The returned bits are that union.
///
/// # Soundness (the double-free crux)
///
/// The value `id` holds at the tail read is EXACTLY its initializer plus every
/// element appended to it in the block; nothing else can be part of the value.
/// The walk therefore:
/// - unions `return_alias_bits(init)` and every append argument's bits — so a
///   param-aliasing element (`[h]` for a by-value heap param `h`) re-derives the
///   `OPAQUE`/`PARAM` leaf through the argument and the binding stays non-fresh
///   (a push that MOVES a borrowed param is conservatively rejected — the
///   over-approximation is a leak, never a double-free);
/// - refuses (`None`) to see through a `mutable` (`var`) binding, whose init a
///   later store could clobber with a param alias;
/// - refuses on any whole-binding reassignment (`Assign` targeting `id`);
/// - refuses on ANY other use of `id` — a call argument, an aggregate operand, a
///   field object that escapes — because such a use is a channel through which a
///   param alias could enter the value unmeasured.
/// - permits exactly one direct `Consume` move into the next immutable `let` in
///   a return chain; the complete statement scan rejects a second use, mutation,
///   reassignment, wrapper, projection, or escape of the moved source.
///
/// A param root is structurally excluded: a parameter has no `Let` in the block,
/// so `init_bits` stays `None` and the reference falls to the leaf. This is the
/// `let x = h; x` re-derivation the crux requires — `x`'s init is the param
/// `BindingRef`, whose bits are `OPAQUE`.
fn see_through_let_binding_bits<P: LeafPolicy>(
    block: &HirBlock,
    id: BindingId,
    policy: &P,
    scope: Option<&SeeThroughScope>,
    forwarded_to: Option<BindingId>,
) -> Option<AliasBits> {
    let mut init_bits: Option<AliasBits> = None;
    let mut init_is_move = false;
    let mut content = AliasBits::EMPTY;
    let mut saw_forward = false;
    for stmt in &block.statements {
        match &stmt.kind {
            // The defining `let`. A `var` (mutable) binding is never seen through
            // — a later store can replace the fresh init with a param alias. `id`
            // cannot appear in its own initializer, so the init bits are its base
            // value directly (walked with the SAME scope so a chained
            // `let y = ..; let x = y; x` re-derives `y`'s bits).
            HirStmtKind::Let(binding, init) if binding.id == id => {
                if binding.mutable {
                    return None;
                }
                let init = init.as_ref()?;
                let move_source = moved_binding_ref(init);
                init_is_move = move_source.is_some();
                init_bits = Some(if let Some(source_id) = move_source {
                    scope
                        .and_then(|s| s.resolve(source_id))
                        .and_then(|source_block| {
                            see_through_let_binding_bits(
                                source_block,
                                source_id,
                                policy,
                                scope,
                                Some(id),
                            )
                        })
                        .unwrap_or_else(|| return_alias_bits_scoped(init, policy, scope))
                } else {
                    return_alias_bits_scoped(init, policy, scope)
                });
            }
            // A whole-binding reassignment of `id` clobbers the proven-fresh init.
            HirStmtKind::Assign { target, .. } if place_root_binding(target) == Some(id) => {
                return None;
            }
            _ => {
                // A direct immutable move into the binding currently being
                // resolved is the source's one permitted forwarding use:
                // `let source = <fresh>; let destination = source; destination`.
                // The Consume intent distinguishes the move from a borrow, and
                // every other mention of `source` still fails closed below.
                if let HirStmtKind::Let(binding, Some(init)) = &stmt.kind {
                    if Some(binding.id) == forwarded_to
                        && !binding.mutable
                        && moved_binding_ref(init) == Some(id)
                    {
                        if init_bits.is_none() || saw_forward {
                            return None;
                        }
                        saw_forward = true;
                        continue;
                    }
                }
                // An interior container append rooted at `id` (`id.push(e)` — the
                // array-literal desugar's push statements): its arguments become
                // the container's content, so union their bits. A self-referential
                // append (`id.push(id)`) is unmodelled → fail closed.
                if let HirStmtKind::Expr(e) = &stmt.kind {
                    if let Some((receiver, args)) = method_receiver_and_args(e) {
                        if place_root_binding(receiver) == Some(id) {
                            if forwarded_to.is_some() || init_is_move {
                                return None;
                            }
                            for arg in &args {
                                if expr_mentions_binding(arg, id) {
                                    return None;
                                }
                                content |= return_alias_bits_scoped(arg, policy, scope);
                            }
                            continue;
                        }
                    }
                }
                // Any other statement that so much as mentions `id` could read it
                // out into an aliasable position → fail closed.
                if stmt_mentions_binding(stmt, id) {
                    return None;
                }
            }
        }
    }
    if forwarded_to.is_some() && !saw_forward {
        return None;
    }
    init_bits.map(|base| base | content)
}

/// The source binding of a direct consuming move (`let destination = source`).
/// Wrappers, projections, aggregate embedding, and non-consuming references are
/// deliberately excluded: only the exact immutable single-move chain is audited.
pub(crate) fn moved_binding_ref(expr: &HirExpr) -> Option<BindingId> {
    if expr.intent != hew_hir::IntentKind::Consume {
        return None;
    }
    match &expr.kind {
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } => Some(*id),
        _ => None,
    }
}

/// The receiver and argument expressions of an audited append method, or `None`
/// for every other expression. Used by [`see_through_let_binding_bits`] to
/// recognise an interior container append (`id.push(e)`) rooted at a tracked
/// binding.
///
/// The checker-authored [`hew_types::MethodTargetFamily`] is the identity
/// authority here: every supported `Vec<T>` push ABI has the same
/// `Vec(VecMethod::Push)` family even though its emitted symbol varies with
/// `T`. Dynamic/static trait dispatch, user var-self methods, numeric methods,
/// and every non-push collection method stay fail-closed. Merely having a
/// receiver and arguments is not proof that a call only appends those arguments
/// to the receiver.
///
/// `pub(crate)` so the frozen-reference see-through reuses the IDENTICAL
/// structural append-recognition (only the may-alias recursion is reimplemented
/// there), keeping the `coarse_verdict_differential` pin byte-identical.
pub(crate) fn method_receiver_and_args(expr: &HirExpr) -> Option<(&HirExpr, Vec<&HirExpr>)> {
    match &expr.kind {
        HirExprKind::ResolvedImplCall {
            receiver,
            target_family: hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Push),
            args,
            ..
        } => Some((receiver, args.iter().collect())),
        _ => None,
    }
}

/// True when `expr` references the binding `id` anywhere — including through an
/// unmodelled heap-bearing form (the `Reachable::unknown` fail-closed marker).
/// Reuses the total reachability visitor so no HIR shape can hide the reference.
///
/// `pub(crate)` so the frozen reference shares the identical mention check.
pub(crate) fn expr_mentions_binding(expr: &HirExpr, id: BindingId) -> bool {
    let mut r = Reachable::default();
    reachable_bindings(expr, &mut r);
    r.unknown || r.bindings.contains(&id)
}

/// True when any expression reachable from `stmt` references the binding `id`
/// (fail-closed on an unmodelled form). The statement-level companion of
/// [`expr_mentions_binding`].
///
/// `pub(crate)` so the frozen reference shares the identical mention check.
pub(crate) fn stmt_mentions_binding(stmt: &HirStmt, id: BindingId) -> bool {
    let mut r = Reachable::default();
    reachable_bindings_in_stmt(stmt, &mut r);
    r.unknown || r.bindings.contains(&id)
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

/// [`coarse_may_alias_borrow`] seeded with the function `body` as the enclosing
/// scope, so a return-value that reaches a `let`-bound-local reference is seen
/// through to its fresh contents (fix (i)). The freshness fixpoint's
/// `return_value_may_alias_borrow` uses this; the bare
/// [`coarse_may_alias_borrow`] (no scope) is kept for any consumer without a
/// function body in hand.
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "only ever called with the pipeline's default-hasher freshness summary map"
)]
pub fn coarse_may_alias_borrow_in_body(
    expr: &HirExpr,
    body: &HirBlock,
    fresh: &HashMap<hew_hir::ItemId, bool>,
) -> bool {
    !return_alias_bits_in_block(expr, body, &CoarsePolicy { fresh }).is_fresh()
}

// ---------------------------------------------------------------------------
// Opaque-extern laundering summary — the table veto on the freshness fact
// ---------------------------------------------------------------------------

/// The `LeafPolicy` that answers ONE question about a return value: can it be
/// (or embed, or project, or launder) the result of an ownership-OPAQUE extern?
///
/// # Why the freshness summary needs this veto
///
/// [`CoarsePolicy`] answers a different question — may this return value alias a
/// by-value heap PARAMETER of the returning function — and for a body-less
/// resolved item it answers "no" through `unwrap_or(true)`. That is sound for
/// its own consumers (a foreign return does not alias a Hew parameter) and it is
/// pinned byte-identical by the frozen-reference differential.
///
/// It is not a proof that the caller may RELEASE the value. A declared
/// `extern "C" fn host() -> string` is body-less, so `unwrap_or(true)` marks it
/// fresh and a Hew WRAPPER inherits that verdict:
///
/// ```hew
/// extern "C" { fn host_string() -> string; }
/// fn wrapper() -> string { unsafe { host_string() } }   // coarse: FRESH
/// fn main() -> i64 { println(f"value={wrapper()}"); 0 }
/// ```
///
/// The wrapper's row then licenses a caller-side owner over an un-audited
/// foreign handle through ONE visible Hew frame. That is observable, not
/// theoretical: an extern declared in a module with stdlib provenance is
/// classified `HeaderAware` ([`crate::model::classify_extern_string_ownership`]),
/// so codegen does NOT adopt-and-copy its return and the minted release lands on
/// the host's own live handle.
///
/// # The transfer
///
/// The bits produced here mean "tainted by an opaque extern", not "aliases a
/// parameter", so the clauses read differently from `CoarsePolicy`:
///
/// 1. a non-item callee (closure, fn-pointer param, dynamic dispatch) can hand
///    back anything, including a value it obtained from a host → `Opaque`;
/// 2. a DECLARED EXTERN — claimed by NAME *and* by declaration id, ahead of every
///    id lookup — is clean ONLY with an audited fresh-`+1`-return row, and
///    `Opaque` otherwise. The name is the primary key because an extern call
///    site's `ResolvedRef::Item` carries a placeholder id rather than the
///    declaration's; the id is checked too so a future lowering that resolves the
///    real id cannot slip past;
/// 3. an ANALYZED module body reads its own row in the taint set under
///    construction — this is the TRANSITIVE step. An already-tainted callee is
///    `Opaque`; an as-yet-clean callee is `ParamSubst`, which keeps walking its
///    ARGUMENTS, so `fn c() -> string {{ forward(unsafe {{ ext() }}) }}` is
///    tainted through the argument even when `forward` itself is clean;
/// 4. a body-less NON-extern resolved item (an aggregate constructor, the minted
///    `string_concat` shim an f-string tail lowers to, a runtime primitive) keeps
///    the cross-ABI owned-return treatment through
///    [`bodyless_item_is_audited_owned_return`] — the one explicit carve-out,
///    unreachable for anything clause 2 owns.
///
/// A non-`Call` leaf contributes NOTHING (`EMPTY`): a literal, an index, a
/// binding are not extern results. That is not a hole, because this summary is
/// only ever read as a VETO on top of the coarse freshness proof, and the coarse
/// proof already fails closed (`OPAQUE`) on every leaf its walk reaches. A path
/// the coarse proof admits therefore consists exclusively of the structural arms
/// and `Call`s this walk classifies — the two walks visit the same nodes.
#[derive(Debug)]
pub struct OpaqueExternTaintPolicy<'a> {
    /// The audited extern owned-return contract table — the authority for every
    /// declared `extern "C"` callee.
    pub extern_table: &'a ExternContractTable,
    /// The `ItemId`s of the module bodies the summary analyzes.
    pub analyzed: &'a HashSet<hew_hir::ItemId>,
    /// The taint set under construction (the fixpoint state).
    pub tainted: &'a HashSet<hew_hir::ItemId>,
    /// The types whose release the program declares — the adoption boundary.
    pub declared_release: &'a DeclaredReleaseTypes,
}

impl LeafPolicy for OpaqueExternTaintPolicy<'_> {
    fn classify_call(&self, callee: &HirExpr) -> CallClass {
        // Clause 1 — an indirect/closure/dynamic callee can hand back anything.
        let HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Item(item_id),
            ..
        } = &callee.kind
        else {
            return CallClass::Opaque;
        };
        // Clause 2 — the audited extern authority, ahead of every id lookup.
        if self.extern_table.is_extern_name(name) || self.extern_table.is_extern_id(*item_id) {
            return if self.extern_table.extern_return_is_audited_fresh_owner(name) {
                CallClass::Fresh
            } else {
                CallClass::Opaque
            };
        }
        // Clause 3 — an analyzed module body: the transitive step.
        if self.analyzed.contains(item_id) {
            return if self.tainted.contains(item_id) {
                CallClass::Opaque
            } else {
                CallClass::ParamSubst
            };
        }
        // Clause 4 — the explicit body-less carve-out.
        if bodyless_item_is_audited_owned_return(name, *item_id, self.extern_table) {
            CallClass::Fresh
        } else {
            CallClass::Opaque
        }
    }

    fn leaf_bits(&self, _expr: &HirExpr) -> AliasBits {
        // A non-call leaf is not an extern result. See the type doc: this
        // summary is a veto on top of a coarse proof that already fails closed
        // on every leaf it reaches.
        AliasBits::EMPTY
    }

    fn missing_position_bits(&self, _enclosing: &HirExpr) -> AliasBits {
        // An absent value position carries no value at all, so it carries no
        // foreign value either. Same reasoning as `leaf_bits`.
        AliasBits::EMPTY
    }

    fn construction_release_is_declared(&self, name: &str) -> bool {
        self.declared_release.release_is_declared(name)
    }
}

/// The DUAL of [`OpaqueExternTaintPolicy`], used to SUPPRESS a release rather
/// than to license one.
///
/// The two policies answer opposite questions, and each is fail-closed for the
/// consumer it serves:
///
/// * [`OpaqueExternTaintPolicy`] answers "is this value PROVABLY free of foreign
///   provenance?", and its consumers mint a caller-side release when it says
///   yes. Doubt must therefore read as foreign, so an indirect callee and an
///   unanalysed item both classify `Opaque`. Being wrong the other way would be
///   a DOUBLE RELEASE.
/// * This policy answers "is this value PROVABLY foreign?", and its one consumer
///   REMOVES a release the compiler would otherwise emit. Doubt must therefore
///   read as domestic, so an indirect callee and an unanalysed item both
///   classify `Fresh`. Being wrong the other way would be a LEAK in ordinary
///   code that never touches an extern at all.
///
/// This is not a permissive second opinion on freshness: it can never turn a
/// `false` from the first policy into a licence to release. Its only power is to
/// take a release away, and it exercises that power exactly when the audited
/// [`ExternContractTable`] — the same single source of truth — says a declared
/// extern with no audited fresh-owner return is in the value's history.
#[derive(Debug)]
pub struct ProvenForeignPolicy<'a> {
    /// The audited extern owned-return contract table. The ONLY thing that can
    /// inject foreignness here.
    pub extern_table: &'a ExternContractTable,
    /// The `ItemId`s of the module bodies the summary analyzes.
    pub analyzed: &'a HashSet<hew_hir::ItemId>,
    /// The proven-foreign taint set (the fixpoint state).
    pub tainted: &'a HashSet<hew_hir::ItemId>,
    /// The types whose release the program declares — the adoption boundary.
    pub declared_release: &'a DeclaredReleaseTypes,
}

impl LeafPolicy for ProvenForeignPolicy<'_> {
    fn classify_call(&self, callee: &HirExpr) -> CallClass {
        // Clause 1 — an indirect/closure/dynamic callee is UNKNOWN, and unknown
        // is not proof. See the type doc for why the polarity flips here.
        let HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Item(item_id),
            ..
        } = &callee.kind
        else {
            return CallClass::Fresh;
        };
        // Clause 2 — the sole injection point: a declared extern that is a
        // FOREIGN host, carries no audited fresh-owner return, and carries no
        // audited owned-RESULT contract either.
        //
        // A `std.*` extern is this compiler's own runtime ABI, reached through
        // its own headers and covered by its own suites; the stdlib's
        // `Vec<string>` / `bytes` / `Stream` / channel handle producers are
        // owned returns by that ABI, and suppressing their bindings' releases
        // would leak in every program that uses the standard library. It is
        // still not MINTABLE — the strict policy vetoes it exactly as before —
        // it merely is not PROOF of foreignness.
        //
        // The declaring module's identity is not the only evidence available,
        // and on its own it is not stable under HOW a file is compiled: the
        // same `std/process.hew` is `Module("std.process")` when imported and
        // `Root` when handed to `hew check` directly. The audited
        // `[[ownership.contracts]]` result axis is evidence about the CALLEE
        // and is invariant under that, so it is conjoined here. An audited
        // owned result is a value the caller owns; calling it proof of an
        // un-releasable foreign handle is simply wrong, and it is what refused
        // `std/process.hew` and `std/time/cron/cron.hew`.
        if self.extern_table.is_extern_name(name) || self.extern_table.is_extern_id(*item_id) {
            return if self.extern_table.extern_return_is_audited_fresh_owner(name)
                || self
                    .extern_table
                    .extern_result_is_provably_not_a_foreign_handle(name)
                || !self.extern_table.extern_is_foreign_host(name, *item_id)
            {
                CallClass::Fresh
            } else {
                CallClass::Opaque
            };
        }
        // Clause 3 — an analyzed module body: the transitive step, read out of
        // THIS set so the proof stays a proof across Hew frames.
        if self.analyzed.contains(item_id) {
            return if self.tainted.contains(item_id) {
                CallClass::Opaque
            } else {
                CallClass::ParamSubst
            };
        }
        // Clause 4 — an unanalysed body-less item is unknown, not proven.
        CallClass::Fresh
    }

    fn leaf_bits(&self, expr: &HirExpr) -> AliasBits {
        // See `composite_position_bits`: the suppression side asks whether a
        // foreign value is REACHABLE, and a container literal is how one hides.
        composite_position_bits(expr, self).unwrap_or(AliasBits::EMPTY)
    }

    fn missing_position_bits(&self, _enclosing: &HirExpr) -> AliasBits {
        AliasBits::EMPTY
    }

    fn construction_release_is_declared(&self, name: &str) -> bool {
        self.declared_release.release_is_declared(name)
    }
}

/// Reads a set of BINDINGS the enclosing lowering has already proven foreign,
/// so the same structural walk that answers the module-level provenance
/// questions also answers "does this value embed a handle that reached here
/// through a `let`?".
///
/// The module authority's walk treats a `BindingRef` as a leaf with no scope in
/// hand, so a foreign handle laundered through one binder re-enters every
/// container mint clean. This policy carries the missing fact — and nothing
/// else: calls classify `ParamSubst` so the walk descends into their arguments
/// without ever ruling on the callee, which stays the authority's business.
#[derive(Debug)]
struct ProvenForeignBindingPolicy<'a> {
    foreign: &'a HashSet<BindingId>,
    declared_release: &'a DeclaredReleaseTypes,
}

impl LeafPolicy for ProvenForeignBindingPolicy<'_> {
    fn classify_call(&self, _callee: &HirExpr) -> CallClass {
        CallClass::ParamSubst
    }

    fn leaf_bits(&self, expr: &HirExpr) -> AliasBits {
        match &expr.kind {
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } if self.foreign.contains(id) => AliasBits::OPAQUE,
            _ => composite_position_bits(expr, self).unwrap_or(AliasBits::EMPTY),
        }
    }

    fn missing_position_bits(&self, _enclosing: &HirExpr) -> AliasBits {
        AliasBits::EMPTY
    }

    fn construction_release_is_declared(&self, name: &str) -> bool {
        self.declared_release.release_is_declared(name)
    }
}

/// Fold `policy` over the value positions of a COMPOSITE literal.
///
/// [`return_alias_bits`] deliberately stops at a container literal: on the
/// mint side a container is the thing being decided about, so descending would
/// beg the question. The two SUPPRESSION-side policies want the opposite —
/// they are asking whether a foreign value is *reachable* from this
/// expression, and a container is exactly how one hides. Both call this from
/// their `leaf_bits`, so the recursion is opt-in and cannot alter the strict
/// walk.
///
/// The composite-ownership rule makes this sound: a container with any
/// opaque-provenance embed is never minted as caller-owned, so removing its
/// binder's release cannot remove a release the program was entitled to.
fn composite_position_bits<P: LeafPolicy>(expr: &HirExpr, policy: &P) -> Option<AliasBits> {
    let parts: Vec<&HirExpr> = match &expr.kind {
        HirExprKind::TupleLiteral { elements } => elements.iter().collect(),
        HirExprKind::StructInit { fields, base, .. } => fields
            .iter()
            .map(|(_, e)| e)
            .chain(base.iter().map(std::convert::AsRef::as_ref))
            .collect(),
        HirExprKind::MachineVariantCtor { payload, .. } => payload
            .iter()
            .flat_map(|fields| fields.iter().map(|(_, e)| e))
            .collect(),
        _ => return None,
    };
    Some(parts.into_iter().fold(AliasBits::EMPTY, |acc, part| {
        acc | return_alias_bits(part, policy)
    }))
}

/// True when `expr` reads any binding in `foreign` at a value position.
///
/// Used to conjoin a lowering's per-function proven-foreign ledger onto
/// [`FreshOwnerVerdicts::value_is_free_of_opaque_foreign_provenance`]. It runs
/// the SAME structural walk, so the two halves agree about which positions of a
/// composite carry its value.
#[must_use]
pub(crate) fn value_reads_a_proven_foreign_binding(
    expr: &HirExpr,
    foreign: &HashSet<BindingId>,
    declared_release: &DeclaredReleaseTypes,
) -> bool {
    if foreign.is_empty() {
        return false;
    }
    return_alias_bits(
        expr,
        &ProvenForeignBindingPolicy {
            foreign,
            declared_release,
        },
    )
    .contains(AliasBits::OPAQUE)
}

/// The ONE explicit path by which a body-less resolved item keeps the cross-ABI
/// owned-return treatment in [`OpaqueExternTaintPolicy`] (clause 4).///
/// The class it covers is the compiler's OWN body-less items: aggregate
/// constructors, the minted stdlib shims an f-string tail lowers to
/// (`string_concat`), and the runtime primitives behind `RecordCloneCall` /
/// `Index` / `Slice`. Every one of them is emitted by this compiler under the
/// owned-return contract — the same trust the walk's fresh structural leaves
/// already extend, and the treatment the coarse policy gives them today.
///
/// It is NOT a place to admit a foreign callee, and it does not widen the
/// audited set. A declared extern can never reach here: clause 2 claims every
/// extern by NAME and by declaration ID first, both answered from the
/// [`ExternContractTable`] built from the module's `HirItem::ExternFn`
/// declarations, imports included. Widening this predicate to cover an extern —
/// or widening the table's audited fresh-return set so an extern reaches clause
/// 2's clean arm — reopens exactly the laundering this summary exists to close.
#[must_use]
fn bodyless_item_is_audited_owned_return(
    name: &str,
    id: hew_hir::ItemId,
    extern_table: &ExternContractTable,
) -> bool {
    !extern_table.is_extern_name(name) && !extern_table.is_extern_id(id)
}

/// The module-global OPAQUE-EXTERN LAUNDERING summary: every `ItemId` whose
/// function can hand back a value that crossed an ownership-opaque extern.
///
/// This is the veto a RELEASE mint must apply on top of the coarse freshness
/// proof (`compute_fn_returns_fresh_owner`), which cannot see externs at all.
/// Membership is TRANSITIVE by construction: every callee's row is read out of
/// the set being built, so a wrapper, a wrapper of a wrapper, a generic wrapper
/// (analyzed once at its origin `ItemId`, which is what a monomorphisation
/// resolves to) and a recursive-looking wrapper all end up in it.
///
/// Monotone least-fixpoint from the EMPTY set, growing only: taint is injected
/// by non-recursive transfers (an opaque extern callee, an indirect callee) and
/// propagated by union, so a cycle that touches an extern anywhere taints every
/// member, while a cycle that touches none stays clean. The set is finite and a
/// pass only ever adds, so it converges.
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "built once over the pipeline's default-hasher origin_fns map"
)]
pub fn compute_fn_return_launders_opaque_extern(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    extern_table: &ExternContractTable,
    declared_release: &DeclaredReleaseTypes,
) -> HashSet<hew_hir::ItemId> {
    let analyzed: HashSet<hew_hir::ItemId> = fns.keys().copied().collect();
    let mut tainted: HashSet<hew_hir::ItemId> = HashSet::new();
    loop {
        let mut changed = false;
        for (&id, &f) in fns {
            if tainted.contains(&id) {
                continue;
            }
            let policy = OpaqueExternTaintPolicy {
                extern_table,
                analyzed: &analyzed,
                tainted: &tainted,
                declared_release,
            };
            let mut return_values: Vec<&HirExpr> = Vec::new();
            crate::lower::collect_return_values_in_block(&f.body, &mut return_values);
            if let Some(tail) = &f.body.tail {
                if !matches!(tail.ty, ResolvedTy::Unit | ResolvedTy::Never) {
                    return_values.push(tail);
                }
            }
            if return_values
                .iter()
                .any(|e| !return_alias_bits_in_block(e, &f.body, &policy).is_fresh())
            {
                tainted.insert(id);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    tainted
}

/// The PROVEN-FOREIGN companion to
/// [`compute_fn_return_launders_opaque_extern`]: every `ItemId` whose function
/// can hand back a value that provably crossed a declared, non-audited extern.
///
/// Same monotone least-fixpoint, same audited table, same transfer function —
/// the only difference is [`ProvenForeignPolicy`]'s flipped treatment of the
/// UNKNOWN cases (an indirect callee, an unanalysed body-less item), which
/// inject nothing here. It is therefore a SUBSET of the strict taint set by
/// construction, and it exists because its consumer removes releases instead of
/// adding them.
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "built once over the pipeline's default-hasher origin_fns map"
)]
pub fn compute_fn_return_carries_proven_foreign(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    extern_table: &ExternContractTable,
    declared_release: &DeclaredReleaseTypes,
) -> HashSet<hew_hir::ItemId> {
    let analyzed: HashSet<hew_hir::ItemId> = fns.keys().copied().collect();
    let mut tainted: HashSet<hew_hir::ItemId> = HashSet::new();
    loop {
        let mut changed = false;
        for (&id, &f) in fns {
            if tainted.contains(&id) {
                continue;
            }
            let policy = ProvenForeignPolicy {
                extern_table,
                analyzed: &analyzed,
                tainted: &tainted,
                declared_release,
            };
            let mut return_values: Vec<&HirExpr> = Vec::new();
            crate::lower::collect_return_values_in_block(&f.body, &mut return_values);
            if let Some(tail) = &f.body.tail {
                if !matches!(tail.ty, ResolvedTy::Unit | ResolvedTy::Never) {
                    return_values.push(tail);
                }
            }
            if return_values
                .iter()
                .any(|e| !return_alias_bits_in_block(e, &f.body, &policy).is_fresh())
            {
                tainted.insert(id);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    tainted
}

// ---------------------------------------------------------------------------
// Type short-circuit — the scalar non-heap leaf (needs no layout registry)
// ---------------------------------------------------------------------------

/// True for a resolved type that is a scalar (or `unit`/`never`) leaf — a value
/// that provably owns no heap and therefore cannot alias any heap parameter.
///
/// Conservative on purpose: it fires ONLY for the primitive-scalar leaves the
/// type short-circuit needs without a layout registry. Hosted in
/// [`hew_hir::declared_release`] beside the clause-3 field predicate that
/// consumes it; re-exported here so every established MIR path keeps naming
/// the one authority.
pub use hew_hir::declared_release::ty_is_scalar_non_heap;

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
    "hew_vec_take_owned",
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
        ResultOwnership::FreshOwnedString
        | ResultOwnership::FreshOwnedBytes
        | ResultOwnership::FreshOwnedVec => AliasBits::EMPTY,
        // `IndependentValue` stays OPAQUE here. It answers the OWNERSHIP
        // question (the caller may keep this value after the receiver dies),
        // which is weaker than the ALIAS question this function asks; the
        // proved-owner symbols above already carve out the cases that have been
        // audited for provenance. Tightening this arm is a separate proof.
        ResultOwnership::Borrowed
        | ResultOwnership::InteriorAliasOfReceiver
        | ResultOwnership::IndependentValue
        | ResultOwnership::Untracked => AliasBits::OPAQUE,
    }
}

/// Every `VecElementProfile` the dispatch authority can be asked about.
///
/// The product is deliberately UNCONSTRAINED — it enumerates combinations no
/// real element ever has (`is_owned` with a `Str` ABI, say). That over-breadth
/// is the safe direction: [`placeholder_method_return_provenance`] UNIONS the
/// answers, so a spurious profile can only add `{OPAQUE}` and push the verdict
/// further fail-closed. Constraining it could only remove an `{OPAQUE}`
/// contributor, which is the unsafe direction.
fn every_vec_element_profile() -> impl Iterator<Item = hew_types::vec_authority::VecElementProfile>
{
    use hew_types::vec_authority::{VecElementProfile, VecElementToken};
    const TOKENS: &[Option<VecElementToken>] = &[
        None,
        Some(VecElementToken::Bool),
        Some(VecElementToken::I8),
        Some(VecElementToken::U8),
        Some(VecElementToken::I16),
        Some(VecElementToken::U16),
        Some(VecElementToken::I32),
        Some(VecElementToken::I64),
        Some(VecElementToken::F32),
        Some(VecElementToken::F64),
        Some(VecElementToken::Str),
        Some(VecElementToken::Ptr),
        Some(VecElementToken::Layout),
    ];
    TOKENS.iter().flat_map(|abi| {
        (0..16u8).map(move |mask| VecElementProfile {
            abi: *abi,
            is_owned: mask & 1 != 0,
            is_copy_layout: mask & 2 != 0,
            is_function_like: mask & 4 != 0,
            is_abstract: mask & 8 != 0,
        })
    })
}

/// Return-provenance of a method call whose HIR `target_symbol` is a
/// PLACEHOLDER rather than the symbol the call site will actually emit, or
/// `None` when the placeholder is not one this can answer.
///
/// # Why a placeholder needs its own answer
///
/// [`method_return_provenance`] is keyed on the EMITTED symbol, and that is not
/// negotiable — keying on a family would admit the receiver-alias class it
/// exists to reject. But two HIR shapes never carry the emitted symbol:
///
///  * a `HashMap::get`, which the checker records as `hew_hashmap_get_layout`
///    while lowering ALWAYS emits the fresh-owner clone choke
///    (`hew_hashmap_get_clone_layout` — `lower_hashmap_index_trap`); and
///  * an element-typed `Vec<T>` method on a GENERIC spine, whose checker
///    dispatch left a `hew_vec_*_FAMILY` placeholder because the element was
///    still a type parameter.
///
/// Looking those spellings up in the emitted-symbol contract yields `Untracked`
/// → `{OPAQUE}`, so the summary of `fn first<T>(v: Vec<T>) -> Option<T> {
/// v.get(0) }` read "may alias the receiver" even though every element class it
/// can ever be instantiated at emits `hew_vec_get_clone`, a proved `+1` owner.
/// The consumer then withheld a legitimate release.
///
/// # The missing input, and how it is supplied
///
/// The input the emitted-symbol resolution needs is the ELEMENT CLASS, and that
/// is genuinely unreachable here: this summary is computed once over the
/// pre-monomorphisation origin function, where the element is still `T`.
///
/// Failing closed on an unreachable input is not the only option. The element
/// class is drawn from a FINITE, enumerable domain, and the resolver
/// ([`hew_types::vec_authority::resolve_runtime_symbol`], the same authority the
/// call lowering consults) is a total function of it. So instead of asking "what
/// does this site emit?" — unanswerable — ask "what could it emit, over every
/// element class?" and UNION the contracts of the answers. When every resolvable
/// element class agrees on a proved-owner symbol, the verdict is knowable
/// WITHOUT the missing input. When they disagree, the union carries `{OPAQUE}`
/// and the result is exactly the fail-closed answer this replaced.
///
/// That makes the widening additive-only by construction: the union is over a
/// superset of the reachable profiles, so it can never be MORE permissive than a
/// per-monomorphisation resolution would be.
///
/// A placeholder with no resolvable profile at all (an element ABI the authority
/// only ever reports `Deferred`/`Unavailable`/`Unsupported` for) yields `None`,
/// and the caller falls back to the raw-symbol contract — still fail-closed.
#[must_use]
pub fn placeholder_method_return_provenance(
    target_symbol: &str,
    target_family: hew_types::MethodTargetFamily,
) -> Option<AliasBits> {
    use hew_types::vec_authority::{
        resolve_runtime_symbol, VecResolutionContext, VecSymbolResolution,
    };
    use hew_types::{HashMapMethod, MethodTargetFamily};

    // `HashMap::get -> Option<V>` is the degenerate quantification: the emitted
    // symbol does not depend on the element class at all.
    if matches!(
        target_family,
        MethodTargetFamily::HashMap(HashMapMethod::Get)
    ) {
        return Some(method_return_provenance("hew_hashmap_get_clone_layout"));
    }

    if !target_symbol.ends_with("_FAMILY") {
        return None;
    }
    let MethodTargetFamily::Vec(vec_method) = target_family else {
        return None;
    };

    let mut resolved_any = false;
    let mut bits = AliasBits::EMPTY;
    for profile in every_vec_element_profile() {
        if let VecSymbolResolution::Resolved(symbol) = resolve_runtime_symbol(
            vec_method,
            profile,
            VecResolutionContext::MonomorphizedPlaceholder,
        ) {
            resolved_any = true;
            bits |= method_return_provenance(&symbol);
        }
    }
    resolved_any.then_some(bits)
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
    /// The NAME-keyed mirror of the audited owned-return allowlist: an extern
    /// whose RETURN is proved a fresh `+1` owner (interim: the scalar-return
    /// externs — a scalar owns nothing and aliases nothing). Name-keyed
    /// because an extern call site cannot be resolved by id (see `names`).
    ///
    /// Every heap-returning extern is ABSENT — that is the whole point of the
    /// interim table (`evil() -> string` returning an interior pointer).
    fresh_return_names: HashSet<String>,
    /// Declared externs whose block was NOT lowered from a `std.*` module —
    /// the genuinely FOREIGN hosts, as distinct from this compiler's own
    /// runtime ABI. Keyed by declaration `ItemId` and by name, from the
    /// per-item [`hew_hir::ExternProvenance`] record captured at HIR lowering:
    /// a positive fact, not a name-prefix guess, so a root extern that spells
    /// its symbol `hew_channel_new` is still `Root` and still foreign.
    ///
    /// Read ONLY by [`ProvenForeignPolicy`], which suppresses releases. The
    /// strict [`OpaqueExternTaintPolicy`] that LICENSES releases ignores this
    /// split entirely — a std extern is exactly as un-mintable as a root one.
    foreign_decl_ids: HashSet<hew_hir::ItemId>,
    foreign_names: HashSet<String>,
    /// The NAME-keyed audited ARGUMENT contract: an extern proved to BORROW the
    /// heap arguments it is handed rather than to consume or retain them.
    ///
    /// # The declared-borrow provenance class
    ///
    /// There is no `return_ty`-shaped shortcut here as there is for scalar
    /// returns: a `string` PARAMETER is a pointer the callee may retain or
    /// release no matter what the declaration says. So a name enters this set
    /// only when an AUDIT says so — see
    /// [`build_extern_contract_table`] for the two admission clauses, for the
    /// argument that they are a proof rather than a fallback, and for why an
    /// UNAUDITED parameter is read as possibly-taken (a leak) rather than as a
    /// declared borrow (a double release).
    ///
    /// Admission keys on the SYMBOL, never on the declaring module's
    /// [`hew_hir::ExternProvenance`]: the same extern reached as a root
    /// compilation unit and through an import must get the same answer.
    borrowing_arg_names: HashSet<String>,
    /// Declared externs whose RESULT is provably not an un-releasable foreign
    /// handle — the set that answers "this call is not PROOF of foreignness".
    ///
    /// # Why the argument axis alone was not enough
    ///
    /// `borrowing_arg_names` reads the ARGUMENT axis of
    /// [`crate::ffi_contracts::extern_ownership_contract`] and nothing read the
    /// RESULT axis of those same rows. The gap only became visible when the
    /// declaring module's identity stopped agreeing with itself: `std/process.hew`
    /// reached through `import std::process` carries `Module("std.process")`
    /// provenance, but the SAME file handed to `hew check` directly is the root
    /// compilation unit and carries `Root`. Under the second reading
    /// `hew_process_last_error` — `result = "fresh"`,
    /// `release-symbol = "hew_string_drop"` — was classified a non-audited
    /// foreign host and the standard library was refused by the compiler that
    /// ships it.
    ///
    /// # Read ONLY by the suppression side
    ///
    /// [`ProvenForeignPolicy`] consults this; the strict
    /// [`OpaqueExternTaintPolicy`] does not. So membership never LICENSES a
    /// caller-side mint — an extern in this set is exactly as un-mintable as
    /// before. Its only effect is to stop calling the value proof of
    /// foreignness, which is what the two admission clauses actually establish.
    ///
    /// See [`build_extern_contract_table`] for the clauses.
    audited_domestic_return_names: HashSet<String>,
    /// Every declared `extern "C"` fn DECLARATION id.
    ///
    /// The name set above is the primary key (an extern call site's
    /// `ResolvedRef::Item` carries a placeholder id today), but a summary that
    /// mints a RELEASE obligation must not depend on that staying true: if a
    /// future lowering resolves an extern call to its real declaration id, the
    /// name lookup alone would still answer correctly while an id-keyed consumer
    /// would not. Both keys are therefore checked, and either one claims the
    /// callee for the extern contract.
    decl_ids: HashSet<hew_hir::ItemId>,
}

impl ExternContractTable {
    /// True when `name` is a declared `extern "C"` fn — the callee must be
    /// classified by the extern contract (interim: `{OPAQUE}` for every
    /// heap-or-unknown return), never by an id lookup.
    #[must_use]
    pub fn is_extern_name(&self, name: &str) -> bool {
        self.names.contains(name)
    }

    /// True when `id` is a declared `extern "C"` fn's DECLARATION id — the
    /// id-keyed companion of [`ExternContractTable::is_extern_name`]. See
    /// `decl_ids` for why both keys are checked by the ownership consumers.
    #[must_use]
    pub fn is_extern_id(&self, id: hew_hir::ItemId) -> bool {
        self.decl_ids.contains(&id)
    }

    /// True when `name` is a declared extern whose RETURN carries an audited
    /// fresh-`+1`-owner contract. A declared extern that is NOT in the audited
    /// set is ownership-OPAQUE: its result is neither provably fresh nor
    /// provably borrowed, so no caller-side release obligation may be minted
    /// for it.
    ///
    /// This is the ownership authority for an extern callee. Membership in the
    /// call-DISPATCH set (`Builder::module_fn_names`, which deliberately
    /// carries every `HirItem::ExternFn` so its calls lower as
    /// `Terminator::Call`) says NOTHING about ownership and must never be
    /// consulted in its place.
    #[must_use]
    pub fn extern_return_is_audited_fresh_owner(&self, name: &str) -> bool {
        self.fresh_return_names.contains(name)
    }

    /// True when this declared extern's block came from OUTSIDE the standard
    /// library — a root compilation unit or a user package module. See
    /// `foreign_decl_ids` for why only the suppression side reads it.
    #[must_use]
    pub fn extern_is_foreign_host(&self, name: &str, id: hew_hir::ItemId) -> bool {
        self.foreign_decl_ids.contains(&id) || self.foreign_names.contains(name)
    }

    /// True when `name` is a declared extern with an audited ARGUMENT contract
    /// proving it BORROWS the heap arguments it is passed, so the caller keeps
    /// the sole release obligation for them.
    ///
    /// An extern with no such audit answers `false`: its behaviour at its
    /// parameters is unknowable, so the caller must assume the handle was
    /// consumed or retained and must NOT keep a release obligation for it.
    ///
    /// Fail-closed direction: `false` costs a leak; `true` on a consuming host
    /// costs a double release (heap corruption). That asymmetry is why the
    /// admission in [`build_extern_contract_table`] requires EVERY parameter to
    /// be audited `Borrow`, not merely the heap-typed ones.
    #[must_use]
    pub fn extern_borrows_audited_heap_args(&self, name: &str) -> bool {
        self.borrowing_arg_names.contains(name)
    }

    /// True when `name` is a declared extern whose RESULT is provably not an
    /// un-releasable foreign handle — either because an audited row says the
    /// call transfers a newly owned allocation together with the release that
    /// balances it, or because the declared return type provably contains no
    /// pointer at all.
    ///
    /// This is a statement about what the result CANNOT be, so it is exactly as
    /// strong as its two clauses and no stronger: it says the value is not
    /// proof of foreignness. It does NOT say the caller may mint a release
    /// wherever it likes — that remains
    /// [`ExternContractTable::extern_return_is_audited_fresh_owner`]'s much
    /// narrower claim, and the strict mint-side policy still reads only that.
    #[must_use]
    pub fn extern_result_is_provably_not_a_foreign_handle(&self, name: &str) -> bool {
        self.audited_domestic_return_names.contains(name)
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

/// Build the extern contract table over a module's `extern "C"` declarations:
/// scalar-return externs → Fresh; every heap-returning extern is omitted (→
/// `{OPAQUE}` on lookup). Zero marker-backed owned-RETURN rows — the
/// trusted-root precursor is required for those (S4b).
///
/// # The audited ARGUMENT contract — the declared-borrow class
///
/// `borrowing_arg_names` is the ARGUMENT-side sibling of round seven's declared
/// RELEASE class, and it is admitted the same way: from a row source the
/// program already declares and a validator already checks, never from a
/// permissive fallback.
///
/// The rows come from [`crate::ffi_contracts::extern_ownership_contract`],
/// which `hew-types/build.rs` projects from the `[[ownership.contracts]]` table
/// of `scripts/jit-symbol-classification.toml` — the single authority for
/// per-parameter consume/borrow facts, validated out of band by
/// `scripts/verify-ffi-symbols.py`. `ExternParamOwnership::Borrow` is defined
/// as "the callee reads (or copies from) the parameter; **the caller keeps the
/// drop obligation**", which is exactly the fact this set is read for.
///
/// Two admission clauses, both required, and NEITHER of them the declaring
/// module's identity:
///
/// 1. [`hew_types::jit_symbols::is_classified_hew_ffi_symbol`] claims the name,
///    and the declaration's arity equals the audited contract's parameter
///    count. This is the positive symbol identity the RESULT axis already
///    requires (Clause A2/A3 below): the audited row is a fact about THIS
///    compiler's own runtime symbol, keyed by the symbol, so a name it does not
///    claim cannot claim the row and a declaration that disagrees with the
///    audited signature is not a declaration of the audited callee;
/// 2. the audited contract exists AND every parameter position is `Borrow`. A
///    `Consume` or `Retain` position anywhere in the signature, or an
///    [`crate::ffi_contracts::ExternOwnershipFact::Absent`] verdict, refuses the
///    name outright.
///
/// Clause 1 used to read [`hew_hir::ExternProvenance::is_stdlib`] instead. That
/// made the answer depend on HOW the declaring file was handed to the compiler:
/// `std/net/net.hew` reached through `import std::net` carries
/// `Module("std.net")` and was admitted, while the SAME file as a root
/// compilation unit carries `Root` and was not — so `connect_timeout` lost
/// every one of its thirteen `host` releases on the root path, including the
/// ordinary `return`. Round ten removed exactly this dependency from the RESULT
/// axis; it was still live here, on the axis the same table had read first.
/// Symbol identity is a property of the callee, which is what the question is
/// about; the declaring module's spelling is a property of the invocation.
///
/// Clause 2 is deliberately whole-signature rather than per-position. The
/// consumer ([`crate::lower::temp_drop::string_call_borrows`]) asks one boolean
/// about the CALL, so a per-position answer could not be expressed there
/// faithfully; requiring every position to borrow makes the boolean sound for
/// every argument of every admitted call. `hew_string_drop`
/// (`params = [Consume]`) is refused by this clause, so the universal release
/// can never be read as borrowing its argument.
///
/// # What an UNAUDITED extern argument means, and why
///
/// An extern with no audited row answers `false` here, and the consumer reads
/// `false` as "the callee may have taken the handle" — so the caller withholds
/// its release. The failure direction is a LEAK: a host that really borrows,
/// but carries no row, keeps a buffer alive forever.
///
/// The alternative reading is available and tempting. Hew spells transfer as
/// `consume`, and [`hew_hir::HirExternFn::param_consume`] carries that modifier
/// per parameter, so "a non-`consume` extern parameter is a declared borrow"
/// could be read straight off the declaration with no audit at all. It is
/// rejected here for one reason: its failure direction is a DOUBLE RELEASE.
/// Nothing in the front end requires an author to have thought about ownership
/// at a heap-typed extern parameter — `ResourceBoundaryParamMustConsume` fires
/// only for an affine `#[resource]`/`#[linear]` parameter, so an omitted
/// `consume` on a `string` is indistinguishable from an omitted THOUGHT. Under
/// that reading every such declaration silently licenses a caller-side free of
/// a pointer the host may already have freed, and heap corruption is neither
/// diagnosable nor recoverable, whereas the leak this file chooses is bounded,
/// observable, and — as of this round — measured file by file by the
/// release-count differential over the shipped corpora.
///
/// Making the declaration load-bearing is the right end state and is a
/// front-end change, not a MIR one: the checker would have to REQUIRE an
/// explicit disposition on every heap-typed extern parameter (a breaking
/// surface rule) before an absent `consume` could be read as a considered
/// borrow. Until it does, "borrow only where audited" is the reading whose
/// mistakes leak instead of corrupting.
///
/// # The RESULT axis — the two clauses that answer "not proof of foreignness"
///
/// `audited_domestic_return_names` admits a declared extern under EITHER of two
/// independent clauses. Both are fail-closed: refusing a name restores the
/// previous verdict (proven foreign ⇒ a release suppressed ⇒ a leak), never a
/// double release. And neither licenses a mint — see the field docs.
///
/// **Clause A — the audited owned transfer.** All three parts required:
///
/// A1. the `[[ownership.contracts]]` row exists, its result is `Fresh` or
///     `Retained`, and it names a non-empty release symbol. `Borrowed`
///     (`hew_vec_get_owned`, an alias invalidated by mutation) is REFUSED:
///     that result genuinely is not the caller's to release, and suppressing
///     its releases is correct behaviour this set must not disturb;
/// A2. [`hew_types::jit_symbols::is_classified_hew_ffi_symbol`] claims the
///     name. The audited row is a fact about this compiler's own runtime
///     symbol, so admitting it requires the same positive symbol identity
///     [`crate::model::classify_extern_string_ownership`] already requires
///     before it will call a root-provenance `-> string` return header-aware.
///     That classifier decides the return's memory LAYOUT from this key today,
///     a strictly more dangerous decision, so the key is trusted with nothing
///     new here;
/// A3. the declaration's arity equals the audited contract's parameter count. A
///     declaration that disagrees with the audited signature is not a
///     declaration of the audited callee, so the row says nothing about it.
///
/// **Clause B — the pointer-free result.** The declared return type provably
/// contains no pointer: a scalar/unit leaf, or a record declared in THIS module
/// all of whose fields are (transitively) such leaves. A value with no pointer
/// in it cannot be a handle, foreign or otherwise, so no audit is needed and
/// none is required. `hew_cron_next_hew -> CronNextResult { status: i32;
/// timestamp: i64 }` is admitted here; it is the shape that refused
/// `std/time/cron/cron.hew`.
///
/// Clause B deliberately does NOT consult the layout registry and deliberately
/// does not admit an `#[opaque]` handle. `ty_owns_heap_mir` reads an absent or
/// partial registry as non-heap — the permissive direction, the exact
/// `Default`-shaped fail-open round 4 hardened against — and an `#[opaque]`
/// declaration is a pointer-width slot that IS the foreign-handle shape this
/// whole authority exists to refuse. The narrow structural predicate answers
/// `false` for anything unresolved.
///
/// # Clause C — the measured transfer, and the only MINT for a pointer
///
/// `fresh_return_names` additionally admits a name under
/// [`extern_result_is_measured_transfer`]. That is a strictly stronger bar than
/// Clauses A and B, because it is the only admission here that hands a
/// pointer-bearing result to the drop plan as a fresh sole owner. It requires
/// Clause A, a declared return type whose type-directed drop plan discharges
/// through exactly the audited release symbol, and — the load-bearing part — an
/// audited `result-retention = "transferred"`, which is recorded only where an
/// executable oracle measured the runtime's actual retention behaviour. See
/// that function for the clause-by-clause argument.
///
/// Clause C deliberately widens ONLY the name-keyed `fresh_return_names` beyond
/// the `AliasBits::EMPTY` row it already writes: an extern call site's
/// `ResolvedRef::Item` carries a placeholder id, so the name-keyed set is the
/// one every reader of this authority actually consults for an extern.
#[must_use]
pub fn build_extern_contract_table(module: &hew_hir::HirModule) -> ExternContractTable {
    let mut rows: HashMap<hew_hir::ItemId, ReturnProvenance> = HashMap::new();
    let mut names: HashSet<String> = HashSet::new();
    let mut fresh_return_names: HashSet<String> = HashSet::new();
    let mut decl_ids: HashSet<hew_hir::ItemId> = HashSet::new();
    let mut foreign_decl_ids: HashSet<hew_hir::ItemId> = HashSet::new();
    let mut foreign_names: HashSet<String> = HashSet::new();
    let mut borrowing_arg_names: HashSet<String> = HashSet::new();
    let mut audited_domestic_return_names: HashSet<String> = HashSet::new();
    let pointer_free_records = PointerFreeRecords::from_module(module);
    let type_decls: HashMap<&str, &hew_hir::HirTypeDecl> = module
        .items
        .iter()
        .filter_map(|item| match item {
            hew_hir::HirItem::TypeDecl(decl) => Some((decl.declaration.full_path(), decl)),
            _ => None,
        })
        .collect();
    for item in &module.items {
        if let hew_hir::HirItem::ExternFn(ef) = item {
            names.insert(ef.name.clone());
            decl_ids.insert(ef.id);
            if !ef.provenance.is_stdlib() {
                foreign_decl_ids.insert(ef.id);
                foreign_names.insert(ef.name.clone());
            }
            if extern_all_params_audited_borrow(&ef.name, ef.param_tys.len()) {
                borrowing_arg_names.insert(ef.name.clone());
            }
            if extern_result_is_audited_owned_transfer(&ef.name, ef.param_tys.len())
                || pointer_free_records.ty_is_pointer_free(&ef.return_ty)
            {
                audited_domestic_return_names.insert(ef.name.clone());
            }
            // The MINT-side admission, and the only lines here the strict
            // policy reads. `ty_is_scalar_non_heap` is the degenerate case of
            // `ty_is_pointer_free`: an `i64` return is admitted because it
            // cannot carry a handle, and a record whose fields are
            // transitively such leaves cannot either. Withholding the record
            // case is not the conservative choice — a pointer-free value has
            // nothing to release, so admitting it can never emit a release for
            // a handle this program does not own, while REFUSING it taints
            // every domestic value the callee derives from it. That is the
            // `hew_cron_next_hew -> CronNextResult` leak: the POD is read as a
            // foreign handle, the taint travels out through
            // `cron_error_from_result`, and the ordinary `+1` string the error
            // carrier holds gets no owner at all.
            //
            // The third disjunct is the only one that mints for a value that
            // DOES carry a pointer, and it mints solely on a measured retention
            // answer plus the exact release that the declared type will run —
            // see `extern_result_is_measured_transfer`.
            if ty_is_scalar_non_heap(&ef.return_ty)
                || pointer_free_records.ty_is_pointer_free(&ef.return_ty)
                || extern_result_is_measured_transfer(&ef.name, ef, &type_decls, module)
            {
                rows.insert(ef.id, AliasBits::EMPTY);
                fresh_return_names.insert(ef.name.clone());
            }
        }
    }
    ExternContractTable {
        rows,
        names,
        fresh_return_names,
        foreign_decl_ids,
        foreign_names,
        borrowing_arg_names,
        audited_domestic_return_names,
        decl_ids,
    }
}

/// Clause A of the RESULT-axis admission — see [`build_extern_contract_table`]
/// for why each part is required.
fn extern_result_is_audited_owned_transfer(symbol: &str, declared_arity: usize) -> bool {
    if !hew_types::jit_symbols::is_classified_hew_ffi_symbol(symbol) {
        return false;
    }
    crate::ffi_contracts::extern_ownership_contract(symbol)
        .contract()
        .is_some_and(|contract| {
            matches!(
                contract.result,
                crate::ffi_contracts::ExternResultOwnership::Fresh
                    | crate::ffi_contracts::ExternResultOwnership::Retained
            ) && !contract.release_symbol.is_empty()
                && contract.params.len() == declared_arity
        })
}

/// Clause C of the RESULT-axis admission, and the ONLY clause that mints a
/// caller-side release for a POINTER-BEARING extern result.
///
/// Clauses A and B answer "this result is not proof of foreignness", which only
/// ever suppresses a taint. Minting is the other direction and needs the harder
/// fact: that the buffer handed back is the caller's to release, and that
/// releasing it exactly once balances the runtime. Getting that wrong upward is
/// a reachable double free, so every conjunct below is load-bearing.
///
/// C1. Clause A holds — an audited `[[ownership.contracts]]` row exists for a
///     symbol this compiler's own runtime claims, its result is owned, it names
///     a release, and the declaration's arity matches the audited signature. A
///     declaration that disagrees with the audited signature is not a
///     declaration of the audited callee.
///
/// C2. the row's RETENTION axis carries a measured positive answer. An
///     exclusive `transferred` result has no surviving alias. A retained
///     `shared-refcount` result may have the same address as another live
///     owner, but one caller release is independently balanced by the callee's
///     retain. An opaque `resource-transfer` instead proves that the returned
///     token is the only caller-controlled authority for the named close.
///     This is a strictly different question from C1: `fresh` says the
///     allocation is new, not that the callee stopped referring to it. A
///     callee that allocates AND keeps an uncounted pointer — to free it later,
///     to overwrite it on the next call, or to hand the same address back again
///     — yields a value the caller must not release. Both members of
///     the `*_last_error` family are `hew_*` C-string returns and only one of
///     them transfers (hew-lang/hew#2828): `hew_process_last_error` copies its
///     thread-local into a fresh header-aware allocation and keeps nothing,
///     while `hew_last_error` hands back the interior of the `CString` the
///     runtime stores. Absence of the axis is the fail-closed reading and is
///     the state of nearly every row, so this clause admits only what an
///     executable oracle has actually measured.
///
/// C3. the audited release symbol is EXACTLY the release the compiler's own
///     type-directed drop plan emits for the declared return type, at `Shallow`
///     depth. This is what ties the AUDITED release to the release that will
///     actually run. The mint does not emit the row's release symbol; it marks
///     the value a fresh sole owner and lets the drop plan derive the release
///     from the declared type. Without this conjunct a row naming
///     `hew_json_free` would license a `hew_string_drop` against an allocation
///     that release never balances. See [`declared_return_release`] for the
///     derivation and for why a type whose discharge is not a single release is
///     refused.
///
/// Fail-closed in every direction: any conjunct answering `false` restores the
/// pre-existing verdict, which is `{OPAQUE}` ⇒ no mint ⇒ a leak. A leak is
/// bounded and is measured file by file by the release-count differential; a
/// double free is neither.
fn extern_result_is_measured_transfer(
    symbol: &str,
    decl: &hew_hir::HirExternFn,
    decls: &HashMap<&str, &hew_hir::HirTypeDecl>,
    module: &hew_hir::HirModule,
) -> bool {
    if !extern_result_is_audited_owned_transfer(symbol, decl.param_tys.len()) {
        return false;
    }
    crate::ffi_contracts::extern_ownership_contract(symbol)
        .contract()
        .is_some_and(|contract| {
            match declared_return_release(&decl.return_ty, decls, 0) {
                // Heap values whose compiler-derived release is a direct C ABI
                // symbol (string, bytes, and a structurally homogeneous record)
                // admit either an exclusive handoff or an independently
                // balanced refcount share.
                ReturnRelease::One(planned) => {
                    contract
                        .result_retention
                        .authorizes_direct_allocation_release()
                        && contract.discharge_depth
                            == crate::ffi_contracts::ReleaseDischargeDepth::Shallow
                        && contract.release_symbol == planned
                }
                // An opaque `#[resource]` discharges through its inherent
                // `<Type>::close` function rather than a raw C symbol.  It is
                // still an exactly-one release, but only when that particular
                // close body directly invokes the exact C close named by the
                // producer row.  This is the resource analogue of `One`: a
                // fresh `Connection` cannot be minted merely because a source
                // marker says resource, nor can a similarly-spelled producer
                // select an unrelated close ritual.
                ReturnRelease::Unresolved => {
                    contract.result_retention
                        == crate::ffi_contracts::ExternResultRetention::ResourceTransfer
                        && resource_opaque_return_releases_through(
                            &decl.return_ty,
                            module,
                            contract.release_symbol,
                            contract.discharge_depth,
                        )
                }
                ReturnRelease::Nothing => false,
            }
        })
}

/// Whether an opaque resource return is discharged by exactly `release_symbol`.
///
/// The resource drop plan and this mint admission read the same checker-admitted
/// lifecycle. Exact release and discharge depth must both agree; `deep` is
/// valid for a single opaque close whose runtime disposer recursively releases
/// its owned tree.
fn resource_opaque_return_releases_through(
    ty: &ResolvedTy,
    module: &hew_hir::HirModule,
    release_symbol: &str,
    discharge_depth: crate::ffi_contracts::ReleaseDischargeDepth,
) -> bool {
    let ResolvedTy::Named {
        name,
        args,
        is_opaque,
        ..
    } = ty
    else {
        return false;
    };
    if !*is_opaque || !args.is_empty() {
        return false;
    }
    module
        .type_classes
        .opaque_resource_lifecycle_for_type_name(name)
        .is_some_and(|lifecycle| {
            discharge_depth != crate::ffi_contracts::ReleaseDischargeDepth::None
                && lifecycle.release_symbol == release_symbol
                && lifecycle.discharge_depth == discharge_depth
        })
}

/// What the compiler's type-directed drop plan will emit for a declared type.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ReturnRelease {
    /// Nothing is released: the value owns no heap.
    Nothing,
    /// The whole value discharges through exactly this one release symbol.
    One(&'static str),
    /// The type is unresolvable here, or its discharge is not a single release
    /// (an `#[opaque]` handle, a generic instantiation, a record mixing two
    /// release symbols, a shape this function does not model). Never admitted.
    Unresolved,
}

/// The single release symbol the type-directed drop plan emits for `ty`,
/// resolved from THIS module's own type declarations.
///
/// Same discipline as [`PointerFreeRecords`]: structural, module-local, and
/// `Unresolved` for anything it cannot resolve. It never consults a layout
/// registry, because an absent or partial registry reads a composite as
/// non-heap, which is the permissive direction.
///
/// `Nothing` is not a licence either — a value with no heap in it is Clause B's
/// business, and Clause C requires `One`, so a contract can only ever be
/// honoured against a release the plan will really emit. A record mixing two
/// release symbols answers `Unresolved`: the contract names ONE release symbol,
/// so it cannot be the balancing release for a value that needs two, and the
/// audit is then not describing what the caller would do.
fn declared_return_release(
    ty: &ResolvedTy,
    decls: &HashMap<&str, &hew_hir::HirTypeDecl>,
    depth: usize,
) -> ReturnRelease {
    /// Bounds the record walk. A record cannot contain itself by value, but the
    /// walk must terminate on a malformed or mutually recursive declaration
    /// without trusting that.
    const MAX_RECORD_DEPTH: usize = 8;

    if ty_is_scalar_non_heap(ty) {
        return ReturnRelease::Nothing;
    }
    match ty {
        ResolvedTy::String => ReturnRelease::One(STRING_RELEASE_SYMBOL),
        ResolvedTy::Bytes => ReturnRelease::One(BYTES_RELEASE_SYMBOL),
        ResolvedTy::Named {
            builtin: Some(hew_types::BuiltinType::Sink),
            ..
        } => ReturnRelease::One("hew_sink_close"),
        ResolvedTy::Named {
            builtin: Some(hew_types::BuiltinType::Stream),
            ..
        } => ReturnRelease::One("hew_stream_close"),
        ResolvedTy::Named {
            name,
            args,
            is_opaque,
            ..
        } => {
            if *is_opaque || !args.is_empty() || depth >= MAX_RECORD_DEPTH {
                return ReturnRelease::Unresolved;
            }
            let Some(decl) = decls.get(name.as_str()) else {
                return ReturnRelease::Unresolved;
            };
            if decl.is_opaque || decl.fields.is_empty() {
                return ReturnRelease::Unresolved;
            }
            let mut planned = ReturnRelease::Nothing;
            for field in &decl.fields {
                match declared_return_release(&field.ty, decls, depth + 1) {
                    ReturnRelease::Nothing => {}
                    ReturnRelease::Unresolved => return ReturnRelease::Unresolved,
                    ReturnRelease::One(symbol) => match planned {
                        ReturnRelease::One(seen) if seen != symbol => {
                            return ReturnRelease::Unresolved;
                        }
                        _ => planned = ReturnRelease::One(symbol),
                    },
                }
            }
            planned
        }
        _ => ReturnRelease::Unresolved,
    }
}

/// The one release the type-directed drop plan emits for a `string`.
const STRING_RELEASE_SYMBOL: &str = "hew_string_drop";

/// The one release the type-directed drop plan emits for `bytes`.
const BYTES_RELEASE_SYMBOL: &str = "hew_bytes_drop";

/// Clause B of the RESULT-axis admission: the module's records that provably
/// contain no pointer, resolved from the module's own type declarations.
///
/// Same discipline as [`DeclaredReleaseTypes`]: structural, module-local, and
/// `false` for anything it cannot resolve. It never consults a layout registry,
/// because an absent or partial registry reads a composite as non-heap and that
/// is the permissive direction.
#[derive(Debug, Default)]
struct PointerFreeRecords {
    names: HashSet<String>,
}

impl PointerFreeRecords {
    fn from_module(module: &hew_hir::HirModule) -> Self {
        let decls: HashMap<&str, &hew_hir::HirTypeDecl> = module
            .items
            .iter()
            .filter_map(|item| match item {
                hew_hir::HirItem::TypeDecl(decl) => Some((decl.declaration.full_path(), decl)),
                _ => None,
            })
            .collect();
        let mut names: HashSet<String> = HashSet::new();
        // Monotone least fixpoint from the empty set: a record is admitted once
        // every field is a scalar leaf or an already-admitted record. Growing
        // only and over a finite item list, so it converges; a cyclic record
        // (which cannot be pointer-free anyway) is never admitted.
        loop {
            let mut changed = false;
            for (identity, decl) in &decls {
                if names.contains(*identity) || decl.is_opaque || decl.fields.is_empty() {
                    continue;
                }
                if decl
                    .fields
                    .iter()
                    .all(|field| field_ty_is_pointer_free(&field.ty, &names))
                {
                    names.insert((*identity).to_string());
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
        Self { names }
    }

    fn ty_is_pointer_free(&self, ty: &ResolvedTy) -> bool {
        field_ty_is_pointer_free(ty, &self.names)
    }
}

/// True for a type that provably contains no pointer: a scalar/unit leaf, or a
/// named non-generic record already admitted to `pointer_free`.
///
/// An `#[opaque]` declaration answers `false` — it is a pointer-width handle
/// slot, and admitting it would exempt the exact shape this authority exists to
/// refuse. So does anything unresolved.
fn field_ty_is_pointer_free(ty: &ResolvedTy, pointer_free: &HashSet<String>) -> bool {
    if ty_is_scalar_non_heap(ty) {
        return true;
    }
    let ResolvedTy::Named {
        name,
        args,
        is_opaque,
        ..
    } = ty
    else {
        return false;
    };
    if *is_opaque || !args.is_empty() {
        return false;
    }
    pointer_free.contains(name.as_str())
}

/// The ARGUMENT-axis admission — see [`build_extern_contract_table`] for why
/// each part is required. Structurally the mirror of
/// [`extern_result_is_audited_owned_transfer`], and deliberately so: the two
/// axes read the same audited rows, so they must key on the same identity.
///
/// A zero-parameter contract vacuously satisfies "every position borrows" and
/// is admitted; it has no heap argument to get wrong, and the consumer's
/// question is only ever asked about a call that passes one.
fn extern_all_params_audited_borrow(symbol: &str, declared_arity: usize) -> bool {
    if !hew_types::jit_symbols::is_classified_hew_ffi_symbol(symbol) {
        return false;
    }
    crate::ffi_contracts::extern_ownership_contract(symbol)
        .contract()
        .is_some_and(|contract| {
            contract.params.len() == declared_arity
                && contract
                    .params
                    .iter()
                    .all(|param| *param == crate::ffi_contracts::ExternParamOwnership::Borrow)
        })
}

// ---------------------------------------------------------------------------
// Module-global preflight context — built once, threaded into every builder
// ---------------------------------------------------------------------------

/// The SINGLE authority for the question "does this call hand back a fresh owner
/// I may mint a caller-side RELEASE obligation over?".
///
/// # Why a type and not a map
///
/// The coarse freshness fixpoint (`compute_fn_returns_fresh_owner`) answers a
/// NARROWER question — may a return value alias a by-value parameter — and it
/// cannot see an extern at all: a declared `extern "C"` fn is body-less, so the
/// coarse walk's `unwrap_or(true)` cross-ABI fallback classifies an un-audited
/// host as a fresh-owner producer, and one Hew frame around it inherits a `true`
/// row. Conjoining the veto at each consumer was tried and failed three times:
/// the defect reappeared one call site at a time, because the coarse `HashMap`
/// stayed reachable and a new consumer could always read it directly.
///
/// So the veto lives in the TYPE. The rows are private, the only analysing
/// constructor is [`FreshOwnerVerdicts::build`] (module-private, called once
/// from [`build_call_scrutinee_provenance`]), and every ownership consumer takes
/// a `&FreshOwnerVerdicts`. A coarse `HashMap<ItemId, bool>` is not
/// type-compatible with any of those signatures, so no consumer — present or
/// future — can obtain a "fresh" answer for a laundering wrapper.
///
/// # The three vetoes it carries
///
/// * **Wrapper laundering (id-keyed).** Every row is a freshness proof
///   CONJOINED with `compute_fn_return_launders_opaque_extern`, a transitive
///   fixpoint over the audited [`ExternContractTable`]. A wrapper, a wrapper of
///   a wrapper, a generic wrapper (analyzed at its origin `ItemId`, which is
///   what a monomorphisation's callee resolves to) and a recursive-looking
///   wrapper all read `false`. The freshness proof itself is the coarse
///   may-alias-a-parameter fixpoint UNIONED with a precise `∅` return summary —
///   see [`FreshOwnerVerdicts::build`] for why the second is needed and why it
///   cannot widen what this veto refuses.
/// * **Direct extern (name-keyed).** An extern call site's `ResolvedRef::Item`
///   carries a PLACEHOLDER id, not the declaration's — an id lookup would both
///   miss the extern and collide with the module-fn summary space. The declared
///   opaque-extern NAMES are therefore carried here too and vetoed first.
/// * **Embedded foreign provenance (structural).**
///   [`FreshOwnerVerdicts::value_is_free_of_opaque_foreign_provenance`] answers
///   the COMPOSITE question the first two cannot: a fresh CONTAINER does not
///   confer ownership of a foreign value embedded in it. See that method for
///   the rule and for what it deliberately does not prove.
///
/// All three vetoes are TYPE-AGNOSTIC: neither the laundering fixpoint nor the
/// extern table filters on `string`, so a record, a tuple, an enum and a `Vec`
/// element are covered identically.
///
/// # No fallback: an absent row is NOT a fresh owner
///
/// [`FreshOwnerVerdicts::item_returns_fresh_owner`] is the ONE reader and it
/// fails CLOSED: `true` only for an id this module analysed AND proved clean.
/// A missing row means "not analysed", and not-analysed never grants a release.
/// The earlier permissive `unwrap_or(true)` sibling is gone: it made an EMPTY
/// authority answer fresh for every id, so any way of obtaining an empty
/// authority — a derived `Default`, a not-yet-threaded field — minted exactly
/// the caller release this type exists to forbid. Costing an absent row a
/// missed move/clone optimisation is the price; the alternative price was a
/// double release.
///
/// The type therefore has NO `Default`. It can only come from
/// [`FreshOwnerVerdicts::build`] (the real conjunction), from the module-private
/// [`FreshOwnerVerdicts::denying_all`] (an explicitly empty authority that
/// grants nothing, needed only to stand up a [`CallScrutineeProvenance`] that
/// has not been built yet), or — under `#[cfg(test)]` only — from
/// `from_parts_for_tests`. It is `pub(crate)`: no consumer outside this crate
/// can name it, hold it, or construct one.
#[derive(Debug, Clone)]
pub(crate) struct FreshOwnerVerdicts {
    /// `ItemId` → (coarse ∨ precise) freshness ∧ ¬laundering. Private: the whole
    /// point.
    rows: HashMap<hew_hir::ItemId, bool>,
    /// Functions whose string return is an independently retained field-load
    /// share. This is deliberately NOT folded into `rows`: a retained share is
    /// releasable once, but still pointer-aliases its source allocation.
    retained_string_projection_returns: HashSet<hew_hir::ItemId>,
    /// Declared `extern "C"` fn names with no audited fresh-owner return.
    opaque_extern_names: HashSet<String>,
    /// The audited extern contract table, carried so
    /// [`Self::value_is_free_of_opaque_foreign_provenance`] can re-run the
    /// module taint transfer at an arbitrary VALUE position.
    extern_table: ExternContractTable,
    /// The `ItemId`s whose bodies this module analysed — the taint fixpoint's
    /// own `analyzed` set, carried for the same reason.
    analyzed: HashSet<hew_hir::ItemId>,
    /// The opaque-extern laundering taint set, carried for the same reason.
    /// This is NOT `rows`-with-`false`: a `false` row also covers a callee that
    /// merely forwards a by-value parameter, which is a value of the ENCLOSING
    /// frame, not a foreign one.
    launders_opaque_extern: HashSet<hew_hir::ItemId>,
    /// The PROVEN-foreign taint set — a subset of `launders_opaque_extern` in
    /// which nothing was injected by an unknown (indirect or unanalysed)
    /// callee. Carried for [`Self::value_carries_proven_foreign_provenance`],
    /// whose consumer removes a release rather than adding one.
    carries_proven_foreign: HashSet<hew_hir::ItemId>,
    /// The types whose release this program DECLARES rather than the compiler
    /// deriving it from their layout — the adoption boundary, carried so both
    /// value-position queries below apply the identical rule the two module
    /// fixpoints were computed under. See [`DeclaredReleaseTypes`].
    declared_release: DeclaredReleaseTypes,
    /// `true` only for an authority produced by [`Self::build`] from a real
    /// module analysis. [`Self::denying_all`] sets it `false`, and every query
    /// that could otherwise answer PERMISSIVELY denies outright.
    ///
    /// The row-keyed queries fail closed on their own (an empty row set answers
    /// `false` for every id), but the composite query cannot: its policy
    /// classifies a body-less non-extern callee as the compiler's own
    /// owned-return item, so an authority with no extern table would report an
    /// UNANALYSED expression free of foreign provenance. That is precisely the
    /// `Default`-shaped fail-open this type was hardened against in round four,
    /// so the flag closes it for every query at once.
    from_module_analysis: bool,
}

impl FreshOwnerVerdicts {
    /// The ONLY analysing constructor. Module-private so the conjunction cannot
    /// be skipped by a caller that happens to hold a coarse map.
    ///
    /// The row set is the UNION of the coarse fixpoint's keys, the precise
    /// summary's keys and the taint set's: a laundering id that the coarse
    /// fixpoint somehow never keyed still lands as an explicit `false` rather
    /// than as an absent row.
    ///
    /// # Why the precise summary is read alongside the coarse one
    ///
    /// The two fixpoints answer DIFFERENT questions, and the freshness warrant
    /// wants the better-informed answer to its own.
    ///
    /// The coarse walk asks "may this return alias a by-value heap PARAMETER?"
    /// and it answers with a blanket `AliasBits::OPAQUE` at every leaf it does
    /// not model — including a scalar literal and a bare `var` binding of type
    /// `f64`. A wholly domestic helper such as `string.parse_valid_float_literal`,
    /// whose last expression is a bare scalar binding, therefore reads
    /// "may alias a borrow" for a value that owns no heap at all, and every
    /// caller up the chain inherits that. `string.to_float`'s
    /// `Result<f64, string>` scrutinee lost its release that way: the value is
    /// domestic, the taint fixpoint is silent about it, and nothing foreign is
    /// anywhere near it — the question was simply answered with too little
    /// information.
    ///
    /// The precise fixpoint answers the same question with more: it
    /// short-circuits scalar non-heap types to `∅`, models string concatenation,
    /// reads audited method contracts, and tracks local bindings. Where it
    /// differs from the coarse walk it is STRICTER in exactly the direction that
    /// matters — a body-less or unresolvable callee is `Opaque` rather than the
    /// coarse walk's permissive "fresh", and EVERY declared extern is `Opaque`
    /// by name — so a precise `∅` cannot re-admit the extern-wrapper shapes the
    /// coarse walk over-admits and the laundering veto removes.
    ///
    /// So a precise `∅` is a positive proof that every return path produces a
    /// value that aliases neither a parameter nor any opaque origin, which is
    /// strictly more than the coarse row claims. It is unioned in, and the
    /// laundering veto still applies to the result: widening the freshness proof
    /// must never widen what the taint fixpoint refuses.
    ///
    /// The coarse walk itself is deliberately left alone. Its own consumers
    /// (the funcupdate materialised-owner gate and the reassign may-alias gate)
    /// are pinned byte-identical by `coarse_verdict_differential`; this reads
    /// past it rather than changing it.
    fn build(
        coarse_fresh_returns: &HashMap<hew_hir::ItemId, bool>,
        precise_returns: &HashMap<hew_hir::ItemId, ReturnProvenance>,
        retained_string_projection_returns: &HashSet<hew_hir::ItemId>,
        launders_opaque_extern: &HashSet<hew_hir::ItemId>,
        carries_proven_foreign: &HashSet<hew_hir::ItemId>,
        extern_table: &ExternContractTable,
        declared_release: &DeclaredReleaseTypes,
    ) -> Self {
        let proven_fresh = |id: hew_hir::ItemId| {
            coarse_fresh_returns.get(&id).copied().unwrap_or(false)
                || precise_returns.get(&id).is_some_and(|bits| bits.is_fresh())
        };
        let mut rows: HashMap<hew_hir::ItemId, bool> = coarse_fresh_returns
            .keys()
            .chain(precise_returns.keys())
            .map(|&id| {
                (
                    id,
                    proven_fresh(id) && !launders_opaque_extern.contains(&id),
                )
            })
            .collect();
        for &id in launders_opaque_extern {
            rows.insert(id, false);
        }
        let opaque_extern_names = extern_table
            .names
            .iter()
            .filter(|name| !extern_table.extern_return_is_audited_fresh_owner(name))
            .cloned()
            .collect();
        let analyzed = rows.keys().copied().collect();
        Self {
            rows,
            retained_string_projection_returns: retained_string_projection_returns.clone(),
            opaque_extern_names,
            extern_table: extern_table.clone(),
            analyzed,
            launders_opaque_extern: launders_opaque_extern.clone(),
            carries_proven_foreign: carries_proven_foreign.clone(),
            declared_release: declared_release.clone(),
            from_module_analysis: true,
        }
    }

    /// The explicitly EMPTY authority: no analysed rows, no declared externs.
    ///
    /// It grants NOTHING — every [`Self::item_returns_fresh_owner`] query
    /// answers `false`, because an absent row is not a freshness proof, and
    /// [`Self::value_is_free_of_opaque_foreign_provenance`] answers `false`
    /// because `from_module_analysis` is unset. Named, rather than a `Default`
    /// derive, so that "I have not been given the real authority yet" is written
    /// out at the one place that means it: the [`CallScrutineeProvenance`]
    /// `Default` that backs a lowering builder before the module context is
    /// threaded in. Module-private: no other module can mint one.
    fn denying_all() -> Self {
        Self {
            rows: HashMap::new(),
            retained_string_projection_returns: HashSet::new(),
            opaque_extern_names: HashSet::new(),
            extern_table: ExternContractTable::default(),
            analyzed: HashSet::new(),
            launders_opaque_extern: HashSet::new(),
            carries_proven_foreign: HashSet::new(),
            declared_release: DeclaredReleaseTypes::default(),
            from_module_analysis: false,
        }
    }

    /// The table-aware freshness verdict for a resolved item: `true` ONLY for an
    /// id whose body this module analysed, proved fresh, and proved free of
    /// opaque-extern laundering.
    ///
    /// FAIL CLOSED. An absent row means "not analysed" — a declared extern's
    /// placeholder call-site id, a cross-module item, an aggregate constructor,
    /// a compiler-minted runtime primitive — and not-analysed never licenses a
    /// caller-side release. The worst case is a missed drop (a leak); the
    /// permissive alternative's worst case was a double release.
    #[must_use]
    pub(crate) fn item_returns_fresh_owner(&self, id: hew_hir::ItemId) -> bool {
        self.rows.get(&id) == Some(&true)
    }

    /// Whether `id` returns a string field projection whose MIR load retains an
    /// independent `+1` share. This licenses exactly one balancing string
    /// release; unlike [`Self::item_returns_fresh_owner`], it says nothing about
    /// pointer non-aliasing and must not be used by composite/destructive-owner
    /// consumers.
    #[must_use]
    pub(crate) fn item_returns_retained_string_owner(&self, id: hew_hir::ItemId) -> bool {
        self.from_module_analysis
            && self.retained_string_projection_returns.contains(&id)
            && !self.launders_opaque_extern.contains(&id)
    }

    /// True when `symbol` names a declared `extern "C"` fn with no audited
    /// fresh-owner return contract — an ownership-OPAQUE callee whose result
    /// may be an interior, static or retained host pointer.
    #[must_use]
    pub(crate) fn symbol_is_ownership_opaque_extern(&self, symbol: &str) -> bool {
        self.opaque_extern_names.contains(symbol)
    }

    /// The adoption boundary this authority was built with, so the per-function
    /// ledger query ([`value_reads_a_proven_foreign_binding`]) walks under the
    /// SAME rule as the two module fixpoints and the two value queries. There is
    /// one table and one place it comes from.
    #[must_use]
    pub(crate) fn declared_release_types(&self) -> &DeclaredReleaseTypes {
        &self.declared_release
    }

    /// The COMPOSITE provenance query: `true` only when `expr` is PROVEN to
    /// evaluate to — and to EMBED — no value that crossed an ownership-opaque
    /// foreign producer.
    ///
    /// # Why the container's own freshness is not the question
    ///
    /// Every composite release this compiler emits is RECURSIVE: releasing a
    /// record frees its fields, releasing a tuple frees its elements, releasing
    /// an enum frees its payload. So a mint over a container is a mint over the
    /// whole tree beneath it, and there is no drop plan that frees the
    /// container's spine while sparing one field — the container's release
    /// symbol is generated from its layout, not from a per-field provenance
    /// map. `Outer { inner: unsafe { host_record() } }` is genuinely a FRESH
    /// outer allocation, and minting it caller-owned is still a release of the
    /// host's handle.
    ///
    /// The rule this query enforces is therefore:
    ///
    /// > **Freshness of a container is not ownership of its contents.** A
    /// > caller-side drop may be minted over a composite only when the
    /// > composite's own allocation is fresh AND every value embedded in it is
    /// > free of ownership-opaque foreign provenance. A container with any
    /// > opaque embed is not minted at all.
    ///
    /// # It is the module taint transfer, evaluated at a value position
    ///
    /// The walk is [`return_alias_bits`] under the SAME
    /// [`OpaqueExternTaintPolicy`] that [`compute_fn_return_launders_opaque_extern`]
    /// runs over each function's RETURN expressions — the same audited
    /// [`ExternContractTable`], the same analysed set, the same taint set. No
    /// new trust is extended and no second policy can drift from the first: the
    /// only change is the position it is asked about. That makes the answer
    /// transitive and TYPE-AGNOSTIC exactly as the row-keyed veto is: a record
    /// field, a tuple element, an enum payload and a nested container all reach
    /// the identical structural arms, and a wrapper, a wrapper of a wrapper and
    /// a generic wrapper are all `false` through their taint rows.
    ///
    /// # What it does NOT prove
    ///
    /// The policy's non-call leaf contributes nothing, so a foreign value that
    /// reaches the container through a `let` binder
    /// (`let h = unsafe { host_record() }; Outer { inner: h }`) is not seen
    /// here. That is not this query's hole to close: the binder ITSELF is minted
    /// caller-owned at the `let`, which releases the foreign handle with no
    /// container involved at all. The `let`-binder construct is unguarded and is
    /// reported as such; closing it is a separate decision, because a root
    /// `extern -> string` return is ADOPTED by codegen and must keep its
    /// release.
    ///
    /// FAIL CLOSED on an authority that was never built from a module analysis.
    #[must_use]
    pub(crate) fn value_is_free_of_opaque_foreign_provenance(&self, expr: &HirExpr) -> bool {
        if !self.from_module_analysis {
            return false;
        }
        let policy = OpaqueExternTaintPolicy {
            extern_table: &self.extern_table,
            analyzed: &self.analyzed,
            tainted: &self.launders_opaque_extern,
            declared_release: &self.declared_release,
        };
        !return_alias_bits(expr, &policy).contains(AliasBits::OPAQUE)
    }

    /// The DROP-SUPPRESSION query: `true` only when this value PROVABLY carries
    /// a handle that came out of a declared, non-audited extern.
    ///
    /// # Why this is not a permissive second opinion
    ///
    /// [`Self::value_is_free_of_opaque_foreign_provenance`] guards the sites
    /// that MINT a release, so it must deny on doubt. This one guards the one
    /// site that REMOVES a release the compiler would otherwise emit — the
    /// `let` binder's scope-exit owner — so it must require proof, or every
    /// binding whose initializer reaches an indirect callee would silently stop
    /// being dropped. The two directions are not symmetric: minting on doubt is
    /// a double release, suppressing on doubt is a leak in code that never
    /// touches an extern.
    ///
    /// It can never license a release. Its only power is to take one away, and
    /// only the audited [`ExternContractTable`] can trigger it.
    ///
    /// # What it deliberately does not cover
    ///
    /// A root `extern "C" -> string` is ADOPTED by codegen (the foreign C
    /// string is copied into a refcounted Hew buffer at the call edge and the
    /// raw pointer is `free`d), so a `string` binding holds a value this program
    /// really does own and its release must survive. The caller carves that
    /// class out; this query does not, because adoption is a property of the
    /// ABI seam, not of the value's history.
    ///
    /// FAIL CLOSED on an authority that was never built from a module analysis —
    /// which here means answering `false`, i.e. changing nothing.
    #[must_use]
    pub(crate) fn value_carries_proven_foreign_provenance(&self, expr: &HirExpr) -> bool {
        if !self.from_module_analysis {
            return false;
        }
        let policy = ProvenForeignPolicy {
            extern_table: &self.extern_table,
            analyzed: &self.analyzed,
            tainted: &self.carries_proven_foreign,
            declared_release: &self.declared_release,
        };
        return_alias_bits(expr, &policy).contains(AliasBits::OPAQUE)
    }

    /// Test-only assembly of an authority from explicit parts, so a unit test
    /// can pin a consumer's behaviour against a hand-built row set without
    /// standing up a whole module. `#[cfg(test)]` keeps the production build's
    /// single-constructor invariant intact.
    ///
    /// It counts as an analysis (`from_module_analysis: true`) — the rows were
    /// seeded deliberately. The composite query it yields therefore reads an
    /// EMPTY extern table and taint set, which is only meaningful for a test
    /// that seeds no foreign producer; the composite rule's own tests derive
    /// their authority from real source instead.
    #[cfg(test)]
    #[must_use]
    pub(crate) fn from_parts_for_tests(
        rows: HashMap<hew_hir::ItemId, bool>,
        opaque_extern_names: HashSet<String>,
    ) -> Self {
        let analyzed = rows.keys().copied().collect();
        Self {
            rows,
            retained_string_projection_returns: HashSet::new(),
            opaque_extern_names,
            extern_table: ExternContractTable::default(),
            analyzed,
            launders_opaque_extern: HashSet::new(),
            carries_proven_foreign: HashSet::new(),
            declared_release: DeclaredReleaseTypes::default(),
            from_module_analysis: true,
        }
    }
}

/// The module-global call-scrutinee return-provenance context the #2648 preflight
/// admission classifier consults at every scrutinee consumer.
///
/// Built ONCE per module (beside the coarse `compute_fn_returns_fresh_owner`
/// summary): the precise three-state provenance summary over every module fn, the
/// set of declared extern `ItemId`s, and the audited owned-return extern table.
///
/// `Default` (empty) fails SAFE: an empty summary classifies every module-fn
/// callee as an unknown item, which the authority
/// ([`FreshOwnerVerdicts::denying_all`]) then declines — `NotApplicable`, no
/// mint — never a wrongly-Fresh admit and never a spurious reject. The
/// live pipeline always threads the fully-built context; the empty default only
/// backs `Builder::default()` in unit tests that do not exercise a forwarder
/// scrutinee. It is hand-written rather than derived because the authority has
/// no `Default` of its own — the empty case must be spelled out, not inferred.
#[derive(Debug, Clone)]
pub(crate) struct CallScrutineeProvenance {
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
    /// The TABLE-AWARE freshness authority: the single object every ownership
    /// consumer asks "does this call produce a fresh owner I may drop".
    ///
    /// It is the CONJUNCTION of facts that no single existing summary carries:
    ///
    /// * a freshness proof — the coarse fixpoint
    ///   (`compute_fn_returns_fresh_owner`), which answers the narrower
    ///   may-alias-a-by-value-parameter question, unioned with a precise `∅`
    ///   return summary for the domestic shapes the coarse walk's blanket
    ///   opaque leaf cannot see through;
    /// * the veto of [`compute_fn_return_launders_opaque_extern`], because the
    ///   coarse proof is built before and independently of the extern contract
    ///   table and classifies EVERY body-less resolved item — a declared extern
    ///   included — as fresh, so a Hew wrapper around an extern inherits a
    ///   `true` row there; and
    /// * the name-keyed direct-extern veto, because an extern call site's
    ///   resolved id is a placeholder that no id lookup can catch.
    ///
    /// Empty default grants NOTHING: with no analysed rows every freshness
    /// query fails closed, so a builder that has not been handed the module
    /// authority cannot mint a caller-side release.
    ///
    /// [`compute_fn_return_launders_opaque_extern`]: crate::return_provenance::compute_fn_return_launders_opaque_extern
    pub fresh_owner_verdicts: FreshOwnerVerdicts,
    /// User functions whose return is safe to treat as one independently
    /// releasable `string` carrier even when it aliases an input buffer.
    ///
    /// This is deliberately separate from `fresh_owner_verdicts`: a function
    /// such as `fn passthru(s: string) -> string { s }` is NOT pointer-fresh,
    /// but lowering retains the selected parameter before writing the return
    /// slot, so the caller receives a real `+1` share. String field projections
    /// have the same postcondition because codegen retains every string field
    /// load. The precise return-provenance fixpoint proves that every path is
    /// either fresh or parameter-derived; any `OPAQUE` path, unanalysed item,
    /// indirect callee, or ownership-opaque extern remains denied.
    pub(crate) owned_string_return_carriers: HashSet<hew_hir::ItemId>,
    /// Emitted MIR symbols for [`Self::owned_string_return_carriers`].
    ///
    /// MIR `Terminator::Call` has already erased the HIR `ItemId`, so the
    /// string-temp/drop derivation must query this emitted-symbol projection
    /// instead of guessing that every non-runtime callee returns a `+1`.
    /// Monomorphisations are keyed by their origin verdict and recorded under
    /// the concrete mangled symbol that reaches MIR.
    pub(crate) owned_string_return_carrier_symbols: HashSet<String>,
    /// Per-function `(variant, field)` return positions whose payload is a
    /// proven fresh owner even though another `Result`/enum arm may not be.
    /// This deliberately does NOT make the enclosing enum fresh: a match may
    /// mint only the selected payload binder, never a shell owner.
    pub(crate) fresh_variant_payloads: HashMap<hew_hir::ItemId, HashSet<(u32, u32)>>,
    /// Emitted-symbol projection of [`Self::fresh_variant_payloads`]. Imported
    /// calls carry a declaration identity distinct from the analysed origin,
    /// so the symbol preserves the measured callee verdict across that seam.
    pub(crate) fresh_variant_payload_symbols: HashMap<String, HashSet<(u32, u32)>>,
}

impl Default for CallScrutineeProvenance {
    fn default() -> Self {
        Self {
            provenance: HashMap::new(),
            extern_names: HashSet::new(),
            extern_table: ExternContractTable::default(),
            may_mutate: HashMap::new(),
            fresh_owner_verdicts: FreshOwnerVerdicts::denying_all(),
            owned_string_return_carriers: HashSet::new(),
            owned_string_return_carrier_symbols: HashSet::new(),
            fresh_variant_payloads: HashMap::new(),
            fresh_variant_payload_symbols: HashMap::new(),
        }
    }
}

impl CallScrutineeProvenance {
    /// Whether `callee` is an analysed Hew body whose every return path
    /// produces one independently releasable string share.
    ///
    /// The call expression's resolved result type is checked by the sole
    /// consumer before this query, so generic origins (`fn id<T>(x: T) -> T`)
    /// can be admitted when instantiated at `string`. This query grants no
    /// general freshness: `ParamsOnly` rows are accepted only for the string
    /// return-carrier protocol.
    #[must_use]
    pub(crate) fn callee_returns_owned_string_carrier(&self, callee: &HirExpr) -> bool {
        let HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Item(item_id),
        } = &callee.kind
        else {
            return false;
        };
        !self.extern_table.is_extern_name(name)
            && self.owned_string_return_carriers.contains(item_id)
    }

    /// True only for an analysed Hew function and an active enum payload whose
    /// every constructor path was proven fresh under both the precise alias and
    /// opaque-extern-transfer analyses. Direct externs never reach this query.
    #[must_use]
    pub(crate) fn callee_returns_fresh_variant_payload(
        &self,
        callee: &HirExpr,
        variant_idx: u32,
        field_idx: u32,
    ) -> bool {
        let HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Item(item_id),
        } = &callee.kind
        else {
            return false;
        };
        !self.extern_table.is_extern_name(name)
            && (self
                .fresh_variant_payloads
                .get(item_id)
                .is_some_and(|fields| fields.contains(&(variant_idx, field_idx)))
                || self
                    .fresh_variant_payload_symbols
                    .get(name)
                    .is_some_and(|fields| fields.contains(&(variant_idx, field_idx))))
    }
}

/// Build the module-global preflight context: the precise return-provenance
/// fixpoint (via the interprocedural mutation summary), the declared-extern id
/// set, and the audited extern contract table.
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "built once over the pipeline's default-hasher origin_fns map"
)]
pub(crate) fn build_call_scrutinee_provenance(
    module: &hew_hir::HirModule,
    origin_fns: &HashMap<hew_hir::ItemId, &HirFn>,
    coarse_fresh_returns: &HashMap<hew_hir::ItemId, bool>,
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
    let owned_string_carrier_provenance =
        compute_owned_string_return_carrier_provenance(origin_fns, &extern_table, &may_mutate);
    let retained_string_projection_returns =
        compute_fn_returns_retained_string_projection_owner(origin_fns);
    // The table-aware freshness authority: the coarse proof MINUS everything the
    // opaque-extern laundering summary vetoes, plus the direct-extern name veto.
    // The coarse map is passed in rather than recomputed so both consumers read
    // one fixpoint, and it is consumed HERE — no builder ever sees it.
    // The adoption boundary, read off the module's own type declarations
    // BEFORE either fixpoint runs, so both are computed under the identical
    // rule the value-position queries later apply.
    let declared_release = DeclaredReleaseTypes::from_module(module);
    let launders_opaque_extern =
        compute_fn_return_launders_opaque_extern(origin_fns, &extern_table, &declared_release);
    let carries_proven_foreign =
        compute_fn_return_carries_proven_foreign(origin_fns, &extern_table, &declared_release);
    let fresh_variant_payloads = compute_fresh_variant_payloads(
        origin_fns,
        &provenance,
        &extern_table,
        &may_mutate,
        &launders_opaque_extern,
        &declared_release,
    );
    let fresh_owner_verdicts = FreshOwnerVerdicts::build(
        coarse_fresh_returns,
        &provenance,
        &retained_string_projection_returns,
        &launders_opaque_extern,
        &carries_proven_foreign,
        &extern_table,
        &declared_release,
    );
    let mut fresh_variant_payload_symbols = HashMap::new();
    for (&id, function) in origin_fns {
        if let Some(fields) = fresh_variant_payloads.get(&id) {
            fresh_variant_payload_symbols.insert(function.name.clone(), fields.clone());
        }
    }
    for mono in &module.monomorphisations {
        if let Some(fields) = fresh_variant_payloads.get(&mono.key.origin) {
            fresh_variant_payload_symbols.insert(mono.mangled_name.clone(), fields.clone());
        }
    }
    // A string return needs one independently releasable share, not necessarily
    // a pointer-distinct allocation. The string-specific fixpoint preserves the
    // established direct-call rules (`Fresh(∅)` and `ParamsOnly({PARAM})`) and
    // additionally reads an indirect call through the ClosureInvoke ABI's
    // one-share postcondition. Calls propagate that fact through wrapper depth.
    // An OPAQUE direct extern remains OPAQUE transitively.
    //
    // Do not filter on the origin's declared return type here: a generic origin
    // may declare `T` and be instantiated at `string`; the consumer checks the
    // concrete call-result type before consulting this set.
    let owned_string_return_carriers = owned_string_carrier_provenance
        .iter()
        .filter_map(|(&id, bits)| (!bits.is_opaque()).then_some(id))
        .collect::<HashSet<_>>();
    let owned_string_return_carrier_symbols = origin_fns
        .iter()
        .filter(|entry| {
            entry.1.type_params.is_empty() && owned_string_return_carriers.contains(entry.0)
        })
        .map(|entry| entry.1.name.clone())
        .chain(
            module
                .monomorphisations
                .iter()
                .filter(|mono| owned_string_return_carriers.contains(&mono.key.origin))
                .map(|mono| mono.mangled_name.clone()),
        )
        // Audited extern transfers carry the same caller-side one-share
        // postcondition. They have no Hew body and therefore no `origin_fns`
        // row, so include their declared symbols explicitly; otherwise the
        // fail-closed terminator classifier drops the established authority
        // for measured producers such as `hew_stream_last_error`.
        .chain(
            extern_names
                .iter()
                .filter(|name| extern_table.extern_return_is_audited_fresh_owner(name))
                .cloned(),
        )
        .collect();
    CallScrutineeProvenance {
        provenance,
        extern_names,
        extern_table,
        may_mutate,
        fresh_owner_verdicts,
        owned_string_return_carriers,
        owned_string_return_carrier_symbols,
        fresh_variant_payloads,
        fresh_variant_payload_symbols,
    }
}

/// Narrow active-variant ownership summary for a mixed enum return.
///
/// A whole `Result<T, E>` is fresh only when *both* arms are fresh, which is
/// the right requirement for minting a recursive shell owner. It is too coarse
/// for `match f() { Ok(value) => ... }` when only `value` crosses an audited
/// transferred-return seam. This summary retains the existing whole-value
/// default-deny and proves only `(variant, field)` positions that are fresh on
/// every constructor path for that position. The match lowering then gives that
/// selected field one owner; it never gives the shell an owner.
#[allow(
    clippy::implicit_hasher,
    reason = "built once over the pipeline's default-hasher origin_fns map"
)]
fn compute_fresh_variant_payloads(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    provenance: &HashMap<hew_hir::ItemId, ReturnProvenance>,
    extern_table: &ExternContractTable,
    may_mutate: &HashMap<hew_hir::ItemId, bool>,
    launders_opaque_extern: &HashSet<hew_hir::ItemId>,
    declared_release: &DeclaredReleaseTypes,
) -> HashMap<hew_hir::ItemId, HashSet<(u32, u32)>> {
    let analyzed: HashSet<hew_hir::ItemId> = fns.keys().copied().collect();
    let mut result = HashMap::new();
    for (&id, &function) in fns {
        let local_bits =
            compute_local_binding_provenance(function, provenance, extern_table, may_mutate);
        let precise = PrecisePolicy {
            provenance,
            extern_table,
            local_bits: &local_bits,
        };
        let foreign = OpaqueExternTaintPolicy {
            extern_table,
            analyzed: &analyzed,
            tainted: launders_opaque_extern,
            declared_release,
        };
        let mut returns = Vec::new();
        crate::lower::collect_return_values_in_block(&function.body, &mut returns);
        if let Some(tail) = &function.body.tail {
            if !matches!(tail.ty, ResolvedTy::Unit | ResolvedTy::Never) {
                returns.push(tail);
            }
        }

        let mut complete = !returns.is_empty();
        let mut payloads: HashMap<(u32, u32), Vec<&HirExpr>> = HashMap::new();
        for value in returns {
            complete &= collect_return_variant_payloads(value, &mut payloads);
        }
        if !complete {
            continue;
        }
        let fresh = payloads
            .into_iter()
            .filter_map(|(position, values)| {
                values
                    .iter()
                    .all(|value| {
                        return_alias_bits_in_block(value, &function.body, &precise).is_fresh()
                            && return_alias_bits_in_block(value, &function.body, &foreign)
                                .is_fresh()
                    })
                    .then_some(position)
            })
            .collect::<HashSet<_>>();
        if !fresh.is_empty() {
            result.insert(id, fresh);
        }
    }
    result
}

/// Collect enum constructor payloads from a value-returning expression. Every
/// accepted branch must expose a constructor; an alias through a binding or an
/// unmodelled control-flow form returns `false` and denies the entire function.
/// The outer return collector supplies explicit `return` sites independently,
/// while this walk expands tail `if`/`match` paths into their active variants.
fn collect_return_variant_payloads<'a>(
    expr: &'a HirExpr,
    payloads: &mut HashMap<(u32, u32), Vec<&'a HirExpr>>,
) -> bool {
    match &expr.kind {
        HirExprKind::MachineVariantCtor {
            state_idx, payload, ..
        } => {
            let Some(payload) = payload else {
                return true;
            };
            for (field_idx, (_, value)) in payload.iter().enumerate() {
                let Ok(field_idx) = u32::try_from(field_idx) else {
                    return false;
                };
                let Ok(variant_idx) = u32::try_from(*state_idx) else {
                    return false;
                };
                payloads
                    .entry((variant_idx, field_idx))
                    .or_default()
                    .push(value);
            }
            true
        }
        HirExprKind::Block(block) => block
            .tail
            .as_deref()
            .is_some_and(|tail| collect_return_variant_payloads(tail, payloads)),
        HirExprKind::If {
            then_expr,
            else_expr: Some(else_expr),
            ..
        } => {
            collect_return_variant_payloads(then_expr, payloads)
                && collect_return_variant_payloads(else_expr, payloads)
        }
        HirExprKind::Match { arms, .. } if !arms.is_empty() => arms
            .iter()
            .all(|arm| collect_return_variant_payloads(&arm.body, payloads)),
        HirExprKind::Return { value: Some(value) } => {
            collect_return_variant_payloads(value, payloads)
        }
        _ => false,
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
        HirExprKind::Call { callee, args, .. } | HirExprKind::SpawnedCall { callee, args, .. } => {
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
        reachable_bindings_in_stmt(stmt, out);
    }
    if let Some(tail) = &block.tail {
        reachable_bindings(tail, out);
    }
}

/// Reachability over a single statement — the per-statement body of
/// [`reachable_bindings_in_block`], factored out so the fresh-owner see-through
/// (`stmt_mentions_binding`) can ask the same total question of one statement.
#[allow(
    clippy::match_same_arms,
    reason = "statement arms mirror the sealed HirStmtKind surface exhaustively"
)]
fn reachable_bindings_in_stmt(stmt: &HirStmt, out: &mut Reachable) {
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
            HirExprKind::Call { callee, args, .. } => {
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
    local_binding_provenance_impl(
        f,
        provenance,
        extern_table,
        may_mutate,
        LocalProvenancePolicy::Precise,
    )
    .0
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
    let (bits, aliased, pattern_binders) = local_binding_provenance_impl(
        f,
        provenance,
        extern_table,
        may_mutate,
        LocalProvenancePolicy::Precise,
    );
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

#[derive(Clone, Copy)]
enum LocalProvenancePolicy {
    Precise,
    OwnedStringCarrier,
}

fn local_binding_provenance_impl(
    f: &HirFn,
    provenance: &HashMap<hew_hir::ItemId, AliasBits>,
    extern_table: &ExternContractTable,
    may_mutate: &HashMap<hew_hir::ItemId, bool>,
    policy_kind: LocalProvenancePolicy,
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
            let mut new_bits = *bits.get(&id).unwrap_or(&AliasBits::EMPTY);
            for src in sources {
                new_bits |= match src {
                    DefSource::Value(e) => match policy_kind {
                        LocalProvenancePolicy::Precise => return_alias_bits(
                            e,
                            &PrecisePolicy {
                                provenance,
                                extern_table,
                                local_bits: &bits,
                            },
                        ),
                        LocalProvenancePolicy::OwnedStringCarrier => return_alias_bits(
                            e,
                            &OwnedStringCarrierPolicy {
                                provenance,
                                extern_table,
                                local_bits: &bits,
                            },
                        ),
                    },
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
        HirExprKind::Call { callee, args, .. } | HirExprKind::SpawnedCall { callee, args, .. } => {
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
            HirExprKind::Call { callee, args, .. } => {
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
    matches!(
        hew_types::runtime_call::RuntimeCallFamily::from_checker_signature(name),
        Some(
            hew_types::runtime_call::RuntimeCallFamily::VecNew
                | hew_types::runtime_call::RuntimeCallFamily::HashMapNew
                | hew_types::runtime_call::RuntimeCallFamily::HashSetNew
        )
    )
}

/// The stdlib catalog's IDENTITY JOIN: the emitted runtime symbol a body-less
/// resolved callee will actually lower to, or `None` when the callee is not a
/// catalog shim row.
///
/// # Why an identity join and not a name lookup
///
/// `LowerCtx::seed_stdlib_fn_registry` mints one `ItemId(u32::MAX - index)` per
/// `stdlib_catalog::entries()` row and registers that SAME id under BOTH
/// spellings a call site can carry — the row's surface `name` (`to_upper_str`,
/// `to_string_i64`) and, when the row's linkage declares one, its emitted
/// runtime `symbol` (`hew_string_to_uppercase`, `hew_i64_to_string`). So the
/// callee's `(name, ItemId)` pair is a compiler-minted identity, and this
/// function is its inverse.
///
/// Both halves are required, and each closes a forgery direction:
///
/// * **id without name.** The id band is not exclusively the catalog's:
///   `SYNTHETIC_OPTION_ITEM` / `SYNTHETIC_RESULT_ITEM` are `u32::MAX - 1` and
///   `u32::MAX - 2`, which index real catalog rows. Requiring the name to match
///   the row rejects those.
/// * **name without id.** A user `fn hew_string_to_uppercase()` or an
///   `extern "C" { fn hew_string_to_uppercase(); }` carries a REAL source
///   `ItemId`, whose `u32::MAX - id` is far past the end of the catalog. This is
///   the same discipline the audited-argument table applies to `hew_fs_rename`:
///   a declaration cannot claim a runtime row by spelling its name.
///
/// Only the three linkages that DECLARE an emitted C symbol
/// ([`BuiltinLinkage::runtime_symbol`]) join. A `CompilerIntrinsic`,
/// `PrintIntercept`, `CalleeNameDispatchOnly`, `NodeRegisterByPid` or
/// `LayoutDescriptorSymbol` row has no single emitted callee whose ownership
/// contract could be read, so it does not join and keeps its caller's
/// fail-closed answer.
#[must_use]
pub fn stdlib_shim_emitted_symbol(name: &str, id: hew_hir::ItemId) -> Option<&'static str> {
    // JUSTIFIED: unsupported sub-32-bit targets cannot decode this id; None is fail-closed.
    let index = usize::try_from(u32::MAX - id.0).ok()?;
    let entry = hew_hir::stdlib_catalog::entries().get(index)?;
    let symbol = entry.linkage.runtime_symbol()?;
    (entry.name == name || symbol == name).then_some(symbol)
}

/// The provenance of a call to a body-less stdlib catalog shim, read from the
/// ONE emitted-symbol ownership contract ([`method_return_provenance`]).
///
/// `Some(CallClass::Fresh)` ONLY when that contract proves the result is a new
/// `+1` owner. Every other answer — a borrowed getter, an interior alias of the
/// receiver, an untracked result, or a callee that is not a catalog shim at all
/// — is `None`, which leaves the caller's own fail-closed answer in place. The
/// function can therefore only ever turn an `Opaque` fall-through into `Fresh`;
/// it can never narrow an admission the caller already made.
///
/// This is not new trust. [`PrecisePolicy`] already reads
/// [`method_return_provenance`] at its `ResolvedImplCall` leaf, for the same
/// question about the same values; the catalog simply did not join at the `Call`
/// arm, so `Some(Row { name: "x".to_upper(), id: n })` read `{OPAQUE}` while
/// `"x".to_upper()` reached in a method position read `∅`.
#[must_use]
pub fn stdlib_shim_return_class(name: &str, id: hew_hir::ItemId) -> Option<CallClass> {
    let symbol = stdlib_shim_emitted_symbol(name, id)?;
    method_return_provenance(symbol)
        .is_fresh()
        .then_some(CallClass::Fresh)
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
        // caller — the jwt/encrypt false-reject contamination). The name-keyed
        // audited authority is exactly the query that avoids that collision, so
        // it is the one asked here, and it is the SAME set
        // `OpaqueExternTaintPolicy` reads: an extern admitted as an audited
        // fresh owner hands back a value that aliases no parameter and carries
        // no opaque origin, which is what `∅` means. Everything else stays
        // `{OPAQUE}`.
        //
        // The set is narrow by construction: a pointer-free result (a scalar,
        // or a module record of scalar leaves) which owns no heap at all and so
        // is vacuously `∅`; and — the only pointer-bearing case — a result
        // whose contract carries a MEASURED `result-retention = "transferred"`
        // and whose audited release is the one the drop plan will emit for the
        // declared type (see `build_extern_contract_table` Clause C).
        //
        // Reading `{OPAQUE}` for those was an over-approximation, and it is the
        // one that withheld the release from every Hew wrapper around the
        // `*_last_error` family: such a wrapper's whole body is the extern
        // call, so `{OPAQUE}` here propagates to every caller and no owner is
        // ever minted for the error string.
        if self.extern_table.is_extern_name(name) {
            return if self.extern_table.extern_return_is_audited_fresh_owner(name) {
                CallClass::Fresh
            } else {
                CallClass::Opaque
            };
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
        // Clause 4: a body-less stdlib CATALOG shim, resolved through the
        // compiler-minted `(name, ItemId)` identity and answered from the one
        // emitted-symbol ownership contract. Additive only: a row that is not a
        // proved `+1` producer, and any callee that is not a catalog shim, falls
        // through to clause 5 unchanged.
        //
        // Without this join a fresh-owner shim reached through a `Call`
        // (`Row { name: "x".to_upper(), .. }`) read `{OPAQUE}` while the SAME
        // contract read `∅` for the same value reached through a method leaf,
        // and the summary's consumer withheld a legitimate release.
        } else if let Some(class) = stdlib_shim_return_class(name, *id) {
            class
        // Clause 5: an unknown/missing item (absent from every table → Opaque).
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
            // A method call → the EMITTED-symbol contract. When the HIR carries a
            // placeholder instead of the symbol the site will emit, the
            // placeholder is resolved against the same dispatch authority first
            // (see [`placeholder_method_return_provenance`]); only a placeholder
            // that authority cannot answer falls back to the raw-symbol lookup,
            // which is fail-closed for every unknown spelling.
            HirExprKind::ResolvedImplCall {
                target_symbol,
                target_family,
                ..
            } => placeholder_method_return_provenance(target_symbol, *target_family)
                .unwrap_or_else(|| method_return_provenance(target_symbol)),
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

/// String-return-carrier sibling of [`PrecisePolicy`].
///
/// This policy changes exactly one leaf: a call through a first-class callable
/// value is an independently releasable carrier when its concrete result is a
/// `string`. Every callable pair targets a compiler-generated
/// `ClosureInvoke` shim, and those shims establish one caller-owned string
/// share before returning: fresh results transfer their existing `+1`, string
/// parameters are retained at the return edge, and string capture/field loads
/// are retained when loaded.
///
/// The policy is consumed only by
/// [`compute_owned_string_return_carrier_provenance`], whose result is queried
/// only after the call site's concrete result type has been checked as
/// `string`. It must never feed the general fresh-owner/composite authorities:
/// an indirect call remains pointer-provenance-opaque there. Direct calls,
/// including declared externs, delegate unchanged to [`PrecisePolicy`], so an
/// ownership-opaque extern and every Hew wrapper that forwards it remain
/// denied.
#[derive(Debug)]
struct OwnedStringCarrierPolicy<'a> {
    provenance: &'a HashMap<hew_hir::ItemId, AliasBits>,
    extern_table: &'a ExternContractTable,
    local_bits: &'a HashMap<BindingId, AliasBits>,
}

impl OwnedStringCarrierPolicy<'_> {
    fn precise(&self) -> PrecisePolicy<'_> {
        PrecisePolicy {
            provenance: self.provenance,
            extern_table: self.extern_table,
            local_bits: self.local_bits,
        }
    }
}

impl LeafPolicy for OwnedStringCarrierPolicy<'_> {
    fn classify_call(&self, callee: &HirExpr) -> CallClass {
        if !matches!(
            &callee.kind,
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Item(_),
                ..
            }
        ) {
            return CallClass::Fresh;
        }
        self.precise().classify_call(callee)
    }

    fn leaf_bits(&self, expr: &HirExpr) -> AliasBits {
        self.precise().leaf_bits(expr)
    }

    fn missing_position_bits(&self, enclosing: &HirExpr) -> AliasBits {
        self.precise().missing_position_bits(enclosing)
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

/// String-specific module fixpoint proving that each function return path
/// establishes one independently releasable carrier.
///
/// This mirrors [`compute_call_scrutinee_return_provenance`] but runs under
/// [`OwnedStringCarrierPolicy`]. Keeping the tables separate is load-bearing:
/// the general table must continue to classify indirect calls as `OPAQUE` for
/// composite/destructive-owner decisions, while this table may use the
/// `ClosureInvoke` string ABI's retained-share postcondition. Direct opaque
/// externs are still `OPAQUE` and that bit propagates through arbitrary Hew
/// wrapper depth.
fn compute_owned_string_return_carrier_provenance(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    extern_table: &ExternContractTable,
    may_mutate: &HashMap<hew_hir::ItemId, bool>,
) -> HashMap<hew_hir::ItemId, ReturnProvenance> {
    let mut provenance: HashMap<hew_hir::ItemId, AliasBits> =
        fns.keys().map(|&id| (id, AliasBits::EMPTY)).collect();
    loop {
        let mut changed = false;
        for (&id, &f) in fns {
            let local_bits = local_binding_provenance_impl(
                f,
                &provenance,
                extern_table,
                may_mutate,
                LocalProvenancePolicy::OwnedStringCarrier,
            )
            .0;
            let policy = OwnedStringCarrierPolicy {
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
            for value in return_values {
                bits |= return_alias_bits(value, &policy);
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

/// Functions whose every value-returning path produces a `string` by loading a
/// record or tuple field.
///
/// This is deliberately separate from [`ReturnProvenance`]. A string field
/// projection still aliases the same refcounted allocation as its object, so it
/// is not "fresh" in the pointer-provenance sense. MIR codegen nevertheless
/// emits `hew_string_clone` for every string-typed `RecordFieldLoad` and
/// `TupleFieldLoad`, giving the returned value an independent `+1` release
/// obligation. The caller may therefore own and drop that returned share once.
///
/// Keeping the proof separate preserves the frozen coarse alias verdicts and
/// prevents the `+1` release fact from being mistaken for non-aliasing by other
/// consumers. [`FreshOwnerVerdicts::item_returns_retained_string_owner`] is its
/// only reader, and the opaque-extern laundering veto still applies there.
fn compute_fn_returns_retained_string_projection_owner(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
) -> HashSet<hew_hir::ItemId> {
    fns.iter()
        .filter_map(|(&id, &f)| {
            let mut return_values: Vec<&HirExpr> = Vec::new();
            crate::lower::collect_return_values_in_block(&f.body, &mut return_values);
            if let Some(tail) = &f.body.tail {
                if !matches!(tail.ty, ResolvedTy::Unit | ResolvedTy::Never) {
                    return_values.push(tail);
                }
            }
            (!return_values.is_empty()
                && return_values
                    .into_iter()
                    .all(value_is_retained_string_projection))
            .then_some(id)
        })
        .collect()
}

/// Whether this value position is guaranteed to lower to a string field-load
/// retain. Value-preserving wrappers are transparent; every branch must satisfy
/// the proof. All calls, locals, literals, and unmodelled forms fail closed.
fn value_is_retained_string_projection(expr: &HirExpr) -> bool {
    if !matches!(expr.ty, ResolvedTy::String) {
        return false;
    }
    match &expr.kind {
        HirExprKind::FieldAccess { .. } | HirExprKind::TupleIndex { .. } => true,
        HirExprKind::Block(block) => block
            .tail
            .as_deref()
            .is_some_and(value_is_retained_string_projection),
        HirExprKind::If {
            then_expr,
            else_expr: Some(else_expr),
            ..
        } => {
            value_is_retained_string_projection(then_expr)
                && value_is_retained_string_projection(else_expr)
        }
        HirExprKind::Match { arms, .. } => {
            !arms.is_empty()
                && arms
                    .iter()
                    .all(|arm| value_is_retained_string_projection(&arm.body))
        }
        HirExprKind::Return { value: Some(value) } => value_is_retained_string_projection(value),
        _ => false,
    }
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
    fn record_field_tys(
        &self,
        _name: &str,
        _args: &[ResolvedTy],
        _builtin: Option<hew_types::BuiltinType>,
    ) -> Option<Vec<ResolvedTy>> {
        None
    }

    fn enum_variant_field_tys(
        &self,
        _name: &str,
        _args: &[ResolvedTy],
    ) -> Option<Vec<Vec<ResolvedTy>>> {
        None
    }

    fn enum_is_indirect(&self, _name: &str, _args: &[ResolvedTy]) -> bool {
        false
    }
}

#[cfg(test)]
#[path = "return_provenance_ref.rs"]
mod frozen_reference;

#[cfg(test)]
pub(crate) mod tests {
    use super::frozen_reference::compute_fn_returns_fresh_owner_ref;
    use super::*;
    use crate::lower::compute_fn_returns_fresh_owner;

    /// Front-end-lower a `.hew` source string to a `HirModule`.
    pub(crate) fn lower_source(source: &str) -> hew_hir::HirModule {
        lower_source_with_opaque_candidate(source, None)
    }

    /// Front-end-lower a source string while supplying the exact checker-owned
    /// opaque lifecycle that an isolated unit fixture cannot derive from the
    /// real stdlib module graph.
    pub(crate) fn lower_source_with_opaque_candidate(
        source: &str,
        candidate: Option<hew_types::OpaqueResourceLifecycleCandidate>,
    ) -> hew_hir::HirModule {
        let expects_candidate = candidate.is_some();
        let parsed = hew_parser::parse(source);
        assert!(
            parsed.errors.is_empty(),
            "parse errors: {:#?}",
            parsed.errors
        );
        let mut checker =
            hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let mut tc_output = checker.check_program(&parsed.program);
        if let Some(mut candidate) = candidate {
            let close_dispatch_key = format!("{}::close", candidate.resource_type);
            candidate.close_declaration = tc_output
                .impl_method_declaration_ids
                .get(&close_dispatch_key)
                .unwrap_or_else(|| {
                    panic!("missing checker close identity for {close_dispatch_key}")
                })
                .clone();
            tc_output
                .opaque_resource_candidates
                .candidates
                .insert(candidate.resource_declaration.clone(), candidate);
        }
        let output = hew_hir::lower_program(
            &parsed.program,
            &tc_output,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        if expects_candidate {
            assert!(
                output.diagnostics.is_empty(),
                "candidate lowering diagnostics: {:#?}",
                output.diagnostics
            );
        }
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
            type Box { data: string }
            fn passthru(x: string) -> string { x }
            fn project(b: Box) -> string { b.data }
            fn ctor(s: string) -> Box { Box { data: s } }
            ",
        );
    }

    /// A string field load aliases its source allocation but codegen retains it,
    /// so release authority and pointer-alias provenance must remain distinct.
    #[test]
    fn retained_string_projection_is_a_release_owner_not_a_fresh_alias() {
        let module = lower_source(
            r"
            type Box { data: string }
            fn record_project(b: Box) -> string { b.data }
            fn tuple_project(t: (string, i64)) -> string { t.0 }
            fn passthru(s: string) -> string { s }
            fn mixed(flag: bool, b: Box, s: string) -> string {
                if flag { b.data } else { s }
            }
            ",
        );
        let origin_fns = origin_fns_of(&module);
        let retained = compute_fn_returns_retained_string_projection_owner(&origin_fns);

        for name in ["record_project", "tuple_project"] {
            assert!(
                retained.contains(&fn_id(&module, name)),
                "`{name}` lowers its returned string projection through a retaining field load"
            );
        }
        for name in ["passthru", "mixed"] {
            assert!(
                !retained.contains(&fn_id(&module, name)),
                "`{name}` has a path that merely forwards a string and must not mint a second owner"
            );
        }

        let coarse = compute_fn_returns_fresh_owner(&origin_fns);
        assert!(
            !coarse[&fn_id(&module, "record_project")],
            "the frozen alias analysis must still report that the projection aliases its parameter"
        );
        let extern_table = build_extern_contract_table(&module);
        let precise = compute_call_scrutinee_return_provenance(
            &origin_fns,
            &extern_table,
            &compute_may_mutate_heap_param(&origin_fns),
        );
        assert!(
            precise[&fn_id(&module, "record_project")].is_params_only(),
            "release authority must not rewrite pointer-alias provenance"
        );
        let authority =
            build_call_scrutinee_provenance(&module, &origin_fns, &coarse).fresh_owner_verdicts;
        let projected = fn_id(&module, "record_project");
        assert!(
            !authority.item_returns_fresh_owner(projected),
            "a retained projection must not broaden the non-aliasing/fresh-owner row"
        );
        assert!(
            authority.item_returns_retained_string_owner(projected),
            "the string-only query must preserve the projection's one retained release"
        );
        assert!(
            !authority.item_returns_retained_string_owner(fn_id(&module, "passthru")),
            "a forwarded parameter has no independently retained projection share"
        );
        assert!(
            !authority.item_returns_retained_string_owner(fn_id(&module, "mixed")),
            "one forwarding return path must veto the retained-projection authority"
        );
    }

    #[test]
    fn indirect_string_carrier_authority_is_separate_and_extern_closed() {
        let module = lower_source(
            r#"
            extern "C" {
                fn host_opaque_string() -> string;
            }

            fn invoke(make: fn() -> string) -> string {
                make()
            }

            fn invoke_via_local(make: fn() -> string) -> string {
                let value = make();
                value
            }

            fn opaque_wrapper() -> string {
                unsafe { host_opaque_string() }
            }
            "#,
        );
        let origin_fns = origin_fns_of(&module);
        let coarse = compute_fn_returns_fresh_owner(&origin_fns);
        let context = build_call_scrutinee_provenance(&module, &origin_fns, &coarse);

        for name in ["invoke", "invoke_via_local"] {
            let id = fn_id(&module, name);
            assert!(
                context.provenance[&id].is_opaque(),
                "`{name}` must remain opaque to the general pointer/composite \
                 freshness authority"
            );
            assert!(
                context.owned_string_return_carriers.contains(&id),
                "`{name}` must carry the ClosureInvoke ABI's independently \
                 releasable string-share fact, including through a local alias"
            );
        }

        let opaque = fn_id(&module, "opaque_wrapper");
        assert!(
            context.provenance[&opaque].is_opaque()
                && !context.owned_string_return_carriers.contains(&opaque),
            "a direct ownership-opaque extern wrapper must be denied by both \
             authorities; the closure-only rule must not launder it"
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
            type Box { data: string }
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
            type Box { data: Vec<i64> }
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

    // -- The stdlib catalog identity join -------------------------------------

    /// The `ItemId` `seed_stdlib_fn_registry` mints for the catalog row at
    /// `index`, computed the same way the seeder computes it.
    fn catalog_item_id(index: usize) -> hew_hir::ItemId {
        hew_hir::ItemId(u32::MAX - u32::try_from(index).expect("catalog index fits in u32"))
    }

    /// The index of the catalog row with the given surface name.
    fn catalog_index_of(name: &str) -> usize {
        hew_hir::stdlib_catalog::entries()
            .iter()
            .position(|entry| entry.name == name)
            .unwrap_or_else(|| panic!("catalog row {name} must exist"))
    }

    #[test]
    fn a_catalog_shim_joins_under_both_spellings_of_its_identity() {
        // `seed_stdlib_fn_registry` registers ONE id under the row's surface
        // name AND under its emitted runtime symbol, so a call site can carry
        // either spelling. Both must join to the same emitted symbol.
        let id = catalog_item_id(catalog_index_of("to_upper_str"));
        assert_eq!(
            stdlib_shim_emitted_symbol("to_upper_str", id),
            Some("hew_string_to_uppercase")
        );
        assert_eq!(
            stdlib_shim_emitted_symbol("hew_string_to_uppercase", id),
            Some("hew_string_to_uppercase")
        );
        // …and the emitted symbol's audited contract is what answers, so the
        // `Call` arm and the method leaf agree about the same value.
        assert!(matches!(
            stdlib_shim_return_class("hew_string_to_uppercase", id),
            Some(CallClass::Fresh)
        ));
    }

    #[test]
    fn a_source_declaration_cannot_claim_a_catalog_row_by_spelling_its_name() {
        // A user `fn`/`extern` spelling a runtime symbol carries a REAL source
        // `ItemId`, which is nowhere near the catalog band. The name alone
        // proves nothing — the same discipline the audited ARGUMENT table
        // applies to a root declaration spelling `hew_fs_rename`.
        for forged in [0_u32, 1, 7, 4096] {
            assert_eq!(
                stdlib_shim_emitted_symbol("hew_string_to_uppercase", hew_hir::ItemId(forged)),
                None,
                "a source item id must not resolve a catalog row"
            );
        }
    }

    #[test]
    fn a_synthetic_id_colliding_with_the_catalog_band_cannot_claim_a_row() {
        // The `u32::MAX - n` band is NOT exclusively the catalog's:
        // `SYNTHETIC_OPTION_ITEM` is `u32::MAX - 1` and `SYNTHETIC_RESULT_ITEM`
        // is `u32::MAX - 2`, both of which index real catalog rows. The name
        // half of the identity is what rejects them.
        for (offset, name) in [(1_usize, "Option"), (2, "Result")] {
            assert_eq!(
                stdlib_shim_emitted_symbol(name, catalog_item_id(offset)),
                None,
                "a synthetic item sharing the catalog id band must not join"
            );
        }
    }

    #[test]
    fn the_join_is_additive_only_and_never_narrows() {
        // A catalog row whose emitted symbol is NOT a proved `+1` producer
        // answers `None`, leaving the caller's own fail-closed answer in place —
        // the join can only ever turn an `Opaque` fall-through into `Fresh`.
        let index = catalog_index_of("len_str");
        let id = catalog_item_id(index);
        assert_eq!(
            stdlib_shim_emitted_symbol("len_str", id),
            Some("hew_string_length"),
            "the row must join"
        );
        assert!(method_return_provenance("hew_string_length").is_opaque());
        assert!(
            stdlib_shim_return_class("len_str", id).is_none(),
            "a non-producer row must not answer at all"
        );
        // A row with no emitted C symbol (an intrinsic, a print intercept, a
        // layout descriptor) has no single callee whose contract could be read,
        // so it does not join either.
        for (index, entry) in hew_hir::stdlib_catalog::entries().iter().enumerate() {
            if entry.linkage.runtime_symbol().is_none() {
                assert_eq!(
                    stdlib_shim_emitted_symbol(entry.name, catalog_item_id(index)),
                    None,
                    "row {} has no emitted symbol and must not join",
                    entry.name
                );
            }
        }
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

    /// The generic-spine placeholder resolves to the SAME verdict the emitted
    /// symbol would give, without the element class the site cannot see.
    ///
    /// `fn first<T>(v: Vec<T>) -> Option<T> { v.get(0) }` is summarised once,
    /// over the pre-monomorphisation body, where the element is still `T`. The
    /// checker left `hew_vec_get_FAMILY`; the emitted-symbol contract has no row
    /// for that spelling and answers `{OPAQUE}`. But the dispatch authority
    /// resolves EVERY element class it can resolve at all to `hew_vec_get_clone`,
    /// a proved `+1` owner — so the verdict is knowable without the element.
    #[test]
    fn a_generic_vec_get_placeholder_is_fresh_over_every_element_class() {
        use hew_types::{MethodTargetFamily, VecMethod};
        assert_eq!(
            placeholder_method_return_provenance(
                "hew_vec_get_FAMILY",
                MethodTargetFamily::Vec(VecMethod::Get),
            ),
            Some(AliasBits::EMPTY),
            "every element class `Vec::get` resolves at all emits `hew_vec_get_clone`, so the \
             generic spine's placeholder must read as fresh — reading it as `{{OPAQUE}}` is what \
             withheld the release for `Some(g) => g` over a generic getter"
        );
        assert_eq!(
            method_return_provenance("hew_vec_get_clone"),
            AliasBits::EMPTY,
            "the quantified answer must agree with the emitted-symbol contract it quantifies over"
        );
    }

    /// The quantification is not a blanket "a placeholder is fresh". Every other
    /// `Vec` method's emitted symbol genuinely depends on the element class, and
    /// the element classes disagree — so the union carries `{OPAQUE}` and the
    /// verdict stays exactly as fail-closed as before.
    #[test]
    fn a_family_placeholder_whose_element_classes_disagree_stays_opaque() {
        use hew_types::{MethodTargetFamily, VecMethod};
        for method in [
            VecMethod::Push,
            VecMethod::Pop,
            VecMethod::Set,
            VecMethod::Remove,
            VecMethod::Contains,
            VecMethod::Clear,
            VecMethod::Clone,
            VecMethod::Append,
        ] {
            assert_eq!(
                placeholder_method_return_provenance(
                    "hew_vec_x_FAMILY",
                    MethodTargetFamily::Vec(method),
                ),
                Some(AliasBits::OPAQUE),
                "{method:?} has no element-class-independent answer, so quantifying over the \
                 element must leave it fail-closed"
            );
        }
        assert_eq!(
            placeholder_method_return_provenance(
                "hew_vec_x_FAMILY",
                MethodTargetFamily::Vec(VecMethod::Join),
            ),
            Some(AliasBits::EMPTY),
            "join always resolves to the same copying string ABI, so its generic \
             placeholder has an element-class-independent fresh-owner answer"
        );
    }

    /// `HashMap::get` is the degenerate quantification: lowering always emits the
    /// fresh-owner clone choke, so the checker's `hew_hashmap_get_layout`
    /// placeholder — which the emitted-symbol contract refuses — must not be the
    /// spelling the walk reads.
    #[test]
    fn a_hashmap_get_placeholder_reads_the_clone_choke_not_the_layout_placeholder() {
        use hew_types::{HashMapMethod, MethodTargetFamily};
        assert_eq!(
            method_return_provenance("hew_hashmap_get_layout"),
            AliasBits::OPAQUE,
            "the placeholder spelling itself must stay unknown to the emitted-symbol contract"
        );
        assert_eq!(
            placeholder_method_return_provenance(
                "hew_hashmap_get_layout",
                MethodTargetFamily::HashMap(HashMapMethod::Get),
            ),
            Some(method_return_provenance("hew_hashmap_get_clone_layout")),
            "the resolved answer must be the contract of the symbol the site EMITS"
        );
    }

    /// A concrete call already carries the symbol it will emit. The placeholder
    /// path must decline it so the raw-symbol contract stays authoritative —
    /// otherwise a family verdict could override a specific one.
    #[test]
    fn a_concrete_emitted_symbol_is_not_rerouted_through_the_placeholder_path() {
        use hew_types::{MethodTargetFamily, VecMethod};
        assert_eq!(
            placeholder_method_return_provenance(
                "hew_vec_get_ptr",
                MethodTargetFamily::Vec(VecMethod::Get),
            ),
            None,
            "a resolved symbol is not a placeholder — `hew_vec_get_ptr` borrows the slot and must \
             keep reading opaque from its own row, not inherit `Get`'s family verdict"
        );
        assert_eq!(
            method_return_provenance("hew_vec_get_ptr"),
            AliasBits::OPAQUE
        );
    }

    /// A non-collection family never reaches the quantification at all.
    #[test]
    fn a_non_vec_family_placeholder_declines() {
        use hew_types::{HashSetMethod, MethodTargetFamily};
        assert_eq!(
            placeholder_method_return_provenance(
                "hew_hashset_x_FAMILY",
                MethodTargetFamily::HashSet(HashSetMethod::Contains),
            ),
            None,
            "only the two placeholder shapes this resolves are answerable here; every other \
             family falls back to its own fail-closed row"
        );
    }

    /// Fix (i) — the fresh-owner see-through. A helper that tail-returns a
    /// single-assignment `let`-bound fresh construction (directly OR through the
    /// `[..]` array-literal desugar) is proven fresh; every borrowed-alias-return
    /// shape STAYS non-fresh. This is the freshness half of the double-free crux:
    /// if any adversarial alias-return flipped to fresh, the caller-side mint
    /// (fix (ii)) would double-free it.
    #[test]
    fn see_through_flips_fresh_construction_keeps_alias_returns_closed() {
        let module = lower_source(
            r#"
            type Holder { items: Vec<string> }
            type Wrap { h: Holder }
            fn mkHolder(i: i64) -> Holder { Holder { items: [f"x{i}", f"y{i}"] } }
            fn mkLetBound(i: i64) -> Holder { let x: Holder = Holder { items: [f"x{i}"] }; x }
            fn passthrough(h: Holder) -> Holder { h }
            fn cond(a: Holder, b: Holder, c: bool) -> Holder { if c { a } else { b } }
            fn getself(w: Wrap) -> Holder { w.h }
            fn remake(h: Holder) -> Holder { var x: Holder = Holder { items: [] }; x = h; x }
            fn viaLet(h: Holder) -> Holder { let x: Holder = h; x }
            fn arrayOfParam(h: Holder) -> Vec<Holder> { [h] }
            "#,
        );
        let origin_fns = origin_fns_of(&module);
        let coarse = compute_fn_returns_fresh_owner(&origin_fns);
        // Newly proven fresh by the see-through.
        for name in ["mkHolder", "mkLetBound"] {
            assert!(
                coarse[&fn_id(&module, name)],
                "`{name}` tail-returns a fresh construction through a `let` local — must be proven fresh"
            );
        }
        // The double-free guard: every alias-return STAYS non-fresh.
        for name in [
            "passthrough",  // bare param forwarder
            "cond",         // conditional param passthrough
            "getself",      // field projection of a param
            "remake",       // `var` reassigned from a param
            "viaLet",       // `let x = h; x` re-derives the param leaf
            "arrayOfParam", // `[h]` — the see-through must union the pushed param
        ] {
            assert!(
                !coarse[&fn_id(&module, name)],
                "`{name}` may alias a by-value param — the see-through must NOT prove it fresh (double-free risk)"
            );
        }
        // The shared walk stays byte-identical to the frozen reference.
        let frozen = compute_fn_returns_fresh_owner_ref(&origin_fns);
        assert_eq!(
            coarse, frozen,
            "see-through drift between the shared walk and the frozen reference"
        );
    }

    #[test]
    fn see_through_accepts_only_audited_vec_push_methods() {
        let module = lower_source(
            r"
            fn appendThenReturn() -> Vec<i64> {
                let items: Vec<i64> = [];
                items.push(1);
                items
            }
            fn inspectThenReturn() -> Vec<i64> {
                let items: Vec<i64> = [];
                items.len();
                items
            }
            ",
        );
        let origin_fns = origin_fns_of(&module);
        let coarse = compute_fn_returns_fresh_owner(&origin_fns);
        assert!(
            coarse[&fn_id(&module, "appendThenReturn")],
            "the checker-authoritative Vec::push identity preserves freshness"
        );
        assert!(
            !coarse[&fn_id(&module, "inspectThenReturn")],
            "a non-append receiver method must remain an other-use and fail closed"
        );
        assert_eq!(
            coarse,
            compute_fn_returns_fresh_owner_ref(&origin_fns),
            "the shared walk and frozen reference must apply the same audited append identity"
        );
    }

    #[test]
    fn see_through_chained_immutable_single_moves_only() {
        let module = lower_source(
            r#"
            type Holder { items: Vec<string> }
            fn observe(h: Holder) -> i64 { h.items.len() }
            fn nestedFresh() -> Holder {
                let a = Holder { items: ["fresh"] };
                let b = a;
                b
            }
            fn nestedParam(h: Holder) -> Holder {
                let a = h;
                let b = a;
                b
            }
            fn nestedReassigned(h: Holder) -> Holder {
                var a = Holder { items: ["fresh"] };
                a = h;
                let b = a;
                b
            }
            fn nestedOtherUse() -> Holder {
                let a = Holder { items: ["fresh"] };
                observe(a);
                let b = a;
                b
            }
            fn nestedMutated() -> Holder {
                let a = Holder { items: ["fresh"] };
                a.items.push("other");
                let b = a;
                b
            }
            "#,
        );
        let origin_fns = origin_fns_of(&module);
        let coarse = compute_fn_returns_fresh_owner(&origin_fns);
        assert!(
            coarse[&fn_id(&module, "nestedFresh")],
            "a direct immutable single-move chain preserves the fresh source"
        );
        for name in [
            "nestedParam",
            "nestedReassigned",
            "nestedOtherUse",
            "nestedMutated",
        ] {
            assert!(
                !coarse[&fn_id(&module, name)],
                "`{name}` is not an immutable single-use move chain and must fail closed"
            );
        }
        assert_eq!(
            coarse,
            compute_fn_returns_fresh_owner_ref(&origin_fns),
            "the shared walk and frozen reference must agree on chained moves and every guard"
        );
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
                let out: Vec<i64> = Vec.new();
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
                        return Status.Bad;
                    },
                    false => Status.Good(input),
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
    // pin the ANALYSIS verdict; at compile time the authority now simply declines
    // to mint for such a callee (the precise diagnostic-bearing reject lands at
    // S4b).
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
            fn helper(xs: Vec<Vec<i64>>, x: i64) { xs.push(Vec.new()); }
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

#[cfg(test)]
mod extern_ownership_opacity {
    //! An extern's ownership behaviour is unknowable, so the authority answers
    //! from an explicit audited contract or not at all.
    //!
    //! This is deliberately NOT `Builder::module_fn_names`. That set carries
    //! every `HirItem::ExternFn` so extern calls lower as `Terminator::Call`
    //! rather than through the runtime-ABI path — it is a call-DISPATCH fact and
    //! says nothing about who owns a returned or passed heap handle.
    use super::*;

    const SOURCE: &str = r#"extern "C" {
    fn host_string() -> string;
    fn host_bytes() -> bytes;
    fn host_len(s: string) -> i64;
    fn host_sink(s: string);
}
fn hew_mk() -> string { "x" }
fn main() {}
"#;

    fn table() -> ExternContractTable {
        build_extern_contract_table(&tests::lower_source(SOURCE))
    }

    #[test]
    fn every_declared_extern_is_recognised_as_extern() {
        let t = table();
        for name in ["host_string", "host_bytes", "host_len", "host_sink"] {
            assert!(t.is_extern_name(name), "`{name}` must be a known extern");
        }
        assert!(
            !t.is_extern_name("hew_mk"),
            "a Hew-bodied function is not an extern"
        );
    }

    #[test]
    fn a_heap_returning_extern_is_never_an_audited_fresh_owner() {
        let t = table();
        for name in ["host_string", "host_bytes"] {
            assert!(
                !t.extern_return_is_audited_fresh_owner(name),
                "`{name}` returns a heap handle whose provenance is unknowable: \
                 the host may hand back an interior or borrowed pointer and \
                 release it itself. Minting a caller-side owner here is the \
                 second release."
            );
        }
    }

    #[test]
    fn a_scalar_returning_extern_carries_no_release_obligation() {
        assert!(
            table().extern_return_is_audited_fresh_owner("host_len"),
            "an `-> i64` return has no heap handle at all, so the audited row \
             is vacuously safe and the summary stays usable"
        );
    }

    #[test]
    fn no_extern_borrows_its_heap_arguments() {
        let t = table();
        for name in ["host_string", "host_bytes", "host_len", "host_sink"] {
            assert!(
                !t.extern_borrows_audited_heap_args(name),
                "`{name}` is a foreign host declaration with no audited row: \
                 nothing proves it borrows rather than retains or drops the \
                 handle it is passed"
            );
        }
    }

    #[test]
    fn a_root_declaration_reads_the_same_audited_argument_row_as_an_import() {
        // `hew_fs_rename` carries an all-`Borrow` audited contract. The row is a
        // fact about the SYMBOL, so it must answer the same for a declaration
        // reached as a root compilation unit as for one reached through
        // `import std::fs` — the axis that cost `std/net/net.hew` thirteen
        // `host` releases when this clause read `ExternProvenance::is_stdlib`.
        const ROOT_UNIT: &str = r#"extern "C" {
    fn hew_fs_rename(from: string, to: string) -> i64;
}
fn main() {}
"#;
        // The declaration is still checked against the audited signature: a
        // block that declares a DIFFERENT function under a classified name is
        // not a declaration of the audited callee, and says nothing about it.
        const WRONG_ARITY: &str = r#"extern "C" {
    fn hew_fs_rename(only_one: string) -> i64;
}
fn main() {}
"#;
        let t = build_extern_contract_table(&tests::lower_source(ROOT_UNIT));
        assert!(
            extern_all_params_audited_borrow("hew_fs_rename", 2),
            "guard: the audited table must actually carry an all-`Borrow` row \
             for `hew_fs_rename`, or this test proves nothing"
        );
        assert!(
            t.extern_borrows_audited_heap_args("hew_fs_rename"),
            "how the declaring file was handed to the compiler is not a property \
             of what the callee does with its arguments"
        );
        let t = build_extern_contract_table(&tests::lower_source(WRONG_ARITY));
        assert!(
            !t.extern_borrows_audited_heap_args("hew_fs_rename"),
            "an arity that disagrees with the audited signature is a different \
             function; the row must not be claimed by name alone"
        );
    }

    #[test]
    fn an_unclassified_symbol_is_not_admitted_by_its_declaration() {
        // The ARGUMENT axis keys on the same positive symbol identity the
        // RESULT axis uses. A host symbol the runtime does not classify has no
        // audited row to read, so it is ownership-opaque and the caller
        // withholds — a leak, never a double release.
        const FOREIGN: &str = r#"extern "C" {
    fn definitely_not_a_hew_runtime_symbol(s: string) -> i64;
}
fn main() {}
"#;
        let t = build_extern_contract_table(&tests::lower_source(FOREIGN));
        assert!(t.is_extern_name("definitely_not_a_hew_runtime_symbol"));
        assert!(!t.extern_borrows_audited_heap_args("definitely_not_a_hew_runtime_symbol"));
    }

    #[test]
    fn an_all_borrow_audited_signature_is_the_only_admitted_shape() {
        assert!(
            extern_all_params_audited_borrow("hew_fs_rename", 2),
            "`hew_fs_rename` is audited `params = [borrow, borrow]`: it reads \
             both paths and releases neither, so the caller keeps the sole drop \
             obligation for the temporaries it passes"
        );
        assert!(
            !extern_all_params_audited_borrow("hew_string_drop", 1),
            "the universal release consumes its argument; reading it as \
             borrowing would leave the caller a second release"
        );
        assert!(
            !extern_all_params_audited_borrow("no_such_audited_symbol", 0),
            "an unaudited symbol has no contract at all and must not be \
             admitted by absence"
        );
    }

    #[test]
    fn an_unknown_name_answers_nothing() {
        let t = table();
        assert!(!t.is_extern_name("not_declared"));
        assert!(!t.extern_return_is_audited_fresh_owner("not_declared"));
        assert!(!t.extern_borrows_audited_heap_args("not_declared"));
    }
}

#[cfg(test)]
mod measured_extern_result_transfer {
    //! Clause C — the only MINT-side admission for a pointer-bearing extern
    //! result, and the compiler half of hew-lang/hew#2828.
    //!
    //! The point of these tests is that the admission tracks the RECORDED
    //! ANSWER and nothing else. Not the name shape, not `result = "fresh"`, not
    //! the fact that the return type happens to own heap.
    use super::*;

    fn table_for(source: &str) -> ExternContractTable {
        build_extern_contract_table(&tests::lower_source(source))
    }

    fn table_for_tcp_lifecycle(source: &str, release_symbol: &str) -> ExternContractTable {
        let module = tests::lower_source_with_opaque_candidate(
            source,
            Some(hew_types::OpaqueResourceLifecycleCandidate {
                resource_declaration: hew_types::DefId::legacy_reconstruct_from_full_path(
                    "Connection",
                ),
                resource_type: "Connection".to_string(),
                owner_module: String::new(),
                close_declaration: hew_types::DefId::legacy_reconstruct_from_full_path(
                    "Connection::close",
                ),
                release_declaration: hew_types::DefId::legacy_reconstruct_from_full_path(
                    release_symbol,
                ),
                release_symbol: release_symbol.to_string(),
                release_param_index: 0,
                discharge_depth: crate::ffi_contracts::ReleaseDischargeDepth::Shallow,
                result_ownership: crate::ffi_contracts::ExternResultOwnership::Fresh,
                result_retention: crate::ffi_contracts::ExternResultRetention::ResourceTransfer,
                producer_symbols: ["hew_tcp_connect".to_string()].into_iter().collect(),
                producer_declarations: [hew_types::DefId::legacy_reconstruct_from_full_path(
                    "hew_tcp_connect",
                )]
                .into_iter()
                .collect(),
                producer_modules: ["std.net".to_string()].into_iter().collect(),
            }),
        );
        let lifecycle = module
            .type_classes
            .opaque_resource_lifecycle_for_type_name("Connection")
            .expect("lowering must admit the exact Connection lifecycle");
        assert_eq!(lifecycle.release_symbol, release_symbol);
        assert_eq!(lifecycle.close_symbol, "Connection::close");
        build_extern_contract_table(&module)
    }

    #[test]
    fn a_measured_transfer_is_minted() {
        const SOURCE: &str = r#"extern "C" {
    fn hew_process_last_error() -> string;
}
fn main() {}
"#;
        let t = table_for(SOURCE);
        assert!(
            t.extern_return_is_audited_fresh_owner("hew_process_last_error"),
            "the runtime copies its thread-local message into a fresh \
             header-aware allocation and keeps no pointer into it, so the \
             caller holds the sole owner and owes exactly one release"
        );
    }

    #[test]
    fn tcp_resource_producer_mints_only_through_its_exact_inherent_close() {
        const MATCHING_CLOSE: &str = r#"
#[resource]
#[opaque]
type Connection {}
impl Connection {
    fn close(conn: Connection) {
        unsafe { hew_tcp_close(conn) };
    }
}
extern "C" {
    fn hew_tcp_close(consume conn: Connection);
    fn hew_tcp_connect(addr: string) -> Connection;
    fn hew_tcp_unclassified(addr: string) -> Connection;
}
fn main() {}
"#;
        const WRONG_CLOSE: &str = r#"
#[resource]
#[opaque]
type Connection {}
impl Connection {
    fn close(conn: Connection) {
        unsafe { hew_tcp_listener_close(conn) };
    }
}
extern "C" {
    fn hew_tcp_listener_close(consume conn: Connection);
    fn hew_tcp_connect(addr: string) -> Connection;
}
fn main() {}
"#;

        let matching = table_for_tcp_lifecycle(MATCHING_CLOSE, "hew_tcp_close");
        assert!(
            matching.extern_return_is_audited_fresh_owner("hew_tcp_connect"),
            "the row's transferred result and `Connection::close -> hew_tcp_close` \
             must jointly mint the TCP producer"
        );
        assert!(
            !matching.extern_return_is_audited_fresh_owner("hew_tcp_unclassified"),
            "an unclassified producer has no positive row and must remain opaque"
        );

        assert!(
            !table_for_tcp_lifecycle(WRONG_CLOSE, "hew_tcp_listener_close")
                .extern_return_is_audited_fresh_owner("hew_tcp_connect"),
            "a resource marker alone is not authority: the carried lifecycle must \
             select the exact release symbol audited for this producer"
        );
    }

    #[test]
    fn deep_opaque_value_tree_mints_through_the_same_typed_lifecycle() {
        const SOURCE: &str = r#"
#[resource]
#[opaque]
type Value {}
impl Value {
    fn close(consuming self) { unsafe { hew_json_free(self) }; }
}
extern "C" {
    fn hew_json_array_new() -> Value;
    fn hew_json_free(consume value: Value);
}
fn main() {}
"#;
        let module = tests::lower_source_with_opaque_candidate(
            SOURCE,
            Some(hew_types::OpaqueResourceLifecycleCandidate {
                resource_declaration: hew_types::DefId::legacy_reconstruct_from_full_path("Value"),
                resource_type: "Value".to_string(),
                owner_module: String::new(),
                close_declaration: hew_types::DefId::legacy_reconstruct_from_full_path(
                    "Value::close",
                ),
                release_declaration: hew_types::DefId::legacy_reconstruct_from_full_path(
                    "hew_json_free",
                ),
                release_symbol: "hew_json_free".to_string(),
                release_param_index: 0,
                discharge_depth: crate::ffi_contracts::ReleaseDischargeDepth::Deep,
                result_ownership: crate::ffi_contracts::ExternResultOwnership::Fresh,
                result_retention: crate::ffi_contracts::ExternResultRetention::ResourceTransfer,
                producer_symbols: ["hew_json_array_new".to_string()].into_iter().collect(),
                producer_declarations: [hew_types::DefId::legacy_reconstruct_from_full_path(
                    "hew_json_array_new",
                )]
                .into_iter()
                .collect(),
                producer_modules: ["std.encoding.json".to_string()].into_iter().collect(),
            }),
        );
        let lifecycle = module
            .type_classes
            .opaque_resource_lifecycle_for_type_name("Value")
            .expect("lowering must admit the exact Value lifecycle");
        assert_eq!(lifecycle.release_symbol, "hew_json_free");
        assert_eq!(
            lifecycle.discharge_depth,
            crate::ffi_contracts::ReleaseDischargeDepth::Deep
        );
        let table = build_extern_contract_table(&module);
        assert!(
            table.extern_return_is_audited_fresh_owner("hew_json_array_new"),
            "deep runtime teardown is still one typed opaque close obligation"
        );
    }

    #[test]
    fn the_whole_measured_family_is_minted() {
        // Every symbol the oracles established, read back through the same
        // admission path a real declaration takes.
        const SOURCE: &str = r#"extern "C" {
    fn hew_tls_last_error() -> string;
    fn hew_smtp_last_error() -> string;
    fn hew_cron_last_error() -> string;
    fn hew_datetime_last_error() -> string;
    fn hew_json_last_error() -> string;
    fn hew_toml_last_error() -> string;
    fn hew_yaml_last_error() -> string;
    fn hew_xml_last_error() -> string;
    fn hew_msgpack_last_error() -> string;
    fn hew_http_last_error() -> string;
    fn hew_quic_last_error() -> string;
    fn hew_stream_last_error() -> string;
}
fn main() {}
"#;
        let t = table_for(SOURCE);
        for name in [
            "hew_tls_last_error",
            "hew_smtp_last_error",
            "hew_cron_last_error",
            "hew_datetime_last_error",
            "hew_json_last_error",
            "hew_toml_last_error",
            "hew_yaml_last_error",
            "hew_xml_last_error",
            "hew_msgpack_last_error",
            "hew_http_last_error",
            "hew_quic_last_error",
            "hew_stream_last_error",
        ] {
            assert!(
                t.extern_return_is_audited_fresh_owner(name),
                "`{name}` carries a measured `result-retention = \"transferred\"` \
                 row, so its result is the caller's to release"
            );
        }
    }

    #[test]
    fn measured_xml_string_results_are_minted() {
        const SOURCE: &str = r#"extern "C" {
    fn hew_xml_get_attribute(node: i64, name: string) -> string;
    fn hew_xml_get_tag(node: i64) -> string;
    fn hew_xml_get_text(node: i64) -> string;
    fn hew_xml_to_string(node: i64) -> string;
}
fn main() {}
"#;
        let table = table_for(SOURCE);
        for name in [
            "hew_xml_get_attribute",
            "hew_xml_get_tag",
            "hew_xml_get_text",
            "hew_xml_to_string",
        ] {
            assert!(
                table.extern_return_is_audited_fresh_owner(name),
                "`{name}` returns a measured transferred string whose one \
                 caller-side `hew_string_drop` balances the allocation"
            );
        }
    }

    #[test]
    fn measured_local_string_result_family_is_minted() {
        // The declarations pin the audited arity as well as the string result.
        // Parameter leaves are scalar placeholders because Clause C asks only
        // whether the returned pointer's type-directed drop agrees with the
        // audited release; the runtime R1/R2/R3 tests own producer semantics.
        const SOURCE: &str = r#"extern "C" {
    fn hew_bytes_to_string(a: i64) -> string;
    fn hew_proto_msg_get_string(a: i64, b: i64) -> string;
    fn hew_toml_last_serialize_error() -> string;
    fn hew_toml_get_string(a: i64) -> string;
    fn hew_toml_stringify(a: i64) -> string;
    fn hew_markdown_to_html(a: i64) -> string;
    fn hew_markdown_to_html_safe(a: i64) -> string;
    fn hew_log_encode_field_value(a: i64) -> string;
    fn hew_char_to_string(a: i64) -> string;
    fn hew_cidr_broadcast(a: i64) -> string;
    fn hew_cidr_network(a: i64) -> string;
    fn hew_msgpack_to_json_hew(a: i64) -> string;
    fn hew_encrypt_must_open_hew(a: i64, b: i64) -> string;
    fn hew_encrypt_try_seal_base64_hew(a: i64, b: i64) -> string;
    fn hew_encrypt_try_open_hew(a: i64, b: i64) -> string;
    fn hew_jwt_decode_hew(a: i64, b: i64, c: i64) -> string;
    fn hew_jwt_decode_insecure(a: i64) -> string;
    fn hew_jwt_encode_hew(a: i64, b: i64, c: i64) -> string;
    fn hew_password_hash(a: i64) -> string;
    fn hew_password_hash_custom(a: i64, b: i64) -> string;
    fn hew_uuid_v4() -> string;
    fn hew_uuid_v7() -> string;
    fn hew_datetime_format(a: i64, b: i64) -> string;
    fn hew_cron_to_string(a: i64) -> string;
    fn hew_url_fragment(a: i64) -> string;
    fn hew_url_host(a: i64) -> string;
    fn hew_url_path(a: i64) -> string;
    fn hew_url_query(a: i64) -> string;
    fn hew_url_scheme(a: i64) -> string;
    fn hew_url_to_string(a: i64) -> string;
    fn hew_json_get_string(a: i64) -> string;
    fn hew_json_stringify(a: i64) -> string;
    fn hew_yaml_get_string(a: i64) -> string;
    fn hew_yaml_stringify(a: i64) -> string;
    fn hew_regex_find(a: i64, b: i64) -> string;
    fn hew_regex_replace(a: i64, b: i64, c: i64) -> string;
}
fn main() {}
"#;
        let table = table_for(SOURCE);
        for name in [
            "hew_bytes_to_string",
            "hew_proto_msg_get_string",
            "hew_toml_last_serialize_error",
            "hew_toml_get_string",
            "hew_toml_stringify",
            "hew_markdown_to_html",
            "hew_markdown_to_html_safe",
            "hew_log_encode_field_value",
            "hew_char_to_string",
            "hew_cidr_broadcast",
            "hew_cidr_network",
            "hew_msgpack_to_json_hew",
            "hew_encrypt_must_open_hew",
            "hew_encrypt_try_seal_base64_hew",
            "hew_encrypt_try_open_hew",
            "hew_jwt_decode_hew",
            "hew_jwt_decode_insecure",
            "hew_jwt_encode_hew",
            "hew_password_hash",
            "hew_password_hash_custom",
            "hew_uuid_v4",
            "hew_uuid_v7",
            "hew_datetime_format",
            "hew_cron_to_string",
            "hew_url_fragment",
            "hew_url_host",
            "hew_url_path",
            "hew_url_query",
            "hew_url_scheme",
            "hew_url_to_string",
            "hew_json_get_string",
            "hew_json_stringify",
            "hew_yaml_get_string",
            "hew_yaml_stringify",
            "hew_regex_find",
            "hew_regex_replace",
        ] {
            assert!(
                table.extern_return_is_audited_fresh_owner(name),
                "`{name}` has an executable R1/R2/R3 transfer proof and must \
                 mint one caller-side string owner"
            );
        }
    }

    #[test]
    fn measured_os_io_string_transfers_are_minted_only_from_their_rows() {
        // Every name here has a direct R1/R2/R3 ownership witness: the runtime
        // exports are measured in `hew-runtime/tests/os_io_string_result_retention.rs`,
        // and DNS/compression in `hew-std/src/os_io_string_retention.rs`.
        // Parameter *types* are intentionally irrelevant to this table test;
        // C1 binds the emitted identity by exact arity, while C2/C3 require the
        // recorded measured retention and the `string` drop-plan release.
        const SOURCE: &str = r#"extern "C" {
    fn hew_args_get(arg: i64) -> string;
    fn hew_cwd() -> string;
    fn hew_env_get(name: string) -> string;
    fn hew_home_dir() -> string;
    fn hew_hostname() -> string;
    fn hew_temp_dir() -> string;
    fn hew_io_read_all() -> string;
    fn hew_io_read_line() -> string;
    fn hew_stream_collect_string(stream: i64) -> string;
    fn hew_process_result_stderr(result: i64) -> string;
    fn hew_process_result_stdout(result: i64) -> string;
    fn hew_file_read(path: string) -> string;
    fn hew_glob_error(result: i64) -> string;
    fn hew_glob_get(result: i64, index: i64) -> string;
    fn hew_path_absolute(path: string) -> string;
    fn hew_dns_lookup_host(host: string) -> string;
    fn hew_dns_lookup_host_timed(host: string, deadline_ms: i64) -> string;
    fn hew_compress_last_error() -> string;
}
fn main() {}
"#;
        let table = table_for(SOURCE);
        for name in [
            "hew_args_get",
            "hew_cwd",
            "hew_env_get",
            "hew_home_dir",
            "hew_hostname",
            "hew_temp_dir",
            "hew_io_read_all",
            "hew_io_read_line",
            "hew_stream_collect_string",
            "hew_process_result_stderr",
            "hew_process_result_stdout",
            "hew_file_read",
            "hew_glob_error",
            "hew_glob_get",
            "hew_path_absolute",
            "hew_dns_lookup_host",
            "hew_dns_lookup_host_timed",
            "hew_compress_last_error",
        ] {
            assert!(
                table.extern_return_is_audited_fresh_owner(name),
                "{name} must mint only because its contract carries a measured \
                 result-retention transfer and its `string` release is exact"
            );
        }
    }

    #[test]
    fn a_measured_record_return_is_minted_through_its_heap_field() {
        // `TlsReadFfiResult { data: bytes; status: i32 }` is not pointer-free,
        // so Clause B refuses it. Its whole discharge is one `hew_bytes_drop`,
        // which is exactly what the audited row names, and the retention answer
        // is measured — so Clause C admits it, and the `Result<bytes, ..>` that
        // `tls.read` builds from it regains its scrutinee release.
        const SOURCE: &str = r#"type TlsReadFfiResult {
    data: bytes;
    status: i32;
}
extern "C" {
    fn hew_tls_read_result(stream: i64, size: i32) -> TlsReadFfiResult;
}
fn main() {}
"#;
        assert!(
            table_for(SOURCE).extern_return_is_audited_fresh_owner("hew_tls_read_result"),
            "a record whose only heap field discharges through the audited \
             release symbol is the caller's to release"
        );
    }

    #[test]
    fn string_to_bytes_mints_only_after_measured_retention_transfer() {
        // `hew_string_to_bytes` is audited `result = "fresh"`, released by
        // `hew_bytes_drop`, shallow. Its dedicated runtime retention oracle
        // proves that source and result storage are independent and balanced
        // in both release orders, so the contract now carries the additional
        // `transferred` axis Clause C requires.
        const SOURCE: &str = r#"extern "C" {
    fn hew_string_to_bytes(input: string) -> bytes;
}
fn main() {}
"#;
        let contract = crate::ffi_contracts::extern_ownership_contract("hew_string_to_bytes")
            .contract()
            .expect("guard: hew_string_to_bytes must be audited, or this proves nothing");
        assert_eq!(
            contract.result,
            crate::ffi_contracts::ExternResultOwnership::Fresh
        );
        assert_eq!(contract.release_symbol, BYTES_RELEASE_SYMBOL);
        assert_eq!(
            contract.result_retention,
            crate::ffi_contracts::ExternResultRetention::Transferred
        );
        assert!(
            table_for(SOURCE).extern_return_is_audited_fresh_owner("hew_string_to_bytes"),
            "the exact fresh/transfer/release contract must mint the returned bytes; \
             caller-side produced-value registration remains a separate authority"
        );
    }

    #[test]
    fn string_clone_mints_one_release_for_its_retained_share() {
        const SOURCE: &str = r#"extern "C" {
    fn hew_string_clone(input: string) -> string;
}
fn main() {}
"#;
        let contract = crate::ffi_contracts::extern_ownership_contract("hew_string_clone")
            .contract()
            .expect("guard: hew_string_clone must carry an audited contract");
        assert_eq!(
            contract.result,
            crate::ffi_contracts::ExternResultOwnership::Retained
        );
        assert_eq!(
            contract.result_retention,
            crate::ffi_contracts::ExternResultRetention::SharedRefcount
        );
        assert!(
            table_for(SOURCE).extern_return_is_audited_fresh_owner("hew_string_clone"),
            "the retained share must mint exactly the release balanced by its refcount bump"
        );
    }

    #[test]
    fn an_unmeasured_fresh_or_retained_heap_result_is_refused() {
        let (symbol, contract) = crate::ffi_contracts::FFI_OWNERSHIP_CONTRACTS
            .iter()
            .find(|(_, contract)| {
                matches!(
                    contract.result,
                    crate::ffi_contracts::ExternResultOwnership::Fresh
                        | crate::ffi_contracts::ExternResultOwnership::Retained
                ) && contract.result_retention
                    == crate::ffi_contracts::ExternResultRetention::Unspecified
                    && contract.discharge_depth
                        == crate::ffi_contracts::ReleaseDischargeDepth::Shallow
                    && matches!(
                        contract.release_symbol,
                        STRING_RELEASE_SYMBOL | BYTES_RELEASE_SYMBOL
                    )
            })
            .expect("the contract corpus must retain an unmeasured owned heap result");
        let return_ty = if contract.release_symbol == STRING_RELEASE_SYMBOL {
            "string"
        } else {
            "bytes"
        };
        let params = (0..contract.params.len())
            .map(|index| format!("p{index}: i64"))
            .collect::<Vec<_>>()
            .join(", ");
        let source =
            format!("extern \"C\" {{ fn {symbol}({params}) -> {return_ty}; }}\nfn main() {{}}\n");

        assert!(
            !table_for(&source).extern_return_is_audited_fresh_owner(symbol),
            "`fresh`/`retained` and an exact release do not prove that the \
             foreign callee stopped referring to its result; absent measured \
             retention must stay fail-closed for `{symbol}`"
        );
    }

    #[test]
    fn the_borrowing_member_of_the_family_is_refused() {
        // `hew_last_error` shares the family's name shape and its C-string
        // return, and hands back the interior of a `CString` the runtime keeps.
        // The recorded answer is `borrowed`, which fails Clause A before the
        // retention axis is even consulted.
        const SOURCE: &str = r#"extern "C" {
    fn hew_last_error() -> string;
}
fn main() {}
"#;
        assert!(
            !table_for(SOURCE).extern_return_is_audited_fresh_owner("hew_last_error"),
            "the family is not uniform, and the authority must read the answer \
             per symbol rather than generalise from the shared prefix"
        );
    }

    #[test]
    fn a_release_the_drop_plan_would_not_emit_is_refused() {
        // Clause C3. `hew_json_array_new` is audited `fresh`, but its release is
        // `hew_json_free` at `deep` depth. Declaring it `-> string` would have
        // the drop plan emit `hew_string_drop` against an allocation that
        // release does not balance, so the audited release must be exactly the
        // one the declared type's drop plan emits.
        const SOURCE: &str = r#"extern "C" {
    fn hew_json_array_new() -> string;
}
fn main() {}
"#;
        let contract = crate::ffi_contracts::extern_ownership_contract("hew_json_array_new")
            .contract()
            .expect("guard: hew_json_array_new must be audited, or this proves nothing");
        assert_eq!(
            contract.result,
            crate::ffi_contracts::ExternResultOwnership::Fresh
        );
        assert_ne!(contract.release_symbol, STRING_RELEASE_SYMBOL);
        assert!(
            !table_for(SOURCE).extern_return_is_audited_fresh_owner("hew_json_array_new"),
            "an audited release the type-directed drop plan will not emit \
             licenses nothing"
        );
    }

    #[test]
    fn an_arity_that_disagrees_with_the_audited_signature_is_refused() {
        // The row is a fact about the audited callee. A block declaring a
        // different function under the same name is not a declaration of it.
        const SOURCE: &str = r#"extern "C" {
    fn hew_process_last_error(unexpected: i64) -> string;
}
fn main() {}
"#;
        assert!(
            !table_for(SOURCE).extern_return_is_audited_fresh_owner("hew_process_last_error"),
            "a measured answer about a zero-argument callee says nothing about \
             a one-argument declaration wearing its name"
        );
    }

    #[test]
    fn an_unclassified_host_symbol_is_refused() {
        const SOURCE: &str = r#"extern "C" {
    fn host_last_error() -> string;
}
fn main() {}
"#;
        assert!(
            !table_for(SOURCE).extern_return_is_audited_fresh_owner("host_last_error"),
            "a foreign host symbol has no measured row, so its result stays \
             ownership-opaque"
        );
    }

    #[test]
    fn a_declared_return_resolves_to_the_release_the_plan_emits() {
        // The C3 derivation, exercised directly so the refusals above are
        // pinned to a stated rule rather than to whatever the table happens to
        // hold. An `#[opaque]` handle, a generic instantiation and a record
        // mixing two release symbols are all `Unresolved`, which Clause C
        // refuses.
        const SOURCE: &str = r"type Pod {
    a: i32;
    b: i64;
}
type OneHeap {
    data: bytes;
    status: i32;
}
type MixedHeap {
    text: string;
    data: bytes;
}
#[opaque]
type Handle {
}
fn main() {}
";
        let module = tests::lower_source(SOURCE);
        let decls: HashMap<&str, &hew_hir::HirTypeDecl> = module
            .items
            .iter()
            .filter_map(|item| match item {
                hew_hir::HirItem::TypeDecl(decl) => Some((decl.declaration.full_path(), decl)),
                _ => None,
            })
            .collect();
        let named = |name: &str| ResolvedTy::Named {
            name: name.to_string(),
            args: vec![],
            is_opaque: false,
            builtin: None,
        };

        assert_eq!(
            declared_return_release(&ResolvedTy::String, &decls, 0),
            ReturnRelease::One("hew_string_drop")
        );
        assert_eq!(
            declared_return_release(&ResolvedTy::Bytes, &decls, 0),
            ReturnRelease::One("hew_bytes_drop")
        );
        assert_eq!(
            declared_return_release(&ResolvedTy::I64, &decls, 0),
            ReturnRelease::Nothing
        );
        assert_eq!(
            declared_return_release(&named("Pod"), &decls, 0),
            ReturnRelease::Nothing,
            "a record of scalar leaves releases nothing — Clause B's business, \
             and never a licence under Clause C"
        );
        assert_eq!(
            declared_return_release(&named("OneHeap"), &decls, 0),
            ReturnRelease::One("hew_bytes_drop")
        );
        assert_eq!(
            declared_return_release(&named("MixedHeap"), &decls, 0),
            ReturnRelease::Unresolved,
            "a contract names ONE release symbol, so it cannot be the balancing \
             release for a value that needs two"
        );
        assert_eq!(
            declared_return_release(&named("Handle"), &decls, 0),
            ReturnRelease::Unresolved,
            "an `#[opaque]` declaration is the foreign-handle shape this \
             authority exists to refuse"
        );
        assert_eq!(
            declared_return_release(&named("NotDeclaredHere"), &decls, 0),
            ReturnRelease::Unresolved
        );
    }

    #[test]
    fn nominal_release_and_pointer_free_proofs_do_not_cross_same_leaf_owners() {
        let declared = DeclaredReleaseTypes {
            names: HashSet::from(["left.Handle".to_string(), "Handle".to_string()]),
        };
        assert!(declared.release_is_declared("left.Handle"));
        assert!(
            !declared.release_is_declared("right.Handle"),
            "a declared close is an exact nominal authority"
        );

        let pointer_free = HashSet::from(["left.Pod".to_string(), "Pod".to_string()]);
        let named = |name: &str| ResolvedTy::Named {
            name: name.to_string(),
            args: Vec::new(),
            builtin: None,
            is_opaque: false,
        };
        assert!(field_ty_is_pointer_free(&named("left.Pod"), &pointer_free));
        assert!(
            !field_ty_is_pointer_free(&named("right.Pod"), &pointer_free),
            "a same-leaf foreign record cannot inherit pointer-free proof"
        );
    }

    #[test]
    fn semantic_nominal_sinks_have_no_short_name_fallbacks() {
        let source = include_str!("return_provenance.rs");
        for prefix in [
            "names.insert(hew_types::",
            "contains(hew_types::",
            ".or_else(|| decls.get(hew_types::",
            "opaque_handles.contains(hew_types::",
        ] {
            let forbidden = format!("{prefix}short_name");
            assert!(
                !source.contains(&forbidden),
                "semantic ownership authority must remain exact-owner keyed: {forbidden}"
            );
        }
    }
}
