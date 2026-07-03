//! Checker-authored dispatch substrate (W4.001 Stage A — foundation).
//!
//! This module introduces the data shapes that will, in subsequent stages,
//! become the **single source of truth** for "how does this method call
//! lower." See the type-level docs on [`ResolvedCall`], [`ImplRegistry`],
//! and [`resolve_method_call`] below for substrate ownership.
//!
//! # Stage A scope
//!
//! Stage A is substrate-only:
//!
//! - Define [`ResolvedCall`], [`ImplId`], [`ImplRegistry`], [`ImplDef`],
//!   [`MethodTarget`], [`CallAbiHint`], [`RuntimeAbi`], [`Bound`],
//!   [`TyPattern`], and [`LookupError`].
//! - Provide [`resolve_method_call`] as a fail-closed resolver that returns
//!   either a [`ResolvedCall`] or a structured [`LookupError`] — never a
//!   silent `None`.
//! - Add a parallel `resolved_calls: HashMap<SpanKey, ResolvedCall>` field
//!   on `TypeCheckOutput` (and the checker's internal state) initialised to
//!   the empty map.
//!
//! Stage A **does not** introduce any production reader of
//! `resolved_calls`. The field is populated only by the
//! `resolved_call_registry_lookup` substrate test and (in Stage B) by the
//! checker's unified resolver. No HIR/MIR/codegen code path reads it. If a
//! reader is accidentally introduced via a defaulted match arm, the
//! reviewer should rewrite it as
//! `unreachable!("hew-types::dispatch invariant: resolved_calls is \
//! populated by the unified resolver; no production reader exists at \
//! this stage of the implementation — adding one before the resolver lands \
//! violates DI-003 fail-closed-by-absence")` rather than silently fall
//! through.
//!
//! # Q281=A data-only discipline
//!
//! [`ImplDef`], [`TyPattern`], [`Bound`], and [`MethodTarget`] are **pure
//! data**: no Rust closures, no `Box<dyn Fn>`, no predicate callbacks.
//! Bound satisfaction is decided by a caller-supplied predicate that lives
//! at the resolver's call boundary (see [`resolve_method_call`]'s
//! `bound_satisfied` argument), **never** by a closure stored inside the
//! registry.
//!
//! This is the non-negotiable invariant that keeps the eventual
//! `.hew`-source migration of the registry mechanical (the entire point
//! of Q281=A). It is enforced structurally by the `Serialize +
//! Deserialize` derives on every type below: a `Box<dyn Fn(...)>` cannot
//! implement `Serialize`, so adding one trips the build, and the
//! `resolved_call_registry_lookup` round-trip test demonstrates the
//! shape is genuinely serialisable.

use crate::traits::MarkerTrait;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Opaque, checker-allocated identity of one impl in the registry.
///
/// Numeric (not string-keyed) per `§7 risk #11` of the design notes:
/// string-tuple impl identity is a known fragility class (DI-001).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ImplId(pub u32);

/// The runtime calling convention for the receiver-side argument of a
/// dispatched method.
///
/// Stage A enumerates the conventions used by existing HashMap/HashSet/Vec
/// callees so Stage E can re-key MIR drop-plan attachment from string-symbol
/// families onto `ImplId` without changing the set of conventions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RuntimeAbi {
    /// Receiver is passed by value (e.g. `Copy` primitives).
    ByValue,
    /// Receiver is passed by shared reference (read-only method).
    ByRef,
    /// Receiver is passed by mutable reference (in-place mutation).
    ByRefMut,
    /// Receiver ownership is transferred into the callee; the caller's
    /// drop slot must be nulled after the call. Aligns with
    /// `MethodSig::consumes_receiver` / `method_call_consumes_receiver`.
    ConsumeReceiver,
}

/// How the call is lowered at MIR/codegen time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CallAbiHint {
    /// Direct call to the symbol named by [`MethodTarget::symbol_name`].
    Direct,
    /// Call goes through a generated runtime shim (e.g. `HashMap` layout
    /// dispatch). The shim's symbol is still named by `symbol_name`; this
    /// variant just records that a shim sits in front of the real impl.
    RuntimeShim,
}

/// A data-only type pattern. Independent of [`crate::ty::Ty`] so it
/// round-trips via serde and stays mechanically migratable to `.hew`
/// source.
///
/// The resolver's caller is responsible for translating a `Ty` into a
/// `TyPattern` for matching, and for evaluating bound-satisfaction on
/// concrete `TyPattern`s. Storing `Ty` directly inside `ImplDef` is
/// rejected because `Ty` is checker-internal and not serialisable.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TyPattern {
    /// Pattern variable bound to whatever concrete pattern appears at this
    /// position (e.g. the `K` in `HashMap<K, V>`). Variable names are local
    /// to a single [`ImplDef`].
    Var(String),
    /// Match a concrete primitive named exactly (e.g. `"i64"`, `"bool"`,
    /// `"String"`). Names follow [`MarkerTrait::Display`]-style canonical
    /// spellings (lowercase for primitives, capitalised for `String`).
    Primitive(String),
    /// Match a named generic constructor: `HashMap<K, V>` →
    /// `App { ctor: "HashMap", args: [Var("K"), Var("V")] }`.
    App { ctor: String, args: Vec<TyPattern> },
    /// Tuple pattern.
    Tuple(Vec<TyPattern>),
}

impl std::fmt::Display for TyPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(name) | Self::Primitive(name) => f.write_str(name),
            Self::App { ctor, args } => {
                write!(f, "{ctor}<")?;
                for (idx, arg) in args.iter().enumerate() {
                    if idx > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                f.write_str(">")
            }
            Self::Tuple(items) => {
                f.write_str("(")?;
                for (idx, item) in items.iter().enumerate() {
                    if idx > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{item}")?;
                }
                if items.len() == 1 {
                    f.write_str(",")?;
                }
                f.write_str(")")
            }
        }
    }
}

/// A where-clause bound: `Trait` applied to one local pattern variable.
///
/// `var` MUST name a [`TyPattern::Var`] that appears somewhere in the
/// enclosing [`ImplDef::self_pattern`]. The resolver evaluates each bound
/// against the concrete pattern bound to that variable at match time.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Bound {
    pub trait_name: MarkerTrait,
    pub var: String,
}

/// Per-method dispatch target: the runtime symbol plus its calling
/// convention and lifecycle facts.
///
/// Stage A captures the fields Stage B's resolver will populate and Stage
/// E's MIR/codegen pickup will consume. No production reader exists yet.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct MethodTarget {
    /// Runtime symbol name (e.g. `"hew_hashmap_insert_layout"`).
    ///
    /// Retained as the concrete linker-edge identifier even after the
    /// typed [`MethodTargetFamily`] sibling lands: codegen ultimately
    /// resolves a callee by C-symbol string. Consumers performing
    /// *dispatch decisions* (which collection? which method?) must read
    /// [`MethodTarget::family`] instead — the string is then only used
    /// as the literal callee name for `Terminator::Call`. A later slice
    /// retires the string for fully-typed runtime calls.
    pub symbol_name: String,
    /// Typed dispatch family — the *closed-set* identity of this method
    /// target. Consumers that need to ask "is this a `HashMap` insert?"
    /// or "is this any Vec push?" must read this field; do NOT match on
    /// `symbol_name.starts_with("hew_hashmap_")` or peer prefix shapes.
    /// The family is the dispatch authority; the string is the linker
    /// authority. They are siblings and must not drift — populator and
    /// MIR consumer share enum coverage so a new collection method
    /// fails to compile in the consumer until both sides are updated.
    pub family: MethodTargetFamily,
    /// Receiver-side calling convention.
    pub abi: RuntimeAbi,
    /// How the call is lowered (direct vs runtime shim).
    pub call_hint: CallAbiHint,
    /// Whether the method consumes its receiver. Mirrors
    /// `MethodSig::consumes_receiver` so Stage E can fold the
    /// `method_call_consumes_receiver` side table onto [`ResolvedCall`]
    /// without semantic drift.
    pub consumes_receiver: bool,
}

/// Typed dispatch family for [`MethodTarget`].
///
/// This is the *dispatch* authority — consumers (HIR/MIR/codegen) that
/// need to discriminate between collection kinds or specific methods
/// must read this enum rather than re-parsing the `symbol_name` string.
///
/// The substrate covers the closed set the checker's
/// `collection_dispatch_registry_impl` populates today: `HashMap`,
/// `HashSet`, and Vec. Adding a new collection family requires extending
/// this enum AND every consumer match — `match`'s exhaustiveness check
/// is the invariant. New Vec methods extend [`VecMethod`] the same way.
///
/// User-impl / open-set collection calls are not handled here: those flow
/// through `MethodCallRewrite::RewriteToFunction` (where `descriptor`
/// is `None` and the user trait `Type::method` key is the only
/// identifier). This enum is for the runtime-known builtin generics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MethodTargetFamily {
    /// `HashMap` method dispatch. Arity invariant: 2 type-args (K, V).
    HashMap(HashMapMethod),
    /// `HashSet` method dispatch. Arity invariant: 1 type-arg (T).
    HashSet(HashSetMethod),
    /// Vec method dispatch. Arity invariant: 1 type-arg (T).
    Vec(VecMethod),
}

/// `HashMap` dispatch methods. Mirrors the methods registered for the
/// `Map for HashMap<K, V>` impl in
/// `collection_dispatch_registry_impl`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HashMapMethod {
    Insert,
    Get,
    ContainsKey,
    Remove,
    Len,
    Keys,
    Values,
    Clone,
    Clear,
}

/// `HashSet` dispatch methods. Mirrors the methods registered for the
/// `Set for HashSet<T>` impl in `collection_dispatch_registry_impl`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HashSetMethod {
    Insert,
    Contains,
    Remove,
    Len,
    IsEmpty,
    Clone,
    /// Snapshot all elements into an owned `Vec<T>` (the `for x in s` source).
    ToVec,
    Clear,
}

/// Vec dispatch methods. Mirrors the methods registered for the
/// `Seq for Vec<T>` impl in `collection_dispatch_registry_impl`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum VecMethod {
    Push,
    Pop,
    Len,
    Get,
    Set,
    Remove,
    Contains,
    IsEmpty,
    Clear,
    Clone,
    Append,
    Join,
}

/// One impl declaration in the registry, e.g.
/// `impl<K, V> Map for HashMap<K, V> where K: Hash + Eq`.
///
/// **Data-only.** No Rust callbacks. Every field round-trips via serde.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ImplDef {
    /// The trait this impl implements (e.g. `"Map"`, `"Set"`, `"Vec"`).
    pub trait_name: String,
    /// The receiver-type pattern this impl matches.
    pub self_pattern: TyPattern,
    /// `where`-clause bounds the receiver's type-args must satisfy.
    pub where_bounds: Vec<Bound>,
    /// Methods this impl provides. Stored as a vector (not a map) so the
    /// declared order is preserved across serde round-trips, which keeps
    /// the data format stable for the eventual `.hew`-source migration.
    pub methods: Vec<(String, MethodTarget)>,
}

/// The substrate's unified verdict for one method call site.
///
/// In Stage B the checker will populate
/// `TypeCheckOutput::resolved_calls: HashMap<SpanKey, ResolvedCall>` from
/// the resolver. In Stage C, HIR lowering will consult it as the dispatch
/// authority for HashMap/HashSet. Stage A defines the shape only.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ResolvedCall {
    /// Identity of the impl that satisfied this call.
    pub impl_id: ImplId,
    /// Method name on that impl (must appear in [`ImplDef::methods`]).
    pub method_name: String,
    /// Concrete type-arguments resolved at the call site, in
    /// first-occurrence order of [`TyPattern::Var`]s in
    /// [`ImplDef::self_pattern`]. Carried as [`TyPattern`] (not [`Ty`])
    /// for the same data-only / serde reasons as [`ImplDef`].
    pub type_args: Vec<TyPattern>,
    /// The resolved target. Cloned from [`ImplDef::methods`] at resolve
    /// time so downstream consumers do not have to re-traverse the
    /// registry.
    pub target: MethodTarget,
}

/// Structured failure verdict from [`resolve_method_call`].
///
/// **No silent `None` swallowing.** Stage A's resolver always commits to
/// one of: a `ResolvedCall`, or a `LookupError` that names exactly what
/// failed. Stage B and C consumers fail closed on a missing
/// `resolved_calls` entry (DI-003 fail-closed-by-absence).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LookupError {
    /// No impl in the registry matched `(trait_name, receiver shape)`.
    NoImpl {
        trait_name: String,
        receiver: TyPattern,
        method: String,
    },
    /// An impl matched structurally but one or more `where_bounds` was
    /// not satisfied by the receiver's concrete type-args.
    BoundsNotSatisfied {
        impl_id: ImplId,
        unsatisfied: Vec<Bound>,
        /// The concrete pattern bound to the first unsatisfied bound's
        /// variable, captured for diagnostic attribution
        /// (e.g. "`f64` does not implement `Hash`").
        witness: TyPattern,
    },
    /// An impl matched and bounds were satisfied, but the requested
    /// method name was not in [`ImplDef::methods`].
    UnknownMethod { impl_id: ImplId, method: String },
}

impl std::fmt::Display for LookupError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoImpl {
                trait_name,
                receiver,
                method,
            } => write!(
                f,
                "no impl found: trait `{trait_name}` / method `{method}` / receiver `{receiver:?}`"
            ),
            Self::BoundsNotSatisfied {
                impl_id,
                unsatisfied,
                witness,
            } => {
                let bounds: Vec<String> = unsatisfied
                    .iter()
                    .map(|b| format!("{}: {}", b.var, b.trait_name))
                    .collect();
                write!(
                    f,
                    "bounds not satisfied on impl {impl_id:?}: [{}] (witness: {witness:?})",
                    bounds.join(", ")
                )
            }
            Self::UnknownMethod { impl_id, method } => {
                write!(f, "method `{method}` not in impl {impl_id:?}")
            }
        }
    }
}

/// The checker-authored impl registry: many [`ImplDef`]s keyed by
/// [`ImplId`].
///
/// Stage A seeds this in tests only; Stage B populates it from the
/// checker's builtin-trait registration sites. The registry stays
/// data-only and is the input to [`resolve_method_call`].
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ImplRegistry {
    impls: HashMap<ImplId, ImplDef>,
    next_id: u32,
}

/// Render structured bound diagnostics from a resolver failure without
/// exposing them through the checker yet.
///
/// Currently exercised only by tests; production readers will land alongside
/// the HIR consumer that replaces the legacy HashMap/HashSet allowlists.
#[must_use]
pub fn bound_diagnostic_strings(error: &LookupError) -> Vec<String> {
    match error {
        LookupError::BoundsNotSatisfied {
            unsatisfied,
            witness,
            ..
        } => unsatisfied
            .iter()
            .map(|bound| format!("{witness} does not implement {}", bound.trait_name))
            .collect(),
        LookupError::NoImpl {
            trait_name,
            receiver,
            method,
        } => vec![format!(
            "no impl found for `{trait_name}::{method}` on `{receiver}`"
        )],
        LookupError::UnknownMethod { impl_id, method } => {
            vec![format!("impl {impl_id:?} has no method `{method}`")]
        }
    }
}

impl ImplRegistry {
    /// Construct an empty registry.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Register an [`ImplDef`] and return its freshly-allocated [`ImplId`].
    ///
    /// # Panics
    ///
    /// Panics if the registry exhausts the `u32` impl-id space (more than
    /// `u32::MAX` impls registered into a single `ImplRegistry`). This is
    /// not a recoverable condition: every builtin trait impl is seeded
    /// once at checker construction, so reaching the limit is a
    /// structural bug, not a runtime overflow.
    pub fn register(&mut self, def: ImplDef) -> ImplId {
        let id = ImplId(self.next_id);
        self.next_id = self
            .next_id
            .checked_add(1)
            .expect("ImplRegistry exhausted u32 impl-id space");
        let prev = self.impls.insert(id, def);
        debug_assert!(
            prev.is_none(),
            "ImplRegistry: freshly-allocated ImplId {id:?} collided with an existing entry"
        );
        id
    }

    /// Look up the [`ImplDef`] for an [`ImplId`].
    #[must_use]
    pub fn get(&self, id: ImplId) -> Option<&ImplDef> {
        self.impls.get(&id)
    }

    /// Iterate `(id, def)` pairs in unspecified order.
    pub fn iter(&self) -> impl Iterator<Item = (ImplId, &ImplDef)> + '_ {
        self.impls.iter().map(|(k, v)| (*k, v))
    }

    /// Number of registered impls.
    #[must_use]
    pub fn len(&self) -> usize {
        self.impls.len()
    }

    /// Whether the registry is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.impls.is_empty()
    }
}

/// Substitution from local [`TyPattern::Var`] names to concrete
/// [`TyPattern`]s, produced by [`match_pattern`].
pub type PatternSubst = HashMap<String, TyPattern>;

/// Structurally match `pat` against `concrete`. Returns the substitution
/// from pattern-variable names to concrete sub-patterns on success, or
/// `None` on mismatch.
///
/// Repeated occurrences of the same variable name must bind to the same
/// concrete pattern (linear pattern semantics).
#[must_use]
pub fn match_pattern(pat: &TyPattern, concrete: &TyPattern) -> Option<PatternSubst> {
    let mut out = HashMap::new();
    if match_into(pat, concrete, &mut out) {
        Some(out)
    } else {
        None
    }
}

fn match_into(pat: &TyPattern, concrete: &TyPattern, out: &mut PatternSubst) -> bool {
    match (pat, concrete) {
        (TyPattern::Var(name), c) => {
            if let Some(prev) = out.get(name) {
                prev == c
            } else {
                out.insert(name.clone(), c.clone());
                true
            }
        }
        (TyPattern::Primitive(a), TyPattern::Primitive(b)) => a == b,
        (TyPattern::App { ctor: a, args: aa }, TyPattern::App { ctor: b, args: bb })
            if a == b && aa.len() == bb.len() =>
        {
            aa.iter().zip(bb.iter()).all(|(p, c)| match_into(p, c, out))
        }
        (TyPattern::Tuple(a), TyPattern::Tuple(b)) if a.len() == b.len() => {
            a.iter().zip(b.iter()).all(|(p, c)| match_into(p, c, out))
        }
        _ => false,
    }
}

/// Resolve one method call against `registry`.
///
/// The caller supplies:
///
/// - `trait_name` / `method`: dispatch target identity.
/// - `receiver`: the receiver type expressed as a [`TyPattern`].
/// - `bound_satisfied`: predicate deciding whether a concrete
///   [`TyPattern`] satisfies a [`MarkerTrait`]. **This predicate lives at
///   the call boundary, not inside the registry.** Q281=A data-only
///   discipline is preserved by passing the predicate as a parameter
///   instead of storing it on [`ImplDef`].
///
/// Returns the [`ResolvedCall`] on success, or a structured
/// [`LookupError`] explaining exactly what failed. Fail-closed by
/// construction: no `Option<ResolvedCall>` `.ok()?` style silent
/// swallowing is possible.
///
/// # Errors
///
/// Returns [`LookupError::NoImpl`] when no impl matches `(trait_name,
/// receiver)` structurally; [`LookupError::BoundsNotSatisfied`] when an
/// impl matches but one or more `where_bounds` are rejected by
/// `bound_satisfied`; [`LookupError::UnknownMethod`] when an impl matches
/// and bounds are satisfied but `method` is absent from its method list.
///
/// # Panics
///
/// Panics if the registry is malformed — specifically, if any
/// [`Bound::var`] names a pattern variable that the corresponding
/// [`ImplDef::self_pattern`] does not introduce. This is a structural
/// invariant the registry-seeding code is required to uphold; refusing
/// to invent a default witness preserves DI-019 (checker authority
/// without defensive guards).
///
/// # Resolution order
///
/// 1. Scan registry for impls matching `(trait_name, self_pattern)`.
/// 2. Among matching impls, evaluate every `where_bound` via
///    `bound_satisfied`. The first impl whose bounds are all satisfied
///    and whose `methods` contains `method` wins.
/// 3. If no impl satisfies all bounds, return
///    [`LookupError::BoundsNotSatisfied`] naming the last partial-match
///    seen (deterministic enough for diagnostics; Stage B replaces the
///    iteration order with a stable insertion order when the registry
///    is seeded from builtin-trait registration).
/// 4. If no impl matches structurally at all,
///    [`LookupError::NoImpl`].
/// 5. If the impl matches but lacks `method`,
///    [`LookupError::UnknownMethod`].
pub fn resolve_method_call(
    registry: &ImplRegistry,
    trait_name: &str,
    method: &str,
    receiver: &TyPattern,
    bound_satisfied: &dyn Fn(MarkerTrait, &TyPattern) -> bool,
) -> Result<ResolvedCall, LookupError> {
    let mut last_partial: Option<(ImplId, Vec<Bound>, TyPattern)> = None;
    for (impl_id, def) in registry.iter() {
        if def.trait_name != trait_name {
            continue;
        }
        let Some(subst) = match_pattern(&def.self_pattern, receiver) else {
            continue;
        };
        let mut unsatisfied = Vec::new();
        for bound in &def.where_bounds {
            let witness = subst.get(&bound.var).unwrap_or_else(|| {
                // The registry is malformed: a where-bound names a var
                // not introduced by self_pattern. Fail closed —
                // refusing to invent a default witness is the whole
                // point of DI-019.
                panic!(
                    "ImplRegistry malformed: where-bound var {:?} not introduced by \
                     self_pattern of impl {:?}",
                    bound.var, impl_id
                )
            });
            if !bound_satisfied(bound.trait_name, witness) {
                unsatisfied.push(bound.clone());
            }
        }
        if !unsatisfied.is_empty() {
            let witness = subst
                .get(&unsatisfied[0].var)
                .cloned()
                .expect("witness must exist; presence checked above");
            last_partial = Some((impl_id, unsatisfied, witness));
            continue;
        }
        let target = def
            .methods
            .iter()
            .find_map(|(name, t)| (name == method).then(|| t.clone()));
        let Some(target) = target else {
            return Err(LookupError::UnknownMethod {
                impl_id,
                method: method.to_string(),
            });
        };
        let type_args = collect_type_args(&def.self_pattern, &subst);
        return Ok(ResolvedCall {
            impl_id,
            method_name: method.to_string(),
            type_args,
            target,
        });
    }
    if let Some((impl_id, unsatisfied, witness)) = last_partial {
        Err(LookupError::BoundsNotSatisfied {
            impl_id,
            unsatisfied,
            witness,
        })
    } else {
        Err(LookupError::NoImpl {
            trait_name: trait_name.to_string(),
            receiver: receiver.clone(),
            method: method.to_string(),
        })
    }
}

/// Project `subst` onto the first-occurrence order of [`TyPattern::Var`]s
/// in `pat`, deduplicated. Used to fill [`ResolvedCall::type_args`].
fn collect_type_args(pat: &TyPattern, subst: &PatternSubst) -> Vec<TyPattern> {
    let mut order: Vec<String> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    collect_vars(pat, &mut order, &mut seen);
    order
        .into_iter()
        .filter_map(|n| subst.get(&n).cloned())
        .collect()
}

fn collect_vars(pat: &TyPattern, order: &mut Vec<String>, seen: &mut HashSet<String>) {
    match pat {
        TyPattern::Var(n) => {
            if seen.insert(n.clone()) {
                order.push(n.clone());
            }
        }
        TyPattern::Primitive(_) => {}
        TyPattern::App { args, .. } | TyPattern::Tuple(args) => {
            for a in args {
                collect_vars(a, order, seen);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn always_true(_: MarkerTrait, _: &TyPattern) -> bool {
        true
    }

    #[test]
    fn register_allocates_distinct_ids() {
        let mut r = ImplRegistry::new();
        let a = r.register(ImplDef {
            trait_name: "T".into(),
            self_pattern: TyPattern::Primitive("i64".into()),
            where_bounds: vec![],
            methods: vec![],
        });
        let b = r.register(ImplDef {
            trait_name: "T".into(),
            self_pattern: TyPattern::Primitive("i32".into()),
            where_bounds: vec![],
            methods: vec![],
        });
        assert_ne!(a, b);
        assert_eq!(r.len(), 2);
    }

    #[test]
    fn match_pattern_binds_vars() {
        let pat = TyPattern::App {
            ctor: "HashMap".into(),
            args: vec![TyPattern::Var("K".into()), TyPattern::Var("V".into())],
        };
        let concrete = TyPattern::App {
            ctor: "HashMap".into(),
            args: vec![
                TyPattern::Primitive("String".into()),
                TyPattern::Primitive("i64".into()),
            ],
        };
        let subst = match_pattern(&pat, &concrete).expect("match should succeed");
        assert_eq!(subst.get("K"), Some(&TyPattern::Primitive("String".into())));
        assert_eq!(subst.get("V"), Some(&TyPattern::Primitive("i64".into())));
    }

    #[test]
    fn match_pattern_rejects_mismatched_ctor() {
        let pat = TyPattern::App {
            ctor: "HashMap".into(),
            args: vec![TyPattern::Var("K".into())],
        };
        let concrete = TyPattern::App {
            ctor: "HashSet".into(),
            args: vec![TyPattern::Primitive("i64".into())],
        };
        assert!(match_pattern(&pat, &concrete).is_none());
    }

    #[test]
    fn match_pattern_enforces_linearity() {
        // `Pair<X, X>` should not match `Pair<i32, i64>`.
        let pat = TyPattern::App {
            ctor: "Pair".into(),
            args: vec![TyPattern::Var("X".into()), TyPattern::Var("X".into())],
        };
        let bad = TyPattern::App {
            ctor: "Pair".into(),
            args: vec![
                TyPattern::Primitive("i32".into()),
                TyPattern::Primitive("i64".into()),
            ],
        };
        assert!(match_pattern(&pat, &bad).is_none());
        let good = TyPattern::App {
            ctor: "Pair".into(),
            args: vec![
                TyPattern::Primitive("i32".into()),
                TyPattern::Primitive("i32".into()),
            ],
        };
        assert!(match_pattern(&pat, &good).is_some());
    }

    #[test]
    fn resolve_returns_no_impl_when_trait_missing() {
        let r = ImplRegistry::new();
        let err = resolve_method_call(
            &r,
            "Map",
            "insert",
            &TyPattern::Primitive("i64".into()),
            &always_true,
        )
        .expect_err("empty registry must not resolve");
        assert!(matches!(err, LookupError::NoImpl { .. }));
    }
}
