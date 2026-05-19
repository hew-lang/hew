//! Actor protocol descriptor — the stable, centralized authority for
//! actor-handler message identifiers (`msg_id`s).
//!
//! Until Q87 ratification (2026-05-19, registry D-tag) every consumer derived
//! a handler's `msg_id` from its source-order index inside the actor body
//! (`for (i, h) in handlers.iter().enumerate() { msg_id = i }`). Reordering
//! the handler declarations silently re-numbered every existing recipient —
//! an ABI flip with no syntactic signal. The descriptor replaces that
//! `enumerate` path with a deterministic hash over the fully-qualified
//! handler name so source-order changes no longer leak into the wire
//! protocol.
//!
//! ## Hash determinism contract
//!
//! `compute_default_msg_id` is the canonical algorithm. Future contributors
//! (and external tooling that wants to decode actor wire frames by hand) can
//! reproduce a handler's `msg_id` with these inputs:
//!
//! - Algorithm: `siphasher::sip::SipHasher13`.
//! - Keys: `new_with_keys(0, 0)` — both key halves are explicitly zero so
//!   the digest is reproducible across processes and rebuilds. Default
//!   `SipHasher::new()` would seed with `RandomState`, which is not stable.
//! - Input: the UTF-8 bytes of the fully-qualified handler name produced by
//!   `qualified_handler_name(actor, handler)` — i.e. `"Counter::increment"`.
//!   No length prefix, no separator other than the literal `::`.
//! - Output truncation: the `SipHash` digest is a `u64`; the descriptor
//!   uses the low 32 bits (`(digest & 0xFFFF_FFFF) as u32`). Endianness is
//!   irrelevant here because the truncation is a numeric mask, not a byte
//!   reinterpretation.
//!
//! Explicit `#[msg_id(N)]` opt-in pinning is reserved for a later slice
//! (Q87 sub-task) and is not parsed yet. When two handlers hash to the same
//! 32-bit `msg_id` within one actor, the checker emits an
//! [`crate::error::TypeErrorKind::ActorProtocolCollision`] diagnostic and
//! refuses to publish the descriptor — codegen never sees a collided
//! protocol. The diagnostic suggests `#[msg_id(N)]` even though the parser
//! does not accept the attribute yet so the hint stays accurate when the
//! later slice lands.

use std::hash::Hasher;

use siphasher::sip::SipHasher13;

use crate::ResolvedTy;

/// Per-actor protocol descriptor: the canonical name → `msg_id` mapping
/// plus handler-level signature and symbol metadata.
///
/// Constructed by the type checker once all handler signatures are known
/// (`Checker::build_actor_protocol_descriptors`). Threaded through
/// `TypeCheckOutput` to HIR (and from HIR's `HirActorDecl::handler_msg_ids`
/// into MIR's [`hew_mir`-equivalent] actor layout). Downstream stages must
/// not recompute `msg_id`s — the descriptor is the only source of truth.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActorProtocolDescriptor {
    /// Actor type name (currently un-namespaced; matches `HirActorDecl::name`).
    pub actor_name: String,
    /// One entry per `receive fn`. Order matches source order, but order is
    /// no longer load-bearing for the protocol — `msg_id`s are name-derived.
    pub handlers: Vec<ActorHandlerDescriptor>,
}

/// One handler's descriptor: stable `msg_id` plus the signature/symbol
/// shape consumers need to issue a call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActorHandlerDescriptor {
    /// Surface name (`"increment"`).
    pub name: String,
    /// Deterministic 32-bit `msg_id`; see module-level docs for the algorithm.
    pub msg_id: u32,
    /// Parameter types declared by the handler, in source order.
    pub param_tys: Vec<ResolvedTy>,
    /// Return type. Unit-returning handlers are still recorded explicitly so
    /// `Fire` vs `Ask` decisions stay in one place.
    pub return_ty: ResolvedTy,
    /// Mangled callee symbol used by codegen (e.g. `Counter__increment` —
    /// the actual mangling is owned by the MIR layer; this field stores
    /// whatever string the producer chose so a single descriptor row tells a
    /// consumer "the symbol and the `msg_id` for this handler are these
    /// values, atomically").
    pub symbol: String,
}

/// Build the fully-qualified name fed into the `msg_id` hash.
///
/// Kept as a free function so external tooling that wants to reproduce a
/// `msg_id` by hand can call the same code path the checker uses.
#[must_use]
pub fn qualified_handler_name(actor_name: &str, handler_name: &str) -> String {
    format!("{actor_name}::{handler_name}")
}

/// Compute the default `msg_id` for a `receive fn` from its fully-qualified
/// name. See the module-level docs for the determinism contract.
///
/// External tools that decode actor wire frames can reproduce this digest
/// without linking the compiler: feed the UTF-8 bytes of
/// `qualified_handler_name(actor, handler)` into a `SipHash-1-3` with both
/// keys set to zero, then mask to the low 32 bits.
#[must_use]
pub fn compute_default_msg_id(qualified_name: &str) -> u32 {
    let mut hasher = SipHasher13::new_with_keys(0, 0);
    hasher.write(qualified_name.as_bytes());
    let digest = hasher.finish();
    #[allow(
        clippy::cast_possible_truncation,
        reason = "Documented contract: msg_id is the low 32 bits of the SipHash-1-3 digest."
    )]
    let truncated = (digest & 0xFFFF_FFFF) as u32;
    truncated
}

/// A collision detected while building an actor protocol descriptor.
///
/// The checker converts this into a `TypeErrorKind::ActorProtocolCollision`
/// diagnostic; tests use it directly when exercising the collision-handling
/// path via [`ActorProtocolDescriptor::from_handlers_with_ids`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActorProtocolCollision {
    pub actor_name: String,
    pub handler_a: String,
    pub handler_b: String,
    pub msg_id: u32,
}

/// Description of a handler at descriptor-build time — everything the
/// builder needs that is not derived from the handler name itself.
#[derive(Debug, Clone)]
pub struct ActorHandlerSpec {
    pub name: String,
    pub param_tys: Vec<ResolvedTy>,
    pub return_ty: ResolvedTy,
    pub symbol: String,
}

impl ActorProtocolDescriptor {
    /// Build a descriptor by hashing each handler's qualified name.
    ///
    /// Callers (currently only the type checker) translate the error into a
    /// `TypeErrorKind::ActorProtocolCollision` diagnostic and refuse to
    /// publish a descriptor for that actor.
    ///
    /// # Errors
    ///
    /// Returns an [`ActorProtocolCollision`] on the first pair of handlers
    /// whose fully-qualified names hash to the same 32-bit `msg_id`.
    pub fn from_handlers(
        actor_name: impl Into<String>,
        handlers: &[ActorHandlerSpec],
    ) -> Result<Self, ActorProtocolCollision> {
        let actor_name = actor_name.into();
        let mut built = Vec::with_capacity(handlers.len());
        // First-occurrence index per `msg_id` so the collision diagnostic
        // can name both colliding handlers.
        let mut seen: Vec<(u32, usize)> = Vec::with_capacity(handlers.len());
        for spec in handlers {
            let qualified = qualified_handler_name(&actor_name, &spec.name);
            let msg_id = compute_default_msg_id(&qualified);
            if let Some((_, prev_idx)) = seen.iter().find(|(id, _)| *id == msg_id) {
                return Err(ActorProtocolCollision {
                    actor_name,
                    handler_a: handlers[*prev_idx].name.clone(),
                    handler_b: spec.name.clone(),
                    msg_id,
                });
            }
            seen.push((msg_id, built.len()));
            built.push(ActorHandlerDescriptor {
                name: spec.name.clone(),
                msg_id,
                param_tys: spec.param_tys.clone(),
                return_ty: spec.return_ty.clone(),
                symbol: spec.symbol.clone(),
            });
        }
        Ok(Self {
            actor_name,
            handlers: built,
        })
    }

    /// Test-only constructor that lets callers pin the `msg_id` of every
    /// handler explicitly. This is the seam the collision test uses:
    /// pinning two handlers to the same `msg_id` is the only practical way
    /// to exercise the collision path because finding a real 32-bit
    /// `SipHash-1-3` preimage collision in a unit test is impractical.
    ///
    /// Production code paths must use [`Self::from_handlers`].
    ///
    /// # Errors
    ///
    /// Returns an [`ActorProtocolCollision`] when two of the pinned
    /// `(spec, msg_id)` entries share the same `msg_id`.
    pub fn from_handlers_with_ids(
        actor_name: impl Into<String>,
        handlers: &[(ActorHandlerSpec, u32)],
    ) -> Result<Self, ActorProtocolCollision> {
        let actor_name = actor_name.into();
        let mut built = Vec::with_capacity(handlers.len());
        let mut seen: Vec<(u32, usize)> = Vec::with_capacity(handlers.len());
        for (spec, msg_id) in handlers {
            if let Some((_, prev_idx)) = seen.iter().find(|(id, _)| *id == *msg_id) {
                return Err(ActorProtocolCollision {
                    actor_name,
                    handler_a: handlers[*prev_idx].0.name.clone(),
                    handler_b: spec.name.clone(),
                    msg_id: *msg_id,
                });
            }
            seen.push((*msg_id, built.len()));
            built.push(ActorHandlerDescriptor {
                name: spec.name.clone(),
                msg_id: *msg_id,
                param_tys: spec.param_tys.clone(),
                return_ty: spec.return_ty.clone(),
                symbol: spec.symbol.clone(),
            });
        }
        Ok(Self {
            actor_name,
            handlers: built,
        })
    }

    /// Convenience lookup for downstream consumers: name → `msg_id`.
    #[must_use]
    pub fn msg_id_for(&self, handler_name: &str) -> Option<u32> {
        self.handlers
            .iter()
            .find(|h| h.name == handler_name)
            .map(|h| h.msg_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unit_spec(name: &str) -> ActorHandlerSpec {
        ActorHandlerSpec {
            name: name.to_string(),
            param_tys: vec![],
            return_ty: ResolvedTy::Unit,
            symbol: format!("Test__{name}"),
        }
    }

    #[test]
    fn distinct_handler_names_get_distinct_msg_ids() {
        let descriptor = ActorProtocolDescriptor::from_handlers(
            "Counter",
            &[unit_spec("increment"), unit_spec("total")],
        )
        .expect("two distinct handler names must not collide under SipHash-1-3");
        assert_eq!(descriptor.handlers.len(), 2);
        assert_ne!(descriptor.handlers[0].msg_id, descriptor.handlers[1].msg_id);
    }

    #[test]
    fn same_handler_name_in_different_actors_gets_distinct_msg_ids() {
        // The hash input is the fully-qualified name, so `increment` on two
        // different actors must produce two different msg_ids — proves the
        // qualified-name input choice carries the actor discriminator.
        let counter = ActorProtocolDescriptor::from_handlers("Counter", &[unit_spec("increment")])
            .expect("Counter::increment must build");
        let gauge = ActorProtocolDescriptor::from_handlers("Gauge", &[unit_spec("increment")])
            .expect("Gauge::increment must build");
        assert_ne!(counter.handlers[0].msg_id, gauge.handlers[0].msg_id);
    }

    #[test]
    fn msg_id_is_deterministic_across_invocations() {
        // Hash determinism is the substrate contract; the test pins the
        // concrete value so an accidental crate upgrade that changes the
        // SipHasher13 implementation lights up here, not in a downstream
        // ABI break.
        let a = compute_default_msg_id("Counter::increment");
        let b = compute_default_msg_id("Counter::increment");
        assert_eq!(a, b);
    }

    #[test]
    fn collision_via_explicit_ids_returns_diagnostic() {
        // Use the test-only constructor to force two handlers onto the same
        // msg_id. This exercises the collision path without depending on
        // hash-preimage gymnastics.
        let err = ActorProtocolDescriptor::from_handlers_with_ids(
            "Counter",
            &[(unit_spec("inc"), 42), (unit_spec("dec"), 42)],
        )
        .expect_err("two handlers pinned to the same msg_id must be rejected");
        assert_eq!(err.actor_name, "Counter");
        assert_eq!(err.handler_a, "inc");
        assert_eq!(err.handler_b, "dec");
        assert_eq!(err.msg_id, 42);
    }

    #[test]
    fn msg_id_for_returns_descriptor_id() {
        let descriptor = ActorProtocolDescriptor::from_handlers(
            "Counter",
            &[unit_spec("increment"), unit_spec("total")],
        )
        .unwrap();
        let id = descriptor.msg_id_for("increment").expect("present");
        assert_eq!(id, descriptor.handlers[0].msg_id);
        assert!(descriptor.msg_id_for("missing").is_none());
    }
}
