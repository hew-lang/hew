//! Resolver-anchored callable identity for the three MIR function types.
//!
//! Every MIR function carries a [`MirCallableKey`]: the resolver-minted
//! declaration identity of the source body it realizes, plus the semantic
//! instance discriminator that separates one realization of that declaration
//! from another (a generic instance, the abstract origin, a synthesized child).
//!
//! WHY this exists: the pipeline used to identify a function by its emitted
//! `name` alone, so every join between Raw/Checked/Elaborated MIR and codegen
//! compared presentation strings. Two callables with equal names — an imported
//! module's function and a local one, two generic instances whose mangling
//! collides, a synthesized closure of a same-named parent — are
//! indistinguishable to such a join, which is why the retain-by-name and
//! fail-open joins downstream exist at all.
//!
//! The key is deliberately NOT constructible from a symbol string: identity is
//! minted once by the resolver ([`DefId`]) and projected here. There is no
//! `From<String>` and no constructor that takes an emitted name.
//!
//! `RawMirFunction::name` (and its Checked/Elaborated twins) stays as the
//! presentation/linkage alias beside the key.

use hew_types::{DefId, ResolvedTy};

/// Canonical identity of one MIR callable.
///
/// `declaration` is the resolver-minted identity of the *source declaration*
/// whose body this callable realizes. For a synthesized child (a closure
/// invoke shim, a generator body, a task-entry adapter) it is the declaration
/// of the enclosing user function — the child's own distinguishing identity
/// lives in `instance`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MirCallableKey {
    pub declaration: DefId,
    pub instance: MirCallableInstance,
}

/// Which realization of [`MirCallableKey::declaration`] this callable is.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MirCallableInstance {
    /// The one body of a declaration that declares no type parameters.
    Monomorphic,
    /// One concrete specialization of a generic declaration. `type_args` is in
    /// declared parameter order and comes from the monomorphisation registry
    /// (`MonoKey::type_args`) or, on the SIR path, `SirInstanceKey::type_args`.
    Generic { type_args: Vec<ResolvedTy> },
    /// The abstract origin of a generic declaration, lowered once against
    /// `ResolvedTy::TypeParam` operands for the representation substrate. It is
    /// never emitted; it is deliberately distinct from every `Generic`
    /// instance of the same declaration.
    Polymorphic,
    /// A callable the lowering synthesized to serve `parent` — it has no source
    /// declaration of its own. `child` names which producer minted it and,
    /// where a producer can mint more than one per parent, the per-parent
    /// ordinal allocated in lowering encounter order.
    Synthesized {
        parent: Box<MirCallableKey>,
        child: SynthesizedCallable,
    },
}

/// Closed set of synthesized-callable producers: one variant per lowering site
/// that mints a `RawMirFunction` with no source declaration of its own.
///
/// The ordinal carried by a variant is allocated per parent in lowering
/// encounter order (`Builder::next_synthesized_ordinal`), so it is stable
/// across runs — the `compile-determinism-verify` gate depends on that.
/// [`SynthesizedCallable::MachineStep`] carries none: a machine layout mints
/// exactly one step dispatch per parent key.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SynthesizedCallable {
    /// `lower/closure_gen.rs::lower_closure_shim` — the invoke shim of a
    /// closure literal.
    ClosureInvokeShim(u32),
    /// `lower/closure_gen.rs::lower_named_fn_invoke_shim` — the invoke shim
    /// that gives a named function the closure ABI.
    NamedFnInvokeShim(u32),
    /// `lower/closure_gen.rs::lower_spawn_lambda_actor` — the body of an
    /// `actor |..| { .. }` literal.
    LambdaActorBody(u32),
    /// `lower/closure_gen.rs::lower_gen_block` — the coroutine body of a
    /// `gen { .. }` block.
    GeneratorBody(u32),
    /// `lower/task.rs::synthesize_task_entry_adapter` — the adapter that gives
    /// a default-callconv function the task-entry ABI.
    TaskEntryAdapter(u32),
    /// `lower/task.rs::synthesize_fork_entry_shim` — the trampoline a
    /// `fork`-block spawn calls through.
    ForkEntryShim(u32),
    /// `lower/machine_synth.rs::synthesize_machine_step_fn` — the `step`
    /// dispatch of one machine layout.
    MachineStep,
}

impl MirCallableKey {
    /// The single body of a declaration with no type parameters.
    #[must_use]
    pub fn declared(declaration: DefId) -> Self {
        Self {
            declaration,
            instance: MirCallableInstance::Monomorphic,
        }
    }

    /// One concrete specialization of a generic declaration. `type_args` must
    /// be in declared parameter order.
    #[must_use]
    pub fn instance(declaration: DefId, type_args: Vec<ResolvedTy>) -> Self {
        Self {
            declaration,
            instance: MirCallableInstance::Generic { type_args },
        }
    }

    /// The abstract generic origin lowered against `ResolvedTy::TypeParam`
    /// operands.
    #[must_use]
    pub fn polymorphic(declaration: DefId) -> Self {
        Self {
            declaration,
            instance: MirCallableInstance::Polymorphic,
        }
    }

    /// The key of a callable this one synthesizes.
    #[must_use]
    pub fn child(&self, child: SynthesizedCallable) -> Self {
        Self {
            declaration: self.declaration.clone(),
            instance: MirCallableInstance::Synthesized {
                parent: Box::new(self.clone()),
                child,
            },
        }
    }

    /// Fixture identity for hand-built MIR. Test builds only — production code
    /// has no constructor that mints identity from a name.
    #[cfg(any(test, feature = "test"))]
    #[doc(hidden)]
    #[must_use]
    pub fn for_test(declaration_path: &str) -> Self {
        Self::declared(DefId::for_test(declaration_path))
    }
}
