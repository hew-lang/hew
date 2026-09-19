//! What releasing a value does, per release glue identity.
//!
//! Two questions share one fixpoint, because both are decided by the leaves a
//! release recipe can reach and both propagate upward through the same glue
//! graph. Recursive types make that graph cyclic, so each answer starts at no
//! and is raised only by a leaf that says yes.
//!
//! * **Runs user-visible action.** The runtime releases a descriptor-backed
//!   collection through an iterative walker so a deep structure costs no
//!   native stack (D457). Joining a walk already in progress reorders a
//!   release against the rest of its parent's cleanup, which only stays
//!   invisible while the released subtree runs no user code. Codegen emits the
//!   walker entry only where this answers no.
//!
//! * **Raises a fault.** A release the frame emits itself can run an authored
//!   `close`, which makes it the frame's fault edge (D516): a failing close
//!   fills the frame's fault record and the frame dispatches that outcome
//!   instead of resuming the source exit. Only a `#[resource]` record or an
//!   authored opaque handle runs a body with the fault ABI; every other
//!   resource protocol releases through a C endpoint that cannot raise one.
//!
//!   A collection, a shared handle, a callable environment and an erased
//!   vtable drop release through runtime glue rather than in the frame's own
//!   code. Generated code brackets such a release with a release-fault sink,
//!   so a failing close inside one still reaches the frame that asked for it,
//!   and they answer the same as a release the frame emits itself.

use super::{DestroyAction, PhysicalModule, PhysicalValueRecipe};

/// Per-glue answers to one question, in glue-id order.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Tables {
    resource: Vec<bool>,
    aggregate: Vec<bool>,
    variant: Vec<bool>,
    vector: Vec<bool>,
    map: Vec<bool>,
    set: Vec<bool>,
    shared: Vec<bool>,
}

/// What releasing a value does, resolved once per module.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ReleaseEffects {
    user_code: Tables,
    faults: Tables,
    suspends: Tables,
}

impl ReleaseEffects {
    /// Resolve both fixpoints over one module's release glue.
    #[must_use]
    pub fn compute(module: &PhysicalModule) -> Self {
        Self {
            // Every resource release runs the program's own close body or its
            // declared release endpoint, so all of them are user-visible.
            user_code: Tables::compute(module, &vec![true; module.resources.len()]),
            faults: Tables::compute(module, &authored_closes(module)),
            suspends: Tables::compute(
                module,
                &module
                    .resources
                    .iter()
                    .map(|resource| match &resource.release {
                        hew_sir::ResourceRelease::RecordClose { close, .. }
                        | hew_sir::ResourceRelease::OpaqueClose { close, .. } => {
                            module.callables[close.0 as usize].is_resumable
                        }
                        hew_sir::ResourceRelease::Generator
                        | hew_sir::ResourceRelease::Stream
                        | hew_sir::ResourceRelease::Sink => true,
                        _ => false,
                    })
                    .collect::<Vec<_>>(),
            ),
        }
    }

    /// Whether releasing a value through `action` runs any user-visible
    /// action, so the runtime must not defer it into a walk in progress.
    #[must_use]
    pub fn runs_user_code(&self, action: DestroyAction) -> bool {
        self.user_code.holds(action)
    }

    /// Whether releasing a value through `action` can raise a fault the
    /// enclosing frame must own.
    #[must_use]
    pub fn raises_fault(&self, action: DestroyAction) -> bool {
        self.faults.holds(action)
    }

    /// Whether consuming this value requires a continuation, including erased
    /// owners whose stored descriptor supplies the concrete release effect.
    #[must_use]
    pub fn suspends(&self, action: DestroyAction) -> bool {
        self.suspends.holds(action)
    }
}

/// Which resources release through a body that can fault. A record's and an
/// authored opaque handle's `close` is an ordinary callable with the fault
/// ABI; every other protocol releases through a C endpoint.
fn authored_closes(module: &PhysicalModule) -> Vec<bool> {
    module
        .resources
        .iter()
        .map(|resource| {
            matches!(
                resource.release,
                hew_sir::ResourceRelease::RecordClose { .. }
                    | hew_sir::ResourceRelease::OpaqueClose { .. }
            )
        })
        .collect()
}

impl Tables {
    fn compute(module: &PhysicalModule, resource: &[bool]) -> Self {
        let mut table = Self {
            resource: resource.to_vec(),
            aggregate: vec![false; module.aggregate_glue.len()],
            variant: vec![false; module.variant_glue.len()],
            vector: vec![false; module.vector_glue.len()],
            map: vec![false; module.map_glue.len()],
            set: vec![false; module.set_glue.len()],
            shared: vec![false; module.shared_glue.len()],
        };
        let mut settled = false;
        while !settled {
            settled = true;
            for glue in &module.aggregate_glue {
                let holds = table.any_recipe(&glue.fields);
                settled &= !raise(&mut table.aggregate[glue.id.0 as usize], holds);
            }
            for glue in &module.variant_glue {
                let holds = glue
                    .variants
                    .iter()
                    .any(|case| table.any_recipe(&case.fields));
                settled &= !raise(&mut table.variant[glue.id.0 as usize], holds);
            }
            for glue in &module.vector_glue {
                let holds = !matches!(glue.ty, hew_types::ResolvedTy::Array(_, 0))
                    && table.recipe(&glue.element);
                settled &= !raise(&mut table.vector[glue.id.0 as usize], holds);
            }
            for glue in &module.map_glue {
                let holds = table.recipe(&glue.key) || table.recipe(&glue.value);
                settled &= !raise(&mut table.map[glue.id.0 as usize], holds);
            }
            for glue in &module.set_glue {
                let holds = table.recipe(&glue.element);
                settled &= !raise(&mut table.set[glue.id.0 as usize], holds);
            }
            // Releasing the last strong reference runs the payload's own
            // release, so a shared handle answers exactly as its payload does.
            for glue in &module.shared_glue {
                let holds = table.recipe(&glue.payload);
                settled &= !raise(&mut table.shared[glue.id.0 as usize], holds);
            }
        }
        table
    }

    fn holds(&self, action: DestroyAction) -> bool {
        match action {
            DestroyAction::Encoding(_)
            | DestroyAction::StringRelease
            | DestroyAction::BytesRelease
            // A weak handle owns no payload; dropping one only decrements.
            | DestroyAction::WeakRelease => false,
            DestroyAction::Callable | DestroyAction::TraitObject => true,
            DestroyAction::Resource(id) => self.resource[id.0 as usize],
            DestroyAction::Aggregate(id) => self.aggregate[id.0 as usize],
            DestroyAction::Variant(id) => self.variant[id.0 as usize],
            DestroyAction::Vector(id) | DestroyAction::Array(id) => self.vector[id.0 as usize],
            DestroyAction::Map(id) => self.map[id.0 as usize],
            DestroyAction::Set(id) => self.set[id.0 as usize],
            DestroyAction::RcRelease(id) => self.shared[id.0 as usize],
        }
    }

    fn recipe(&self, recipe: &PhysicalValueRecipe) -> bool {
        recipe.destroy.is_some_and(|action| self.holds(action))
    }

    fn any_recipe(&self, recipes: &[PhysicalValueRecipe]) -> bool {
        recipes.iter().any(|recipe| self.recipe(recipe))
    }
}

/// Raise one entry when this round found the answer yes. Reports whether the
/// answer moved, which is what keeps the fixpoint iterating.
fn raise(entry: &mut bool, holds: bool) -> bool {
    if !*entry && holds {
        *entry = true;
        return true;
    }
    false
}
