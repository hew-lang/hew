//! What releasing a value does, per release glue identity.
//!
//! User code, faults and suspension share one fixpoint over the leaves a
//! release recipe can reach. Recursive types make that graph cyclic, so each
//! property starts at no and is raised only by a leaf that says yes.
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
//!
//! * **Suspends.** Resource continuations and authored asynchronous closes
//!   require suspension-capable cleanup, including through aggregate fields.

use super::{DestroyAction, PhysicalModule, PhysicalValueRecipe};

/// The release properties propagated together through the glue graph.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct Effects {
    user_code: bool,
    faults: bool,
    suspends: bool,
}

impl Effects {
    const ALL: Self = Self {
        user_code: true,
        faults: true,
        suspends: true,
    };

    fn union(self, other: Self) -> Self {
        Self {
            user_code: self.user_code || other.user_code,
            faults: self.faults || other.faults,
            suspends: self.suspends || other.suspends,
        }
    }
}

/// Per-glue release properties, in glue-id order.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Tables {
    resource: Vec<Effects>,
    aggregate: Vec<Effects>,
    variant: Vec<Effects>,
    vector: Vec<Effects>,
    map: Vec<Effects>,
    set: Vec<Effects>,
    shared: Vec<Effects>,
}

/// What releasing a value does, resolved once per module.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ReleaseEffects {
    table: Tables,
}

impl ReleaseEffects {
    /// Resolve the release properties together over one module's glue graph.
    #[must_use]
    pub(crate) fn compute(module: &PhysicalModule) -> Self {
        let resources = module
            .resources
            .iter()
            .map(|resource| {
                let authored = match &resource.release {
                    hew_sir::ResourceRelease::RecordClose { close, .. }
                    | hew_sir::ResourceRelease::OpaqueClose { close, .. } => {
                        Some(module.callables[close.0 as usize].is_resumable)
                    }
                    _ => None,
                };
                let asynchronous = matches!(
                    resource.release,
                    hew_sir::ResourceRelease::Generator
                        | hew_sir::ResourceRelease::ActorCall
                        | hew_sir::ResourceRelease::ActorRequest
                        | hew_sir::ResourceRelease::Stream
                        | hew_sir::ResourceRelease::Sink
                );
                Effects {
                    user_code: true,
                    faults: authored.is_some() || asynchronous,
                    suspends: authored.unwrap_or(asynchronous),
                }
            })
            .collect::<Vec<_>>();
        Self {
            table: Tables::compute(module, resources),
        }
    }

    /// Whether releasing a value through `action` runs any user-visible
    /// action, so the runtime must not defer it into a walk in progress.
    #[must_use]
    pub fn runs_user_code(&self, action: DestroyAction) -> bool {
        self.table.holds(action).user_code
    }

    /// Whether releasing a value through `action` can raise a fault the
    /// enclosing frame must own.
    #[must_use]
    pub fn raises_fault(&self, action: DestroyAction) -> bool {
        self.table.holds(action).faults
    }

    /// Whether consuming this value requires a continuation, including erased
    /// owners whose stored descriptor supplies the concrete release effect.
    #[must_use]
    pub fn suspends(&self, action: DestroyAction) -> bool {
        self.table.holds(action).suspends
    }
}

impl Tables {
    fn compute(module: &PhysicalModule, resource: Vec<Effects>) -> Self {
        let mut table = Self {
            resource,
            aggregate: vec![Effects::default(); module.aggregate_glue.len()],
            variant: vec![Effects::default(); module.variant_glue.len()],
            vector: vec![Effects::default(); module.vector_glue.len()],
            map: vec![Effects::default(); module.map_glue.len()],
            set: vec![Effects::default(); module.set_glue.len()],
            shared: vec![Effects::default(); module.shared_glue.len()],
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
                    .fold(Effects::default(), |effects, case| {
                        effects.union(table.any_recipe(&case.fields))
                    });
                settled &= !raise(&mut table.variant[glue.id.0 as usize], holds);
            }
            for glue in &module.vector_glue {
                let holds = if matches!(glue.ty, hew_types::ResolvedTy::Array(_, 0)) {
                    Effects::default()
                } else {
                    table.recipe(&glue.element)
                };
                settled &= !raise(&mut table.vector[glue.id.0 as usize], holds);
            }
            for glue in &module.map_glue {
                let holds = table.recipe(&glue.key).union(table.recipe(&glue.value));
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

    fn holds(&self, action: DestroyAction) -> Effects {
        match action {
            DestroyAction::Encoding(_)
            | DestroyAction::StringRelease
            | DestroyAction::BytesRelease
            | DestroyAction::ActorRelease
            // A weak handle owns no payload; dropping one only decrements.
            | DestroyAction::WeakRelease => Effects::default(),
            DestroyAction::Callable | DestroyAction::TraitObject => Effects::ALL,
            DestroyAction::Resource(id) => self.resource[id.0 as usize],
            DestroyAction::Aggregate(id) => self.aggregate[id.0 as usize],
            DestroyAction::Variant(id) => self.variant[id.0 as usize],
            DestroyAction::Vector(id) | DestroyAction::Array(id) => self.vector[id.0 as usize],
            DestroyAction::Map(id) => self.map[id.0 as usize],
            DestroyAction::Set(id) => self.set[id.0 as usize],
            DestroyAction::RcRelease(id) => self.shared[id.0 as usize],
        }
    }

    fn recipe(&self, recipe: &PhysicalValueRecipe) -> Effects {
        recipe
            .destroy
            .map_or_else(Effects::default, |action| self.holds(action))
    }

    fn any_recipe(&self, recipes: &[PhysicalValueRecipe]) -> Effects {
        recipes.iter().fold(Effects::default(), |effects, recipe| {
            effects.union(self.recipe(recipe))
        })
    }
}

/// Add newly discovered properties and report whether the fixpoint moved.
fn raise(entry: &mut Effects, holds: Effects) -> bool {
    let combined = entry.union(holds);
    let changed = *entry != combined;
    *entry = combined;
    changed
}
