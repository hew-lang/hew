//! Which destroy recipes release nothing a program can observe.
//!
//! The runtime releases a descriptor-backed collection through an iterative
//! walker so a deep structure costs no native stack (D457). Joining a walk
//! already in progress reorders a release against the rest of its parent's
//! cleanup, which only stays invisible while the released subtree runs no user
//! code. Physical MIR decides that here, once per glue identity, and codegen
//! emits the walker entry only where it holds.
//!
//! Impurity comes from three leaves and propagates upward: a resource close is
//! user code, a callable environment may capture one, and an erased vtable drop
//! is not known until run time. Recursive types make the glue graph cyclic, so
//! the answer is the greatest fixpoint: assume pure, then retract.

use super::{DestroyAction, PhysicalModule, PhysicalValueRecipe};

/// Per-glue answer to "does releasing this run any user-visible action?".
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PureDataReleases {
    aggregate: Vec<bool>,
    variant: Vec<bool>,
    vector: Vec<bool>,
    map: Vec<bool>,
    set: Vec<bool>,
}

impl PureDataReleases {
    /// Resolve the fixpoint over one module's release glue.
    #[must_use]
    pub fn compute(module: &PhysicalModule) -> Self {
        let mut table = Self {
            aggregate: vec![true; module.aggregate_glue.len()],
            variant: vec![true; module.variant_glue.len()],
            vector: vec![true; module.vector_glue.len()],
            map: vec![true; module.map_glue.len()],
            set: vec![true; module.set_glue.len()],
        };
        let mut settled = false;
        while !settled {
            settled = true;
            for glue in &module.aggregate_glue {
                let pure = table.recipes_are_pure(&glue.fields);
                settled &= !retract(&mut table.aggregate[glue.id.0 as usize], pure);
            }
            for glue in &module.variant_glue {
                let pure = glue
                    .variants
                    .iter()
                    .all(|case| table.recipes_are_pure(&case.fields));
                settled &= !retract(&mut table.variant[glue.id.0 as usize], pure);
            }
            for glue in &module.vector_glue {
                let pure = table.recipe_is_pure(&glue.element);
                settled &= !retract(&mut table.vector[glue.id.0 as usize], pure);
            }
            for glue in &module.map_glue {
                let pure = table.recipe_is_pure(&glue.key) && table.recipe_is_pure(&glue.value);
                settled &= !retract(&mut table.map[glue.id.0 as usize], pure);
            }
            for glue in &module.set_glue {
                let pure = table.recipe_is_pure(&glue.element);
                settled &= !retract(&mut table.set[glue.id.0 as usize], pure);
            }
        }
        table
    }

    /// Whether releasing a value through `action` runs no user-visible action,
    /// so the runtime may defer it into a walk already in progress.
    #[must_use]
    pub fn action_is_pure(&self, action: DestroyAction) -> bool {
        match action {
            DestroyAction::Encoding(_)
            | DestroyAction::StringRelease
            | DestroyAction::BytesRelease => true,
            DestroyAction::Resource(_) | DestroyAction::Callable | DestroyAction::TraitObject => {
                false
            }
            DestroyAction::Aggregate(id) => self.aggregate[id.0 as usize],
            DestroyAction::Variant(id) => self.variant[id.0 as usize],
            DestroyAction::Vector(id) | DestroyAction::Array(id) => self.vector[id.0 as usize],
            DestroyAction::Map(id) => self.map[id.0 as usize],
            DestroyAction::Set(id) => self.set[id.0 as usize],
        }
    }

    fn recipe_is_pure(&self, recipe: &PhysicalValueRecipe) -> bool {
        recipe
            .destroy
            .is_none_or(|action| self.action_is_pure(action))
    }

    fn recipes_are_pure(&self, recipes: &[PhysicalValueRecipe]) -> bool {
        recipes.iter().all(|recipe| self.recipe_is_pure(recipe))
    }
}

/// Retract one entry when this round found it impure. Reports whether the
/// answer moved, which is what keeps the fixpoint iterating.
fn retract(entry: &mut bool, pure: bool) -> bool {
    if *entry && !pure {
        *entry = false;
        return true;
    }
    false
}
