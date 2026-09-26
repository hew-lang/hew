//! Exact checked test entries for one compiled dispatcher.

use std::collections::HashSet;

use hew_types::{DefId, EntryExitPlan};

use crate::{HirItem, HirModule};

/// Install the checker's ordered test plans after source lowering. No source
/// spelling or registry key may substitute for a missing declaration body.
///
/// # Errors
///
/// Returns a boundary error if a plan is missing, duplicated, non-root, or
/// points at a body HIR did not emit.
pub fn install_test_entry_plans(
    module: &mut HirModule,
    plans: &[EntryExitPlan],
) -> Result<(), String> {
    if plans.is_empty() {
        return Err("selected tests have no checked entry plans".into());
    }
    if module.entry_exit_plan.is_some() {
        return Err("test dispatcher conflicts with a selected process entry".into());
    }
    let mut seen = HashSet::<DefId>::new();
    for plan in plans {
        if !seen.insert(plan.entry) {
            return Err(format!("duplicate checked test entry {:?}", plan.entry));
        }
        let function = module.items.iter().find_map(|item| match item {
            HirItem::Function(function) if function.declaration == plan.entry => Some(function),
            _ => None,
        });
        let Some(function) = function else {
            return Err(format!(
                "checked test entry {:?} has no emitted HIR body",
                plan.entry
            ));
        };
        if !module.root_item_ids.contains(&function.id) {
            return Err(format!(
                "checked test entry {:?} is not a root source function",
                plan.entry
            ));
        }
        if !function.params.is_empty() || !function.type_params.is_empty() || function.is_generator
        {
            return Err(format!(
                "checked test entry {:?} is not parameterless and monomorphic",
                plan.entry
            ));
        }
    }
    module.test_entry_plans = plans.to_vec();
    Ok(())
}
