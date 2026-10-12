//! Exact checked test entries for one compiled dispatcher.

use std::collections::HashSet;

use hew_types::{DefId, TestEntryPlan};

use crate::{HirItem, HirModule};

/// Install the checker's ordered test plans after source lowering. No source
/// spelling or registry key may substitute for a missing declaration body.
///
/// # Errors
///
/// Returns a boundary error if a plan is missing, duplicated, invalid, or
/// points at a body HIR did not emit.
pub fn install_test_entry_plans(
    module: &mut HirModule,
    plans: &[TestEntryPlan],
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
        if function.params.len() != plan.arguments.len()
            || !function.type_params.is_empty()
            || function.is_generator
        {
            return Err(format!(
                "checked test entry {:?} does not match its checked arguments or is not monomorphic",
                plan.entry
            ));
        }
    }
    module.test_entry_plans = plans.to_vec();
    Ok(())
}
