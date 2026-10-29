use std::ffi::c_int;
pub const STRATEGY_ONE_FOR_ONE: c_int = 0;
pub const STRATEGY_ONE_FOR_ALL: c_int = 1;
pub const STRATEGY_REST_FOR_ONE: c_int = 2;
pub const STRATEGY_SIMPLE_ONE_FOR_ONE: c_int = 3;
/// One stable child identity in the parent's declaration order.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum RestartChildRole {
    Actor(u64),
    Supervisor { index: usize, identity: u64 },
}

impl RestartChildRole {
    pub(crate) fn identity(self) -> u64 {
        match self {
            Self::Actor(identity) | Self::Supervisor { identity, .. } => identity,
        }
    }
}

pub(crate) fn choose_roles(
    roles: impl Iterator<Item = (RestartChildRole, bool)>,
    strategy: c_int,
    failed: RestartChildRole,
) -> Vec<RestartChildRole> {
    let mut roles: Vec<_> = roles
        .filter_map(|(role, spent)| (!spent).then_some(role))
        .collect();
    roles.sort_unstable_by_key(|role| role.identity());
    let selected: Vec<_> = roles
        .into_iter()
        .filter(|role| match strategy {
            STRATEGY_ONE_FOR_ONE | STRATEGY_SIMPLE_ONE_FOR_ONE => *role == failed,
            STRATEGY_ONE_FOR_ALL => true,
            STRATEGY_REST_FOR_ONE => role.identity() >= failed.identity(),
            _ => unreachable!("unknown supervisor restart strategy"),
        })
        .collect();
    if selected.contains(&failed) {
        selected
    } else {
        Vec::new()
    }
}
#[cfg(all(test, not(target_arch = "wasm32")))]
#[path = "../../../proofs/runtime_protocols/src/restart_policy_properties.rs"]
mod failure_properties;
