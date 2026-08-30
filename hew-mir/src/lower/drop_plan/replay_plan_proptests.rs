//! Randomized companions to `replay_plan_tests`: the same replay contract
//! checked over generated event streams.

use super::*;
use crate::model::{OwnerId, OwnershipEvent};
use proptest::prelude::*;

fn owner_events(n: u32, transferred: &[u32]) -> Vec<Instr> {
    let mut events = Vec::new();
    for i in 0..n {
        let owner = OwnerId {
            binding: BindingId(i),
            generation: 0,
        };
        events.push(Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner,
            place: Place::Local(i),
            ty: ResolvedTy::String,
        }));
        events.push(Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner,
            recipe: crate::model::OwnerDropRecipe {
                declaration_order: i,
                ..checked_test_string_recipe()
            },
        }));
    }
    for &i in transferred {
        events.push(Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: OwnerId {
                binding: BindingId(i),
                generation: 0,
            },
            from: Place::Local(i),
            to: None,
            to_owner: None,
            to_ty: None,
        }));
    }
    events
}

proptest! {
    /// `dropped(return) == minted - transferred`, in reverse declaration
    /// order, for every subset of transferred owners.
    #[test]
    fn dropped_equals_minted_minus_transferred(n in 0u32..8, transferred_mask in 0u32..256) {
        let transferred: Vec<u32> = (0..n).filter(|i| (transferred_mask >> i) & 1 == 1).collect();
        let blocks = vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: owner_events(n, &transferred),
            terminator: Terminator::Return,
        }];
        let (_, exits) = enumerate_exits(&blocks, &HashSet::new());
        let plans = derive_drop_plans_from_replay(&blocks, &[], exits);
        prop_assert_eq!(plans.len(), 1);
        let dropped: Vec<Place> = plans[0].1.drops.iter().map(|drop| drop.place).collect();
        let expected: Vec<Place> = (0..n)
            .rev()
            .filter(|i| !transferred.contains(i))
            .map(Place::Local)
            .collect();
        prop_assert_eq!(dropped, expected);
    }
}
