//! Typed publication-handoff recognition for temporary-drop derivation.

use std::collections::HashSet;

use crate::{Instr, Place};

pub(super) fn instruction_carries_typed_handoff(
    instruction: &Instr,
    value: Place,
    handoffs: &HashSet<(Place, Place)>,
) -> bool {
    handoffs.iter().any(|(source, destination)| {
        if *source != value {
            return false;
        }
        match instruction {
            Instr::Move { dest, src } => {
                *src == value
                    && (*dest == *destination
                        || super::base_local(*dest) == super::base_local(*destination))
            }
            Instr::TupleConstruct { elements, dest } => {
                elements.contains(&value) && *dest == *destination
            }
            Instr::RecordInit { fields, dest, .. } => {
                fields.iter().any(|(_, field)| *field == value) && *dest == *destination
            }
            _ => false,
        }
    })
}
