//! Production gate checked for every valid local state and legal operation.
#![allow(dead_code, unexpected_cfgs)]
#[path = "../../hew-runtime/src/ready_state.rs"]
mod gate;
#[path = "expected.rs"]
mod specification;
use gate::{AtomicByte, ReadyState};
use std::cell::Cell;
use std::rc::Rc;
use std::sync::atomic::Ordering;

#[derive(Debug, Clone)]
struct Atomic(Rc<Cell<u8>>);
impl AtomicByte for Atomic {
    fn load(&self, _: Ordering) -> u8 {
        self.0.get()
    }
    fn fetch_or(&self, value: u8, _: Ordering) -> u8 {
        let old = self.0.get();
        self.0.set(old | value);
        old
    }
    fn fetch_and(&self, value: u8, _: Ordering) -> u8 {
        let old = self.0.get();
        self.0.set(old & value);
        old
    }
    fn compare_exchange(&self, old: u8, new: u8, _: Ordering, _: Ordering) -> Result<u8, u8> {
        let actual = self.0.get();
        if old == actual {
            self.0.set(new);
            Ok(actual)
        } else {
            Err(actual)
        }
    }
    fn compare_exchange_weak(
        &self,
        old: u8,
        new: u8,
        success: Ordering,
        failure: Ordering,
    ) -> Result<u8, u8> {
        self.compare_exchange(old, new, success, failure)
    }
}

#[kani::proof]
#[kani::unwind(3)]
fn production_refines_lean_rules_for_all_valid_states() {
    let raw: u8 = kani::any();
    let event: u8 = kani::any();
    assert_eq!(specification::TABLE.len(), 16);
    assert_eq!(specification::TABLE[0].len(), 6);
    kani::assume(event < 6);
    let expected = specification::TABLE
        .get(usize::from(raw))
        .map_or(0, |row| row[usize::from(event)]);
    let phase = raw & !8;
    let legal = phase <= 4 && (event != 4 || phase == 2 || phase == 3);
    // Prevent a malformed oracle from silently dropping difficult cases or
    // making the refinement claim vacuous. Check the domain before assuming it.
    assert_eq!(
        expected & 128 != 0,
        legal,
        "ready gate: oracle omits or adds legal inputs"
    );
    kani::assume(legal);
    let atomic = Atomic(Rc::new(Cell::new(raw)));
    let gate = ReadyState::from_atomic(atomic.clone());
    assert_eq!(
        gate.frozen_quiescent(),
        specification::QUIESCENT[usize::from(raw)]
    );
    let actual = match event {
        0 => gate.notify(),
        1 => gate.try_begin(),
        2 => gate.freeze(),
        3 => gate.unfreeze(),
        4 => gate.finish(false),
        _ => gate.finish(true),
    };
    assert_eq!(
        atomic.0.get(),
        expected & 15,
        "ready gate: next state differs from Lean"
    );
    assert_eq!(
        actual,
        expected & 16 != 0,
        "ready gate: return value differs from Lean"
    );
}
