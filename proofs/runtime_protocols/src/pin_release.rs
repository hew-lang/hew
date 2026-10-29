//! Every machine-width live pin count, including closure and exhaustion.
use super::*;
#[kani::proof]
fn final_supervisor_pin_preserves_closure_and_authorizes_drain_notification() {
    let before: usize = kani::any();
    kani::assume(before & PINS != 0); // release requires one owned pin
    let state = PinState {
        access: AtomicUsize::new(before),
        teardown: AtomicBool::new(false),
    };
    let notified = state.release_pin();
    let count = before % CLOSING;
    assert_eq!(
        notified,
        before >= CLOSING && count == 1,
        "lifecycle: final closed supervisor pin lost or duplicated drain authority"
    );
    assert_eq!(
        state.access.load(Ordering::Acquire),
        before - 1,
        "lifecycle: releasing a supervisor pin corrupted its count"
    );
    assert_eq!(
        state.access.load(Ordering::Acquire) >= CLOSING,
        before >= CLOSING,
        "lifecycle: supervisor pin release reopened admission"
    );
}
