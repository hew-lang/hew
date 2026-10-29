//! Readiness ownership for work driven by the runtime scheduler.

use std::sync::Arc;

#[path = "ready_state.rs"]
mod state;
pub(crate) use state::ReadyState;

pub(crate) trait ReadyWork: Send + Sync {
    fn poll(self: Arc<Self>);
}

pub(crate) fn enqueue(work: Arc<dyn ReadyWork>) {
    #[cfg(not(target_arch = "wasm32"))]
    if !crate::driver::active() {
        crate::scheduler::enqueue_work(work);
        return;
    }
    crate::driver::publish_work(work);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn frozen_task_keeps_notifications_without_exposing_a_running_frame() {
        let state = ReadyState::queued();
        state.begin();
        assert!(!state.freeze());
        assert!(!state.frozen_quiescent());
        assert!(!state.notify());
        assert!(!state.finish(false));
        assert!(state.frozen_quiescent());
        assert!(!state.try_begin());
        assert!(state.unfreeze());
        assert!(state.try_begin());
        assert!(!state.finish(true));
        assert!(!state.notify());
    }

    #[test]
    fn freezing_queued_task_preserves_its_one_pending_poll() {
        let state = ReadyState::queued();
        assert!(state.freeze());
        assert!(!state.try_begin());
        assert!(!state.notify());
        assert!(state.unfreeze());
        assert!(state.try_begin());
        assert!(!state.try_begin());
        assert!(!state.finish(false));
    }
}
