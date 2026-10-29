use std::collections::VecDeque;

#[derive(Debug, Default)]
pub(super) struct RestartHistory(VecDeque<u64>);
impl RestartHistory {
    pub(super) fn record(&mut self, now: u64, limit: usize) {
        if limit == 0 {
            return;
        }
        while self.0.len() >= limit {
            self.0.pop_front();
        }
        self.0.push_back(now);
    }
    pub(super) fn recent(&self, now: u64, window_secs: i32) -> i32 {
        let count = self
            .0
            .iter()
            .rev()
            .take_while(|time| within_window(now, **time, window_secs))
            .count();
        i32::try_from(count).unwrap_or(i32::MAX)
    }
}

#[cfg(all(kani, hew_protocol_host))]
#[path = "../../../proofs/runtime_protocols/src/restart_history.rs"]
mod formal;

fn within_window(now: u64, stamp: u64, window_secs: i32) -> bool {
    now.wrapping_sub(stamp) <= u64::try_from(window_secs).unwrap_or(0) * 1000
}
#[cfg(all(test, not(target_arch = "wasm32")))]
#[path = "../../../proofs/runtime_protocols/src/restart_history_properties.rs"]
mod failure_properties;
