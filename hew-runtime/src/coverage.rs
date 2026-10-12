//! Execution counts for compiler-assigned SIR block counters.

use std::collections::BTreeMap;
use std::sync::Mutex;

use crate::util::MutexExt;

static COUNTERS: Mutex<BTreeMap<u32, u64>> = Mutex::new(BTreeMap::new());

#[no_mangle]
pub extern "C" fn hew_coverage_hit(counter: u32) {
    let mut counters = COUNTERS.lock_or_recover();
    let hits = counters.entry(counter).or_default();
    *hits = hits.saturating_add(1);
}

pub(crate) fn snapshot() -> BTreeMap<u32, u64> {
    COUNTERS.lock_or_recover().clone()
}
