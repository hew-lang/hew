//! Registry/actor boundary for the production periodic registration. std Arc
//! supplies allocation ownership; Loom instruments the mutable protocol state.
use crate::actor::HewActor;
use crate::timer_wheel::HewTimerWheel;
use loom::sync::Mutex;
use std::sync::{Arc, Weak};

#[path = "../../../hew-runtime/src/timer_periodic/entry.rs"]
mod entry;

loom::lazy_static! {
    static ref REGISTRY: Mutex<Vec<Arc<entry::PeriodicCtx>>> = Mutex::new(Vec::new());
}

fn register_timer(_: *mut HewActor, ctx: Arc<entry::PeriodicCtx>) {
    REGISTRY.lock().unwrap().push(ctx);
}
fn unregister_timer(_: *mut HewActor, address: usize) {
    REGISTRY
        .lock()
        .unwrap()
        .retain(|ctx| Arc::as_ptr(ctx) as usize != address);
}

pub struct Probe(Weak<entry::PeriodicCtx>);
impl Probe {
    pub fn alive(&self) -> bool {
        self.0.upgrade().is_some()
    }
}
pub fn probe(handle: usize) -> Probe {
    let registry = REGISTRY.lock().unwrap();
    let ctx = registry
        .iter()
        .find(|ctx| Arc::as_ptr(ctx) as usize == handle)
        .expect("periodic: scheduled timer must remain registered");
    Probe(Arc::downgrade(ctx))
}
pub fn cancel(handle: usize) {
    let ctx = REGISTRY
        .lock()
        .unwrap()
        .iter()
        .find(|ctx| Arc::as_ptr(ctx) as usize == handle)
        .cloned();
    if let Some(ctx) = ctx {
        entry::cancel(&ctx);
    }
}
pub unsafe fn schedule(actor: *mut HewActor, wheel: *mut HewTimerWheel, interval: u64) -> usize {
    unsafe { entry::schedule_periodic_ns_on_wheel(actor, 7, interval, wheel, 0) as usize }
}
pub fn empty() -> bool {
    REGISTRY.lock().unwrap().is_empty()
}

pub fn global_wheel() -> *mut HewTimerWheel {
    panic!("unmodeled process timer wheel")
}
