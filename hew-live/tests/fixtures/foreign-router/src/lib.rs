//! Independent Rust implementation of the same portable router contract.

use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};

wit_bindgen::generate!({
    path: "../../../wit",
    world: "router",
});

use exports::hew::live_router::handler::{Event, Guest, Snapshot};
use hew::live_router::{events::Delivery, services};

struct Foreign;
static INITIALIZED: AtomicBool = AtomicBool::new(false);
static DRAINING: AtomicBool = AtomicBool::new(false);
static ACCEPTED: AtomicU64 = AtomicU64::new(0);
static ACTIVE: AtomicUsize = AtomicUsize::new(0);

struct Admission;

impl Drop for Admission {
    fn drop(&mut self) {
        ACTIVE.fetch_sub(1, Ordering::Relaxed);
    }
}

impl Guest for Foreign {
    fn initialize(state: Snapshot) -> Result<(), String> {
        if cfg!(feature = "rollback-refusal") && state.accepted != 0 {
            return Err("foreign candidate refused".into());
        }
        if DRAINING.load(Ordering::Relaxed) || INITIALIZED.swap(true, Ordering::Relaxed) {
            return Err("router is already initialized".into());
        }
        ACCEPTED.store(state.accepted, Ordering::Relaxed);
        Ok(())
    }

    async fn drive() -> Option<u64> {
        None
    }

    async fn route(message: Event) -> Result<bool, String> {
        if !INITIALIZED.load(Ordering::Relaxed) || DRAINING.load(Ordering::Relaxed) {
            return Err("router is not accepting events".into());
        }
        ACTIVE.fetch_add(1, Ordering::Relaxed);
        let _admission = Admission;
        let Some(destination) = services::destination(message.source).await? else {
            return Ok(false);
        };
        services::emit(Delivery {
            destination,
            body: message.body,
        })
        .await?;
        ACCEPTED.fetch_add(1, Ordering::Relaxed);
        Ok(true)
    }

    async fn quiesce() -> Result<Snapshot, String> {
        if !INITIALIZED.load(Ordering::Relaxed)
            || DRAINING.load(Ordering::Relaxed)
            || ACTIVE.load(Ordering::Relaxed) != 0
        {
            return Err("router must drain admitted calls before quiescence".into());
        }
        DRAINING.store(true, Ordering::Relaxed);
        Ok(Snapshot {
            accepted: ACCEPTED.load(Ordering::Relaxed),
        })
    }

    fn resume() -> Result<(), String> {
        if cfg!(feature = "rollback-refusal") {
            return Err("foreign resumption refused".into());
        }
        if !INITIALIZED.load(Ordering::Relaxed) || !DRAINING.swap(false, Ordering::Relaxed) {
            return Err("router has no suspended instance to resume".into());
        }
        Ok(())
    }

    async fn dispose() -> Result<(), String> {
        if !DRAINING.load(Ordering::Relaxed) || !INITIALIZED.swap(false, Ordering::Relaxed) {
            return Err("router must quiesce before disposal".into());
        }
        Ok(())
    }
}

export!(Foreign);
