//! Stable identities for process-local actor and supervisor handles.

use std::cell::Cell;
use std::collections::HashMap;
#[cfg(test)]
use std::sync::atomic::AtomicBool;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Condvar, Mutex, PoisonError};

use crate::lifetime::PoisonSafe;
use crate::runtime_id::RuntimeId;

/// One-word semantic identity carried by an in-language `LocalPid<T>`.
///
/// Zero is reserved as the invalid/`None` niche. Production identities are
/// allocated from one process-global counter and are never reset or reused.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct HewLocalPidId(usize);

impl HewLocalPidId {
    /// Invalid handle value and the scalar niche used by `Option<LocalPid<T>>`.
    pub const INVALID: Self = Self(0);

    /// Return the target-word representation used by the runtime ABI.
    #[must_use]
    pub const fn as_usize(self) -> usize {
        self.0
    }
}

impl From<HewLocalPidId> for usize {
    fn from(value: HewLocalPidId) -> Self {
        value.as_usize()
    }
}

/// The next process-wide local handle identity. This authority deliberately
/// outlives every `RuntimeInner`: cleanup and session reset must not create an
/// ABA opportunity by restarting the sequence.
static NEXT_LOCAL_PID_ID: AtomicUsize = AtomicUsize::new(1);

fn allocate_from(next: &AtomicUsize) -> Option<HewLocalPidId> {
    let mut candidate = next.load(Ordering::Relaxed);
    loop {
        if candidate == 0 || candidate == usize::MAX {
            return None;
        }
        let successor = candidate.checked_add(1)?;
        match next.compare_exchange_weak(candidate, successor, Ordering::Relaxed, Ordering::Relaxed)
        {
            Ok(_) => return Some(HewLocalPidId(candidate)),
            Err(observed) => candidate = observed,
        }
    }
}

fn allocate() -> Option<HewLocalPidId> {
    allocate_from(&NEXT_LOCAL_PID_ID)
}

/// Semantic target behind one local handle identity.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Route {
    /// One concrete actor incarnation. Restart replacements receive a distinct
    /// token and can never inherit this route.
    Actor {
        runtime_id: RuntimeId,
        actor_id: u64,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum LocalHandleError {
    Exhausted,
    ShuttingDown,
    ActorAlreadyRegistered,
    IdentityCollision,
    PublicationAlreadyUsed,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum RetireActorResult {
    Retired,
    AlreadyRetired,
    Mismatch,
}

#[derive(Default)]
struct LocalHandleState {
    routes: HashMap<HewLocalPidId, Route>,
    actor_tokens: HashMap<u64, HewLocalPidId>,
}

/// Runtime-owned semantic route authority for local handle values.
///
/// The registry stores IDs only, never actor pointers. Callers copy a route,
/// release this lock, then resolve and pin the `ActorId` through `LiveActors`.
pub(crate) struct LocalHandles {
    state: PoisonSafe<LocalHandleState>,
    publication_gate: PublicationGate,
    #[cfg(test)]
    fail_next_registration: AtomicBool,
    #[cfg(test)]
    admission_hook: Mutex<AdmissionHook>,
}

struct PublicationGate {
    state: Mutex<PublicationGateState>,
    idle: Condvar,
}

struct PublicationGateState {
    accepting: bool,
    in_flight: usize,
}

#[cfg(test)]
type AdmissionHook = Option<(
    std::sync::Arc<std::sync::Barrier>,
    std::sync::Arc<std::sync::Barrier>,
)>;

/// In-flight publication lease. Shutdown waits for every lease to drop before
/// draining live actors, so no actor can be observed between route reservation
/// and liveness publication.
pub(crate) struct ActorPublication<'a> {
    handles: &'a LocalHandles,
    registered: Cell<bool>,
}

impl Drop for ActorPublication<'_> {
    fn drop(&mut self) {
        let mut state = self
            .handles
            .publication_gate
            .state
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        state.in_flight = state
            .in_flight
            .checked_sub(1)
            .expect("actor publication count underflow");
        if state.in_flight == 0 {
            self.handles.publication_gate.idle.notify_all();
        }
    }
}

impl LocalHandles {
    pub(crate) fn new() -> Self {
        Self {
            state: PoisonSafe::new(LocalHandleState::default()),
            publication_gate: PublicationGate {
                state: Mutex::new(PublicationGateState {
                    accepting: true,
                    in_flight: 0,
                }),
                idle: Condvar::new(),
            },
            #[cfg(test)]
            fail_next_registration: AtomicBool::new(false),
            #[cfg(test)]
            admission_hook: Mutex::new(None),
        }
    }

    pub(crate) fn begin_publication(&self) -> Result<ActorPublication<'_>, LocalHandleError> {
        #[cfg(test)]
        self.run_admission_hook();
        let mut state = self
            .publication_gate
            .state
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        if !state.accepting {
            return Err(LocalHandleError::ShuttingDown);
        }
        state.in_flight = state
            .in_flight
            .checked_add(1)
            .ok_or(LocalHandleError::Exhausted)?;
        Ok(ActorPublication {
            handles: self,
            registered: Cell::new(false),
        })
    }

    pub(crate) fn begin_shutdown(&self) {
        let mut state = self
            .publication_gate
            .state
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        state.accepting = false;
        while state.in_flight != 0 {
            state = self
                .publication_gate
                .idle
                .wait(state)
                .unwrap_or_else(PoisonError::into_inner);
        }
    }

    #[cfg(any(test, target_arch = "wasm32"))]
    fn reopen_after_shutdown(&self) {
        let mut state = self
            .publication_gate
            .state
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        assert_eq!(state.in_flight, 0);
        assert_eq!(self.actor_route_count(), 0);
        state.accepting = true;
    }

    #[cfg(test)]
    fn run_admission_hook(&self) {
        let hook = self
            .admission_hook
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .clone();
        if let Some((entered, release)) = hook {
            entered.wait();
            release.wait();
        }
    }

    #[cfg(test)]
    fn install_admission_hook(
        &self,
        entered: std::sync::Arc<std::sync::Barrier>,
        release: std::sync::Arc<std::sync::Barrier>,
    ) {
        *self
            .admission_hook
            .lock()
            .unwrap_or_else(PoisonError::into_inner) = Some((entered, release));
    }

    /// Register one concrete actor incarnation and return its stable token.
    fn register_actor_reserved(
        &self,
        runtime_id: RuntimeId,
        actor_id: u64,
    ) -> Result<HewLocalPidId, LocalHandleError> {
        self.state.access(|state| {
            if state.actor_tokens.contains_key(&actor_id) {
                return Err(LocalHandleError::ActorAlreadyRegistered);
            }
            #[cfg(test)]
            if self.fail_next_registration.swap(false, Ordering::AcqRel) {
                return Err(LocalHandleError::Exhausted);
            }
            let token = allocate().ok_or(LocalHandleError::Exhausted)?;
            match state.routes.entry(token) {
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(Route::Actor {
                        runtime_id,
                        actor_id,
                    });
                }
                std::collections::hash_map::Entry::Occupied(_) => {
                    return Err(LocalHandleError::IdentityCollision);
                }
            }
            state.actor_tokens.insert(actor_id, token);
            Ok(token)
        })
    }

    #[cfg_attr(
        not(test),
        allow(
            dead_code,
            reason = "direct registration is a registry unit-test surface"
        )
    )]
    pub(crate) fn register_actor(
        &self,
        runtime_id: RuntimeId,
        actor_id: u64,
    ) -> Result<HewLocalPidId, LocalHandleError> {
        let publication = self.begin_publication()?;
        publication.register_actor(runtime_id, actor_id)
    }

    #[cfg(test)]
    pub(crate) fn fail_next_registration_for_test(&self) {
        self.fail_next_registration.store(true, Ordering::Release);
    }

    /// Copy the concrete `ActorId` behind a direct token for this runtime.
    #[cfg_attr(
        not(test),
        allow(dead_code, reason = "consumed by the runtime token operation commit")
    )]
    pub(crate) fn resolve_actor(&self, runtime_id: RuntimeId, token: HewLocalPidId) -> Option<u64> {
        if token == HewLocalPidId::INVALID {
            return None;
        }
        self.state.access(|state| match state.routes.get(&token) {
            Some(Route::Actor {
                runtime_id: owner,
                actor_id,
            }) if *owner == runtime_id => Some(*actor_id),
            _ => None,
        })
    }

    /// Return the registered direct token for an `ActorId` in this runtime.
    #[cfg_attr(
        not(test),
        allow(dead_code, reason = "consumed by the atomic compiler token cutover")
    )]
    pub(crate) fn actor_token(
        &self,
        runtime_id: RuntimeId,
        actor_id: u64,
    ) -> Option<HewLocalPidId> {
        self.state.access(|state| {
            let token = *state.actor_tokens.get(&actor_id)?;
            match state.routes.get(&token) {
                Some(Route::Actor {
                    runtime_id: owner,
                    actor_id: routed,
                }) if *owner == runtime_id && *routed == actor_id => Some(token),
                _ => None,
            }
        })
    }

    /// Retire exactly one `(token, ActorId)` pair.
    ///
    /// A stale or mismatched request leaves the live route untouched.
    pub(crate) fn retire_actor(&self, token: HewLocalPidId, actor_id: u64) -> RetireActorResult {
        if token == HewLocalPidId::INVALID {
            return RetireActorResult::AlreadyRetired;
        }
        self.state.access(|state| {
            if !state.routes.contains_key(&token) && !state.actor_tokens.contains_key(&actor_id) {
                return RetireActorResult::AlreadyRetired;
            }
            if !matches!(
                state.routes.get(&token),
                Some(Route::Actor {
                    actor_id: routed,
                    ..
                }) if *routed == actor_id
            ) || state.actor_tokens.get(&actor_id) != Some(&token)
            {
                return RetireActorResult::Mismatch;
            }
            state.routes.remove(&token);
            state.actor_tokens.remove(&actor_id);
            RetireActorResult::Retired
        })
    }

    #[cfg(test)]
    pub(crate) fn counts(&self) -> (usize, usize) {
        self.state
            .access(|state| (state.routes.len(), state.actor_tokens.len()))
    }

    pub(crate) fn actor_route_count(&self) -> usize {
        self.state.access(|state| state.actor_tokens.len())
    }
}

impl ActorPublication<'_> {
    pub(crate) fn register_actor(
        &self,
        runtime_id: RuntimeId,
        actor_id: u64,
    ) -> Result<HewLocalPidId, LocalHandleError> {
        if self.registered.replace(true) {
            return Err(LocalHandleError::PublicationAlreadyUsed);
        }
        self.handles.register_actor_reserved(runtime_id, actor_id)
    }

    pub(crate) fn retire_actor(&self, token: HewLocalPidId, actor_id: u64) -> RetireActorResult {
        self.handles.retire_actor(token, actor_id)
    }
}

#[cfg(target_arch = "wasm32")]
static WASM_LOCAL_HANDLES: std::sync::OnceLock<LocalHandles> = std::sync::OnceLock::new();

#[cfg(not(target_arch = "wasm32"))]
fn current_handles() -> Option<&'static LocalHandles> {
    crate::runtime::rt_current_opt().map(|rt| &rt.local_handles)
}

#[cfg(target_arch = "wasm32")]
fn current_handles() -> Option<&'static LocalHandles> {
    Some(WASM_LOCAL_HANDLES.get_or_init(LocalHandles::new))
}

/// Start one actor publication in the current runtime authority.
#[cfg(target_arch = "wasm32")]
pub(crate) fn begin_current_actor_publication(
) -> Result<ActorPublication<'static>, LocalHandleError> {
    current_handles()
        .ok_or(LocalHandleError::ShuttingDown)?
        .begin_publication()
}

/// Start one actor publication in an explicitly selected runtime authority.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn begin_actor_publication_in(
    handles: &LocalHandles,
) -> Result<ActorPublication<'_>, LocalHandleError> {
    handles.begin_publication()
}

/// Retire an exact direct actor route in the current runtime authority.
#[cfg(target_arch = "wasm32")]
pub(crate) fn retire_current_actor(token: HewLocalPidId, actor_id: u64) -> RetireActorResult {
    current_handles().map_or(RetireActorResult::AlreadyRetired, |handles| {
        handles.retire_actor(token, actor_id)
    })
}

/// Retire an exact direct route in an explicitly selected runtime authority.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn retire_actor_in(
    handles: &LocalHandles,
    token: HewLocalPidId,
    actor_id: u64,
) -> RetireActorResult {
    handles.retire_actor(token, actor_id)
}

/// Assert that actor cleanup retired every direct actor route.
pub(crate) fn assert_current_actor_routes_empty() {
    let count = current_handles().map_or(0, LocalHandles::actor_route_count);
    assert_eq!(count, 0, "local actor handle routes survived actor cleanup");
}

/// Close publication and wait for every in-flight spawn before bulk drain.
pub(crate) fn begin_current_shutdown() {
    if let Some(handles) = current_handles() {
        handles.begin_shutdown();
    }
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn finish_current_shutdown() {
    if let Some(handles) = current_handles() {
        handles.reopen_after_shutdown();
    }
}

#[cfg(test)]
pub(crate) fn current_counts_for_test() -> (usize, usize) {
    current_handles().map_or((0, 0), LocalHandles::counts)
}

/// Resolve a direct actor route in the current runtime authority.
#[allow(dead_code, reason = "consumed by the runtime token operation commit")]
pub(crate) fn resolve_current_actor(token: HewLocalPidId) -> Option<u64> {
    #[cfg(not(target_arch = "wasm32"))]
    let runtime_id = crate::runtime::rt_current_opt()?.runtime_id();
    #[cfg(target_arch = "wasm32")]
    let runtime_id = RuntimeId::DEFAULT;
    current_handles()?.resolve_actor(runtime_id, token)
}

/// Return the direct token registered for an `ActorId` in the current runtime.
#[allow(
    dead_code,
    reason = "consumed by `this` and spawn during the atomic compiler token cutover"
)]
pub(crate) fn current_actor_token(actor_id: u64) -> Option<HewLocalPidId> {
    #[cfg(not(target_arch = "wasm32"))]
    let runtime_id = crate::runtime::rt_current_opt()?.runtime_id();
    #[cfg(target_arch = "wasm32")]
    let runtime_id = RuntimeId::DEFAULT;
    current_handles()?.actor_token(runtime_id, actor_id)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_pid_identity_is_exactly_one_target_word() {
        assert_eq!(
            std::mem::size_of::<HewLocalPidId>(),
            std::mem::size_of::<usize>()
        );
        assert_eq!(
            std::mem::align_of::<HewLocalPidId>(),
            std::mem::align_of::<usize>()
        );
    }

    #[test]
    fn allocator_refuses_exhaustion_without_wrapping() {
        let next = AtomicUsize::new(usize::MAX - 1);
        let last = allocate_from(&next).expect("last checked identity");
        assert_eq!(last.as_usize(), usize::MAX - 1);
        assert_eq!(allocate_from(&next), None);
        assert_eq!(next.load(std::sync::atomic::Ordering::Relaxed), usize::MAX);
    }

    #[test]
    fn direct_actor_routes_retire_exactly_and_never_retarget() {
        let handles = LocalHandles::new();
        let runtime_id = RuntimeId::DEFAULT;
        let old = handles
            .register_actor(runtime_id, 41)
            .expect("old direct route");

        assert_eq!(handles.resolve_actor(runtime_id, old), Some(41));
        assert_eq!(handles.retire_actor(old, 41), RetireActorResult::Retired);
        assert_eq!(
            handles.retire_actor(old, 41),
            RetireActorResult::AlreadyRetired
        );
        assert_eq!(handles.resolve_actor(runtime_id, old), None);

        // A replacement allocation may reuse the same storage address, but the
        // route contains semantic ActorId only. Its new token cannot revive the
        // retired identity.
        let replacement = handles
            .register_actor(runtime_id, 42)
            .expect("replacement direct route");
        assert_ne!(replacement, old);
        assert_eq!(handles.resolve_actor(runtime_id, old), None);
        assert_eq!(handles.resolve_actor(runtime_id, replacement), Some(42));
    }

    #[test]
    fn sequential_runtime_registries_never_reuse_process_identity() {
        let first = LocalHandles::new();
        let old = first
            .register_actor(RuntimeId::DEFAULT, 7)
            .expect("first runtime route");
        assert_eq!(first.retire_actor(old, 7), RetireActorResult::Retired);
        assert_eq!(first.counts(), (0, 0));
        drop(first);

        let second = LocalHandles::new();
        let new = second
            .register_actor(RuntimeId::DEFAULT, 7)
            .expect("second runtime route");
        assert_ne!(new, old);
        assert_eq!(second.resolve_actor(RuntimeId::DEFAULT, old), None);
        assert_eq!(second.resolve_actor(RuntimeId::DEFAULT, new), Some(7));
    }

    #[test]
    fn sequential_wasm_style_sessions_reopen_without_reusing_identity() {
        let handles = LocalHandles::new();
        let first = handles
            .register_actor(RuntimeId::DEFAULT, 71)
            .expect("first session route");
        assert_eq!(handles.retire_actor(first, 71), RetireActorResult::Retired);
        handles.begin_shutdown();
        assert!(matches!(
            handles.register_actor(RuntimeId::DEFAULT, 72),
            Err(LocalHandleError::ShuttingDown)
        ));

        handles.reopen_after_shutdown();
        let second = handles
            .register_actor(RuntimeId::DEFAULT, 72)
            .expect("reopened session route");
        assert_ne!(second, first);
        assert_eq!(handles.resolve_actor(RuntimeId::DEFAULT, second), Some(72));
    }

    #[test]
    fn publication_lease_reserves_at_most_one_actor_route() {
        let handles = LocalHandles::new();
        let publication = handles.begin_publication().expect("publication lease");
        let token = publication
            .register_actor(RuntimeId::DEFAULT, 81)
            .expect("first reservation");
        assert_eq!(
            publication.register_actor(RuntimeId::DEFAULT, 82),
            Err(LocalHandleError::PublicationAlreadyUsed)
        );
        assert_eq!(
            publication.retire_actor(token, 81),
            RetireActorResult::Retired
        );
    }

    #[test]
    fn shutdown_rejects_publication_that_reaches_gate_after_close() {
        let handles = std::sync::Arc::new(LocalHandles::new());
        let entered = std::sync::Arc::new(std::sync::Barrier::new(2));
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        handles.install_admission_hook(
            std::sync::Arc::clone(&entered),
            std::sync::Arc::clone(&release),
        );

        let publisher_handles = std::sync::Arc::clone(&handles);
        let publisher = std::thread::spawn(move || {
            matches!(
                publisher_handles.begin_publication(),
                Err(LocalHandleError::ShuttingDown)
            )
        });
        entered.wait();

        // The late publisher has not acquired the gate. Shutdown closes it and
        // observes no admitted publications in the same critical section.
        handles.begin_shutdown();
        release.wait();
        assert!(publisher.join().expect("late publisher"));
        assert_eq!(handles.counts(), (0, 0));
    }

    #[test]
    fn owner_runtime_is_part_of_route_authority() {
        let handles = LocalHandles::new();
        let owner = RuntimeId::DEFAULT;
        let foreign = RuntimeId(9);
        let token = handles
            .register_actor(owner, 91)
            .expect("direct actor route");

        assert_eq!(handles.resolve_actor(owner, token), Some(91));
        assert_eq!(handles.resolve_actor(foreign, token), None);
        assert_eq!(handles.retire_actor(token, 92), RetireActorResult::Mismatch);
        assert_eq!(handles.resolve_actor(owner, token), Some(91));
    }
}
