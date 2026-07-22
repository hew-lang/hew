//! Stable identities for process-local actor and supervisor handles.

use std::cell::Cell;
use std::collections::HashMap;
#[cfg(test)]
use std::sync::atomic::AtomicBool;
use std::sync::atomic::{AtomicUsize, Ordering};
#[cfg(not(target_arch = "wasm32"))]
use std::sync::Arc;
use std::sync::{Condvar, Mutex, PoisonError};
#[cfg(not(target_arch = "wasm32"))]
use std::time::{Duration, Instant};

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
    #[cfg(not(target_arch = "wasm32"))]
    Supervisor {
        runtime_id: RuntimeId,
        control_id: HewLocalPidId,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum LocalHandleError {
    Exhausted,
    ShuttingDown,
    ActorAlreadyRegistered,
    #[cfg(not(target_arch = "wasm32"))]
    SupervisorAlreadyRegistered,
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
    #[cfg(not(target_arch = "wasm32"))]
    controls: HashMap<HewLocalPidId, Arc<SupervisorControl>>,
    #[cfg(not(target_arch = "wasm32"))]
    supervisor_tokens: HashMap<usize, HewLocalPidId>,
}

#[cfg(not(target_arch = "wasm32"))]
const SUPERVISOR_CLOSING_BIT: usize = 1usize << (usize::BITS - 1);
#[cfg(not(target_arch = "wasm32"))]
const SUPERVISOR_PIN_MASK: usize = !SUPERVISOR_CLOSING_BIT;

/// Stable liveness authority for one supervisor allocation.
///
/// The high state bit closes admission; the remaining bits count operations
/// that may dereference the allocation. Reclamation starts only after the
/// direct route is retired and this count drains to zero.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct SupervisorControl {
    supervisor_addr: usize,
    direct_id: HewLocalPidId,
    runtime_id: RuntimeId,
    access_state: AtomicUsize,
    drain_mutex: Mutex<()>,
    drained: Condvar,
}

#[cfg(not(target_arch = "wasm32"))]
impl SupervisorControl {
    fn new(
        supervisor: *mut crate::supervisor::HewSupervisor,
        direct_id: HewLocalPidId,
        runtime_id: RuntimeId,
    ) -> Self {
        Self {
            supervisor_addr: supervisor as usize,
            direct_id,
            runtime_id,
            access_state: AtomicUsize::new(0),
            drain_mutex: Mutex::new(()),
            drained: Condvar::new(),
        }
    }

    pub(crate) fn supervisor(&self) -> *mut crate::supervisor::HewSupervisor {
        self.supervisor_addr as *mut crate::supervisor::HewSupervisor
    }

    pub(crate) fn direct_id(&self) -> HewLocalPidId {
        self.direct_id
    }

    pub(crate) fn runtime_id(&self) -> RuntimeId {
        self.runtime_id
    }

    pub(crate) fn try_pin(self: &Arc<Self>) -> Option<SupervisorPin> {
        let mut state = self.access_state.load(Ordering::Acquire);
        loop {
            if state & SUPERVISOR_CLOSING_BIT != 0
                || state & SUPERVISOR_PIN_MASK == SUPERVISOR_PIN_MASK
            {
                return None;
            }
            match self.access_state.compare_exchange_weak(
                state,
                state + 1,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => return Some(SupervisorPin(Arc::clone(self))),
                Err(observed) => state = observed,
            }
        }
    }

    /// Close new admission. Returns true only to the linearization winner.
    pub(crate) fn begin_close(&self) -> bool {
        self.access_state
            .fetch_or(SUPERVISOR_CLOSING_BIT, Ordering::AcqRel)
            & SUPERVISOR_CLOSING_BIT
            == 0
    }

    pub(crate) fn wait_for_pins(&self, timeout: Duration) -> bool {
        let deadline = Instant::now().checked_add(timeout);
        let mut guard = self
            .drain_mutex
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        loop {
            if self.access_state.load(Ordering::Acquire) & SUPERVISOR_PIN_MASK == 0 {
                return true;
            }
            let Some(deadline) = deadline else {
                return false;
            };
            let Some(remaining) = deadline.checked_duration_since(Instant::now()) else {
                return false;
            };
            let (next, result) = self
                .drained
                .wait_timeout(guard, remaining)
                .unwrap_or_else(PoisonError::into_inner);
            guard = next;
            if result.timed_out()
                && self.access_state.load(Ordering::Acquire) & SUPERVISOR_PIN_MASK != 0
            {
                return false;
            }
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct SupervisorPin(Arc<SupervisorControl>);

#[cfg(not(target_arch = "wasm32"))]
impl SupervisorPin {
    pub(crate) fn supervisor(&self) -> *mut crate::supervisor::HewSupervisor {
        self.0.supervisor()
    }

    pub(crate) fn control(&self) -> Arc<SupervisorControl> {
        Arc::clone(&self.0)
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl Drop for SupervisorPin {
    fn drop(&mut self) {
        let previous = self.0.access_state.fetch_sub(1, Ordering::AcqRel);
        debug_assert_ne!(previous & SUPERVISOR_PIN_MASK, 0);
        if previous & SUPERVISOR_CLOSING_BIT != 0 && previous & SUPERVISOR_PIN_MASK == 1 {
            let _guard = self
                .0
                .drain_mutex
                .lock()
                .unwrap_or_else(PoisonError::into_inner);
            self.0.drained.notify_all();
        }
    }
}

/// Runtime-owned semantic route authority for local handle values.
///
/// The registry stores IDs only, never actor pointers. Callers copy a route,
/// release this lock, then resolve and pin the `ActorId` through `LiveActors`.
pub(crate) struct LocalHandles {
    state: PoisonSafe<LocalHandleState>,
    publication_gate: PublicationGate,
    #[cfg(not(target_arch = "wasm32"))]
    supervisor_teardown_gate: SupervisorTeardownGate,
    #[cfg(test)]
    fail_next_registration: AtomicBool,
    #[cfg(test)]
    admission_hook: Mutex<AdmissionHook>,
    #[cfg(all(test, not(target_arch = "wasm32")))]
    supervisor_teardown_drain_hook: Mutex<AdmissionHook>,
}

struct PublicationGate {
    state: Mutex<PublicationGateState>,
    idle: Condvar,
}

struct PublicationGateState {
    accepting: bool,
    in_flight: usize,
}

#[cfg(not(target_arch = "wasm32"))]
struct SupervisorTeardownGate {
    state: Mutex<SupervisorTeardownGateState>,
    idle: Condvar,
}

#[cfg(not(target_arch = "wasm32"))]
struct SupervisorTeardownGateState {
    accepting: bool,
    in_flight: usize,
}

/// One destructor owner that remains visible after its access pin and shutdown
/// root are released. Runtime cleanup closes admission and drains these leases
/// before it can reclaim actors, controls, or the owning `RuntimeInner`.
#[cfg(not(target_arch = "wasm32"))]
#[derive(Clone)]
pub(crate) struct SupervisorTeardownLease {
    _owner: Arc<SupervisorTeardownOwner>,
}

#[cfg(not(target_arch = "wasm32"))]
struct SupervisorTeardownOwner {
    handles: *const LocalHandles,
}

// SAFETY: runtime cleanup cannot drop `LocalHandles` until every lease has
// released itself through the gate. The pointed-to authority therefore outlives
// a lease moved to a deferred destructor thread.
#[cfg(not(target_arch = "wasm32"))]
unsafe impl Send for SupervisorTeardownOwner {}
// SAFETY: the owner only uses the pointed-to gate through its internally
// synchronized mutex and condvar, under the same cleanup lifetime guarantee.
#[cfg(not(target_arch = "wasm32"))]
unsafe impl Sync for SupervisorTeardownOwner {}

#[cfg(not(target_arch = "wasm32"))]
impl Drop for SupervisorTeardownOwner {
    fn drop(&mut self) {
        // SAFETY: the cleanup drain described above keeps the authority live.
        let handles = unsafe { &*self.handles };
        let mut state = handles
            .supervisor_teardown_gate
            .state
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        state.in_flight = state
            .in_flight
            .checked_sub(1)
            .expect("supervisor teardown count underflow");
        if state.in_flight == 0 {
            handles.supervisor_teardown_gate.idle.notify_all();
        }
    }
}

#[cfg(test)]
type AdmissionHook = Option<(
    std::sync::Arc<std::sync::Barrier>,
    std::sync::Arc<std::sync::Barrier>,
)>;

/// In-flight handle-publication lease. Shutdown waits for every lease to drop
/// before draining actors or supervisors, so neither kind can be published
/// across the cleanup admission boundary.
pub(crate) struct LocalHandlePublication<'a> {
    handles: &'a LocalHandles,
    registered: Cell<bool>,
}

impl Drop for LocalHandlePublication<'_> {
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
            .expect("local handle publication count underflow");
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
            #[cfg(not(target_arch = "wasm32"))]
            supervisor_teardown_gate: SupervisorTeardownGate {
                state: Mutex::new(SupervisorTeardownGateState {
                    accepting: true,
                    in_flight: 0,
                }),
                idle: Condvar::new(),
            },
            #[cfg(test)]
            fail_next_registration: AtomicBool::new(false),
            #[cfg(test)]
            admission_hook: Mutex::new(None),
            #[cfg(all(test, not(target_arch = "wasm32")))]
            supervisor_teardown_drain_hook: Mutex::new(None),
        }
    }

    pub(crate) fn begin_publication(&self) -> Result<LocalHandlePublication<'_>, LocalHandleError> {
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
        Ok(LocalHandlePublication {
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

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn begin_supervisor_teardown(&self) -> Option<SupervisorTeardownLease> {
        let mut state = self
            .supervisor_teardown_gate
            .state
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        if !state.accepting {
            return None;
        }
        state.in_flight = state.in_flight.checked_add(1)?;
        Some(SupervisorTeardownLease {
            _owner: Arc::new(SupervisorTeardownOwner {
                handles: std::ptr::from_ref(self),
            }),
        })
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn close_supervisor_teardown_admission(&self) {
        self.supervisor_teardown_gate
            .state
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .accepting = false;
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn wait_for_supervisor_teardowns(&self, timeout: Duration) -> bool {
        let deadline = Instant::now().checked_add(timeout);
        let mut state = self
            .supervisor_teardown_gate
            .state
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        #[cfg(test)]
        if state.in_flight != 0 {
            drop(state);
            self.run_supervisor_teardown_drain_hook();
            state = self
                .supervisor_teardown_gate
                .state
                .lock()
                .unwrap_or_else(PoisonError::into_inner);
        }
        while state.in_flight != 0 {
            let Some(deadline) = deadline else {
                return false;
            };
            let Some(remaining) = deadline.checked_duration_since(Instant::now()) else {
                return false;
            };
            let (next, result) = self
                .supervisor_teardown_gate
                .idle
                .wait_timeout(state, remaining)
                .unwrap_or_else(PoisonError::into_inner);
            state = next;
            if result.timed_out() && state.in_flight != 0 {
                return false;
            }
        }
        true
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

    #[cfg(all(test, not(target_arch = "wasm32")))]
    fn run_supervisor_teardown_drain_hook(&self) {
        let hook = self
            .supervisor_teardown_drain_hook
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .take();
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

    #[cfg(all(test, not(target_arch = "wasm32")))]
    fn install_supervisor_teardown_drain_hook(
        &self,
        entered: std::sync::Arc<std::sync::Barrier>,
        release: std::sync::Arc<std::sync::Barrier>,
    ) {
        *self
            .supervisor_teardown_drain_hook
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

    #[cfg(not(target_arch = "wasm32"))]
    fn register_supervisor_reserved(
        &self,
        runtime_id: RuntimeId,
        supervisor: *mut crate::supervisor::HewSupervisor,
    ) -> Result<HewLocalPidId, LocalHandleError> {
        let supervisor_addr = supervisor as usize;
        self.state.access(|state| {
            if state.supervisor_tokens.contains_key(&supervisor_addr) {
                return Err(LocalHandleError::SupervisorAlreadyRegistered);
            }
            let token = allocate().ok_or(LocalHandleError::Exhausted)?;
            let control = Arc::new(SupervisorControl::new(supervisor, token, runtime_id));
            match state.routes.entry(token) {
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(Route::Supervisor {
                        runtime_id,
                        control_id: token,
                    });
                }
                std::collections::hash_map::Entry::Occupied(_) => {
                    return Err(LocalHandleError::IdentityCollision);
                }
            }
            state.controls.insert(token, control);
            state.supervisor_tokens.insert(supervisor_addr, token);
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

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn pin_supervisor(
        &self,
        runtime_id: RuntimeId,
        token: HewLocalPidId,
    ) -> Option<SupervisorPin> {
        let control = self.state.access(|state| match state.routes.get(&token) {
            Some(Route::Supervisor {
                runtime_id: owner,
                control_id,
            }) if *owner == runtime_id && *control_id == token => {
                state.controls.get(control_id).cloned()
            }
            _ => None,
        })?;
        control.try_pin()
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn supervisor_control_for_raw(
        &self,
        token: HewLocalPidId,
        supervisor: *mut crate::supervisor::HewSupervisor,
    ) -> Option<Arc<SupervisorControl>> {
        self.state.access(|state| {
            let control = state.controls.get(&token)?;
            (control.supervisor() == supervisor).then(|| Arc::clone(control))
        })
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn retire_supervisor_route(&self, control: &SupervisorControl) {
        self.state.access(|state| {
            if matches!(
                state.routes.get(&control.direct_id()),
                Some(Route::Supervisor { runtime_id, control_id })
                    if *runtime_id == control.runtime_id()
                        && *control_id == control.direct_id()
            ) {
                state.routes.remove(&control.direct_id());
            }
            if state.supervisor_tokens.get(&control.supervisor_addr) == Some(&control.direct_id()) {
                state.supervisor_tokens.remove(&control.supervisor_addr);
            }
        });
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn remove_supervisor_control(&self, control: &SupervisorControl) {
        self.state.access(|state| {
            let matches = state
                .controls
                .get(&control.direct_id())
                .is_some_and(|stored| std::ptr::eq(stored.as_ref(), control));
            if matches {
                state.controls.remove(&control.direct_id());
            }
        });
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn close_supervisors_for_cleanup(&self, timeout: Duration) -> bool {
        let controls = self
            .state
            .access(|state| state.controls.values().cloned().collect::<Vec<_>>());
        for control in &controls {
            control.begin_close();
            self.retire_supervisor_route(control);
        }
        controls
            .iter()
            .all(|control| control.wait_for_pins(timeout))
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn supervisor_counts(&self) -> (usize, usize) {
        self.state.access(|state| {
            let routes = state
                .routes
                .values()
                .filter(|route| matches!(route, Route::Supervisor { .. }))
                .count();
            (routes, state.controls.len())
        })
    }
}

impl LocalHandlePublication<'_> {
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

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn register_supervisor(
        &self,
        runtime_id: RuntimeId,
        supervisor: *mut crate::supervisor::HewSupervisor,
    ) -> Result<HewLocalPidId, LocalHandleError> {
        if self.registered.replace(true) {
            return Err(LocalHandleError::PublicationAlreadyUsed);
        }
        self.handles
            .register_supervisor_reserved(runtime_id, supervisor)
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
) -> Result<LocalHandlePublication<'static>, LocalHandleError> {
    current_handles()
        .ok_or(LocalHandleError::ShuttingDown)?
        .begin_publication()
}

/// Start one actor publication in an explicitly selected runtime authority.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn begin_actor_publication_in(
    handles: &LocalHandles,
) -> Result<LocalHandlePublication<'_>, LocalHandleError> {
    handles.begin_publication()
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn begin_supervisor_publication_in(
    handles: &LocalHandles,
) -> Result<LocalHandlePublication<'_>, LocalHandleError> {
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

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn close_current_supervisors_for_cleanup(timeout: Duration) -> bool {
    current_handles().is_none_or(|handles| handles.close_supervisors_for_cleanup(timeout))
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn close_current_supervisor_teardown_admission() {
    if let Some(handles) = current_handles() {
        handles.close_supervisor_teardown_admission();
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn wait_for_current_supervisor_teardowns(timeout: Duration) -> bool {
    current_handles().is_none_or(|handles| handles.wait_for_supervisor_teardowns(timeout))
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn assert_current_supervisors_empty() {
    let counts = current_handles().map_or((0, 0), LocalHandles::supervisor_counts);
    assert_eq!(
        counts,
        (0, 0),
        "local supervisor routes or controls survived runtime cleanup"
    );
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

#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) fn current_supervisor_counts_for_test() -> (usize, usize) {
    current_handles().map_or((0, 0), LocalHandles::supervisor_counts)
}

#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) fn current_supervisor_teardown_state_for_test() -> (bool, usize) {
    current_handles().map_or((false, 0), |handles| {
        let state = handles
            .supervisor_teardown_gate
            .state
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        (state.accepting, state.in_flight)
    })
}

#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) fn install_current_supervisor_teardown_drain_hook_for_test(
    entered: std::sync::Arc<std::sync::Barrier>,
    release: std::sync::Arc<std::sync::Barrier>,
) {
    current_handles()
        .expect("runtime authority for supervisor teardown drain hook")
        .install_supervisor_teardown_drain_hook(entered, release);
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn pin_current_supervisor(token: HewLocalPidId) -> Option<SupervisorPin> {
    let runtime = crate::runtime::rt_current_opt()?;
    runtime
        .local_handles
        .pin_supervisor(runtime.runtime_id(), token)
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn begin_current_supervisor_teardown() -> Option<SupervisorTeardownLease> {
    current_handles()?.begin_supervisor_teardown()
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn close_current_supervisor(control: &SupervisorControl) -> bool {
    let Some(runtime) = crate::runtime::rt_current_opt() else {
        return false;
    };
    if runtime.runtime_id() != control.runtime_id() {
        return false;
    }
    let won_close = control.begin_close();
    runtime.local_handles.retire_supervisor_route(control);
    won_close
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
