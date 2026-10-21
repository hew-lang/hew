//! Node-instance observation. Canonical membership and names stay in their owners.

use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};

use hew_cabi::string::{string_from_str, HewString};

use super::{with_current_node_read, HewNode, NODE_STATE_RUNNING};
use crate::channel_core::ChannelCore;
use crate::node_identity::{HewLocation, HewNodeId};
use crate::util::MutexExt;

const SUBSCRIPTION_LIMIT: usize = 64;

#[derive(Debug, Default)]
struct Observers {
    subscribers: Vec<Weak<Subscription>>,
    closed: bool,
}

#[derive(Debug, Default)]
pub(crate) struct NodeObservation {
    observers: Mutex<Observers>,
}

impl NodeObservation {
    fn subscribe(self: &Arc<Self>) -> Result<Arc<Subscription>, i64> {
        let mut observers = self.observers.lock_or_recover();
        if observers.closed {
            return Err(1);
        }
        observers
            .subscribers
            .retain(|entry| entry.strong_count() != 0);
        if observers.subscribers.len() >= SUBSCRIPTION_LIMIT {
            return Err(2);
        }
        let subscription = Arc::new(Subscription {
            owner: Arc::clone(self),
            queue: Arc::new(ChannelCore::new(1)),
            extracted: AtomicBool::new(false),
            finished: AtomicBool::new(false),
        });
        observers.subscribers.push(Arc::downgrade(&subscription));
        Ok(subscription)
    }

    /// Readiness coalesces. Wake consumers only after dropping the hub lock.
    pub(crate) fn notify(&self) {
        let subscribers = {
            let mut observers = self.observers.lock_or_recover();
            observers
                .subscribers
                .retain(|entry| entry.strong_count() != 0);
            observers
                .subscribers
                .iter()
                .filter_map(Weak::upgrade)
                .collect::<Vec<_>>()
        };
        for subscriber in subscribers {
            let _ = subscriber.queue.try_send_owned(Vec::new());
        }
    }

    pub(crate) fn finish(&self) {
        let subscribers = {
            let mut observers = self.observers.lock_or_recover();
            observers.closed = true;
            std::mem::take(&mut observers.subscribers)
                .iter()
                .filter_map(Weak::upgrade)
                .collect::<Vec<_>>()
        };
        for subscriber in subscribers {
            subscriber.finish();
        }
    }
}

#[derive(Debug)]
struct Subscription {
    owner: Arc<NodeObservation>,
    queue: Arc<ChannelCore>,
    extracted: AtomicBool,
    finished: AtomicBool,
}

impl Subscription {
    fn finish(&self) {
        if !self.finished.swap(true, Ordering::AcqRel) {
            self.queue.close_sink();
        }
    }
}

impl Drop for Subscription {
    fn drop(&mut self) {
        self.finish();
    }
}

/// Sole source owner of one readiness subscription; contains no node pointer.
#[derive(Debug)]
pub struct HewNodeWatch {
    subscription: Result<Arc<Subscription>, i64>,
}

#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct Member {
    id: HewNodeId,
    session: u32,
    state: i32,
}

#[derive(Debug)]
pub struct HewNodeSnapshot {
    local: Member,
    members: Vec<Member>,
    registrations: Vec<(String, HewLocation)>,
}

#[derive(Debug, Default)]
#[repr(C)]
pub struct Registration {
    name: *mut HewString,
    location: HewLocation,
}

fn with_node<R>(
    owner: Option<&Arc<NodeObservation>>,
    read: impl FnOnce(&HewNode) -> Option<R>,
) -> Option<R> {
    // This read guard excludes CURRENT_NODE's shutdown/free barrier. No callback,
    // readiness receive or teardown wait runs inside the guarded operation.
    with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: CURRENT_NODE's read guard pins this allocation and its children.
        let node = unsafe { &*node };
        if node.state.load(Ordering::Acquire) != NODE_STATE_RUNNING || node.registry.is_null() {
            return None;
        }
        // SAFETY: the pinned node owns its registry.
        let registry = unsafe { &*node.registry };
        if owner.is_some_and(|expected| !Arc::ptr_eq(expected, &registry.observer())) {
            return None;
        }
        read(node)
    })
}

fn snapshot(owner: Option<&Arc<NodeObservation>>) -> *mut HewNodeSnapshot {
    let value = with_node(owner, |node| {
        let local = Member {
            id: node.auth.node_identity()?.into(),
            session: node.auth.session_incarnation()?,
            state: crate::cluster::MEMBER_ALIVE,
        };
        let mut members = vec![local];
        if !node.cluster.is_null() {
            // SAFETY: CURRENT_NODE pins the cluster. Its own snapshot follows
            // sessions -> tokens -> members, as authenticated publication does.
            for (slot, session, state) in unsafe { &*node.cluster }.observation_members() {
                if slot == node.route_slot {
                    continue;
                }
                if let Some(id) = node.auth.node_id_for_route_slot(slot) {
                    members.push(Member {
                        id: id.into(),
                        session,
                        state,
                    });
                }
            }
        }
        members.sort_by_key(|member| (member.id.hi, member.id.lo));
        // SAFETY: the pinned node owns this registry; these are immutable copies
        // of discovery metadata, never a second registry or callable authority.
        let registry = unsafe { &*node.registry };
        let mut registrations = registry
            .remote_names
            .lock_or_recover()
            .iter()
            .map(|(name, location)| (name.clone(), HewLocation::from(*location)))
            .collect::<Vec<_>>();
        registrations.sort_by(|a, b| a.0.cmp(&b.0));
        Some(HewNodeSnapshot {
            local,
            members,
            registrations,
        })
    });
    value.map_or(ptr::null_mut(), |value| Box::into_raw(Box::new(value)))
}

/// Register before taking the initial snapshot, so a racing mutation is either
/// visible in that snapshot or leaves readiness queued. The failed handle also
/// has an owner, allowing ordinary source cleanup without a last-error channel.
#[no_mangle]
pub extern "C" fn hew_node_watch_open() -> *mut HewNodeWatch {
    let subscription = with_node(None, |node| {
        // SAFETY: with_node pins the registry through this bounded registration.
        Some(unsafe { &*node.registry }.observer().subscribe())
    })
    .unwrap_or(Err(1));
    Box::into_raw(Box::new(HewNodeWatch { subscription }))
}

/// # Safety
/// `watch` is the live watch handle owned by the caller.
#[no_mangle]
pub unsafe extern "C" fn hew_node_watch_status(watch: *const HewNodeWatch) -> i64 {
    // SAFETY: caller owns the live handle, when non-null.
    unsafe { watch.as_ref() }.map_or(1, |watch| {
        watch.subscription.as_ref().err().copied().unwrap_or(0)
    })
}

/// Transfer this subscription's unique readable stream exactly once.
/// # Safety
/// `watch` is live and borrowed for this call. The returned pair is owned.
#[no_mangle]
pub unsafe extern "C" fn hew_node_watch_changes(
    watch: *const HewNodeWatch,
) -> *mut crate::stream::HewStreamPair {
    // SAFETY: caller owns the live watch handle, when non-null.
    let Some(subscription) =
        unsafe { watch.as_ref() }.and_then(|watch| watch.subscription.as_ref().ok())
    else {
        return ptr::null_mut();
    };
    if subscription.extracted.swap(true, Ordering::AcqRel) {
        return ptr::null_mut();
    }
    crate::stream::notification_stream(Arc::clone(&subscription.queue))
}

/// # Safety
/// Consume the unique watch handle. Its stream may outlive it and receives EOF.
#[no_mangle]
pub unsafe extern "C" fn hew_node_watch_free(watch: *mut HewNodeWatch) {
    if !watch.is_null() {
        // SAFETY: ownership transfers from the caller exactly once.
        drop(unsafe { Box::from_raw(watch) });
    }
}

/// Copy current canonical discovery metadata; null when no node is running.
#[no_mangle]
pub extern "C" fn hew_node_snapshot_current() -> *mut HewNodeSnapshot {
    snapshot(None)
}

/// # Safety
/// `watch` is borrowed and live; the returned immutable snapshot is owned.
#[no_mangle]
pub unsafe extern "C" fn hew_node_watch_snapshot(
    watch: *const HewNodeWatch,
) -> *mut HewNodeSnapshot {
    // SAFETY: caller owns the live watch handle, when non-null.
    let Some(subscription) =
        unsafe { watch.as_ref() }.and_then(|watch| watch.subscription.as_ref().ok())
    else {
        return ptr::null_mut();
    };
    snapshot(Some(&subscription.owner))
}

#[no_mangle]
pub extern "C" fn hew_node_snapshot_valid(snapshot: *const HewNodeSnapshot) -> bool {
    !snapshot.is_null()
}

/// # Safety
/// `snapshot` is borrowed and live.
#[no_mangle]
pub unsafe extern "C" fn hew_node_snapshot_local(snapshot: *const HewNodeSnapshot) -> Member {
    // SAFETY: caller owns a valid snapshot.
    unsafe { (*snapshot).local }
}

/// # Safety
/// `snapshot` is borrowed and live; `registrations` chooses which list to count.
#[no_mangle]
pub unsafe extern "C" fn hew_node_snapshot_count(
    snapshot: *const HewNodeSnapshot,
    registrations: bool,
) -> i64 {
    // SAFETY: caller owns a valid snapshot.
    let snapshot = unsafe { &*snapshot };
    i64::try_from(if registrations {
        snapshot.registrations.len()
    } else {
        snapshot.members.len()
    })
    .unwrap_or(i64::MAX)
}

/// # Safety
/// `snapshot` is live and `index` lies in its member count.
#[no_mangle]
pub unsafe extern "C" fn hew_node_snapshot_member(
    snapshot: *const HewNodeSnapshot,
    index: i64,
) -> Member {
    // SAFETY: caller checked the immutable snapshot's count before indexing.
    unsafe { (&(*snapshot).members)[usize::try_from(index).expect("checked snapshot index")] }
}

/// # Safety
/// `snapshot` is live and `index` lies in its registration count. The result's
/// name is a fresh managed owner, released by ordinary Hew value cleanup.
#[no_mangle]
pub unsafe extern "C" fn hew_node_snapshot_registration(
    snapshot: *const HewNodeSnapshot,
    index: i64,
) -> Registration {
    // SAFETY: caller checked the immutable snapshot's count before indexing.
    let (name, location) = unsafe {
        &(&(*snapshot).registrations)[usize::try_from(index).expect("checked snapshot index")]
    };
    Registration {
        name: string_from_str(name),
        location: *location,
    }
}

/// # Safety
/// Consume the uniquely owned snapshot, or null after a failed read.
#[no_mangle]
pub unsafe extern "C" fn hew_node_snapshot_free(snapshot: *mut HewNodeSnapshot) {
    if !snapshot.is_null() {
        // SAFETY: ownership transfers from the caller exactly once.
        drop(unsafe { Box::from_raw(snapshot) });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hew_node::tests::{install_test_auth, test_node_id};
    use crate::hew_node::{hew_node_free, hew_node_new, hew_node_start, hew_node_stop};

    #[test]
    fn bounded_readiness_coalesces_and_releases_subscriptions() {
        let owner = Arc::new(NodeObservation::default());
        let mut subscriptions = (0..SUBSCRIPTION_LIMIT)
            .map(|_| owner.subscribe().unwrap())
            .collect::<Vec<_>>();
        assert_eq!(owner.subscribe().unwrap_err(), 2);
        owner.notify();
        owner.notify();
        owner.notify();
        for subscription in &subscriptions {
            assert_eq!(subscription.queue.try_recv(), Some(Vec::new()));
            assert_eq!(subscription.queue.try_recv(), None);
        }
        subscriptions.pop();
        let replacement = owner.subscribe().expect("closed owner returns capacity");
        owner.finish();
        assert_eq!(replacement.queue.blocking_recv(), None);
        assert_eq!(owner.subscribe().unwrap_err(), 1);
    }

    #[test]
    fn snapshot_subscription_windows_and_instance_retirement() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: this test exclusively owns the node, watch and snapshot handles.
        unsafe {
            let node = hew_node_new(51, c"127.0.0.1:0".as_ptr());
            install_test_auth(node, 51);
            assert_eq!(hew_node_start(node), 0);
            let watch = hew_node_watch_open();
            assert_eq!(hew_node_watch_status(watch), 0);
            let subscription = (*watch).subscription.as_ref().unwrap();
            let registration = HewLocation::from(
                crate::node_identity::Location::new(test_node_id(51), 99, 1).unwrap(),
            );
            let registry = &*(*node).registry;
            // Mutation after registration but before the initial read is both
            // in the snapshot and independently ready; neither side loses it.
            super::super::node_registry_gossip_callback(
                c"named".as_ptr(),
                &raw const registration,
                true,
                (*node).registry.cast(),
            );
            let first = hew_node_watch_snapshot(watch);
            assert_eq!(hew_node_snapshot_count(first, true), 1);
            // Each record transfers an independent rc=1 managed name. Freeing
            // those names leaves both the immutable copy and registry intact.
            let one = hew_node_snapshot_registration(first, 0);
            let two = hew_node_snapshot_registration(first, 0);
            assert_ne!(one.name, two.name);
            for name in [one.name, two.name] {
                #[expect(
                    clippy::cast_ptr_alignment,
                    reason = "the managed carrier supplies the aligned atomic field offset"
                )]
                let count = &*name
                    .cast::<u8>()
                    .add(hew_cabi::string::STRING_DEBUG_REFCOUNT_OFFSET)
                    .cast::<std::sync::atomic::AtomicU32>();
                assert_eq!(count.load(Ordering::Acquire), 1);
                assert_eq!(hew_cabi::string::string_as_str(name), "named");
                hew_cabi::string::string_release(name);
            }
            let later = hew_node_snapshot_registration(first, 0);
            assert_eq!(hew_cabi::string::string_as_str(later.name), "named");
            hew_cabi::string::string_release(later.name);
            assert_eq!(subscription.queue.try_recv(), Some(Vec::new()));
            hew_node_snapshot_free(first);
            // Mutation after a read produces the next readiness and exact data.
            super::super::node_registry_gossip_callback(
                c"named".as_ptr(),
                &raw const registration,
                false,
                (*node).registry.cast(),
            );
            assert_eq!(subscription.queue.try_recv(), Some(Vec::new()));
            let after = hew_node_watch_snapshot(watch);
            assert_eq!(hew_node_snapshot_count(after, true), 0);
            hew_node_snapshot_free(after);
            assert!(registry.remote_names.lock_or_recover().is_empty());
            // Repointed registrations and locations from another session do
            // not belong to the retiring actor's exact current Location.
            let actor_id = crate::pid::hew_pid_make(51, 99);
            let successor = crate::node_identity::Location::new(test_node_id(51), 100, 1).unwrap();
            let other_session =
                crate::node_identity::Location::new(test_node_id(51), 99, 2).unwrap();
            registry
                .remote_names
                .lock_or_recover()
                .insert("repointed".into(), successor);
            registry
                .remote_names
                .lock_or_recover()
                .insert("other-session".into(), other_session);
            super::super::retire_current_actor_names(actor_id);
            assert_eq!(
                registry.remote_names.lock_or_recover().get("repointed"),
                Some(&successor)
            );
            assert_eq!(
                registry.remote_names.lock_or_recover().get("other-session"),
                Some(&other_session)
            );
            assert_eq!(hew_node_stop(node), 0);
            while subscription.queue.try_recv().is_some() {}
            let (_ready, waker) = crate::wake::blocking::Readiness::new();
            assert_eq!(subscription.queue.poll_recv_ready(&Arc::new(waker)), 2);
            assert!(hew_node_watch_snapshot(watch).is_null());
            assert_eq!(hew_node_start(node), 0);
            assert!(
                hew_node_watch_snapshot(watch).is_null(),
                "a reused node allocation must not revive its old subscription"
            );
            let current = hew_node_snapshot_current();
            assert!(!current.is_null());
            hew_node_snapshot_free(current);
            hew_node_watch_free(watch);
            hew_node_free(node);
        }
    }

    #[test]
    fn eof_follows_join_and_snapshot_does_not_wait_on_shutdown() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: the test owns the node and transfers only stop execution to
        // the worker. The allocation stays live until that worker is joined.
        unsafe {
            let node = hew_node_new(52, c"127.0.0.1:0".as_ptr());
            install_test_auth(node, 52);
            assert_eq!(hew_node_start(node), 0);
            let watch = hew_node_watch_open();
            let subscription = (*watch).subscription.as_ref().unwrap();
            super::super::stop_node_accept_thread(&mut *node);
            let (release, blocked) = std::sync::mpsc::channel();
            *(*node).accept_thread.lock_or_recover() = Some(std::thread::spawn(move || {
                blocked.recv().unwrap();
            }));
            let address = node as usize;
            let stop = std::thread::spawn(move || hew_node_stop(address as *mut HewNode));
            // State observation is the rendezvous; the test has no speed assertion.
            while (*node).state.load(Ordering::Acquire) == NODE_STATE_RUNNING {
                std::thread::yield_now();
            }
            assert!(hew_node_watch_snapshot(watch).is_null());
            let (_ready, waker) = crate::wake::blocking::Readiness::new();
            assert_eq!(
                subscription.queue.poll_recv_ready(&Arc::new(waker)),
                0,
                "teardown is still blocked on its owned worker"
            );
            // Stop holds KNOWN_NODES while blocked on its owned worker. Actor
            // terminal retirement must not wait on that same teardown lock.
            while super::super::with_current_node_read(|current| *current != 0) {
                std::thread::yield_now();
            }
            let (retired, retired_rx) = std::sync::mpsc::channel();
            let retire = std::thread::spawn(move || {
                super::super::retire_current_actor_names(crate::pid::hew_pid_make(52, 99));
                retired.send(()).unwrap();
            });
            let completed = retired_rx.recv_timeout(std::time::Duration::from_secs(10));
            release.send(()).unwrap();
            assert_eq!(stop.join().unwrap(), 0);
            retire.join().unwrap();
            assert!(
                completed.is_ok(),
                "actor retirement waited for node teardown"
            );
            while subscription.queue.try_recv().is_some() {}
            let (_ready, waker) = crate::wake::blocking::Readiness::new();
            assert_eq!(subscription.queue.poll_recv_ready(&Arc::new(waker)), 2);
            hew_node_watch_free(watch);
            hew_node_free(node);
        }
    }
}
