//! Generic owned-operation adapter for the existing readiness loop.

use std::io;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use crate::async_io::{AcceptedConnection, HewAsyncIo, IoFailure, IoProducer, IoValue};

use super::{
    deliver_orphan_close, ensure_reactor_started, unregister_fd, ActorIncarnation, HewIoPoller,
    Pending, RegMode, Registration, HEW_IO_ERROR, HEW_IO_HUP, HEW_IO_READ,
    LISTENER_ADMISSION_CLOSED, REACTOR_STATE, REACTOR_STOP,
};

static IN_FLIGHT: AtomicUsize = AtomicUsize::new(0);

/// Keep shutdown's idle probe false between registry removal and readiness
/// notification, and while a queued add is being promoted. The guard is taken
/// under the registry lock before that work leaves the registry/pending queue.
pub(super) struct Flight;

impl Flight {
    pub(super) fn new() -> Self {
        IN_FLIGHT.fetch_add(1, Ordering::SeqCst);
        Self
    }
}

impl Drop for Flight {
    fn drop(&mut self) {
        IN_FLIGHT.fetch_sub(1, Ordering::SeqCst);
    }
}

pub(super) fn in_flight() -> bool {
    IN_FLIGHT.load(Ordering::SeqCst) != 0
}

pub(crate) fn reactor_await_async_io(
    handle: i32,
    accept: bool,
    operation: Arc<HewAsyncIo>,
) -> Result<(), IoFailure> {
    if crate::runtime::rt_current_opt().is_none() {
        return Err(IoFailure::invalid(
            "asynchronous TCP I/O requires an installed runtime",
        ));
    }
    if REACTOR_STOP.load(Ordering::SeqCst) || LISTENER_ADMISSION_CLOSED.load(Ordering::SeqCst) {
        return Err(IoFailure::from_io(
            "register TCP I/O",
            &io::Error::from_raw_os_error(libc::ECANCELED),
        ));
    }
    let fd = if accept {
        crate::transport::tcp_listener_raw_fd(handle)
    } else {
        crate::transport::tcp_conn_raw_fd(handle)
    }
    .ok_or_else(|| {
        IoFailure::from_io(
            "register TCP I/O",
            &io::Error::from_raw_os_error(libc::EBADF),
        )
    })?;
    let nonblocking = if accept {
        crate::transport::tcp_listener_set_nonblocking_result(handle, true)
    } else {
        crate::transport::tcp_conn_set_nonblocking_result(handle, true)
    };
    nonblocking.map_err(|error| IoFailure::from_io("set TCP nonblocking", &error))?;
    if !ensure_reactor_started() {
        return Err(IoFailure::from_io(
            "start TCP reactor",
            &io::Error::other("reactor unavailable"),
        ));
    }
    let operation = IoProducer::new(operation);
    let queued = REACTOR_STATE.access(|state| {
        if REACTOR_STOP.load(Ordering::SeqCst) || LISTENER_ADMISSION_CLOSED.load(Ordering::SeqCst) {
            return Err(IoFailure::from_io(
                "register TCP I/O",
                &io::Error::from_raw_os_error(libc::ECANCELED),
            ));
        }
        let occupied = state.registry.values().any(|reg| reg.conn == handle)
            || state.pending.iter().any(|request| {
                matches!(request,
                Pending::Add { reg, .. } if reg.conn == handle)
            });
        if occupied {
            return Err(IoFailure::from_io(
                "TCP handle already has pending I/O",
                &io::Error::from_raw_os_error(libc::EBUSY),
            ));
        }
        state.pending.push(Pending::Add {
            fd,
            reg: Registration::new(
                handle,
                crate::transport::HewActorRef {
                    kind: 0,
                    data: crate::transport::HewActorRefData {
                        local: std::ptr::null_mut(),
                    },
                },
                ActorIncarnation::NONE,
                RegMode::AsyncIo { operation, accept },
            ),
        });
        Ok(())
    });
    queued
}

/// Recheck after promotion leaves the queue. A second start can race that
/// interval; it must not replace an existing operation or its poller entry.
pub(super) fn admit_registration(fd: i32, reg: &Registration) -> bool {
    if let RegMode::AsyncIo { operation, .. } = &reg.mode {
        if !operation.is_pending() {
            return false;
        }
    }
    let conflict = REACTOR_STATE.access(|state| {
        state.registry.get(&fd).is_some_and(|existing| {
            matches!(reg.mode, RegMode::AsyncIo { .. })
                || matches!(existing.mode, RegMode::AsyncIo { .. })
        })
    });
    if conflict {
        if let RegMode::AsyncIo { operation, .. } = &reg.mode {
            operation.complete(Err(IoFailure::from_io(
                "TCP handle already has pending I/O",
                &io::Error::from_raw_os_error(libc::EBUSY),
            )));
        } else {
            deliver_orphan_close(reg);
        }
        return false;
    }
    true
}

/// Detach by operation identity, not by a recyclable fd number. Queue the
/// poller removal under the same lock that removes its registration, before
/// the caller can close its borrowed resource and make the fd reusable.
pub(crate) fn reactor_detach_async_io(operation: &HewAsyncIo) {
    let evicted = REACTOR_STATE.access(|state| {
        let belongs = |reg: &Registration| {
            matches!(&reg.mode,
            RegMode::AsyncIo { operation: current, .. } if std::ptr::eq(&raw const **current, std::ptr::from_ref(operation)))
        };
        let fds: Vec<i32> = state
            .registry
            .iter()
            .filter_map(|(fd, reg)| belongs(reg).then_some(*fd))
            .collect();
        let mut evicted = Vec::new();
        for fd in &fds {
            if let Some(registration) = state.registry.remove(fd) {
                evicted.push(registration);
            }
            state.conn_to_fd.retain(|_, current| current != fd);
            state.pending.push(Pending::UnregisterFd { fd: *fd });
        }
        let pending = std::mem::take(&mut state.pending);
        for request in pending {
            match request {
                Pending::Add { reg, .. } if belongs(&reg) => evicted.push(reg),
                other => state.pending.push(other),
            }
        }
        if !fds.is_empty() {
            crate::observe::record_reactor_unregistration(fds.len() as u64);
        }
        evicted
    });
    drop(evicted);
}

pub(super) fn handle_ready(
    poller: *mut HewIoPoller,
    fd: i32,
    handle: i32,
    events: i32,
    accept: bool,
    operation: &HewAsyncIo,
) {
    if !operation.is_pending() {
        unregister_fd(poller, fd);
        return;
    }
    if events & (HEW_IO_READ | HEW_IO_HUP | HEW_IO_ERROR) == 0 {
        return;
    }
    let closed = events & (HEW_IO_HUP | HEW_IO_ERROR) != 0;
    let result = if accept {
        match crate::transport::tcp_listener_accept_nonblocking_result(handle) {
            Ok(crate::transport::AcceptOutcome::Accepted(connection)) => {
                Ok(IoValue::Connection(AcceptedConnection(connection)))
            }
            Ok(crate::transport::AcceptOutcome::WouldBlock) if !closed => return,
            Ok(crate::transport::AcceptOutcome::WouldBlock) => Err(IoFailure::from_io(
                "accept TCP connection",
                &io::Error::other("listener closed during readiness"),
            )),
            Ok(crate::transport::AcceptOutcome::Closed) => Err(IoFailure::from_io(
                "accept TCP connection",
                &io::Error::from_raw_os_error(libc::EBADF),
            )),
            Err(error) => Err(IoFailure::from_io("accept TCP connection", &error)),
        }
    } else {
        match crate::transport::tcp_conn_read_available_result(handle) {
            Ok(crate::transport::ActiveReadOutcome::Data(bytes)) => Ok(IoValue::Bytes(bytes)),
            Ok(crate::transport::ActiveReadOutcome::Eof) => Ok(IoValue::Bytes(Vec::new())),
            Ok(crate::transport::ActiveReadOutcome::WouldBlock) if !closed => return,
            Ok(crate::transport::ActiveReadOutcome::WouldBlock) => Err(IoFailure::from_io(
                "read TCP connection",
                &io::Error::other("connection closed during readiness"),
            )),
            Ok(crate::transport::ActiveReadOutcome::Closed) => Err(IoFailure::from_io(
                "read TCP connection",
                &io::Error::from_raw_os_error(libc::EBADF),
            )),
            Err(error) => Err(IoFailure::from_io("read TCP connection", &error)),
        }
    };
    // Remove readiness first. The woken consumer may immediately start another
    // read on this handle; it must not race an old unregister after its new add.
    unregister_fd(poller, fd);
    operation.complete(result);
}

pub(super) fn cancel_all() -> usize {
    let mut cancelled = 0;
    loop {
        let operations: Vec<IoProducer> = REACTOR_STATE.access(|state| {
            state
                .registry
                .values()
                .chain(state.pending.iter().filter_map(|request| {
                    if let Pending::Add { reg, .. } = request {
                        Some(reg)
                    } else {
                        None
                    }
                }))
                .filter_map(|reg| {
                    if let RegMode::AsyncIo { operation, .. } = &reg.mode {
                        Some(operation.clone())
                    } else {
                        None
                    }
                })
                .collect()
        });
        let empty = operations.is_empty();
        for operation in operations {
            cancelled += usize::from(operation.cancel_and_wake());
        }
        // Promotion can briefly own a pending registration outside the map.
        // Once it settles, repeat the scrub before declaring the source quiet.
        if empty && !in_flight() {
            return cancelled;
        }
        std::thread::yield_now();
    }
}
