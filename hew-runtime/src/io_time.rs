//! File I/O, sleep, clock, and I/O poller for the Hew runtime.
//!
//! Provides `hew_read_file`, `hew_sleep_ms`, `hew_sleep_ns`, `hew_sleep_until_ns`,
//! `hew_now_ms`, duration helpers,
//! and a platform I/O poller (epoll on Linux, kqueue on FreeBSD/macOS, stub
//! elsewhere).
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

#[cfg(any(target_os = "linux", target_os = "freebsd", target_os = "macos"))]
use std::ffi::c_void;
use std::ffi::{c_char, c_int, CStr};

// ---------------------------------------------------------------------------
// Duration
// ---------------------------------------------------------------------------

pub use crate::duration::{hew_milliseconds, hew_seconds, HewDuration};

// ---------------------------------------------------------------------------
// File I/O
// ---------------------------------------------------------------------------

/// Read an entire file and return a `malloc`-allocated C string.
///
/// Returns a null pointer on failure.
///
/// # Safety
///
/// `path` must be a valid, NUL-terminated C string. The caller is responsible
/// for calling `free()` on the returned pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_read_file(path: *const c_char) -> *mut c_char {
    cabi_guard!(path.is_null(), std::ptr::null_mut());
    // SAFETY: caller guarantees `path` is a valid C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return std::ptr::null_mut();
    };
    let Ok(contents) = std::fs::read_to_string(rust_path) else {
        return std::ptr::null_mut();
    };
    // Header-aware (S1): the result reaches hew_string_drop / free_cstring.
    let buf = crate::cabi::alloc_cstring_from_str(&contents); // CSTRING-ALLOC: str-open (hew_read_file — header-aware String result; reaches hew_string_drop)
    if buf.is_null() {
        return std::ptr::null_mut();
    }
    buf
}

// ---------------------------------------------------------------------------
// Sleep / Clock
// ---------------------------------------------------------------------------

/// Sleep for `ns` nanoseconds (the `sleep(duration)` ABI).
///
/// Called by the blocking (free-fn) path. Suspending actor callers are
/// intercepted at the MIR lowering stage and arm the timer wheel directly.
///
/// # Safety
///
/// No preconditions — delegates to the OS.
#[no_mangle]
pub unsafe extern "C" fn hew_sleep_ns(ns: i64) {
    if ns > 0 {
        // SAFETY: ns > 0 checked above, so cast is lossless.
        #[expect(clippy::cast_sign_loss, reason = "guarded by ns > 0")]
        let dur = std::time::Duration::from_nanos(ns as u64);
        std::thread::sleep(dur);
    }
}

/// Sleep until `instant_ns` (nanosecond monotonic timestamp).
///
/// Computes remaining = `instant_ns - now`; if positive, sleeps that duration.
/// Called by the blocking (free-fn) path. Suspending actor callers are
/// intercepted at MIR lowering and arm the timer wheel for the remaining ms.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_sleep_until_ns(instant_ns: i64) {
    // SAFETY: hew_instant_now has no preconditions.
    let now_ns = unsafe { hew_instant_now() };
    let remaining_ns = instant_ns.saturating_sub(now_ns);
    if remaining_ns > 0 {
        // SAFETY: remaining_ns > 0 checked above.
        #[expect(clippy::cast_sign_loss, reason = "guarded by remaining_ns > 0")]
        let dur = std::time::Duration::from_nanos(remaining_ns as u64);
        std::thread::sleep(dur);
    }
}

/// Cross-platform monotonic clock in milliseconds, anchored on the
/// process-wide epoch ([`crate::monotonic`]).
fn monotonic_ms() -> u64 {
    crate::monotonic::monotonic_ms()
}

/// Return the current monotonic clock time in milliseconds.
///
/// When simulated time is enabled (via [`crate::deterministic::hew_simtime_enable`]),
/// returns the simulated clock value instead of the real clock.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_now_ms() -> u64 {
    // Check simulated time first (testing fast-path).
    if let Some(ms) = crate::deterministic::simtime_now() {
        return ms;
    }

    monotonic_ms()
}

/// Return the current monotonic clock time in nanoseconds.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_instant_now() -> i64 {
    let ms = crate::deterministic::simtime_now().unwrap_or_else(monotonic_ms);
    i64::try_from(ms)
        .unwrap_or(i64::MAX)
        .saturating_mul(1_000_000)
}

/// Return elapsed nanoseconds since `instant_ns`.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_instant_elapsed(instant_ns: i64) -> i64 {
    // SAFETY: hew_instant_now has no preconditions.
    let now = unsafe { hew_instant_now() };
    now.saturating_sub(instant_ns)
}

/// Return the saturating duration between two instant nanosecond stamps.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_instant_duration_since(later_ns: i64, earlier_ns: i64) -> i64 {
    later_ns.saturating_sub(earlier_ns)
}

/// Return duration nanoseconds unchanged.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_nanos(duration_ns: i64) -> i64 {
    duration_ns
}

/// Return duration whole microseconds.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_micros(duration_ns: i64) -> i64 {
    duration_ns / 1_000
}

/// Return duration whole milliseconds.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_millis(duration_ns: i64) -> i64 {
    duration_ns / 1_000_000
}

/// Return duration whole seconds.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_secs(duration_ns: i64) -> i64 {
    duration_ns / 1_000_000_000
}

/// Return duration whole minutes.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_mins(duration_ns: i64) -> i64 {
    duration_ns / 60_000_000_000
}

/// Return duration whole hours.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_hours(duration_ns: i64) -> i64 {
    duration_ns / 3_600_000_000_000
}

/// Return the saturating absolute value of a duration.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_abs(duration_ns: i64) -> i64 {
    duration_ns.saturating_abs()
}

/// Return 1 if a duration is zero, otherwise 0.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_is_zero(duration_ns: i64) -> i32 {
    i32::from(duration_ns == 0)
}

// ---------------------------------------------------------------------------
// I/O Poller (epoll on Linux, kqueue on FreeBSD/macOS, stub elsewhere)
// ---------------------------------------------------------------------------

/// I/O event interest flags.
pub const HEW_IO_READ: c_int = 0x01;
/// I/O event interest flag: write-ready.
pub const HEW_IO_WRITE: c_int = 0x02;
/// I/O event interest flag: error.
pub const HEW_IO_ERROR: c_int = 0x04;
/// I/O event interest flag: hang-up.
pub const HEW_IO_HUP: c_int = 0x08;

#[cfg(any(target_os = "linux", target_os = "freebsd", target_os = "macos"))]
use crate::actor::hew_actor_send;
use crate::actor::HewActor;

// ---- Linux (epoll) --------------------------------------------------------

#[cfg(target_os = "linux")]
mod platform {
    use super::{
        c_int, c_void, hew_actor_send, HewActor, HEW_IO_ERROR, HEW_IO_HUP, HEW_IO_READ,
        HEW_IO_WRITE,
    };
    use std::collections::HashMap;

    /// Per-fd registration data.
    #[derive(Debug)]
    struct FdEntry {
        actor: *mut HewActor,
        msg_type: c_int,
    }

    /// Epoll-backed I/O poller.
    #[derive(Debug)]
    pub struct HewIoPoller {
        epfd: c_int,
        entries: HashMap<c_int, FdEntry>,
    }

    // SAFETY: The poller is only accessed through `extern "C"` functions which
    // take `&mut` semantics via `*mut` — no concurrent access.
    unsafe impl Send for HewIoPoller {}

    impl HewIoPoller {
        #[must_use]
        pub fn new() -> Option<Self> {
            // SAFETY: `epoll_create1(0)` is always valid.
            let epfd = unsafe { libc::epoll_create1(0) };
            if epfd < 0 {
                return None;
            }
            Some(Self {
                epfd,
                entries: HashMap::new(),
            })
        }
    }

    impl Drop for HewIoPoller {
        fn drop(&mut self) {
            if self.epfd >= 0 {
                // SAFETY: closing our own epoll fd.
                unsafe {
                    libc::close(self.epfd);
                }
            }
        }
    }

    fn hew_to_epoll(events: c_int) -> u32 {
        let mut ep: u32 = 0;
        if events & HEW_IO_READ != 0 {
            ep |= libc::EPOLLIN as u32;
        }
        if events & HEW_IO_WRITE != 0 {
            ep |= libc::EPOLLOUT as u32;
        }
        if events & HEW_IO_ERROR != 0 {
            ep |= libc::EPOLLERR as u32;
        }
        if events & HEW_IO_HUP != 0 {
            ep |= libc::EPOLLHUP as u32;
        }
        ep
    }

    fn epoll_to_hew(ep: u32) -> c_int {
        let mut events: c_int = 0;
        if ep & libc::EPOLLIN as u32 != 0 {
            events |= HEW_IO_READ;
        }
        if ep & libc::EPOLLOUT as u32 != 0 {
            events |= HEW_IO_WRITE;
        }
        if ep & libc::EPOLLERR as u32 != 0 {
            events |= HEW_IO_ERROR;
        }
        if ep & libc::EPOLLHUP as u32 != 0 {
            events |= HEW_IO_HUP;
        }
        events
    }

    /// Create a new I/O poller.
    ///
    /// # Safety
    ///
    /// No preconditions.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_new() -> *mut HewIoPoller {
        match HewIoPoller::new() {
            Some(p) => Box::into_raw(Box::new(p)),
            None => std::ptr::null_mut(),
        }
    }

    /// Register a file descriptor with the poller.
    ///
    /// Returns 0 on success, -1 on error.
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`]. `actor`
    /// must remain valid for the lifetime of the registration.
    #[no_mangle]
    #[expect(
        clippy::cast_sign_loss,
        reason = "fd stored as u64 in epoll_event for later recovery"
    )]
    pub unsafe extern "C" fn hew_io_poller_register(
        p: *mut HewIoPoller,
        fd: c_int,
        actor: *mut HewActor,
        msg_type: c_int,
        events: c_int,
    ) -> c_int {
        cabi_guard!(p.is_null(), -1);
        // SAFETY: caller guarantees `p` is valid.
        let poller = unsafe { &mut *p };

        let mut ev = libc::epoll_event {
            events: hew_to_epoll(events),
            u64: fd as u64,
        };

        // SAFETY: epoll_ctl with valid epfd and event pointer.
        let rc = unsafe { libc::epoll_ctl(poller.epfd, libc::EPOLL_CTL_ADD, fd, &raw mut ev) };
        if rc < 0 {
            return -1;
        }
        poller.entries.insert(fd, FdEntry { actor, msg_type });
        0
    }

    /// Unregister a file descriptor from the poller.
    ///
    /// Returns 0 on success, -1 on error.
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`].
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_unregister(p: *mut HewIoPoller, fd: c_int) -> c_int {
        cabi_guard!(p.is_null(), -1);
        // SAFETY: caller guarantees `p` is valid.
        let poller = unsafe { &mut *p };

        // SAFETY: epoll_ctl DEL with valid epfd.
        let rc =
            unsafe { libc::epoll_ctl(poller.epfd, libc::EPOLL_CTL_DEL, fd, std::ptr::null_mut()) };
        if rc < 0 {
            return -1;
        }
        poller.entries.remove(&fd);
        0
    }

    /// Maximum number of epoll events to process per poll call.
    const MAX_EVENTS: c_int = 64;

    /// Poll for I/O events, dispatching to registered actors.
    ///
    /// Returns the number of events dispatched, or -1 on error.
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`].
    /// All registered actor pointers must still be valid.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_poll(p: *mut HewIoPoller, timeout_ms: c_int) -> c_int {
        cabi_guard!(p.is_null(), -1);
        // SAFETY: caller guarantees `p` is valid.
        let poller = unsafe { &mut *p };

        let mut ep_events = [libc::epoll_event { events: 0, u64: 0 }; MAX_EVENTS as usize];

        // SAFETY: epoll_wait with valid fd and buffer.
        let n = unsafe {
            libc::epoll_wait(poller.epfd, ep_events.as_mut_ptr(), MAX_EVENTS, timeout_ms)
        };
        if n < 0 {
            return -1;
        }

        #[expect(clippy::cast_sign_loss, reason = "n >= 0 checked above")]
        let count = n as usize;
        for ev in &ep_events[..count] {
            #[expect(
                clippy::cast_possible_truncation,
                reason = "fd was stored as u64; fits in c_int"
            )]
            let fd = ev.u64 as c_int;
            if let Some(entry) = poller.entries.get(&fd) {
                let mut hew_ev = epoll_to_hew(ev.events);
                // SAFETY: actor pointer is valid per caller contract; sending
                // the event int by reference.
                unsafe {
                    hew_actor_send(
                        entry.actor,
                        entry.msg_type,
                        std::ptr::addr_of_mut!(hew_ev).cast::<c_void>(),
                        std::mem::size_of::<c_int>(),
                    );
                }
            }
        }

        n
    }

    /// Poll for I/O readiness and report the ready fds WITHOUT dispatching.
    ///
    /// Unlike [`hew_io_poller_poll`], which auto-sends a 4-byte event-mask
    /// message to the registered actor from inside the poll loop (no liveness
    /// guard, wrong payload for byte-stream `on_data` delivery), this variant
    /// writes each ready `(fd, hew_event_mask)` pair into the caller-provided
    /// `out_fds` / `out_events` buffers and returns the number of pairs
    /// written. The caller (the active-mode reactor) then performs the
    /// liveness check, the socket read, and the deep-copied mailbox delivery
    /// itself — outside any lock. This is the primitive the
    /// "I/O completion as a mailbox message" reactor needs; the auto-send
    /// variant is unsuitable because it neither reads the data nor checks
    /// actor liveness.
    ///
    /// Returns the number of ready fds (0..=`out_cap`), or -1 on error.
    /// At most `out_cap` (clamped to [`MAX_EVENTS`]) fds are reported per call;
    /// any excess remain ready and surface on the next poll (level/edge per
    /// the registration flags).
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`].
    /// `out_fds` and `out_events` must each point to at least `out_cap`
    /// writable `c_int` slots, or be null when `out_cap` is 0.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_poll_ready(
        p: *mut HewIoPoller,
        timeout_ms: c_int,
        out_fds: *mut c_int,
        out_events: *mut c_int,
        out_cap: c_int,
    ) -> c_int {
        cabi_guard!(p.is_null(), -1);
        if out_cap <= 0 || out_fds.is_null() || out_events.is_null() {
            return 0;
        }
        // SAFETY: caller guarantees `p` is valid.
        let poller = unsafe { &mut *p };

        let mut ep_events = [libc::epoll_event { events: 0, u64: 0 }; MAX_EVENTS as usize];

        // SAFETY: epoll_wait with valid fd and buffer.
        let n = unsafe {
            libc::epoll_wait(poller.epfd, ep_events.as_mut_ptr(), MAX_EVENTS, timeout_ms)
        };
        if n < 0 {
            return -1;
        }

        #[expect(clippy::cast_sign_loss, reason = "n >= 0 checked above")]
        let count = (n as usize).min(out_cap as usize);
        for (i, ev) in ep_events[..count].iter().enumerate() {
            #[expect(
                clippy::cast_possible_truncation,
                reason = "fd was stored as u64; fits in c_int"
            )]
            let fd = ev.u64 as c_int;
            let hew_ev = epoll_to_hew(ev.events);
            // SAFETY: i < count <= out_cap; out_fds/out_events have out_cap slots.
            unsafe {
                *out_fds.add(i) = fd;
                *out_events.add(i) = hew_ev;
            }
        }

        #[expect(clippy::cast_possible_truncation, reason = "count <= out_cap (c_int)")]
        #[expect(clippy::cast_possible_wrap, reason = "count <= out_cap (c_int)")]
        {
            count as c_int
        }
    }

    /// Stop and destroy the poller.
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`], and must
    /// not be used after this call.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_stop(p: *mut HewIoPoller) {
        if !p.is_null() {
            // SAFETY: caller guarantees `p` is valid and surrenders ownership.
            let _ = unsafe { Box::from_raw(p) };
        }
    }
}

// ---- FreeBSD / macOS (kqueue) ----------------------------------------------

#[cfg(any(target_os = "freebsd", target_os = "macos"))]
mod platform {
    use super::{
        c_int, c_void, hew_actor_send, HewActor, HEW_IO_ERROR, HEW_IO_HUP, HEW_IO_READ,
        HEW_IO_WRITE,
    };
    use std::collections::HashMap;

    /// Per-fd registration data.
    #[derive(Debug)]
    struct FdEntry {
        actor: *mut HewActor,
        msg_type: c_int,
    }

    /// Kqueue-backed I/O poller.
    #[derive(Debug)]
    pub struct HewIoPoller {
        kq: c_int,
        entries: HashMap<c_int, FdEntry>,
    }

    // SAFETY: The poller is only accessed through `extern "C"` functions which
    // take `&mut` semantics via `*mut` — no concurrent access.
    unsafe impl Send for HewIoPoller {}

    impl HewIoPoller {
        #[must_use]
        pub fn new() -> Option<Self> {
            // SAFETY: `kqueue()` is always valid.
            let kq = unsafe { libc::kqueue() };
            if kq < 0 {
                return None;
            }
            Some(Self {
                kq,
                entries: HashMap::new(),
            })
        }
    }

    impl Drop for HewIoPoller {
        fn drop(&mut self) {
            if self.kq >= 0 {
                // SAFETY: closing our own kqueue fd.
                unsafe {
                    libc::close(self.kq);
                }
            }
        }
    }

    /// Maximum number of kqueue events to process per poll call.
    const MAX_EVENTS: usize = 64;

    /// Create a new I/O poller.
    ///
    /// # Safety
    ///
    /// No preconditions.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_new() -> *mut HewIoPoller {
        match HewIoPoller::new() {
            Some(p) => Box::into_raw(Box::new(p)),
            None => std::ptr::null_mut(),
        }
    }

    /// Register a file descriptor with the poller.
    ///
    /// Returns 0 on success, -1 on error.
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`]. `actor`
    /// must remain valid for the lifetime of the registration.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_register(
        p: *mut HewIoPoller,
        fd: c_int,
        actor: *mut HewActor,
        msg_type: c_int,
        events: c_int,
    ) -> c_int {
        cabi_guard!(p.is_null(), -1);
        // SAFETY: caller guarantees `p` is valid.
        let poller = unsafe { &mut *p };

        // Build changelist for the requested filters.
        let mut changelist: Vec<libc::kevent> = Vec::new();

        if events & HEW_IO_READ != 0 {
            #[expect(
                clippy::cast_sign_loss,
                reason = "fd is a valid file descriptor from the OS, always non-negative"
            )]
            changelist.push(libc::kevent {
                ident: fd as usize,
                filter: libc::EVFILT_READ,
                flags: libc::EV_ADD | libc::EV_CLEAR,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
                // FreeBSD 15+ kevent has an extra `ext: [u64; 4]` field.
                #[cfg(target_os = "freebsd")]
                ext: [0; 4],
            });
        }
        if events & HEW_IO_WRITE != 0 {
            #[expect(
                clippy::cast_sign_loss,
                reason = "fd is a valid file descriptor from the OS, always non-negative"
            )]
            changelist.push(libc::kevent {
                ident: fd as usize,
                filter: libc::EVFILT_WRITE,
                flags: libc::EV_ADD | libc::EV_CLEAR,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
                // FreeBSD 15+ kevent has an extra `ext: [u64; 4]` field.
                #[cfg(target_os = "freebsd")]
                ext: [0; 4],
            });
        }

        if changelist.is_empty() {
            return -1;
        }

        #[expect(
            clippy::cast_possible_truncation,
            reason = "changelist has at most 2 entries, fits in c_int"
        )]
        #[expect(
            clippy::cast_possible_wrap,
            reason = "changelist has at most 2 entries, fits in c_int"
        )]
        // SAFETY: kevent with valid kq, changelist, and no eventlist.
        let rc = unsafe {
            libc::kevent(
                poller.kq,
                changelist.as_ptr(),
                changelist.len() as c_int,
                std::ptr::null_mut(),
                0,
                std::ptr::null(),
            )
        };
        if rc < 0 {
            return -1;
        }

        poller.entries.insert(fd, FdEntry { actor, msg_type });
        0
    }

    /// Unregister a file descriptor from the poller.
    ///
    /// Returns 0 on success, -1 on error.
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`].
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_unregister(p: *mut HewIoPoller, fd: c_int) -> c_int {
        cabi_guard!(p.is_null(), -1);
        // SAFETY: caller guarantees `p` is valid.
        let poller = unsafe { &mut *p };

        // Delete both read and write filters — ignore errors for filters that
        // were not registered (kevent returns -1 with ENOENT, which is benign).
        #[expect(
            clippy::cast_sign_loss,
            reason = "fd is a valid file descriptor from the OS, always non-negative"
        )]
        let changelist = [
            libc::kevent {
                ident: fd as usize,
                filter: libc::EVFILT_READ,
                flags: libc::EV_DELETE,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
                #[cfg(target_os = "freebsd")]
                ext: [0; 4],
            },
            libc::kevent {
                ident: fd as usize,
                filter: libc::EVFILT_WRITE,
                flags: libc::EV_DELETE,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
                #[cfg(target_os = "freebsd")]
                ext: [0; 4],
            },
        ];

        #[expect(
            clippy::cast_possible_truncation,
            reason = "changelist is a fixed-size array of 2 elements, fits in c_int"
        )]
        #[expect(
            clippy::cast_possible_wrap,
            reason = "changelist is a fixed-size array of 2 elements, fits in c_int"
        )]
        // SAFETY: kevent with valid kq and changelist.
        unsafe {
            libc::kevent(
                poller.kq,
                changelist.as_ptr(),
                changelist.len() as c_int,
                std::ptr::null_mut(),
                0,
                std::ptr::null(),
            );
        }

        poller.entries.remove(&fd);
        0
    }

    /// Poll for I/O events, dispatching to registered actors.
    ///
    /// Returns the number of events dispatched, or -1 on error.
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`].
    /// All registered actor pointers must still be valid.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_poll(p: *mut HewIoPoller, timeout_ms: c_int) -> c_int {
        cabi_guard!(p.is_null(), -1);
        // SAFETY: caller guarantees `p` is valid.
        let poller = unsafe { &mut *p };

        // SAFETY: `libc::kevent` is a C struct of integer and pointer fields;
        // an all-zeroes bit pattern is a valid representation.
        let mut kq_events: [libc::kevent; MAX_EVENTS] =
            // SAFETY: libc::kevent is a POD C struct; all-zero is valid.
            unsafe { std::mem::zeroed() };

        // Build timeout: negative means block indefinitely (pass null).
        let ts;
        let timeout_ptr = if timeout_ms < 0 {
            std::ptr::null()
        } else {
            ts = libc::timespec {
                tv_sec: libc::time_t::from(timeout_ms / 1000),
                tv_nsec: libc::c_long::from(timeout_ms % 1000) * 1_000_000,
            };
            &raw const ts
        };

        #[expect(
            clippy::cast_possible_truncation,
            reason = "MAX_EVENTS is 64, fits in c_int"
        )]
        #[expect(clippy::cast_possible_wrap, reason = "MAX_EVENTS is 64, fits in c_int")]
        let max_events_cint = MAX_EVENTS as c_int;

        // SAFETY: kevent with valid kq, no changelist, eventlist buffer.
        let n = unsafe {
            libc::kevent(
                poller.kq,
                std::ptr::null(),
                0,
                kq_events.as_mut_ptr(),
                max_events_cint,
                timeout_ptr,
            )
        };
        if n < 0 {
            return -1;
        }

        #[expect(clippy::cast_sign_loss, reason = "n >= 0 checked above")]
        let count = n as usize;
        for ev in &kq_events[..count] {
            #[expect(
                clippy::cast_possible_truncation,
                reason = "fd was stored as usize via ident; fits in c_int"
            )]
            #[expect(
                clippy::cast_possible_wrap,
                reason = "fd was stored as usize via ident; original value was a non-negative c_int"
            )]
            let fd = ev.ident as c_int;
            if let Some(entry) = poller.entries.get(&fd) {
                let mut hew_ev: c_int = 0;

                // Map kqueue filter to Hew event flags.
                if ev.filter == libc::EVFILT_READ {
                    hew_ev |= HEW_IO_READ;
                }
                if ev.filter == libc::EVFILT_WRITE {
                    hew_ev |= HEW_IO_WRITE;
                }
                // Check for EOF and error conditions.
                if ev.flags & libc::EV_EOF != 0 {
                    hew_ev |= HEW_IO_HUP;
                }
                if ev.flags & libc::EV_ERROR != 0 {
                    hew_ev |= HEW_IO_ERROR;
                }

                // SAFETY: actor pointer is valid per caller contract; sending
                // the event int by reference.
                unsafe {
                    hew_actor_send(
                        entry.actor,
                        entry.msg_type,
                        std::ptr::addr_of_mut!(hew_ev).cast::<c_void>(),
                        std::mem::size_of::<c_int>(),
                    );
                }
            }
        }

        n
    }

    /// Poll for I/O readiness and report the ready fds WITHOUT dispatching.
    ///
    /// kqueue counterpart of the epoll `hew_io_poller_poll_ready`. Writes each
    /// ready `(fd, hew_event_mask)` pair into the caller buffers and returns
    /// the count, leaving the liveness check, socket read, and deep-copied
    /// mailbox delivery to the active-mode reactor (outside any lock). See the
    /// epoll variant's rationale for why the auto-send `hew_io_poller_poll`
    /// is unsuitable for byte-stream `on_data` delivery.
    ///
    /// Returns the number of ready fds (0..=`out_cap`), or -1 on error.
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`].
    /// `out_fds` and `out_events` must each point to at least `out_cap`
    /// writable `c_int` slots, or be null when `out_cap` is 0.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_poll_ready(
        p: *mut HewIoPoller,
        timeout_ms: c_int,
        out_fds: *mut c_int,
        out_events: *mut c_int,
        out_cap: c_int,
    ) -> c_int {
        cabi_guard!(p.is_null(), -1);
        if out_cap <= 0 || out_fds.is_null() || out_events.is_null() {
            return 0;
        }
        // SAFETY: caller guarantees `p` is valid.
        let poller = unsafe { &mut *p };

        // SAFETY: `libc::kevent` is a POD C struct; all-zero is a valid bit pattern.
        let mut kq_events: [libc::kevent; MAX_EVENTS] = unsafe { std::mem::zeroed() };

        let ts;
        let timeout_ptr = if timeout_ms < 0 {
            std::ptr::null()
        } else {
            ts = libc::timespec {
                tv_sec: libc::time_t::from(timeout_ms / 1000),
                tv_nsec: libc::c_long::from(timeout_ms % 1000) * 1_000_000,
            };
            &raw const ts
        };

        #[expect(
            clippy::cast_possible_truncation,
            reason = "MAX_EVENTS is 64, fits in c_int"
        )]
        #[expect(clippy::cast_possible_wrap, reason = "MAX_EVENTS is 64, fits in c_int")]
        let max_events_cint = MAX_EVENTS as c_int;

        // SAFETY: kevent with valid kq, no changelist, eventlist buffer.
        let n = unsafe {
            libc::kevent(
                poller.kq,
                std::ptr::null(),
                0,
                kq_events.as_mut_ptr(),
                max_events_cint,
                timeout_ptr,
            )
        };
        if n < 0 {
            return -1;
        }

        #[expect(clippy::cast_sign_loss, reason = "n >= 0 checked above")]
        let count = (n as usize).min(out_cap as usize);
        for (i, ev) in kq_events[..count].iter().enumerate() {
            #[expect(
                clippy::cast_possible_truncation,
                reason = "fd was stored as usize via ident; fits in c_int"
            )]
            #[expect(
                clippy::cast_possible_wrap,
                reason = "fd was stored as usize via ident; original value was a non-negative c_int"
            )]
            let fd = ev.ident as c_int;
            let mut hew_ev: c_int = 0;
            if ev.filter == libc::EVFILT_READ {
                hew_ev |= HEW_IO_READ;
            }
            if ev.filter == libc::EVFILT_WRITE {
                hew_ev |= HEW_IO_WRITE;
            }
            if ev.flags & libc::EV_EOF != 0 {
                hew_ev |= HEW_IO_HUP;
            }
            if ev.flags & libc::EV_ERROR != 0 {
                hew_ev |= HEW_IO_ERROR;
            }
            // SAFETY: i < count <= out_cap; out_fds/out_events have out_cap slots.
            unsafe {
                *out_fds.add(i) = fd;
                *out_events.add(i) = hew_ev;
            }
        }

        #[expect(clippy::cast_possible_truncation, reason = "count <= out_cap (c_int)")]
        #[expect(clippy::cast_possible_wrap, reason = "count <= out_cap (c_int)")]
        {
            count as c_int
        }
    }

    /// Stop and destroy the poller.
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`], and must
    /// not be used after this call.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_stop(p: *mut HewIoPoller) {
        if !p.is_null() {
            // SAFETY: caller guarantees `p` is valid and surrenders ownership.
            let _ = unsafe { Box::from_raw(p) };
        }
    }
}

// ---- Windows (IOCP + AFD_POLL readiness bridge) ----------------------------
//
// IOCP is completion-based; the Hew reactor contract is readiness-based. We
// synthesize readiness with the AFD_POLL technique (the mio/wepoll production
// approach): a single `\Device\Afd` helper handle is associated with an I/O
// completion port, and each registered socket arms a one-shot
// `NtDeviceIoControlFile(IOCTL_AFD_POLL)` whose completion reports which socket
// events fired. `poll_ready` dequeues those completions, translates the AFD
// event mask to the `HEW_IO_*` flags the reactor expects, writes `(token, mask)`
// pairs, and re-arms the one-shot poll. The reactor — not the poller — then does
// the recv/accept and the mailbox delivery, exactly as on epoll/kqueue.
//
// D-2a token model: a Windows `SOCKET` is pointer-width and does not fit the
// poller's `c_int fd` ABI, so the reactor registers the user-facing connection
// (or listener) HANDLE as the `c_int` token; the poller resolves token→`SOCKET`
// via `crate::transport::tcp_handle_raw_socket`. The engine never sees a `SOCKET`.
#[cfg(windows)]
#[allow(
    non_snake_case,
    non_camel_case_types,
    clippy::upper_case_acronyms,
    clippy::unreadable_literal,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    reason = "Win32/NT FFI module: struct/field names mirror the platform headers \
              verbatim, status/handle values are fixed-width casts at the ABI boundary, \
              and the constants are copied from the documented Win32 headers"
)]
mod platform {
    use super::{c_int, HEW_IO_ERROR, HEW_IO_HUP, HEW_IO_READ, HEW_IO_WRITE};
    use std::collections::HashMap;
    use std::ffi::c_void;
    use std::ptr;

    type HANDLE = *mut c_void;
    type SOCKET = usize;
    type NTSTATUS = i32;

    // NT status codes (low 32 bits of the IO_STATUS_BLOCK union).
    const STATUS_SUCCESS: NTSTATUS = 0x0000_0000;
    const STATUS_PENDING: NTSTATUS = 0x0000_0103;
    const STATUS_CANCELLED: u32 = 0xC000_0120;

    // Win32 / NT constants.
    const SYNCHRONIZE: u32 = 0x0010_0000;
    const FILE_OPEN: u32 = 0x0000_0001;
    const FILE_SHARE_READ: u32 = 0x0000_0001;
    const FILE_SHARE_WRITE: u32 = 0x0000_0002;
    const WAIT_TIMEOUT: u32 = 258;
    const INFINITE: u32 = 0xFFFF_FFFF;
    const IOCTL_AFD_POLL: u32 = 0x0001_2024;
    const SIO_BASE_HANDLE: u32 = 0x4800_0022;

    // AFD poll event flags (the undocumented-but-stable wepoll/mio set).
    const AFD_POLL_RECEIVE: u32 = 0x0001;
    const AFD_POLL_RECEIVE_EXPEDITED: u32 = 0x0002;
    const AFD_POLL_SEND: u32 = 0x0004;
    const AFD_POLL_DISCONNECT: u32 = 0x0008;
    const AFD_POLL_ABORT: u32 = 0x0010;
    const AFD_POLL_LOCAL_CLOSE: u32 = 0x0020;
    const AFD_POLL_ACCEPT: u32 = 0x0080;
    const AFD_POLL_CONNECT_FAIL: u32 = 0x0100;

    /// Completion key used when associating the AFD helper handle with the IOCP.
    /// Unused for routing (we key on the completion's overlapped/ApcContext
    /// pointer) but a stable constant aids debugging.
    const AFD_COMPLETION_KEY: usize = 0xAFD0;

    /// Maximum completions dequeued per `GetQueuedCompletionStatusEx` call.
    const MAX_COMPLETIONS: usize = 64;

    #[repr(C)]
    struct UNICODE_STRING {
        Length: u16,
        MaximumLength: u16,
        Buffer: *mut u16,
    }

    #[repr(C)]
    struct OBJECT_ATTRIBUTES {
        Length: u32,
        RootDirectory: HANDLE,
        ObjectName: *mut UNICODE_STRING,
        Attributes: u32,
        SecurityDescriptor: *mut c_void,
        SecurityQualityOfService: *mut c_void,
    }

    /// `IO_STATUS_BLOCK`: the first member is a pointer-sized union of an
    /// `NTSTATUS` and a `PVOID`. We store it as `isize` and read the status from
    /// its low 32 bits (little-endian), matching the kernel's layout.
    #[repr(C)]
    #[derive(Clone, Copy)]
    struct IO_STATUS_BLOCK {
        status: isize,
        information: usize,
    }

    impl IO_STATUS_BLOCK {
        const fn zeroed() -> Self {
            Self {
                status: 0,
                information: 0,
            }
        }
        fn ntstatus(self) -> u32 {
            // Read the NTSTATUS from the low 32 bits of the pointer-sized union.
            self.status as u32
        }
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    struct AFD_POLL_HANDLE_INFO {
        Handle: HANDLE,
        Events: u32,
        Status: NTSTATUS,
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    struct AFD_POLL_INFO {
        Timeout: i64,
        NumberOfHandles: u32,
        Exclusive: u32,
        Handles: [AFD_POLL_HANDLE_INFO; 1],
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    struct OVERLAPPED_ENTRY {
        lpCompletionKey: usize,
        lpOverlapped: *mut c_void,
        Internal: usize,
        dwNumberOfBytesTransferred: u32,
    }

    #[link(name = "ntdll")]
    extern "system" {
        fn NtCreateFile(
            FileHandle: *mut HANDLE,
            DesiredAccess: u32,
            ObjectAttributes: *mut OBJECT_ATTRIBUTES,
            IoStatusBlock: *mut IO_STATUS_BLOCK,
            AllocationSize: *mut i64,
            FileAttributes: u32,
            ShareAccess: u32,
            CreateDisposition: u32,
            CreateOptions: u32,
            EaBuffer: *mut c_void,
            EaLength: u32,
        ) -> NTSTATUS;

        fn NtDeviceIoControlFile(
            FileHandle: HANDLE,
            Event: HANDLE,
            ApcRoutine: *mut c_void,
            ApcContext: *mut c_void,
            IoStatusBlock: *mut IO_STATUS_BLOCK,
            IoControlCode: u32,
            InputBuffer: *mut c_void,
            InputBufferLength: u32,
            OutputBuffer: *mut c_void,
            OutputBufferLength: u32,
        ) -> NTSTATUS;

        fn NtCancelIoFileEx(
            FileHandle: HANDLE,
            IoRequestToCancel: *mut IO_STATUS_BLOCK,
            IoStatusBlock: *mut IO_STATUS_BLOCK,
        ) -> NTSTATUS;
    }

    #[link(name = "kernel32")]
    extern "system" {
        fn CreateIoCompletionPort(
            FileHandle: HANDLE,
            ExistingCompletionPort: HANDLE,
            CompletionKey: usize,
            NumberOfConcurrentThreads: u32,
        ) -> HANDLE;
        fn GetQueuedCompletionStatusEx(
            CompletionPort: HANDLE,
            lpCompletionPortEntries: *mut OVERLAPPED_ENTRY,
            ulCount: u32,
            ulNumEntriesRemoved: *mut u32,
            dwMilliseconds: u32,
            fAlertable: i32,
        ) -> i32;
        fn CloseHandle(hObject: HANDLE) -> i32;
        fn GetLastError() -> u32;
    }

    #[link(name = "ws2_32")]
    extern "system" {
        fn WSAIoctl(
            s: SOCKET,
            dwIoControlCode: u32,
            lpvInBuffer: *mut c_void,
            cbInBuffer: u32,
            lpvOutBuffer: *mut c_void,
            cbOutBuffer: u32,
            lpcbBytesReturned: *mut u32,
            lpOverlapped: *mut c_void,
            lpCompletionRoutine: *mut c_void,
        ) -> i32;
    }

    fn invalid_handle() -> HANDLE {
        (-1isize) as HANDLE
    }

    /// Resolve a socket's AFD base handle (peeling any layered service
    /// providers). Falls back to the socket itself when `SIO_BASE_HANDLE` is
    /// unsupported (the common case for vanilla loopback TCP).
    fn base_socket(socket: SOCKET) -> SOCKET {
        let mut base: SOCKET = 0;
        let mut bytes: u32 = 0;
        // SAFETY: `socket` is a live OS SOCKET; the output buffer is a local
        // `SOCKET`-sized slot. A failed ioctl leaves `base` unchanged.
        let rc = unsafe {
            WSAIoctl(
                socket,
                SIO_BASE_HANDLE,
                ptr::null_mut(),
                0,
                ptr::addr_of_mut!(base).cast::<c_void>(),
                std::mem::size_of::<SOCKET>() as u32,
                ptr::addr_of_mut!(bytes),
                ptr::null_mut(),
                ptr::null_mut(),
            )
        };
        if rc == 0 && base != 0 {
            base
        } else {
            socket
        }
    }

    /// Translate the requested `HEW_IO_*` interest into the AFD poll event mask
    /// to arm. Read interest always includes the close/abort family so EOF and
    /// resets surface as `HUP`/`ERROR` readiness; `ACCEPT` lets a listener token
    /// report readability for `accept()`.
    fn afd_interest(events: c_int) -> u32 {
        let mut mask =
            AFD_POLL_ABORT | AFD_POLL_CONNECT_FAIL | AFD_POLL_DISCONNECT | AFD_POLL_LOCAL_CLOSE;
        if events & HEW_IO_READ != 0 {
            mask |= AFD_POLL_RECEIVE | AFD_POLL_RECEIVE_EXPEDITED | AFD_POLL_ACCEPT;
        }
        if events & HEW_IO_WRITE != 0 {
            mask |= AFD_POLL_SEND;
        }
        mask
    }

    /// Translate fired AFD poll events into the reactor's `HEW_IO_*` mask.
    fn afd_to_hew(afd: u32) -> c_int {
        let mut hew = 0;
        if afd & (AFD_POLL_RECEIVE | AFD_POLL_RECEIVE_EXPEDITED | AFD_POLL_ACCEPT) != 0 {
            hew |= HEW_IO_READ;
        }
        if afd & AFD_POLL_SEND != 0 {
            hew |= HEW_IO_WRITE;
        }
        if afd & (AFD_POLL_DISCONNECT | AFD_POLL_LOCAL_CLOSE) != 0 {
            hew |= HEW_IO_HUP;
        }
        if afd & (AFD_POLL_ABORT | AFD_POLL_CONNECT_FAIL) != 0 {
            hew |= HEW_IO_ERROR;
        }
        hew
    }

    /// Per-registered-socket AFD poll state. Boxed so its heap address is stable;
    /// that address is passed as the `ApcContext` of the AFD poll IOCTL and comes
    /// back as the completion's `lpOverlapped`, keying the completion to this
    /// state. The `poll_info`/`iosb` buffers are owned here and MUST outlive any
    /// in-flight poll (the kernel writes into them on completion), which is why a
    /// cancelled state is held in `zombies` until its cancellation completion is
    /// drained.
    struct AfdState {
        token: c_int,
        base_socket: SOCKET,
        interest: u32,
        /// Whether this state currently has an ARMED (genuinely in-flight) AFD
        /// poll IOCTL. `true` from a successful `arm_poll` until its completion is
        /// dequeued in `poll_ready`; set back to `false` on a delivered TERMINAL
        /// (HUP/ERROR) completion that is not re-armed. Distinguishes a state with
        /// a pending kernel completion (must be cancelled+drained / zombied) from
        /// a terminal one with nothing in flight (can be dropped directly).
        armed: bool,
        poll_info: AFD_POLL_INFO,
        iosb: IO_STATUS_BLOCK,
    }

    /// Arm a one-shot AFD poll for `state` on the `afd` helper handle. Returns the
    /// raw `NTSTATUS`; `STATUS_PENDING` (armed) and `STATUS_SUCCESS` (already
    /// ready, completion queued) are both success.
    fn arm_poll(afd: HANDLE, state: &mut AfdState) -> NTSTATUS {
        // ApcContext = this state's stable heap address; comes back as the
        // completion's lpOverlapped.
        let apc_ctx = (state as *mut AfdState).cast::<c_void>();
        state.poll_info.Timeout = i64::MAX;
        state.poll_info.NumberOfHandles = 1;
        state.poll_info.Exclusive = 0;
        state.poll_info.Handles[0].Handle = state.base_socket as HANDLE;
        state.poll_info.Handles[0].Status = 0;
        state.poll_info.Handles[0].Events = state.interest;
        state.iosb.status = STATUS_PENDING as isize;
        let info_ptr = ptr::addr_of_mut!(state.poll_info).cast::<c_void>();
        let iosb_ptr = ptr::addr_of_mut!(state.iosb);
        let size = std::mem::size_of::<AFD_POLL_INFO>() as u32;
        // SAFETY: `afd` is the live helper handle bound to the IOCP; the buffers
        // are owned by `state` and outlive the in-flight poll (held in `entries`
        // or `zombies`); ApcRoutine is null so the completion posts to the IOCP
        // with `apc_ctx` as the overlapped pointer.
        unsafe {
            NtDeviceIoControlFile(
                afd,
                ptr::null_mut(),
                ptr::null_mut(),
                apc_ctx,
                iosb_ptr,
                IOCTL_AFD_POLL,
                info_ptr,
                size,
                info_ptr,
                size,
            )
        }
    }

    fn arm_ok(status: NTSTATUS) -> bool {
        status == STATUS_PENDING || status == STATUS_SUCCESS
    }

    /// IOCP + `AFD_POLL` readiness poller. Single-threaded: every method runs on the
    /// reactor thread (the sole poller owner), so no interior locking is needed.
    #[derive(Debug)]
    pub struct HewIoPoller {
        iocp: HANDLE,
        afd: HANDLE,
        /// Active registrations, keyed by reactor token.
        entries: HashMap<c_int, Box<AfdState>>,
        /// `AfdState` heap address → token, for routing a completion (keyed by its
        /// `lpOverlapped`) back to the owning entry.
        index: HashMap<usize, c_int>,
        /// Cancelled registrations awaiting their final (cancellation) completion;
        /// kept alive so the kernel's last write into their `iosb` is not a UAF.
        zombies: HashMap<usize, Box<AfdState>>,
    }

    impl std::fmt::Debug for AfdState {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("AfdState")
                .field("token", &self.token)
                .finish_non_exhaustive()
        }
    }

    impl HewIoPoller {
        fn new() -> Option<Self> {
            // SAFETY: no preconditions; creating a fresh completion port.
            let iocp = unsafe { CreateIoCompletionPort(invalid_handle(), ptr::null_mut(), 0, 0) };
            if iocp.is_null() {
                return None;
            }
            let Some(afd) = open_afd_helper() else {
                // SAFETY: iocp is our freshly-created port; close it on failure.
                unsafe { CloseHandle(iocp) };
                return None;
            };
            // Associate the AFD helper handle with the completion port.
            // SAFETY: both handles are live and owned here.
            let assoc = unsafe { CreateIoCompletionPort(afd, iocp, AFD_COMPLETION_KEY, 0) };
            if assoc != iocp {
                // SAFETY: both handles are live and owned here.
                unsafe {
                    CloseHandle(afd);
                    CloseHandle(iocp);
                }
                return None;
            }
            Some(Self {
                iocp,
                afd,
                entries: HashMap::new(),
                index: HashMap::new(),
                zombies: HashMap::new(),
            })
        }

        fn register(&mut self, token: c_int, events: c_int) -> c_int {
            if self.entries.contains_key(&token) {
                // Already armed for this token; idempotent success.
                return 0;
            }
            let Some(raw) = crate::transport::tcp_handle_raw_socket(token) else {
                return -1; // unknown handle (closed/never existed)
            };
            let socket = raw as SOCKET;
            // SAFETY: `socket` is a live OS SOCKET from the transport table.
            let base = base_socket(socket);
            let mut state = Box::new(AfdState {
                token,
                base_socket: base,
                interest: afd_interest(events),
                armed: false,
                // SAFETY: AFD_POLL_INFO / IO_STATUS_BLOCK are POD; all-zero is valid.
                poll_info: unsafe { std::mem::zeroed() },
                iosb: IO_STATUS_BLOCK::zeroed(),
            });
            let addr = ptr::addr_of!(*state) as usize;
            let status = arm_poll(self.afd, &mut state);
            if !arm_ok(status) {
                // Arm failed: no I/O was queued, so dropping the box is safe.
                return -1;
            }
            state.armed = true;
            self.index.insert(addr, token);
            self.entries.insert(token, state);
            0
        }

        fn unregister(&mut self, token: c_int) -> c_int {
            let Some(mut state) = self.entries.remove(&token) else {
                // Benign double-unregister (mirrors the unix backends).
                return 0;
            };
            let addr = ptr::addr_of!(*state) as usize;
            self.index.remove(&addr);
            if !state.armed {
                // A terminal (HUP/ERROR) completion was already delivered and the
                // poll was NOT re-armed, so nothing is in flight: no future
                // completion will ever arrive. Drop the state directly — issuing
                // NtCancelIoFileEx would be a no-op and parking it in `zombies`
                // would leak it (the zombie-cleanup path in `poll_ready` only runs
                // when a completion is dequeued, which never happens here) and
                // would inflate the shutdown in-flight count.
                drop(state);
                return 0;
            }
            let mut cancel_iosb = IO_STATUS_BLOCK::zeroed();
            // SAFETY: `afd` is live; `state.iosb` identifies the in-flight poll to
            // cancel. The state is moved into `zombies` so its buffers stay valid
            // until the cancellation completion is drained in `poll_ready`/shutdown.
            unsafe {
                NtCancelIoFileEx(
                    self.afd,
                    ptr::addr_of_mut!(state.iosb),
                    ptr::addr_of_mut!(cancel_iosb),
                );
            }
            self.zombies.insert(addr, state);
            0
        }

        fn poll_ready(
            &mut self,
            timeout_ms: c_int,
            out_fds: *mut c_int,
            out_events: *mut c_int,
            out_cap: c_int,
        ) -> c_int {
            // SAFETY: OVERLAPPED_ENTRY is POD; all-zero is a valid bit pattern.
            let mut completions: [OVERLAPPED_ENTRY; MAX_COMPLETIONS] =
                unsafe { std::mem::zeroed() };
            let mut removed: u32 = 0;
            let timeout = if timeout_ms < 0 {
                INFINITE
            } else {
                timeout_ms as u32
            };
            // SAFETY: `iocp` is live; the entries buffer holds MAX_COMPLETIONS slots.
            let ok = unsafe {
                GetQueuedCompletionStatusEx(
                    self.iocp,
                    completions.as_mut_ptr(),
                    MAX_COMPLETIONS as u32,
                    ptr::addr_of_mut!(removed),
                    timeout,
                    0,
                )
            };
            if ok == 0 {
                // SAFETY: no preconditions.
                let err = unsafe { GetLastError() };
                if err == WAIT_TIMEOUT {
                    return 0;
                }
                return -1;
            }

            let cap = out_cap as usize;
            let mut count: usize = 0;
            for entry in completions.iter().take(removed as usize) {
                let addr = entry.lpOverlapped as usize;
                if let Some(&token) = self.index.get(&addr) {
                    let (mask, is_close) = {
                        let state = self
                            .entries
                            .get(&token)
                            .expect("index points to a live entry");
                        let cancelled = state.iosb.ntstatus() == STATUS_CANCELLED;
                        let mask = if cancelled || state.poll_info.NumberOfHandles == 0 {
                            0
                        } else {
                            afd_to_hew(state.poll_info.Handles[0].Events)
                        };
                        (mask, mask & (HEW_IO_HUP | HEW_IO_ERROR) != 0)
                    };
                    if mask != 0 && count < cap {
                        // SAFETY: count < cap <= out_cap; the buffers have out_cap slots.
                        unsafe {
                            *out_fds.add(count) = token;
                            *out_events.add(count) = mask;
                        }
                        count += 1;
                    }
                    if is_close {
                        // Close/error reported: do NOT re-arm (avoid busy-cycling on
                        // a level-triggered close). The reactor will unregister.
                        // Mark the state disarmed: its one-shot completion has been
                        // dequeued and no new poll is in flight, so a later
                        // `unregister` must drop it directly (no zombie) and
                        // shutdown must not count it as in-flight.
                        if let Some(state) = self.entries.get_mut(&token) {
                            state.armed = false;
                        }
                        continue;
                    }
                    // Re-arm the one-shot poll for the next readiness.
                    let state = self
                        .entries
                        .get_mut(&token)
                        .expect("index points to a live entry");
                    if !arm_ok(arm_poll(self.afd, state)) {
                        // Re-arm failed (socket gone): surface a HUP so the reactor
                        // closes + unregisters, and drop the entry (no I/O queued).
                        if count < cap {
                            // SAFETY: count < cap <= out_cap.
                            unsafe {
                                *out_fds.add(count) = token;
                                *out_events.add(count) = HEW_IO_HUP;
                            }
                            count += 1;
                        }
                        self.index.remove(&addr);
                        self.entries.remove(&token);
                    }
                } else {
                    // Cancelled entry's final completion: drop the zombie now that
                    // the kernel is done writing into its buffers.
                    self.zombies.remove(&addr);
                }
            }
            count as c_int
        }

        /// Cancel every genuinely in-flight AFD poll (armed `entries` + already-
        /// cancelled `zombies`) and block until EVERY one of their guaranteed
        /// completions has been dequeued from the IOCP. Returns
        /// `(pending, drained)`; on return all `entries` are disarmed and
        /// `zombies` is empty, so a second call is a no-op (no double-cancel hang).
        ///
        /// Termination proof (no bounded give-up needed): a one-shot
        /// `IOCTL_AFD_POLL` is either still pending — in which case
        /// `NtCancelIoFileEx` forces the AFD driver to complete the IRP with
        /// `STATUS_CANCELLED` — or it has already completed and its packet is
        /// queued on the port. Either way each in-flight request posts EXACTLY one
        /// completion to `iocp`, so `drained` reaches `pending` after a finite
        /// number of `GetQueuedCompletionStatusEx` dequeues. A poll timeout is NOT
        /// treated as progress; the loop simply keeps waiting until the count is
        /// reached, so no `AfdState`/`iosb`/`poll_info` buffer is ever freed while
        /// the kernel still owns a pending write into it.
        fn cancel_and_drain_inflight(&mut self) -> (usize, usize) {
            let mut pending = self.zombies.len();
            for state in self.entries.values_mut() {
                if !state.armed {
                    // Terminal completion already dequeued, not re-armed: nothing
                    // is in flight, so it must NOT be cancelled or counted.
                    continue;
                }
                let mut cancel_iosb = IO_STATUS_BLOCK::zeroed();
                // SAFETY: `afd` is live; cancels the in-flight poll for this state.
                unsafe {
                    NtCancelIoFileEx(
                        self.afd,
                        ptr::addr_of_mut!(state.iosb),
                        ptr::addr_of_mut!(cancel_iosb),
                    );
                }
                // Its completion is now accounted for in `pending`; the buffers
                // remain alive (still owned by `entries`) until drained below.
                state.armed = false;
                pending += 1;
            }
            let mut drained = 0usize;
            while drained < pending {
                // SAFETY: OVERLAPPED_ENTRY is POD; all-zero is valid.
                let mut completions: [OVERLAPPED_ENTRY; MAX_COMPLETIONS] =
                    unsafe { std::mem::zeroed() };
                let mut removed: u32 = 0;
                // SAFETY: `iocp` is live; buffer holds MAX_COMPLETIONS slots.
                let ok = unsafe {
                    GetQueuedCompletionStatusEx(
                        self.iocp,
                        completions.as_mut_ptr(),
                        MAX_COMPLETIONS as u32,
                        ptr::addr_of_mut!(removed),
                        1000,
                        0,
                    )
                };
                if ok == 0 {
                    // SAFETY: no preconditions.
                    let err = unsafe { GetLastError() };
                    if err == WAIT_TIMEOUT {
                        // Timeout is NOT progress: the cancelled completions are
                        // guaranteed to arrive, so keep waiting rather than free
                        // buffers the kernel may still write.
                        continue;
                    }
                    // A hard port error: we can no longer prove further completions
                    // will be posted, so stop to avoid an unbounded hang. The
                    // `debug_assert` in `shutdown` flags the (should-be-impossible)
                    // free-while-pending this would imply.
                    break;
                }
                drained += removed as usize;
            }
            // Their completions have been drained, so the zombie boxes are now
            // safe to free; clearing here keeps a repeat call a no-op.
            self.zombies.clear();
            (pending, drained)
        }

        /// Cancel and drain every in-flight AFD poll before the handles are closed
        /// and the state boxes freed, so the kernel never writes into a freed
        /// `iosb`/`poll_info`.
        fn shutdown(&mut self) {
            let (pending, drained) = self.cancel_and_drain_inflight();
            // Instrumentation: we must NOT free any buffer while a completion is
            // still pending. With a healthy port `drained == pending` always.
            debug_assert_eq!(
                drained, pending,
                "AFD buffers freed while a kernel completion was still pending"
            );
            // SAFETY: all in-flight I/O has completed/cancelled above, so closing
            // the handles and freeing the state boxes is sound.
            unsafe {
                CloseHandle(self.afd);
                CloseHandle(self.iocp);
            }
            self.entries.clear();
            self.index.clear();
            self.zombies.clear();
        }

        /// (entries, zombies, index) sizes — test-only inspection of internal
        /// bookkeeping (no zombie retention / phantom in-flight checks).
        #[cfg(test)]
        pub(crate) fn debug_state_counts(&self) -> (usize, usize, usize) {
            (self.entries.len(), self.zombies.len(), self.index.len())
        }

        /// Test-only: cancel + drain all in-flight polls and return
        /// `(pending, drained)` without closing handles, so a test can assert the
        /// drain-before-free invariant on a still-usable poller.
        #[cfg(test)]
        pub(crate) fn cancel_and_drain_for_test(&mut self) -> (usize, usize) {
            self.cancel_and_drain_inflight()
        }
    }

    /// Open a `\Device\Afd` helper handle for arming AFD polls.
    fn open_afd_helper() -> Option<HANDLE> {
        let name: Vec<u16> = r"\Device\Afd\Hew".encode_utf16().collect();
        let byte_len = (name.len() * 2) as u16;
        let mut unicode = UNICODE_STRING {
            Length: byte_len,
            MaximumLength: byte_len,
            Buffer: name.as_ptr().cast_mut(),
        };
        let mut object_attributes = OBJECT_ATTRIBUTES {
            Length: std::mem::size_of::<OBJECT_ATTRIBUTES>() as u32,
            RootDirectory: ptr::null_mut(),
            ObjectName: ptr::addr_of_mut!(unicode),
            Attributes: 0,
            SecurityDescriptor: ptr::null_mut(),
            SecurityQualityOfService: ptr::null_mut(),
        };
        let mut handle: HANDLE = ptr::null_mut();
        let mut iosb = IO_STATUS_BLOCK::zeroed();
        // SAFETY: all pointers reference live locals that outlive the call; `name`
        // backs the UNICODE_STRING buffer for the duration of NtCreateFile.
        let status = unsafe {
            NtCreateFile(
                ptr::addr_of_mut!(handle),
                SYNCHRONIZE,
                ptr::addr_of_mut!(object_attributes),
                ptr::addr_of_mut!(iosb),
                ptr::null_mut(),
                0,
                FILE_SHARE_READ | FILE_SHARE_WRITE,
                FILE_OPEN,
                0,
                ptr::null_mut(),
                0,
            )
        };
        if status == STATUS_SUCCESS {
            Some(handle)
        } else {
            None
        }
    }

    /// Create a new I/O poller.
    ///
    /// # Safety
    ///
    /// No preconditions.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_new() -> *mut HewIoPoller {
        match HewIoPoller::new() {
            Some(p) => Box::into_raw(Box::new(p)),
            None => ptr::null_mut(),
        }
    }

    /// Register a connection/listener token for read readiness.
    ///
    /// The `actor`/`msg_type` parameters are unused on this backend (the reactor
    /// drives the readiness-reporting `poll_ready` path and does its own
    /// lookup/recv/deliver). Returns 0 on success, -1 on error.
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`].
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_register(
        p: *mut HewIoPoller,
        fd: c_int,
        _actor: *mut super::HewActor,
        _msg_type: c_int,
        events: c_int,
    ) -> c_int {
        if p.is_null() {
            return -1;
        }
        // SAFETY: caller guarantees `p` is valid and reactor-owned (single thread).
        let poller = unsafe { &mut *p };
        poller.register(fd, events)
    }

    /// Unregister a token from the poller. Idempotent (benign double-unregister).
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`].
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_unregister(p: *mut HewIoPoller, fd: c_int) -> c_int {
        if p.is_null() {
            return -1;
        }
        // SAFETY: caller guarantees `p` is valid and reactor-owned.
        let poller = unsafe { &mut *p };
        poller.unregister(fd)
    }

    /// Auto-send poll variant — unsupported on this backend (the reactor uses
    /// `hew_io_poller_poll_ready`). Always returns -1.
    ///
    /// # Safety
    ///
    /// No preconditions.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_poll(_p: *mut HewIoPoller, _timeout_ms: c_int) -> c_int {
        -1
    }

    /// Poll for readiness and report ready `(token, hew_event_mask)` pairs without
    /// dispatching (the Windows IOCP/AFD counterpart of the epoll/kqueue
    /// `hew_io_poller_poll_ready`). Returns the number of ready tokens
    /// (0..=`out_cap`), or -1 on error.
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`]. `out_fds`
    /// and `out_events` must each point to at least `out_cap` writable `c_int`
    /// slots, or be null when `out_cap` is 0.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_poll_ready(
        p: *mut HewIoPoller,
        timeout_ms: c_int,
        out_fds: *mut c_int,
        out_events: *mut c_int,
        out_cap: c_int,
    ) -> c_int {
        if p.is_null() {
            return -1;
        }
        if out_cap <= 0 || out_fds.is_null() || out_events.is_null() {
            return 0;
        }
        // SAFETY: caller guarantees `p` is valid and reactor-owned.
        let poller = unsafe { &mut *p };
        poller.poll_ready(timeout_ms, out_fds, out_events, out_cap)
    }

    /// Stop and destroy the poller, draining all in-flight AFD polls first.
    ///
    /// # Safety
    ///
    /// `p` must be a valid pointer returned by [`hew_io_poller_new`], and must
    /// not be used after this call.
    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_stop(p: *mut HewIoPoller) {
        if p.is_null() {
            return;
        }
        // SAFETY: caller surrenders ownership of `p`.
        let mut poller = unsafe { Box::from_raw(p) };
        poller.shutdown();
    }
}

// ---- Stub (other unsupported native targets) -------------------------------

#[cfg(not(any(
    target_os = "linux",
    target_os = "freebsd",
    target_os = "macos",
    windows
)))]
mod platform {
    use std::ffi::c_int;

    /// Stub poller for unsupported platforms.
    #[derive(Debug)]
    pub struct HewIoPoller {
        _unused: u8,
    }

    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_new() -> *mut HewIoPoller {
        std::ptr::null_mut()
    }

    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_register(
        _p: *mut HewIoPoller,
        _fd: c_int,
        _actor: *mut super::HewActor,
        _msg_type: c_int,
        _events: c_int,
    ) -> c_int {
        -1
    }

    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_unregister(_p: *mut HewIoPoller, _fd: c_int) -> c_int {
        -1
    }

    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_poll(_p: *mut HewIoPoller, _timeout_ms: c_int) -> c_int {
        -1
    }

    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_poll_ready(
        _p: *mut HewIoPoller,
        _timeout_ms: c_int,
        _out_fds: *mut c_int,
        _out_events: *mut c_int,
        _out_cap: c_int,
    ) -> c_int {
        -1
    }

    #[no_mangle]
    pub unsafe extern "C" fn hew_io_poller_stop(_p: *mut HewIoPoller) {}
}

// Re-export the platform poller type so consumers can reference it.
pub use platform::HewIoPoller;

// Re-export the poller C-ABI entry points so the in-process active-mode reactor
// (`crate::reactor`) can drive the poller directly without an FFI round-trip.
// These are `#[no_mangle] extern "C"` for the codegen/runtime boundary; the
// re-export only adds a Rust path, it does not change the ABI.
//
// Re-exported unconditionally: both the real Unix poller (epoll/kqueue) and the
// fail-closed stub module (Windows and other unsupported targets) define all
// five entry points, so consumers can name `crate::io_time::hew_io_poller_*`
// on every non-wasm target. (Gating this to Unix previously left the symbols
// unreachable on Windows even though the stub `platform` module defines them.)
pub use platform::{
    hew_io_poller_new, hew_io_poller_poll_ready, hew_io_poller_register, hew_io_poller_stop,
    hew_io_poller_unregister,
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    // -- Duration -----------------------------------------------------------

    #[test]
    fn seconds_positive_converts_to_milliseconds() {
        // SAFETY: pure arithmetic, no preconditions.
        let d = unsafe { hew_seconds(5) };
        assert_eq!(d.ms, 5000);
    }

    #[test]
    fn seconds_zero_returns_zero() {
        // SAFETY: pure arithmetic, no preconditions.
        let d = unsafe { hew_seconds(0) };
        assert_eq!(d.ms, 0);
    }

    #[test]
    fn seconds_negative_wraps_via_unsigned_cast() {
        // -1i32.cast_unsigned() == u32::MAX; wrapping_mul(1000) wraps.
        // SAFETY: pure arithmetic, no preconditions.
        let d = unsafe { hew_seconds(-1) };
        let expected = u64::from(u32::MAX).wrapping_mul(1000);
        assert_eq!(d.ms, expected);
    }

    #[test]
    fn seconds_large_value_wraps_correctly() {
        // SAFETY: pure arithmetic, no preconditions.
        let d = unsafe { hew_seconds(c_int::MAX) };
        let expected = u64::from(c_int::MAX.cast_unsigned()).wrapping_mul(1000);
        assert_eq!(d.ms, expected);
    }

    #[test]
    fn milliseconds_positive_passes_through() {
        // SAFETY: pure arithmetic, no preconditions.
        let d = unsafe { hew_milliseconds(42) };
        assert_eq!(d.ms, 42);
    }

    #[test]
    fn milliseconds_zero_returns_zero() {
        // SAFETY: pure arithmetic, no preconditions.
        let d = unsafe { hew_milliseconds(0) };
        assert_eq!(d.ms, 0);
    }

    #[test]
    fn milliseconds_negative_wraps_via_unsigned_cast() {
        // SAFETY: pure arithmetic, no preconditions.
        let d = unsafe { hew_milliseconds(-1) };
        assert_eq!(d.ms, u64::from(u32::MAX));
    }

    // -- Clock --------------------------------------------------------------

    #[test]
    fn monotonic_ms_is_non_decreasing() {
        let t1 = monotonic_ms();
        let t2 = monotonic_ms();
        assert!(t2 >= t1);
    }

    #[test]
    fn now_ms_without_simtime_returns_monotonic_value() {
        // With simtime disabled (default), hew_now_ms delegates to
        // monotonic_ms. Verify it returns a sane value.
        // SAFETY: no preconditions.
        let t = unsafe { hew_now_ms() };
        // Epoch set on first call; subsequent calls return elapsed ms.
        // Verify within reasonable range (< 1 day).
        assert!(t < 86_400_000);
    }

    // -- File I/O -----------------------------------------------------------

    /// Helper: read a malloc'd C string, assert non-null, free it.
    ///
    /// # Safety
    ///
    /// `ptr` must be a non-null, NUL-terminated, malloc-allocated C string.
    unsafe fn read_and_free(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null(), "expected non-null C string pointer");
        // SAFETY: caller guarantees ptr is a valid NUL-terminated C string.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated header-aware by hew_read_file.
        unsafe { crate::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open (read_and_free test helper frees hew_read_file output)
        s
    }

    #[test]
    fn read_file_null_path_returns_null() {
        // SAFETY: null is explicitly handled by cabi_guard.
        let result = unsafe { hew_read_file(std::ptr::null()) };
        assert!(result.is_null());
    }

    #[test]
    fn read_file_nonexistent_path_returns_null() {
        let path = CString::new("/tmp/hew_test_nonexistent_file_XXXXXX").unwrap();
        // SAFETY: path is a valid NUL-terminated C string.
        let result = unsafe { hew_read_file(path.as_ptr()) };
        assert!(result.is_null());
    }

    #[test]
    fn read_file_valid_content_roundtrip() {
        let tmp = std::env::temp_dir().join(std::format!("hew_iotime_read_{}", std::process::id()));
        std::fs::write(&tmp, "hello from hew").unwrap();

        let path = CString::new(tmp.to_str().unwrap()).unwrap();
        // SAFETY: path is valid NUL-terminated; returned pointer is
        // malloc'd and non-null on success.
        let text = unsafe { read_and_free(hew_read_file(path.as_ptr())) };
        assert_eq!(text, "hello from hew");

        let _ = std::fs::remove_file(&tmp);
    }

    #[test]
    fn read_file_empty_returns_empty_string() {
        let tmp =
            std::env::temp_dir().join(std::format!("hew_iotime_empty_{}", std::process::id()));
        std::fs::write(&tmp, "").unwrap();

        let path = CString::new(tmp.to_str().unwrap()).unwrap();
        // SAFETY: path is a valid NUL-terminated C string.
        let text = unsafe { read_and_free(hew_read_file(path.as_ptr())) };
        assert_eq!(text, "");

        let _ = std::fs::remove_file(&tmp);
    }

    #[test]
    fn read_file_invalid_utf8_path_returns_null() {
        // Bytes that are valid Latin-1 but invalid UTF-8.
        // CStr::to_str() fails inside hew_read_file -> returns null.
        let bad_bytes: &[u8] = b"/tmp/\xff\xfe\x00";
        let c_path = CStr::from_bytes_with_nul(bad_bytes).unwrap();
        // SAFETY: c_path is a valid NUL-terminated C string.
        let result = unsafe { hew_read_file(c_path.as_ptr()) };
        assert!(result.is_null());
    }

    #[test]
    fn read_file_embedded_nul_truncates_at_first_nul() {
        // CONTRACT: hew_read_file returns a malloc'd C string (NUL-terminated).
        // Files with interior NUL bytes are read successfully, but the
        // returned C string is truncated at the first interior NUL — this
        // is inherent to C string semantics, not a bug. The full 7-byte
        // buffer ("abc\0def") is allocated, but CStr::from_ptr and any
        // C caller will only see "abc".
        let tmp = std::env::temp_dir().join(std::format!("hew_iotime_nul_{}", std::process::id()));
        std::fs::write(&tmp, "abc\0def").unwrap();

        let path = CString::new(tmp.to_str().unwrap()).unwrap();
        // SAFETY: path is valid NUL-terminated.
        let ptr = unsafe { hew_read_file(path.as_ptr()) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is malloc'd and NUL-terminated.
        let seen = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap();
        // C string truncates at first interior NUL.
        assert_eq!(seen, "abc");
        assert_eq!(seen.len(), 3, "C string length should stop at interior NUL");
        // SAFETY: ptr was allocated header-aware by hew_read_file.
        unsafe { crate::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open (test frees hew_read_file output)

        let _ = std::fs::remove_file(&tmp);
    }

    // -- Event flag constants -----------------------------------------------

    #[test]
    fn io_event_flags_are_distinct_bits() {
        let all = [HEW_IO_READ, HEW_IO_WRITE, HEW_IO_ERROR, HEW_IO_HUP];
        for (i, &a) in all.iter().enumerate() {
            assert_eq!(a.count_ones(), 1, "flag is not a single bit");
            for &b in &all[i + 1..] {
                assert_eq!(a & b, 0, "flags overlap");
            }
        }
    }

    // -- I/O Poller lifecycle -----------------------------------------------
    //
    // Exercises create/register/unregister/poll/stop using OS pipes.
    // Actual event *dispatch* (sending to an HewActor) requires an
    // initialised scheduler and is covered by E2E tests.

    #[cfg(any(target_os = "linux", target_os = "freebsd", target_os = "macos"))]
    mod poller {
        use super::*;
        use crate::io_time::platform::*;

        /// Non-null dummy actor pointer for register calls that will
        /// never trigger dispatch (no data written to pipe).
        fn dummy_actor() -> *mut HewActor {
            std::ptr::NonNull::<HewActor>::dangling().as_ptr()
        }

        /// Create an OS pipe pair, returning `(read_fd, write_fd)`.
        fn make_pipe() -> (c_int, c_int) {
            let mut fds: [c_int; 2] = [0; 2];
            // SAFETY: fds is a valid 2-element array.
            assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
            (fds[0], fds[1])
        }

        #[test]
        fn new_returns_non_null() {
            // SAFETY: no preconditions.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            // SAFETY: p is valid, surrendering ownership.
            unsafe { hew_io_poller_stop(p) };
        }

        #[test]
        fn stop_null_is_safe() {
            // SAFETY: null is explicitly guarded.
            unsafe { hew_io_poller_stop(std::ptr::null_mut()) };
        }

        #[test]
        fn register_null_poller_returns_error() {
            // SAFETY: null poller is guarded by cabi_guard.
            let rc = unsafe {
                hew_io_poller_register(
                    std::ptr::null_mut(),
                    0,
                    std::ptr::null_mut(),
                    0,
                    HEW_IO_READ,
                )
            };
            assert_eq!(rc, -1);
        }

        #[test]
        fn unregister_null_poller_returns_error() {
            // SAFETY: null poller is guarded by cabi_guard.
            let rc = unsafe { hew_io_poller_unregister(std::ptr::null_mut(), 0) };
            assert_eq!(rc, -1);
        }

        #[test]
        fn poll_null_poller_returns_error() {
            // SAFETY: null poller is guarded by cabi_guard.
            let rc = unsafe { hew_io_poller_poll(std::ptr::null_mut(), 0) };
            assert_eq!(rc, -1);
        }

        #[test]
        fn register_bad_fd_returns_error() {
            // SAFETY: no preconditions for new.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());

            // SAFETY: p is valid; -1 is an invalid fd that will make
            // epoll_ctl / kevent return an error.
            let rc = unsafe { hew_io_poller_register(p, -1, dummy_actor(), 1, HEW_IO_READ) };
            assert_eq!(rc, -1);

            // SAFETY: p is valid, surrendering ownership.
            unsafe { hew_io_poller_stop(p) };
        }

        #[test]
        fn register_and_unregister_valid_pipe() {
            // SAFETY: no preconditions for new.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            let (rfd, wfd) = make_pipe();

            // SAFETY: p is valid; rfd is a valid fd from pipe();
            // dummy_actor is non-null but never dispatched.
            let reg = unsafe { hew_io_poller_register(p, rfd, dummy_actor(), 1, HEW_IO_READ) };
            assert_eq!(reg, 0);

            // SAFETY: p is valid; rfd was previously registered.
            let unreg = unsafe { hew_io_poller_unregister(p, rfd) };
            assert_eq!(unreg, 0);

            // SAFETY: closing our own fds; p surrendering ownership.
            unsafe {
                libc::close(rfd);
                libc::close(wfd);
                hew_io_poller_stop(p);
            }
        }

        // Platform-specific: epoll_ctl(DEL) fails for unregistered fds
        // on Linux (returns -1), but the kqueue backend on macOS/FreeBSD
        // ignores EV_DELETE errors and always returns 0.
        #[test]
        fn unregister_unknown_fd_platform_behaviour() {
            // SAFETY: no preconditions for new.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());

            // SAFETY: p is valid; fd 9999 was never registered.
            let rc = unsafe { hew_io_poller_unregister(p, 9999) };

            #[cfg(target_os = "linux")]
            assert_eq!(rc, -1, "epoll returns -1 for unregistered fd");

            #[cfg(any(target_os = "macos", target_os = "freebsd"))]
            assert_eq!(rc, 0, "kqueue silently ignores EV_DELETE for unknown fd");

            // SAFETY: p is valid, surrendering ownership.
            unsafe { hew_io_poller_stop(p) };
        }

        #[test]
        fn poll_empty_zero_timeout_returns_zero() {
            // SAFETY: no preconditions for new.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());

            // SAFETY: p is valid; 0ms timeout returns immediately.
            let n = unsafe { hew_io_poller_poll(p, 0) };
            assert_eq!(n, 0);

            // SAFETY: p is valid, surrendering ownership.
            unsafe { hew_io_poller_stop(p) };
        }

        #[test]
        fn poll_registered_no_data_zero_timeout_returns_zero() {
            // SAFETY: no preconditions for new.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            let (rfd, wfd) = make_pipe();

            // SAFETY: p is valid; rfd from pipe(); dummy_actor non-null.
            let reg = unsafe { hew_io_poller_register(p, rfd, dummy_actor(), 1, HEW_IO_READ) };
            assert_eq!(reg, 0);

            // Nothing written to the pipe -> 0ms timeout -> 0 events.
            // SAFETY: p is valid.
            let n = unsafe { hew_io_poller_poll(p, 0) };
            assert_eq!(n, 0);

            // SAFETY: p is valid; closing our own fds.
            unsafe {
                hew_io_poller_unregister(p, rfd);
                libc::close(rfd);
                libc::close(wfd);
                hew_io_poller_stop(p);
            }
        }

        #[test]
        fn poll_ready_reports_ready_fd_without_dispatch() {
            // SAFETY: no preconditions for new.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            let (rfd, wfd) = make_pipe();

            // Register the read end. The actor is a dangling dummy that must
            // NEVER be dereferenced — poll_ready reports readiness, it does not
            // dispatch, so the dummy is safe even though data is written.
            // SAFETY: p valid; rfd from pipe(); dummy actor never dispatched.
            let reg = unsafe { hew_io_poller_register(p, rfd, dummy_actor(), 1, HEW_IO_READ) };
            assert_eq!(reg, 0);

            // Write a byte so the read end becomes ready.
            let byte = [0xABu8];
            // SAFETY: wfd is a valid write fd; byte is one valid byte.
            let written = unsafe { libc::write(wfd, byte.as_ptr().cast(), 1) };
            assert_eq!(written, 1);

            let mut out_fds = [0i32; 8];
            let mut out_events = [0i32; 8];
            // SAFETY: p valid; out buffers have 8 slots each; cap is 8.
            let n = unsafe {
                hew_io_poller_poll_ready(p, 100, out_fds.as_mut_ptr(), out_events.as_mut_ptr(), 8)
            };
            assert_eq!(n, 1, "exactly one fd should be ready");
            assert_eq!(out_fds[0], rfd, "the ready fd is the registered read end");
            assert!(
                out_events[0] & HEW_IO_READ != 0,
                "ready event must carry READ"
            );

            // SAFETY: p valid; closing our own fds.
            unsafe {
                hew_io_poller_unregister(p, rfd);
                libc::close(rfd);
                libc::close(wfd);
                hew_io_poller_stop(p);
            }
        }

        #[test]
        fn poll_ready_empty_returns_zero() {
            // SAFETY: no preconditions for new.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            let mut out_fds = [0i32; 4];
            let mut out_events = [0i32; 4];
            // SAFETY: p valid; buffers have 4 slots; 0ms timeout returns at once.
            let n = unsafe {
                hew_io_poller_poll_ready(p, 0, out_fds.as_mut_ptr(), out_events.as_mut_ptr(), 4)
            };
            assert_eq!(n, 0);
            // SAFETY: p valid, surrendering ownership.
            unsafe { hew_io_poller_stop(p) };
        }

        #[test]
        fn poll_ready_zero_cap_returns_zero() {
            // SAFETY: no preconditions for new.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            // SAFETY: p valid; out_cap 0 means no slots, returns 0 without
            // touching the (null) buffers.
            let n = unsafe {
                hew_io_poller_poll_ready(p, 0, std::ptr::null_mut(), std::ptr::null_mut(), 0)
            };
            assert_eq!(n, 0);
            // SAFETY: p valid, surrendering ownership.
            unsafe { hew_io_poller_stop(p) };
        }

        #[test]
        fn register_both_read_and_write_succeeds() {
            // SAFETY: no preconditions for new.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            let (rfd, wfd) = make_pipe();

            // SAFETY: p valid; rfd from pipe(); dummy_actor non-null.
            let rc = unsafe {
                hew_io_poller_register(p, rfd, dummy_actor(), 1, HEW_IO_READ | HEW_IO_WRITE)
            };
            assert_eq!(rc, 0);

            // SAFETY: p valid; closing our own fds.
            unsafe {
                hew_io_poller_unregister(p, rfd);
                libc::close(rfd);
                libc::close(wfd);
                hew_io_poller_stop(p);
            }
        }
    }

    // -- Windows IOCP/AFD_POLL readiness (G0 spike + backend coverage) -------
    //
    // Drives the real IOCP + AFD_POLL backend over loopback TCP sockets, proving
    // peer-write → AFD_POLL_RECEIVE → HEW_IO_READ readiness and peer-close →
    // HEW_IO_HUP, plus the register/unregister/stop lifecycle.
    #[cfg(windows)]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        clippy::cast_sign_loss,
        reason = "test-only FFI: every pointer is a fresh local poller/socket the test \
                  body sets up and tears down; the lifecycle is described inline"
    )]
    mod afd_poller {
        use super::*;
        use std::io::Write;
        use std::time::{Duration, Instant};

        /// Poll until `poll_ready` reports the token (any event) or the deadline
        /// elapses. Returns the reported event mask, or `None` on timeout.
        unsafe fn wait_ready(p: *mut HewIoPoller, token: c_int, ms: u64) -> Option<c_int> {
            let deadline = Instant::now() + Duration::from_millis(ms);
            let mut fds = [0_i32; 8];
            let mut evs = [0_i32; 8];
            while Instant::now() < deadline {
                let n = unsafe {
                    hew_io_poller_poll_ready(p, 50, fds.as_mut_ptr(), evs.as_mut_ptr(), 8)
                };
                if n > 0 {
                    for i in 0..n as usize {
                        if fds[i] == token {
                            return Some(evs[i]);
                        }
                    }
                }
            }
            None
        }

        #[test]
        fn new_returns_non_null_and_stop_is_clean() {
            // SAFETY: no preconditions.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null(), "IOCP poller creation failed");
            // SAFETY: p valid; surrenders ownership.
            unsafe { hew_io_poller_stop(p) };
        }

        #[test]
        fn register_unknown_token_returns_error() {
            // SAFETY: no preconditions.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            // A token that names no live socket must fail closed.
            // SAFETY: p valid.
            let rc =
                unsafe { hew_io_poller_register(p, 999_999, std::ptr::null_mut(), 0, HEW_IO_READ) };
            assert_eq!(rc, -1);
            // SAFETY: p valid.
            unsafe { hew_io_poller_stop(p) };
        }

        #[test]
        fn peer_write_reports_read_readiness() {
            // SAFETY: no preconditions.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            let (conn, mut client) = crate::transport::tcp_socketpair_conn_for_test();
            // SAFETY: conn is a live stream handle = the poller token (D-2a).
            let token = crate::transport::tcp_conn_raw_fd(conn).expect("conn token");
            assert!(crate::transport::tcp_conn_set_nonblocking(conn, true));
            // SAFETY: p valid; token names a live socket.
            assert_eq!(
                unsafe { hew_io_poller_register(p, token, std::ptr::null_mut(), 0, HEW_IO_READ) },
                0
            );

            client.write_all(b"ping").expect("client write");
            client.flush().ok();

            // SAFETY: p valid; token registered.
            let mask = unsafe { wait_ready(p, token, 2000) }.expect("no read readiness reported");
            assert!(
                mask & HEW_IO_READ != 0,
                "expected HEW_IO_READ, got {mask:#x}"
            );

            // SAFETY: cleanup.
            unsafe {
                hew_io_poller_unregister(p, token);
                hew_io_poller_stop(p);
            }
            drop(client);
            crate::transport::tcp_close_raw_for_test(conn);
        }

        #[test]
        fn peer_close_reports_hup() {
            // SAFETY: no preconditions.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            let (conn, client) = crate::transport::tcp_socketpair_conn_for_test();
            // SAFETY: conn is a live stream handle.
            let token = crate::transport::tcp_conn_raw_fd(conn).expect("conn token");
            assert!(crate::transport::tcp_conn_set_nonblocking(conn, true));
            // SAFETY: p valid; token names a live socket.
            assert_eq!(
                unsafe { hew_io_poller_register(p, token, std::ptr::null_mut(), 0, HEW_IO_READ) },
                0
            );

            // Peer closes its end → AFD reports disconnect/abort.
            drop(client);

            // SAFETY: p valid; token registered.
            let mask = unsafe { wait_ready(p, token, 2000) }.expect("no close readiness reported");
            assert!(
                mask & (HEW_IO_HUP | HEW_IO_ERROR | HEW_IO_READ) != 0,
                "expected close/read readiness, got {mask:#x}"
            );

            // SAFETY: cleanup.
            unsafe {
                hew_io_poller_unregister(p, token);
                hew_io_poller_stop(p);
            }
            crate::transport::tcp_close_raw_for_test(conn);
        }

        #[test]
        fn double_unregister_is_benign() {
            // SAFETY: no preconditions.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            let (conn, client) = crate::transport::tcp_socketpair_conn_for_test();
            // SAFETY: conn is a live stream handle.
            let token = crate::transport::tcp_conn_raw_fd(conn).expect("conn token");
            assert!(crate::transport::tcp_conn_set_nonblocking(conn, true));
            // SAFETY: p valid.
            assert_eq!(
                unsafe { hew_io_poller_register(p, token, std::ptr::null_mut(), 0, HEW_IO_READ) },
                0
            );
            // SAFETY: p valid; second unregister must be a no-op (returns 0).
            unsafe {
                assert_eq!(hew_io_poller_unregister(p, token), 0);
                assert_eq!(hew_io_poller_unregister(p, token), 0);
                hew_io_poller_stop(p);
            }
            drop(client);
            crate::transport::tcp_close_raw_for_test(conn);
        }

        /// Poll until `poll_ready` reports a TERMINAL (HUP/ERROR) mask for `token`
        /// or the deadline elapses. Returns the terminal mask, or `None`.
        unsafe fn wait_terminal(p: *mut HewIoPoller, token: c_int, ms: u64) -> Option<c_int> {
            let deadline = Instant::now() + Duration::from_millis(ms);
            let mut fds = [0_i32; 8];
            let mut evs = [0_i32; 8];
            while Instant::now() < deadline {
                let n = unsafe {
                    hew_io_poller_poll_ready(p, 50, fds.as_mut_ptr(), evs.as_mut_ptr(), 8)
                };
                for i in 0..n.max(0) as usize {
                    if fds[i] == token && evs[i] & (HEW_IO_HUP | HEW_IO_ERROR) != 0 {
                        return Some(evs[i]);
                    }
                }
            }
            None
        }

        /// REGRESSION (MED/code, zombie leak): a peer close delivers a terminal
        /// completion that is NOT re-armed; the subsequent `unregister` must DROP
        /// the disarmed state directly — no `NtCancelIoFileEx`, no zombie — so
        /// there is no per-connection heap leak and no phantom in-flight count at
        /// shutdown, and the whole HUP→unregister→stop sequence has bounded latency.
        #[test]
        fn close_readiness_unregister_drops_without_zombie() {
            // SAFETY: no preconditions.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            let (conn, client) = crate::transport::tcp_socketpair_conn_for_test();
            // SAFETY: conn is a live stream handle = the poller token.
            let token = crate::transport::tcp_conn_raw_fd(conn).expect("conn token");
            assert!(crate::transport::tcp_conn_set_nonblocking(conn, true));
            // SAFETY: p valid; token names a live socket.
            assert_eq!(
                unsafe { hew_io_poller_register(p, token, std::ptr::null_mut(), 0, HEW_IO_READ) },
                0
            );

            // Peer closes → AFD reports disconnect/abort: a terminal completion the
            // poller delivers and deliberately does not re-arm.
            drop(client);
            // SAFETY: p valid; token registered.
            let mask = unsafe { wait_terminal(p, token, 2000) }
                .expect("no terminal (HUP/ERROR) close readiness reported");
            assert!(mask & (HEW_IO_HUP | HEW_IO_ERROR) != 0, "got {mask:#x}");

            // The state is now disarmed (terminal completion drained, no re-arm).
            // unregister must drop it directly rather than zombieing it.
            let start = Instant::now();
            // SAFETY: p valid.
            assert_eq!(unsafe { hew_io_poller_unregister(p, token) }, 0);
            // SAFETY: p valid; inspecting internal bookkeeping on the reactor thread.
            let (entries, zombies, index) = unsafe { (*p).debug_state_counts() };
            assert_eq!(entries, 0, "entry not dropped on unregister");
            assert_eq!(
                zombies, 0,
                "terminal state was zombied — per-connection leak"
            );
            assert_eq!(index, 0, "index left dangling");

            // Nothing is in flight, so stop drains zero and returns promptly.
            // SAFETY: p valid; surrenders ownership.
            unsafe { hew_io_poller_stop(p) };
            assert!(
                start.elapsed() < Duration::from_millis(750),
                "HUP→unregister→stop latency unbounded: {:?}",
                start.elapsed()
            );
            crate::transport::tcp_close_raw_for_test(conn);
        }

        /// REGRESSION (SEC-HIGH, shutdown UAF): with a genuinely-pending AFD poll
        /// IOCTL in flight, shutdown must CANCEL and DRAIN its completion before any
        /// buffer is freed. The drain is unbounded/proven: it reports exactly one
        /// pending request and drains exactly one completion (`pending == drained`),
        /// so no `AfdState`/`iosb`/`poll_info` is freed while the kernel still owns a
        /// pending write. (`shutdown` also carries a `debug_assert_eq!` enforcing
        /// the same no-free-while-pending invariant.)
        #[test]
        fn shutdown_drains_genuinely_pending_inflight_poll() {
            // SAFETY: no preconditions.
            let p = unsafe { hew_io_poller_new() };
            assert!(!p.is_null());
            let (conn, client) = crate::transport::tcp_socketpair_conn_for_test();
            // SAFETY: conn is a live stream handle = the poller token.
            let token = crate::transport::tcp_conn_raw_fd(conn).expect("conn token");
            assert!(crate::transport::tcp_conn_set_nonblocking(conn, true));
            // Arm a poll and leave it genuinely in flight: no peer write, no close,
            // so the one-shot IOCTL_AFD_POLL is still pending in the kernel.
            // SAFETY: p valid; token names a live socket.
            assert_eq!(
                unsafe { hew_io_poller_register(p, token, std::ptr::null_mut(), 0, HEW_IO_READ) },
                0
            );
            // SAFETY: p valid; single-threaded inspection.
            let (entries, zombies, _) = unsafe { (*p).debug_state_counts() };
            assert_eq!(entries, 1, "expected one armed entry");
            assert_eq!(zombies, 0);

            // Drive the exact cancel-and-drain shutdown performs, observing counts.
            // SAFETY: p valid and still owned (cancel_and_drain leaves handles open).
            let (pending, drained) = unsafe { (*p).cancel_and_drain_for_test() };
            assert_eq!(pending, 1, "the in-flight poll was not counted as pending");
            assert_eq!(
                drained, pending,
                "shutdown would free buffers with a pending completion (UAF)"
            );

            // Now fully stop/free (the in-flight completion is already drained, so
            // this second cancel-and-drain is a no-op and cannot hang).
            // SAFETY: p valid; surrenders ownership.
            unsafe { hew_io_poller_stop(p) };
            drop(client);
            crate::transport::tcp_close_raw_for_test(conn);
        }
    }
}
