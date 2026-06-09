//! File I/O, sleep, clock, and I/O poller for the Hew runtime.
//!
//! Provides `hew_read_file`, `hew_sleep_ms`, `hew_now_ms`, duration helpers,
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

/// Sleep for `ms` milliseconds.
///
/// # Safety
///
/// No preconditions — delegates to the OS.
#[no_mangle]
pub unsafe extern "C" fn hew_sleep_ms(ms: i64) {
    if ms > 0 {
        // SAFETY: ms > 0 checked above, so cast is lossless.
        #[expect(clippy::cast_sign_loss, reason = "guarded by ms > 0")]
        let dur = std::time::Duration::from_millis(ms as u64);
        std::thread::sleep(dur);
    }
}

/// Cross-platform monotonic clock in milliseconds.
fn monotonic_ms() -> u64 {
    use std::sync::OnceLock;
    use std::time::Instant;
    static EPOCH: OnceLock<Instant> = OnceLock::new();
    let epoch = EPOCH.get_or_init(Instant::now);
    #[expect(
        clippy::cast_possible_truncation,
        reason = "monotonic ms since process start won't exceed u64"
    )]
    {
        epoch.elapsed().as_millis() as u64
    }
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

// ---- Stub (Windows, WASM, etc.) --------------------------------------------

#[cfg(not(any(target_os = "linux", target_os = "freebsd", target_os = "macos")))]
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

    // -- Sleep --------------------------------------------------------------

    #[test]
    fn sleep_zero_is_noop() {
        // SAFETY: no preconditions; ms <= 0 is a no-op.
        unsafe { hew_sleep_ms(0) };
    }

    #[test]
    fn sleep_negative_is_noop() {
        // SAFETY: no preconditions; ms <= 0 is a no-op.
        unsafe { hew_sleep_ms(-1) };
    }

    #[test]
    fn sleep_small_positive_completes() {
        // Verify it doesn't panic or hang. No timing assertion.
        // SAFETY: no preconditions.
        unsafe { hew_sleep_ms(1) };
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
}
