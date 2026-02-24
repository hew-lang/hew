//! File I/O, sleep, clock, and I/O poller for the Hew runtime.
//!
//! Provides `hew_read_file`, `hew_sleep_ms`, `hew_now_ms`, duration helpers,
//! and an epoll-based I/O poller (Linux only; stub on other platforms).

use std::ffi::{c_char, c_int, c_void, CStr};

// ---------------------------------------------------------------------------
// Duration
// ---------------------------------------------------------------------------

/// Duration value exchanged with C code.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewDuration {
    /// Milliseconds.
    pub ms: u64,
}

/// Create a [`HewDuration`] from whole seconds.
///
/// # Safety
///
/// No preconditions — pure arithmetic.
#[no_mangle]
pub unsafe extern "C" fn hew_seconds(s: c_int) -> HewDuration {
    HewDuration {
        ms: u64::from(s.cast_unsigned()).wrapping_mul(1000),
    }
}

/// Create a [`HewDuration`] from milliseconds.
///
/// # Safety
///
/// No preconditions — pure arithmetic.
#[no_mangle]
pub unsafe extern "C" fn hew_milliseconds(ms: c_int) -> HewDuration {
    HewDuration {
        ms: u64::from(ms.cast_unsigned()),
    }
}

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
    if path.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `path` is a valid C string.
    let c_path = unsafe { CStr::from_ptr(path) };
    let Ok(rust_path) = c_path.to_str() else {
        return std::ptr::null_mut();
    };
    let Ok(contents) = std::fs::read_to_string(rust_path) else {
        return std::ptr::null_mut();
    };
    let len = contents.len();
    // SAFETY: allocating len+1 bytes via libc::malloc is valid for any positive size.
    let buf = unsafe { libc::malloc(len + 1) }.cast::<u8>();
    if buf.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `buf` is freshly allocated with at least `len + 1` bytes.
    unsafe {
        std::ptr::copy_nonoverlapping(contents.as_ptr(), buf, len);
        *buf.add(len) = 0; // NUL terminator
    }
    buf.cast::<c_char>()
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
pub unsafe extern "C" fn hew_sleep_ms(ms: c_int) {
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
    epoch.elapsed().as_millis() as u64
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

// ---------------------------------------------------------------------------
// I/O Poller (epoll on Linux, stub elsewhere)
// ---------------------------------------------------------------------------

/// I/O event interest flags.
pub const HEW_IO_READ: c_int = 0x01;
/// I/O event interest flag: write-ready.
pub const HEW_IO_WRITE: c_int = 0x02;
/// I/O event interest flag: error.
pub const HEW_IO_ERROR: c_int = 0x04;
/// I/O event interest flag: hang-up.
pub const HEW_IO_HUP: c_int = 0x08;

/// Opaque actor handle — the actor module is not yet implemented so we declare
/// the send function as an external symbol.
#[repr(C)]
#[derive(Debug)]
pub struct HewActor {
    _opaque: [u8; 0],
}

extern "C" {
    fn hew_actor_send(actor: *mut HewActor, msg_type: c_int, data: *mut c_void, data_size: usize);
}

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
        if p.is_null() {
            return -1;
        }
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
        if p.is_null() {
            return -1;
        }
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
        if p.is_null() {
            return -1;
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

// ---- Non-Linux (stub) -----------------------------------------------------

#[cfg(not(target_os = "linux"))]
mod platform {
    use std::ffi::c_int;

    /// Stub poller for non-Linux platforms.
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
    pub unsafe extern "C" fn hew_io_poller_stop(_p: *mut HewIoPoller) {}
}

// Re-export the platform poller type so consumers can reference it.
pub use platform::HewIoPoller;
