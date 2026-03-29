//! File I/O, sleep, clock, and I/O poller for the Hew runtime.
//!
//! Provides `hew_read_file`, `hew_sleep_ms`, `hew_now_ms`, duration helpers,
//! and a platform I/O poller (epoll on Linux, kqueue on FreeBSD/macOS, stub
//! elsewhere).

#[cfg(any(target_os = "linux", target_os = "freebsd", target_os = "macos"))]
use std::ffi::c_void;
use std::ffi::{c_char, c_int, CStr};

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
    cabi_guard!(path.is_null(), std::ptr::null_mut());
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
            #[expect(clippy::cast_sign_loss, reason = "fd is a valid file descriptor from the OS, always non-negative")]
            changelist.push(libc::kevent {
                ident: fd as usize,
                filter: libc::EVFILT_READ,
                flags: libc::EV_ADD | libc::EV_CLEAR,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
            });
        }
        if events & HEW_IO_WRITE != 0 {
            #[expect(clippy::cast_sign_loss, reason = "fd is a valid file descriptor from the OS, always non-negative")]
            changelist.push(libc::kevent {
                ident: fd as usize,
                filter: libc::EVFILT_WRITE,
                flags: libc::EV_ADD | libc::EV_CLEAR,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
            });
        }

        if changelist.is_empty() {
            return -1;
        }

        // SAFETY: kevent with valid kq, changelist, and no eventlist.
        #[expect(clippy::cast_possible_truncation, reason = "changelist has at most 2 entries, fits in c_int")]
        #[expect(clippy::cast_possible_wrap, reason = "changelist has at most 2 entries, fits in c_int")]
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
        #[expect(clippy::cast_sign_loss, reason = "fd is a valid file descriptor from the OS, always non-negative")]
        let changelist = [
            libc::kevent {
                ident: fd as usize,
                filter: libc::EVFILT_READ,
                flags: libc::EV_DELETE,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
            },
            libc::kevent {
                ident: fd as usize,
                filter: libc::EVFILT_WRITE,
                flags: libc::EV_DELETE,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
            },
        ];

        // SAFETY: kevent with valid kq and changelist.
        #[expect(clippy::cast_possible_truncation, reason = "changelist is a fixed-size array of 2 elements, fits in c_int")]
        #[expect(clippy::cast_possible_wrap, reason = "changelist is a fixed-size array of 2 elements, fits in c_int")]
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

        #[expect(clippy::cast_possible_truncation, reason = "MAX_EVENTS is 64, fits in c_int")]
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
    pub unsafe extern "C" fn hew_io_poller_stop(_p: *mut HewIoPoller) {}
}

// Re-export the platform poller type so consumers can reference it.
pub use platform::HewIoPoller;

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
        // SAFETY: ptr was allocated with libc::malloc.
        unsafe { libc::free(ptr.cast()) };
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
        // SAFETY: ptr was allocated with libc::malloc.
        unsafe { libc::free(ptr.cast()) };

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
