//! File reads and the platform readiness poller for the Hew runtime.
//!
//! The poller is the per-OS half of the reactor: epoll with an eventfd wake on
//! Linux, kqueue with an `EVFILT_USER` wake on FreeBSD and macOS, and an I/O
//! completion port with `AFD_POLL` readiness and a posted wake packet on
//! Windows. Every arm is one-shot: one arm reports at most one readiness, and
//! the waiting task re-arms after its next syscall would block. Arming is
//! thread-safe, so the task that met `WouldBlock` arms from its own worker.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use hew_cabi::string::HewString;
use std::ffi::c_int;

// ---------------------------------------------------------------------------
// Duration
// ---------------------------------------------------------------------------

pub use crate::duration::{hew_milliseconds, hew_seconds, HewDuration};

// ---------------------------------------------------------------------------
// File I/O
// ---------------------------------------------------------------------------

/// Read an entire file and return an owned managed string.
///
/// Returns a null pointer on failure.
///
/// # Safety
///
/// `path` must be a live managed string or canonical null/empty.
/// Release the returned managed owner with `hew_string_drop`.
#[no_mangle]
pub unsafe extern "C" fn hew_read_file(path: *const HewString) -> *mut HewString {
    // SAFETY: this alias shares the managed path and result contract.
    unsafe { crate::file_io::hew_file_read(path) }
}

// ---------------------------------------------------------------------------
// Readiness poller
// ---------------------------------------------------------------------------

/// Readiness interest and report flag: a read would make progress.
pub const HEW_IO_READ: c_int = 0x01;
/// Readiness interest and report flag: a write would make progress.
pub const HEW_IO_WRITE: c_int = 0x02;
/// Readiness report flag: the socket reported an error.
pub const HEW_IO_ERROR: c_int = 0x04;
/// Readiness report flag: the peer hung up.
pub const HEW_IO_HUP: c_int = 0x08;

/// One readiness report. `token` names the reactor slot on Unix; on Windows it
/// is the completion context the arm supplied.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Event {
    pub(crate) token: u64,
    pub(crate) events: c_int,
}

/// Reserved token for the wake source; no slot uses it.
#[cfg(any(target_os = "linux", target_os = "freebsd", target_os = "macos"))]
const WAKE_TOKEN: u64 = u64::MAX;

/// Most events drained per wait. Excess readiness surfaces on the next wait.
const MAX_EVENTS: usize = 256;

// ---- Linux (epoll + eventfd) ------------------------------------------------

#[cfg(target_os = "linux")]
#[allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    reason = "descriptors, tokens and event counts are nonnegative kernel values; counts \
              are bounded by MAX_EVENTS"
)]
mod platform {
    use super::{
        Event, HEW_IO_ERROR, HEW_IO_HUP, HEW_IO_READ, HEW_IO_WRITE, MAX_EVENTS, WAKE_TOKEN,
    };
    use std::ffi::c_int;
    use std::io;
    use std::os::fd::RawFd;

    /// Epoll set plus the eventfd that interrupts a wait.
    #[derive(Debug)]
    pub(crate) struct Poller {
        epfd: RawFd,
        wake: RawFd,
    }

    fn check(rc: c_int) -> io::Result<c_int> {
        if rc < 0 {
            Err(io::Error::last_os_error())
        } else {
            Ok(rc)
        }
    }

    fn interest_bits(interest: c_int) -> u32 {
        let mut bits = libc::EPOLLONESHOT as u32;
        if interest & HEW_IO_READ != 0 {
            bits |= (libc::EPOLLIN | libc::EPOLLRDHUP) as u32;
        }
        if interest & HEW_IO_WRITE != 0 {
            bits |= libc::EPOLLOUT as u32;
        }
        bits
    }

    fn report_bits(bits: u32) -> c_int {
        let mut events = 0;
        if bits & libc::EPOLLIN as u32 != 0 {
            events |= HEW_IO_READ;
        }
        if bits & libc::EPOLLOUT as u32 != 0 {
            events |= HEW_IO_WRITE;
        }
        if bits & libc::EPOLLERR as u32 != 0 {
            events |= HEW_IO_ERROR;
        }
        if bits & (libc::EPOLLHUP | libc::EPOLLRDHUP) as u32 != 0 {
            events |= HEW_IO_HUP;
        }
        events
    }

    impl Poller {
        pub(crate) fn new() -> io::Result<Self> {
            // SAFETY: no pointer arguments.
            let epfd = check(unsafe { libc::epoll_create1(libc::EPOLL_CLOEXEC) })?;
            // SAFETY: no pointer arguments.
            let wake =
                match check(unsafe { libc::eventfd(0, libc::EFD_CLOEXEC | libc::EFD_NONBLOCK) }) {
                    Ok(fd) => fd,
                    Err(error) => {
                        // SAFETY: closing the epoll fd this constructor created.
                        unsafe { libc::close(epfd) };
                        return Err(error);
                    }
                };
            let poller = Self { epfd, wake };
            // The wake source stays level-triggered: a pending count keeps the
            // next wait from sleeping until the reactor drains it.
            let mut event = libc::epoll_event {
                events: libc::EPOLLIN as u32,
                u64: WAKE_TOKEN,
            };
            // SAFETY: both fds are live and the event is a valid local.
            check(unsafe { libc::epoll_ctl(epfd, libc::EPOLL_CTL_ADD, wake, &raw mut event) })?;
            Ok(poller)
        }

        /// Arm one readiness report for `fd`. `added` says whether `fd` is
        /// already in the set; the first arm adds it, later arms modify it.
        pub(crate) fn arm(
            &self,
            fd: RawFd,
            token: u64,
            interest: c_int,
            added: bool,
        ) -> io::Result<()> {
            let mut event = libc::epoll_event {
                events: interest_bits(interest),
                u64: token,
            };
            let op = if added {
                libc::EPOLL_CTL_MOD
            } else {
                libc::EPOLL_CTL_ADD
            };
            // SAFETY: fd is a live socket owned by the caller's slot.
            check(unsafe { libc::epoll_ctl(self.epfd, op, fd, &raw mut event) }).map(|_| ())
        }

        /// Remove `fd` before its slot closes it. A later report for the old
        /// token is ignored because the slot no longer exists.
        pub(crate) fn remove(&self, fd: RawFd) {
            // SAFETY: fd is still open; failure only means it was never added.
            unsafe { libc::epoll_ctl(self.epfd, libc::EPOLL_CTL_DEL, fd, std::ptr::null_mut()) };
        }

        /// Interrupt a wait. The count persists until drained, so a wake sent
        /// before the reactor sleeps is never lost.
        pub(crate) fn wake(&self) {
            let one: u64 = 1;
            // SAFETY: writes eight bytes from a local to the eventfd.
            unsafe { libc::write(self.wake, (&raw const one).cast(), 8) };
        }

        /// Wait up to `timeout_ms` (negative: no limit) and append readiness
        /// reports. A wake is drained here and reports nothing.
        pub(crate) fn wait(&self, timeout_ms: c_int, out: &mut Vec<Event>) -> io::Result<()> {
            let mut events = [libc::epoll_event { events: 0, u64: 0 }; MAX_EVENTS];
            // SAFETY: the buffer holds MAX_EVENTS entries.
            let count = check(unsafe {
                libc::epoll_wait(
                    self.epfd,
                    events.as_mut_ptr(),
                    MAX_EVENTS as c_int,
                    timeout_ms,
                )
            })?;
            for event in &events[..count as usize] {
                if event.u64 == WAKE_TOKEN {
                    let mut drained: u64 = 0;
                    // SAFETY: reads eight bytes into a local; EAGAIN is benign.
                    unsafe { libc::read(self.wake, (&raw mut drained).cast(), 8) };
                    continue;
                }
                out.push(Event {
                    token: event.u64,
                    events: report_bits(event.events),
                });
            }
            Ok(())
        }
    }
}

// ---- FreeBSD / macOS (kqueue + EVFILT_USER) ---------------------------------

#[cfg(any(target_os = "freebsd", target_os = "macos"))]
#[allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    reason = "descriptors, tokens and event counts are nonnegative kernel values; counts \
              are bounded by MAX_EVENTS"
)]
mod platform {
    use super::{
        Event, HEW_IO_ERROR, HEW_IO_HUP, HEW_IO_READ, HEW_IO_WRITE, MAX_EVENTS, WAKE_TOKEN,
    };
    use std::ffi::c_int;
    use std::io;
    use std::os::fd::RawFd;

    /// Kqueue plus a user event that interrupts a wait.
    #[derive(Debug)]
    pub(crate) struct Poller {
        kq: RawFd,
    }

    fn change(ident: usize, filter: i16, flags: u16, fflags: u32, token: u64) -> libc::kevent {
        libc::kevent {
            ident,
            filter,
            flags,
            fflags,
            data: 0,
            udata: token as usize as *mut libc::c_void,
            #[cfg(target_os = "freebsd")]
            ext: [0; 4],
        }
    }

    impl Poller {
        fn apply(&self, changes: &[libc::kevent]) -> io::Result<()> {
            // SAFETY: changes is a valid slice; no event list is requested.
            let rc = unsafe {
                libc::kevent(
                    self.kq,
                    changes.as_ptr(),
                    changes.len() as c_int,
                    std::ptr::null_mut(),
                    0,
                    std::ptr::null(),
                )
            };
            if rc < 0 {
                Err(io::Error::last_os_error())
            } else {
                Ok(())
            }
        }

        pub(crate) fn new() -> io::Result<Self> {
            // SAFETY: no arguments.
            let kq = unsafe { libc::kqueue() };
            if kq < 0 {
                return Err(io::Error::last_os_error());
            }
            // SAFETY: kq is the descriptor just created.
            unsafe { libc::fcntl(kq, libc::F_SETFD, libc::FD_CLOEXEC) };
            let poller = Self { kq };
            poller.apply(&[change(
                0,
                libc::EVFILT_USER,
                libc::EV_ADD | libc::EV_CLEAR,
                0,
                WAKE_TOKEN,
            )])?;
            Ok(poller)
        }

        /// Arm one readiness report per requested direction. Each filter is
        /// one-shot, so arming reads leaves an armed write filter alone.
        pub(crate) fn arm(
            &self,
            fd: RawFd,
            token: u64,
            interest: c_int,
            _added: bool,
        ) -> io::Result<()> {
            let flags = libc::EV_ADD | libc::EV_ONESHOT;
            let mut changes = Vec::with_capacity(2);
            if interest & HEW_IO_READ != 0 {
                changes.push(change(fd as usize, libc::EVFILT_READ, flags, 0, token));
            }
            if interest & HEW_IO_WRITE != 0 {
                changes.push(change(fd as usize, libc::EVFILT_WRITE, flags, 0, token));
            }
            self.apply(&changes)
        }

        /// Remove both filters before the slot closes `fd`.
        pub(crate) fn remove(&self, fd: RawFd) {
            // Each deletion may fail with ENOENT when that filter already
            // fired; apply them one at a time so one failure skips nothing.
            for filter in [libc::EVFILT_READ, libc::EVFILT_WRITE] {
                let _ = self.apply(&[change(fd as usize, filter, libc::EV_DELETE, 0, 0)]);
            }
        }

        /// Trigger the user event; it stays pending until the next wait.
        pub(crate) fn wake(&self) {
            let _ = self.apply(&[change(
                0,
                libc::EVFILT_USER,
                0,
                libc::NOTE_TRIGGER,
                WAKE_TOKEN,
            )]);
        }

        pub(crate) fn wait(&self, timeout_ms: c_int, out: &mut Vec<Event>) -> io::Result<()> {
            // SAFETY: kevent is plain data; all-zero is a valid value.
            let mut events: [libc::kevent; MAX_EVENTS] = unsafe { std::mem::zeroed() };
            let spec;
            let timeout = if timeout_ms < 0 {
                std::ptr::null()
            } else {
                spec = libc::timespec {
                    tv_sec: libc::time_t::from(timeout_ms / 1000),
                    tv_nsec: libc::c_long::from(timeout_ms % 1000) * 1_000_000,
                };
                &raw const spec
            };
            // SAFETY: the buffer holds MAX_EVENTS entries.
            let count = unsafe {
                libc::kevent(
                    self.kq,
                    std::ptr::null(),
                    0,
                    events.as_mut_ptr(),
                    MAX_EVENTS as c_int,
                    timeout,
                )
            };
            if count < 0 {
                return Err(io::Error::last_os_error());
            }
            for event in &events[..count as usize] {
                if event.filter == libc::EVFILT_USER {
                    continue;
                }
                let mut report = 0;
                if event.filter == libc::EVFILT_READ {
                    report |= HEW_IO_READ;
                }
                if event.filter == libc::EVFILT_WRITE {
                    report |= HEW_IO_WRITE;
                }
                if event.flags & libc::EV_EOF != 0 {
                    report |= HEW_IO_HUP;
                }
                if event.flags & libc::EV_ERROR != 0 {
                    report |= HEW_IO_ERROR;
                }
                out.push(Event {
                    token: event.udata as usize as u64,
                    events: report,
                });
            }
            Ok(())
        }
    }
}

// ---- Windows (IOCP + AFD_POLL) ----------------------------------------------
//
// IOCP reports completions; the reactor needs readiness. The `AFD_POLL`
// technique (mio, wepoll) supplies it: one `\Device\Afd` helper handle is bound
// to the completion port, and each arm issues a one-shot `IOCTL_AFD_POLL` on a
// socket whose completion reports which events fired. The poll buffers live in
// the socket's slot and the arm passes the slot's retained address as the
// completion context, so the kernel never writes into freed memory. A wake is a
// posted completion packet with its own key.

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
    use super::{Event, HEW_IO_ERROR, HEW_IO_HUP, HEW_IO_READ, HEW_IO_WRITE, MAX_EVENTS};
    use std::ffi::{c_int, c_void};
    use std::io;
    use std::ptr;

    type HANDLE = *mut c_void;
    type SOCKET = usize;
    type NTSTATUS = i32;

    const STATUS_SUCCESS: NTSTATUS = 0x0000_0000;
    const STATUS_PENDING: NTSTATUS = 0x0000_0103;
    const STATUS_CANCELLED: u32 = 0xC000_0120;

    const SYNCHRONIZE: u32 = 0x0010_0000;
    const FILE_OPEN: u32 = 0x0000_0001;
    const FILE_SHARE_READ: u32 = 0x0000_0001;
    const FILE_SHARE_WRITE: u32 = 0x0000_0002;
    const WAIT_TIMEOUT: u32 = 258;
    const INFINITE: u32 = 0xFFFF_FFFF;
    const IOCTL_AFD_POLL: u32 = 0x0001_2024;
    const SIO_BASE_HANDLE: u32 = 0x4800_0022;

    const AFD_POLL_RECEIVE: u32 = 0x0001;
    const AFD_POLL_RECEIVE_EXPEDITED: u32 = 0x0002;
    const AFD_POLL_SEND: u32 = 0x0004;
    const AFD_POLL_DISCONNECT: u32 = 0x0008;
    const AFD_POLL_ABORT: u32 = 0x0010;
    const AFD_POLL_LOCAL_CLOSE: u32 = 0x0020;
    const AFD_POLL_ACCEPT: u32 = 0x0080;
    const AFD_POLL_CONNECT_FAIL: u32 = 0x0100;

    /// Completion key of AFD poll completions.
    const AFD_KEY: usize = 0xAFD0;
    /// Completion key of a posted wake.
    const WAKE_KEY: usize = 0x3A4E;

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

    /// `IO_STATUS_BLOCK`: a pointer-sized status/pointer union and a count.
    #[repr(C)]
    #[derive(Clone, Copy)]
    struct IO_STATUS_BLOCK {
        status: isize,
        information: usize,
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
        fn PostQueuedCompletionStatus(
            CompletionPort: HANDLE,
            dwNumberOfBytesTransferred: u32,
            dwCompletionKey: usize,
            lpOverlapped: *mut c_void,
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

    /// Resolve a socket's AFD base handle, peeling layered providers.
    fn base_socket(socket: SOCKET) -> SOCKET {
        let mut base: SOCKET = 0;
        let mut bytes: u32 = 0;
        // SAFETY: socket is live; the output is a local SOCKET slot.
        let rc = unsafe {
            WSAIoctl(
                socket,
                SIO_BASE_HANDLE,
                ptr::null_mut(),
                0,
                ptr::addr_of_mut!(base).cast(),
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

    fn afd_interest(interest: c_int) -> u32 {
        let mut mask =
            AFD_POLL_ABORT | AFD_POLL_CONNECT_FAIL | AFD_POLL_DISCONNECT | AFD_POLL_LOCAL_CLOSE;
        if interest & HEW_IO_READ != 0 {
            mask |= AFD_POLL_RECEIVE | AFD_POLL_RECEIVE_EXPEDITED | AFD_POLL_ACCEPT;
        }
        if interest & HEW_IO_WRITE != 0 {
            mask |= AFD_POLL_SEND;
        }
        mask
    }

    fn afd_report(afd: u32) -> c_int {
        let mut report = 0;
        if afd & (AFD_POLL_RECEIVE | AFD_POLL_RECEIVE_EXPEDITED | AFD_POLL_ACCEPT) != 0 {
            report |= HEW_IO_READ;
        }
        if afd & AFD_POLL_SEND != 0 {
            report |= HEW_IO_WRITE;
        }
        if afd & (AFD_POLL_DISCONNECT | AFD_POLL_LOCAL_CLOSE) != 0 {
            report |= HEW_IO_HUP;
        }
        if afd & (AFD_POLL_ABORT | AFD_POLL_CONNECT_FAIL) != 0 {
            report |= HEW_IO_ERROR;
        }
        report
    }

    /// The buffers of one in-flight AFD poll. They live in the socket's slot;
    /// the kernel writes them until the completion is dequeued.
    pub(crate) struct AfdPoll {
        base: SOCKET,
        info: AFD_POLL_INFO,
        iosb: IO_STATUS_BLOCK,
    }

    impl AfdPoll {
        pub(crate) fn new(socket: SOCKET) -> Self {
            Self {
                base: base_socket(socket),
                // SAFETY: plain data; all-zero is valid.
                info: unsafe { std::mem::zeroed() },
                iosb: IO_STATUS_BLOCK {
                    status: 0,
                    information: 0,
                },
            }
        }

        /// The readiness a dequeued completion reported; nothing when the
        /// poll was cancelled.
        pub(crate) fn report(&self) -> c_int {
            if self.iosb.status as u32 == STATUS_CANCELLED || self.info.NumberOfHandles == 0 {
                0
            } else {
                afd_report(self.info.Handles[0].Events)
            }
        }
    }

    /// Completion port plus the AFD helper handle bound to it.
    pub(crate) struct Poller {
        iocp: HANDLE,
        afd: HANDLE,
    }

    // SAFETY: both handles are kernel objects safe to use from any thread.
    unsafe impl Send for Poller {}
    // SAFETY: see Send; every method is a thread-safe kernel call.
    unsafe impl Sync for Poller {}

    impl std::fmt::Debug for Poller {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Poller").finish_non_exhaustive()
        }
    }

    fn open_afd_helper() -> io::Result<HANDLE> {
        let name: Vec<u16> = r"\Device\Afd\Hew".encode_utf16().collect();
        let byte_len = (name.len() * 2) as u16;
        let mut unicode = UNICODE_STRING {
            Length: byte_len,
            MaximumLength: byte_len,
            Buffer: name.as_ptr().cast_mut(),
        };
        let mut attributes = OBJECT_ATTRIBUTES {
            Length: std::mem::size_of::<OBJECT_ATTRIBUTES>() as u32,
            RootDirectory: ptr::null_mut(),
            ObjectName: ptr::addr_of_mut!(unicode),
            Attributes: 0,
            SecurityDescriptor: ptr::null_mut(),
            SecurityQualityOfService: ptr::null_mut(),
        };
        let mut handle: HANDLE = ptr::null_mut();
        let mut iosb = IO_STATUS_BLOCK {
            status: 0,
            information: 0,
        };
        // SAFETY: every pointer names a live local for the call.
        let status = unsafe {
            NtCreateFile(
                ptr::addr_of_mut!(handle),
                SYNCHRONIZE,
                ptr::addr_of_mut!(attributes),
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
            Ok(handle)
        } else {
            Err(io::Error::other(format!(
                "open AFD helper: NTSTATUS {status:#x}"
            )))
        }
    }

    impl Poller {
        pub(crate) fn new() -> io::Result<Self> {
            // SAFETY: creating a fresh completion port.
            let iocp =
                unsafe { CreateIoCompletionPort((-1isize) as HANDLE, ptr::null_mut(), 0, 0) };
            if iocp.is_null() {
                return Err(io::Error::last_os_error());
            }
            let afd = match open_afd_helper() {
                Ok(afd) => afd,
                Err(error) => {
                    // SAFETY: the port was created above.
                    unsafe { CloseHandle(iocp) };
                    return Err(error);
                }
            };
            // SAFETY: both handles are live.
            if unsafe { CreateIoCompletionPort(afd, iocp, AFD_KEY, 0) } != iocp {
                let error = io::Error::last_os_error();
                // SAFETY: both handles were created above.
                unsafe {
                    CloseHandle(afd);
                    CloseHandle(iocp);
                }
                return Err(error);
            }
            Ok(Self { iocp, afd })
        }

        /// Issue a one-shot poll. `context` returns as the completion's token.
        ///
        /// # Safety
        /// `poll` stays live and unmoved until the completion is dequeued, and
        /// no other poll on it is in flight.
        pub(crate) unsafe fn arm(
            &self,
            poll: *mut AfdPoll,
            interest: c_int,
            context: usize,
        ) -> io::Result<()> {
            let poll = &mut *poll;
            poll.info.Timeout = i64::MAX;
            poll.info.NumberOfHandles = 1;
            poll.info.Exclusive = 0;
            poll.info.Handles[0].Handle = poll.base as HANDLE;
            poll.info.Handles[0].Status = 0;
            poll.info.Handles[0].Events = afd_interest(interest);
            poll.iosb.status = STATUS_PENDING as isize;
            let info = ptr::addr_of_mut!(poll.info).cast::<c_void>();
            let size = std::mem::size_of::<AFD_POLL_INFO>() as u32;
            let status = NtDeviceIoControlFile(
                self.afd,
                ptr::null_mut(),
                ptr::null_mut(),
                context as *mut c_void,
                ptr::addr_of_mut!(poll.iosb),
                IOCTL_AFD_POLL,
                info,
                size,
                info,
                size,
            );
            if status == STATUS_PENDING || status == STATUS_SUCCESS {
                Ok(())
            } else {
                Err(io::Error::other(format!(
                    "arm AFD poll: NTSTATUS {status:#x}"
                )))
            }
        }

        /// Cancel an in-flight poll; its completion still arrives.
        ///
        /// # Safety
        /// `poll` has an in-flight arm.
        pub(crate) unsafe fn cancel(&self, poll: *mut AfdPoll) {
            let mut cancel = IO_STATUS_BLOCK {
                status: 0,
                information: 0,
            };
            NtCancelIoFileEx(
                self.afd,
                ptr::addr_of_mut!((*poll).iosb),
                ptr::addr_of_mut!(cancel),
            );
        }

        pub(crate) fn wake(&self) {
            // SAFETY: posts a packet with no overlapped structure.
            unsafe { PostQueuedCompletionStatus(self.iocp, 0, WAKE_KEY, ptr::null_mut()) };
        }

        /// Wait for completions. Each AFD completion reports its arm context
        /// as the token with no events; the slot reads its own poll report.
        pub(crate) fn wait(&self, timeout_ms: c_int, out: &mut Vec<Event>) -> io::Result<()> {
            // SAFETY: plain data; all-zero is valid.
            let mut entries: [OVERLAPPED_ENTRY; MAX_EVENTS] = unsafe { std::mem::zeroed() };
            let mut removed: u32 = 0;
            let timeout = if timeout_ms < 0 {
                INFINITE
            } else {
                timeout_ms as u32
            };
            // SAFETY: the buffer holds MAX_EVENTS entries.
            let ok = unsafe {
                GetQueuedCompletionStatusEx(
                    self.iocp,
                    entries.as_mut_ptr(),
                    MAX_EVENTS as u32,
                    ptr::addr_of_mut!(removed),
                    timeout,
                    0,
                )
            };
            if ok == 0 {
                // SAFETY: no preconditions.
                if unsafe { GetLastError() } == WAIT_TIMEOUT {
                    return Ok(());
                }
                return Err(io::Error::last_os_error());
            }
            for entry in &entries[..removed as usize] {
                if entry.lpCompletionKey == AFD_KEY {
                    out.push(Event {
                        token: entry.lpOverlapped as usize as u64,
                        events: 0,
                    });
                }
            }
            Ok(())
        }
    }
}

// ---- Other native targets ----------------------------------------------------

#[cfg(not(any(
    target_os = "linux",
    target_os = "freebsd",
    target_os = "macos",
    windows
)))]
mod platform {
    use super::Event;
    use std::ffi::c_int;
    use std::io;

    /// No readiness source on this target: the reactor refuses to start.
    #[derive(Debug)]
    pub(crate) struct Poller;

    impl Poller {
        pub(crate) fn new() -> io::Result<Self> {
            Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "no readiness poller on this target",
            ))
        }

        pub(crate) fn arm(
            &self,
            _fd: c_int,
            _token: u64,
            _interest: c_int,
            _added: bool,
        ) -> io::Result<()> {
            Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "no readiness poller on this target",
            ))
        }

        pub(crate) fn remove(&self, _fd: c_int) {}

        pub(crate) fn wake(&self) {}

        pub(crate) fn wait(&self, _timeout_ms: c_int, _out: &mut Vec<Event>) -> io::Result<()> {
            Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "no readiness poller on this target",
            ))
        }
    }
}

#[cfg(windows)]
pub(crate) use platform::AfdPoll;
pub(crate) use platform::Poller;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_string::ManagedString;
    use hew_cabi::string::{string_as_str, string_release};

    // -- File I/O -----------------------------------------------------------

    /// Helper: read a managed string, assert non-null, free it.
    ///
    /// # Safety
    ///
    /// `ptr` must be a live managed string, including canonical null/empty.
    unsafe fn read_and_free(ptr: *mut HewString) -> String {
        // SAFETY: caller guarantees ptr is a live managed string.
        let s = unsafe { string_as_str(ptr) }.to_owned();
        // SAFETY: ptr was allocated by hew_read_file.
        unsafe { string_release(ptr) };
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
        let path = ManagedString::new("/tmp/hew_test_nonexistent_file_XXXXXX");
        // SAFETY: path is a live managed string.
        let result = unsafe { hew_read_file(path.as_ptr()) };
        assert!(result.is_null());
    }

    #[test]
    fn read_file_valid_content_roundtrip() {
        let tmp = std::env::temp_dir().join(std::format!("hew_iotime_read_{}", std::process::id()));
        std::fs::write(&tmp, "hello from hew").unwrap();

        let path = ManagedString::new(tmp.to_str().unwrap());
        // SAFETY: path is a live managed owner borrowed by the call.
        let text = unsafe { read_and_free(hew_read_file(path.as_ptr())) };
        assert_eq!(text, "hello from hew");

        let _ = std::fs::remove_file(&tmp);
    }

    #[test]
    fn read_file_empty_returns_empty_string() {
        let tmp =
            std::env::temp_dir().join(std::format!("hew_iotime_empty_{}", std::process::id()));
        std::fs::write(&tmp, "").unwrap();

        let path = ManagedString::new(tmp.to_str().unwrap());
        // SAFETY: path is a live managed string.
        let text = unsafe { read_and_free(hew_read_file(path.as_ptr())) };
        assert_eq!(text, "");

        let _ = std::fs::remove_file(&tmp);
    }

    #[test]
    fn read_file_nul_path_returns_null() {
        let c_path = ManagedString::new("path\0suffix");
        // SAFETY: the path is a live managed string.
        let result = unsafe { hew_read_file(c_path.as_ptr()) };
        assert!(result.is_null());
    }

    #[test]
    fn read_file_embedded_nul_preserves_content() {
        let tmp = std::env::temp_dir().join(std::format!("hew_iotime_nul_{}", std::process::id()));
        std::fs::write(&tmp, "abc\0def").unwrap();

        let path = ManagedString::new(tmp.to_str().unwrap());
        // SAFETY: path is a live managed value.
        let ptr = unsafe { hew_read_file(path.as_ptr()) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a live managed result.
        let seen = unsafe { string_as_str(ptr) };
        assert_eq!(seen, "abc\0def");
        assert_eq!(seen.len(), 7);
        // SAFETY: ptr was allocated by hew_read_file.
        unsafe { string_release(ptr) };

        let _ = std::fs::remove_file(&tmp);
    }

    // -- Poller ----------------------------------------------------------------

    #[cfg(any(target_os = "linux", target_os = "freebsd", target_os = "macos"))]
    mod poller {
        use super::super::{Event, Poller, HEW_IO_READ, HEW_IO_WRITE};
        use std::io::Write;
        use std::net::{TcpListener, TcpStream};
        use std::os::fd::AsRawFd;
        use std::time::{Duration, Instant};

        fn pair() -> (TcpStream, TcpStream) {
            let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
            let client = TcpStream::connect(listener.local_addr().unwrap()).expect("connect");
            let (server, _) = listener.accept().expect("accept");
            server.set_nonblocking(true).unwrap();
            (server, client)
        }

        fn wait(poller: &Poller, timeout_ms: i32) -> Vec<Event> {
            let mut events = Vec::new();
            poller.wait(timeout_ms, &mut events).expect("wait");
            events
        }

        #[test]
        fn one_arm_reports_one_readiness() {
            let poller = Poller::new().expect("poller");
            let (server, mut client) = pair();
            poller
                .arm(server.as_raw_fd(), 7, HEW_IO_READ, false)
                .unwrap();
            client.write_all(b"x").unwrap();
            let events = wait(&poller, 1000);
            assert_eq!(events.len(), 1);
            assert_eq!(events[0].token, 7);
            assert_ne!(events[0].events & HEW_IO_READ, 0);
            // The byte is still unread, but the arm was spent.
            assert!(wait(&poller, 50).is_empty(), "a spent arm must stay silent");
            // Re-arming reports the still-pending byte again.
            poller
                .arm(server.as_raw_fd(), 7, HEW_IO_READ, true)
                .unwrap();
            assert_eq!(wait(&poller, 1000).len(), 1);
            poller.remove(server.as_raw_fd());
        }

        #[test]
        fn write_interest_reports_writable_socket() {
            let poller = Poller::new().expect("poller");
            let (server, _client) = pair();
            poller
                .arm(server.as_raw_fd(), 9, HEW_IO_WRITE, false)
                .unwrap();
            let events = wait(&poller, 1000);
            assert_eq!(events.len(), 1);
            assert_ne!(events[0].events & HEW_IO_WRITE, 0);
            poller.remove(server.as_raw_fd());
        }

        #[test]
        fn wake_interrupts_an_unbounded_wait_and_is_sticky() {
            let poller = std::sync::Arc::new(Poller::new().expect("poller"));
            // A wake sent before the wait is not lost.
            poller.wake();
            let started = Instant::now();
            assert!(wait(&poller, 5000).is_empty());
            assert!(started.elapsed() < Duration::from_secs(1));
            let waker = std::sync::Arc::clone(&poller);
            let thread = std::thread::spawn(move || {
                std::thread::sleep(Duration::from_millis(20));
                waker.wake();
            });
            let started = Instant::now();
            assert!(wait(&poller, -1).is_empty());
            assert!(started.elapsed() < Duration::from_secs(5));
            thread.join().unwrap();
        }
    }
}
