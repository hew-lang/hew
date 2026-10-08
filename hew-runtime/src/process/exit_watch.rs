//! A descriptor that becomes readable when one child process exits.
//!
//! `Child.wait` parks on it through the I/O reactor like a pipe read, so a
//! waiting task holds no thread. Linux uses a pidfd; FreeBSD and macOS use a
//! private kqueue holding one `EVFILT_PROC`/`NOTE_EXIT` filter, itself
//! readable in the reactor's kqueue. Each wait opens its own watch and closes
//! it when the wait ends or is cancelled.
//!
//! The watch names the child by process ID only while it is opened, under the
//! child's lock with the child still unreaped, so the ID cannot have been
//! reused. Afterwards it asks only its own descriptor.

use std::io;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd, RawFd};

/// One child's exit, observable without waiting.
#[derive(Debug)]
pub(crate) struct ExitWatch {
    fd: OwnedFd,
    /// The exit was already seen: the kqueue reports it once.
    #[cfg(any(target_os = "freebsd", target_os = "macos"))]
    exited: std::sync::atomic::AtomicBool,
}

impl ExitWatch {
    /// Watch the unreaped child `pid`.
    #[cfg(target_os = "linux")]
    pub(crate) fn open(pid: u32) -> io::Result<Self> {
        let pid =
            libc::pid_t::try_from(pid).map_err(|_| io::Error::from_raw_os_error(libc::EINVAL))?;
        // SAFETY: pidfd_open takes a process ID and flags; the kernel sets
        // close-on-exec on the descriptor it returns.
        let fd = unsafe { libc::syscall(libc::SYS_pidfd_open, pid, 0) };
        if fd < 0 {
            return Err(io::Error::last_os_error());
        }
        let fd = RawFd::try_from(fd).map_err(|_| io::Error::from_raw_os_error(libc::EBADF))?;
        // SAFETY: the descriptor was just returned to this call alone.
        let fd = unsafe { OwnedFd::from_raw_fd(fd) };
        Ok(Self { fd })
    }

    /// Watch the unreaped child `pid`.
    #[cfg(any(target_os = "freebsd", target_os = "macos"))]
    pub(crate) fn open(pid: u32) -> io::Result<Self> {
        // SAFETY: no arguments.
        let kq = unsafe { libc::kqueue() };
        if kq < 0 {
            return Err(io::Error::last_os_error());
        }
        // SAFETY: the descriptor was just returned to this call alone.
        let fd = unsafe { OwnedFd::from_raw_fd(kq) };
        // SAFETY: fcntl on the live descriptor above.
        unsafe { libc::fcntl(fd.as_raw_fd(), libc::F_SETFD, libc::FD_CLOEXEC) };
        // SAFETY: kevent is plain data; all-zero is a valid value.
        let mut change: libc::kevent = unsafe { std::mem::zeroed() };
        change.ident = pid as libc::uintptr_t;
        change.filter = libc::EVFILT_PROC;
        change.flags = libc::EV_ADD;
        change.fflags = libc::NOTE_EXIT;
        // SAFETY: one change, no event list, on the live kqueue.
        let rc = unsafe {
            libc::kevent(
                fd.as_raw_fd(),
                &raw const change,
                1,
                std::ptr::null_mut(),
                0,
                std::ptr::null(),
            )
        };
        let mut exited = false;
        if rc < 0 {
            let error = io::Error::last_os_error();
            // macOS refuses to attach to a child that already exited and
            // awaits its reap; the child is unreaped, so it is ours.
            if error.raw_os_error() != Some(libc::ESRCH) {
                return Err(error);
            }
            exited = true;
        }
        Ok(Self {
            fd,
            exited: std::sync::atomic::AtomicBool::new(exited),
        })
    }

    /// Watch the unreaped child `pid`: no exit report on this target.
    #[cfg(not(any(target_os = "linux", target_os = "freebsd", target_os = "macos")))]
    pub(crate) fn open(_pid: u32) -> io::Result<Self> {
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "no child exit report on this target",
        ))
    }

    /// The descriptor the reactor arms for read readiness.
    pub(crate) fn fd(&self) -> RawFd {
        self.fd.as_raw_fd()
    }

    /// Whether the child has exited. Never waits.
    #[cfg(target_os = "linux")]
    pub(crate) fn exited(&self) -> bool {
        let mut poll = libc::pollfd {
            fd: self.fd.as_raw_fd(),
            events: libc::POLLIN,
            revents: 0,
        };
        // SAFETY: one live descriptor in a local array, no wait.
        unsafe { libc::poll(&raw mut poll, 1, 0) != 0 }
    }

    /// Whether the child has exited. Never waits.
    #[cfg(any(target_os = "freebsd", target_os = "macos"))]
    pub(crate) fn exited(&self) -> bool {
        use std::sync::atomic::Ordering;
        if self.exited.load(Ordering::SeqCst) {
            return true;
        }
        // SAFETY: kevent is plain data; all-zero is a valid value.
        let mut event: libc::kevent = unsafe { std::mem::zeroed() };
        let zero = libc::timespec {
            tv_sec: 0,
            tv_nsec: 0,
        };
        // SAFETY: room for one event; a zero timeout never waits.
        let count = unsafe {
            libc::kevent(
                self.fd.as_raw_fd(),
                std::ptr::null(),
                0,
                &raw mut event,
                1,
                &raw const zero,
            )
        };
        // An error leaves the waiter to try again on the next report.
        if count > 0 {
            self.exited.store(true, Ordering::SeqCst);
        }
        count > 0
    }

    /// Whether the child has exited: never on this target.
    #[cfg(not(any(target_os = "linux", target_os = "freebsd", target_os = "macos")))]
    pub(crate) fn exited(&self) -> bool {
        false
    }
}
