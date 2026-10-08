//! Termination signals: SIGTERM and SIGINT on Unix, console control events on
//! Windows.
//!
//! With no subscriber, a signal starts graceful runtime shutdown (spec §5.9):
//! the handler records it and the first parking worker initiates shutdown in
//! ordinary thread context. While a program holds an `os.shutdown_signal()`
//! stream, a signal instead delivers one `()` to every subscriber and the
//! program decides when to stop. A program that never started the actor
//! runtime and holds no subscription keeps the platform default and ends.
//!
//! On Unix the handler only writes one byte to a self-pipe; a dispatcher thread
//! reads it and feeds the subscribers' queues. Windows runs console control
//! handlers on their own thread, so delivery happens there directly. Windows
//! ends the process once the handler returns from `CTRL_CLOSE_EVENT`, so a
//! console close is delivered but the program cannot rely on acting on it.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, Once};

use crate::channel_core::ChannelCore;
use crate::util::MutexExt;

/// Live subscriber queues, each the core of one `shutdown_signal()` stream.
static SUBSCRIBERS: Mutex<Vec<Arc<ChannelCore>>> = Mutex::new(Vec::new());
/// How many subscribers are registered. The signal handler reads only this.
static SUBSCRIBED: AtomicUsize = AtomicUsize::new(0);
/// The actor runtime has started and owns the default reaction.
static RUNTIME_HANDLES: AtomicBool = AtomicBool::new(false);
/// A signal arrived with no subscriber; a worker starts shutdown.
static SHUTDOWN_PENDING: AtomicBool = AtomicBool::new(false);
static INSTALL: Once = Once::new();

/// Install the handlers and make the actor runtime the default reaction: a
/// signal with no subscriber starts graceful shutdown. Called once the
/// scheduler starts.
pub fn install_runtime_handlers() {
    install();
    RUNTIME_HANDLES.store(true, Ordering::Release);
}

/// Called by a worker before it parks: start the shutdown a signal requested.
pub fn check_pending() {
    if SHUTDOWN_PENDING
        .compare_exchange(true, false, Ordering::AcqRel, Ordering::Relaxed)
        .is_ok()
    {
        crate::shutdown::hew_shutdown_initiate(0);
    }
}

/// A signal with no subscriber to receive it.
fn unclaimed(signal: c_int) {
    if RUNTIME_HANDLES.load(Ordering::Acquire) {
        SHUTDOWN_PENDING.store(true, Ordering::Release);
    } else {
        platform_default(signal);
    }
}

/// Hand one request to every subscriber. A subscriber whose queue already
/// holds an unread request keeps that one: requests coalesce until read.
fn deliver(signal: c_int) {
    let subscribers = SUBSCRIBERS.lock_or_recover();
    if subscribers.is_empty() {
        drop(subscribers);
        unclaimed(signal);
        return;
    }
    for core in subscribers.iter() {
        let _ = core.try_send_owned(Vec::new());
    }
}

/// Register a new subscriber queue, holding at most one unread request.
fn subscribe() -> Result<Arc<ChannelCore>, String> {
    install();
    #[cfg(unix)]
    unix::ensure_dispatcher()?;
    let core = Arc::new(ChannelCore::new(1));
    SUBSCRIBERS.lock_or_recover().push(Arc::clone(&core));
    SUBSCRIBED.fetch_add(1, Ordering::AcqRel);
    Ok(core)
}

/// Withdraw a subscriber; with none left, signals take the default reaction.
pub(crate) fn unsubscribe(core: &Arc<ChannelCore>) {
    let mut subscribers = SUBSCRIBERS.lock_or_recover();
    if let Some(index) = subscribers.iter().position(|live| Arc::ptr_eq(live, core)) {
        subscribers.swap_remove(index);
        SUBSCRIBED.fetch_sub(1, Ordering::AcqRel);
    }
}

/// Start receiving termination signals as a `Stream<()>`; the program owns the
/// shutdown decision while the stream lives. Null, with the stream error set,
/// when the dispatcher cannot start.
#[no_mangle]
pub extern "C" fn hew_shutdown_signal_stream() -> *mut crate::stream::HewStreamPair {
    match subscribe() {
        Ok(core) => crate::stream::shutdown_signal_stream(core),
        Err(message) => {
            crate::stream_error::set_last_error(message);
            std::ptr::null_mut()
        }
    }
}

#[cfg(unix)]
fn install() {
    INSTALL.call_once(|| {
        // SAFETY: the handler performs only async-signal-safe operations.
        unsafe { unix::install_handlers() };
    });
}

#[cfg(windows)]
fn install() {
    INSTALL.call_once(|| {
        // SAFETY: the handler runs on the console control thread and uses only
        // ordinary synchronization.
        unsafe { windows::install_handler() };
    });
}

#[cfg(unix)]
fn platform_default(signal: c_int) {
    // SAFETY: restoring the default action and re-raising are async-signal-safe.
    unsafe {
        libc::signal(signal, libc::SIG_DFL);
        libc::raise(signal);
    }
}

#[cfg(windows)]
fn platform_default(_signal: c_int) {
    // The control handler reports the event unhandled instead; see `windows`.
}

#[cfg(unix)]
mod unix {
    use std::ffi::c_int;
    use std::sync::atomic::{AtomicI32, Ordering};
    use std::sync::Mutex;

    use crate::util::MutexExt;

    /// The self-pipe's write end, or -1 before the dispatcher starts.
    static WAKE_FD: AtomicI32 = AtomicI32::new(-1);
    /// The signal the handler last forwarded through the pipe.
    static LAST_SIGNAL: AtomicI32 = AtomicI32::new(libc::SIGTERM);
    /// Serializes the dispatcher's one start.
    static DISPATCHER: Mutex<()> = Mutex::new(());

    pub(super) unsafe fn install_handlers() {
        let mut action: libc::sigaction = std::mem::zeroed();
        action.sa_sigaction =
            handler as extern "C" fn(c_int, *mut libc::siginfo_t, *mut std::ffi::c_void) as usize;
        action.sa_flags = libc::SA_SIGINFO | libc::SA_RESTART;
        libc::sigemptyset(&raw mut action.sa_mask);
        libc::sigaction(libc::SIGTERM, &raw const action, std::ptr::null_mut());
        libc::sigaction(libc::SIGINT, &raw const action, std::ptr::null_mut());
    }

    /// Only async-signal-safe work: atomics, `write`, `signal` and `raise`.
    extern "C" fn handler(signal: c_int, _info: *mut libc::siginfo_t, _ctx: *mut std::ffi::c_void) {
        let fd = WAKE_FD.load(Ordering::Acquire);
        if super::SUBSCRIBED.load(Ordering::Acquire) > 0 && fd >= 0 {
            LAST_SIGNAL.store(signal, Ordering::Release);
            let saved = errno::get();
            let byte = 1u8;
            // SAFETY: a nonblocking write of one byte from a live local. A
            // full pipe already holds an undelivered request.
            unsafe { libc::write(fd, (&raw const byte).cast(), 1) };
            errno::set(saved);
            return;
        }
        super::unclaimed(signal);
    }

    /// Create the self-pipe and the thread that turns its bytes into requests.
    /// A failed start is reported to this caller and retried by the next.
    pub(super) fn ensure_dispatcher() -> Result<(), String> {
        let _start = DISPATCHER.lock_or_recover();
        if WAKE_FD.load(Ordering::Acquire) >= 0 {
            return Ok(());
        }
        let mut fds = [-1; 2];
        // SAFETY: `fds` is a writable two-descriptor array.
        if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
            return Err(format!(
                "cannot create the signal pipe: {}",
                std::io::Error::last_os_error()
            ));
        }
        for fd in fds {
            // SAFETY: fcntl on descriptors this call just created.
            unsafe { libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC) };
        }
        // SAFETY: as above; the handler must never block on a full pipe.
        unsafe {
            let flags = libc::fcntl(fds[1], libc::F_GETFL);
            libc::fcntl(fds[1], libc::F_SETFL, flags | libc::O_NONBLOCK);
        }
        let read_fd = fds[0];
        let spawned = std::thread::Builder::new()
            .name("hew-signal".into())
            .spawn(move || dispatch(read_fd));
        if let Err(error) = spawned {
            for fd in fds {
                // SAFETY: closing the descriptors this call created.
                unsafe { libc::close(fd) };
            }
            return Err(format!("cannot start the signal dispatcher: {error}"));
        }
        WAKE_FD.store(fds[1], Ordering::Release);
        Ok(())
    }

    fn dispatch(read_fd: c_int) {
        let mut buffer = [0u8; 64];
        loop {
            // SAFETY: a blocking read into a live local buffer.
            let count = unsafe { libc::read(read_fd, buffer.as_mut_ptr().cast(), buffer.len()) };
            if count > 0 {
                super::deliver(LAST_SIGNAL.load(Ordering::Acquire));
            } else if count == 0
                || std::io::Error::last_os_error().kind() != std::io::ErrorKind::Interrupted
            {
                return;
            }
        }
    }

    /// The calling thread's `errno`, preserved across the handler's write.
    mod errno {
        #[cfg(any(target_os = "linux", target_os = "android"))]
        unsafe fn location() -> *mut libc::c_int {
            libc::__errno_location()
        }
        #[cfg(any(target_os = "macos", target_os = "ios", target_os = "freebsd"))]
        unsafe fn location() -> *mut libc::c_int {
            libc::__error()
        }
        #[cfg(any(target_os = "netbsd", target_os = "openbsd"))]
        unsafe fn location() -> *mut libc::c_int {
            libc::__errno()
        }

        pub(super) fn get() -> libc::c_int {
            // SAFETY: the thread's errno slot is always live.
            unsafe { *location() }
        }

        pub(super) fn set(value: libc::c_int) {
            // SAFETY: as above.
            unsafe { *location() = value };
        }
    }
}

#[cfg(windows)]
mod windows {
    use std::sync::atomic::Ordering;

    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn SetConsoleCtrlHandler(
            handler: Option<unsafe extern "system" fn(u32) -> i32>,
            add: i32,
        ) -> i32;
    }

    /// `CTRL_C_EVENT`, `CTRL_BREAK_EVENT` and `CTRL_CLOSE_EVENT`.
    const LAST_HANDLED_EVENT: u32 = 2;

    unsafe extern "system" fn handler(event: u32) -> i32 {
        if event > LAST_HANDLED_EVENT {
            return 0;
        }
        // The console thread is ordinary thread context.
        let signal = if event == 0 {
            libc::SIGINT
        } else {
            libc::SIGTERM
        };
        if super::SUBSCRIBED.load(Ordering::Acquire) > 0 {
            super::deliver(signal);
            return 1;
        }
        if super::RUNTIME_HANDLES.load(Ordering::Acquire) {
            super::SHUTDOWN_PENDING.store(true, Ordering::Release);
            return 1;
        }
        // Unhandled: the next handler, ultimately the default, ends the process.
        0
    }

    pub(super) unsafe fn install_handler() {
        SetConsoleCtrlHandler(Some(handler), 1);
    }
}
