//! Unix fault handling: alternate stacks, classification, one record, and death
//! by the original signal.
//!
//! The handler runs on a per-thread alternate stack, so it also runs when the
//! thread's own stack is exhausted. It builds the record in static storage,
//! writes it with one `write(2)`, restores the default disposition and
//! re-raises. The process therefore dies by the real signal: the parent sees
//! `WIFSIGNALED`, and the kernel core (Linux, FreeBSD) or `ReportCrash` report
//! (macOS) is produced exactly as without the runtime. `ulimit -c` decides
//! whether a core is written.

use std::cell::{RefCell, UnsafeCell};
use std::mem::MaybeUninit;
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Once;

use super::frames;
use super::record::{FaultKind, FaultRecord};
use super::stack::{self, ThreadLabel};

const ALT_STACK_BYTES: usize = 64 * 1024;
const RECORD_BYTES: usize = 8 * 1024;

const FAULT_SIGNALS: [libc::c_int; 5] = [
    libc::SIGSEGV,
    libc::SIGBUS,
    libc::SIGFPE,
    libc::SIGILL,
    libc::SIGTRAP,
];

/// Storage only the thread that won [`REPORTING`] touches.
struct ReportSlot {
    record: UnsafeCell<FaultRecord>,
    text: UnsafeCell<[u8; RECORD_BYTES]>,
}

// SAFETY: access is serialized by REPORTING; exactly one handler invocation
// ever owns the slot, and the process dies afterwards.
unsafe impl Sync for ReportSlot {}

static SLOT: ReportSlot = ReportSlot {
    record: UnsafeCell::new(FaultRecord::EMPTY),
    text: UnsafeCell::new([0; RECORD_BYTES]),
};
static REPORTING: AtomicBool = AtomicBool::new(false);

#[derive(Debug)]
struct AlternateStack(Box<[u8]>);

fn registered_alt_stack() -> libc::stack_t {
    let mut stack = MaybeUninit::<libc::stack_t>::uninit();
    // SAFETY: a null first argument queries this thread's registration.
    if unsafe { libc::sigaltstack(ptr::null(), stack.as_mut_ptr()) } != 0 {
        std::process::abort();
    }
    // SAFETY: the successful query initialized the complete stack_t.
    unsafe { stack.assume_init() }
}

impl Drop for AlternateStack {
    fn drop(&mut self) {
        let registered = registered_alt_stack();
        if registered.ss_flags & libc::SS_DISABLE != 0
            || registered.ss_sp != self.0.as_mut_ptr().cast()
        {
            return;
        }
        let disabled = libc::stack_t {
            ss_sp: ptr::null_mut(),
            ss_flags: libc::SS_DISABLE,
            ss_size: self.0.len(),
        };
        // SAFETY: this thread installed and still owns the registered memory.
        unsafe {
            if libc::sigaltstack(&raw const disabled, ptr::null_mut()) != 0 {
                std::process::abort();
            }
        }
    }
}

thread_local! {
    /// Owns the memory registered with `sigaltstack` for this thread's
    /// lifetime. The handler never touches this Rust container.
    static ALT_STACK: RefCell<Option<AlternateStack>> = const { RefCell::new(None) };
}

/// Give the calling thread an alternate signal stack of at least
/// [`ALT_STACK_BYTES`], replacing a smaller one: Rust's own per-thread stack
/// is sized for its overflow message, not for walking the unwind tables.
fn ensure_alt_stack() {
    ALT_STACK.with(|slot| {
        if slot.borrow().is_some() {
            return;
        }
        let registered = registered_alt_stack();
        if registered.ss_flags & libc::SS_DISABLE == 0 && registered.ss_size >= ALT_STACK_BYTES {
            return;
        }
        let mut memory = vec![0_u8; ALT_STACK_BYTES].into_boxed_slice();
        let stack = libc::stack_t {
            ss_sp: memory.as_mut_ptr().cast(),
            ss_flags: 0,
            ss_size: memory.len(),
        };
        // SAFETY: memory remains owned by ALT_STACK until thread teardown.
        if unsafe { libc::sigaltstack(&raw const stack, ptr::null_mut()) } != 0 {
            std::process::abort();
        }
        *slot.borrow_mut() = Some(AlternateStack(memory));
    });
}

/// Prepare the calling thread to report its own faults.
pub(crate) fn init_thread(label: ThreadLabel) {
    stack::register_current_thread(label);
    ensure_alt_stack();
}

const fn signal_name(signal: libc::c_int) -> &'static str {
    match signal {
        libc::SIGSEGV => "SIGSEGV",
        libc::SIGBUS => "SIGBUS",
        libc::SIGFPE => "SIGFPE",
        libc::SIGILL => "SIGILL",
        libc::SIGTRAP => "SIGTRAP",
        _ => "signal",
    }
}

/// Whether `kill`, `raise` or `pthread_kill` sent the signal, rather than a
/// faulting instruction. Such a signal carries no fault address.
fn sent_by_process(info: &libc::siginfo_t) -> bool {
    #[cfg(any(target_os = "linux", target_os = "android"))]
    {
        info.si_code <= 0
    }
    #[cfg(not(any(target_os = "linux", target_os = "android")))]
    {
        // Darwin and FreeBSD number SI_USER, SI_QUEUE, SI_LWP and the other
        // non-fault origins from 0x10001. Darwin delivers a kill-sent fault
        // signal with a fault's si_code and no sender pid, so there it reads
        // as a fault.
        info.si_code & 0x10000 != 0
    }
}

/// Program counter of the faulting context.
///
/// # Safety
///
/// `context` must be the `ucontext_t` the kernel passed to an `SA_SIGINFO`
/// handler.
#[allow(
    clippy::unnecessary_cast,
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    reason = "register words are address-sized on every 64-bit target this maps"
)]
unsafe fn fault_pc(context: *mut libc::c_void) -> Option<usize> {
    if context.is_null() {
        return None;
    }
    let uc = context.cast::<libc::ucontext_t>();
    // SAFETY: the kernel-provided context is valid for the handler's duration.
    unsafe {
        #[cfg(all(
            any(target_os = "linux", target_os = "android"),
            target_arch = "x86_64"
        ))]
        {
            return Some((*uc).uc_mcontext.gregs[libc::REG_RIP as usize] as usize);
        }
        #[cfg(all(
            any(target_os = "linux", target_os = "android"),
            target_arch = "aarch64"
        ))]
        {
            return Some((*uc).uc_mcontext.pc as usize);
        }
        #[cfg(all(target_vendor = "apple", target_arch = "x86_64"))]
        {
            let mc = (*uc).uc_mcontext;
            if mc.is_null() {
                return None;
            }
            return Some((*mc).__ss.__rip as usize);
        }
        #[cfg(all(target_vendor = "apple", target_arch = "aarch64"))]
        {
            let mc = (*uc).uc_mcontext;
            if mc.is_null() {
                return None;
            }
            return Some((*mc).__ss.__pc as usize);
        }
        #[cfg(all(target_os = "freebsd", target_arch = "x86_64"))]
        {
            return Some((*uc).uc_mcontext.mc_rip as usize);
        }
        #[cfg(all(target_os = "freebsd", target_arch = "aarch64"))]
        {
            return Some((*uc).uc_mcontext.mc_gpregs.gp_elr as usize);
        }
        #[allow(unreachable_code, reason = "targets without a register map")]
        {
            let _ = uc;
            None
        }
    }
}

/// Fill the static record for a fault on the calling thread.
///
/// # Safety
///
/// The caller owns [`SLOT`] (it won [`REPORTING`]); `info` and `context` are
/// the handler's arguments.
unsafe fn fill_record(
    record: &mut FaultRecord,
    signal: libc::c_int,
    info: *const libc::siginfo_t,
    context: *mut libc::c_void,
) {
    let bounds = stack::current();
    // SAFETY: the kernel passes a valid siginfo to SA_SIGINFO handlers.
    let (sent, address) = match unsafe { info.as_ref() } {
        Some(info) if sent_by_process(info) => (true, 0),
        // SAFETY: si_addr is defined for the fault signals this handles.
        Some(info) => (false, unsafe { info.si_addr() } as usize),
        None => (false, 0),
    };
    record.kind = if !sent && bounds.in_guard(address) {
        FaultKind::StackOverflow
    } else {
        FaultKind::Signal
    };
    record.signal = signal.unsigned_abs();
    record.signal_name = signal_name(signal);
    record.sent = sent;
    record.address = address;
    record.thread = stack::current_label();
    record.load_image_range();
    record.frames.clear();
    // SAFETY: forwarded from the handler.
    if let Some(pc) = unsafe { fault_pc(context) } {
        if walk_is_safe(record.kind) {
            frames::capture(pc, bounds, &mut record.frames);
        } else {
            record.frames.push(pc);
        }
    }
}

/// Whether the platform unwinder can step out of this fault's frame.
///
/// `FreeBSD`'s unwinder looks up the faulting frame's CFI at `pc - 1`; in a
/// prologue that has just moved the stack pointer into the guard, that row
/// sends it to read the guard page, and the nested fault ends the process
/// without a record. An overflow there reports only the faulting pc.
const fn walk_is_safe(kind: FaultKind) -> bool {
    !(cfg!(target_os = "freebsd") && matches!(kind, FaultKind::StackOverflow))
}

unsafe extern "C" fn fatal_signal_handler(
    signal: libc::c_int,
    info: *mut libc::siginfo_t,
    context: *mut libc::c_void,
) {
    if REPORTING.swap(true, Ordering::AcqRel) {
        // Another thread is reporting and will end the process; the five fault
        // signals stay blocked here, so this thread just waits for that.
        loop {
            // SAFETY: pause has no preconditions.
            unsafe { libc::pause() };
        }
    }
    // SAFETY: this invocation owns SLOT from here on; every operation below is
    // async-signal-safe (stores to static memory, write, sigaction, raise).
    unsafe {
        let record = &mut *SLOT.record.get();
        fill_record(record, signal, info, context);
        let text = record.format_into(&mut *SLOT.text.get());
        crate::trap_code::write_stderr(text);
        let mut action = MaybeUninit::<libc::sigaction>::zeroed().assume_init();
        action.sa_sigaction = libc::SIG_DFL;
        libc::sigemptyset(&raw mut action.sa_mask);
        libc::sigaction(signal, &raw const action, ptr::null_mut());
        // The signal is blocked while this handler runs, so it is delivered
        // with the default disposition once the faulting context is restored:
        // the core and crash report show the faulting instruction. A sent
        // signal would not recur on its own, so the raise covers it too.
        libc::raise(signal);
    }
}

/// Find the load range of the image this runtime is linked into. Called at
/// installation, never from the handler.
fn record_image_range() {
    let anchor = fatal_signal_handler as *const () as usize;
    let mut info = MaybeUninit::<libc::Dl_info>::zeroed();
    // SAFETY: dladdr fills `info` for an address inside a loaded image.
    if unsafe { libc::dladdr(anchor as *const libc::c_void, info.as_mut_ptr()) } == 0 {
        return;
    }
    // SAFETY: dladdr returned success, so the structure is initialized.
    let base = unsafe { info.assume_init() }.dli_fbase as usize;
    // SAFETY: dli_fbase is the image's mapped header.
    let end = unsafe { image::end(base, anchor) };
    super::record::set_image_range(base, end.unwrap_or(usize::MAX));
}

#[cfg(any(target_os = "linux", target_os = "android", target_os = "freebsd"))]
mod image {
    /// `PT_LOAD`, which `libc` does not export for every ELF target.
    const PT_LOAD: u32 = 1;

    /// The end of the loaded ELF object that contains `anchor`.
    #[allow(
        clippy::cast_possible_truncation,
        reason = "ELF addresses fit the pointer width of the process that mapped them"
    )]
    pub(super) unsafe fn end(_base: usize, anchor: usize) -> Option<usize> {
        struct Search {
            anchor: usize,
            end: Option<usize>,
        }
        unsafe extern "C" fn visit(
            info: *mut libc::dl_phdr_info,
            _size: libc::size_t,
            data: *mut libc::c_void,
        ) -> libc::c_int {
            // SAFETY: dl_iterate_phdr passes a valid info record and our data.
            let (info, search) = unsafe { (&*info, &mut *data.cast::<Search>()) };
            let bias = info.dlpi_addr as usize;
            let mut low = usize::MAX;
            let mut high = 0;
            for index in 0..usize::from(info.dlpi_phnum) {
                // SAFETY: dlpi_phdr has dlpi_phnum entries.
                let header = unsafe { &*info.dlpi_phdr.add(index) };
                if header.p_type == PT_LOAD {
                    let start = bias + header.p_vaddr as usize;
                    low = low.min(start);
                    high = high.max(start + header.p_memsz as usize);
                }
            }
            if low <= search.anchor && search.anchor < high {
                search.end = Some(high);
                return 1;
            }
            0
        }
        let mut search = Search { anchor, end: None };
        // SAFETY: the callback only reads the records it is given.
        unsafe { libc::dl_iterate_phdr(Some(visit), (&raw mut search).cast()) };
        search.end
    }
}

#[cfg(target_vendor = "apple")]
mod image {
    /// The end of the Mach-O image whose header is at `base`: its highest
    /// segment, relative to `__TEXT`.
    #[allow(deprecated, reason = "libc's Mach-O header types match the loaded ABI")]
    pub(super) unsafe fn end(base: usize, _anchor: usize) -> Option<usize> {
        const LC_SEGMENT_64: u32 = 0x19;
        // SAFETY: `base` is a mapped mach_header_64 from dladdr.
        unsafe {
            let header = &*(base as *const libc::mach_header_64);
            let mut command = base + size_of::<libc::mach_header_64>();
            let mut text = None;
            let mut high = 0_u64;
            for _ in 0..header.ncmds {
                let load = &*(command as *const libc::load_command);
                if load.cmd == LC_SEGMENT_64 {
                    let segment = &*(command as *const libc::segment_command_64);
                    if segment.filesize != 0 || segment.vmsize != 0 {
                        if segment.fileoff == 0 && segment.filesize != 0 {
                            text = Some(segment.vmaddr);
                        }
                        high = high.max(segment.vmaddr + segment.vmsize);
                    }
                }
                command += load.cmdsize as usize;
            }
            Some(base + usize::try_from(high.checked_sub(text?)?).ok()?)
        }
    }
}

#[cfg(not(any(
    target_os = "linux",
    target_os = "android",
    target_os = "freebsd",
    target_vendor = "apple"
)))]
mod image {
    pub(super) unsafe fn end(_base: usize, _anchor: usize) -> Option<usize> {
        None
    }
}

pub(crate) fn init_crash_handling() {
    static INSTALL: Once = Once::new();
    let mut installed_here = false;
    INSTALL.call_once(|| {
        installed_here = true;
        record_image_range();
        // Bind the unwinder's lazy symbols and caches now; resolving them in
        // the handler would cost stack and take the loader's locks.
        frames::warm_up();
        for signal in FAULT_SIGNALS {
            // SAFETY: zero is a valid initial representation; all required
            // sigaction fields are initialized below.
            let mut action = unsafe { MaybeUninit::<libc::sigaction>::zeroed().assume_init() };
            action.sa_sigaction = fatal_signal_handler as *const () as usize;
            action.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK;
            // SAFETY: action is initialized; every fault signal is blocked
            // while the handler runs, so a nested fault kills the process
            // instead of re-entering a handler that waits on REPORTING.
            unsafe {
                libc::sigemptyset(&raw mut action.sa_mask);
                for blocked in FAULT_SIGNALS {
                    libc::sigaddset(&raw mut action.sa_mask, blocked);
                }
                if libc::sigaction(signal, &raw const action, ptr::null_mut()) != 0 {
                    std::process::abort();
                }
            }
        }
    });
    // The process entry thread installs the handlers: it runs `main` and
    // synchronous Hew code and needs the same alternate stack and bounds as a
    // worker. Any later thread that enters the runtime this way registers once.
    if installed_here {
        init_thread(ThreadLabel::Main);
    } else if !stack::is_registered() {
        init_thread(ThreadLabel::Other);
    }
}

pub(crate) fn init_worker_recovery(worker_id: u32) {
    let label = if worker_id == u32::MAX {
        ThreadLabel::Other
    } else {
        ThreadLabel::Worker(worker_id)
    };
    init_thread(label);
}

pub(crate) fn ignore_sigpipe() {
    // SAFETY: setting SIGPIPE to SIG_IGN is process-wide and idempotent.
    unsafe {
        libc::signal(libc::SIGPIPE, libc::SIG_IGN);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(any(target_os = "linux", target_os = "android"))]
    use crate::signal::stack::StackBounds;

    #[cfg(all(
        any(target_os = "linux", target_os = "android"),
        target_arch = "x86_64"
    ))]
    fn forged_context(pc: usize) -> libc::ucontext_t {
        // SAFETY: ucontext_t is plain data; zero is a valid representation.
        let mut uc: libc::ucontext_t = unsafe { std::mem::zeroed() };
        uc.uc_mcontext.gregs[libc::REG_RIP as usize] = i64::try_from(pc).unwrap();
        uc
    }

    #[cfg(all(
        any(target_os = "linux", target_os = "android"),
        target_arch = "aarch64"
    ))]
    fn forged_context(pc: usize) -> libc::ucontext_t {
        // SAFETY: ucontext_t is plain data; zero is a valid representation.
        let mut uc: libc::ucontext_t = unsafe { std::mem::zeroed() };
        uc.uc_mcontext.pc = pc as u64;
        uc
    }

    /// A forged fault at `address`, outside any signal frame.
    #[cfg(any(target_os = "linux", target_os = "android"))]
    fn classify_forged(address: usize, bounds: StackBounds) -> FaultRecord {
        // SAFETY: siginfo_t is plain data; the fields read are set below.
        let mut info: libc::siginfo_t = unsafe { std::mem::zeroed() };
        info.si_signo = libc::SIGSEGV;
        info.si_code = 1; // SEGV_MAPERR
                          // SAFETY: on LP64 Linux si_addr is the first word of the union, which
                          // starts at the second word, after the three leading ints and padding;
                          // the assertion checks the forged value against libc's accessor.
        unsafe {
            let base = (&raw mut info).cast::<u8>();
            base.add(2 * size_of::<usize>())
                .cast::<usize>()
                .write_unaligned(address);
            assert_eq!(info.si_addr() as usize, address);
        }
        let mut uc = forged_context(0xDEAD);
        let previous = stack::set_current_for_test(bounds);
        let mut record = FaultRecord::EMPTY;
        // SAFETY: the record is local; info and context are forged but valid.
        unsafe {
            fill_record(
                &mut record,
                libc::SIGSEGV,
                &raw const info,
                (&raw mut uc).cast(),
            );
        }
        let _ = stack::set_current_for_test(previous);
        record
    }

    #[test]
    #[cfg(any(target_os = "linux", target_os = "android"))]
    fn a_fault_below_the_stack_classifies_as_overflow() {
        let local = 0_usize;
        let here = (&raw const local) as usize;
        let bounds = StackBounds {
            low: here - 0x1000,
            high: here + 0x1000,
            guard: 4096,
        };
        let overflow = classify_forged(bounds.low - 16, bounds);
        assert_eq!(overflow.kind, FaultKind::StackOverflow);
        assert_eq!(overflow.address, bounds.low - 16);
        // The walk starts at the faulting pc; this test's own frames follow.
        assert_eq!(
            overflow.frames.runs().first().map(|run| run.pc),
            Some(0xDEAD)
        );

        let wild = classify_forged(8, bounds);
        assert_eq!(wild.kind, FaultKind::Signal);
    }

    #[test]
    #[cfg(any(target_os = "linux", target_os = "android"))]
    fn a_sent_signal_is_never_overflow() {
        // SAFETY: siginfo_t is plain data.
        let mut info: libc::siginfo_t = unsafe { std::mem::zeroed() };
        info.si_code = 0; // SI_USER
        assert!(sent_by_process(&info));
        info.si_code = 1;
        assert!(!sent_by_process(&info));
    }

    #[test]
    fn the_image_range_covers_the_handler() {
        record_image_range();
        let mut record = FaultRecord::EMPTY;
        record.load_image_range();
        let handler = fatal_signal_handler as *const () as usize;
        assert!(record.image_base != 0 && record.image_base <= handler);
        assert!(handler < record.image_end, "{record:x?}");
        // libc lives in another image (statically linked targets aside).
        assert!(record.image_end != usize::MAX);
    }
}
