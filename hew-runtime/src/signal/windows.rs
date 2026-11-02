//! Windows fault handling: a vectored handler that classifies, writes one
//! record and lets the exception continue to the system.
//!
//! The handler never terminates the process itself. Returning
//! `EXCEPTION_CONTINUE_SEARCH` leaves the exception unhandled, so Windows Error
//! Reporting writes its LocalDumps and the process exits with the exception
//! code (`0xC00000FD` for a stack overflow). `HEW_MINIDUMP=<dir>` additionally
//! writes a minidump from a helper thread started at initialization.

use std::cell::UnsafeCell;
use std::mem::MaybeUninit;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU32, Ordering};
use std::sync::{Once, OnceLock};

use windows_sys::Win32::Foundation::{
    CloseHandle, EXCEPTION_ACCESS_VIOLATION, EXCEPTION_ARRAY_BOUNDS_EXCEEDED, EXCEPTION_BREAKPOINT,
    EXCEPTION_DATATYPE_MISALIGNMENT, EXCEPTION_FLT_DENORMAL_OPERAND, EXCEPTION_FLT_DIVIDE_BY_ZERO,
    EXCEPTION_FLT_INEXACT_RESULT, EXCEPTION_FLT_INVALID_OPERATION, EXCEPTION_FLT_OVERFLOW,
    EXCEPTION_FLT_STACK_CHECK, EXCEPTION_FLT_UNDERFLOW, EXCEPTION_ILLEGAL_INSTRUCTION,
    EXCEPTION_INT_DIVIDE_BY_ZERO, EXCEPTION_INT_OVERFLOW, EXCEPTION_IN_PAGE_ERROR,
    EXCEPTION_PRIV_INSTRUCTION, EXCEPTION_STACK_OVERFLOW, GENERIC_WRITE, HANDLE,
    INVALID_HANDLE_VALUE, WAIT_OBJECT_0,
};
use windows_sys::Win32::Storage::FileSystem::{CreateFileW, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL};
use windows_sys::Win32::System::Diagnostics::Debug::{
    AddVectoredExceptionHandler, MiniDumpWithIndirectlyReferencedMemory, MiniDumpWithThreadInfo,
    MiniDumpWriteDump, EXCEPTION_CONTINUE_SEARCH, EXCEPTION_POINTERS,
    MINIDUMP_EXCEPTION_INFORMATION,
};
use windows_sys::Win32::System::Threading::{
    CreateEventW, GetCurrentProcess, GetCurrentProcessId, GetCurrentThreadId, SetEvent,
    SetThreadStackGuarantee, WaitForSingleObject, INFINITE,
};

use super::frames::Frames;
use super::record::{FaultKind, FaultRecord};
use super::stack::{self, StackBounds, ThreadLabel};

/// Stack kept back for the handler when a thread's stack overflows. The
/// handler formats into static storage, so this only has to hold its frames
/// and the system's exception dispatch.
const STACK_GUARANTEE_BYTES: u32 = 64 * 1024;
const RECORD_BYTES: usize = 8 * 1024;
/// How long the faulting thread waits for the minidump helper.
const MINIDUMP_WAIT_MS: u32 = 60_000;

struct ReportSlot {
    record: UnsafeCell<FaultRecord>,
    text: UnsafeCell<[u8; RECORD_BYTES]>,
}

// SAFETY: access is serialized by REPORTING; one handler invocation owns it.
unsafe impl Sync for ReportSlot {}

static SLOT: ReportSlot = ReportSlot {
    record: UnsafeCell::new(FaultRecord::EMPTY),
    text: UnsafeCell::new([0; RECORD_BYTES]),
};
static REPORTING: AtomicBool = AtomicBool::new(false);

const fn exception_name(code: i32) -> Option<&'static str> {
    Some(match code {
        EXCEPTION_STACK_OVERFLOW => "EXCEPTION_STACK_OVERFLOW",
        EXCEPTION_ACCESS_VIOLATION => "EXCEPTION_ACCESS_VIOLATION",
        EXCEPTION_ARRAY_BOUNDS_EXCEEDED => "EXCEPTION_ARRAY_BOUNDS_EXCEEDED",
        EXCEPTION_IN_PAGE_ERROR => "EXCEPTION_IN_PAGE_ERROR",
        EXCEPTION_DATATYPE_MISALIGNMENT => "EXCEPTION_DATATYPE_MISALIGNMENT",
        EXCEPTION_ILLEGAL_INSTRUCTION => "EXCEPTION_ILLEGAL_INSTRUCTION",
        EXCEPTION_PRIV_INSTRUCTION => "EXCEPTION_PRIV_INSTRUCTION",
        EXCEPTION_BREAKPOINT => "EXCEPTION_BREAKPOINT",
        EXCEPTION_INT_DIVIDE_BY_ZERO => "EXCEPTION_INT_DIVIDE_BY_ZERO",
        EXCEPTION_INT_OVERFLOW => "EXCEPTION_INT_OVERFLOW",
        EXCEPTION_FLT_DENORMAL_OPERAND => "EXCEPTION_FLT_DENORMAL_OPERAND",
        EXCEPTION_FLT_DIVIDE_BY_ZERO => "EXCEPTION_FLT_DIVIDE_BY_ZERO",
        EXCEPTION_FLT_INEXACT_RESULT => "EXCEPTION_FLT_INEXACT_RESULT",
        EXCEPTION_FLT_INVALID_OPERATION => "EXCEPTION_FLT_INVALID_OPERATION",
        EXCEPTION_FLT_OVERFLOW => "EXCEPTION_FLT_OVERFLOW",
        EXCEPTION_FLT_STACK_CHECK => "EXCEPTION_FLT_STACK_CHECK",
        EXCEPTION_FLT_UNDERFLOW => "EXCEPTION_FLT_UNDERFLOW",
        // Everything else (C++ and Rust unwinds, guard-page notifications the
        // OS uses to grow stacks, debugger events) is not a fatal fault.
        _ => return None,
    })
}

/// The kind of a fatal exception. A coroutine or other non-OS stack overflow
/// arrives as an access violation in the guard region, the same lookup the
/// Unix handler makes.
fn classify(code: i32, address: usize, bounds: StackBounds) -> FaultKind {
    if code == EXCEPTION_STACK_OVERFLOW
        || (code == EXCEPTION_ACCESS_VIOLATION && bounds.in_guard(address))
    {
        FaultKind::StackOverflow
    } else {
        FaultKind::Signal
    }
}

/// Walk the faulting thread's stack with the image unwind tables. x64 code
/// does not keep frame pointers, so the unwinder, not the frame chain, is the
/// authority here.
#[cfg(target_arch = "x86_64")]
unsafe fn capture_frames(
    context: *const windows_sys::Win32::System::Diagnostics::Debug::CONTEXT,
    bounds: StackBounds,
    out: &mut Frames,
) {
    use windows_sys::Win32::System::Diagnostics::Debug::{
        RtlLookupFunctionEntry, RtlVirtualUnwind, CONTEXT, UNW_FLAG_NHANDLER,
    };
    struct UnwindContext(UnsafeCell<MaybeUninit<CONTEXT>>);
    // SAFETY: only the REPORTING owner uses it.
    unsafe impl Sync for UnwindContext {}
    static UNWIND: UnwindContext = UnwindContext(UnsafeCell::new(MaybeUninit::uninit()));

    out.clear();
    if context.is_null() {
        return;
    }
    // SAFETY: the caller owns the static unwind context; the exception
    // context is valid for the handler's duration.
    unsafe {
        let ctx = (*UNWIND.0.get()).write(*context);
        out.push(ctx.Rip as usize);
        for _ in 0..(1 << 20) {
            let previous_sp = ctx.Rsp;
            let mut image_base = 0_u64;
            let entry = RtlLookupFunctionEntry(ctx.Rip, &raw mut image_base, std::ptr::null_mut());
            if entry.is_null() {
                // A leaf function: the return address is at the stack pointer.
                if !bounds.contains(ctx.Rsp as usize) || ctx.Rsp % 8 != 0 {
                    return;
                }
                ctx.Rip = (ctx.Rsp as *const u64).read();
                ctx.Rsp += 8;
            } else {
                let mut handler_data = std::ptr::null_mut();
                let mut establisher = 0_u64;
                RtlVirtualUnwind(
                    UNW_FLAG_NHANDLER,
                    image_base,
                    ctx.Rip,
                    entry,
                    ctx,
                    &raw mut handler_data,
                    &raw mut establisher,
                    std::ptr::null_mut(),
                );
            }
            if ctx.Rip == 0 || ctx.Rsp <= previous_sp || !bounds.contains(ctx.Rsp as usize) {
                return;
            }
            // Report the call instruction, not the return address after it.
            out.push(ctx.Rip as usize - 1);
        }
    }
}

#[cfg(not(target_arch = "x86_64"))]
unsafe fn capture_frames(
    context: *const windows_sys::Win32::System::Diagnostics::Debug::CONTEXT,
    _bounds: StackBounds,
    out: &mut Frames,
) {
    out.clear();
    if !context.is_null() {
        // SAFETY: the exception context is valid for the handler's duration.
        out.push(unsafe { (*context).Pc } as usize);
    }
}

unsafe extern "system" fn fatal_exception_handler(info: *mut EXCEPTION_POINTERS) -> i32 {
    // SAFETY: Windows provides valid exception records for the callback.
    let exception = unsafe { &*(*info).ExceptionRecord };
    let code = exception.ExceptionCode;
    let Some(name) = exception_name(code) else {
        return EXCEPTION_CONTINUE_SEARCH;
    };
    if REPORTING.swap(true, Ordering::AcqRel) {
        // One report per process; the first fault ends it.
        return EXCEPTION_CONTINUE_SEARCH;
    }
    // Avoid Rust formatting, allocation, locks and unwinding on a damaged
    // thread: the record and its text live in static storage.
    // SAFETY: this invocation owns SLOT.
    unsafe {
        let record = &mut *SLOT.record.get();
        let bounds = stack::current();
        // For access violations, the second parameter is the faulting address.
        let address = if exception.NumberParameters >= 2 {
            exception.ExceptionInformation[1]
        } else {
            exception.ExceptionAddress as usize
        };
        record.kind = classify(code, address, bounds);
        record.signal = code as u32;
        record.signal_name = name;
        record.sent = false;
        record.address = address;
        record.thread = stack::current_label();
        record.load_image_range();
        capture_frames((*info).ContextRecord, bounds, &mut record.frames);
        let text = record.format_into(&mut *SLOT.text.get());
        crate::trap_code::write_stderr(text);
        minidump::write_from_handler(info);
    }
    EXCEPTION_CONTINUE_SEARCH
}

mod minidump {
    use super::*;

    struct Config {
        /// NUL-terminated wide path for CreateFileW.
        wide_path: Vec<u16>,
        /// The same path for the stderr note.
        display: Vec<u8>,
        request: HANDLE,
        done: HANDLE,
    }

    // SAFETY: event handles are process-global kernel handles.
    unsafe impl Send for Config {}
    // SAFETY: as above; the handles are only signalled and waited on.
    unsafe impl Sync for Config {}

    static CONFIG: OnceLock<Config> = OnceLock::new();
    static EXCEPTION: AtomicPtr<EXCEPTION_POINTERS> = AtomicPtr::new(std::ptr::null_mut());
    static THREAD: AtomicU32 = AtomicU32::new(0);
    static WRITTEN: AtomicBool = AtomicBool::new(false);

    /// Read `HEW_MINIDUMP` and start the helper thread that waits to write.
    pub(super) fn configure() {
        let Some(dir) = std::env::var_os("HEW_MINIDUMP").filter(|dir| !dir.is_empty()) else {
            return;
        };
        let stem = std::env::current_exe()
            .ok()
            .and_then(|exe| exe.file_stem().map(std::ffi::OsStr::to_os_string))
            .unwrap_or_else(|| "hew".into());
        let mut file = stem;
        file.push(format!("-{}.dmp", std::process::id()));
        let path = std::path::Path::new(&dir).join(file);
        let wide_path = {
            use std::os::windows::ffi::OsStrExt;
            path.as_os_str().encode_wide().chain([0]).collect()
        };
        // SAFETY: unnamed auto-reset and manual-reset events.
        let (request, done) = unsafe {
            (
                CreateEventW(std::ptr::null(), 0, 0, std::ptr::null()),
                CreateEventW(std::ptr::null(), 1, 0, std::ptr::null()),
            )
        };
        if request.is_null() || done.is_null() {
            return;
        }
        let config = Config {
            wide_path,
            display: path.display().to_string().into_bytes(),
            request,
            done,
        };
        if CONFIG.set(config).is_err() {
            return;
        }
        let _ = std::thread::Builder::new()
            .name("hew-minidump".into())
            .spawn(helper);
    }

    fn helper() {
        let Some(config) = CONFIG.get() else { return };
        // SAFETY: the events live for the process; MiniDumpWriteDump receives
        // the faulting thread's exception pointers, valid while it waits.
        unsafe {
            if WaitForSingleObject(config.request, INFINITE) != WAIT_OBJECT_0 {
                return;
            }
            let file = CreateFileW(
                config.wide_path.as_ptr(),
                GENERIC_WRITE,
                0,
                std::ptr::null(),
                CREATE_ALWAYS,
                FILE_ATTRIBUTE_NORMAL,
                std::ptr::null_mut(),
            );
            if file != INVALID_HANDLE_VALUE {
                let exception = MINIDUMP_EXCEPTION_INFORMATION {
                    ThreadId: THREAD.load(Ordering::Acquire),
                    ExceptionPointers: EXCEPTION.load(Ordering::Acquire),
                    ClientPointers: 0,
                };
                let ok = MiniDumpWriteDump(
                    GetCurrentProcess(),
                    GetCurrentProcessId(),
                    file,
                    MiniDumpWithIndirectlyReferencedMemory | MiniDumpWithThreadInfo,
                    &raw const exception,
                    std::ptr::null(),
                    std::ptr::null(),
                );
                WRITTEN.store(ok != 0, Ordering::Release);
                CloseHandle(file);
            }
            SetEvent(config.done);
        }
    }

    /// Hand the fault to the helper and wait for the dump.
    ///
    /// # Safety
    ///
    /// `info` is the vectored handler's argument.
    pub(super) unsafe fn write_from_handler(info: *mut EXCEPTION_POINTERS) {
        let Some(config) = CONFIG.get() else { return };
        EXCEPTION.store(info, Ordering::Release);
        // SAFETY: GetCurrentThreadId has no preconditions; the events are live.
        unsafe {
            THREAD.store(GetCurrentThreadId(), Ordering::Release);
            SetEvent(config.request);
            let waited = WaitForSingleObject(config.done, MINIDUMP_WAIT_MS);
            if waited == WAIT_OBJECT_0 && WRITTEN.load(Ordering::Acquire) {
                crate::trap_code::write_stderr(b"hew: minidump written to ");
                crate::trap_code::write_stderr(&config.display);
                crate::trap_code::write_stderr(b"\n");
            } else {
                crate::trap_code::write_stderr(b"hew: minidump could not be written to ");
                crate::trap_code::write_stderr(&config.display);
                crate::trap_code::write_stderr(b"\n");
            }
        }
    }
}

/// Find the load range of the image this runtime is linked into from its PE
/// header. Called at installation, never from the handler.
fn record_image_range() {
    use windows_sys::Win32::System::Diagnostics::Debug::RtlPcToFileHeader;
    let mut base = std::ptr::null_mut();
    // SAFETY: the handler's own address lies in this image.
    unsafe { RtlPcToFileHeader(fatal_exception_handler as *const _, &raw mut base) };
    let base = base as usize;
    if base == 0 {
        return;
    }
    // SAFETY: a loaded image starts with its DOS header; e_lfanew locates the
    // NT headers, whose optional header holds SizeOfImage at offset 56.
    let size = unsafe {
        let nt = base + (base as *const u8).add(0x3C).cast::<u32>().read_unaligned() as usize;
        (nt as *const u8)
            .add(4 + 20 + 56)
            .cast::<u32>()
            .read_unaligned() as usize
    };
    super::record::set_image_range(base, base + size);
}

/// Prepare the calling thread to report its own faults.
fn init_thread(label: ThreadLabel) {
    stack::register_current_thread(label);
    let mut guarantee = STACK_GUARANTEE_BYTES;
    // SAFETY: the argument is a valid in/out size. A failure leaves the
    // system default guarantee, which still delivers EXCEPTION_STACK_OVERFLOW.
    unsafe { SetThreadStackGuarantee(&raw mut guarantee) };
}

pub(crate) fn init_crash_handling() {
    static INSTALL: Once = Once::new();
    let mut installed_here = false;
    INSTALL.call_once(|| {
        installed_here = true;
        record_image_range();
        minidump::configure();
        // SAFETY: the callback remains resident for the process lifetime.
        if unsafe { AddVectoredExceptionHandler(1, Some(fatal_exception_handler)) }.is_null() {
            std::process::abort();
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
    init_thread(if worker_id == u32::MAX {
        ThreadLabel::Other
    } else {
        ThreadLabel::Worker(worker_id)
    });
}

pub(crate) fn ignore_sigpipe() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stack_overflow_and_guard_violations_classify_alike() {
        let bounds = StackBounds {
            low: 0x7_0000_0000,
            high: 0x7_0010_0000,
            guard: 0,
        };
        assert_eq!(
            classify(EXCEPTION_STACK_OVERFLOW, 0, bounds),
            FaultKind::StackOverflow
        );
        assert_eq!(
            classify(EXCEPTION_ACCESS_VIOLATION, 0x7_0000_0000 - 8, bounds),
            FaultKind::StackOverflow
        );
        assert_eq!(
            classify(EXCEPTION_ACCESS_VIOLATION, 8, bounds),
            FaultKind::Signal
        );
        assert!(exception_name(EXCEPTION_STACK_OVERFLOW).is_some());
        // Rust panics and C++ exceptions are not faults.
        assert!(exception_name(0xE06D_7363_u32 as i32).is_none());
    }
}
