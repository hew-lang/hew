//! Owned logical-failure transport for the private native Hew callable ABI.
//!
//! A successful call initializes its result and leaves the fault output null.
//! A failed call transfers one non-null fault owner instead; callers execute
//! their explicit SIR cleanup before propagating or disposing of that owner.
//! This module does not install a catch boundary or recover hardware faults.
//! The handle is opaque to generated code and is not a public embedding API.

use std::io::{self, Write};
use std::sync::{Arc, Mutex};

use crate::util::MutexExt;

use hew_cabi::string::{string_as_str, HewString};

use crate::internal::types::{ExitReason, HEW_TRAP_USER_PANIC};

/// Private completion codes kept distinct from source panic and trap codes.
pub const HEW_FAULT_CANCELLED: i32 = -1;
pub const HEW_FAULT_DEADLINE: i32 = -2;
/// Internal cancellation requested after another race child has completed.
/// Only the owning race drain may suppress this completion diagnostic.
pub(crate) const HEW_FAULT_RACE_LOST: i32 = -3;

/// An opaque, uniquely owned logical fault wrapper. Diagnostic identity is
/// shared across terminal observers so propagation never reports it twice.
#[derive(Debug, Clone)]
pub struct HewFault {
    primary: Arc<FaultDiagnostic>,
    secondary: Vec<Arc<FaultDiagnostic>>,
}

#[derive(Debug)]
struct FaultDiagnostic {
    code: i32,
    message: Option<Box<str>>,
    reported: Mutex<bool>,
}

impl HewFault {
    fn new(code: i32, message: Option<Box<str>>) -> Self {
        Self {
            primary: Arc::new(FaultDiagnostic {
                code,
                message,
                reported: Mutex::new(false),
            }),
            secondary: Vec::new(),
        }
    }

    pub(crate) fn with_message(code: i32, message: String) -> Self {
        Self::new(code, Some(message.into_boxed_str()))
    }

    pub(crate) fn code(&self) -> i32 {
        self.primary.code
    }

    pub(crate) fn append(&mut self, secondary: Self) {
        self.secondary.push(secondary.primary);
        self.secondary.extend(secondary.secondary);
    }

    fn diagnostics(&self) -> impl Iterator<Item = &FaultDiagnostic> {
        std::iter::once(self.primary.as_ref()).chain(self.secondary.iter().map(AsRef::as_ref))
    }
}

/// Actor incarnations whose terminal crash has been published. This supports
/// resource disclosure from their legacy task threads; checked local cleanup
/// carries its fault in the invocation state and never changes this registry.
static CRASHED_ACTORS: std::sync::Mutex<Vec<u64>> = std::sync::Mutex::new(Vec::new());

thread_local! {
    /// The crashed actor whose owned state this thread is releasing right
    /// now (`free_actor_resources` runs the state drop synchronously).
    static CRASH_RELEASING: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };

    /// The actor that owns the task running on this thread, if any.
    ///
    /// A forked task is not its owning actor - `hew_actor_self()` stays null
    /// on the task's thread, and the spawn path keeps it that way on purpose -
    /// but a resource the task releases still belongs to that actor. Without
    /// this, a sink cancelled out of a forked task closed cleanly while its
    /// owner was unwinding a crash.
    static OWNING_ACTOR: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

/// The actor that owns the work being spawned from this thread: the actor
/// whose turn this is, or the actor that owns the task running here.
///
/// Called at the spawn site, on the spawning thread, and carried into the new
/// task's thread with [`with_owning_actor`].
#[must_use]
pub(crate) fn owning_actor_at_spawn() -> u64 {
    let actor = crate::actor::hew_actor_self();
    if actor.is_null() {
        return OWNING_ACTOR.with(std::cell::Cell::get);
    }
    // SAFETY: the current context's actor is live for the current turn.
    unsafe { (*actor).id }
}

/// Run `body` as work owned by actor `id`, so a release inside it discloses
/// that actor's crash. Restores the previous owner, for nested task threads.
pub(crate) fn with_owning_actor<T>(id: u64, body: impl FnOnce() -> T) -> T {
    let previous = OWNING_ACTOR.with(|cell| cell.replace(id));
    let result = body();
    OWNING_ACTOR.with(|cell| cell.set(previous));
    result
}

/// Register an actual terminal crash before cancelling owned task work.
pub(crate) fn note_actor_crash(id: u64) {
    let mut crashed = CRASHED_ACTORS
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if !crashed.contains(&id) {
        crashed.push(id);
    }
}

/// Release the owned state of actor `id` on its terminal teardown. While
/// `release` runs, a pipe sink it drops learns whether the owner crashed.
///
/// One call site: the actor's terminal free. Also retires its terminal crash
/// entry, so the registry only ever holds live crashed actors.
pub(crate) fn release_actor_state(id: u64, crashed: bool, release: impl FnOnce()) {
    CRASHED_ACTORS
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .retain(|unwinding| *unwinding != id);
    if !crashed {
        release();
        return;
    }
    CRASH_RELEASING.with(|cell| cell.set(id));
    release();
    CRASH_RELEASING.with(|cell| cell.set(0));
}

/// The crashed actor releasing a resource on the current thread, if the
/// release belongs to a crash: either this thread is dropping a crashed
/// actor's state, or its owning actor has published a terminal crash.
#[must_use]
pub(crate) fn crashing_owner() -> Option<u64> {
    let releasing = CRASH_RELEASING.with(std::cell::Cell::get);
    if releasing != 0 {
        return Some(releasing);
    }
    let actor = crate::actor::hew_actor_self();
    let id = if actor.is_null() {
        // Not an actor turn. A task forked by an actor still releases that
        // actor's resources, and its scope is joined inside the crashing
        // turn, so the owner is still recorded as crashed here.
        OWNING_ACTOR.with(std::cell::Cell::get)
    } else {
        // SAFETY: the current context's actor is live for the current turn.
        unsafe { (*actor).id }
    };
    if id == 0 {
        return None;
    }
    CRASHED_ACTORS
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .contains(&id)
        .then_some(id)
}

/// Create an owned logical-failure code. Unknown codes report as `UnknownFault`.
///
/// Returns one owner, released by [`hew_fault_drop`]. Allocation failure remains
/// process-fatal; this operation never converts allocator failure into a fault.
#[no_mangle]
#[must_use]
pub extern "C" fn hew_fault_new(code: i32) -> *mut HewFault {
    Box::into_raw(Box::new(HewFault::new(code, None)))
}

/// Copy a borrowed managed string into one owned logical panic fault.
/// The caller may release the original string immediately after this returns.
/// Allocation failure remains process-fatal, as with [`hew_fault_new`].
///
/// # Safety
/// `message` must be a live managed string for this call; null means empty.
#[no_mangle]
#[must_use]
pub unsafe extern "C" fn hew_fault_new_panic(message: *const HewString) -> *mut HewFault {
    // SAFETY: the caller supplies a live length-carrying UTF-8 string borrow.
    let message = unsafe { string_as_str(message) }.into();
    Box::into_raw(Box::new(HewFault::new(HEW_TRAP_USER_PANIC, Some(message))))
}

/// Copy a borrowed managed string into the fault a `fails` handler raises
/// when its declared error has no caller to answer. The submission arrived
/// through a mailbox view, so the failure is the actor's own.
///
/// The caller may release the original string immediately after this returns.
/// Allocation failure remains process-fatal, as with [`hew_fault_new`].
///
/// # Safety
/// `message` must be a live managed string for this call; null means empty.
#[no_mangle]
#[must_use]
pub unsafe extern "C" fn hew_fault_new_unhandled_failure(
    message: *const HewString,
) -> *mut HewFault {
    // SAFETY: the caller supplies a live length-carrying UTF-8 string borrow.
    let message = unsafe { string_as_str(message) }.into();
    Box::into_raw(Box::new(HewFault::new(
        crate::internal::types::HEW_TRAP_ACTOR_UNHANDLED_FAILURE,
        Some(message),
    )))
}

/// Combine optional fault owners, preserving the primary code and message.
/// Secondary diagnostics are flattened in occurrence order into owned text.
/// This compiler-private operation does not read or change callable status.
/// Allocation failure remains process-fatal, as with [`hew_fault_new`].
///
/// # Safety
/// Each non-null argument must be a live, uniquely owned [`HewFault`], with no
/// outstanding borrows. Non-null arguments must be distinct. Both owners are
/// consumed; only the returned owner may subsequently be used or dropped.
#[no_mangle]
#[must_use]
pub unsafe extern "C" fn hew_fault_combine(
    primary: *mut HewFault,
    secondary: *mut HewFault,
) -> *mut HewFault {
    if primary.is_null() {
        return secondary;
    }
    if secondary.is_null() {
        return primary;
    }
    // SAFETY: the caller transfers distinct unique owners without live borrows.
    let primary_fault = unsafe { &mut *primary };
    // SAFETY: secondary is a distinct allocation consumed exactly once here.
    let secondary = unsafe { Box::from_raw(secondary) };
    primary_fault.append(*secondary);
    primary
}

/// Remove the cancellation used only to request cooperative cleanup.
/// Preserve the first actual cleanup failure and every following diagnostic.
///
/// # Safety
/// A non-null pointer transfers one unique fault owner without borrowers.
#[no_mangle]
#[must_use]
pub unsafe extern "C" fn hew_fault_finish_cleanup(fault: *mut HewFault) -> *mut HewFault {
    if fault.is_null() {
        return fault;
    }
    // SAFETY: the caller transfers the unique allocation.
    let mut fault = unsafe { Box::from_raw(fault) };
    if !matches!(
        fault.code(),
        HEW_FAULT_CANCELLED | HEW_FAULT_DEADLINE | HEW_FAULT_RACE_LOST
    ) {
        return Box::into_raw(fault);
    }
    let mut secondary = fault.secondary.into_iter();
    let Some(first) = secondary.find(|diagnostic| {
        !matches!(
            diagnostic.code,
            HEW_FAULT_CANCELLED | HEW_FAULT_DEADLINE | HEW_FAULT_RACE_LOST
        )
    }) else {
        return std::ptr::null_mut();
    };
    fault.primary = first;
    fault.secondary = secondary.collect();
    Box::into_raw(fault)
}

/// Remove only the race's own cancellation marker. Ordinary cancellation,
/// deadlines and cleanup diagnostics retain their original order and ownership.
///
/// # Safety
/// `fault` transfers one optional unique fault owner.
pub(crate) unsafe fn finish_race_loser(fault: *mut HewFault) -> *mut HewFault {
    if fault.is_null() {
        return fault;
    }
    // SAFETY: the caller transfers the unique allocation.
    let mut fault = unsafe { Box::from_raw(fault) };
    fault
        .secondary
        .retain(|diagnostic| diagnostic.code != HEW_FAULT_RACE_LOST);
    if fault.code() == HEW_FAULT_RACE_LOST {
        if fault.secondary.is_empty() {
            return std::ptr::null_mut();
        }
        let first = fault.secondary.remove(0);
        fault.primary = first;
    }
    Box::into_raw(fault)
}

/// Consume one private logical fault into a public host error without reporting.
/// The diagnostic preserves the complete primary and secondary report bytes;
/// the public category is independent of internal trap-code numbering.
///
/// # Safety
/// `fault` is one live non-null unique owner, with no outstanding borrows.
#[no_mangle]
#[must_use]
pub unsafe extern "C" fn hew_fault_into_host_error(
    fault: *mut HewFault,
) -> *mut hew_cabi::host_error::HewError {
    use hew_cabi::host_error::{HewError, HostStatus};
    // SAFETY: the caller transfers a non-null unique fault allocation.
    let fault = unsafe { Box::from_raw(fault) };
    let mut diagnostic = Vec::new();
    // Writing to Vec cannot return an I/O error; allocation failure is fatal.
    let _ = write_report(&fault, &mut diagnostic);
    // SAFETY: write_report concatenates UTF-8 str components and ASCII formatting.
    // Taking the bytes preserves all text without replacement or a second copy.
    let message = unsafe { String::from_utf8_unchecked(diagnostic) };
    HewError::new(HostStatus::LogicalFault, message).into_raw()
}

/// Consume a logical fault into an owned diagnostic for scope recovery.
///
/// # Safety
/// `fault` must be a live, unique, non-null fault owner without borrowers.
#[no_mangle]
#[must_use]
pub unsafe extern "C" fn hew_fault_take_message(fault: *mut HewFault) -> *mut HewString {
    // SAFETY: the caller transfers the unique fault allocation.
    let fault = unsafe { Box::from_raw(fault) };
    let mut diagnostic = Vec::new();
    let _ = write_report(&fault, &mut diagnostic);
    // SAFETY: reports consist entirely of UTF-8 strings and ASCII formatting.
    let message = unsafe { String::from_utf8_unchecked(diagnostic) };
    hew_cabi::string::string_from_str(&message)
}

/// Release one fault owner. Null is accepted for an empty fault output slot.
///
/// # Safety
/// A non-null pointer must be a live owner returned by [`hew_fault_new`] or
/// [`hew_fault_new_panic`] or [`hew_fault_combine`]. No
/// borrower may remain in use, and this owner must not have been released before.
#[no_mangle]
pub unsafe extern "C" fn hew_fault_drop(fault: *mut HewFault) {
    if !fault.is_null() {
        // SAFETY: the caller transfers the unique allocation owner to us.
        drop(unsafe { Box::from_raw(fault) });
    }
}

/// Read a logical failure code without reporting or consuming the diagnostic.
/// # Safety
/// `fault` is null or points to a live fault borrowed for this call.
#[no_mangle]
pub unsafe extern "C" fn hew_fault_code(fault: *const HewFault) -> i32 {
    // SAFETY: the caller retains the immutable fault through this read.
    unsafe { fault.as_ref() }.map_or(0, HewFault::code)
}

/// Report each diagnostic to stderr once across all observers. Return 0 on
/// success, 1 on I/O failure or an absent fault. Reporting does not consume
/// the owner or remove any text available to scope recovery and host errors.
///
/// # Safety
/// A non-null pointer must refer to a live [`HewFault`] for the duration of this
/// call. No concurrent call may release it.
#[no_mangle]
pub unsafe extern "C" fn hew_fault_report(fault: *const HewFault) -> i32 {
    // SAFETY: a non-null input is valid for this borrow by the caller's contract.
    let Some(fault) = (unsafe { fault.as_ref() }) else {
        return 1;
    };
    // Unlike eprintln!, an output error must not panic across this C boundary.
    i32::from(write_unreported(fault, &mut io::stderr().lock()).is_err())
}

/// Raise a fault that generated drop glue has no owner to carry.
///
/// A release a frame emits itself is that frame's fault edge: the failing
/// `close` fills the frame's fault record and the frame keeps releasing. This
/// is the trap path for the releases that have no such frame — a descriptor
/// drop thunk, a collection element release, a shared payload's destructor —
/// where nothing can hand the failure back to the value's owner. It reports
/// the fault's own typed line — so a `panic` inside `close` keeps its message
/// — then routes into the one trap bridge, which crashes the actor when a
/// supervisor can rule on the failure and otherwise ends the run with
/// status 1.
///
/// `fault` transfers one optional fault owner; `code` is the status `close`
/// returned and stands alone when no fault accompanies it.
///
/// # Safety
/// A non-null `fault` must be a live, unique owner without borrowers.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_fault_trap(code: i32, fault: *mut HewFault) {
    if fault.is_null() {
        // SAFETY: the bridge accepts any context; nothing was reported yet.
        unsafe { crate::trap_code::fault_trap_bridge(code, false) };
        return;
    }
    // A collection release in progress finishes releasing what it owns before
    // this fault leaves the runtime, so the failing element does not strand
    // its siblings. That walk raises it again once it is done.
    // SAFETY: the caller transfers one live, unique fault owner.
    if unsafe { crate::release_walker::held_fault(code, fault) } {
        return;
    }
    // SAFETY: the caller transfers one live, unique fault owner.
    let fault = unsafe { Box::from_raw(fault) };
    let code = fault.code();
    let _ = write_report(&fault, &mut io::stderr().lock());
    drop(fault);
    // SAFETY: the bridge accepts any context; the typed line is already out.
    unsafe { crate::trap_code::fault_trap_bridge(code, true) };
}

/// Write one fault's typed line to stderr for a code with no fault owner.
///
/// The main-context trap bridge reaches a fatal trap with a code and nothing
/// else; the checked path reaches the same diagnostic through
/// [`hew_fault_report`]. Both print the one line HEW-SPEC-2026 5.8 promises,
/// from this one formatter, so the text does not depend on which path failed.
pub(crate) fn report_trap_code(code: i32) {
    let fault = HewFault::new(code, None);
    let _ = write_report(&fault, &mut io::stderr().lock());
}

fn fault_reason(code: i32) -> &'static str {
    if matches!(code, HEW_FAULT_CANCELLED | HEW_FAULT_RACE_LOST) {
        return "Cancelled";
    }
    if code == HEW_FAULT_DEADLINE {
        return "Deadline";
    }
    match ExitReason::from_error_code(code) {
        // A logical handle cannot make a hardware fault recoverable.
        ExitReason::Signal(_) | ExitReason::Normal => "UnknownFault",
        reason => reason.trap_kind_name(),
    }
}

fn write_diagnostic(
    diagnostic: &FaultDiagnostic,
    secondary: bool,
    output: &mut impl Write,
) -> io::Result<()> {
    let label = if secondary {
        "secondary failure"
    } else {
        "failure"
    };
    write!(
        output,
        "hew: {label}: {} ({})",
        fault_reason(diagnostic.code),
        diagnostic.code
    )?;
    if let Some(message) = &diagnostic.message {
        output.write_all(b": ")?;
        output.write_all(message.as_bytes())?;
    }
    output.write_all(b"\n")
}

fn write_report(fault: &HewFault, output: &mut impl Write) -> io::Result<()> {
    for (index, diagnostic) in fault.diagnostics().enumerate() {
        write_diagnostic(diagnostic, index != 0, output)?;
    }
    Ok(())
}

fn write_unreported(fault: &HewFault, output: &mut impl Write) -> io::Result<()> {
    for (index, diagnostic) in fault.diagnostics().enumerate() {
        let mut reported = diagnostic.reported.lock_or_recover();
        if !*reported {
            write_diagnostic(diagnostic, index != 0, output)?;
            *reported = true;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cloned_faults_report_once_without_losing_recovery_text() {
        let mut fault = HewFault::with_message(212, "primary".into());
        fault.append(HewFault::with_message(212, "secondary".into()));
        let mut observed = fault.clone();
        let mut output = Vec::new();
        write_unreported(&fault, &mut output).unwrap();
        let complete = output.clone();
        write_unreported(&observed, &mut output).unwrap();
        assert_eq!(output, complete);

        observed.append(HewFault::with_message(212, "later close".into()));
        write_unreported(&observed, &mut output).unwrap();
        assert_eq!(
            &output[complete.len()..],
            b"hew: secondary failure: UserPanic (212): later close\n"
        );
        let mut recovery = Vec::new();
        write_report(&observed, &mut recovery).unwrap();
        assert_eq!(recovery, output);
    }

    #[test]
    fn a_failed_report_can_be_retried_by_another_observer() {
        let fault = HewFault::with_message(212, "retry".into());
        let observer = fault.clone();
        assert!(write_unreported(&fault, &mut [].as_mut_slice()).is_err());
        let mut output = Vec::new();
        write_unreported(&observer, &mut output).unwrap();
        assert_eq!(output, b"hew: failure: UserPanic (212): retry\n");
    }

    fn panic_fault(text: &str) -> *mut HewFault {
        let source = hew_cabi::string::string_from_str(text);
        // SAFETY: the constructor copies the live source borrow.
        let fault = unsafe { hew_fault_new_panic(source) };
        // SAFETY: the fault no longer borrows the source owner.
        unsafe { hew_cabi::string::string_release(source) };
        fault
    }

    #[test]
    fn combine_transfers_each_optional_owner() {
        for has_primary in [false, true] {
            for has_secondary in [false, true] {
                let primary = if has_primary {
                    hew_fault_new(202)
                } else {
                    std::ptr::null_mut()
                };
                let secondary = if has_secondary {
                    panic_fault("secondary")
                } else {
                    std::ptr::null_mut()
                };
                // SAFETY: these are distinct unique owners or null.
                let combined = unsafe { hew_fault_combine(primary, secondary) };
                assert_eq!(combined, if has_primary { primary } else { secondary });
                if !combined.is_null() {
                    // SAFETY: only the combined owner remains live.
                    let fault = unsafe { &*combined };
                    assert_eq!(
                        fault.code(),
                        if has_primary {
                            202
                        } else {
                            HEW_TRAP_USER_PANIC
                        }
                    );
                    assert_eq!(
                        fault.primary.message.as_deref(),
                        if has_primary { None } else { Some("secondary") }
                    );
                    assert_eq!(
                        fault.secondary.len(),
                        usize::from(has_primary && has_secondary)
                    );
                    if let Some(secondary) = fault.secondary.first() {
                        assert_eq!(secondary.code, HEW_TRAP_USER_PANIC);
                        assert_eq!(secondary.message.as_deref(), Some("secondary"));
                    }
                }
                // SAFETY: transfer the sole remaining owner, including null.
                unsafe { hew_fault_drop(combined) };
            }
        }
    }

    #[test]
    fn recovery_owns_complete_primary_and_secondary_diagnostics() {
        let primary = panic_fault("primary\0é");
        let secondary = panic_fault("cleanup 雪");
        // SAFETY: both faults are distinct unique owners; recovery consumes them.
        let message = unsafe { hew_fault_take_message(hew_fault_combine(primary, secondary)) };
        // SAFETY: recovery returned a live managed string owned by this test.
        assert_eq!(unsafe { string_as_str(message) }, "hew: failure: UserPanic (212): primary\0é\nhew: secondary failure: UserPanic (212): cleanup 雪\n");
        // SAFETY: no borrow remains, so release the transferred owner once.
        unsafe { hew_cabi::string::string_release(message) };
    }

    #[test]
    fn internal_close_cancellation_promotes_cleanup_failure_without_losing_text() {
        for code in [HEW_FAULT_CANCELLED, HEW_FAULT_DEADLINE] {
            // SAFETY: each operation consumes one unique fault owner.
            unsafe {
                assert!(hew_fault_finish_cleanup(hew_fault_new(code)).is_null());
                let failed = hew_fault_combine(hew_fault_new(code), panic_fault("cleanup\0雪"));
                let failed = hew_fault_combine(failed, panic_fault("older cleanup"));
                let failed = hew_fault_finish_cleanup(failed);
                assert_eq!((*failed).code(), HEW_TRAP_USER_PANIC);
                let mut report = Vec::new();
                write_report(&*failed, &mut report).unwrap();
                assert_eq!(report, "hew: failure: UserPanic (212): cleanup\0雪\nhew: secondary failure: UserPanic (212): older cleanup\n".as_bytes());
                hew_fault_drop(failed);
            }
        }
    }

    #[test]
    fn race_loser_cleanup_preserves_real_faults_and_parent_cancellation() {
        // SAFETY: each operation consumes distinct owned faults exactly once.
        unsafe {
            assert!(finish_race_loser(hew_fault_new(HEW_FAULT_RACE_LOST)).is_null());
            for retained in [HEW_FAULT_CANCELLED, HEW_FAULT_DEADLINE, HEW_TRAP_USER_PANIC] {
                let fault =
                    hew_fault_combine(hew_fault_new(HEW_FAULT_RACE_LOST), hew_fault_new(retained));
                let fault = hew_fault_combine(fault, panic_fault("cleanup 雪"));
                let fault = hew_fault_combine(fault, hew_fault_new(HEW_FAULT_RACE_LOST));
                let fault = finish_race_loser(fault);
                assert_eq!((*fault).code(), retained);
                assert_eq!((*fault).secondary.len(), 1);
                assert_eq!(
                    (&(*fault).secondary)[0].message.as_deref(),
                    Some("cleanup 雪")
                );
                hew_fault_drop(fault);
            }
        }
    }

    #[test]
    fn nested_combine_preserves_primary_and_retryable_diagnostic_order() {
        let text = "primary\0é 🦀\nunchanged";
        let primary = panic_fault(text);
        // SAFETY: each combination consumes distinct unique allocations.
        let combined = unsafe {
            let primary = hew_fault_combine(primary, hew_fault_new(202));
            let secondary = hew_fault_combine(panic_fault("secondary\0雪\nend"), panic_fault(""));
            hew_fault_combine(primary, secondary)
        };
        assert_eq!(combined, primary);
        // SAFETY: combined is the sole live owner throughout these report borrows.
        let fault = unsafe { &*combined };
        assert_eq!(fault.code(), HEW_TRAP_USER_PANIC);
        assert_eq!(fault.primary.message.as_deref(), Some(text));
        let expected = format!(
            "hew: failure: UserPanic (212): {text}\n\
             hew: secondary failure: DivideByZero (202)\n\
             hew: secondary failure: UserPanic (212): secondary\0雪\nend\n\
             hew: secondary failure: UserPanic (212): \n"
        );
        let mut output = Vec::new();
        write_report(fault, &mut output).unwrap();
        assert_eq!(output, expected.as_bytes());
        // Exercise partial writes in both primary and secondary diagnostics,
        // including UTF-8, NUL and the explicitly empty secondary message.
        for limit in 0..output.len() {
            let mut buffer = vec![0; limit];
            assert_eq!(
                write_report(fault, &mut buffer.as_mut_slice())
                    .unwrap_err()
                    .kind(),
                io::ErrorKind::WriteZero
            );
            assert_eq!(buffer, output[..limit]);
            let mut retry = Vec::new();
            write_report(fault, &mut retry).unwrap();
            assert_eq!(retry, output);
        }
        // SAFETY: reports are nonconsuming; release the combined owner and text.
        unsafe { hew_fault_drop(combined) };
    }

    #[test]
    fn panic_owns_the_complete_message_after_the_source_is_released() {
        for text in ["", "before\0é 🦀\nafter"] {
            let source = hew_cabi::string::string_from_str(text);
            // SAFETY: the constructor borrows this live managed string.
            let fault = unsafe { hew_fault_new_panic(source) };
            // SAFETY: the constructor has copied the message; the source owner is ours.
            unsafe { hew_cabi::string::string_release(source) };
            let mut output = Vec::new();
            // SAFETY: the constructor returned a uniquely owned live fault.
            write_report(unsafe { &*fault }, &mut output).unwrap();
            assert_eq!(
                output,
                format!("hew: failure: UserPanic (212): {text}\n").as_bytes()
            );
            // Fail at every output boundary, including inside UTF-8 and after NUL.
            // A partial report must leave the fault available for another attempt.
            for limit in 0..output.len() {
                let mut buffer = vec![0; limit];
                // SAFETY: reporting only borrows the same live fault owner.
                let error =
                    write_report(unsafe { &*fault }, &mut buffer.as_mut_slice()).unwrap_err();
                assert_eq!(error.kind(), io::ErrorKind::WriteZero);
                assert_eq!(buffer, output[..limit]);
            }
            let mut retry = Vec::new();
            // SAFETY: failed reports neither release nor mutate the fault.
            write_report(unsafe { &*fault }, &mut retry).unwrap();
            assert_eq!(retry, output);
            // SAFETY: the test transfers its sole fault and message owner.
            unsafe { hew_fault_drop(fault) };
        }
    }

    #[test]
    fn absent_fault_is_not_reported_as_success() {
        // SAFETY: the ABI explicitly accepts an empty fault output slot.
        assert_eq!(unsafe { hew_fault_report(std::ptr::null()) }, 1);
    }

    #[test]
    fn logical_fault_report_preserves_canonical_reason_and_code() {
        let mut output = Vec::new();
        write_report(&HewFault::new(202, None), &mut output).unwrap();
        assert_eq!(output, b"hew: failure: DivideByZero (202)\n");
    }

    #[test]
    fn unknown_fault_code_is_not_reported_as_a_hardware_signal() {
        for (code, expected) in [
            (-7, "hew: failure: UnknownFault (-7)\n"),
            (0, "hew: failure: UnknownFault (0)\n"),
        ] {
            let mut output = Vec::new();
            write_report(&HewFault::new(code, None), &mut output).unwrap();
            assert_eq!(output, expected.as_bytes());
        }
    }

    #[test]
    fn report_write_failure_is_returned_without_panicking() {
        struct Unwritable;
        impl Write for Unwritable {
            fn write(&mut self, _: &[u8]) -> io::Result<usize> {
                Err(io::ErrorKind::BrokenPipe.into())
            }

            fn flush(&mut self) -> io::Result<()> {
                Ok(())
            }
        }
        assert_eq!(
            write_report(&HewFault::new(202, None), &mut Unwritable)
                .unwrap_err()
                .kind(),
            io::ErrorKind::BrokenPipe
        );
    }
}
