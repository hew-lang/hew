//! Owned logical-failure transport for the private native Hew callable ABI.
//!
//! A successful call initializes its result and leaves the fault output null.
//! A failed call transfers one non-null fault owner instead; callers execute
//! their explicit SIR cleanup before propagating or disposing of that owner.
//! This module does not install a catch boundary or recover hardware faults.
//! The handle is opaque to generated code and is not a public embedding API.

use std::io::{self, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

use crate::util::MutexExt;

use hew_cabi::string::{string_as_str, HewString};

use crate::failure_record::{Backtrace, FailureRecord};
use crate::internal::types::HEW_TRAP_USER_PANIC;

/// Private completion codes kept distinct from source panic and trap codes.
pub const HEW_FAULT_CANCELLED: i32 = -1;
/// Delivery completion used only by a compiled interceptor wrapper.
pub const HEW_FAULT_REFUSED: i32 = -4;

/// Whether `code` is a cancellation that a requested shutdown caused. Such a
/// cancellation is an expected stop, not a fault. The wasm32 driver has no
/// shutdown sequence, so there it is always false.
pub(crate) fn cancelled_by_shutdown(code: i32) -> bool {
    #[cfg(not(target_arch = "wasm32"))]
    {
        code == HEW_FAULT_CANCELLED && crate::shutdown::hew_is_shutting_down() != 0
    }
    #[cfg(target_arch = "wasm32")]
    {
        let _ = code;
        false
    }
}
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
    assertion: Option<AssertionOperands>,
    /// The failure as described at its origin; absent for the completion
    /// markers (cancellation, deadline, refusal) that are not failures.
    record: Option<Mutex<Box<FailureRecord>>>,
    /// NUL-terminated source name owned for as long as the record can be read.
    site_file: OnceLock<Option<std::ffi::CString>>,
    /// Names from the originating image, retained after that image unloads.
    image_frames: OnceLock<Vec<Option<OwnedFrameText>>>,
    /// Owned source facts copied while the failing code image remains live.
    snapshot: OnceLock<Option<Arc<crate::failure_snapshot::FailureSnapshot>>>,
    /// The record has gone through [`hew_fault_raise`].
    raised: AtomicBool,
    reported: Mutex<bool>,
}

#[derive(Debug)]
struct OwnedFrameText {
    name: std::ffi::CString,
    file: Option<std::ffi::CString>,
    line: u32,
}

/// Copy a compiler table string without charging the failing actor.
unsafe fn copy_image_text(text: *const std::ffi::c_char) -> Option<std::ffi::CString> {
    if text.is_null() {
        return None;
    }
    // SAFETY: the originating image remains live during this copy.
    let bytes = unsafe { std::ffi::CStr::from_ptr(text) }.to_bytes_with_nul();
    if bytes.len() > 4096 {
        return None;
    }
    let mut owned = Vec::new();
    owned.try_reserve_exact(bytes.len()).ok()?;
    owned.extend_from_slice(bytes);
    // SAFETY: the source C string has exactly one terminating NUL.
    Some(unsafe { std::ffi::CString::from_vec_with_nul_unchecked(owned) })
}

impl FaultDiagnostic {
    /// Raise this failure once: name its frames and hand it to
    /// [`hew_fault_raise`], the one point a debugger breaks on.
    ///
    /// Inlined, with the raise as its last call on a heap record, so the
    /// optimizer can make it a tail call: a debugger stopped in
    /// `hew_fault_raise` then sees the failing Hew function as its caller.
    #[allow(
        clippy::inline_always,
        reason = "the raise must stay in the caller so hew_fault_raise is a tail call"
    )]
    #[inline(always)]
    fn raise(&self) {
        let Some(record) = &self.record else {
            return;
        };
        if self.raised.swap(true, Ordering::AcqRel) {
            return;
        }
        let record: *const FailureRecord = {
            let mut record = record.lock_or_recover();
            crate::failure_record::name_frames(&mut record);
            &raw const **record
        };
        // SAFETY: the record is boxed in this diagnostic, which outlives the
        // call; its only writer (the site attachment) has finished.
        unsafe { raise_record(record) };
    }

    /// The record to report: the origin's, or a bare one for a marker.
    fn report_record(&self) -> FailureRecord {
        let mut record = self.record.as_ref().map_or_else(
            || {
                let mut record = FailureRecord::new(self.code);
                // Completion markers do not capture an origin, but an
                // unhandled marker still belongs to the current actor turn.
                crate::crash::describe_current_actor(&mut record);
                record
            },
            |record| **record.lock_or_recover(),
        );
        if record.message_len == 0 {
            if let Some(message) = &self.message {
                record.set_message(message);
            }
        }
        record
    }
}

/// Rendered values from one failed comparison, retained with its fault.
#[derive(Debug, Clone, serde::Serialize)]
pub(crate) struct AssertionOperands {
    pub operator: String,
    pub left: String,
    pub right: String,
}

/// Whether `code` describes a failure, as opposed to a completion marker.
fn is_failure(code: i32) -> bool {
    code > 0
}

impl HewFault {
    fn new(code: i32, message: Option<Box<str>>) -> Self {
        Self::new_with_assertion(code, message, None)
    }

    /// A failure raised by the runtime outside a Hew handler. It has no
    /// originating Hew stack or source location to attribute to the actor.
    pub(crate) fn external(code: i32, actor_id: u64, spawn_serial: u64) -> Self {
        let fault = Self::build(code, None, None, false);
        if let Some(record) = &fault.primary.record {
            let mut record = record.lock_or_recover();
            record.actor.id = actor_id;
            record.actor.spawn_serial = spawn_serial;
            record.wall_time_ns = crate::failure_record::wall_time_ns();
            record.build = crate::failure_record::build_info();
        }
        fault
    }

    fn new_with_assertion(
        code: i32,
        message: Option<Box<str>>,
        assertion: Option<AssertionOperands>,
    ) -> Self {
        Self::build(code, message, assertion, true)
    }

    fn build(
        code: i32,
        message: Option<Box<str>>,
        assertion: Option<AssertionOperands>,
        capture: bool,
    ) -> Self {
        let record = is_failure(code).then(|| {
            let mut record = if capture {
                origin_record(code)
            } else {
                Box::new(FailureRecord::new(code))
            };
            if let Some(message) = &message {
                record.set_message(message);
            }
            Mutex::new(record)
        });
        Self {
            primary: Arc::new(FaultDiagnostic {
                code,
                message,
                assertion,
                record,
                site_file: OnceLock::new(),
                image_frames: OnceLock::new(),
                snapshot: OnceLock::new(),
                raised: AtomicBool::new(false),
                reported: Mutex::new(false),
            }),
            secondary: Vec::new(),
        }
    }

    pub(crate) fn with_message(code: i32, message: String) -> Self {
        Self::new(code, Some(message.into_boxed_str()))
    }

    /// Copy the failure's source facts before its originating image can unload.
    /// Clones of this fault share the same immutable snapshot.
    ///
    /// # Safety
    /// The caller retains the code image containing the failure record's
    /// source and symbol tables until this call returns.
    pub(crate) unsafe fn snapshot(&self) -> Option<Arc<crate::failure_snapshot::FailureSnapshot>> {
        self.primary
            .snapshot
            .get_or_init(|| {
                let record = self.primary.record.as_ref()?.lock_or_recover();
                let message = self
                    .primary
                    .message
                    .as_deref()
                    .unwrap_or_else(|| fault_reason(self.code()));
                // SAFETY: the caller retains the source and symbol tables until
                // this first snapshot has finished copying their text.
                Some(Arc::new(unsafe {
                    crate::failure_snapshot::capture(
                        &record,
                        message,
                        crate::failure_record::Backtrace::current()
                            != crate::failure_record::Backtrace::Off,
                    )
                }))
            })
            .clone()
    }

    /// Snapshot terminal cleanup diagnostics for the authenticated observation wire.
    /// The primary code remains separate from the bounded application message.
    pub(crate) fn remote_payload(&self) -> (i32, String) {
        let mut message = self
            .primary
            .message
            .as_deref()
            .unwrap_or_default()
            .to_owned();
        for diagnostic in &self.secondary {
            let mut line = Vec::new();
            let _ = write_diagnostic(diagnostic, true, &mut line);
            message.push('\n');
            message.push_str(
                std::str::from_utf8(&line)
                    .expect("fault diagnostics are UTF-8")
                    .trim_end_matches('\n'),
            );
        }
        let limit = crate::envelope::MAX_TERMINAL_FAULT_MESSAGE_BYTES;
        if message.len() > limit {
            const SUFFIX: &str = " [truncated]";
            let mut end = limit - SUFFIX.len();
            while !message.is_char_boundary(end) {
                end -= 1;
            }
            message.truncate(end);
            message.push_str(SUFFIX);
        }
        (self.code(), message)
    }

    /// The originating node already reported this terminal cleanup diagnostic.
    /// Recovery still owns its text, but propagation must not print it again.
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn from_remote(code: i32, message: String) -> Self {
        let fault = Self::new(
            code,
            (!message.is_empty()).then(|| message.into_boxed_str()),
        );
        *fault.primary.reported.lock_or_recover() = true;
        // The origin node raised it; this node only carries it on.
        fault.primary.raised.store(true, Ordering::Release);
        fault
    }

    /// Application text when supplied, otherwise the structured fault kind.
    pub(crate) fn message(&self) -> &str {
        self.primary
            .message
            .as_deref()
            .unwrap_or_else(|| fault_reason(self.code()))
    }

    /// Raise every diagnostic that has not been raised yet.
    pub(crate) fn raise(&self) {
        for diagnostic in self.diagnostics() {
            diagnostic.raise();
        }
    }

    fn recovery_message(&self) -> String {
        let mut message = self.message().to_owned();
        for diagnostic in &self.secondary {
            message.push_str("; secondary failure: ");
            message.push_str(fault_reason(diagnostic.code));
            if let Some(text) = &diagnostic.message {
                message.push_str(": ");
                message.push_str(text);
            }
        }
        message
    }

    pub(crate) fn code(&self) -> i32 {
        self.primary.code
    }

    /// Remove an expected completion marker while retaining cleanup faults.
    pub(crate) fn without_primary(mut self) -> Option<Self> {
        if self.secondary.is_empty() {
            return None;
        }
        self.primary = self.secondary.remove(0);
        Some(self)
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

/// Ordinary vector indexing diagnostic.
pub const BOUNDS_VECTOR_INDEX: u32 = 0;
/// Two-index vector swap diagnostic.
pub const BOUNDS_VECTOR_SWAP: u32 = 1;
/// Byte sequence indexing diagnostic.
pub const BOUNDS_BYTES_INDEX: u32 = 2;
/// Unicode scalar indexing diagnostic.
pub const BOUNDS_STRING_INDEX: u32 = 3;
/// Vector or byte sequence range diagnostic.
pub const BOUNDS_SLICE: u32 = 4;
/// Unicode scalar range diagnostic.
pub const BOUNDS_STRING_SLICE: u32 = 5;
/// Open-ended Unicode scalar range diagnostic.
pub const BOUNDS_STRING_SLICE_FROM: u32 = 6;
/// General string range diagnostic.
pub const BOUNDS_STRING_RANGE: u32 = 7;
/// Missing map index diagnostic.
pub const BOUNDS_MAP_INDEX: u32 = 8;

/// Create an owned checked bounds fault with its operation detail.
#[no_mangle]
#[must_use]
pub extern "C" fn hew_fault_new_bounds(
    kind: u32,
    start: i64,
    end: i64,
    length: i64,
) -> *mut HewFault {
    let detail = match kind {
        BOUNDS_VECTOR_SWAP => "vector swap index out of bounds".to_owned(),
        BOUNDS_MAP_INDEX => "map key not found".to_owned(),
        BOUNDS_STRING_RANGE => "string range out of bounds".to_owned(),
        BOUNDS_SLICE => format!("slice {start}..{end} out of bounds for length {length}"),
        BOUNDS_STRING_SLICE => {
            format!("string slice {start}..{end} out of bounds for length {length}")
        }
        BOUNDS_STRING_SLICE_FROM => {
            format!("string slice {start}.. out of bounds for length {length}")
        }
        _ => {
            let sequence = match kind {
                BOUNDS_BYTES_INDEX => "bytes",
                BOUNDS_STRING_INDEX => "string",
                _ => "vector",
            };
            format!("{sequence} index {start} out of bounds for length {length}")
        }
    };
    Box::into_raw(Box::new(HewFault::with_message(
        crate::internal::types::HEW_TRAP_INDEX_OUT_OF_BOUNDS,
        detail,
    )))
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

/// Copy one assertion's human report and rendered comparison values into an
/// owned fault. The four input strings remain caller-owned.
///
/// # Safety
/// Every non-null pointer must reference a live managed string for this call.
#[no_mangle]
#[must_use]
pub unsafe extern "C" fn hew_fault_new_assertion(
    message: *const HewString,
    operator: *const HewString,
    left: *const HewString,
    right: *const HewString,
) -> *mut HewFault {
    // SAFETY: the caller retains all four borrowed managed strings until return.
    let (message, operator, left, right) = unsafe {
        (
            string_as_str(message),
            string_as_str(operator),
            string_as_str(left),
            string_as_str(right),
        )
    };
    Box::into_raw(Box::new(HewFault::new_with_assertion(
        HEW_TRAP_USER_PANIC,
        Some(message.into()),
        Some(AssertionOperands {
            operator: operator.to_owned(),
            left: left.to_owned(),
            right: right.to_owned(),
        }),
    )))
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
    hew_cabi::string::string_from_str(&fault.recovery_message())
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

/// Attach the source site that raised a newly created fault, then raise it.
///
/// Codegen calls this immediately after constructing the fault, in the Hew
/// function that failed, so the raise happens at the origin with the site
/// known.
///
/// # Safety
/// `fault` must be a live uniquely owned fault without concurrent readers.
#[no_mangle]
pub unsafe extern "C" fn hew_fault_set_site(fault: *mut HewFault, offset: u32) {
    // SAFETY: the executable's source tables remain loaded for the process.
    unsafe { set_site(fault, offset, crate::symtab_abi::resolve_site(offset), None) };
}

/// Attach a site using the tables of the image that raised the fault.
///
/// # Safety
/// `fault` is a live unique owner. The table pointers are complete compiler
/// tables, and their image remains loaded for the duration of this call.
#[no_mangle]
pub unsafe extern "C" fn hew_fault_set_site_in_image(
    fault: *mut HewFault,
    offset: u32,
    files: *const u8,
    sites: *const u8,
    symbols: *const u8,
) {
    // SAFETY: the caller retains the originating image and its tables.
    let position = unsafe { crate::symtab_abi::resolve_site_in(files, sites, offset) };
    // SAFETY: the caller supplies the unique live fault and its image tables.
    unsafe { set_site(fault, offset, position, Some((files, symbols))) };
}

unsafe fn set_site(
    fault: *mut HewFault,
    offset: u32,
    position: Option<crate::symtab_abi::SourcePosition>,
    image: Option<(*const u8, *const u8)>,
) {
    // SAFETY: the caller retains a unique live owner for this operation.
    let Some(fault) = (unsafe { fault.as_ref() }) else {
        return;
    };
    if let Some(record) = &fault.primary.record {
        let mut record = record.lock_or_recover();
        record.site.offset = offset;
        if let Some((files, symbols)) = image {
            let frames = fault.primary.image_frames.get_or_init(|| {
                let mut names = Vec::new();
                if names.try_reserve_exact(record.frames().len()).is_err() {
                    return names;
                }
                for frame in record.frames() {
                    // A return address is one past the actual call.
                    // SAFETY: the caller retains this image's complete tables.
                    let function = unsafe {
                        crate::symtab_abi::function_at_in(
                            files,
                            symbols,
                            frame.pc.saturating_sub(1),
                        )
                    };
                    let owned = function.and_then(|function| {
                        // SAFETY: table text remains live during this copy.
                        let name = unsafe { copy_image_text(function.name) }?;
                        // SAFETY: table text remains live during this copy.
                        let file = function
                            .position
                            .and_then(|position| unsafe { copy_image_text(position.file) });
                        let line = function.position.map_or(0, |position| position.line);
                        Some(OwnedFrameText { name, file, line })
                    });
                    names.push(owned);
                }
                names
            });
            for (frame, text) in record.frames.iter_mut().zip(frames) {
                if let Some(text) = text {
                    frame.hew_fn = text.name.as_ptr();
                    frame.file = text
                        .file
                        .as_ref()
                        .map_or(std::ptr::null(), |file| file.as_ptr());
                    frame.line = text.line;
                }
            }
        }
        if let Some(position) = position {
            // SAFETY: the source image remains live through this call.
            let owned = fault
                .primary
                .site_file
                .get_or_init(|| unsafe { copy_image_text(position.file) });
            record.site.file = owned
                .as_ref()
                .map_or(std::ptr::null(), |name| name.as_ptr());
            record.site.module_id = position.module_id;
            record.site.line = position.line;
            record.site.col = position.col;
        }
    }
    fault.primary.raise();
    // Capture at the origin while every participating image is still live.
    // SAFETY: generated code is executing in the image which supplied the site.
    let _ = unsafe { fault.snapshot() };
}

// ── The raise point ──────────────────────────────────────────────────────

/// What to do when a failure is raised (`HEW_ON_FAULT`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OnFault {
    /// Report it when it reaches its terminal observer and unwind as usual.
    Report,
    /// Report it now and abort at the origin, so a core keeps the stack.
    Abort,
    /// Stop in an attached debugger at the origin, then continue as `Report`.
    Trap,
}

#[derive(Debug)]
struct Policy {
    on_fault: OnFault,
    failure_log: Option<Mutex<std::fs::File>>,
}

/// The failure policy, read from the environment once.
fn policy() -> &'static Policy {
    static POLICY: OnceLock<Policy> = OnceLock::new();
    POLICY.get_or_init(|| {
        let on_fault = match std::env::var("HEW_ON_FAULT").as_deref() {
            Ok("abort") => OnFault::Abort,
            Ok("trap") => OnFault::Trap,
            _ => OnFault::Report,
        };
        if let Some(level) = std::env::var("HEW_BACKTRACE")
            .ok()
            .as_deref()
            .and_then(Backtrace::parse)
        {
            level.set();
        }
        let failure_log = std::env::var_os("HEW_FAILURE_LOG").and_then(|path| {
            match std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&path)
            {
                Ok(file) => Some(Mutex::new(file)),
                Err(error) => {
                    crate::output::write(
                        crate::output::Stream::Err,
                        format!(
                            "hew: cannot open HEW_FAILURE_LOG {}: {error}\n",
                            std::path::Path::new(&path).display()
                        )
                        .as_bytes(),
                    );
                    None
                }
            }
        });
        crate::failure_record::prime_build_info();
        // Start the process clock that `/api/crashes` times are relative to.
        let _ = crate::monotonic::monotonic_ns();
        Policy {
            on_fault,
            failure_log,
        }
    })
}

/// Read the failure policy and image facts before any failure needs them.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn configure_from_env() {
    let _ = policy();
    crate::debug_accessors::retain();
}

/// A record of `code` with everything known at its origin: the stack, the
/// running actor, the thread and the time.
#[inline(never)]
fn origin_record(code: i32) -> Box<FailureRecord> {
    let _ = policy();
    crate::debug_accessors::retain();
    let mut record = Box::new(FailureRecord::new(code));
    crate::failure_record::capture_frames(&mut record);
    record.wall_time_ns = crate::failure_record::wall_time_ns();
    record.build = crate::failure_record::build_info();
    if let Some(name) = std::thread::current().name() {
        crate::failure_record::copy_name(&mut record.thread, name);
    }
    crate::crash::describe_current_actor(&mut record);
    record
}

/// Raise a failure: publish it as the last failure (`hew_debug_last_fault()`),
/// append it to `HEW_FAILURE_LOG`, then enter [`hew_fault_raise`]. Publishing
/// first means a debugger stopped at the raise already reads the record.
///
/// # Safety
/// `record` points at a record live for this call.
#[allow(
    clippy::inline_always,
    reason = "the raise must stay in the caller so hew_fault_raise is a tail call"
)]
#[inline(always)]
pub(crate) unsafe fn raise_record(record: *const FailureRecord) {
    // SAFETY: forwarded caller contract.
    publish(unsafe { &*record });
    // SAFETY: forwarded caller contract.
    unsafe { hew_fault_raise(record) };
}

/// Publish a raised record to the last-fault slot and the failure log. Kept
/// out of line so the raise that follows stays a tail call.
#[inline(never)]
fn publish(record: &FailureRecord) {
    crate::failure_record::publish_last_fault(record);
    if let Some(log) = &policy().failure_log {
        let line = record.to_ndjson();
        let _ = log.lock_or_recover().write_all(line.as_bytes());
    }
}

/// The one point every failure passes through at its origin, after it is
/// published; applies `HEW_ON_FAULT`.
///
/// A debugger stops here with `break hew_fault_raise`: the caller is the
/// failing code and `*record`, the same record `hew_debug_last_fault()`
/// returns, describes the failure.
///
/// # Safety
/// `record` is null or points at a record live for this call.
#[no_mangle]
#[inline(never)]
pub unsafe extern "C" fn hew_fault_raise(record: *const FailureRecord) {
    // SAFETY: forwarded caller contract.
    let Some(record) = (unsafe { record.as_ref() }) else {
        return;
    };
    let policy = policy();
    match policy.on_fault {
        OnFault::Report => {}
        OnFault::Abort => {
            print_record(record, None);
            crate::output::flush();
            // JUSTIFIED: abort-on-fault is the requested debugging mode; the
            // core it leaves keeps the failing stack intact.
            std::process::abort();
        }
        OnFault::Trap => {
            if crate::failure_record::tracer_attached() {
                breakpoint();
            }
        }
    }
}

/// Stop in the attached debugger.
fn breakpoint() {
    #[cfg(unix)]
    // SAFETY: raising SIGTRAP with a debugger attached stops the process there.
    unsafe {
        libc::raise(libc::SIGTRAP);
    }
    #[cfg(windows)]
    {
        #[link(name = "kernel32")]
        unsafe extern "system" {
            fn DebugBreak();
        }
        // SAFETY: called only while a debugger is attached.
        unsafe { DebugBreak() };
    }
}

/// Render one record to stderr in a single write.
fn print_record(record: &FailureRecord, message: Option<&str>) {
    let mut out = Vec::new();
    write_record(record, message, false, &mut out);
    crate::output::write(crate::output::Stream::Err, &out);
}

/// Append the rendering of `record` to `out`. `message` replaces the inline
/// copy, which is truncated, when the full text is still at hand.
fn write_record(record: &FailureRecord, message: Option<&str>, secondary: bool, out: &mut Vec<u8>) {
    let mut record = *record;
    // Splice the text back in when the inline copy cannot show it: longer
    // than the capacity, or an explicitly empty message (still `": "`).
    let message = message.filter(|message| {
        message.is_empty() || message.len() > crate::failure_record::MESSAGE_CAPACITY
    });
    if message.is_some() {
        record.message_len = 0;
    }
    let start = out.len();
    out.resize(start + 16 * 1024, 0);
    let len = crate::failure_record::format_with(
        &record,
        Backtrace::current(),
        secondary,
        &mut out[start..],
    );
    out.truncate(start + len);
    if let Some(message) = message {
        // Splice the full message into the first line, before its newline.
        let first_line_end = out[start..]
            .iter()
            .position(|&byte| byte == b'\n')
            .map_or(out.len(), |offset| start + offset);
        let mut tail = out.split_off(first_line_end);
        out.extend_from_slice(b": ");
        out.extend_from_slice(message.as_bytes());
        out.append(&mut tail);
    }
}

type PendingCrashCause = (Box<FailureRecord>, bool, Option<HewFault>);

thread_local! {
    /// The failure an actor crash on this thread is about to record: the
    /// record and whether it already reached stderr.
    static CRASH_CAUSE: std::cell::RefCell<Option<PendingCrashCause>> =
        const { std::cell::RefCell::new(None) };
}

/// Raise a failure that a raw trap reports by code alone (the trap bridge).
/// The record is built here, at the origin, and kept for the report or the
/// actor crash that follows on this thread.
pub(crate) fn raise_trap(code: i32) {
    let record = origin_record(code);
    let mut named = record;
    crate::failure_record::name_frames(&mut named);
    // SAFETY: the record is live for the call.
    unsafe { raise_record(&raw const *named) };
    CRASH_CAUSE.with(|cause| *cause.borrow_mut() = Some((named, false, None)));
}

/// The failure behind an actor crash of `code` on this thread, and whether it
/// was already printed. `None` when the crash had no raised failure.
pub(crate) fn take_crash_cause(code: i32) -> Option<(FailureRecord, bool, Option<HewFault>)> {
    CRASH_CAUSE
        .with(|cause| cause.borrow_mut().take())
        .filter(|(record, _, _)| record.code == code)
        .map(|(record, printed, owner)| (*record, printed, owner))
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
    fault.raise();
    // Unlike eprintln!, an output error must not panic across this C boundary.
    let mut report = crate::output::Report::new(crate::output::Stream::Err);
    let failed = write_unreported(fault, &mut report).is_err();
    let context = crate::execution_context::current_context();
    if !failed && !report.is_empty() && !context.is_null() {
        // SAFETY: the installed context remains live throughout this report.
        let target = unsafe { (*context).actor_link_target };
        let _ = report.write_all(crate::crash::remote_origin(target).as_bytes());
    }
    if !crate::actor::hew_actor_self().is_null() {
        // The actor crash that follows records this failure without
        // printing it a second time.
        let record = Box::new(fault.primary.report_record());
        CRASH_CAUSE
            .with(|cause| *cause.borrow_mut() = Some((record, !failed, Some(fault.clone()))));
    }
    i32::from(failed)
}

/// Report the process entry's terminal fault and return its process exit code.
/// Requested shutdown cancellation exits successfully when cleanup also succeeds.
/// Actor faults use [`hew_fault_report`] and cannot satisfy `#[should_panic]`
/// merely because a test later exits non-zero with crash debt.
///
/// # Safety
/// `fault` is a live borrow for this call, without concurrent release.
#[no_mangle]
pub unsafe extern "C" fn hew_fault_report_entry(fault: *const HewFault) -> i32 {
    // SAFETY: the caller retains a live immutable fault through this report.
    if let Some(fault) = unsafe { fault.as_ref() } {
        if cancelled_by_shutdown(fault.code()) && fault.secondary.is_empty() {
            return 0;
        }
        note_terminal(fault);
    }
    // SAFETY: forwarded unchanged from the caller.
    let _ = unsafe { hew_fault_report(fault) };
    1
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
    fault.raise();
    if crate::actor::hew_actor_self().is_null() {
        note_terminal(&fault);
    }
    let _ = write_report(
        &fault,
        &mut crate::output::Report::new(crate::output::Stream::Err),
    );
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
    let (record, _owner) = if let Some((record, _, owner)) = take_crash_cause(code) {
        (record, owner)
    } else {
        let mut record = origin_record(code);
        crate::failure_record::name_frames(&mut record);
        // SAFETY: the record is live for the call.
        unsafe { raise_record(&raw const *record) };
        (*record, None)
    };
    crate::test_report::note_terminal_failure(&record, None, None);
    print_record(&record, None);
}

/// Hand a terminal fault to the `hew test` report.
fn note_terminal(fault: &HewFault) {
    let primary = fault.primary.as_ref();
    crate::test_report::note_terminal_failure(
        &primary.report_record(),
        primary.message.as_deref(),
        primary.assertion.as_ref(),
    );
}

/// Report a `fails` error that escaped the program entry and return the
/// process status. The record has kind `Failed`, the error's display text and
/// the site of the propagation that let it escape.
///
/// # Safety
/// `message` is null or a live managed string for this call.
#[no_mangle]
pub unsafe extern "C" fn hew_fault_report_entry_error(message: *const HewString, site: u32) -> i32 {
    // SAFETY: the caller supplies a live length-carrying UTF-8 string borrow.
    let message = unsafe { string_as_str(message) };
    let fault = HewFault::new(
        crate::internal::types::HEW_TRAP_FAILED,
        Some(message.into()),
    );
    // SAFETY: `fault` is a live unique local owner.
    unsafe { hew_fault_set_site((&raw const fault).cast_mut(), site) };
    let mut report = crate::output::Report::new(crate::output::Stream::Err);
    let _ = write_unreported(&fault, &mut report);
    1
}

fn fault_reason(code: i32) -> &'static str {
    crate::failure_record::kind_cstr(code)
        .to_str()
        .expect("kind names are ASCII")
}

fn write_diagnostic(
    diagnostic: &FaultDiagnostic,
    secondary: bool,
    output: &mut impl Write,
) -> io::Result<()> {
    let mut rendered = Vec::new();
    write_record(
        &diagnostic.report_record(),
        diagnostic.message.as_deref(),
        secondary,
        &mut rendered,
    );
    output.write_all(&rendered)
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
    fn cloned_faults_share_owned_source_details() {
        let path = std::ffi::CString::new("module.hew").unwrap();
        let fault = HewFault::with_message(212, "problem".into());
        {
            let mut record = fault.primary.record.as_ref().unwrap().lock_or_recover();
            record.site.file = path.as_ptr();
            record.site.line = 7;
            record.site.col = 3;
        }
        let clone = fault.clone();
        // SAFETY: the source text remains live through these calls.
        let first = unsafe { fault.snapshot() }.unwrap();
        // SAFETY: the source text remains live through this call as well.
        let second = unsafe { clone.snapshot() }.unwrap();
        assert!(Arc::ptr_eq(&first, &second));
        drop(path);
        drop(fault);
        drop(clone);
        assert_eq!(first.site.as_ref().unwrap().file, "module.hew");
        assert_eq!(first.message.as_deref(), Some("problem"));
    }

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
        let text = unsafe { string_as_str(message) };
        assert_eq!(text, "primary\0é; secondary failure: UserPanic: cleanup 雪");
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

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn remote_cleanup_diagnostic_is_bounded_and_already_reported() {
        let text = "雪".repeat(crate::envelope::MAX_TERMINAL_FAULT_MESSAGE_BYTES);
        let local = HewFault::with_message(212, text);
        let (code, message) = local.remote_payload();
        assert_eq!(code, 212);
        assert!(message.len() <= crate::envelope::MAX_TERMINAL_FAULT_MESSAGE_BYTES);
        assert!(message.ends_with(" [truncated]"));
        let remote = HewFault::from_remote(code, message.clone());
        let mut output = Vec::new();
        write_unreported(&remote, &mut output).unwrap();
        assert!(
            output.is_empty(),
            "the origin already printed this diagnostic"
        );
        write_report(&remote, &mut output).unwrap();
        assert!(std::str::from_utf8(&output).unwrap().contains(&message));
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
