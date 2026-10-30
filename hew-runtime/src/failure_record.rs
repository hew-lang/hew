//! The one description of a failure.
//!
//! A [`FailureRecord`] is built where the failure happens and every report is a
//! projection of it: the stderr diagnostic, the crash ring behind
//! `/api/crashes`, the NDJSON failure log, the crash metrics and the record a
//! debugger reads through `hew_debug_last_fault()`.
//!
//! The record is `#[repr(C)]`, fixed-size and holds no heap pointers. Text the
//! runtime owns is copied inline; names that live in the binary's read-only data
//! (kind names, source files, Hew function names) are NUL-terminated pointers,
//! so a debugger reading a core file sees the whole record without calling into
//! the process. [`format_into`] renders it into a caller buffer without
//! allocating, locking or touching thread-locals, so the signal path can use it.

use std::ffi::{c_char, CStr};
use std::sync::atomic::{AtomicU8, Ordering};

use crate::failure_kinds;

/// Layout version of [`FailureRecord`]; bumped whenever a field moves.
pub const FAILURE_RECORD_VERSION: u32 = 1;
/// Leading bytes of every record, so a core reader can recognize one.
pub const FAILURE_RECORD_MAGIC: [u8; 8] = *b"HEWFAIL\0";
/// Bytes of failure message kept inline; longer messages are truncated.
pub const MESSAGE_CAPACITY: usize = 256;
/// Frames captured at the origin.
pub const MAX_FRAMES: usize = 48;
/// Bytes kept for each inline name.
pub const NAME_CAPACITY: usize = 64;
/// Bytes kept for the supervisor role path.
pub const ROLE_PATH_CAPACITY: usize = 128;
/// Bytes kept for the failing thread's name.
pub const THREAD_NAME_CAPACITY: usize = 32;
/// Bytes kept for the image build id.
pub const BUILD_ID_CAPACITY: usize = 32;

/// Where in the source the failure was raised.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FailureSite {
    /// Source file, NUL-terminated in the binary's file table; null if unknown.
    pub file: *const c_char,
    /// Module the site belongs to.
    pub module_id: u32,
    /// One-based line, zero if unknown.
    pub line: u32,
    /// One-based column, zero if unknown.
    pub col: u32,
    /// The site value codegen attached to the fault, `u32::MAX` if none.
    pub offset: u32,
}

/// One return address captured at the origin.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FailureFrame {
    /// Return address (the faulting address for frame zero of a signal).
    pub pc: usize,
    /// Hew function containing `pc`, NUL-terminated; null for runtime frames.
    pub hew_fn: *const c_char,
    /// Source file of `pc`, NUL-terminated; null if unknown.
    pub file: *const c_char,
    /// One-based line of `pc`, zero if unknown.
    pub line: u32,
    /// Consecutive frames at this `pc` the entry stands for: deep recursion
    /// collapses into one entry. Zero in an unused slot.
    pub repeat: u32,
}

/// The actor that was running when the failure was raised.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FailureActor {
    /// Actor id; zero outside an actor turn.
    pub id: u64,
    /// Full spawn serial of the incarnation.
    pub spawn_serial: u64,
    /// The incarnation this actor replaced after a restart, or zero.
    pub previous_id: u64,
    /// Spawn serial of `previous_id`.
    pub previous_spawn_serial: u64,
    /// Message type of the turn, or zero.
    pub msg_type: i32,
    /// Hew actor type, NUL-padded.
    pub actor_type: [u8; NAME_CAPACITY],
    /// Qualified handler name, NUL-padded.
    pub handler: [u8; NAME_CAPACITY],
}

/// Facts about the executable, identical for every record of a process.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FailureBuild {
    /// Load address of the executable image.
    pub image_base: usize,
    /// GNU build id, Mach-O `LC_UUID` or PE `CodeView` GUID and age.
    pub build_id: [u8; BUILD_ID_CAPACITY],
    /// Valid bytes in `build_id`.
    pub build_id_len: u32,
    /// The compiler emitted debug tables for this image.
    pub has_debug_info: u8,
    /// Runtime version, NUL-terminated.
    pub hew_version: *const c_char,
}

/// Hardware detail of a failure that arrived as a signal or exception.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FailureSignal {
    /// Signal number (or Windows exception code), zero for logical failures.
    pub signal: i32,
    /// `si_code`.
    pub si_code: i32,
    /// Faulting address.
    pub fault_addr: usize,
    /// The address fell in a stack guard region.
    pub guard_hit: u8,
}

/// One failure, described once at its origin.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FailureRecord {
    /// [`FAILURE_RECORD_MAGIC`].
    pub magic: [u8; 8],
    /// [`FAILURE_RECORD_VERSION`].
    pub version: u32,
    /// Failure code (a trap code from the generated kind list, or a signal).
    pub code: i32,
    /// Kind name, NUL-terminated static text.
    pub kind: *const c_char,
    /// Valid bytes in `message`.
    pub message_len: u32,
    /// Failure message, truncated at a character boundary.
    pub message: [u8; MESSAGE_CAPACITY],
    /// Source site of the failure.
    pub site: FailureSite,
    /// Valid entries in `frames`.
    pub frame_count: u32,
    /// Frames walked after `frames` filled.
    pub frames_omitted: u32,
    /// Return addresses from the origin outward.
    pub frames: [FailureFrame; MAX_FRAMES],
    /// Name of the failing thread, NUL-padded.
    pub thread: [u8; THREAD_NAME_CAPACITY],
    /// Scheduler worker index plus one, zero off the worker pool.
    pub worker_id: u32,
    /// The actor whose turn failed.
    pub actor: FailureActor,
    /// Supervisor role path of the actor, NUL-padded.
    pub supervisor: [u8; ROLE_PATH_CAPACITY],
    /// Wall-clock time of the failure, nanoseconds since the Unix epoch.
    pub wall_time_ns: u64,
    /// Image facts.
    pub build: FailureBuild,
    /// Hardware detail; all zero for logical failures.
    pub signal: FailureSignal,
}

// SAFETY: every raw pointer in a record refers to immutable static data (kind
// names, the compiler's file and symbol tables, the version string); the
// record owns no heap state and is plain data across threads.
unsafe impl Send for FailureRecord {}
// SAFETY: as above; shared reads of static text are race-free.
unsafe impl Sync for FailureRecord {}
// SAFETY: the version pointer refers to static text.
unsafe impl Send for FailureBuild {}
// SAFETY: as above.
unsafe impl Sync for FailureBuild {}

const EMPTY_FRAME: FailureFrame = FailureFrame {
    pc: 0,
    hew_fn: std::ptr::null(),
    file: std::ptr::null(),
    line: 0,
    repeat: 0,
};

impl FailureRecord {
    /// A record with no failure in it.
    pub const EMPTY: Self = Self {
        magic: FAILURE_RECORD_MAGIC,
        version: FAILURE_RECORD_VERSION,
        code: 0,
        kind: failure_kinds::KIND_UNKNOWNFAULT_CSTR.as_ptr(),
        message_len: 0,
        message: [0; MESSAGE_CAPACITY],
        site: FailureSite {
            file: std::ptr::null(),
            module_id: 0,
            line: 0,
            col: 0,
            offset: u32::MAX,
        },
        frame_count: 0,
        frames_omitted: 0,
        frames: [EMPTY_FRAME; MAX_FRAMES],
        thread: [0; THREAD_NAME_CAPACITY],
        worker_id: 0,
        actor: FailureActor {
            id: 0,
            spawn_serial: 0,
            previous_id: 0,
            previous_spawn_serial: 0,
            msg_type: 0,
            actor_type: [0; NAME_CAPACITY],
            handler: [0; NAME_CAPACITY],
        },
        supervisor: [0; ROLE_PATH_CAPACITY],
        wall_time_ns: 0,
        build: FailureBuild {
            image_base: 0,
            build_id: [0; BUILD_ID_CAPACITY],
            build_id_len: 0,
            has_debug_info: 0,
            hew_version: HEW_VERSION.as_ptr(),
        },
        signal: FailureSignal {
            signal: 0,
            si_code: 0,
            fault_addr: 0,
            guard_hit: 0,
        },
    };

    /// A record of `code` with its kind name filled in.
    #[must_use]
    pub fn new(code: i32) -> Self {
        Self {
            code,
            kind: kind_cstr(code).as_ptr(),
            ..Self::EMPTY
        }
    }

    /// The kind name.
    #[must_use]
    pub fn kind_name(&self) -> &str {
        // SAFETY: `kind` always points at static NUL-terminated ASCII.
        unsafe { static_str(self.kind) }.unwrap_or(failure_kinds::KIND_UNKNOWNFAULT)
    }

    /// The message bytes kept inline.
    #[must_use]
    pub fn message_bytes(&self) -> &[u8] {
        &self.message[..(self.message_len as usize).min(MESSAGE_CAPACITY)]
    }

    /// Copy `message`, truncated to [`MESSAGE_CAPACITY`] at a character boundary.
    pub fn set_message(&mut self, message: &str) {
        let len = copy_truncated(&mut self.message, message);
        self.message_len = u32::try_from(len).unwrap_or(u32::MAX);
    }

    /// The captured frames.
    #[must_use]
    pub fn frames(&self) -> &[FailureFrame] {
        &self.frames[..(self.frame_count as usize).min(MAX_FRAMES)]
    }

    /// Append the next outer frame, folding a repeat of the previous `pc`
    /// into its count. Async-signal-safe.
    pub fn push_frame(&mut self, pc: usize) {
        let count = (self.frame_count as usize).min(MAX_FRAMES);
        if let Some(last) = self.frames[..count].last_mut() {
            if last.pc == pc {
                last.repeat = last.repeat.saturating_add(1);
                return;
            }
        }
        if count == MAX_FRAMES {
            self.frames_omitted = self.frames_omitted.saturating_add(1);
            return;
        }
        self.frames[count] = FailureFrame {
            pc,
            repeat: 1,
            ..EMPTY_FRAME
        };
        self.frame_count += 1;
    }

    /// Forget every captured frame.
    pub fn clear_frames(&mut self) {
        self.frame_count = 0;
        self.frames_omitted = 0;
    }

    /// The source file of the site, if the binary's tables named one.
    #[must_use]
    pub fn site_file(&self) -> Option<&'static str> {
        // SAFETY: a non-null site file points into the static file table.
        unsafe { static_str(self.site.file) }
    }

    /// Render the record as one NDJSON line (with its trailing newline).
    #[must_use]
    pub fn to_ndjson(&self) -> String {
        let frames: Vec<serde_json::Value> = self
            .frames()
            .iter()
            .map(|frame| {
                serde_json::json!({
                    "pc": format!("{:#x}", frame.pc),
                    // SAFETY: frame names point into the static symbol table.
                    "hew_fn": unsafe { static_str(frame.hew_fn) },
                    // SAFETY: frame files point into the static file table.
                    "file": unsafe { static_str(frame.file) },
                    "line": (frame.line != 0).then_some(frame.line),
                    "repeat": frame.repeat,
                })
            })
            .collect();
        let build_id =
            hex(&self.build.build_id[..(self.build.build_id_len as usize).min(BUILD_ID_CAPACITY)]);
        let mut value = serde_json::json!({
            "version": self.version,
            "kind": self.kind_name(),
            "code": self.code,
            "message": String::from_utf8_lossy(self.message_bytes()),
            "site": {
                "file": self.site_file(),
                "module_id": self.site.module_id,
                "line": (self.site.line != 0).then_some(self.site.line),
                "col": (self.site.col != 0).then_some(self.site.col),
                "offset": (self.site.offset != u32::MAX).then_some(self.site.offset),
            },
            "frames": frames,
            "frames_omitted": self.frames_omitted,
            "thread": padded_str(&self.thread),
            "worker_id": (self.worker_id != 0).then(|| self.worker_id - 1),
            "actor": (self.actor.id != 0).then(|| serde_json::json!({
                "id": self.actor.id,
                "spawn_serial": self.actor.spawn_serial,
                "type": padded_str(&self.actor.actor_type),
                "handler": padded_str(&self.actor.handler),
                "msg_type": self.actor.msg_type,
                "previous_id": (self.actor.previous_id != 0).then_some(self.actor.previous_id),
                "previous_spawn_serial": (self.actor.previous_id != 0)
                    .then_some(self.actor.previous_spawn_serial),
            })),
            "supervisor": Some(padded_str(&self.supervisor)).filter(|path| !path.is_empty()),
            "wall_time_ns": self.wall_time_ns,
            "build": {
                "image_base": format!("{:#x}", self.build.image_base),
                "build_id": Some(build_id).filter(|id| !id.is_empty()),
                "has_debug_info": self.build.has_debug_info != 0,
                // SAFETY: the version always points at static text.
                "hew_version": unsafe { static_str(self.build.hew_version) },
            },
        });
        if self.signal.signal != 0 {
            value["signal"] = serde_json::json!({
                "signal": self.signal.signal,
                "si_code": self.signal.si_code,
                "fault_addr": format!("{:#x}", self.signal.fault_addr),
                "guard_hit": self.signal.guard_hit != 0,
            });
        }
        let mut line = value.to_string();
        line.push('\n');
        line
    }
}

/// The runtime version every record carries.
pub static HEW_VERSION: &CStr =
    match CStr::from_bytes_with_nul(concat!(env!("CARGO_PKG_VERSION"), "\0").as_bytes()) {
        Ok(version) => version,
        Err(_) => c"unknown",
    };

/// Wall-clock time now, nanoseconds since the Unix epoch.
#[must_use]
pub fn wall_time_ns() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |elapsed| {
            u64::try_from(elapsed.as_nanos()).unwrap_or(u64::MAX)
        })
}

/// The NUL-terminated kind name of any failure code.
#[must_use]
pub fn kind_cstr(code: i32) -> &'static CStr {
    if let Some(name) = failure_kinds::kind_cstr(code) {
        return name;
    }
    match code {
        crate::fault::HEW_FAULT_CANCELLED | crate::fault::HEW_FAULT_RACE_LOST => {
            failure_kinds::KIND_CANCELLED_CSTR
        }
        crate::fault::HEW_FAULT_DEADLINE => failure_kinds::KIND_DEADLINE_CSTR,
        _ => failure_kinds::KIND_UNKNOWNFAULT_CSTR,
    }
}

/// Read static NUL-terminated text.
///
/// # Safety
/// `text` is null or points at NUL-terminated data that lives forever.
unsafe fn static_str(text: *const c_char) -> Option<&'static str> {
    if text.is_null() {
        return None;
    }
    // SAFETY: forwarded caller contract.
    unsafe { CStr::from_ptr(text) }.to_str().ok()
}

/// The UTF-8 text of a NUL-padded inline buffer.
#[must_use]
pub fn padded_str(buffer: &[u8]) -> &str {
    let end = buffer.iter().position(|&b| b == 0).unwrap_or(buffer.len());
    std::str::from_utf8(&buffer[..end]).unwrap_or_default()
}

/// Copy `text` into `buffer`, truncated at a character boundary, NUL-padding
/// the rest. Returns the bytes copied.
pub fn copy_truncated(buffer: &mut [u8], text: &str) -> usize {
    let mut len = text.len().min(buffer.len());
    while !text.is_char_boundary(len) {
        len -= 1;
    }
    buffer[..len].copy_from_slice(&text.as_bytes()[..len]);
    buffer[len..].fill(0);
    len
}

/// Copy a name into a NUL-padded buffer, keeping one NUL so C readers stop.
pub fn copy_name(buffer: &mut [u8], text: &str) {
    if let Some((_, room)) = buffer.split_last_mut() {
        copy_truncated(room, text);
        buffer[buffer.len() - 1] = 0;
    }
}

fn hex(bytes: &[u8]) -> String {
    use std::fmt::Write as _;
    bytes.iter().fold(String::new(), |mut out, byte| {
        let _ = write!(out, "{byte:02x}");
        out
    })
}

// ── Reporting policy (HEW_BACKTRACE) ─────────────────────────────────────

/// How much of the stack a report prints.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Backtrace {
    /// The failure line and its actor context only.
    Off = 0,
    /// Hew frames; runtime frames collapse into a count.
    Hew = 1,
    /// Every frame, with its address.
    Full = 2,
    /// Every frame, plus a snapshot of every live actor.
    All = 3,
}

const BACKTRACE_UNSET: u8 = u8::MAX;
static BACKTRACE: AtomicU8 = AtomicU8::new(BACKTRACE_UNSET);

impl Backtrace {
    /// Parse a `HEW_BACKTRACE` value. Unknown text keeps the default.
    #[must_use]
    pub fn parse(value: &str) -> Option<Self> {
        match value {
            "0" => Some(Self::Off),
            "1" => Some(Self::Hew),
            "full" => Some(Self::Full),
            "all" => Some(Self::All),
            _ => None,
        }
    }

    /// The configured level; async-signal-safe.
    #[must_use]
    pub fn current() -> Self {
        match BACKTRACE.load(Ordering::Acquire) {
            1 => Self::Hew,
            2 => Self::Full,
            3 => Self::All,
            BACKTRACE_UNSET if crate::symtab_abi::has_debug_info() => Self::Hew,
            _ => Self::Off,
        }
    }

    /// Set the level for the rest of the process.
    pub fn set(self) {
        BACKTRACE.store(self as u8, Ordering::Release);
    }
}

// ── Async-signal-safe formatting ─────────────────────────────────────────

/// A bounded writer over a caller buffer. Writes past the end are dropped.
struct Cursor<'a> {
    buffer: &'a mut [u8],
    len: usize,
}

impl Cursor<'_> {
    fn bytes(&mut self, bytes: &[u8]) {
        let room = self.buffer.len() - self.len;
        let take = bytes.len().min(room);
        self.buffer[self.len..self.len + take].copy_from_slice(&bytes[..take]);
        self.len += take;
    }

    fn decimal(&mut self, value: u64) {
        let mut digits = [0u8; 20];
        self.bytes(crate::trap_code::fmt_decimal_u64(value, &mut digits));
    }

    fn signed(&mut self, value: i64) {
        let mut digits = [0u8; 20];
        self.bytes(crate::trap_code::fmt_decimal_i64(value, &mut digits));
    }

    fn hex(&mut self, value: u64) {
        const DIGITS: &[u8; 16] = b"0123456789abcdef";
        let mut out = [0u8; 16];
        let mut cursor = out.len();
        let mut rest = value;
        loop {
            cursor -= 1;
            out[cursor] = DIGITS[(rest & 0xf) as usize];
            rest >>= 4;
            if rest == 0 {
                break;
            }
        }
        self.bytes(b"0x");
        self.bytes(&out[cursor..]);
    }

    fn cstr(&mut self, text: *const c_char) {
        if !text.is_null() {
            // SAFETY: record text pointers refer to static NUL-terminated data;
            // `strlen` is async-signal-safe.
            self.bytes(unsafe { CStr::from_ptr(text) }.to_bytes());
        }
    }

    fn padded(&mut self, buffer: &[u8]) {
        let end = buffer.iter().position(|&b| b == 0).unwrap_or(buffer.len());
        self.bytes(&buffer[..end]);
    }
}

/// Render `record` as the user-facing diagnostic into `buffer` and return the
/// bytes written. Never allocates, locks or reads thread-locals, so a signal
/// handler can call it; output that does not fit is truncated.
///
/// ```text
/// hew: failure: DivideByZero (202) at app.hew:10:15
/// hew:   in actor Fragile#1 handler Fragile.bad (thread hew-worker-1)
/// hew:   at Fragile.bad (app.hew:10)
/// hew:   ... 6 runtime frames
/// hew:   build 7f3a... hew 0.10.0
/// ```
pub fn format_into(record: &FailureRecord, buffer: &mut [u8]) -> usize {
    format_with(record, Backtrace::current(), false, buffer)
}

/// [`format_into`] with an explicit backtrace level. `secondary` labels a
/// cleanup failure reported after the primary one.
#[allow(
    clippy::too_many_lines,
    reason = "one straight-line rendering of the record, section by section"
)]
pub fn format_with(
    record: &FailureRecord,
    backtrace: Backtrace,
    secondary: bool,
    buffer: &mut [u8],
) -> usize {
    let mut out = Cursor { buffer, len: 0 };
    out.bytes(if secondary {
        b"hew: secondary failure: "
    } else {
        b"hew: failure: "
    });
    out.cstr(record.kind);
    out.bytes(b" (");
    out.signed(i64::from(record.code));
    out.bytes(b")");
    if !record.site.file.is_null() {
        out.bytes(b" at ");
        out.cstr(record.site.file);
        if record.site.line != 0 {
            out.bytes(b":");
            out.decimal(u64::from(record.site.line));
            if record.site.col != 0 {
                out.bytes(b":");
                out.decimal(u64::from(record.site.col));
            }
        }
    }
    if record.message_len != 0 {
        out.bytes(b": ");
        out.bytes(record.message_bytes());
    }
    out.bytes(b"\n");
    if secondary {
        return out.len;
    }

    if record.signal.signal != 0 {
        let (name, sent) =
            crate::signal::describe_fault(record.signal.signal, record.signal.si_code);
        out.bytes(b"hew:   ");
        out.bytes(name.as_bytes());
        out.bytes(b" (");
        // Windows exception codes read as hex; Unix signal numbers as decimal.
        let code = record.signal.signal.cast_unsigned();
        if code > 0xFFFF {
            out.hex(u64::from(code));
        } else {
            out.decimal(u64::from(code));
        }
        out.bytes(b")");
        if sent {
            out.bytes(b" sent by kill");
        } else {
            out.bytes(b" at ");
            out.hex(record.signal.fault_addr as u64);
            if record.signal.guard_hit != 0 {
                out.bytes(b" (stack guard)");
            }
        }
        // An actor's line below names its thread.
        if record.actor.id == 0 && record.thread[0] != 0 {
            out.bytes(b", thread ");
            out.padded(&record.thread);
        }
        out.bytes(b"\n");
    }

    if record.actor.id != 0 {
        out.bytes(b"hew:   in actor ");
        if record.actor.actor_type[0] == 0 {
            out.bytes(b"Actor");
        } else {
            out.padded(&record.actor.actor_type);
        }
        out.bytes(b"#");
        out.decimal(record.actor.id);
        if record.actor.handler[0] != 0 {
            out.bytes(b" handler ");
            out.padded(&record.actor.handler);
        }
        if record.thread[0] != 0 {
            out.bytes(b" (thread ");
            out.padded(&record.thread);
            out.bytes(b")");
        }
        out.bytes(b"\n");
        if record.supervisor[0] != 0 {
            out.bytes(b"hew:   supervised as ");
            out.padded(&record.supervisor);
            out.bytes(b"\n");
        }
        if record.actor.previous_id != 0 {
            out.bytes(b"hew:   restarted from actor #");
            out.decimal(record.actor.previous_id);
            out.bytes(b"\n");
        }
    }

    if backtrace >= Backtrace::Hew {
        let mut collapsed: u64 = 0;
        for frame in record.frames() {
            let named = !frame.hew_fn.is_null();
            if !named && backtrace == Backtrace::Hew {
                collapsed += u64::from(frame.repeat.max(1));
                continue;
            }
            if collapsed != 0 {
                runtime_frames(&mut out, collapsed);
                collapsed = 0;
            }
            out.bytes(b"hew:   at ");
            if named {
                out.cstr(frame.hew_fn);
            } else {
                out.hex(frame.pc as u64);
            }
            if !frame.file.is_null() {
                out.bytes(b" (");
                out.cstr(frame.file);
                if frame.line != 0 {
                    out.bytes(b":");
                    out.decimal(u64::from(frame.line));
                }
                out.bytes(b")");
            }
            out.bytes(b"\n");
            if frame.repeat > 1 {
                out.bytes(b"hew:     (... ");
                out.decimal(u64::from(frame.repeat - 1));
                out.bytes(b" identical frames)\n");
            }
        }
        if record.frames_omitted != 0 {
            out.bytes(b"hew:   ... ");
            out.decimal(u64::from(record.frames_omitted));
            out.bytes(b" more frames\n");
        }
        // Frames past the last Hew frame are the runtime's own entry; a
        // trailing count says nothing a user can act on.
        out.bytes(b"hew:   build ");
        let id_len = (record.build.build_id_len as usize).min(BUILD_ID_CAPACITY);
        if id_len == 0 {
            out.bytes(b"unknown");
        } else {
            for byte in &record.build.build_id[..id_len.min(8)] {
                const DIGITS: &[u8; 16] = b"0123456789abcdef";
                out.bytes(&[
                    DIGITS[usize::from(byte >> 4)],
                    DIGITS[usize::from(byte & 0xf)],
                ]);
            }
            if id_len > 8 {
                out.bytes(b"...");
            }
        }
        out.bytes(b" hew ");
        out.cstr(record.build.hew_version);
        out.bytes(b"\n");
    }
    out.len
}

fn runtime_frames(out: &mut Cursor<'_>, count: u64) {
    out.bytes(b"hew:   ... ");
    out.decimal(count);
    out.bytes(if count == 1 {
        b" runtime frame\n"
    } else {
        b" runtime frames\n"
    });
}

// ── The last failure ─────────────────────────────────────────────────────

/// Storage for the most recent failure, at a fixed symbol.
#[repr(transparent)]
#[allow(
    missing_debug_implementations,
    reason = "a debugger reads it; Rust code reads it through last_fault"
)]
pub struct LastFault(std::cell::UnsafeCell<FailureRecord>);

// SAFETY: writers serialize on `LAST_FAULT_WRITER`; readers are debuggers and
// [`last_fault`], which copies under the same flag.
unsafe impl Sync for LastFault {}

/// The most recent failure raised in this process. A debugger reads it from a
/// live process or a core without running code; `code == 0` means none.
#[no_mangle]
pub static hew_last_fault_record: LastFault =
    LastFault(std::cell::UnsafeCell::new(FailureRecord::EMPTY));

static LAST_FAULT_WRITER: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Publish `record` as the last failure. Async-signal-safe: a writer that
/// finds another write in progress drops its update rather than wait.
pub fn publish_last_fault(record: &FailureRecord) {
    if LAST_FAULT_WRITER
        .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
        .is_err()
    {
        return;
    }
    // SAFETY: the writer flag gives this thread exclusive access.
    unsafe { *hew_last_fault_record.0.get() = *record };
    LAST_FAULT_WRITER.store(false, Ordering::Release);
}

/// A copy of the last failure, if one was raised.
#[must_use]
pub fn last_fault() -> Option<FailureRecord> {
    while LAST_FAULT_WRITER
        .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
        .is_err()
    {
        std::hint::spin_loop();
    }
    // SAFETY: the writer flag excludes concurrent writes during the copy.
    let record = unsafe { *hew_last_fault_record.0.get() };
    LAST_FAULT_WRITER.store(false, Ordering::Release);
    (record.code != 0).then_some(record)
}

/// The last failure's storage, or null if no failure was raised.
#[must_use]
pub fn last_fault_ptr() -> *const FailureRecord {
    // SAFETY: a racy read of one i32 only decides between two pointers.
    let code = unsafe { (*hew_last_fault_record.0.get()).code };
    if code == 0 {
        std::ptr::null()
    } else {
        hew_last_fault_record.0.get().cast_const()
    }
}

// ── Origin capture ───────────────────────────────────────────────────────

/// Name each captured frame from the compiler's symbol table. Frames outside
/// Hew code keep a null name and collapse in the report. Async-signal-safe.
pub fn name_frames(record: &mut FailureRecord) {
    let count = (record.frame_count as usize).min(MAX_FRAMES);
    let fault = record.signal.signal != 0;
    for (index, frame) in record.frames[..count].iter_mut().enumerate() {
        // A return address points one past the call; look up the call
        // itself. A fault's first frame is the faulting instruction.
        let lookup = if fault && index == 0 {
            frame.pc
        } else {
            frame.pc.saturating_sub(1)
        };
        if let Some(function) = crate::symtab_abi::function_at(lookup) {
            frame.hew_fn = function.name;
            if let Some(position) = function.position {
                frame.file = position.file;
                frame.line = position.line;
            }
        }
    }
}

/// Capture the calling thread's return addresses into `record`, innermost
/// first, skipping the capture machinery itself.
///
/// The runtime is built without frame pointers, so a frame-pointer walk from
/// here would stop at the first runtime frame. The platform unwinder reads the
/// unwind tables both the runtime and Hew code carry instead.
#[inline(never)]
pub fn capture_frames(record: &mut FailureRecord) {
    record.clear_frames();
    #[cfg(unix)]
    {
        let mut addresses = [0usize; MAX_FRAMES];
        // Skip `capture_current` and this function.
        let count = crate::signal::capture_current(2, &mut addresses);
        for pc in &addresses[..count] {
            record.push_frame(*pc);
        }
    }
    #[cfg(windows)]
    {
        let mut addresses = [std::ptr::null_mut::<std::ffi::c_void>(); MAX_FRAMES];
        // SAFETY: the buffer holds MAX_FRAMES pointers; a null hash is allowed.
        let count = unsafe {
            windows_ffi::RtlCaptureStackBackTrace(
                1,
                MAX_FRAMES as u32,
                addresses.as_mut_ptr(),
                std::ptr::null_mut(),
            )
        };
        for address in &addresses[..usize::from(count)] {
            record.push_frame(*address as usize);
        }
    }
}

/// Walk a frame-pointer chain from `fp`, writing return addresses to `out`.
///
/// For callers that hold a frame pointer from a register context (a signal or
/// exception handler), where calling the unwinder is not safe. Each step is
/// validated: a frame must be aligned, inside `stack` (`[low, high)`) and
/// strictly above the previous one. Where the chain leaves a coroutine's
/// stack, `coro_stack_parent` maps the outermost coroutine frame to the frame
/// that resumed it and returns that stack's bounds, so the walk continues
/// across the switch. Returns the frames written.
pub fn walk_frame_pointers(
    mut fp: usize,
    mut stack: (usize, usize),
    coro_stack_parent: impl Fn(usize) -> Option<(usize, (usize, usize))>,
    out: &mut [usize],
) -> usize {
    let word = std::mem::size_of::<usize>();
    let mut count = 0;
    while count < out.len() {
        let valid = fp.is_multiple_of(word)
            && fp >= stack.0
            && fp.checked_add(2 * word).is_some_and(|end| end <= stack.1);
        if !valid {
            match coro_stack_parent(fp) {
                Some((parent, bounds)) if parent != fp => {
                    fp = parent;
                    stack = bounds;
                    continue;
                }
                _ => break,
            }
        }
        // SAFETY: `fp` and the word after it lie inside the stack bounds the
        // caller vouched for and are aligned.
        let (next, ret) = unsafe { (*(fp as *const usize), *((fp + word) as *const usize)) };
        if ret == 0 {
            break;
        }
        out[count] = ret;
        count += 1;
        if next <= fp {
            // A chain must grow towards the stack base; anything else is the
            // end of the chain or corruption. A coroutine boundary may still
            // continue the walk on its parent stack.
            match coro_stack_parent(fp) {
                Some((parent, bounds)) if parent != fp => {
                    fp = parent;
                    stack = bounds;
                }
                _ => break,
            }
            continue;
        }
        fp = next;
    }
    count
}

/// The coroutine boundary hook for [`walk_frame_pointers`]: the frame that
/// resumed the coroutine whose outermost frame is `fp`, with its stack bounds.
/// Coroutine stacks are not yet registered, so a walk ends at the boundary.
#[must_use]
pub fn coro_stack_parent(_fp: usize) -> Option<(usize, (usize, usize))> {
    None
}

// ── Image facts ──────────────────────────────────────────────────────────

static BUILD: std::sync::OnceLock<FailureBuild> = std::sync::OnceLock::new();

/// The image facts every record carries. Computed once outside signal
/// context ([`prime_build_info`]); afterwards reading it is lock-free.
#[must_use]
pub fn build_info() -> FailureBuild {
    BUILD.get().copied().unwrap_or(FailureRecord::EMPTY.build)
}

/// Compute the image facts now, so a later signal-context read finds them.
pub fn prime_build_info() -> FailureBuild {
    *BUILD.get_or_init(|| {
        let mut build = FailureRecord::EMPTY.build;
        build.has_debug_info = u8::from(crate::symtab_abi::has_debug_info());
        image::fill(&mut build);
        build
    })
}

#[cfg(any(target_os = "linux", target_os = "freebsd", target_os = "android"))]
mod image {
    use std::ffi::{c_int, c_void};

    use super::{FailureBuild, BUILD_ID_CAPACITY};

    const NT_GNU_BUILD_ID: u32 = 3;
    /// `PT_NOTE`, which the libc crate does not export on every BSD.
    const PT_NOTE: u32 = 4;

    #[allow(
        clippy::cast_possible_truncation,
        reason = "ELF addresses and sizes of a loaded image fit the native pointer width"
    )]
    unsafe extern "C" fn first_image(
        info: *mut libc::dl_phdr_info,
        _size: libc::size_t,
        data: *mut c_void,
    ) -> c_int {
        // SAFETY: `data` is the `FailureBuild` handed to dl_iterate_phdr and
        // `info` describes a loaded image for the duration of this call.
        let (build, info) = unsafe { (&mut *data.cast::<FailureBuild>(), &*info) };
        build.image_base = info.dlpi_addr as usize;
        for index in 0..usize::from(info.dlpi_phnum) {
            // SAFETY: the program header table has `dlpi_phnum` entries.
            let header = unsafe { &*info.dlpi_phdr.add(index) };
            if header.p_type != PT_NOTE {
                continue;
            }
            let start = (info.dlpi_addr as usize).wrapping_add(header.p_vaddr as usize);
            // SAFETY: a PT_NOTE segment is mapped for its whole memory size.
            let notes =
                unsafe { std::slice::from_raw_parts(start as *const u8, header.p_memsz as usize) };
            if let Some(id) = gnu_build_id(notes) {
                let len = id.len().min(BUILD_ID_CAPACITY);
                build.build_id[..len].copy_from_slice(&id[..len]);
                build.build_id_len = len as u32;
                break;
            }
        }
        // The first image is the executable; stop there.
        1
    }

    /// Find the GNU build id in a run of ELF notes.
    pub(super) fn gnu_build_id(mut notes: &[u8]) -> Option<&[u8]> {
        let word = |bytes: &[u8]| u32::from_ne_bytes(bytes.try_into().expect("4-byte field"));
        let align = |len: usize| (len + 3) & !3;
        while notes.len() >= 12 {
            let name_len = word(&notes[0..4]) as usize;
            let desc_len = word(&notes[4..8]) as usize;
            let kind = word(&notes[8..12]);
            let desc_start = 12 + align(name_len);
            let end = desc_start + align(desc_len);
            if desc_start + desc_len > notes.len() {
                return None;
            }
            if kind == NT_GNU_BUILD_ID && notes[12..12 + name_len] == *b"GNU\0" {
                return Some(&notes[desc_start..desc_start + desc_len]);
            }
            notes = notes.get(end..)?;
        }
        None
    }

    pub(super) fn fill(build: &mut FailureBuild) {
        // SAFETY: the callback only writes the `FailureBuild` it is handed.
        unsafe { libc::dl_iterate_phdr(Some(first_image), std::ptr::from_mut(build).cast()) };
    }
}

#[cfg(target_os = "macos")]
mod image {
    use super::{FailureBuild, BUILD_ID_CAPACITY};

    const LC_UUID: u32 = 0x1b;
    /// `sizeof(struct mach_header_64)`; `ncmds` is the word at byte 16.
    const HEADER_SIZE: usize = 32;
    const NCMDS_OFFSET: usize = 16;

    unsafe extern "C" {
        fn _dyld_get_image_header(index: u32) -> *const u8;
    }

    pub(super) fn fill(build: &mut FailureBuild) {
        // SAFETY: image 0 is the main executable, mapped for the process life.
        let header = unsafe { _dyld_get_image_header(0) };
        if header.is_null() {
            return;
        }
        build.image_base = header as usize;
        // SAFETY: the 64-bit header and the load commands that follow it are
        // mapped; each command starts with its `cmd` and `cmdsize` words.
        unsafe {
            let word = |at: *const u8| std::ptr::read_unaligned(at.cast::<u32>());
            let mut command = header.add(HEADER_SIZE);
            for _ in 0..word(header.add(NCMDS_OFFSET)) {
                if word(command) == LC_UUID {
                    let uuid = std::slice::from_raw_parts(command.add(8), 16);
                    build.build_id[..16.min(BUILD_ID_CAPACITY)].copy_from_slice(uuid);
                    build.build_id_len = 16;
                    return;
                }
                command = command.add(word(command.add(4)) as usize);
            }
        }
    }
}

#[cfg(windows)]
mod image {
    use super::{FailureBuild, BUILD_ID_CAPACITY};

    const IMAGE_DIRECTORY_ENTRY_DEBUG: usize = 6;
    const IMAGE_DEBUG_TYPE_CODEVIEW: u32 = 2;

    pub(super) fn fill(build: &mut FailureBuild) {
        // SAFETY: a null module name returns the executable's mapped image.
        let base = unsafe { super::windows_ffi::GetModuleHandleW(std::ptr::null()) } as usize;
        if base == 0 {
            return;
        }
        build.image_base = base;
        let read32 = |offset: usize| {
            // SAFETY: every offset below stays inside the mapped PE headers
            // and debug directory the loader validated.
            unsafe { std::ptr::read_unaligned((base + offset) as *const u32) }
        };
        let nt = read32(0x3c) as usize;
        // PE32+ optional header: data directories start 112 bytes in.
        let directory = nt + 24 + 112 + IMAGE_DIRECTORY_ENTRY_DEBUG * 8;
        let (rva, size) = (read32(directory) as usize, read32(directory + 4) as usize);
        for entry in (0..size / 28).map(|index| rva + index * 28) {
            if read32(entry + 12) != IMAGE_DEBUG_TYPE_CODEVIEW {
                continue;
            }
            let record = read32(entry + 20) as usize;
            if read32(record) != u32::from_le_bytes(*b"RSDS") {
                continue;
            }
            // GUID (16 bytes) then age (4 bytes): the PDB identity.
            for index in 0..20.min(BUILD_ID_CAPACITY) {
                // SAFETY: as above; the RSDS record holds 20 identity bytes.
                build.build_id[index] = unsafe { *((base + record + 4 + index) as *const u8) };
            }
            build.build_id_len = 20;
            return;
        }
    }
}

#[cfg(not(any(
    target_os = "linux",
    target_os = "freebsd",
    target_os = "android",
    target_os = "macos",
    windows
)))]
mod image {
    pub(super) fn fill(_build: &mut super::FailureBuild) {}
}

#[cfg(windows)]
mod windows_ffi {
    use std::ffi::c_void;

    #[link(name = "kernel32")]
    unsafe extern "system" {
        pub(super) fn RtlCaptureStackBackTrace(
            skip: u32,
            count: u32,
            frames: *mut *mut c_void,
            hash: *mut u32,
        ) -> u16;
        pub(super) fn GetModuleHandleW(name: *const u16) -> *mut c_void;
        pub(super) fn IsDebuggerPresent() -> i32;
    }
}

// ── Debugger presence ────────────────────────────────────────────────────

/// Whether a debugger is attached to this process.
#[must_use]
pub fn tracer_attached() -> bool {
    #[cfg(any(target_os = "linux", target_os = "android"))]
    {
        std::fs::read_to_string("/proc/self/status").is_ok_and(|status| {
            status
                .lines()
                .find_map(|line| line.strip_prefix("TracerPid:"))
                .is_some_and(|pid| pid.trim() != "0")
        })
    }
    #[cfg(target_os = "freebsd")]
    {
        let mut info: libc::kinfo_proc = unsafe_zeroed_kinfo();
        kern_proc_self(&raw mut info, std::mem::size_of::<libc::kinfo_proc>())
            && info.ki_flag & i64::from(libc::P_TRACED) != 0
    }
    #[cfg(target_os = "macos")]
    {
        // The libc crate does not describe Darwin's `struct kinfo_proc`
        // (648 bytes); `kp_proc.p_flag` is the int at byte 32 and
        // `P_TRACED` is 0x800 (<sys/proc.h>).
        const KINFO_PROC_SIZE: usize = 648;
        const P_FLAG_OFFSET: usize = 32;
        const P_TRACED: i32 = 0x800;
        let mut info = [0u8; KINFO_PROC_SIZE];
        kern_proc_self(info.as_mut_ptr(), KINFO_PROC_SIZE) && {
            let flag = i32::from_ne_bytes([
                info[P_FLAG_OFFSET],
                info[P_FLAG_OFFSET + 1],
                info[P_FLAG_OFFSET + 2],
                info[P_FLAG_OFFSET + 3],
            ]);
            flag & P_TRACED != 0
        }
    }
    #[cfg(windows)]
    {
        // SAFETY: no preconditions.
        unsafe { windows_ffi::IsDebuggerPresent() != 0 }
    }
    #[cfg(not(any(
        target_os = "linux",
        target_os = "android",
        target_os = "macos",
        target_os = "freebsd",
        windows
    )))]
    {
        false
    }
}

/// Fill `size` bytes at `info` with this process's `KERN_PROC_PID` record.
#[cfg(any(target_os = "macos", target_os = "freebsd"))]
fn kern_proc_self<T>(info: *mut T, size: usize) -> bool {
    let mut mib = [
        libc::CTL_KERN,
        libc::KERN_PROC,
        libc::KERN_PROC_PID,
        // SAFETY: getpid has no preconditions.
        unsafe { libc::getpid() },
    ];
    let mut len = size;
    // SAFETY: `info` is writable for `size` bytes and sysctl writes at most
    // `len` of them; `mib` and `len` are live locals.
    let status = unsafe {
        libc::sysctl(
            mib.as_mut_ptr(),
            4,
            info.cast(),
            &raw mut len,
            std::ptr::null_mut(),
            0,
        )
    };
    status == 0
}

#[cfg(target_os = "freebsd")]
fn unsafe_zeroed_kinfo() -> libc::kinfo_proc {
    // SAFETY: kinfo_proc is plain data, valid when zeroed.
    unsafe { std::mem::zeroed() }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn render(record: &FailureRecord, backtrace: Backtrace) -> String {
        let mut buffer = [0u8; 4096];
        let len = format_with(record, backtrace, false, &mut buffer);
        String::from_utf8(buffer[..len].to_vec()).unwrap()
    }

    #[test]
    fn a_plain_failure_keeps_the_one_line_report() {
        let mut record = FailureRecord::new(212);
        record.set_message("level3 refuses n > 2");
        assert_eq!(
            render(&record, Backtrace::Off),
            "hew: failure: UserPanic (212): level3 refuses n > 2\n"
        );
        assert_eq!(
            render(&FailureRecord::new(202), Backtrace::Off),
            "hew: failure: DivideByZero (202)\n"
        );
        assert_eq!(
            render(&FailureRecord::new(-7), Backtrace::Off),
            "hew: failure: UnknownFault (-7)\n"
        );
    }

    #[test]
    fn located_actor_failure_names_site_actor_and_hew_frames() {
        let mut record = FailureRecord::new(202);
        record.site.file = c"app.hew".as_ptr();
        record.site.line = 10;
        record.site.col = 15;
        record.actor.id = 1;
        copy_name(&mut record.actor.actor_type, "Fragile");
        copy_name(&mut record.actor.handler, "Fragile.bad");
        copy_name(&mut record.thread, "hew-worker-1");
        copy_name(&mut record.supervisor, "App/fragile");
        record.frames[0] = FailureFrame {
            pc: 0x10,
            ..EMPTY_FRAME
        };
        record.frames[1] = FailureFrame {
            pc: 0x20,
            hew_fn: c"Fragile.bad".as_ptr(),
            file: c"app.hew".as_ptr(),
            line: 10,
            repeat: 1,
        };
        record.frames[2] = FailureFrame {
            pc: 0x30,
            ..EMPTY_FRAME
        };
        record.frame_count = 3;
        record.build.build_id[..10].copy_from_slice(&[0x7f, 0x3a, 1, 2, 3, 4, 5, 6, 7, 8]);
        record.build.build_id_len = 10;
        assert_eq!(
            render(&record, Backtrace::Hew),
            format!(
                "hew: failure: DivideByZero (202) at app.hew:10:15\n\
                 hew:   in actor Fragile#1 handler Fragile.bad (thread hew-worker-1)\n\
                 hew:   supervised as App/fragile\n\
                 hew:   ... 1 runtime frame\n\
                 hew:   at Fragile.bad (app.hew:10)\n\
                 hew:   build 7f3a010203040506... hew {}\n",
                env!("CARGO_PKG_VERSION")
            )
        );
        let full = render(&record, Backtrace::Full);
        assert!(full.contains("hew:   at 0x10\n"), "{full}");
        assert!(full.contains("hew:   at 0x30\n"), "{full}");
    }

    #[test]
    fn signal_detail_is_reported() {
        let mut record = FailureRecord::new(failure_kinds::HEW_TRAP_STACK_OVERFLOW);
        record.signal = FailureSignal {
            signal: 11,
            si_code: 2,
            fault_addr: 0x7ff0,
            guard_hit: 1,
        };
        let (name, _) = crate::signal::describe_fault(11, 2);
        assert_eq!(
            render(&record, Backtrace::Off),
            format!(
                "hew: failure: StackOverflow (215)\nhew:   {name} (11) at 0x7ff0 (stack guard)\n"
            )
        );
    }

    #[test]
    fn formatter_truncates_into_any_buffer_without_panicking() {
        let mut record = FailureRecord::new(212);
        record.set_message(&"雪".repeat(200));
        record.actor.id = 9;
        let complete = render(&record, Backtrace::Full);
        for size in 0..complete.len() {
            let mut buffer = vec![0u8; size];
            let len = format_with(&record, Backtrace::Full, false, &mut buffer);
            assert_eq!(len, size);
            assert_eq!(&buffer[..], &complete.as_bytes()[..size]);
        }
    }

    #[test]
    fn message_truncates_at_a_character_boundary() {
        let mut record = FailureRecord::new(212);
        record.set_message(&"é".repeat(MESSAGE_CAPACITY));
        assert_eq!(record.message_len as usize, MESSAGE_CAPACITY);
        assert!(std::str::from_utf8(record.message_bytes()).is_ok());
        record.set_message(&format!("a{}", "é".repeat(MESSAGE_CAPACITY)));
        assert_eq!(record.message_len as usize, MESSAGE_CAPACITY - 1);
    }

    #[test]
    fn record_layout_is_stable_plain_data() {
        // The record is copied into crash rings and statics and read from
        // cores: it must stay `Copy` and free of heap ownership.
        fn assert_copy<T: Copy>() {}
        assert_copy::<FailureRecord>();
        assert_eq!(std::mem::offset_of!(FailureRecord, magic), 0);
        assert_eq!(FailureRecord::EMPTY.magic, FAILURE_RECORD_MAGIC);
    }

    #[test]
    fn ndjson_round_trips_every_field() {
        let mut record = FailureRecord::new(205);
        record.set_message("vector index 5 out of bounds for length 3");
        record.site.file = c"vec.hew".as_ptr();
        record.site.line = 8;
        record.site.col = 3;
        record.site.offset = 120;
        record.frames[0] = FailureFrame {
            pc: 0xabc,
            hew_fn: c"main".as_ptr(),
            file: c"vec.hew".as_ptr(),
            line: 8,
            repeat: 1,
        };
        record.frame_count = 1;
        record.actor.id = 4;
        record.actor.spawn_serial = 44;
        record.actor.previous_id = 3;
        record.actor.previous_spawn_serial = 33;
        copy_name(&mut record.actor.actor_type, "Store");
        copy_name(&mut record.actor.handler, "Store.get");
        copy_name(&mut record.thread, "hew-worker-0");
        record.worker_id = 1;
        record.wall_time_ns = 1_700_000_000_000_000_000;
        record.build.build_id[..4].copy_from_slice(&[0xde, 0xad, 0xbe, 0xef]);
        record.build.build_id_len = 4;
        record.build.image_base = 0x5555_0000;
        let line = record.to_ndjson();
        assert!(line.ends_with('\n') && line.matches('\n').count() == 1);
        let value: serde_json::Value = serde_json::from_str(&line).unwrap();
        assert_eq!(value["kind"], "IndexOutOfBounds");
        assert_eq!(value["code"], 205);
        assert_eq!(
            value["message"],
            "vector index 5 out of bounds for length 3"
        );
        assert_eq!(value["site"]["file"], "vec.hew");
        assert_eq!(value["site"]["line"], 8);
        assert_eq!(value["site"]["col"], 3);
        assert_eq!(value["site"]["offset"], 120);
        assert_eq!(value["frames"][0]["pc"], "0xabc");
        assert_eq!(value["frames"][0]["hew_fn"], "main");
        assert_eq!(value["frames"][0]["line"], 8);
        assert_eq!(value["thread"], "hew-worker-0");
        assert_eq!(value["worker_id"], 0);
        assert_eq!(value["actor"]["id"], 4);
        assert_eq!(value["actor"]["spawn_serial"], 44);
        assert_eq!(value["actor"]["type"], "Store");
        assert_eq!(value["actor"]["handler"], "Store.get");
        assert_eq!(value["actor"]["previous_id"], 3);
        assert_eq!(value["actor"]["previous_spawn_serial"], 33);
        assert_eq!(value["wall_time_ns"], 1_700_000_000_000_000_000_u64);
        assert_eq!(value["build"]["build_id"], "deadbeef");
        assert_eq!(value["build"]["image_base"], "0x55550000");
        assert_eq!(value["build"]["hew_version"], env!("CARGO_PKG_VERSION"));
        assert!(value.get("signal").is_none());
    }

    #[test]
    fn backtrace_levels_parse() {
        assert_eq!(Backtrace::parse("0"), Some(Backtrace::Off));
        assert_eq!(Backtrace::parse("1"), Some(Backtrace::Hew));
        assert_eq!(Backtrace::parse("full"), Some(Backtrace::Full));
        assert_eq!(Backtrace::parse("all"), Some(Backtrace::All));
        assert_eq!(Backtrace::parse("yes"), None);
    }

    #[test]
    fn every_code_names_a_static_kind() {
        for &(code, name) in failure_kinds::FAILURE_KINDS {
            assert_eq!(FailureRecord::new(code).kind_name(), name);
        }
        assert_eq!(FailureRecord::new(-1).kind_name(), "Cancelled");
        assert_eq!(FailureRecord::new(-2).kind_name(), "Deadline");
        assert_eq!(FailureRecord::new(11).kind_name(), "UnknownFault");
    }

    /// A forged stack: writes go through one raw pointer so the walk's reads
    /// stay valid.
    struct Forged {
        words: Vec<usize>,
    }

    impl Forged {
        fn new(len: usize) -> Self {
            Self {
                words: vec![0; len],
            }
        }
        fn at(&self, index: usize) -> usize {
            self.words.as_ptr() as usize + index * std::mem::size_of::<usize>()
        }
        fn set(&mut self, index: usize, value: usize) {
            self.words[index] = value;
        }
        fn bounds(&self) -> (usize, usize) {
            (self.at(0), self.at(self.words.len()))
        }
    }

    #[test]
    fn frame_pointer_walk_follows_a_forged_chain_and_stops_at_an_invalid_pointer() {
        // Three frames, each `[next fp, return address]`, the last pointing
        // outside the stack.
        let mut stack = Forged::new(16);
        let (f2, f6, f10) = (stack.at(2), stack.at(6), stack.at(10));
        stack.set(2, f6);
        stack.set(3, 0x1111);
        stack.set(6, f10);
        stack.set(7, 0x2222);
        stack.set(10, 0xdead_0000);
        stack.set(11, 0x3333);
        let mut out = [0usize; MAX_FRAMES];
        let count = walk_frame_pointers(f2, stack.bounds(), |_| None, &mut out);
        assert_eq!(&out[..count], &[0x1111, 0x2222, 0x3333]);

        // A backwards pointer ends the chain; so does a misaligned start.
        stack.set(6, f2);
        let count = walk_frame_pointers(f2, stack.bounds(), |_| None, &mut out);
        assert_eq!(&out[..count], &[0x1111, 0x2222]);
        assert_eq!(
            walk_frame_pointers(f2 + 1, stack.bounds(), |_| None, &mut out),
            0
        );

        // The output bound is respected.
        stack.set(6, f10);
        let mut one = [0usize; 1];
        assert_eq!(
            walk_frame_pointers(f2, stack.bounds(), |_| None, &mut one),
            1
        );
    }

    #[test]
    fn frame_pointer_walk_crosses_a_coroutine_stack_switch() {
        let mut parent = Forged::new(8);
        let mut coroutine = Forged::new(8);
        coroutine.set(1, 0xc0); // fp 0 ends the coroutine's own chain
        parent.set(1, 0xa0);
        let (cb, pb, parent_bounds) = (coroutine.at(0), parent.at(0), parent.bounds());
        let mut out = [0usize; 4];
        let count = walk_frame_pointers(
            cb,
            coroutine.bounds(),
            |fp| (fp == cb).then_some((pb, parent_bounds)),
            &mut out,
        );
        assert_eq!(&out[..count], &[0xc0, 0xa0]);
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn the_running_image_has_a_build_id_and_base() {
        let build = prime_build_info();
        assert_ne!(build.image_base, 0);
        // Rust test binaries are linked with a GNU build id on Linux.
        assert!(build.build_id_len > 0);
    }

    #[cfg(any(target_os = "linux", target_os = "freebsd"))]
    #[test]
    fn gnu_build_id_is_found_among_notes() {
        let mut notes = Vec::new();
        for (kind, name, desc) in [
            (1u32, &b"XYZ\0"[..], &[9u8, 9][..]),
            (3, b"GNU\0", &[1, 2, 3, 4, 5]),
        ] {
            notes.extend_from_slice(&(u32::try_from(name.len()).unwrap()).to_ne_bytes());
            notes.extend_from_slice(&(u32::try_from(desc.len()).unwrap()).to_ne_bytes());
            notes.extend_from_slice(&kind.to_ne_bytes());
            notes.extend_from_slice(name);
            notes.extend_from_slice(desc);
            while notes.len() % 4 != 0 {
                notes.push(0);
            }
        }
        assert_eq!(image::gnu_build_id(&notes), Some(&[1u8, 2, 3, 4, 5][..]));
        assert_eq!(image::gnu_build_id(&notes[..20]), None);
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn frames_are_captured_at_the_caller() {
        let mut record = FailureRecord::new(212);
        capture_frames(&mut record);
        assert!(record.frame_count > 2, "{}", record.frame_count);
        assert!(record.frames().iter().all(|frame| frame.pc != 0));
    }
}
