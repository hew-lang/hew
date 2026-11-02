//! The hardware-fault failure record, formatted without allocation.
//!
//! A fault handler fills one [`FaultRecord`] in static storage and formats it
//! into a fixed buffer, then writes the buffer to stderr in one call.

use std::sync::atomic::{AtomicUsize, Ordering};

use super::frames::Frames;
use super::stack::ThreadLabel;

/// What the handler concluded about the fault.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum FaultKind {
    /// The faulting address lies in the thread's stack guard region.
    StackOverflow,
    /// Any other synchronous fault, or a fault signal sent with kill or raise.
    Signal,
}

impl FaultKind {
    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::StackOverflow => "StackOverflow",
            Self::Signal => "Signal",
        }
    }
}

/// Everything one fault report says.
#[derive(Debug)]
pub(crate) struct FaultRecord {
    pub(crate) kind: FaultKind,
    /// Unix signal number, or the Windows exception code.
    pub(crate) signal: u32,
    pub(crate) signal_name: &'static str,
    /// Whether the fault came from `kill`/`raise` rather than an instruction.
    pub(crate) sent: bool,
    pub(crate) address: usize,
    pub(crate) thread: ThreadLabel,
    /// Load range `[image_base, image_end)` of the image the runtime is linked
    /// into, so a frame's `pc - image_base` is a file-relative address for
    /// symbolization. Frames outside the range (system libraries) print bare.
    pub(crate) image_base: usize,
    pub(crate) image_end: usize,
    pub(crate) frames: Frames,
}

impl FaultRecord {
    pub(crate) const EMPTY: Self = Self {
        kind: FaultKind::Signal,
        signal: 0,
        signal_name: "",
        sent: false,
        address: 0,
        thread: ThreadLabel::Other,
        image_base: 0,
        image_end: 0,
        frames: Frames::EMPTY,
    };

    /// Format the record into `out`; returns the bytes written. Output that
    /// does not fit is truncated at a line boundary-agnostic byte limit.
    pub(crate) fn format_into<'b>(&self, out: &'b mut [u8]) -> &'b [u8] {
        let mut w = Writer { buf: out, len: 0 };
        w.str("hew: failure: ");
        w.str(self.kind.name());
        if let Some(first) = self.frames.runs().first() {
            w.str(" at ");
            w.hex(first.pc);
        }
        w.str("\n  ");
        w.str(self.signal_name);
        w.str(" (");
        // Windows exception codes read as hex; Unix signal numbers as decimal.
        if self.signal > 0xFFFF {
            w.hex(self.signal as usize);
        } else {
            w.dec(u64::from(self.signal));
        }
        w.str(")");
        if self.sent {
            w.str(" sent by kill");
        } else {
            w.str(" at address ");
            w.hex(self.address);
        }
        w.str(", thread ");
        match self.thread {
            ThreadLabel::Main => w.str("main"),
            ThreadLabel::Worker(id) => {
                w.str("hew-worker-");
                w.dec(u64::from(id));
            }
            ThreadLabel::Other => w.str("unnamed"),
        }
        w.str("\n");
        for (index, run) in self.frames.runs().iter().enumerate() {
            w.str("  #");
            w.dec(index as u64);
            w.str(" ");
            w.hex(run.pc);
            if self.image_base != 0 && self.image_base <= run.pc && run.pc < self.image_end {
                w.str(" (image+");
                w.hex(run.pc - self.image_base);
                w.str(")");
            }
            w.str("\n");
            if run.count > 1 {
                w.str("     (... ");
                w.dec(run.count - 1);
                w.str(" identical frames)\n");
            }
        }
        if self.frames.omitted != 0 {
            w.str("  (... ");
            w.dec(self.frames.omitted);
            w.str(" more frames)\n");
        }
        let len = w.len;
        &w.buf[..len]
    }
}

static IMAGE_BASE: AtomicUsize = AtomicUsize::new(0);
static IMAGE_END: AtomicUsize = AtomicUsize::new(0);

/// Publish the runtime image's load range, found once at installation.
pub(crate) fn set_image_range(base: usize, end: usize) {
    IMAGE_BASE.store(base, Ordering::Relaxed);
    IMAGE_END.store(end, Ordering::Relaxed);
}

impl FaultRecord {
    pub(crate) fn load_image_range(&mut self) {
        self.image_base = IMAGE_BASE.load(Ordering::Relaxed);
        self.image_end = IMAGE_END.load(Ordering::Relaxed);
    }
}

struct Writer<'b> {
    buf: &'b mut [u8],
    len: usize,
}

impl Writer<'_> {
    fn bytes(&mut self, bytes: &[u8]) {
        let room = self.buf.len() - self.len;
        let take = bytes.len().min(room);
        self.buf[self.len..self.len + take].copy_from_slice(&bytes[..take]);
        self.len += take;
    }

    fn str(&mut self, text: &str) {
        self.bytes(text.as_bytes());
    }

    fn dec(&mut self, value: u64) {
        let mut digits = [0_u8; 20];
        self.bytes(crate::trap_code::fmt_decimal_u64(value, &mut digits));
    }

    fn hex(&mut self, value: usize) {
        let mut digits = [0_u8; 2 + 2 * size_of::<usize>()];
        let mut at = digits.len();
        let mut rest = value;
        loop {
            at -= 1;
            digits[at] = b"0123456789abcdef"[rest & 0xF];
            rest >>= 4;
            if rest == 0 {
                break;
            }
        }
        at -= 2;
        digits[at] = b'0';
        digits[at + 1] = b'x';
        self.bytes(&digits[at..]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn record(kind: FaultKind, pcs: &[usize]) -> FaultRecord {
        let mut record = FaultRecord::EMPTY;
        record.kind = kind;
        record.signal = 11;
        record.signal_name = "SIGSEGV";
        record.address = 0x7ffd_0000_0ff8;
        record.thread = ThreadLabel::Main;
        record.image_base = 0x5555_0000_0000;
        record.image_end = 0x5555_0001_0000;
        for &pc in pcs {
            record.frames.push(pc);
        }
        record
    }

    #[test]
    fn overflow_record_collapses_recursion() {
        let mut pcs = vec![0x5555_0000_1234; 5000];
        pcs.push(0x5555_0000_2000);
        let record = record(FaultKind::StackOverflow, &pcs);
        let mut buf = [0_u8; 1024];
        let text = std::str::from_utf8(record.format_into(&mut buf)).unwrap();
        assert_eq!(
            text,
            "hew: failure: StackOverflow at 0x555500001234\n\
             \x20 SIGSEGV (11) at address 0x7ffd00000ff8, thread main\n\
             \x20 #0 0x555500001234 (image+0x1234)\n\
             \x20    (... 4999 identical frames)\n\
             \x20 #1 0x555500002000 (image+0x2000)\n"
        );
    }

    #[test]
    fn a_sent_signal_has_no_fault_address() {
        let mut record = record(FaultKind::Signal, &[0x10]);
        record.sent = true;
        record.thread = ThreadLabel::Worker(3);
        let mut buf = [0_u8; 512];
        let text = std::str::from_utf8(record.format_into(&mut buf)).unwrap();
        assert!(
            text.starts_with(
                "hew: failure: Signal at 0x10\n  SIGSEGV (11) sent by kill, \
                 thread hew-worker-3\n"
            ),
            "{text}"
        );
    }

    #[test]
    fn frames_outside_the_image_have_no_offset() {
        let record = record(FaultKind::Signal, &[0x7f00_0000_0010]);
        let mut buf = [0_u8; 512];
        let text = std::str::from_utf8(record.format_into(&mut buf)).unwrap();
        assert!(text.contains("  #0 0x7f0000000010\n"), "{text}");
    }

    #[test]
    fn output_is_truncated_not_overrun() {
        let record = record(FaultKind::StackOverflow, &[0x1, 0x2, 0x3]);
        let mut buf = [0_u8; 16];
        assert_eq!(record.format_into(&mut buf), b"hew: failure: St");
    }
}
