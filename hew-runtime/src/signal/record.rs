//! A hardware fault described in the shared [`FailureRecord`].
//!
//! The platform handler fills one record in static storage through
//! [`begin`] and its own frame walk, then [`finish`] names the frames,
//! publishes the record where a debugger reads it and renders the one
//! diagnostic. Nothing here allocates, locks or touches a lazily initialized
//! thread-local, so it runs inside a signal or exception handler.

use super::stack::ThreadLabel;
use crate::failure_kinds::{HEW_TRAP_STACK_OVERFLOW, KIND_SIGNAL_CSTR};
use crate::failure_record::{self, FailureRecord, FailureSignal};

/// What the handler learned about one fault.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Fault {
    /// Unix signal number, or the Windows exception code.
    pub(crate) signal: i32,
    /// `si_code` on Unix; zero on Windows.
    pub(crate) si_code: i32,
    /// The faulting data address; zero for a sent signal.
    pub(crate) address: usize,
    /// The address lies in the thread's stack guard region.
    pub(crate) overflow: bool,
}

/// Describe `fault` on the calling thread in `record`, which holds no
/// failure yet. The caller then captures the frames.
///
/// Fields are written in place: a handler on a thread without the runtime's
/// alternate stack runs on the small one Rust installs, with no room for a
/// record-sized temporary.
pub(crate) fn begin(record: &mut FailureRecord, fault: Fault, thread: ThreadLabel) {
    if fault.overflow {
        record.code = HEW_TRAP_STACK_OVERFLOW;
        record.kind = failure_record::kind_cstr(HEW_TRAP_STACK_OVERFLOW).as_ptr();
    } else {
        record.code = fault.signal;
        record.kind = KIND_SIGNAL_CSTR.as_ptr();
    }
    record.signal = FailureSignal {
        signal: fault.signal,
        si_code: fault.si_code,
        fault_addr: fault.address,
        guard_hit: u8::from(fault.overflow),
    };
    record.wall_time_ns = failure_record::wall_time_ns();
    record.build = failure_record::build_info();
    match thread {
        ThreadLabel::Main => failure_record::copy_name(&mut record.thread, "main"),
        ThreadLabel::Worker(id) => {
            record.worker_id = id.saturating_add(1);
            let prefix = b"hew-worker-";
            let mut digits = [0_u8; 20];
            let digits = crate::trap_code::fmt_decimal_u64(u64::from(id), &mut digits);
            let name = &mut record.thread;
            let end = (prefix.len() + digits.len()).min(name.len() - 1);
            name[..prefix.len()].copy_from_slice(prefix);
            name[prefix.len()..end].copy_from_slice(&digits[..end - prefix.len()]);
        }
        ThreadLabel::Other => {}
    }
}

/// Name the captured frames, take the innermost Hew frame as the fault's
/// location, publish the record as the last failure and render it into
/// `text`.
pub(crate) fn finish<'b>(record: &mut FailureRecord, text: &'b mut [u8]) -> &'b [u8] {
    failure_record::name_frames(record);
    if let Some((file, line)) = record
        .frames()
        .iter()
        .find(|frame| !frame.file.is_null())
        .map(|frame| (frame.file, frame.line))
    {
        record.site.file = file;
        record.site.line = line;
    }
    failure_record::publish_last_fault(record);
    let len = failure_record::format_into(record, text);
    &text[..len]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fault(overflow: bool) -> Fault {
        Fault {
            signal: 11,
            si_code: 2,
            address: 0x7ffd_0000_0ff8,
            overflow,
        }
    }

    fn render(record: &FailureRecord) -> String {
        let mut text = [0_u8; 1024];
        let len =
            failure_record::format_with(record, failure_record::Backtrace::Full, false, &mut text);
        String::from_utf8_lossy(&text[..len]).into_owned()
    }

    #[test]
    fn an_overflow_is_a_stack_overflow_failure_and_collapses_recursion() {
        let mut record = FailureRecord::EMPTY;
        begin(&mut record, fault(true), ThreadLabel::Main);
        for _ in 0..5000 {
            record.push_frame(0x5555_0000_1234);
        }
        record.push_frame(0x5555_0000_2000);
        let text = render(&record);
        assert!(
            text.starts_with("hew: failure: StackOverflow (215)\n"),
            "{text}"
        );
        let signal = if cfg!(windows) {
            "exception"
        } else {
            "SIGSEGV"
        };
        assert!(
            text.contains(&format!(
                "{signal} (11) at 0x7ffd00000ff8 (stack guard), thread main\n"
            )),
            "{text}"
        );
        assert!(
            text.contains(
                "hew:   at 0x555500001234\nhew:     (... 4999 identical frames)\n\
                 hew:   at 0x555500002000\n"
            ),
            "{text}"
        );
        assert_eq!(failure_record::padded_str(&record.thread), "main");
    }

    #[test]
    fn any_other_fault_is_a_signal_failure_on_its_thread() {
        let mut record = FailureRecord::EMPTY;
        begin(&mut record, fault(false), ThreadLabel::Worker(3));
        assert_eq!(record.kind_name(), "Signal");
        assert_eq!(record.code, 11);
        assert_eq!(record.worker_id, 4);
        assert_eq!(failure_record::padded_str(&record.thread), "hew-worker-3");
        let text = render(&record);
        assert!(text.starts_with("hew: failure: Signal (11)\n"), "{text}");
        assert!(!text.contains("stack guard"), "{text}");
    }
}
