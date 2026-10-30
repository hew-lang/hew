//! Allocation-free frame capture for fault handlers.
//!
//! Frames come from the image's unwind tables (CFI), the same tables that
//! carry Hew failures through cleanup pads, so the walk needs no frame
//! pointers from generated code. Runs of one return address (deep recursion)
//! collapse into a single entry with a repeat count, so an overflow of
//! hundreds of thousands of frames still fits.

/// Distinct frame runs kept for the record.
pub(crate) const MAX_FRAMES: usize = 48;

/// One run of identical frames.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct FrameRun {
    pub(crate) pc: usize,
    /// How many consecutive frames had this pc (at least 1).
    pub(crate) count: u64,
}

/// A captured stack, in a fixed buffer.
#[derive(Debug)]
pub(crate) struct Frames {
    runs: [FrameRun; MAX_FRAMES],
    len: usize,
    /// Frames walked after the buffer filled.
    pub(crate) omitted: u64,
}

impl Frames {
    pub(crate) const EMPTY: Self = Self {
        runs: [FrameRun { pc: 0, count: 0 }; MAX_FRAMES],
        len: 0,
        omitted: 0,
    };

    pub(crate) fn clear(&mut self) {
        self.len = 0;
        self.omitted = 0;
    }

    pub(crate) fn runs(&self) -> &[FrameRun] {
        &self.runs[..self.len]
    }

    pub(crate) fn push(&mut self, pc: usize) {
        if self.len > 0 && self.runs[self.len - 1].pc == pc {
            self.runs[self.len - 1].count += 1;
        } else if self.len < MAX_FRAMES {
            self.runs[self.len] = FrameRun { pc, count: 1 };
            self.len += 1;
        } else {
            self.omitted += 1;
        }
    }
}

#[cfg(unix)]
pub(crate) use unwind::capture;

/// Upper bound on frames visited; an 8 MiB stack of minimal frames holds
/// fewer.
#[cfg(unix)]
const MAX_STEPS: usize = 1 << 20;

/// Turns the unwinder's frames, which start inside the fault handler, into
/// the faulting thread's frames.
#[cfg(unix)]
struct Collector<'a> {
    fault_pc: usize,
    reached: bool,
    steps: usize,
    out: &'a mut Frames,
}

#[cfg(unix)]
impl Collector<'_> {
    /// Take one frame; returns whether to keep walking.
    ///
    /// Frames before the faulting pc belong to the handler and the signal
    /// trampoline and are skipped. A frame whose `ip` is a return address
    /// reports `ip - 1`, the call itself, so its line is the call site.
    fn visit(&mut self, ip: usize, before_insn: bool) -> bool {
        self.steps += 1;
        if ip == 0 || self.steps > MAX_STEPS {
            return false;
        }
        if !self.reached {
            if ip == self.fault_pc {
                self.reached = true;
                self.out.push(ip);
            }
            return true;
        }
        self.out.push(if before_insn { ip } else { ip - 1 });
        true
    }

    /// Close the walk: an unwinder that cannot step through the signal frame
    /// never reaches the fault, and then the faulting pc is the whole record.
    fn finish(self) {
        if !self.reached {
            self.out.clear();
            self.out.push(self.fault_pc);
        }
    }
}

#[cfg(unix)]
mod unwind {
    use std::ffi::{c_int, c_void};

    use super::{Collector, Frames};

    #[repr(C)]
    struct UnwindContext {
        _opaque: [u8; 0],
    }

    const URC_NO_REASON: c_int = 0;
    const URC_END_OF_STACK: c_int = 5;

    type TraceFn = unsafe extern "C" fn(*mut UnwindContext, *mut c_void) -> c_int;

    // The platform unwinder: libgcc_s on Linux, LLVM libunwind (as libgcc_s)
    // on FreeBSD, libunwind in libSystem on Darwin. Rust's own unwinding
    // already links it.
    unsafe extern "C" {
        fn _Unwind_Backtrace(trace: TraceFn, arg: *mut c_void) -> c_int;
        fn _Unwind_GetIPInfo(context: *mut UnwindContext, ip_before_insn: *mut c_int) -> usize;
    }

    unsafe extern "C" fn trace(context: *mut UnwindContext, arg: *mut c_void) -> c_int {
        // SAFETY: `capture` passes its live collector as the argument.
        let collector = unsafe { &mut *arg.cast::<Collector<'_>>() };
        let mut before_insn: c_int = 0;
        // SAFETY: the unwinder passes a valid context for this frame.
        let ip = unsafe { _Unwind_GetIPInfo(context, &raw mut before_insn) };
        if collector.visit(ip, before_insn != 0) {
            URC_NO_REASON
        } else {
            URC_END_OF_STACK
        }
    }

    /// Capture the faulting thread's frames, starting at `fault_pc`.
    ///
    /// Called from the fault handler on the alternate stack. The unwinder
    /// steps through the kernel's signal frame into the interrupted context
    /// and reads only CFI and the stack it describes; it does not allocate.
    /// Its table lookup may take the loader's lock, which is recursive for
    /// the faulting thread, so the walk can only wait on another thread that
    /// is itself still running.
    pub(crate) fn capture(fault_pc: usize, out: &mut Frames) {
        out.clear();
        let mut collector = Collector {
            fault_pc,
            reached: false,
            steps: 0,
            out,
        };
        // SAFETY: `trace` only uses the collector for the call's duration.
        unsafe { _Unwind_Backtrace(trace, (&raw mut collector).cast()) };
        collector.finish();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(unix)]
    fn collect(fault_pc: usize, frames: &[(usize, bool)]) -> Vec<(usize, u64)> {
        let mut out = Frames::EMPTY;
        let mut collector = Collector {
            fault_pc,
            reached: false,
            steps: 0,
            out: &mut out,
        };
        for &(ip, before_insn) in frames {
            if !collector.visit(ip, before_insn) {
                break;
            }
        }
        collector.finish();
        out.runs().iter().map(|run| (run.pc, run.count)).collect()
    }

    #[test]
    #[cfg(unix)]
    fn handler_frames_are_skipped_and_recursion_collapses() {
        let mut frames = vec![(0x7000, false), (0x7100, false), (0xAAA0, true)];
        frames.extend(std::iter::repeat_n((0xAAB1, false), 1000));
        frames.extend([(0xBBB1, false), (0xCCC1, false)]);
        assert_eq!(
            collect(0xAAA0, &frames),
            [(0xAAA0, 1), (0xAAB0, 1000), (0xBBB0, 1), (0xCCC0, 1)]
        );
    }

    #[test]
    #[cfg(unix)]
    fn a_walk_that_never_reaches_the_fault_reports_only_its_pc() {
        let frames = [(0x7000, false), (0x7100, false), (0x7200, false)];
        assert_eq!(collect(0x4242, &frames), [(0x4242, 1)]);
    }

    #[test]
    #[cfg(unix)]
    fn a_zero_ip_ends_the_walk() {
        let frames = [(0x4242, true), (0x5001, false), (0, false), (0x6001, false)];
        assert_eq!(collect(0x4242, &frames), [(0x4242, 1), (0x5000, 1)]);
    }

    #[test]
    #[cfg(unix)]
    fn the_live_unwinder_without_a_fault_frame_reports_only_its_pc() {
        let mut out = Frames::EMPTY;
        capture(0xDEAD, &mut out);
        assert_eq!(
            out.runs(),
            &[FrameRun {
                pc: 0xDEAD,
                count: 1
            }]
        );
    }

    #[test]
    fn distinct_frames_past_capacity_are_counted() {
        let mut frames = Frames::EMPTY;
        for pc in 0..(MAX_FRAMES + 5) {
            frames.push(pc);
        }
        assert_eq!(frames.runs().len(), MAX_FRAMES);
        assert_eq!(frames.omitted, 5);
    }
}
