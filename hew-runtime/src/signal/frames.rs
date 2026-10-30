//! Allocation-free frame capture for fault handlers.
//!
//! Frames come from the faulting context's frame-pointer chain. Every read is
//! validated against the thread's recorded stack bounds before it happens, so
//! a corrupt or absent chain ends the walk instead of faulting again. Runs of
//! one return address (deep recursion) collapse into a single entry with a
//! repeat count, so an overflow of tens of thousands of frames still fits.

use super::stack::StackBounds;

/// Distinct frame runs kept for the record.
pub(crate) const MAX_FRAMES: usize = 48;

/// Upper bound on chain links followed; a stack of 8 MiB holds well under this
/// many frames of 16 bytes.
const MAX_LINKS: usize = 1 << 20;

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

/// Read one machine word from a validated stack address.
pub(crate) trait StackReader {
    fn read_word(&self, addr: usize) -> usize;
}

/// Reads the live stack of the faulting thread.
pub(crate) struct LiveStack;

impl StackReader for LiveStack {
    fn read_word(&self, addr: usize) -> usize {
        // SAFETY: `walk` only reads words it has proved lie inside the
        // thread's recorded, mapped stack.
        unsafe { (addr as *const usize).read() }
    }
}

const WORD: usize = size_of::<usize>();

/// Walk the frame-pointer chain from a faulting `pc` and frame pointer `fp`.
///
/// Each frame record is `[fp] = caller's fp`, `[fp + WORD] = return address`,
/// as on x86-64 and `AArch64` for System V, `Darwin` and `FreeBSD`. The chain must stay
/// inside `bounds`, be word aligned and strictly ascend; the first link that
/// does not ends the walk.
pub(crate) fn walk(
    pc: usize,
    mut fp: usize,
    bounds: StackBounds,
    reader: &impl StackReader,
    out: &mut Frames,
) {
    out.clear();
    out.push(pc);
    for _ in 0..MAX_LINKS {
        if !fp.is_multiple_of(WORD) || !bounds.contains(fp) {
            return;
        }
        let Some(ret_slot) = fp.checked_add(WORD) else {
            return;
        };
        if !bounds.contains(ret_slot) {
            return;
        }
        let next = reader.read_word(fp);
        let ret = reader.read_word(ret_slot);
        if ret == 0 {
            return;
        }
        // The return address points after the call; report the call itself
        // so the line is the caller's call site.
        out.push(ret - 1);
        if next <= fp {
            return;
        }
        fp = next;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    struct ForgedStack(BTreeMap<usize, usize>);

    impl StackReader for ForgedStack {
        fn read_word(&self, addr: usize) -> usize {
            *self
                .0
                .get(&addr)
                .unwrap_or_else(|| panic!("walk read unvalidated address {addr:#x}"))
        }
    }

    const LOW: usize = 0x1000;
    const HIGH: usize = 0x9000;

    fn bounds() -> StackBounds {
        StackBounds {
            low: LOW,
            high: HIGH,
            guard: 4096,
        }
    }

    /// A chain of `(fp, saved fp, return address)` records.
    fn forge(links: &[(usize, usize, usize)]) -> ForgedStack {
        let mut words = BTreeMap::new();
        for &(fp, next, ret) in links {
            words.insert(fp, next);
            words.insert(fp + WORD, ret);
        }
        ForgedStack(words)
    }

    #[test]
    fn recursion_collapses_into_one_run() {
        let mut links = Vec::new();
        let mut fp = LOW + 0x100;
        for _ in 0..1000 {
            links.push((fp, fp + 0x10, 0xAAA1));
            fp += 0x10;
        }
        links.push((fp, fp + 0x40, 0xBBB1));
        links.push((fp + 0x40, 0, 0xCCC1));
        let mut frames = Frames::EMPTY;
        walk(0xAAA0, LOW + 0x100, bounds(), &forge(&links), &mut frames);
        let runs: Vec<(usize, u64)> = frames.runs().iter().map(|r| (r.pc, r.count)).collect();
        assert_eq!(runs, [(0xAAA0, 1001), (0xBBB0, 1), (0xCCC0, 1)]);
    }

    #[test]
    fn a_chain_leaving_the_stack_stops_before_reading() {
        // The second link points below the stack; the walk must not read it.
        let links = [(LOW + 0x100, LOW - 0x100, 0x2001)];
        let mut frames = Frames::EMPTY;
        walk(
            0x1000_0000,
            LOW + 0x100,
            bounds(),
            &forge(&links),
            &mut frames,
        );
        assert_eq!(
            frames.runs().iter().map(|run| run.pc).collect::<Vec<_>>(),
            [0x1000_0000, 0x2000]
        );
    }

    #[test]
    fn a_descending_or_misaligned_chain_stops() {
        let descending = [(LOW + 0x200, LOW + 0x100, 0x2001)];
        let mut frames = Frames::EMPTY;
        walk(
            0x10,
            LOW + 0x200,
            bounds(),
            &forge(&descending),
            &mut frames,
        );
        assert_eq!(frames.runs().len(), 2);

        let mut frames = Frames::EMPTY;
        walk(0x10, LOW + 0x203, bounds(), &forge(&[]), &mut frames);
        assert_eq!(frames.runs().len(), 1);
    }

    #[test]
    fn no_frame_pointer_still_reports_the_faulting_pc() {
        let mut frames = Frames::EMPTY;
        walk(0x4242, 0, bounds(), &forge(&[]), &mut frames);
        assert_eq!(
            frames.runs(),
            &[FrameRun {
                pc: 0x4242,
                count: 1
            }]
        );
        let mut frames = Frames::EMPTY;
        walk(
            0x4242,
            0x1234,
            StackBounds::UNKNOWN,
            &forge(&[]),
            &mut frames,
        );
        assert_eq!(frames.runs().len(), 1);
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
