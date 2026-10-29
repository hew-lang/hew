use super::*;
#[cfg(kani)]
fn unexpected_panic(_: std::fmt::Arguments<'_>) -> ! {
    assert!(false, "fault: unexpected panic");
    kani::assume(false);
    loop {}
}
#[cfg(kani)]
#[kani::proof]
#[kani::unwind(66)]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
fn fault_record_ids_never_alias_or_become_none() {
    let authority = ExitStatusAuthority::new();
    let next: u64 = kani::any();
    authority.next_id.store(next, Ordering::Relaxed);
    let first = authority.open_record();
    let second = authority.open_record();
    assert!(
        first.is_some() || authority.terminal.load(Ordering::Relaxed),
        "fault: ID exhaustion disappeared as successful recovery"
    );
    assert!(
        second.is_some() || authority.terminal.load(Ordering::Relaxed),
        "fault: ID exhaustion returned NONE without terminal failure"
    );
    if first.is_some() && second.is_some() {
        assert_ne!(first, second, "fault: unrelated crashes share one record");
    }
    assert!(authority.faulted(), "fault: open crash was masked");
    kani::cover!(next == u64::MAX, "fault cover: exhausted identity source");
}

#[cfg(kani)]
#[kani::proof]
#[kani::unwind(66)]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
fn fault_rulings_preserve_pending_and_terminal_ownership() {
    let authority = ExitStatusAuthority::new();
    let records = [authority.open_record(), authority.open_record()];
    let mut pending = [true, true];
    let mut terminal = false;
    for _ in 0..4 {
        let which: u8 = kani::any();
        let action: u8 = kani::any();
        kani::assume(which < 3 && action < 4);
        let record = if which < 2 {
            records[which as usize]
        } else {
            FaultRecord::NONE
        };
        let ruling = match action {
            0 => FaultRuling::Handled,
            1 => FaultRuling::Escalated,
            2 => FaultRuling::ArmedForRestart,
            _ => FaultRuling::Unrecovered,
        };
        terminal |= action == 3;
        let settles = action == 0 || action == 3;
        let expected = which < 2 && pending[which as usize] && settles;
        assert_eq!(
            authority.settle(record, ruling),
            expected,
            "fault: wrong authority or provisional ruling retired a record"
        );
        if expected {
            pending[which as usize] = false;
        }
        assert_eq!(
            authority.open_count(),
            usize::from(pending[0]) + usize::from(pending[1]),
            "fault: record lost or retired twice"
        );
        assert_eq!(
            authority.faulted(),
            terminal || pending[0] || pending[1],
            "fault: recovery masked a pending or terminal failure"
        );
    }
}

#[cfg(kani)]
#[kani::proof]
#[kani::unwind(66)]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
fn fault_table_exhaustion_cannot_report_recovery() {
    let authority = ExitStatusAuthority::new();
    let gap: usize = kani::any();
    kani::assume(gap < MAX_OPEN_RECORDS);
    for (index, slot) in authority.slots.iter().enumerate() {
        slot.store(
            if index == gap { 0 } else { index as u64 + 1 },
            Ordering::Relaxed,
        );
    }
    authority.next_id.store(65, Ordering::Relaxed);
    let accepted = authority.open_record();
    assert_eq!(
        accepted.as_raw(),
        65,
        "fault: last table slot was not usable"
    );
    assert_eq!(
        authority.slots[gap].load(Ordering::Relaxed),
        65,
        "fault: existing record overwritten"
    );
    assert_eq!(
        authority.open_record(),
        FaultRecord::NONE,
        "fault: full table admitted an untracked fault"
    );
    assert!(
        authority.terminal.load(Ordering::Relaxed),
        "fault: full table silently dropped failure"
    );
    assert_eq!(
        authority.open_count(),
        MAX_OPEN_RECORDS,
        "fault: overflow damaged open records"
    );
    for slot in &authority.slots {
        slot.store(0, Ordering::Relaxed);
    }
    assert!(
        authority.faulted(),
        "fault: unrelated recovery erased overflow failure"
    );
}

#[cfg(kani)]
#[kani::proof]
#[kani::unwind(66)]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
fn fault_publication_guard_survives_reset_and_nested_completion() {
    let authority = ExitStatusAuthority::new();
    authority.begin_publication();
    authority.begin_publication();
    authority.end_publication();
    assert!(
        authority.faulted(),
        "fault: one completed publisher masked another"
    );
    let record = authority.open_record();
    authority.end_publication();
    assert!(
        authority.faulted(),
        "fault: publication-to-record handoff lost failure"
    );
    authority.settle(record, FaultRuling::Handled);
    assert!(
        !authority.faulted(),
        "fault: completed recovery stayed failing"
    );
    authority.begin_publication();
    authority.reset();
    authority.end_publication();
    assert!(
        !authority.faulted(),
        "fault: stale publication guard underflow pinned a new runtime"
    );
}

#[cfg(hew_fault_loom)]
pub(crate) fn verify() {
    use loom::{sync::Arc, thread};
    for shutdown in [false, true] {
        let mut model = loom::model::Builder::new();
        model.preemption_bound = None;
        model.max_permutations = None;
        model.max_duration = None;
        model.checkpoint_file = None;
        model.expect_explicit_explore = false;
        model.check(move || {
            let authority = Arc::new(ExitStatusAuthority::new());
            authority.begin_publication();
            let source = authority.clone();
            let publisher = thread::spawn(move || {
                let record = source.open_record();
                source.end_publication();
                record
            });
            assert!(
                if shutdown {
                    authority.unsettled()
                } else {
                    authority.faulted()
                },
                "fault: crash publication briefly reported successful process status"
            );
            let record = publisher.join().unwrap();
            assert!(authority.faulted(), "fault: published crash disappeared");
            assert!(authority.settle(record, FaultRuling::Handled));
            assert!(
                !authority.faulted(),
                "fault: actual recovery failed to clear its record"
            );
        });
        println!(
            "Verified fault publication_{} (all interleavings)",
            if shutdown { "shutdown" } else { "exit" }
        );
    }
    for (name, first, second) in [
        ("single_ruling", FaultRuling::Handled, FaultRuling::Handled),
        (
            "terminal_ruling",
            FaultRuling::Handled,
            FaultRuling::Unrecovered,
        ),
        (
            "effective_recovery",
            FaultRuling::Escalated,
            FaultRuling::Handled,
        ),
        (
            "provisional_escrow",
            FaultRuling::Escalated,
            FaultRuling::ArmedForRestart,
        ),
    ] {
        let mut model = loom::model::Builder::new();
        model.preemption_bound = None;
        model.max_permutations = None;
        model.max_duration = None;
        model.checkpoint_file = None;
        model.expect_explicit_explore = false;
        model.check(move || {
            let authority = Arc::new(ExitStatusAuthority::new());
            let record = authority.open_record();
            let a = authority.clone();
            let one = thread::spawn(move || a.settle(record, first));
            let a = authority.clone();
            let two = thread::spawn(move || a.settle(record, second));
            let settled = usize::from(one.join().unwrap()) + usize::from(two.join().unwrap());
            let final_ruling = matches!(first, FaultRuling::Handled | FaultRuling::Unrecovered)
                || matches!(second, FaultRuling::Handled | FaultRuling::Unrecovered);
            assert_eq!(
                settled,
                usize::from(final_ruling),
                "fault: racing rulings lost or duplicated settlement authority"
            );
            let terminal = first == FaultRuling::Unrecovered || second == FaultRuling::Unrecovered;
            assert_eq!(
                authority.faulted(),
                terminal || !final_ruling,
                "fault: provisional plans or unrelated recovery masked failure"
            );
        });
        println!("Verified fault {name} (all interleavings)");
    }
}
