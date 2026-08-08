//! Exactly-once release for a `#[resource]` held inside a `machine` state.
//!
//! Two defects lived here and neither was visible from a plain run:
//!
//! - A `reenter` that carried a payload field through unchanged released the
//!   source state's field BEFORE the body read it, so the handle was closed and
//!   then moved, closed, into the state the body built. The next step re-closed
//!   a zeroed slot.
//! - A machine value is `ValueClass::Unknown`, so its binding fell through every
//!   scope-exit drop class and the handle held in the state the machine ended
//!   its scope in was never closed at all.
//!
//! Both shapes are silent without an allocator that objects: the first prints a
//! plausible-looking extra close (or a `fd=0` one), the second prints nothing.
//! Under `MallocScribble` the double-close is a crash and under `leaks(1)` the
//! missed close is a nonzero byte count, so this oracle distinguishes correct
//! from garbage rather than asserting the program merely ran.
//!
//! The handle owns a real runtime allocation (`hew_deque_new`) so a missed close
//! shows up as leaked bytes; a payload string beside it makes the mixed-leaf
//! case observable, where releasing one leaf and not the other is the failure
//! neither a leak count nor a crash alone would separate.

#![cfg(unix)]

mod support;

use std::fmt::Write as _;
use std::path::Path;

use support::leak_slope::{
    compile_to_native, measure_leaks_exact, require_leaks_tool, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

const REENTER_FRAMES: usize = 6;

/// `Active { h }` re-entered `frames` times carrying `self.h` through. The
/// handle must be closed exactly once, at the machine value's scope exit.
fn reenter_carry_source(frames: usize) -> String {
    format!(
        r#"
#[opaque]
type Dq {{}}

#[resource]
type Handle {{ raw: Dq; }}

impl Handle {{
    fn close(self) {{ unsafe {{ hew_deque_free(self.raw) }}; print("C"); }}
}}

extern "C" {{
    fn hew_deque_new() -> Dq;
    fn hew_deque_free(consume dq: Dq);
}}

machine Session {{
    events {{ Open; UseIt; }}
    state Idle;
    state Active {{ h: Handle; }}

    on Open: Idle => Active {{ Active {{ h: Handle {{ raw: unsafe {{ hew_deque_new() }} }} }} }}
    on UseIt: Active => Active reenter {{ Active {{ h: self.h }} }}

    default {{ state }}
}}

fn main() {{
    var s = Session::Idle;
    s.step(SessionEvent::Open);
    for _ in 0..{frames} {{
        s.step(SessionEvent::UseIt);
        print("R");
    }}
}}
"#
    )
}

/// Two release paths, each with the payload class its authority covers.
///
/// `Mixed` carries a resource BESIDE a heap-owning string and ends its scope in
/// the live state, so the tag-aware scope-exit release owns both leaves and must
/// free each exactly once.
///
/// `Plain` steps back to the payload-free state, so the transition-out release
/// owns the handle and the scope exit must not release it again. Its payload is
/// resource-only on purpose: the transition-out release enumerates only
/// `#[resource]` fields, so a heap leaf there is a separate tracked leak, not
/// this oracle's subject.
const RELEASE_PATH_SOURCE: &str = r#"
#[opaque]
type Dq {}

#[resource]
type Handle { raw: Dq; }

impl Handle {
    fn close(self) { unsafe { hew_deque_free(self.raw) }; print("C"); }
}

extern "C" {
    fn hew_deque_new() -> Dq;
    fn hew_deque_free(consume dq: Dq);
}

machine Mixed {
    events { Open; Touch; }
    state Idle;
    state Active { h: Handle; label: string; }

    on Open: Idle => Active {
        Active { h: Handle { raw: unsafe { hew_deque_new() } }, label: "live".to_upper() }
    }
    on Touch: Active => Active reenter { Active { h: self.h, label: self.label } }

    default { state }
}

machine Plain {
    events { Open; Shut; }
    state Idle;
    state Live { h: Handle; }

    on Open: Idle => Live { Live { h: Handle { raw: unsafe { hew_deque_new() } } } }
    on Shut: Live => Idle { Idle }

    default { state }
}

fn hold_mixed() {
    var s = Mixed::Idle;
    s.step(MixedEvent::Open);
    s.step(MixedEvent::Touch);
    s.step(MixedEvent::Touch);
}

fn shut_plain() {
    var s = Plain::Idle;
    s.step(PlainEvent::Open);
    s.step(PlainEvent::Shut);
}

fn main() {
    for _ in 0..4 {
        hold_mixed();
        shut_plain();
    }
}
"#;

fn assert_exact_zero_leaks(bin: &Path, shape: &str) {
    require_leaks_tool();
    let (count, bytes) = measure_leaks_exact(bin);
    assert_eq!(
        (count, bytes),
        (0, 0),
        "{shape}: expected `0 leaks for 0 total leaked bytes`, got \
         {count} leak(s) for {bytes} bytes; re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}`",
        bin.display()
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn machine_reenter_carrying_resource_closes_exactly_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("machine-reenter-resource-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &reenter_carry_source(REENTER_FRAMES),
        dir.path(),
        "machine_reenter_resource",
    );

    // A carried-through payload MOVES into the state the body builds, so the
    // re-entries close nothing: `R` per frame, then one `C` at scope exit.
    let mut expected = String::new();
    for _ in 0..REENTER_FRAMES {
        let _ = write!(expected, "R");
    }
    expected.push('C');

    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "machine reenter carrying a resource must run clean under the poisoned \
         allocator; a crash indicates the source state released a field the body \
         had already moved into the new state:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected,
        "the carried handle must close exactly once, after the last re-entry"
    );
    assert_exact_zero_leaks(&bin, "machine_reenter_carrying_resource");
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn machine_state_resource_beside_heap_leaf_closes_exactly_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("machine-release-paths-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(RELEASE_PATH_SOURCE, dir.path(), "machine_release_paths");

    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "a machine state holding a resource beside a heap leaf must run clean \
         under the poisoned allocator on both the transition-away and the \
         scope-exit release paths:\n{}",
        describe_output(&output)
    );
    // Eight machine values, each opening exactly one handle: four released by
    // the transition to the payload-free state, four by the scope exit.
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "C".repeat(8),
        "each machine value must close its handle exactly once, whichever path \
         it ends its scope on"
    );
    // The string leaf shares the state with the handle on the scope-exit path.
    // Releasing the resource and skipping the string is a leak the close count
    // alone cannot see.
    assert_exact_zero_leaks(&bin, "machine_state_resource_beside_heap_leaf");
}
