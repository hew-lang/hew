//! Death tests for the hardware-fault boundary.
//!
//! Each case re-runs this test binary as a child that initializes the runtime
//! on a thread with a tight stack and then faults. The parent asserts the one
//! record on stderr and that the child died by the original signal (Unix) or
//! exception (Windows), so the operating system's crash artefacts still apply.

#![cfg(any(unix, windows))]

use std::process::{Command, Output};

const CHILD_CASE: &str = "HEW_SIGNAL_FAULT_RECORD_CASE";
/// Small enough that the recursion overflows in a few thousand frames.
const CHILD_STACK_BYTES: usize = 256 * 1024;

fn run_child(case: &str) -> Output {
    Command::new(std::env::current_exe().expect("test executable resolves"))
        .args(["child_entry", "--exact", "--nocapture", "--test-threads=1"])
        .env(CHILD_CASE, case)
        .output()
        .expect("child test process launches")
}

#[inline(never)]
fn descend(depth: u64) -> u64 {
    // A frame-sized local keeps every level on the stack at any opt level.
    let pad = std::hint::black_box([depth; 32]);
    if std::hint::black_box(depth) == u64::MAX {
        return pad[0];
    }
    descend(depth + 1).wrapping_add(std::hint::black_box(pad[1]))
}

fn disable_core_dumps() {
    #[cfg(unix)]
    {
        let none = libc::rlimit {
            rlim_cur: 0,
            rlim_max: 0,
        };
        // SAFETY: setrlimit reads a valid rlimit; lowering is always allowed.
        unsafe { libc::setrlimit(libc::RLIMIT_CORE, &raw const none) };
    }
}

/// The child half: never returns normally for a known case.
#[test]
fn child_entry() {
    let Ok(case) = std::env::var(CHILD_CASE) else {
        return;
    };
    disable_core_dumps();
    let worker = std::thread::Builder::new()
        .stack_size(CHILD_STACK_BYTES)
        .spawn(move || {
            assert_eq!(hew_runtime::scheduler::hew_sched_init(), 0);
            match case.as_str() {
                "overflow" => {
                    std::hint::black_box(descend(0));
                }
                #[cfg(unix)]
                "kill" => {
                    // SAFETY: sending a signal to this process is the case.
                    unsafe { libc::kill(libc::getpid(), libc::SIGSEGV) };
                    std::thread::sleep(std::time::Duration::from_secs(10));
                }
                "null" => {
                    let null = std::hint::black_box(std::ptr::null::<u64>());
                    // SAFETY: deliberately faults; this child is a death test.
                    std::hint::black_box(unsafe { null.read_volatile() });
                }
                other => panic!("unknown child case {other}"),
            }
        })
        .expect("child thread spawns");
    let _ = worker.join();
    panic!("child case survived its fault");
}

fn stderr_of(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

#[cfg(unix)]
fn assert_died_by(output: &Output, signal: i32) {
    use std::os::unix::process::ExitStatusExt;
    assert_eq!(
        output.status.signal(),
        Some(signal),
        "child must die by signal {signal}, got {:?}; stderr:\n{}",
        output.status,
        stderr_of(output)
    );
}

#[test]
#[cfg_attr(miri, ignore = "spawns a subprocess that overflows its stack")]
fn stack_overflow_is_reported_once_and_dies_by_the_signal() {
    let output = run_child("overflow");
    let stderr = stderr_of(&output);
    assert_eq!(
        stderr.matches("hew: failure: StackOverflow at 0x").count(),
        1,
        "exactly one StackOverflow record: {stderr}"
    );
    assert!(
        stderr.contains("thread main\n  #0 0x"),
        "the record names the thread and the faulting frame: {stderr}"
    );
    #[cfg(unix)]
    {
        // macOS reports a guard-page hit as SIGBUS on some releases.
        use std::os::unix::process::ExitStatusExt;
        assert!(
            matches!(output.status.signal(), Some(libc::SIGSEGV | libc::SIGBUS)),
            "overflow must die by SIGSEGV or SIGBUS, got {:?}; stderr:\n{stderr}",
            output.status
        );
        assert!(stderr.contains("at address 0x"), "{stderr}");
    }
    #[cfg(windows)]
    assert_eq!(
        output.status.code().map(i32::cast_unsigned),
        Some(0xC000_00FD),
        "overflow must exit with STATUS_STACK_OVERFLOW; stderr:\n{stderr}"
    );
}

#[test]
#[cfg_attr(miri, ignore = "spawns a subprocess that faults")]
fn a_wild_pointer_is_a_signal_not_an_overflow() {
    let output = run_child("null");
    let stderr = stderr_of(&output);
    assert!(
        stderr.contains("hew: failure: Signal at 0x"),
        "null read is an ordinary fault: {stderr}"
    );
    assert!(!stderr.contains("StackOverflow"), "{stderr}");
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        assert!(
            matches!(output.status.signal(), Some(libc::SIGSEGV | libc::SIGBUS)),
            "null read must die by its signal, got {:?}; stderr:\n{stderr}",
            output.status
        );
        assert!(stderr.contains("at address 0x0,"), "{stderr}");
    }
    #[cfg(windows)]
    assert_eq!(
        output.status.code().map(i32::cast_unsigned),
        Some(0xC000_0005),
        "null read must exit with STATUS_ACCESS_VIOLATION; stderr:\n{stderr}"
    );
}

#[test]
#[cfg(unix)]
#[cfg_attr(miri, ignore = "spawns a subprocess that is sent SIGSEGV")]
fn a_sent_fault_signal_is_reported_and_re_raised() {
    let output = run_child("kill");
    let stderr = stderr_of(&output);
    assert!(
        stderr.contains("hew: failure: Signal") && stderr.contains("SIGSEGV (11) sent by kill"),
        "kill -SEGV is reported as sent: {stderr}"
    );
    assert_died_by(&output, libc::SIGSEGV);
}
