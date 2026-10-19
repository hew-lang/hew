//! Thread-count regression checks for generators and `fork`.
//!
//! Both tests read `/proc/self/task` from inside the running Hew program.
//! The generator probe retains each task's ID and name in the same pass that
//! counts it, so a changed peak can identify the thread without a later sample.
//! Blocking-pool threads are excluded: they run offloaded calls,
//! including the file reads that take the count. Linux-only: `/proc/self/task` has no cross-platform equivalent
//! here, and the portable alternative (a runtime-owned spawn counter) does
//! not exist yet.
//!
//! - `generator_iteration_creates_no_os_threads` checks that generator
//!   iteration creates no threads.
//! - `fork_children_create_no_os_threads` requires fork continuations to park
//!   on the shared scheduler without creating OS threads.

#![cfg(target_os = "linux")]

mod support;

use support::{require_codegen, run_bounded_hew_run};

/// Extracts the integer printed on the line immediately after a given label
/// line (`println("label"); println(value);` in the fixture).
fn value_after_label(stdout: &str, label: &str) -> i64 {
    let mut lines = stdout.lines();
    while let Some(line) = lines.next() {
        if line.trim() == label {
            let value_line = lines
                .next()
                .unwrap_or_else(|| panic!("no line after label {label:?}; stdout:\n{stdout}"));
            return value_line
                .trim()
                .parse()
                .unwrap_or_else(|e| panic!("label {label:?}: {e}; line: {value_line:?}"));
        }
    }
    panic!("label {label:?} not found in stdout:\n{stdout}");
}

/// Each generator surface samples immediately before its loop and on every
/// iteration. Its peak must equal that surface's own pre-loop baseline;
/// spawning `Emitter` legitimately changes the receive-generator baseline.
#[test]
fn generator_iteration_creates_no_os_threads() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("generator_thread_fence.hew");
    std::fs::write(&path, GENERATOR_THREAD_FENCE_SOURCE)
        .expect("write generator thread-fence fixture");

    let output = run_bounded_hew_run(&path, dir.path());
    assert!(
        output.status.success(),
        "generator thread-fence fixture should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();

    for (form, expected_iters) in [
        ("genfn", 500),
        ("genblock", 3),
        ("asyncgen", 50),
        ("recvgen", 200),
    ] {
        assert_generator_surface_ran_clean(&stdout, form, expected_iters);
    }
}

/// A surface that yields nothing would trivially show `peak == pre`;
/// requiring the iteration count too means an empty loop cannot pass.
fn assert_generator_surface_ran_clean(stdout: &str, form: &str, expected_iters: i64) {
    let pre = value_after_label(stdout, &format!("pre-{form}"));
    let peak = value_after_label(stdout, &format!("peak-{form}"));
    let iters = value_after_label(stdout, &format!("iters-{form}"));
    assert_eq!(
        iters, expected_iters,
        "{form}: expected {expected_iters} loop iterations, got {iters}; stdout:\n{stdout}"
    );
    assert_eq!(
        peak, pre,
        "{form}: generator iteration must create no OS threads (pre={pre}, peak={peak}); \
         stdout:\n{stdout}"
    );
}

/// Fixture for `generator_iteration_creates_no_os_threads`: samples the
/// thread count before each of the four generator surfaces' loops, tracks
/// the peak across every iteration, and counts iterations so a surface that
/// yields nothing cannot pass on a vacuous `peak == pre`.
const GENERATOR_THREAD_FENCE_SOURCE: &str = r#"
import std.fs;

// Blocking-pool threads run offloaded calls, including the reads below; the
// fence counts every other thread. Retain IDs and names from the same reads
// used for the count so a transient peak can be identified without resampling.
fn threads() -> (i64, string) {
    let names = fs.list_dir("/proc/self/task").expect("the task directory is readable");
    var count = 0;
    var identities = "";
    for name in names {
        match fs.read(f"/proc/self/task/{name}/comm") {
            .Ok(comm) => {
                if !comm.starts_with("hew-blocking") {
                    count += 1;
                    identities = identities + f" {name}:{comm.trim()}";
                }
            }
            .Err(_) => {}
        }
    }
    (count, identities)
}

gen fn counter(n: i64) -> i64 {
    for i in 0..n {
        yield i;
    }
}

gen fn aticks(n: i64) -> i64 {
    for i in 0..n {
        yield i;
    }
}

actor Emitter {
    receive gen fn ticks(n: i64) -> i64 {
        for i in 0..n {
            yield i;
        }
    }
}

fn main() {
    // gen fn + for-in
    let (pre_genfn, pre_genfn_threads) = threads();
    var peak_genfn = pre_genfn;
    var peak_genfn_threads = pre_genfn_threads;
    var iters_genfn = 0;
    for v in counter(500) {
        let (t, thread_ids) = threads();
        if t > peak_genfn {
            peak_genfn = t;
            peak_genfn_threads = thread_ids;
        }
        iters_genfn = iters_genfn + 1;
        if v < 0 { println("unreachable"); }
    }
    println("pre-genfn");
    println(pre_genfn);
    println("peak-genfn");
    println(peak_genfn);
    println("iters-genfn");
    println(iters_genfn);
    if peak_genfn != pre_genfn {
        println(f"thread identities genfn baseline={pre_genfn_threads} peak={peak_genfn_threads}");
    }

    // gen {} block
    let (pre_genblock, pre_genblock_threads) = threads();
    var peak_genblock = pre_genblock;
    var peak_genblock_threads = pre_genblock_threads;
    var iters_genblock = 0;
    let g = gen {
        yield 1;
        yield 2;
        yield 3;
    };
    for v in g {
        let (t, thread_ids) = threads();
        if t > peak_genblock {
            peak_genblock = t;
            peak_genblock_threads = thread_ids;
        }
        iters_genblock = iters_genblock + 1;
        if v < 0 { println("unreachable"); }
    }
    println("pre-genblock");
    println(pre_genblock);
    println("peak-genblock");
    println(peak_genblock);
    println("iters-genblock");
    println(iters_genblock);
    if peak_genblock != pre_genblock {
        println(f"thread identities genblock baseline={pre_genblock_threads} peak={peak_genblock_threads}");
    }

    // second gen fn + for
    let (pre_asyncgen, pre_asyncgen_threads) = threads();
    var peak_asyncgen = pre_asyncgen;
    var peak_asyncgen_threads = pre_asyncgen_threads;
    var iters_asyncgen = 0;
    for v in aticks(50) {
        let (t, thread_ids) = threads();
        if t > peak_asyncgen {
            peak_asyncgen = t;
            peak_asyncgen_threads = thread_ids;
        }
        iters_asyncgen = iters_asyncgen + 1;
        if v < 0 { println("unreachable"); }
    }
    println("pre-asyncgen");
    println(pre_asyncgen);
    println("peak-asyncgen");
    println(peak_asyncgen);
    println("iters-asyncgen");
    println(iters_asyncgen);
    if peak_asyncgen != pre_asyncgen {
        println(f"thread identities asyncgen baseline={pre_asyncgen_threads} peak={peak_asyncgen_threads}");
    }

    // receive gen fn + for (cross-actor stream producer)
    let e = spawn Emitter;
    let (pre_recvgen, pre_recvgen_threads) = threads();
    var peak_recvgen = pre_recvgen;
    var peak_recvgen_threads = pre_recvgen_threads;
    var iters_recvgen = 0;
    for v in e.ticks(200) {
        let (t, thread_ids) = threads();
        if t > peak_recvgen {
            peak_recvgen = t;
            peak_recvgen_threads = thread_ids;
        }
        iters_recvgen = iters_recvgen + 1;
        if v < 0 { println("unreachable"); }
    }
    println("pre-recvgen");
    println(pre_recvgen);
    println("peak-recvgen");
    println(peak_recvgen);
    println("iters-recvgen");
    println(iters_recvgen);
    if peak_recvgen != pre_recvgen {
        println(f"thread identities recvgen baseline={pre_recvgen_threads} peak={peak_recvgen_threads}");
    }
}
"#;

/// Park several fork children and sample from another child while all are live.
/// Different child counts distinguish per-task thread creation from fixed costs.
#[test]
fn fork_children_create_no_os_threads() {
    require_codegen();

    let results: Vec<(i64, i64, i64)> = [1i64, 5]
        .into_iter()
        .map(|n| {
            let (pre, during) = run_fork_thread_probe(n);
            (n, pre, during)
        })
        .collect();

    let report = || {
        results
            .iter()
            .map(|(n, pre, during)| {
                format!("N={n}: pre={pre} during={during} delta={}", during - pre)
            })
            .collect::<Vec<_>>()
            .join("; ")
    };
    for &(n, pre, during) in &results {
        assert_eq!(
            during,
            pre,
            "N={n}: fork children must not create OS threads while parked on a \
             long sleep; full run: {}",
            report()
        );
    }
}

/// Builds and runs a fixture with `n` fork children each sleeping for
/// 400ms (long enough to still be live when the reporter samples), plus one
/// reporter fork that sleeps 50ms then samples the thread count while every
/// other child is still parked. Returns (pre-scope count, during-scope count).
///
fn run_fork_thread_probe(n: i64) -> (i64, i64) {
    let dir = support::tempdir();
    let path = dir.path().join(format!("fork_thread_fence_{n}.hew"));

    let napper_forks: String = (0..n)
        .map(|_| "            fork { napper(); };\n")
        .collect();

    let source = format!(
        r#"
import std.fs;

// Blocking-pool threads run offloaded calls, including the reads below; the
// fence counts every other thread.
fn threads() -> i64 {{
    let names = fs.list_dir("/proc/self/task").expect("the task directory is readable");
    var count = 0;
    for name in names {{
        match fs.read(f"/proc/self/task/{{name}}/comm") {{
            .Ok(comm) => {{
                if !comm.starts_with("hew-blocking") {{
                    count += 1;
                }}
            }}
            .Err(_) => {{}}
        }}
    }}
    count
}}

fn napper() {{
    sleep(400ms);
}}

fn reporter() {{
    sleep(50ms);
    println("during");
    println(threads());
}}

actor Driver {{ 
    receive fn go() -> i64 {{
        // Warm the shared `hew-timer-tick` thread before sampling the
        // baseline: it is spawned lazily on the first ctx-bearing `sleep`
        // (this handler is coroutine-hosted, so this suspends rather than
        // blocking). Without this warm-up, `pre` would be sampled before the
        // timer thread exists and `during` after — a false +1 unrelated to
        // fork children.
        sleep(1ms); println("pre"); println(threads()); scope {{
{napper_forks}            fork {{ reporter() }};
        }};
        0
    }}
}}

fn main() -> i64 {{
    let d = spawn Driver;
    match d.go() {{
        .Ok(v) => v,
        .Err(_e) => 1,
    }}
}}
"#
    );
    std::fs::write(&path, source).expect("write fork thread-fence fixture");

    let mut command = support::hew_command();
    command
        .args(["run"])
        .arg(&path)
        .current_dir(dir.path())
        .env("HEW_WORKERS", "1");
    let output = support::run_bounded_command(command, "fork thread fence on one worker");
    assert!(
        output.status.success(),
        "N={n}: fork thread-fence fixture should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();

    let pre = value_after_label(&stdout, "pre");
    let during = value_after_label(&stdout, "during");
    (pre, during)
}
