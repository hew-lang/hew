//! Thread-count regression fences for generators and `fork` (U370).
//!
//! Both tests read `/proc/self/task` from inside the running Hew program to
//! count OS threads at a known point, so the count is exact, not sampled from
//! outside. Linux-only: `/proc/self/task` has no cross-platform equivalent
//! here, and the portable alternative (a runtime-owned spawn counter) does
//! not exist yet.
//!
//! - `generator_iteration_creates_no_os_threads` passes today: all four
//!   generator surfaces (`gen fn`, `gen {}`, `async gen fn`, `receive gen fn`)
//!   already run on `llvm.coro` frames, not OS threads. It is a regression
//!   fence so the P4 concurrency lane cannot quietly re-thread the pump.
//! - `fork_children_create_no_os_threads` FAILS today: every `fork` child
//!   gets its own `std::thread::spawn` OS thread
//!   (`hew-runtime/src/task_scope.rs:1479`), one per child. It is ledgered in
//!   `scripts/nextest-expected-failures.tsv` as an expected failure so the P4
//!   pure-coroutine `fork` lane must flip it, not delete it.

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

/// U370: all four generator surfaces already run on coroutine frames, not
/// OS threads — the `cut-gen-coro` cutover held. Each block samples the
/// thread count immediately before its loop starts, then samples again on
/// every iteration and keeps the max; the peak must equal the pre-loop
/// baseline for that block specifically (not a shared global baseline —
/// spawning the `Emitter` actor for the `receive gen fn` case legitimately
/// changes its own pre-loop count).
#[test]
fn generator_iteration_creates_no_os_threads() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("generator_thread_fence.hew");
    std::fs::write(
        &path,
        r#"
import std.fs;

fn threads() -> i64 {
    let names = fs.list_dir("/proc/self/task");
    names.len()
}

gen fn counter(n: i64) -> i64 {
    for i in 0..n {
        yield i;
    }
}

async gen fn aticks(n: i64) -> i64 {
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
    let pre_genfn = threads();
    var peak_genfn = pre_genfn;
    for v in counter(500) {
        let t = threads();
        if t > peak_genfn { peak_genfn = t; }
    }
    println("pre-genfn");
    println(pre_genfn);
    println("peak-genfn");
    println(peak_genfn);

    // gen {} block
    let pre_genblock = threads();
    var peak_genblock = pre_genblock;
    let g = gen {
        yield 1;
        yield 2;
        yield 3;
    };
    for v in g {
        let t = threads();
        if t > peak_genblock { peak_genblock = t; }
    }
    println("pre-genblock");
    println(pre_genblock);
    println("peak-genblock");
    println(peak_genblock);

    // async gen fn + for await
    let pre_asyncgen = threads();
    var peak_asyncgen = pre_asyncgen;
    for await v in aticks(50) {
        let t = threads();
        if t > peak_asyncgen { peak_asyncgen = t; }
    }
    println("pre-asyncgen");
    println(pre_asyncgen);
    println("peak-asyncgen");
    println(peak_asyncgen);

    // receive gen fn + for await (cross-actor stream producer)
    let e = spawn Emitter;
    let pre_recvgen = threads();
    var peak_recvgen = pre_recvgen;
    for await v in e.ticks(200) {
        let t = threads();
        if t > peak_recvgen { peak_recvgen = t; }
    }
    println("pre-recvgen");
    println(pre_recvgen);
    println("peak-recvgen");
    println(peak_recvgen);
}
"#,
    )
    .expect("write generator thread-fence fixture");

    let output = run_bounded_hew_run(&path, dir.path());
    assert!(
        output.status.success(),
        "generator thread-fence fixture should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();

    for form in ["genfn", "genblock", "asyncgen", "recvgen"] {
        let pre = value_after_label(&stdout, &format!("pre-{form}"));
        let peak = value_after_label(&stdout, &format!("peak-{form}"));
        assert_eq!(
            peak, pre,
            "{form}: generator iteration must create no OS threads (pre={pre}, peak={peak}); \
             stdout:\n{stdout}"
        );
    }
}

/// U370: `fork` children each get a dedicated `std::thread::spawn` OS
/// thread today (`hew-runtime/src/task_scope.rs:1479`), one per child,
/// linear in child count. This asserts the ideal — the thread count while N
/// children are all parked on a long sleep equals the pre-scope count — which
/// is false today by exactly `N + 1` (the N nappers plus the in-scope
/// reporter's own thread). It is ledgered in
/// `scripts/nextest-expected-failures.tsv` as an expected failure so the P4
/// pure-coroutine `fork` lane must flip it, not delete it.
///
/// Parameterized over N so a per-child leak (this) is distinguishable from a
/// constant per-scope overhead: every N must show the fixture over by exactly
/// `N + 1`, not by some N-independent constant. Every N runs to completion
/// (no fail-fast) so a partial ratchet run always carries the full evidence.
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
    // A per-N breakdown that survives even when the assertion above never
    // fires (e.g. after a partial P4 landing changes the failure shape):
    // every delta must scale with N, not sit at a fixed constant.
    for &(n, pre, during) in &results {
        assert_eq!(
            during - pre,
            n + 1,
            "N={n}: delta must be exactly N+1 (N nappers + 1 reporter), not a \
             constant; full run: {}",
            report()
        );
    }
}

/// Builds and runs a fixture with `n` fork children each sleeping for
/// 400ms (long enough to still be live when the reporter samples), plus one
/// reporter fork that sleeps 50ms then samples the thread count while every
/// other child is still parked. Returns (pre-scope count, during-scope count).
///
/// WHY the sleeps live in free functions (`napper`, `reporter`) instead of
/// inline in the `fork {}` body: a fork body cannot contain a suspend point
/// today — the compiler refuses it (`hew-mir/src/lower/closure_gen.rs:714-730`,
/// #2863, "MIR lowering for suspension inside a closure is not implemented
/// yet"). A free function is `Default` callconv, so its `sleep` lowers to the
/// blocking `hew_sleep_ns` instead of a suspend carrier, which is exactly
/// the behaviour under test — the thread blocks rather than the coroutine
/// parking. WHEN this seam closes: once #2863 lands the ramp/driver calling
/// convention for suspending closures (tracked under the P4 concurrency
/// lane). WHAT the real fix looks like: `sleep(400ms)` written directly
/// inside `fork { }`, once a fork body can lower to a coroutine frame.
fn run_fork_thread_probe(n: i64) -> (i64, i64) {
    let dir = support::tempdir();
    let path = dir.path().join(format!("fork_thread_fence_{n}.hew"));

    let napper_forks: String = (0..n)
        .map(|_| "            fork { napper(); };\n")
        .collect();

    let source = format!(
        r#"
import std.fs;

fn threads() -> i64 {{
    let names = fs.list_dir("/proc/self/task");
    names.len()
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
        sleep(1ms);
        println("pre");
        println(threads());
        scope {{
{napper_forks}            fork {{ reporter(); }};
        }};
        0
    }}
}}

fn main() -> i64 {{
    let d = spawn Driver;
    match await d.go() {{
        .Ok(v) => v,
        .Err(_e) => 1,
    }}
}}
"#
    );
    std::fs::write(&path, source).expect("write fork thread-fence fixture");

    let output = run_bounded_hew_run(&path, dir.path());
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
