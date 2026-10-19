//! Thread-count regression checks for generators and `fork`.
//!
//! Both tests read `/proc/self/task` from inside the running Hew program.
//! The generator probe retains each task's ID and name on every sample, then
//! classifies blocking-pool threads using all names observed during the run.
//! Those workers run offloaded calls, including the file reads that take the
//! count. Linux-only: `/proc/self/task` has no cross-platform equivalent here,
//! and the portable alternative (a runtime-owned spawn counter) does not exist
//! yet.
//!
//! - `generator_iteration_creates_no_os_threads` checks that generator
//!   iteration creates no threads.
//! - `fork_children_create_no_os_threads` requires fork continuations to park
//!   on the shared scheduler without creating OS threads.

#![cfg(target_os = "linux")]

mod support;

use std::collections::{BTreeMap, BTreeSet};

use support::{require_codegen, run_bounded_hew_run};

#[derive(Clone, Debug, Eq, PartialEq)]
struct ThreadIdentity {
    tid: u32,
    name: String,
}

type ThreadSnapshot = Vec<ThreadIdentity>;

fn parse_thread_snapshot(encoded: &str) -> ThreadSnapshot {
    encoded
        .split_whitespace()
        .map(|identity| {
            let (tid, name) = identity
                .split_once(':')
                .unwrap_or_else(|| panic!("invalid thread identity {identity:?}"));
            ThreadIdentity {
                tid: tid
                    .parse()
                    .unwrap_or_else(|e| panic!("invalid thread ID {tid:?}: {e}")),
                name: name.to_owned(),
            }
        })
        .collect()
}

fn thread_snapshots(stdout: &str, label: &str) -> Vec<ThreadSnapshot> {
    let prefix = format!("{label}:");
    let encoded = stdout
        .lines()
        .find_map(|line| line.strip_prefix(&prefix))
        .unwrap_or_else(|| panic!("thread snapshot label {label:?} not found; stdout:\n{stdout}"));
    encoded.split('|').map(parse_thread_snapshot).collect()
}

fn single_thread_snapshot(stdout: &str, label: &str) -> ThreadSnapshot {
    thread_snapshots(stdout, label)
        .into_iter()
        .next()
        .unwrap_or_else(|| panic!("thread snapshot {label:?} is empty"))
}

fn blocking_worker_tids(snapshots: &[ThreadSnapshot]) -> BTreeSet<u32> {
    snapshots
        .iter()
        .flatten()
        .filter(|identity| identity.name.starts_with("hew-blocking"))
        .map(|identity| identity.tid)
        .collect()
}

fn counted_threads(snapshot: &ThreadSnapshot, blocking_tids: &BTreeSet<u32>) -> usize {
    snapshot
        .iter()
        .filter(|identity| !blocking_tids.contains(&identity.tid))
        .count()
}

fn peak_count(samples: &[ThreadSnapshot], blocking_tids: &BTreeSet<u32>) -> (usize, usize) {
    samples
        .iter()
        .enumerate()
        .map(|(index, sample)| (index, counted_threads(sample, blocking_tids)))
        .max_by_key(|(_, count)| *count)
        .expect("thread samples include a baseline")
}

fn thread_name_history(snapshots: &[ThreadSnapshot]) -> BTreeMap<u32, BTreeSet<String>> {
    let mut history = BTreeMap::new();
    for identity in snapshots.iter().flatten() {
        history
            .entry(identity.tid)
            .or_insert_with(BTreeSet::new)
            .insert(identity.name.clone());
    }
    history
}

fn format_snapshot(snapshot: &ThreadSnapshot, blocking_tids: &BTreeSet<u32>) -> String {
    snapshot
        .iter()
        .map(|identity| {
            let classification = if blocking_tids.contains(&identity.tid) {
                "[blocking]"
            } else {
                ""
            };
            format!("{}:{}{}", identity.tid, identity.name, classification)
        })
        .collect::<Vec<_>>()
        .join(" ")
}

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
/// iteration. After classifying blocking workers across the complete run, its
/// peak must equal that surface's own pre-loop baseline; spawning `Emitter`
/// legitimately changes the receive-generator baseline.
#[test]
fn generator_iteration_creates_no_os_threads() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("generator_thread_fence.hew");
    let source =
        GENERATOR_THREAD_FENCE_SOURCE.replace("// RAW_THREAD_SNAPSHOT_FN", RAW_THREAD_SNAPSHOT_FN);
    std::fs::write(&path, source).expect("write generator thread-fence fixture");

    let output = run_bounded_hew_run(&path, dir.path());
    assert!(
        output.status.success(),
        "generator thread-fence fixture should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();

    let mut samples = BTreeMap::new();
    for (form, _) in [
        ("genfn", 500),
        ("genblock", 3),
        ("asyncgen", 50),
        ("recvgen", 200),
    ] {
        samples.insert(form, thread_snapshots(&stdout, &format!("samples-{form}")));
    }
    let final_snapshot = single_thread_snapshot(&stdout, "final-thread-snapshot");
    let mut all_snapshots: Vec<_> = samples.values().flatten().cloned().collect();
    all_snapshots.push(final_snapshot);
    let blocking_tids = blocking_worker_tids(&all_snapshots);

    for (form, expected_iters) in [
        ("genfn", 500),
        ("genblock", 3),
        ("asyncgen", 50),
        ("recvgen", 200),
    ] {
        assert_generator_surface_ran_clean(
            &stdout,
            form,
            expected_iters,
            &samples[form],
            &blocking_tids,
            &all_snapshots,
        );
    }
}

/// A surface that yields nothing would trivially show `peak == pre`;
/// requiring the iteration count too means an empty loop cannot pass.
fn assert_generator_surface_ran_clean(
    stdout: &str,
    form: &str,
    expected_iters: i64,
    samples: &[ThreadSnapshot],
    blocking_tids: &BTreeSet<u32>,
    all_snapshots: &[ThreadSnapshot],
) {
    assert_eq!(
        samples.len(),
        expected_iters as usize + 1,
        "{form}: expected one baseline and a sample from every iteration"
    );
    let counts: Vec<_> = samples
        .iter()
        .map(|sample| counted_threads(sample, blocking_tids))
        .collect();
    let pre = counts[0];
    let (peak_index, peak) = peak_count(samples, blocking_tids);
    let iters = value_after_label(stdout, &format!("iters-{form}"));
    assert_eq!(
        iters, expected_iters,
        "{form}: expected {expected_iters} loop iterations, got {iters}; stdout:\n{stdout}"
    );
    assert_eq!(
        peak,
        pre,
        "{form}: generator iteration must create no OS threads (filtered pre={pre}, peak={peak} \
         at sample {peak_index}); raw counts={:?}; filtered counts={counts:?}; \
         peak sample={}; blocking TID/name history={:?}; raw snapshots={}; \
         final classification snapshot={}",
        samples.iter().map(Vec::len).collect::<Vec<_>>(),
        format_snapshot(&samples[peak_index], blocking_tids),
        thread_name_history(all_snapshots),
        samples
            .iter()
            .map(|sample| format!("[{}]", format_snapshot(sample, blocking_tids)))
            .collect::<Vec<_>>()
            .join(" "),
        all_snapshots
            .last()
            .map(|snapshot| format_snapshot(snapshot, blocking_tids))
            .unwrap_or_default(),
    );
}

/// Hew helper shared by the generator and fork thread probes.
const RAW_THREAD_SNAPSHOT_FN: &str = r#"
fn threads() -> string {
    let names = fs.list_dir("/proc/self/task").expect("the task directory is readable");
    var identities = "";
    for name in names {
        match fs.read(f"/proc/self/task/{name}/comm") {
            .Ok(comm) => {
                identities = identities + f" {name}:{comm.trim()}";
            }
            .Err(_) => {
                identities = identities + f" {name}:unknown";
            }
        }
    }
    identities
}
"#;

/// Fixture for `generator_iteration_creates_no_os_threads`: records raw
/// thread identities before each generator loop and on every iteration, and
/// counts iterations so a surface that yields nothing cannot pass vacuously.
const GENERATOR_THREAD_FENCE_SOURCE: &str = r#"
import std.fs;
// RAW_THREAD_SNAPSHOT_FN

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
    var samples_genfn = threads();
    var iters_genfn = 0;
    for v in counter(500) {
        samples_genfn = samples_genfn + "|" + threads();
        iters_genfn = iters_genfn + 1;
        if v < 0 { println("unreachable"); }
    }
    println(f"samples-genfn:{samples_genfn}");
    println("iters-genfn");
    println(iters_genfn);

    // gen {} block
    var samples_genblock = threads();
    var iters_genblock = 0;
    let g = gen {
        yield 1;
        yield 2;
        yield 3;
    };
    for v in g {
        samples_genblock = samples_genblock + "|" + threads();
        iters_genblock = iters_genblock + 1;
        if v < 0 { println("unreachable"); }
    }
    println(f"samples-genblock:{samples_genblock}");
    println("iters-genblock");
    println(iters_genblock);

    // second gen fn + for
    var samples_asyncgen = threads();
    var iters_asyncgen = 0;
    for v in aticks(50) {
        samples_asyncgen = samples_asyncgen + "|" + threads();
        iters_asyncgen = iters_asyncgen + 1;
        if v < 0 { println("unreachable"); }
    }
    println(f"samples-asyncgen:{samples_asyncgen}");
    println("iters-asyncgen");
    println(iters_asyncgen);

    // receive gen fn + for (cross-actor stream producer)
    let e = spawn Emitter;
    var samples_recvgen = threads();
    var iters_recvgen = 0;
    for v in e.ticks(200) {
        samples_recvgen = samples_recvgen + "|" + threads();
        iters_recvgen = iters_recvgen + 1;
        if v < 0 { println("unreachable"); }
    }
    println(f"samples-recvgen:{samples_recvgen}");
    println("iters-recvgen");
    println(iters_recvgen);
    println(f"final-thread-snapshot:{threads()}");
}
"#;

#[test]
fn thread_fence_reclassifies_all_samples_after_worker_names_change() {
    let samples = vec![
        parse_thread_snapshot("1:hew-worker-0"),
        parse_thread_snapshot("1:hew-worker-0 10:hew-worker-0 11:hew-worker-0 12:hew-worker-0"),
        parse_thread_snapshot("1:hew-worker-0 13:hew-worker-0"),
    ];
    let final_snapshot = parse_thread_snapshot(
        "1:hew-worker-0 10:hew-blocking-0 11:hew-blocking-1 12:hew-blocking-2",
    );
    let mut all_samples = samples.clone();
    all_samples.push(final_snapshot);
    let blocking_tids = blocking_worker_tids(&all_samples);
    let counts: Vec<_> = samples
        .iter()
        .map(|sample| counted_threads(sample, &blocking_tids))
        .collect();

    assert_eq!(samples.iter().map(Vec::len).collect::<Vec<_>>(), [1, 4, 2]);
    assert_eq!(counts, [1, 1, 2]);
    assert!(*counts[1..].iter().max().unwrap() > counts[0]);
    assert_eq!(blocking_tids, BTreeSet::from([10, 11, 12]));
}

#[test]
fn thread_fence_peak_keeps_baseline_when_threads_leave() {
    let samples = vec![
        parse_thread_snapshot("1:hew-worker-0 2:producer"),
        parse_thread_snapshot("1:hew-worker-0"),
    ];
    let (peak_index, peak) = peak_count(&samples, &BTreeSet::new());

    assert_eq!((peak_index, peak), (0, 2));
}

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
// RAW_THREAD_SNAPSHOT_FN

fn napper() {{
    sleep(400ms);
}}

fn reporter() {{
    sleep(50ms);
    println("during");
    println(f"fork-during:{{threads()}}");
}}

actor Driver {{ 
    receive fn go() -> i64 {{
        // Warm the shared `hew-timer-tick` thread before sampling the
        // baseline: it is spawned lazily on the first ctx-bearing `sleep`
        // (this handler is coroutine-hosted, so this suspends rather than
        // blocking). Without this warm-up, `pre` would be sampled before the
        // timer thread exists and `during` after — a false +1 unrelated to
        // fork children.
        sleep(1ms); println("pre"); println(f"fork-pre:{{threads()}}"); scope {{
{napper_forks}            fork {{ reporter() }};
        }};
        0
    }}
}}

fn main() -> i64 {{
    let d = spawn Driver;
    let result = match d.go() {{
        .Ok(v) => v,
        .Err(_e) => 1,
    }};
    println(f"final-thread-snapshot:{{threads()}}");
    result
}}
"#
    )
    .replace("// RAW_THREAD_SNAPSHOT_FN", RAW_THREAD_SNAPSHOT_FN);
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

    let samples = vec![
        single_thread_snapshot(&stdout, "fork-pre"),
        single_thread_snapshot(&stdout, "fork-during"),
        single_thread_snapshot(&stdout, "final-thread-snapshot"),
    ];
    let blocking_tids = blocking_worker_tids(&samples);
    let during = counted_threads(&samples[1], &blocking_tids) as i64;
    let pre = counted_threads(&samples[0], &blocking_tids) as i64;
    (pre, during)
}
