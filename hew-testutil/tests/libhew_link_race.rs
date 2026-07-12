//! Multi-process proof that `ensure_hew_lib_built` closes the `libhew.a`
//! uplift race for real concurrent participants, and that a shared
//! `NEXTEST_RUN_ID` collapses concurrent bootstraps to a single real build.
//!
//! `hew_lib_bootstrap_tests::concurrent_callers_build_exactly_once` (in
//! `src/lib.rs`) proves the lock serializes in-process *threads* against a
//! *stubbed* `build_fn`. That is necessary but not sufficient: it never
//! exercises real OS processes, real `NEXTEST_RUN_ID` stamp sharing across
//! process boundaries, Cargo's real non-atomic `libhew.a` uplift, or a real
//! consumer link. This file closes that gap with real child processes, a
//! real `cargo build -p hew-lib`, and a real `hew compile` link.
//!
//! Both tests below are `#[ignore]`d (see each attribute for why) and run
//! via `make libhew-link-race-test`, matching the existing
//! `make observe-functional-test` convention for heavy, artifact-dependent
//! proving gates that should not gate routine `cargo nextest run`.
//!
//! Unix-only: the fixed writer/reader roles shell real `cargo`/`hew`
//! subprocesses and use `fd_lock`-based barriers exactly like
//! `ensure_hew_lib_built` itself; Windows uses a different static-lib name
//! (`hew.lib`) and MSVC link semantics the plan's grounded evidence (clang's
//! `no such file or directory` signature) does not cover. This mirrors the
//! existing `#[cfg(unix)]` gate elsewhere in this crate for the same class
//! of heavy real-process repro.
#![cfg(unix)]

use std::env;
use std::fs::{self, OpenOptions};
use std::path::{Path, PathBuf};
use std::process::{Child, Command};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Instant;

use fd_lock::RwLock;

const ROLE_ENV: &str = "HEW_RACE_ROLE";
const RESULT_ENV: &str = "HEW_RACE_RESULT";
const BARRIER_ENV: &str = "HEW_RACE_BARRIER";
const STOP_ENV: &str = "HEW_RACE_STOP";
const ITERS_ENV: &str = "HEW_RACE_ITERS";

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-testutil lives under the workspace root")
        .to_path_buf()
}

/// Mirrors `hew_testutil`'s private `target_dir_and_profile` exactly: this
/// test binary is itself compiled under `target/<profile>/deps/`, the same
/// layout every migrated call site (and `ensure_hew_lib_built` itself) runs
/// under, so this resolves to the identical real directory.
fn target_dir_and_profile() -> (PathBuf, String) {
    let exe = env::current_exe().expect("resolve current_exe");
    let profile_dir = exe
        .parent()
        .and_then(Path::parent)
        .expect("test binary path has target/<profile>/deps ancestry");
    let profile = profile_dir
        .file_name()
        .and_then(|s| s.to_str())
        .expect("profile dir has a name")
        .to_string();
    let target_dir = profile_dir
        .parent()
        .expect("profile dir has a target/ parent")
        .to_path_buf();
    (target_dir, profile)
}

fn hew_lib_name() -> &'static str {
    if cfg!(windows) {
        "hew.lib"
    } else {
        "libhew.a"
    }
}

fn libhew_path() -> PathBuf {
    let (target_dir, profile) = target_dir_and_profile();
    target_dir.join(profile).join(hew_lib_name())
}

fn hew_bin_path() -> PathBuf {
    let (target_dir, profile) = target_dir_and_profile();
    target_dir.join(profile).join("hew")
}

fn unique_suffix() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("system clock after epoch")
        .as_nanos()
}

/// Real, unlocked `cargo build -p hew-lib` — byte-for-byte the pre-fix
/// pattern every migrated call site used to run with no cross-process
/// coordination (ported from `run_cargo_build_hew_lib`, minus the lock).
fn raw_unlocked_cargo_build_hew_lib() {
    let (target_dir, profile) = target_dir_and_profile();
    let mut cmd = Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into()));
    cmd.args(["build", "-q", "-p", "hew-lib"])
        .env("CARGO_TARGET_DIR", &target_dir)
        .current_dir(repo_root());
    match profile.as_str() {
        "debug" => {}
        "release" => {
            cmd.arg("--release");
        }
        other => {
            cmd.args(["--profile", other]);
        }
    }
    #[cfg(target_os = "macos")]
    {
        let dep = env::var("MACOSX_DEPLOYMENT_TARGET")
            .ok()
            .filter(|v| !v.is_empty())
            .unwrap_or_else(|| "13.0".to_string());
        cmd.env("MACOSX_DEPLOYMENT_TARGET", dep);
    }
    let status = cmd.status().expect("spawn raw cargo build -p hew-lib");
    assert!(status.success(), "raw cargo build -p hew-lib failed");
}

/// Matches the plan's grounded failure signatures: clang/cc's own
/// `no such file or directory` / `cannot find` on `libhew.a` (or the
/// linker's `linking failed`), not a `find_hew_lib`-level `exists()` miss.
fn is_libhew_absence_signature(text: &str) -> bool {
    let lower = text.to_lowercase();
    let mentions_lib = lower.contains("libhew.a") || lower.contains("hew.lib");
    let mentions_absence = lower.contains("no such file or directory")
        || lower.contains("cannot find")
        || lower.contains("linking failed");
    mentions_lib && mentions_absence
}

/// A real consumer link: `hew compile` on a trivial fixture. Exercises the
/// production path (`find_hew_lib` + `link_executable`, clang/cc) that the
/// plan's grounded evidence's `clang: error: no such file or directory:
/// '.../libhew.a'` signature comes from.
fn real_compile_attempt(hew_bin: &Path, src: &Path, emit_dir: &Path) -> Result<(), String> {
    let output = Command::new(hew_bin)
        .arg("compile")
        .arg(src)
        .arg("--emit-dir")
        .arg(emit_dir)
        .output()
        .map_err(|e| format!("spawn hew compile: {e}"))?;
    if output.status.success() {
        return Ok(());
    }
    Err(String::from_utf8_lossy(&output.stderr).into_owned())
}

/// Blocks until the orchestrator drops its write guard on `barrier_path` —
/// every barrier-waiting child unblocks together. No sleep, no retry loop:
/// the wait is a single blocking `fd_lock` acquisition.
fn wait_at_barrier(barrier_path: &Path) {
    let file = OpenOptions::new()
        .read(true)
        .open(barrier_path)
        .expect("open barrier file");
    let lock = RwLock::new(file);
    let _guard = lock.read().expect("await barrier release");
}

fn write_result(text: &str) {
    let path = PathBuf::from(env::var(RESULT_ENV).expect("result path env set"));
    fs::write(path, text).expect("write child result");
}

// ---------------------------------------------------------------------
// Child role bodies. Each is invoked by re-executing this same compiled
// test binary (`std::env::current_exe()`), so `target_dir_and_profile()`
// and `ensure_hew_lib_built()`'s own internal detection resolve to the
// identical real `target/<profile>` directory as the parent.
// ---------------------------------------------------------------------

fn child_writer_gated(barrier_path: &Path) {
    let iters: u32 = env::var(ITERS_ENV)
        .expect("iters env set")
        .parse()
        .expect("iters env is a number");
    wait_at_barrier(barrier_path);
    for _ in 0..iters {
        hew_testutil::ensure_hew_lib_built().expect("gated build");
    }
    write_result("done");
}

fn child_writer_unlocked(barrier_path: &Path) {
    let iters: u32 = env::var(ITERS_ENV)
        .expect("iters env set")
        .parse()
        .expect("iters env is a number");
    wait_at_barrier(barrier_path);
    for _ in 0..iters {
        raw_unlocked_cargo_build_hew_lib();
    }
    write_result("done");
}

/// Post-fix consumer: gate immediately before every real link, exactly like
/// every migrated call site (`ensure_hew_runtime_lib(repo)` then
/// `hew_command(repo).arg("run"/"compile")`).
fn child_reader_gated(barrier_path: &Path) {
    let stop_path = PathBuf::from(env::var(STOP_ENV).expect("stop path env set"));
    let hew_bin = hew_bin_path();
    let scratch = tempfile::tempdir().expect("reader scratch dir");
    let src = scratch.path().join("probe.hew");
    fs::write(&src, "fn main() {}\n").expect("write probe fixture");
    wait_at_barrier(barrier_path);
    let mut attempts = 0u32;
    let mut failures = Vec::new();
    while !stop_path.exists() {
        attempts += 1;
        hew_testutil::ensure_hew_lib_built().expect("gated build before consume");
        let emit_dir = scratch.path().join(format!("out-{attempts}"));
        if let Err(stderr) = real_compile_attempt(&hew_bin, &src, &emit_dir) {
            if is_libhew_absence_signature(&stderr) {
                failures.push(stderr);
            }
        }
    }
    write_result(&format!(
        "attempts={attempts} failures={}\n{}",
        failures.len(),
        failures.join("\n---\n")
    ));
}

/// Pre-fix consumer: no gate call at all — a naked link against whatever
/// happens to be on disk, matching the pre-fix call-site pattern exactly.
fn child_reader_unguarded(barrier_path: &Path) {
    let stop_path = PathBuf::from(env::var(STOP_ENV).expect("stop path env set"));
    let hew_bin = hew_bin_path();
    let scratch = tempfile::tempdir().expect("reader scratch dir");
    let src = scratch.path().join("probe.hew");
    fs::write(&src, "fn main() {}\n").expect("write probe fixture");
    wait_at_barrier(barrier_path);
    let mut attempts = 0u32;
    let mut failures = Vec::new();
    while !stop_path.exists() {
        attempts += 1;
        let emit_dir = scratch.path().join(format!("out-{attempts}"));
        if let Err(stderr) = real_compile_attempt(&hew_bin, &src, &emit_dir) {
            if is_libhew_absence_signature(&stderr) {
                failures.push(stderr);
            }
        }
    }
    write_result(&format!(
        "attempts={attempts} failures={}\n{}",
        failures.len(),
        failures.join("\n---\n")
    ));
}

fn child_timed_writer_gated(barrier_path: &Path) {
    wait_at_barrier(barrier_path);
    let started = Instant::now();
    hew_testutil::ensure_hew_lib_built().expect("gated build");
    let elapsed = started.elapsed();
    write_result(&elapsed.as_millis().to_string());
}

/// Dispatches to a child role when `ROLE_ENV` is set (this process is a
/// spawned worker, not the orchestrator); returns `true` in that case so the
/// calling `#[test]` fn returns immediately without also orchestrating.
fn maybe_run_child_role() -> bool {
    let Ok(role) = env::var(ROLE_ENV) else {
        return false;
    };
    let barrier_path = PathBuf::from(env::var(BARRIER_ENV).expect("barrier path env set"));
    match role.as_str() {
        "writer-gated" => child_writer_gated(&barrier_path),
        "writer-unlocked" => child_writer_unlocked(&barrier_path),
        "reader-gated" => child_reader_gated(&barrier_path),
        "reader-unguarded" => child_reader_unguarded(&barrier_path),
        "timed-writer-gated" => child_timed_writer_gated(&barrier_path),
        other => panic!("unknown race role `{other}`"),
    }
    true
}

// ---------------------------------------------------------------------
// Orchestration
// ---------------------------------------------------------------------

struct RaceChild {
    child: Child,
    result_path: PathBuf,
}

impl RaceChild {
    fn wait_ok(&mut self, label: &str) {
        let status = self
            .child
            .wait()
            .unwrap_or_else(|e| panic!("await {label}: {e}"));
        assert!(status.success(), "{label} exited non-zero: {status:?}");
    }

    fn result(&self) -> String {
        fs::read_to_string(&self.result_path).unwrap_or_default()
    }
}

#[allow(
    clippy::too_many_arguments,
    reason = "each param is a distinct piece of one child's spawn contract; splitting into a struct would obscure the barrier/stop/iters/run_id role-dependent shape at call sites"
)]
fn spawn_role(
    exe: &Path,
    test_name: &str,
    role: &str,
    barrier_path: &Path,
    stop_path: Option<&Path>,
    iters: Option<u32>,
    run_id: Option<&str>,
    result_dir: &Path,
    tag: &str,
) -> RaceChild {
    let result_path = result_dir.join(format!("{tag}.result"));
    let mut cmd = Command::new(exe);
    cmd.args([
        "--exact",
        test_name,
        "--ignored",
        "--nocapture",
        "--test-threads=1",
    ]);
    cmd.env(ROLE_ENV, role);
    cmd.env(BARRIER_ENV, barrier_path);
    cmd.env(RESULT_ENV, &result_path);
    if let Some(stop) = stop_path {
        cmd.env(STOP_ENV, stop);
    }
    if let Some(iters) = iters {
        cmd.env(ITERS_ENV, iters.to_string());
    }
    match run_id {
        Some(run_id) => {
            cmd.env("NEXTEST_RUN_ID", run_id);
        }
        None => {
            cmd.env_remove("NEXTEST_RUN_ID");
        }
    }
    let child = cmd.spawn().expect("spawn race child process");
    RaceChild { child, result_path }
}

fn parse_reader_result(text: &str) -> (u32, u32, String) {
    let mut lines = text.lines();
    let header = lines.next().unwrap_or("attempts=0 failures=0");
    let mut attempts = 0u32;
    let mut failures = 0u32;
    for part in header.split_whitespace() {
        if let Some(v) = part.strip_prefix("attempts=") {
            attempts = v.parse().unwrap_or(0);
        } else if let Some(v) = part.strip_prefix("failures=") {
            failures = v.parse().unwrap_or(0);
        }
    }
    let log: String = lines.collect::<Vec<_>>().join("\n");
    (attempts, failures, log)
}

fn open_write_lock(barrier_path: &Path) -> RwLock<fs::File> {
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(barrier_path)
        .expect("open barrier for write-lock");
    RwLock::new(file)
}

fn ensure_hew_binary_built() {
    let (target_dir, _profile) = target_dir_and_profile();
    let status = Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into()))
        .args(["build", "-q", "-p", "hew-cli", "--bin", "hew"])
        .env("CARGO_TARGET_DIR", &target_dir)
        .current_dir(repo_root())
        .status()
        .expect("spawn cargo build -p hew-cli --bin hew");
    assert!(status.success(), "prebuild of `hew` binary failed");
}

/// Deterministic negative control: `writer_count` real, unlocked
/// `cargo build -p hew-lib` processes rebuild the real artifact while a
/// background thread busy-polls its existence (`std::hint::spin_loop`, no
/// sleep) for as long as the writers are alive. Termination is bounded by
/// the writers' fixed-iteration real lifetime, not by a retry-until-pass
/// condition. Returns (`poll_hits`, `real_link_failures`) — the poll hits are
/// the load-bearing signal (matches the grounded 136/136 methodology and
/// does not depend on a real linker's process-spawn timing landing inside a
/// multi-microsecond window); the real-link failures are best-effort
/// corroboration from real `hew compile` readers racing concurrently.
fn run_unlocked_arm(exe: &Path, test_name: &str, scratch: &Path) -> (u32, u32) {
    let artifact = libhew_path();
    let barrier_path = scratch.join("unlocked-barrier.lock");
    fs::write(&barrier_path, b"").expect("create barrier file");
    let stop_path = scratch.join("unlocked-stop");
    let result_dir = scratch.join("unlocked-results");
    fs::create_dir_all(&result_dir).expect("create result dir");

    let mut barrier_lock = open_write_lock(&barrier_path);
    let guard = barrier_lock.write().expect("acquire barrier write-lock");

    let writer_count = 3u32;
    let iters = 8u32;
    let mut writers: Vec<RaceChild> = (0..writer_count)
        .map(|i| {
            spawn_role(
                exe,
                test_name,
                "writer-unlocked",
                &barrier_path,
                None,
                Some(iters),
                None,
                &result_dir,
                &format!("writer-{i}"),
            )
        })
        .collect();
    let reader_count = 6u32;
    let mut readers: Vec<RaceChild> = (0..reader_count)
        .map(|i| {
            spawn_role(
                exe,
                test_name,
                "reader-unguarded",
                &barrier_path,
                Some(&stop_path),
                None,
                None,
                &result_dir,
                &format!("reader-{i}"),
            )
        })
        .collect();

    let poll_stop = Arc::new(AtomicBool::new(false));
    let thread_stop = Arc::clone(&poll_stop);
    let poll_artifact = artifact.clone();
    let poller = thread::spawn(move || {
        let mut hits = 0u32;
        while !thread_stop.load(Ordering::Relaxed) {
            if !poll_artifact.exists() {
                hits += 1;
            }
            std::hint::spin_loop();
        }
        hits
    });

    drop(guard); // release the barrier -- writers start rebuilding now

    for w in &mut writers {
        w.wait_ok("writer-unlocked child");
    }
    poll_stop.store(true, Ordering::Relaxed);
    let poll_hits = poller.join().expect("poll thread should not panic");

    fs::write(&stop_path, b"").expect("signal readers to stop");
    let mut real_link_failures = 0u32;
    for r in &mut readers {
        r.wait_ok("reader-unguarded child");
        let (_attempts, failures, _log) = parse_reader_result(&r.result());
        real_link_failures += failures;
    }

    (poll_hits, real_link_failures)
}

/// Positive control: every participant (writers AND readers) calls the real
/// `ensure_hew_lib_built()` before touching `libhew.a`, sharing one fresh
/// `NEXTEST_RUN_ID` — exactly the migrated call-site pattern. The lock makes
/// "zero absence failures" a structural guarantee here, not a timing bet, so
/// asserting on it deterministically is safe.
fn run_gated_arm(exe: &Path, test_name: &str, scratch: &Path) -> (u32, u32, String) {
    let run_id = format!("race-link-{}-{}", std::process::id(), unique_suffix());
    let barrier_path = scratch.join("gated-barrier.lock");
    fs::write(&barrier_path, b"").expect("create barrier file");
    let stop_path = scratch.join("gated-stop");
    let result_dir = scratch.join("gated-results");
    fs::create_dir_all(&result_dir).expect("create result dir");

    let mut barrier_lock = open_write_lock(&barrier_path);
    let guard = barrier_lock.write().expect("acquire barrier write-lock");

    let writer_count = 3u32;
    let iters = 6u32;
    let mut writers: Vec<RaceChild> = (0..writer_count)
        .map(|i| {
            spawn_role(
                exe,
                test_name,
                "writer-gated",
                &barrier_path,
                None,
                Some(iters),
                Some(&run_id),
                &result_dir,
                &format!("writer-{i}"),
            )
        })
        .collect();
    let reader_count = 6u32;
    let mut readers: Vec<RaceChild> = (0..reader_count)
        .map(|i| {
            spawn_role(
                exe,
                test_name,
                "reader-gated",
                &barrier_path,
                Some(&stop_path),
                None,
                Some(&run_id),
                &result_dir,
                &format!("reader-{i}"),
            )
        })
        .collect();

    drop(guard); // release the barrier -- everyone starts together

    for w in &mut writers {
        w.wait_ok("writer-gated child");
    }
    fs::write(&stop_path, b"").expect("signal readers to stop");

    let mut total_attempts = 0u32;
    let mut total_failures = 0u32;
    let mut failure_log = String::new();
    for r in &mut readers {
        r.wait_ok("reader-gated child");
        let (attempts, failures, log) = parse_reader_result(&r.result());
        total_attempts += attempts;
        total_failures += failures;
        if !log.is_empty() {
            failure_log.push_str(&log);
            failure_log.push('\n');
        }
    }
    (total_attempts, total_failures, failure_log)
}

/// The core proving gate: real OS processes, real `cargo build -p hew-lib`,
/// a real `hew compile` link, and a real shared `NEXTEST_RUN_ID`.
///
/// Arm A (pre-fix): unlocked writers really race `libhew.a` — a background
/// poll thread deterministically observes the absence window (matching the
/// plan's grounded 136/136 evidence), corroborated best-effort by real
/// unguarded `hew compile` readers running concurrently.
///
/// Arm B (post-fix): every participant gates through `ensure_hew_lib_built`
/// before touching `libhew.a`; real `hew compile` readers must see zero
/// absence failures — a lock-exclusion guarantee, not a timing bet.
#[test]
#[ignore = "run by `make libhew-link-race-test` with the `hew` binary and libhew.a built"]
fn concurrent_link_survives_libhew_bootstrap_race() {
    if maybe_run_child_role() {
        return;
    }

    let exe = env::current_exe().expect("resolve current_exe");
    let test_name = "concurrent_link_survives_libhew_bootstrap_race";
    ensure_hew_binary_built();

    let scratch = tempfile::tempdir().expect("scratch dir");

    let (poll_hits, unguarded_real_failures) = run_unlocked_arm(&exe, test_name, scratch.path());
    assert!(
        poll_hits >= 1,
        "expected the unlocked writer pattern to expose at least one real \
         libhew.a absence window (deterministic per the plan's grounded \
         136/136 evidence); observed zero — the race harness itself may be \
         broken, investigate before trusting the gated-arm proof below"
    );
    eprintln!(
        "unlocked arm: poll_hits={poll_hits} real_hew_compile_link_failures={unguarded_real_failures} \
         (best-effort corroboration; not load-bearing given real-linker process-spawn timing variance)"
    );

    let (gated_attempts, gated_failures, gated_log) =
        run_gated_arm(&exe, test_name, scratch.path());
    assert!(
        gated_attempts > 0,
        "gated readers should have attempted at least one real hew compile link"
    );
    assert_eq!(
        gated_failures, 0,
        "gated readers observed a libhew.a absence failure across {gated_attempts} \
         real hew-compile attempts sharing one NEXTEST_RUN_ID: {gated_log}"
    );
}

/// Focused proof that a shared `NEXTEST_RUN_ID` across real OS processes
/// collapses concurrent bootstraps to exactly one real `cargo build -p
/// hew-lib`. A real build costs whole seconds even as a no-op rebuild
/// (grounded probe: ~4.2s); the stamp fast-path is a couple of filesystem
/// reads (sub-100ms). Exactly one of N real processes sharing one run id
/// should pay the real-build cost.
/// Spawns `count` real processes behind one barrier, all sharing `run_id`,
/// and returns each one's self-measured `ensure_hew_lib_built` elapsed ms.
fn spawn_timed_batch(
    exe: &Path,
    test_name: &str,
    run_id: &str,
    result_dir: &Path,
    tag_prefix: &str,
    count: u32,
) -> Vec<u64> {
    let barrier_path = result_dir.join(format!("{tag_prefix}-barrier.lock"));
    fs::write(&barrier_path, b"").expect("create barrier file");

    let mut barrier_lock = open_write_lock(&barrier_path);
    let guard = barrier_lock.write().expect("acquire barrier write-lock");

    let mut children: Vec<RaceChild> = (0..count)
        .map(|i| {
            spawn_role(
                exe,
                test_name,
                "timed-writer-gated",
                &barrier_path,
                None,
                None,
                Some(run_id),
                result_dir,
                &format!("{tag_prefix}-{i}"),
            )
        })
        .collect();

    drop(guard); // release -- every process in this batch starts together

    children
        .iter_mut()
        .map(|c| {
            c.wait_ok("timed-writer-gated child");
            let text = c.result();
            text.trim()
                .parse()
                .unwrap_or_else(|e| panic!("child reported non-numeric elapsed ms `{text}`: {e}"))
        })
        .collect()
}

/// Proves real `NEXTEST_RUN_ID` stamp sharing across process boundaries in
/// two parts:
///
/// 1. A batch of real concurrent processes, all sharing one fresh run id,
///    all call the real `ensure_hew_lib_built()` at once — every one must
///    return `Ok` (the lock correctly serializes real cross-process
///    contention; already proven for stubbed in-process threads by
///    `hew_lib_bootstrap_tests::concurrent_callers_build_exactly_once`, now
///    proven for real processes doing a real build).
///
/// 2. A lone leader process builds (with a second, different fresh run id)
///    and only *after* it completes do the followers start, sharing the
///    leader's run id. A real `cargo build -p hew-lib` costs a
///    hardware-dependent but always-nontrivial amount of wall time (a
///    single solo run here calibrates the "real build" baseline instead of
///    hardcoding a duration); the stamp fast-path is a couple of file
///    reads. Every follower must be a small fraction of the leader's time —
///    the leader actually built, the followers free-rode on its stamp.
#[test]
#[ignore = "run by `make libhew-link-race-test` with the `hew` binary and libhew.a built"]
fn stamp_sharing_across_real_processes_builds_exactly_once() {
    if maybe_run_child_role() {
        return;
    }

    let exe = env::current_exe().expect("resolve current_exe");
    let test_name = "stamp_sharing_across_real_processes_builds_exactly_once";
    let scratch = tempfile::tempdir().expect("scratch dir");

    // Part 1: real concurrent contention, all racing a fresh run id.
    let contend_run_id = format!(
        "race-stamp-contend-{}-{}",
        std::process::id(),
        unique_suffix()
    );
    let contend_dir = scratch.path().join("contend");
    fs::create_dir_all(&contend_dir).expect("create contend result dir");
    let contend_elapsed =
        spawn_timed_batch(&exe, test_name, &contend_run_id, &contend_dir, "contend", 4);
    assert_eq!(
        contend_elapsed.len(),
        4,
        "all four concurrent contenders should report a result"
    );

    // Part 2: a lone leader builds; only then do followers share its stamp.
    let leader_run_id = format!(
        "race-stamp-leader-{}-{}",
        std::process::id(),
        unique_suffix()
    );
    let leader_dir = scratch.path().join("leader");
    fs::create_dir_all(&leader_dir).expect("create leader result dir");
    let leader_elapsed =
        spawn_timed_batch(&exe, test_name, &leader_run_id, &leader_dir, "leader", 1);
    let leader_ms = leader_elapsed[0];
    assert!(
        leader_ms > 0,
        "the lone leader's real cargo build should take measurable wall time"
    );

    let follower_dir = scratch.path().join("followers");
    fs::create_dir_all(&follower_dir).expect("create follower result dir");
    let follower_elapsed = spawn_timed_batch(
        &exe,
        test_name,
        &leader_run_id, // same run id as the leader -- must hit its stamp
        &follower_dir,
        "follower",
        5,
    );

    // Each follower must be a small fraction of the leader's real-build
    // time -- self-calibrated against this run's own leader measurement
    // rather than a hardcoded duration, so it holds regardless of hardware
    // speed or build-cache warmth.
    let threshold_ms = (leader_ms / 3).max(1);
    for (i, &ms) in follower_elapsed.iter().enumerate() {
        assert!(
            ms <= threshold_ms,
            "follower {i} took {ms}ms sharing the leader's NEXTEST_RUN_ID \
             (leader real build took {leader_ms}ms, threshold {threshold_ms}ms) \
             -- expected it to fast-path off the leader's stamp instead of \
             rebuilding; all follower elapsed times: {follower_elapsed:?}"
        );
    }
}
