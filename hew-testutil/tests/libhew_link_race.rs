//! Multi-process proof that `ensure_hew_lib_built` closes the `libhew.a`
//! uplift race for real concurrent participants, and that a shared
//! `NEXTEST_RUN_ID` collapses concurrent bootstraps to a single real
//! `cargo build -p hew-lib` invocation.
//!
//! `hew_lib_bootstrap_tests::concurrent_callers_build_exactly_once` (in
//! `src/lib.rs`) proves the lock serializes in-process *threads* against a
//! *stubbed* `build_fn`. That is necessary but not sufficient: it never
//! exercises real OS processes, real `NEXTEST_RUN_ID` stamp sharing across
//! process boundaries, Cargo's real non-atomic `libhew.a` uplift, or a real
//! consumer link. This file closes that gap with real child processes, a
//! real `cargo build -p hew-lib`, and a real `hew compile` link.
//!
//! Design invariants (each closes a specific cross-review finding):
//!
//! 1. **Exactly-once cargo invocation, not latency inference.** A shared,
//!    fresh `NEXTEST_RUN_ID` must produce exactly one real `cargo build -p
//!    hew-lib` invocation no matter how many real processes race it. This is
//!    proved by a "cargo spy" wrapper (`CARGO` overridden to a small `sh`
//!    script that atomically records its own invocation via `mktemp`, then
//!    `exec`s the real cargo) and counting records after the fact -- not by
//!    comparing wall-clock elapsed time, which a fast-but-still-real
//!    invocation could pass by accident.
//! 2. **Every real link outcome is classified, none discarded.** Real `hew
//!    compile` attempts are bucketed into `Success` / `Absence` (the
//!    `libhew.a`-missing signature) / `Unexpected` (anything else -- a
//!    genuine bug should never be silently swallowed alongside the race
//!    being studied). The gated arm requires at least one real success and
//!    zero of either failure kind.
//! 3. **A real, acknowledged, bounded barrier -- and a diagnostic, not
//!    mandatory, negative control.** Every spawned writer/reader/poller
//!    signals readiness (about to block on the shared barrier) before the
//!    orchestrator releases it, bounded by a deadline so a child that fails
//!    to start fails the test loudly instead of hanging forever. Real link
//!    operations and child reaps are themselves deadline-bounded, and
//!    cleanup (killing stragglers, signalling any stop file) always runs on
//!    the way out, including through a panic. The unlocked arm's poll-based
//!    absence observation is scheduler-timing-dependent, not a structural
//!    guarantee, so it is reported as a diagnostic rather than asserted;
//!    the deterministic proof lives entirely in the gated arm's structural
//!    (lock-exclusion) guarantees.
//!
//! None of the above uses a sleep, or a retry-until-pass loop, as proof.
//! Bounded polling for readiness/deadlines exists only as a hang backstop.
//!
//! Both tests below are `#[ignore]`d (see each attribute for why) and run
//! via `make libhew-link-race-test`, matching the existing
//! `make observe-functional-test` convention for heavy, artifact-dependent
//! proving gates that should not gate routine `cargo nextest run`.
//!
//! Unix-only: the fixed writer/reader roles shell real `cargo`/`hew`
//! subprocesses, chmod a spy wrapper, and use `fd_lock`-based barriers
//! exactly like `ensure_hew_lib_built` itself; Windows uses a different
//! static-lib name (`hew.lib`) and MSVC link semantics the plan's grounded
//! evidence (clang's `no such file or directory` signature) does not cover.
//! This mirrors the existing `#[cfg(unix)]` gate elsewhere in this crate for
//! the same class of heavy real-process repro.
#![cfg(unix)]

use std::env;
use std::fs::{self, OpenOptions};
use std::io::Read;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};

use fd_lock::RwLock;

const ROLE_ENV: &str = "HEW_RACE_ROLE";
const RESULT_ENV: &str = "HEW_RACE_RESULT";
const BARRIER_ENV: &str = "HEW_RACE_BARRIER";
const READY_ENV: &str = "HEW_RACE_READY";
const STOP_ENV: &str = "HEW_RACE_STOP";
const ITERS_ENV: &str = "HEW_RACE_ITERS";

/// Bounds how long the orchestrator waits for every spawned participant to
/// signal it has reached the shared barrier. A hang backstop, not a proof
/// mechanism: exceeding it always fails the test loudly rather than wedging
/// CI, and never retries.
const READY_ACK_DEADLINE: Duration = Duration::from_secs(30);
/// Bounds how long the orchestrator waits for any single spawned child
/// process to exit once released. Generous enough for several real,
/// cold-cache `cargo build -p hew-lib` invocations in sequence.
const CHILD_WAIT_DEADLINE: Duration = Duration::from_mins(3);
/// Bounds a single real `hew compile` link attempt.
const LINK_DEADLINE: Duration = Duration::from_secs(30);
/// Bounds the one-time prebuild of the `hew` binary itself.
const PREBUILD_DEADLINE: Duration = Duration::from_mins(5);
/// Poll interval used only for bounded-wait backstops (readiness, deadline
/// reaping) -- never as the race's evidentiary basis.
const POLL_SLEEP: Duration = Duration::from_millis(5);

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

fn real_cargo_path() -> PathBuf {
    env::var_os("CARGO").map_or_else(|| PathBuf::from("cargo"), PathBuf::from)
}

/// Real, unlocked `cargo build -p hew-lib` -- byte-for-byte the pre-fix
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

/// Writes a POSIX-sh "cargo spy" wrapper: atomically records this
/// invocation (one file per call, created via `mktemp`'s `O_CREAT|O_EXCL`
/// guarantee so concurrent invocations can never collide or clobber each
/// other's record), then `exec`s the real cargo. Overriding `CARGO` to
/// point here lets a test count real cargo invocations directly instead of
/// inferring them from wall-clock timing -- a fast-but-still-real
/// invocation would pass a latency check, so latency alone never proves
/// the shared-stamp fast path was taken.
fn write_cargo_spy(wrapper_path: &Path, record_dir: &Path, real_cargo: &Path) {
    fs::create_dir_all(record_dir).expect("create cargo-spy record dir");
    let script = format!(
        "#!/bin/sh\n\
         set -e\n\
         mktemp \"{rd}/invoke-XXXXXXXX\" >/dev/null\n\
         exec \"{real}\" \"$@\"\n",
        rd = record_dir.display(),
        real = real_cargo.display(),
    );
    fs::write(wrapper_path, script).expect("write cargo-spy wrapper");
    let mut perms = fs::metadata(wrapper_path)
        .expect("stat cargo-spy wrapper")
        .permissions();
    perms.set_mode(0o755);
    fs::set_permissions(wrapper_path, perms).expect("chmod cargo-spy wrapper");
}

fn count_cargo_spy_invocations(record_dir: &Path) -> usize {
    fs::read_dir(record_dir)
        .expect("read cargo-spy record dir")
        .filter_map(Result::ok)
        .count()
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

/// A classified outcome of one real `hew compile` link attempt. Every
/// attempt lands in exactly one bucket -- no outcome is silently discarded,
/// so a genuine (non-race) bug can never hide behind the race being
/// studied.
enum Classification {
    Success,
    Absence(String),
    Unexpected(String),
}

/// Runs `cmd` to completion, piping stdout/stderr and returning them
/// alongside the exit status, but never blocking past `deadline`: a child
/// that overruns is killed (not left to finish on its own clock) and
/// reported as an error. This is a hang backstop, not the race's proof --
/// `LINK_DEADLINE`/`CHILD_WAIT_DEADLINE` are generous multiples of observed
/// real link/build time.
fn wait_piped_with_deadline(
    mut child: Child,
    deadline: Duration,
) -> Result<(ExitStatus, String, String), String> {
    let mut stdout = child.stdout.take().expect("stdout piped");
    let mut stderr = child.stderr.take().expect("stderr piped");
    let stdout_thread = thread::spawn(move || {
        let mut buf = String::new();
        let _ = stdout.read_to_string(&mut buf);
        buf
    });
    let stderr_thread = thread::spawn(move || {
        let mut buf = String::new();
        let _ = stderr.read_to_string(&mut buf);
        buf
    });
    let status = wait_with_deadline(&mut child, deadline)?;
    let out = stdout_thread
        .join()
        .unwrap_or_else(|_| "[stdout reader thread panicked]".to_string());
    let err = stderr_thread
        .join()
        .unwrap_or_else(|_| "[stderr reader thread panicked]".to_string());
    Ok((status, out, err))
}

/// A real consumer link: `hew compile` on a trivial fixture. Exercises the
/// production path (`find_hew_lib` + `link_executable`, clang/cc) that the
/// plan's grounded evidence's `clang: error: no such file or directory:
/// '.../libhew.a'` signature comes from. Deadline-bound: a hung linker
/// fails the attempt instead of hanging the whole test.
fn real_compile_attempt(hew_bin: &Path, src: &Path, emit_dir: &Path) -> Classification {
    let mut cmd = Command::new(hew_bin);
    cmd.arg("compile")
        .arg(src)
        .arg("--emit-dir")
        .arg(emit_dir)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let child = match cmd.spawn() {
        Ok(c) => c,
        Err(e) => return Classification::Unexpected(format!("spawn hew compile: {e}")),
    };
    match wait_piped_with_deadline(child, LINK_DEADLINE) {
        Ok((status, _stdout, stderr)) => {
            if status.success() {
                Classification::Success
            } else if is_libhew_absence_signature(&stderr) {
                Classification::Absence(stderr)
            } else {
                Classification::Unexpected(stderr)
            }
        }
        Err(e) => Classification::Unexpected(format!("hew compile did not complete: {e}")),
    }
}

/// Waits (bounded, polling, no sleep-as-proof) for a spawned child to exit.
/// A child that outlives `deadline` is killed and reaped, never left
/// running past its test -- this is the hang backstop underlying every
/// child reap and real link attempt in this file.
fn wait_with_deadline(child: &mut Child, deadline: Duration) -> Result<ExitStatus, String> {
    let start = Instant::now();
    loop {
        if let Some(status) = child.try_wait().map_err(|e| format!("try_wait: {e}"))? {
            return Ok(status);
        }
        if start.elapsed() > deadline {
            let _ = child.kill();
            let _ = child.wait();
            return Err(format!("child did not exit within {deadline:?}; killed"));
        }
        thread::sleep(POLL_SLEEP);
    }
}

/// Blocks until the orchestrator drops its write guard on `barrier_path` --
/// every barrier-waiting participant unblocks together. No sleep, no retry
/// loop: the wait is a single blocking `fd_lock` acquisition. Writes a
/// readiness marker immediately beforehand so the orchestrator can confirm
/// (bounded, before releasing) that every participant is at or past this
/// point -- otherwise a slow-to-spawn child could still be mid-exec when a
/// fast one races off alone, undermining the "synchronized start" the race
/// proof depends on.
fn wait_at_barrier(barrier_path: &Path, ready_path: &Path) {
    fs::write(ready_path, b"ready").expect("write ready marker");
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

fn write_reader_result(attempts: u32, successes: u32, absence: &[String], unexpected: &[String]) {
    let mut out = format!(
        "attempts={attempts} successes={successes} absence={} unexpected={}\n",
        absence.len(),
        unexpected.len()
    );
    out.push_str("##ABSENCE##\n");
    out.push_str(&absence.join("\n---\n"));
    out.push_str("\n##UNEXPECTED##\n");
    out.push_str(&unexpected.join("\n---\n"));
    write_result(&out);
}

struct ReaderResult {
    successes: u32,
    absence: u32,
    unexpected: u32,
    unexpected_log: String,
}

fn parse_reader_result(text: &str) -> ReaderResult {
    let mut successes = 0u32;
    let mut absence = 0u32;
    let mut unexpected = 0u32;
    let header = text.lines().next().unwrap_or_default();
    for part in header.split_whitespace() {
        if let Some(v) = part.strip_prefix("successes=") {
            successes = v.parse().unwrap_or(0);
        } else if let Some(v) = part.strip_prefix("absence=") {
            absence = v.parse().unwrap_or(0);
        } else if let Some(v) = part.strip_prefix("unexpected=") {
            unexpected = v.parse().unwrap_or(0);
        }
    }
    let unexpected_log = text
        .split("##UNEXPECTED##\n")
        .nth(1)
        .unwrap_or_default()
        .trim()
        .to_string();
    ReaderResult {
        successes,
        absence,
        unexpected,
        unexpected_log,
    }
}

// ---------------------------------------------------------------------
// Child role bodies. Each is invoked by re-executing this same compiled
// test binary (`std::env::current_exe()`), so `target_dir_and_profile()`
// and `ensure_hew_lib_built()`'s own internal detection resolve to the
// identical real `target/<profile>` directory as the parent.
// ---------------------------------------------------------------------

fn child_writer_gated(barrier_path: &Path, ready_path: &Path) {
    let iters: u32 = env::var(ITERS_ENV)
        .expect("iters env set")
        .parse()
        .expect("iters env is a number");
    wait_at_barrier(barrier_path, ready_path);
    for _ in 0..iters {
        hew_testutil::ensure_hew_lib_built().expect("gated build");
    }
    write_result("done");
}

fn child_writer_unlocked(barrier_path: &Path, ready_path: &Path) {
    let iters: u32 = env::var(ITERS_ENV)
        .expect("iters env set")
        .parse()
        .expect("iters env is a number");
    wait_at_barrier(barrier_path, ready_path);
    for _ in 0..iters {
        raw_unlocked_cargo_build_hew_lib();
    }
    write_result("done");
}

/// Shared reader loop for both consumer roles: `gated` mirrors every
/// migrated call site (gate immediately before every real link); `!gated`
/// mirrors the pre-fix pattern (a naked link against whatever happens to
/// be on disk). Every attempt is classified (`Classification`), never
/// discarded.
fn child_reader(barrier_path: &Path, ready_path: &Path, gated: bool) {
    let stop_path = PathBuf::from(env::var(STOP_ENV).expect("stop path env set"));
    let hew_bin = hew_bin_path();
    let scratch = tempfile::tempdir().expect("reader scratch dir");
    let src = scratch.path().join("probe.hew");
    fs::write(&src, "fn main() {}\n").expect("write probe fixture");
    wait_at_barrier(barrier_path, ready_path);
    let mut attempts = 0u32;
    let mut successes = 0u32;
    let mut absence_log: Vec<String> = Vec::new();
    let mut unexpected_log: Vec<String> = Vec::new();
    while !stop_path.exists() {
        attempts += 1;
        if gated {
            hew_testutil::ensure_hew_lib_built().expect("gated build before consume");
        }
        let emit_dir = scratch.path().join(format!("out-{attempts}"));
        match real_compile_attempt(&hew_bin, &src, &emit_dir) {
            Classification::Success => successes += 1,
            Classification::Absence(text) => absence_log.push(text),
            Classification::Unexpected(text) => unexpected_log.push(text),
        }
    }
    write_reader_result(attempts, successes, &absence_log, &unexpected_log);
}

fn child_timed_writer_gated(barrier_path: &Path, ready_path: &Path) {
    wait_at_barrier(barrier_path, ready_path);
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
    let ready_path = PathBuf::from(env::var(READY_ENV).expect("ready path env set"));
    match role.as_str() {
        "writer-gated" => child_writer_gated(&barrier_path, &ready_path),
        "writer-unlocked" => child_writer_unlocked(&barrier_path, &ready_path),
        "reader-gated" => child_reader(&barrier_path, &ready_path, true),
        "reader-unguarded" => child_reader(&barrier_path, &ready_path, false),
        "timed-writer-gated" => child_timed_writer_gated(&barrier_path, &ready_path),
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

/// Owns a batch of already-spawned race children (writers, readers, or
/// timed builders) for one barrier-synchronized round. Guarantees no child
/// outlives this batch and that `stop_path` (if any) is always signalled --
/// even if an assertion earlier in the test panics, a `while
/// !stop_path.exists()` reader must never spin as an orphan, and a
/// still-running writer must never survive past its test. `reap_all` is the
/// expected path (deadline-bounded, records outcomes without panicking so
/// every child is always fully reaped first); `Drop` is the backstop for
/// any panic that happens before `reap_all` runs (e.g. a readiness-ack
/// timeout while other children are still live).
struct ChildBatch {
    children: Vec<RaceChild>,
    stop_path: Option<PathBuf>,
}

impl ChildBatch {
    fn new(stop_path: Option<PathBuf>) -> Self {
        Self {
            children: Vec::new(),
            stop_path,
        }
    }

    fn push(&mut self, child: RaceChild) {
        self.children.push(child);
    }

    /// Reaps every child (deadline-bounded, kills stragglers), returning
    /// each one's outcome. Never panics mid-loop -- every child is fully
    /// reaped before the caller inspects outcomes and decides whether to
    /// fail, so a single bad child can never orphan its siblings.
    fn reap_all(&mut self, label: &str) -> Vec<(PathBuf, Result<String, String>)> {
        let mut outcomes = Vec::new();
        for mut child in self.children.drain(..) {
            let outcome = match wait_with_deadline(&mut child.child, CHILD_WAIT_DEADLINE) {
                Ok(status) if status.success() => {
                    Ok(fs::read_to_string(&child.result_path).unwrap_or_default())
                }
                Ok(status) => Err(format!("{label} exited non-zero: {status:?}")),
                Err(e) => Err(format!("{label}: {e}")),
            };
            outcomes.push((child.result_path.clone(), outcome));
        }
        outcomes
    }
}

impl Drop for ChildBatch {
    fn drop(&mut self) {
        if let Some(stop) = &self.stop_path {
            let _ = fs::write(stop, b"");
        }
        for mut child in self.children.drain(..) {
            let _ = child.child.kill();
            let _ = child.child.wait();
        }
    }
}

#[allow(
    clippy::too_many_arguments,
    reason = "each param is a distinct piece of one child's spawn contract; splitting into a struct would obscure the barrier/ready/stop/iters/run_id/cargo role-dependent shape at call sites"
)]
fn spawn_role(
    exe: &Path,
    test_name: &str,
    role: &str,
    barrier_path: &Path,
    ready_path: &Path,
    stop_path: Option<&Path>,
    iters: Option<u32>,
    run_id: Option<&str>,
    cargo_override: Option<&Path>,
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
    cmd.env(READY_ENV, ready_path);
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
    if let Some(cargo) = cargo_override {
        cmd.env("CARGO", cargo);
    }
    let child = cmd.spawn().expect("spawn race child process");
    RaceChild { child, result_path }
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
    let mut cmd = Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into()));
    cmd.args(["build", "-q", "-p", "hew-cli", "--bin", "hew"])
        .env("CARGO_TARGET_DIR", &target_dir)
        .current_dir(repo_root())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let child = cmd.spawn().expect("spawn cargo build -p hew-cli --bin hew");
    let (status, _stdout, stderr) = wait_piped_with_deadline(child, PREBUILD_DEADLINE)
        .expect("prebuild of `hew` binary did not complete");
    assert!(
        status.success(),
        "prebuild of `hew` binary failed: {stderr}"
    );
}

/// Waits (bounded, polling, no sleep-as-proof) for every path in
/// `ready_paths` to exist and for `poller_ready` to report true, i.e. for
/// every spawned participant -- process or in-process thread -- to reach
/// the shared barrier. A child that fails to start (spawn/exec error, crash
/// before reaching the barrier) fails the test loudly here instead of the
/// orchestrator racing ahead with a false "everyone is ready" assumption.
fn await_ready(ready_paths: &[PathBuf], poller_ready: impl Fn() -> bool, deadline: Duration) {
    let start = Instant::now();
    loop {
        if ready_paths.iter().all(|p| p.exists()) && poller_ready() {
            return;
        }
        if start.elapsed() > deadline {
            let missing: Vec<_> = ready_paths
                .iter()
                .filter(|p| !p.exists())
                .map(|p| p.display().to_string())
                .collect();
            panic!(
                "timed out after {deadline:?} waiting for participants to reach \
                 the barrier (missing ready markers: {missing:?}, poller_ready={}) \
                 -- a child likely failed to start rather than the race itself \
                 being at fault",
                poller_ready()
            );
        }
        thread::sleep(POLL_SLEEP);
    }
}

/// Spawns `count` writer children sharing one `role`/`iters`/`run_id`/cargo
/// override, recording each one's readiness-marker path into `ready_paths`.
/// Shared by both arms to keep their spawn contracts identical apart from
/// the role/lock behaviour under test.
#[allow(
    clippy::too_many_arguments,
    reason = "each param is a distinct piece of the shared writer spawn contract"
)]
fn spawn_batch_writers(
    exe: &Path,
    test_name: &str,
    role: &str,
    barrier_path: &Path,
    iters: u32,
    run_id: Option<&str>,
    cargo_override: Option<&Path>,
    result_dir: &Path,
    count: u32,
    ready_paths: &mut Vec<PathBuf>,
) -> ChildBatch {
    let mut batch = ChildBatch::new(None);
    for i in 0..count {
        let tag = format!("writer-{i}");
        let ready_path = result_dir.join(format!("{tag}.ready"));
        batch.push(spawn_role(
            exe,
            test_name,
            role,
            barrier_path,
            &ready_path,
            None,
            Some(iters),
            run_id,
            cargo_override,
            result_dir,
            &tag,
        ));
        ready_paths.push(ready_path);
    }
    batch
}

/// Spawns `count` reader children sharing one `role`/`run_id`/cargo override
/// and one `stop_path`, recording each one's readiness-marker path into
/// `ready_paths`. Shared by both arms; see `spawn_batch_writers`.
#[allow(
    clippy::too_many_arguments,
    reason = "each param is a distinct piece of the shared reader spawn contract"
)]
fn spawn_batch_readers(
    exe: &Path,
    test_name: &str,
    role: &str,
    barrier_path: &Path,
    stop_path: &Path,
    run_id: Option<&str>,
    cargo_override: Option<&Path>,
    result_dir: &Path,
    count: u32,
    ready_paths: &mut Vec<PathBuf>,
) -> ChildBatch {
    let mut batch = ChildBatch::new(Some(stop_path.to_path_buf()));
    for i in 0..count {
        let tag = format!("reader-{i}");
        let ready_path = result_dir.join(format!("{tag}.ready"));
        batch.push(spawn_role(
            exe,
            test_name,
            role,
            barrier_path,
            &ready_path,
            Some(stop_path),
            None,
            run_id,
            cargo_override,
            result_dir,
            &tag,
        ));
        ready_paths.push(ready_path);
    }
    batch
}

/// Panics on the first `Err` outcome, naming which child and path it came
/// from. Called only after every child has already been reaped (never
/// mid-reap), so a bad outcome here can never orphan a sibling.
fn panic_on_any_err(label: &str, outcomes: &[(PathBuf, Result<String, String>)]) {
    for (path, outcome) in outcomes {
        if let Err(e) = outcome {
            panic!("{label} at {}: {e}", path.display());
        }
    }
}

/// Aggregated, classified reader outcomes across a whole batch.
struct ReaderTotals {
    successes: u32,
    absence: u32,
    unexpected: u32,
    unexpected_log: String,
}

fn aggregate_reader_outcomes(
    label: &str,
    outcomes: &[(PathBuf, Result<String, String>)],
) -> ReaderTotals {
    let mut totals = ReaderTotals {
        successes: 0,
        absence: 0,
        unexpected: 0,
        unexpected_log: String::new(),
    };
    for (path, outcome) in outcomes {
        match outcome {
            Ok(text) => {
                let r = parse_reader_result(text);
                totals.successes += r.successes;
                totals.absence += r.absence;
                totals.unexpected += r.unexpected;
                if !r.unexpected_log.is_empty() {
                    totals.unexpected_log.push_str(&r.unexpected_log);
                    totals.unexpected_log.push('\n');
                }
            }
            Err(e) => panic!("{label} at {}: {e}", path.display()),
        }
    }
    totals
}

/// Diagnostic result of the unlocked (pre-fix) arm. `poll_hits` is a
/// scheduler-timing observation, not a structural guarantee -- reported,
/// never asserted. The real-link classification counts are best-effort
/// corroboration from real unguarded `hew compile` readers racing
/// concurrently, also non-load-bearing.
struct UnlockedArmResult {
    poll_hits: u64,
    successes: u32,
    absence: u32,
    unexpected: u32,
}

/// Pre-fix arm: `writer_count` real, unlocked `cargo build -p hew-lib`
/// processes rebuild the real artifact while a background thread
/// busy-polls its existence (`std::hint::spin_loop`, no sleep) for as long
/// as the writers are alive, and real unguarded `hew compile` readers race
/// concurrently. Every participant (writers, readers, poller) acknowledges
/// readiness before the shared barrier releases, bounded by
/// `READY_ACK_DEADLINE`. Nothing here is asserted upon by the caller --
/// see `UnlockedArmResult`'s doc comment for why.
fn run_unlocked_arm(exe: &Path, test_name: &str, scratch: &Path) -> UnlockedArmResult {
    let artifact = libhew_path();
    let barrier_path = scratch.join("unlocked-barrier.lock");
    fs::write(&barrier_path, b"").expect("create barrier file");
    let stop_path = scratch.join("unlocked-stop");
    let result_dir = scratch.join("unlocked-results");
    fs::create_dir_all(&result_dir).expect("create result dir");

    let mut barrier_lock = open_write_lock(&barrier_path);
    let guard = barrier_lock.write().expect("acquire barrier write-lock");

    let mut ready_paths = Vec::new();
    let mut writer_batch = spawn_batch_writers(
        exe,
        test_name,
        "writer-unlocked",
        &barrier_path,
        8,
        None,
        None,
        &result_dir,
        3,
        &mut ready_paths,
    );
    let mut reader_batch = spawn_batch_readers(
        exe,
        test_name,
        "reader-unguarded",
        &barrier_path,
        &stop_path,
        None,
        None,
        &result_dir,
        6,
        &mut ready_paths,
    );

    let poll_stop = Arc::new(AtomicBool::new(false));
    let poll_ready = Arc::new(AtomicBool::new(false));
    let thread_stop = Arc::clone(&poll_stop);
    let thread_ready = Arc::clone(&poll_ready);
    let poll_artifact = artifact.clone();
    let poller = thread::spawn(move || {
        thread_ready.store(true, Ordering::Release);
        let mut hits = 0u64;
        while !thread_stop.load(Ordering::Relaxed) {
            if !poll_artifact.exists() {
                hits += 1;
            }
            std::hint::spin_loop();
        }
        hits
    });

    await_ready(
        &ready_paths,
        || poll_ready.load(Ordering::Acquire),
        READY_ACK_DEADLINE,
    );

    drop(guard); // release the barrier -- every participant starts together

    panic_on_any_err(
        "writer-unlocked child",
        &writer_batch.reap_all("writer-unlocked child"),
    );

    poll_stop.store(true, Ordering::Relaxed);
    let poll_hits = poller.join().expect("poll thread should not panic");

    fs::write(&stop_path, b"").expect("signal readers to stop");

    let totals = aggregate_reader_outcomes(
        "reader-unguarded child",
        &reader_batch.reap_all("reader-unguarded child"),
    );

    UnlockedArmResult {
        poll_hits,
        successes: totals.successes,
        absence: totals.absence,
        unexpected: totals.unexpected,
    }
}

/// Deterministic result of the gated (post-fix) arm. Every field here is a
/// structural (lock-exclusion) guarantee, not a timing bet, so the caller
/// asserts on all of them.
struct GatedArmResult {
    successes: u32,
    absence: u32,
    unexpected: u32,
    unexpected_log: String,
    cargo_invocations: usize,
}

/// Post-fix arm: every participant (writers AND readers) calls the real
/// `ensure_hew_lib_built()` before touching `libhew.a`, sharing one fresh
/// `NEXTEST_RUN_ID` -- exactly the migrated call-site pattern. `CARGO` is
/// overridden to a spy wrapper shared by every participant so the real
/// number of `cargo build -p hew-lib` invocations can be counted directly:
/// with the stamp fast path working, 3 writers + 6 readers all racing one
/// fresh run id must still produce exactly one.
fn run_gated_arm(exe: &Path, test_name: &str, scratch: &Path) -> GatedArmResult {
    let run_id = format!("race-link-{}-{}", std::process::id(), unique_suffix());
    let barrier_path = scratch.join("gated-barrier.lock");
    fs::write(&barrier_path, b"").expect("create barrier file");
    let stop_path = scratch.join("gated-stop");
    let result_dir = scratch.join("gated-results");
    fs::create_dir_all(&result_dir).expect("create result dir");

    let cargo_record_dir = scratch.join("gated-cargo-invocations");
    let cargo_spy = scratch.join("gated-cargo-spy.sh");
    write_cargo_spy(&cargo_spy, &cargo_record_dir, &real_cargo_path());

    let mut barrier_lock = open_write_lock(&barrier_path);
    let guard = barrier_lock.write().expect("acquire barrier write-lock");

    let mut ready_paths = Vec::new();
    let mut writer_batch = spawn_batch_writers(
        exe,
        test_name,
        "writer-gated",
        &barrier_path,
        6,
        Some(&run_id),
        Some(&cargo_spy),
        &result_dir,
        3,
        &mut ready_paths,
    );
    let mut reader_batch = spawn_batch_readers(
        exe,
        test_name,
        "reader-gated",
        &barrier_path,
        &stop_path,
        Some(&run_id),
        Some(&cargo_spy),
        &result_dir,
        6,
        &mut ready_paths,
    );

    await_ready(&ready_paths, || true, READY_ACK_DEADLINE);

    drop(guard); // release the barrier -- every participant starts together

    panic_on_any_err(
        "writer-gated child",
        &writer_batch.reap_all("writer-gated child"),
    );

    fs::write(&stop_path, b"").expect("signal readers to stop");

    let totals = aggregate_reader_outcomes(
        "reader-gated child",
        &reader_batch.reap_all("reader-gated child"),
    );

    let cargo_invocations = count_cargo_spy_invocations(&cargo_record_dir);

    GatedArmResult {
        successes: totals.successes,
        absence: totals.absence,
        unexpected: totals.unexpected,
        unexpected_log: totals.unexpected_log,
        cargo_invocations,
    }
}

/// The core proving gate: real OS processes, real `cargo build -p hew-lib`,
/// a real `hew compile` link, and a real shared `NEXTEST_RUN_ID`.
///
/// Arm A (pre-fix, diagnostic): unlocked writers really race `libhew.a` --
/// a background poll thread observes the absence window and real unguarded
/// `hew compile` readers race concurrently. Reported, not asserted: the
/// observation is scheduler-timing-dependent, not a structural guarantee.
///
/// Arm B (post-fix, deterministic): every participant gates through
/// `ensure_hew_lib_built` before touching `libhew.a`; real `hew compile`
/// readers must see at least one success and zero of either failure kind,
/// and exactly one real `cargo build -p hew-lib` invocation must have
/// happened across all 9 participants sharing one run id -- lock-exclusion
/// guarantees, not timing bets.
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

    let unlocked = run_unlocked_arm(&exe, test_name, scratch.path());
    eprintln!(
        "unlocked arm (diagnostic, scheduler-timing-dependent -- not asserted): \
         poll_hits={} real_successes={} real_absence_failures={} \
         real_unexpected_failures={}",
        unlocked.poll_hits, unlocked.successes, unlocked.absence, unlocked.unexpected
    );
    if unlocked.poll_hits == 0 {
        eprintln!(
            "note: the unlocked writer pattern did not expose a libhew.a absence \
             window this run; this reflects scheduler timing, not a structural \
             guarantee, so it is not asserted -- the deterministic proof is the \
             gated arm below"
        );
    }

    let gated = run_gated_arm(&exe, test_name, scratch.path());
    assert!(
        gated.successes >= 1,
        "gated readers should have completed at least one successful real hew \
         compile link across their shared NEXTEST_RUN_ID; observed {} successes",
        gated.successes
    );
    assert_eq!(
        gated.absence, 0,
        "gated readers observed {} libhew.a absence failure(s) despite every \
         participant gating through ensure_hew_lib_built",
        gated.absence
    );
    assert_eq!(
        gated.unexpected, 0,
        "gated readers hit {} unexpected (non-absence) compile/link failure(s): {}",
        gated.unexpected, gated.unexpected_log
    );
    assert_eq!(
        gated.cargo_invocations, 1,
        "expected exactly one real `cargo build -p hew-lib` invocation across 3 \
         writers + 6 readers sharing one NEXTEST_RUN_ID; observed {} -- the \
         shared stamp should short-circuit every caller but the first",
        gated.cargo_invocations
    );
}

/// Proves real `NEXTEST_RUN_ID` stamp sharing across process boundaries by
/// counting real `cargo build -p hew-lib` invocations directly (via the
/// cargo-spy wrapper), not by inferring it from wall-clock latency: a
/// shared, fresh run id raced by several real processes calling the real
/// `ensure_hew_lib_built()` must produce exactly one real cargo invocation,
/// no matter how many processes race it -- the fd_lock-serialized winner
/// performs the real build and writes the stamp; every other racer's
/// re-check under the same lock finds the stamp already fresh and never
/// reaches `build_fn` at all.
#[test]
#[ignore = "run by `make libhew-link-race-test` with the `hew` binary and libhew.a built"]
fn stamp_sharing_across_real_processes_builds_exactly_once() {
    if maybe_run_child_role() {
        return;
    }

    let exe = env::current_exe().expect("resolve current_exe");
    let test_name = "stamp_sharing_across_real_processes_builds_exactly_once";
    let scratch = tempfile::tempdir().expect("scratch dir");

    let run_id = format!("race-stamp-{}-{}", std::process::id(), unique_suffix());
    let record_dir = scratch.path().join("cargo-invocations");
    let spy = scratch.path().join("cargo-spy.sh");
    write_cargo_spy(&spy, &record_dir, &real_cargo_path());

    let barrier_path = scratch.path().join("stamp-barrier.lock");
    fs::write(&barrier_path, b"").expect("create barrier file");
    let result_dir = scratch.path().join("stamp-results");
    fs::create_dir_all(&result_dir).expect("create result dir");

    let mut barrier_lock = open_write_lock(&barrier_path);
    let guard = barrier_lock.write().expect("acquire barrier write-lock");

    let mut batch = ChildBatch::new(None);
    let mut ready_paths = Vec::new();
    let count = 6u32;
    for i in 0..count {
        let tag = format!("contender-{i}");
        let ready_path = result_dir.join(format!("{tag}.ready"));
        batch.push(spawn_role(
            &exe,
            test_name,
            "timed-writer-gated",
            &barrier_path,
            &ready_path,
            None,
            None,
            Some(&run_id),
            Some(&spy),
            &result_dir,
            &tag,
        ));
        ready_paths.push(ready_path);
    }

    await_ready(&ready_paths, || true, READY_ACK_DEADLINE);

    drop(guard); // release the barrier -- every contender starts together

    let outcomes = batch.reap_all("timed-writer-gated child");
    let mut elapsed_ms = Vec::new();
    for (path, outcome) in &outcomes {
        match outcome {
            Ok(text) => {
                let ms: u64 = text.trim().parse().unwrap_or_else(|e| {
                    panic!(
                        "child at {} reported non-numeric elapsed ms `{text}`: {e}",
                        path.display()
                    )
                });
                elapsed_ms.push(ms);
            }
            Err(e) => panic!("timed-writer-gated child at {}: {e}", path.display()),
        }
    }
    eprintln!(
        "stamp sharing: {count} real processes shared one fresh NEXTEST_RUN_ID, \
         elapsed_ms={elapsed_ms:?} (diagnostic only -- the load-bearing proof is \
         the cargo-invocation count below)"
    );

    let invocations = count_cargo_spy_invocations(&record_dir);
    assert_eq!(
        invocations, 1,
        "expected exactly one real `cargo build -p hew-lib` invocation across \
         {count} real processes racing one fresh NEXTEST_RUN_ID through \
         ensure_hew_lib_built; observed {invocations} -- the shared stamp should \
         collapse every caller but the winner onto the fast path with no further \
         cargo invocation"
    );
}
