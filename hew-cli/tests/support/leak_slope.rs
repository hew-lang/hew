//! Shared leak-oracle slope harness — the single authority for the
//! per-iteration leak-slope methodology the `*_leak_oracle.rs` integration
//! tests build on.
//!
//! ## Why slope, not single-shot exact-zero
//!
//! A single `leaks --atExit` measurement is nondeterministic: transient
//! allocations, attach races, and constant root-set baseline noise let an exact
//! `0 leaks for 0 total leaked bytes` assertion pass (or fail) spuriously, so an
//! exact-zero gate cannot be trusted to catch a real per-iteration leak. The
//! trustworthy signal is the PER-ITERATION SLOPE: compile the SAME shape at a
//! LOW and a HIGH iteration count, measure leak NODE counts under the
//! poisoned-allocator triple, and assert the delta stays within a small
//! constant (`high_leaks <= low_leaks + tolerance`). The delta cancels the
//! constant baseline noise; a genuine per-iteration leak shows as a positive
//! slope that scales with the iteration count.
//!
//! This module consolidates the previously per-file copies of `compile_to_native`
//! / `measure_leaks` / `assert_frame_slope_below_tolerance` into one place so the
//! slope logic cannot drift between oracles. The poisoned-allocator scribble
//! primitive ([`run_under_malloc_scribble`]) is shared for the double-free /
//! use-after-free correctness pins that accompany the slope assertions.
//!
//! ## No configuration may report success without measuring
//!
//! This harness used to early-return green three separate ways: on a non-macOS
//! host, on a macOS host with no `leaks(1)` on `PATH`, and whenever `leaks`
//! declined to attach or timed out. Every one of them logged `skip:` to stderr
//! and returned, so the caller's `#[test]` reported PASS having asserted
//! nothing — the same absent-capability-reads-as-success shape deleted from
//! `try_require_wasi_runner` (see [`super::require_wasi_runner`]).
//!
//! The property this module now holds is: **a leak oracle either measures
//! something or it does not report success.** Concretely:
//!
//!   * Host platform is a COMPILE-TIME fact, so it is gated at compile time.
//!     Every test that reaches this harness carries
//!     `#[cfg_attr(not(target_os = "macos"), ignore = "…")]`, which nextest and
//!     libtest both record as a SKIP with its reason in the run summary — a
//!     visible, counted outcome rather than a silent pass.
//!     [`require_leaks_tool`] fails closed if one is ever missed.
//!   * A macOS host with no `leaks(1)`, or a `leaks(1)` that declines to attach
//!     / times out / emits no summary, is a PROVISIONING FAILURE and panics.
//!   * A probe that compiled and ran but did not perform the work the shape
//!     describes cannot count either: [`assert_frame_slope_below_tolerance_with`]
//!     asserts a WORK WITNESS — a plain run of the probe, which must terminate
//!     under its own control (never by a signal, never past the timeout) and
//!     whose stdout line count must not shrink from LOW to HIGH — before it
//!     trusts the leak numbers. Shapes whose output volume scales with the frame
//!     count take the stronger
//!     [`assert_frame_slope_below_tolerance_exact_lines`], which pins the exact
//!     printed line count at both frame counts. Without a witness, a probe that
//!     deadlocks at HIGH reports a low leak count and reads as a flat slope —
//!     exactly how the `for_await_stream_bytes` shape passed while draining zero
//!     frames at its HIGH probe.
//!
//! ## What this costs, and what covers the surface elsewhere
//!
//! These oracles are macOS-only and always were: `leaks(1)` and the
//! `MallocScribble` / `MallocPreScribble` / `MallocGuardEdges` triple are Darwin
//! facilities, and on another platform the env vars are ignored, so a
//! use-after-free probe reads intact memory and "passes" without detecting
//! anything. Recording that as a counted skip does not remove Linux coverage —
//! the old early return fired before compiling anything, so Linux was already
//! measuring nothing. It makes the gap visible in the run summary instead of
//! invisible in a green tick.
//!
//! What genuinely covers generated-code leaks on Linux:
//!
//!   * `.github/workflows/nightly-sanitizers.yml` — the `compiled-fixture-asan`
//!     job (ubuntu, `make asan-fixtures` → `scripts/asan-fixture-check.sh`)
//!     builds an `ASan` `hew` and runs compiled `.hew` fixtures under
//!     `LeakSanitizer`, so leaks in GENERATED code are caught, plus
//!     `rust-runtime-asan` for `hew-runtime` itself. Both are scheduled daily,
//!     not per-PR, and cover a fixed handful of fixtures rather than the shapes
//!     enumerated here.
//!   * The `hew-mir` / `hew-codegen-rs` unit suites, which run on every host and
//!     pin drop EMISSION structurally rather than observing an allocator — see
//!     `hew-mir/tests/lowering_expr/funcupdate_field_override_release.rs` for the
//!     pattern. A shape whose ownership invariant can be stated as "which drop
//!     instruction is emitted where" belongs there, because that assertion is
//!     platform-independent and runs per-PR. That is the durable answer to a
//!     leak bar that would otherwise live on one developer's laptop.

#![allow(
    dead_code,
    reason = "shared leak-oracle helpers are not used by every test target"
)]

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::time::Duration;

use super::{describe_output, hew_binary, repo_root, require_codegen, try_run_bounded_command};

/// Hard wall-clock deadline for each macOS `leaks(1)` inspection.
///
/// Restricted CI processes are not debuggable, and `leaks --atExit` can hang
/// indefinitely instead of reporting that restriction. Keep every inspection
/// bounded — but generously: a probe that parks its actors for a few seconds
/// before exiting is normal (the recv shapes sleep 3s in `main`), and exceeding
/// this deadline is now a FAILURE, not a skip. Sized for the slowest probe in
/// the suite with an order of magnitude of headroom for a loaded CI host.
const LEAKS_TIMEOUT: Duration = Duration::from_secs(90);

/// Low iteration count: exercises the per-iteration path enough times to leave
/// the constant-overhead floor while staying cheap to compile and scan.
pub const LOW_FRAMES: usize = 3;

/// High iteration count for the slope check. A per-iteration leak of even one
/// node grows by `HIGH_FRAMES - LOW_FRAMES = 47` nodes here — an order of
/// magnitude above the tolerance.
pub const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-NODE delta between the HIGH and LOW probes. Absorbs
/// the one-off scheduler/runtime allocations that appear only in the HIGH run
/// while still catching a slope of ~0.1 leaks/iteration.
pub const SLOPE_TOLERANCE: usize = 5;

/// Parse the canonical summary emitted by `leaks --atExit`.
///
/// Returns both the leaked node count and total leaked bytes. Callers remain
/// responsible for attaching their own fail-closed diagnostic when no summary
/// is present, because that message should name the probe they were measuring.
pub fn parse_leaks_summary(report: &str) -> Option<(usize, usize)> {
    parse_leaks_summary_line(report).map(|(_, leaks, bytes)| (leaks, bytes))
}

fn parse_leaks_summary_line(report: &str) -> Option<(&str, usize, usize)> {
    report.lines().find_map(|line| {
        let rest = line.strip_prefix("Process ")?;
        if !rest.chars().next().is_some_and(|c| c.is_ascii_digit()) {
            return None;
        }
        let summary = rest.split_once(": ")?.1;
        let mut words = summary.split_whitespace();
        let leaks = words.next()?.parse().ok()?;
        let leak_word = words.next()?;
        if leak_word != "leak" && leak_word != "leaks" {
            return None;
        }
        if words.next()? != "for" {
            return None;
        }
        let bytes = words.next()?.parse().ok()?;
        Some((line, leaks, bytes))
    })
}

/// Compile `source` to a native binary via `hew compile --emit-dir` and return
/// the binary path. Panics with the captured compiler output on failure.
pub fn compile_to_native(source: &str, dir: &Path, name: &str) -> PathBuf {
    let hew_src = dir.join(format!("{name}.hew"));
    std::fs::write(&hew_src, source).expect("write hew source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit-dir utf-8"),
            hew_src.to_str().expect("hew src utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    assert!(
        output.status.success(),
        "hew compile failed for {name}:\n{}",
        describe_output(&output)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let bin = stdout
        .lines()
        .find_map(|l| l.strip_prefix("native: "))
        .unwrap_or_else(|| panic!("no `native:` line for {name}:\n{stdout}"))
        .to_string();
    PathBuf::from(bin)
}

/// One observation of a probe binary: what it did, and what it leaked.
///
/// `program_lines` is the WORK WITNESS — the number of stdout lines the probe
/// itself printed on a plain (non-`leaks`) run that exited successfully. A probe
/// that deadlocked, trapped, or bailed out early prints fewer lines than one
/// that ran the loop under test to completion, and its leak count must not be
/// trusted as a slope sample.
#[derive(Debug, Clone, Copy)]
pub struct LeakProbe {
    /// Leak NODE count from the `Process <pid>: N leak(s) for B total leaked
    /// bytes.` summary.
    pub leak_nodes: usize,
    /// Lines the probe itself printed on its witness run.
    pub program_lines: usize,
}

/// Run `bin` plainly (no `leaks(1)`), require it to terminate under its own
/// control, and return the number of stdout lines it printed.
///
/// This is the work witness every slope measurement is gated on. `leaks --atExit`
/// does NOT propagate the inspected program's fate (measured: a probe returning
/// 92 makes `leaks` exit 0, and a probe killed by a signal does too), so a probe
/// that crashed or never ran its loop is invisible in the leak numbers alone.
/// Running it once on its own is what makes that visible.
///
/// The universal assertions here are deliberately narrow, because probe exit
/// codes are not uniform across the suite — several shapes return an accumulated
/// checksum from `main() -> i64`, so a non-zero status is the RESULT, not a
/// failure. What is never legitimate is a probe that hangs past
/// [`LEAKS_TIMEOUT`] or dies to a signal. Shapes whose output volume scales with
/// the frame count get the stronger witness via
/// [`assert_frame_slope_below_tolerance_exact_lines`], which pins the printed
/// line count at both frame counts and is what catches a probe that parked on
/// backpressure and drained zero frames.
pub fn run_probe_witness(bin: &Path, args: &[&str]) -> usize {
    let mut command = Command::new(bin);
    command.args(args);
    let output = match try_run_bounded_command(
        command,
        format!("run probe {} for its work witness", bin.display()),
        LEAKS_TIMEOUT,
    ) {
        Ok(output) => output,
        Err(error) => panic!(
            "probe {} did not finish within {LEAKS_TIMEOUT:?}: {error}. A probe that never \
             completes has measured nothing; a leak oracle over it must not report success. \
             Common cause: the shape saturates a bounded channel/pipe and parks forever on \
             backpressure with no consumer to drain it.",
            bin.display()
        ),
    };
    assert!(
        output.status.code().is_some(),
        "probe {} was killed by a signal ({}), so it did not reach its own exit and its leak \
         count is not a slope sample:\n{}",
        bin.display(),
        output.status,
        describe_output(&output)
    );
    String::from_utf8_lossy(&output.stdout).lines().count()
}

/// Run `bin` under the poisoned-allocator triple + `leaks --atExit` and return
/// the leak NODE count.
///
/// Panics when `leaks(1)` cannot produce a usable measurement — it declined to
/// attach, exceeded [`LEAKS_TIMEOUT`], or emitted no
/// `Process <pid>: N leak(s) for B total leaked bytes.` summary. Each of those
/// used to return `None` and let the caller early-return green; an oracle that
/// could not measure has established nothing, and reporting that as success is
/// how a leak bar goes quietly hollow.
pub fn measure_leaks(bin: &Path) -> usize {
    measure_leaks_with_args(bin, &[])
}

/// Run [`measure_leaks`] with command-line arguments for a runtime-configurable
/// probe. This lets a slope test compile one binary and exercise it at multiple
/// iteration counts without rebuilding the same program shape.
pub fn measure_leaks_with_args(bin: &Path, args: &[&str]) -> usize {
    require_leaks_tool();
    let mut command = Command::new("leaks");
    command
        .args(["--atExit", "--"])
        .arg(bin)
        .args(args)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1");
    try_measure_leaks_command(command, &bin.display().to_string(), LEAKS_TIMEOUT)
        .unwrap_or_else(|error| panic!("{error}"))
        .0
}

/// Drive one already-configured `leaks(1)` command and return its parsed
/// `(node_count, byte_count)` verdict.
///
/// This is public only so the fail-closed harness selftest can inject commands
/// that model a missing inspector, a declined attach, malformed output, and a
/// timeout. Production callers construct the real Darwin `leaks --atExit`
/// command above. Keeping the verdict logic here means those counterfactuals
/// exercise the same spawn, deadline, attach, and parser path as the live
/// oracle rather than a test-only imitation.
#[doc(hidden)]
pub fn try_measure_leaks_command(
    command: Command,
    subject: &str,
    timeout: Duration,
) -> Result<(usize, usize), String> {
    let output =
        try_run_bounded_command(command, format!("inspect {subject} with leaks(1)"), timeout)
            .map_err(|error| {
                format!(
            "leaks(1) could not inspect {subject} within {timeout:?}: {error}. A leak oracle \
             that cannot measure must not report success — fix the host (a restricted, \
             non-debuggable CI process cannot be inspected) or the probe (one that never \
             exits will always exhaust the deadline)."
        )
            })?;
    if !output.status.success() && output.stdout.is_empty() {
        return Err(format!(
            "leaks(1) declined to attach to {subject}: {}. A leak oracle that cannot measure \
             must not report success — the inspected process is not debuggable on this host.",
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    let report = String::from_utf8_lossy(&output.stdout);
    let Some((line, leaks, bytes)) = parse_leaks_summary_line(&report) else {
        return Err(format!(
            "leaks(1) emitted no `Process <pid>: N leak(s) for B total leaked bytes.` summary \
             for {subject}: stderr=\n{}\nA leak oracle that cannot measure must not report \
             success.",
            String::from_utf8_lossy(&output.stderr)
        ));
    };
    eprintln!("  parsed leak count from line: {line}");
    Ok((leaks, bytes))
}

/// Fail closed unless `leaks(1)` can actually be invoked on this host.
///
/// There is deliberately no `bool`-returning variant. The predecessor
/// (`leaks_supported`) returned `false` for BOTH "not macOS" and "`leaks` is
/// missing", and every caller turned that into an early `return` — so the two
/// were indistinguishable at the call site and both reported PASS.
///
/// The two conditions are not alike and are handled apart:
///
///   * NOT MACOS is a compile-time fact and belongs in a compile-time gate. A
///     test that reaches this harness must carry
///     `#[cfg_attr(not(target_os = "macos"), ignore = "…")]` so the runner
///     RECORDS a skip. Reaching this function off macOS means that attribute
///     is missing, which is a defect in the test — so it panics and names the
///     attribute rather than quietly restoring the old behaviour.
///   * A MISSING `leaks(1)` ON MACOS is a provisioning failure of the host,
///     exactly as a missing `wasmtime` is (`#2826`), and it panics.
pub fn require_leaks_tool() {
    #[cfg(not(target_os = "macos"))]
    panic!(
        "leak oracle reached the leaks(1) harness on a non-macOS host. Platform gating for \
         these oracles is a COMPILE-TIME concern: annotate the test with \
         `#[cfg_attr(not(target_os = \"macos\"), ignore = \"leaks(1) is macOS-only\")]` so \
         the runner records a SKIP. Silently returning success here is what let this whole \
         file report green on Linux and Windows while asserting nothing."
    );

    #[cfg(target_os = "macos")]
    {
        let avail = Command::new("which")
            .arg("leaks")
            .output()
            .is_ok_and(|o| o.status.success());
        assert!(
            avail,
            "`leaks` is not on PATH on this macOS host. That is a provisioning failure, not a \
             reason to pass: `leaks(1)` ships with the Xcode command line tools \
             (`xcode-select --install`). A leak oracle with no allocator inspector must not \
             report success (LESSONS: absent capability is not a green result — see \
             `require_wasi_runner`)."
        );
    }
}

/// Build `shape_name` at [`LOW_FRAMES`] and [`HIGH_FRAMES`], measure leak NODE
/// counts, and assert the delta stays within [`SLOPE_TOLERANCE`]. The canonical
/// entry point — see [`assert_frame_slope_below_tolerance_with`] for an explicit
/// iteration/tolerance override.
pub fn assert_frame_slope_below_tolerance(shape_name: &str, source_fn: fn(usize) -> String) {
    assert_frame_slope_below_tolerance_with(
        shape_name,
        source_fn,
        LOW_FRAMES,
        HIGH_FRAMES,
        SLOPE_TOLERANCE,
    );
}

/// Explicit-parameter variant of [`assert_frame_slope_below_tolerance`].
///
/// Builds the shape at `low_frames` and `high_frames`, measures leak NODE counts
/// under the poisoned-allocator triple, and asserts
/// `high_leaks <= low_leaks + tolerance`. A positive slope above the tolerance
/// means a per-iteration allocation is not being released; the failure message
/// names the excess and the re-run command.
///
/// Before it trusts either leak number it asserts a WORK WITNESS: the HIGH probe
/// must not have printed FEWER lines than the LOW probe. A slope oracle compares
/// two runs of the same shape at different iteration counts, so the HIGH run
/// performs at least as much observable work as the LOW one — always, for every
/// shape in the suite. When it performs LESS, the probe did not run the loop
/// under test (it deadlocked, trapped, or exited early), its leak count is not a
/// sample of the thing being measured, and a flat or negative delta is an
/// artefact rather than a clean bill of health. That is not hypothetical: the
/// `for_await_stream_bytes` shape sent `frames²` items into a bounded pipe, so
/// its HIGH probe parked forever on backpressure and drained ZERO frames while
/// the slope assertion happily compared the resulting numbers.
///
/// Panics (never skips) when `leaks(1)` is unavailable — see
/// [`require_leaks_tool`].
pub fn assert_frame_slope_below_tolerance_with(
    shape_name: &str,
    source_fn: fn(usize) -> String,
    low_frames: usize,
    high_frames: usize,
    tolerance: usize,
) {
    assert_frame_slope_below_tolerance_witnessed(
        shape_name,
        source_fn,
        low_frames,
        high_frames,
        tolerance,
        None,
    );
}

/// [`assert_frame_slope_below_tolerance_with`] plus an EXACT work witness.
///
/// `expected_program_lines(frames)` returns the number of stdout lines the probe
/// must print at that iteration count. Shapes whose loop body prints exactly one
/// line per iteration can pin the drained count precisely instead of settling for
/// the monotonicity check, which turns "the loop ran fewer times than it was
/// asked to" from an invisible artefact into a named failure. Use this wherever
/// the shape's output is a deterministic function of `frames`.
pub fn assert_frame_slope_below_tolerance_exact_lines(
    shape_name: &str,
    source_fn: fn(usize) -> String,
    expected_program_lines: fn(usize) -> usize,
) {
    assert_frame_slope_below_tolerance_witnessed(
        shape_name,
        source_fn,
        LOW_FRAMES,
        HIGH_FRAMES,
        SLOPE_TOLERANCE,
        Some(expected_program_lines),
    );
}

fn assert_frame_slope_below_tolerance_witnessed(
    shape_name: &str,
    source_fn: fn(usize) -> String,
    low_frames: usize,
    high_frames: usize,
    tolerance: usize,
    expected_program_lines: Option<fn(usize) -> usize>,
) {
    require_leaks_tool();
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("leak-slope-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let bin_low = compile_to_native(
        &source_fn(low_frames),
        dir.path(),
        &format!("{shape_name}_low"),
    );
    let bin_high = compile_to_native(
        &source_fn(high_frames),
        dir.path(),
        &format!("{shape_name}_high"),
    );

    let low = LeakProbe {
        program_lines: run_probe_witness(&bin_low, &[]),
        leak_nodes: measure_leaks(&bin_low),
    };
    let high = LeakProbe {
        program_lines: run_probe_witness(&bin_high, &[]),
        leak_nodes: measure_leaks(&bin_high),
    };

    eprintln!(
        "{shape_name}: low_frames={low_frames} low_leaks={} low_lines={} \
         high_frames={high_frames} high_leaks={} high_lines={} tolerance={tolerance}",
        low.leak_nodes, low.program_lines, high.leak_nodes, high.program_lines
    );
    validate_work_witness(
        shape_name,
        low_frames,
        high_frames,
        low,
        high,
        expected_program_lines.map(|expected| (expected(low_frames), expected(high_frames))),
        &bin_low,
        &bin_high,
    )
    .unwrap_or_else(|error| panic!("{error}"));
    assert!(
        high.leak_nodes <= low.leak_nodes + tolerance,
        "{shape_name}: per-iteration leak SLOPE — low_frames={low_frames} low_leaks={}, \
         high_frames={high_frames} high_leaks={}. Excess of {} NODES over the \
         tolerance of {tolerance} indicates a per-iteration allocation is not being released. \
         Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked allocation \
         stack.",
        low.leak_nodes,
        high.leak_nodes,
        high.leak_nodes.saturating_sub(low.leak_nodes + tolerance),
        bin_high.display()
    );
}

/// Validate the observable-work half of a slope verdict.
///
/// Exposed for the fail-closed harness selftest so a missing HIGH witness can
/// be proved red without compiling a deliberately deadlocking Hew program.
/// Live slope oracles call this with the measurements and binaries produced
/// above.
#[doc(hidden)]
#[allow(
    clippy::too_many_arguments,
    reason = "the two slope observations and their frame/bin identities form one verdict"
)]
pub fn validate_work_witness(
    shape_name: &str,
    low_frames: usize,
    high_frames: usize,
    low: LeakProbe,
    high: LeakProbe,
    expected_program_lines: Option<(usize, usize)>,
    bin_low: &Path,
    bin_high: &Path,
) -> Result<(), String> {
    if high.program_lines < low.program_lines {
        return Err(format!(
            "{shape_name}: WORK WITNESS — the HIGH probe ({high_frames} frames) printed {} lines, \
             FEWER than the LOW probe ({low_frames} frames) at {} lines. The HIGH probe did not \
             run the loop under test to completion, so its leak count is not a slope sample and \
             the slope assertion below would be measuring nothing. Common cause: the probe shape \
             saturates a bounded channel/pipe at the higher frame count and parks forever on \
             backpressure with no consumer to drain it. Run `{}` directly to see how far it got.",
            high.program_lines,
            low.program_lines,
            bin_high.display()
        ));
    }
    if let Some((expected_low, expected_high)) = expected_program_lines {
        for (frames, probe, expected, bin) in [
            (low_frames, low, expected_low, bin_low),
            (high_frames, high, expected_high, bin_high),
        ] {
            if probe.program_lines != expected {
                return Err(format!(
                    "{shape_name}: WORK WITNESS — the {frames}-frame probe printed {} lines, \
                     not the {expected} its shape prescribes. The probe did not perform the \
                     work under measurement, so its leak count is not a slope sample. Run `{}` \
                     directly to see how far it got.",
                    probe.program_lines,
                    bin.display()
                ));
            }
        }
    }
    Ok(())
}

/// Run `bin` under the poisoned-allocator triple + `leaks --atExit` and return
/// `(leak_node_count, leaked_bytes)`.
///
/// The byte total matters for the exact-zero pins: a shape that must release
/// everything is checked as `(0, 0)`, and reporting the byte figure alongside
/// the node count makes a partial release readable in the failure. Fails closed
/// on every path that cannot produce a measurement, for the same reason
/// [`measure_leaks`] does.
pub fn measure_leaks_exact(bin: &Path) -> (usize, usize) {
    require_leaks_tool();
    let mut command = Command::new("leaks");
    command
        .args(["--atExit", "--"])
        .arg(bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1");
    try_measure_leaks_command(command, &bin.display().to_string(), LEAKS_TIMEOUT)
        .unwrap_or_else(|error| panic!("{error}"))
}

/// Fail closed unless the macOS poisoned allocator is available.
///
/// The `MallocScribble` / `MallocPreScribble` / `MallocGuardEdges` triple is a
/// Darwin libmalloc facility. On any other platform the environment variables
/// are ignored, so a freed buffer keeps its old contents and a use-after-free
/// probe reads the value it expected — a PASS that proves nothing. Probes built
/// on [`run_under_malloc_scribble`] are therefore macOS-only for the same reason
/// the `leaks(1)` oracles are, and gate on the same compile-time
/// `#[cfg_attr(not(target_os = "macos"), ignore = "…")]` attribute so the runner
/// records a skip. This guard catches a caller that forgot it.
pub fn require_macos_poisoned_allocator() {
    #[cfg(not(target_os = "macos"))]
    panic!(
        "poisoned-allocator probe reached its assertion on a non-macOS host. \
         `MallocScribble` / `MallocPreScribble` / `MallocGuardEdges` are Darwin libmalloc \
         facilities; elsewhere they are ignored and freed memory keeps its contents, so a \
         use-after-free probe passes without detecting anything. Annotate the test with \
         `#[cfg_attr(not(target_os = \"macos\"), ignore = \"poisoned allocator is \
         macOS-only\")]` so the runner records a SKIP instead."
    );
}

/// Run `bin` under the poisoned-allocator triple (`MallocScribble` +
/// `MallocPreScribble` + `MallocGuardEdges`) and return the captured output.
/// Shared primitive for the no-double-free / use-after-free correctness pins:
/// an over-eager drop frees memory the program still owns, which the scribbled
/// allocator turns into an abort (double-free) or a poisoned read.
pub fn run_under_malloc_scribble(bin: &Path) -> Output {
    require_macos_poisoned_allocator();
    let mut command = Command::new(bin);
    command
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1");
    try_run_bounded_command(
        command,
        format!("run {} under the poisoned allocator", bin.display()),
        LEAKS_TIMEOUT,
    )
    .unwrap_or_else(|error| {
        panic!(
            "probe {} did not finish under the poisoned allocator within \
             {LEAKS_TIMEOUT:?}: {error}. A hung or deadlocked memory-safety \
             probe has established nothing and must not report success.",
            bin.display()
        )
    })
}
