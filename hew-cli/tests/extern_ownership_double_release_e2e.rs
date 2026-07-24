//! End-to-end proof that a declared `extern` is treated as OWNERSHIP-OPAQUE by
//! the f-string interpolation temp-drop gates.
//!
//! Those gates exist to fix a LEAK. The failure mode these tests
//! guard is strictly worse than the leak: a DOUBLE RELEASE. Both gates used to
//! read `Builder::module_fn_names` as an ownership authority, but that set is
//! seeded with every `HirItem::ExternFn` purely so extern calls lower as
//! `Terminator::Call`. Membership is a call-DISPATCH fact. An extern's real
//! ownership behaviour is unknowable, so:
//!
//!   * an extern's `-> string` result must NOT mint a caller-side owner (the
//!     host may hand back an interior or borrowed handle and release it
//!     itself); and
//!   * an extern's `string` argument must NOT count as a proven borrow (the
//!     host may retain or release the exact handle it is passed, so the
//!     composite's `EnumInPlace` scope-exit drop would be a second release).
//!
//! # Why these tests count releases exactly, rather than watching for a crash
//!
//! Every pre-existing oracle in this repo asserts either a leak SLOPE (macOS
//! `leaks(1)` only) or a clean exit under a poisoned allocator. Neither has
//! teeth for the direction that matters here: a ZERO-release regression --
//! re-admitting the drop the fix removes -- exits cleanly on non-Darwin unix
//! under any allocator setting, because a `free()` of a still-live, still-
//! reachable buffer usually does not fault. So these tests observe the EXACT
//! release count directly.
//!
//! The observation is possible because the `string` handle Hew hands an extern
//! is the real, non-copied runtime handle: `hew-cabi` pins a 16-byte header
//! `{ magic: u64, rc: u32, _reserved: u32 }` immediately below the pointer, so
//! a host staticlib can read `rc` at `data - 8` and check the `HEW_CSTR` magic
//! at `data - 16`. The spy biases `rc` by a large constant when it retains a
//! handle, which (a) guarantees `free_cstring` never reaches zero and frees
//! underneath the observation, and (b) leaves every later `hew_string_drop`
//! visible as an exact decrement. `releases = rc_at_retain + BIAS - rc_now`.
//!
//! This works on any unix, needs no allocator instrumentation, and is exact.
#![cfg(not(target_os = "windows"))]

mod support;

use std::path::{Path, PathBuf};
use std::process::Command;

use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

/// The spy staticlib. Non-copying throughout: it stores and inspects the exact
/// pointer Hew passes, and never clones, duplicates, or re-allocates a handle.
///
/// `spy_retain` is the "non-copying external sink that retains the exact passed
/// handle" the double-release guard needs: after it returns, the host owns a
/// live reference, so any further release by the Hew caller is a second
/// release of a handle the caller no longer owns.
const SPY_RUST: &str = r#"//! Exact release-counting spy over the pinned hew-cabi cstring header.
use std::sync::atomic::{AtomicUsize, AtomicU64, Ordering};

/// `hew-cabi` pins `CSTRING_HEADER_SIZE = 16` with the layout
/// `{ magic: u64, rc: u32, _reserved: u32 }`, and hands out `base + 16`.
const HEADER: isize = 16;
/// `CSTRING_MAGIC` — b"HEW_CSTR" read as a little-endian u64.
const MAGIC: u64 = 0x4845_575F_4353_5452;
/// Large enough that no realistic release count can drive `rc` to zero, so the
/// observed buffer is never freed underneath us.
const BIAS: u32 = 1_000_000;

const SLOTS: usize = 64;

static COUNT: AtomicUsize = AtomicUsize::new(0);
static HELD: [AtomicU64; SLOTS] = [const { AtomicU64::new(0) }; SLOTS];
static RC_AT_RETAIN: [AtomicU64; SLOTS] = [const { AtomicU64::new(0) }; SLOTS];
/// Bumped whenever a handle fails the header check, so a silent
/// representation change fails the test instead of reporting a fake zero.
static BAD_HEADER: AtomicUsize = AtomicUsize::new(0);

unsafe fn rc_ptr(data: *const u8) -> *mut u32 {
    unsafe { data.offset(-8) as *mut u32 }
}

unsafe fn header_ok(data: *const u8) -> bool {
    !data.is_null()
        && unsafe { std::ptr::read_unaligned(data.offset(-HEADER) as *const u64) } == MAGIC
}

/// Retain the EXACT handle passed, without copying it. Returns the slot index,
/// or -1 if the handle did not carry the expected runtime header.
#[no_mangle]
pub unsafe extern "C" fn spy_retain(data: *const u8) -> i64 {
    if !unsafe { header_ok(data) } {
        BAD_HEADER.fetch_add(1, Ordering::SeqCst);
        return -1;
    }
    let slot = COUNT.fetch_add(1, Ordering::SeqCst);
    if slot >= SLOTS {
        return -1;
    }
    let rc = unsafe { std::ptr::read_unaligned(rc_ptr(data)) };
    unsafe { std::ptr::write_unaligned(rc_ptr(data), rc + BIAS) };
    RC_AT_RETAIN[slot].store(u64::from(rc), Ordering::SeqCst);
    HELD[slot].store(data as u64, Ordering::SeqCst);
    slot as i64
}

/// Net releases observed across every retained handle:
/// `sum over slots of (rc_at_retain + BIAS - rc_now)`.
#[no_mangle]
pub extern "C" fn spy_releases() -> i64 {
    let n = COUNT.load(Ordering::SeqCst).min(SLOTS);
    let mut total: i64 = 0;
    for slot in 0..n {
        let data = HELD[slot].load(Ordering::SeqCst) as *const u8;
        if data.is_null() {
            continue;
        }
        let rc = unsafe { std::ptr::read_unaligned(rc_ptr(data)) };
        let expected = RC_AT_RETAIN[slot].load(Ordering::SeqCst) + u64::from(BIAS);
        total += expected as i64 - i64::from(rc);
    }
    total
}

#[no_mangle]
pub extern "C" fn spy_retained() -> i64 {
    COUNT.load(Ordering::SeqCst) as i64
}

#[no_mangle]
pub extern "C" fn spy_bad_headers() -> i64 {
    BAD_HEADER.load(Ordering::SeqCst) as i64
}

extern "C" {
    fn hew_string_drop(s: *mut std::ffi::c_char);
}

/// Positive control: release one retained handle FROM THE HOST, through the
/// same runtime entry point the compiler emits. Proves `spy_releases` observes
/// a real `hew_string_drop` as exactly one decrement, so a reported zero is a
/// measurement and not a broken probe.
#[no_mangle]
pub extern "C" fn spy_release_one_from_host() -> i64 {
    let data = HELD[0].load(Ordering::SeqCst) as *mut std::ffi::c_char;
    if data.is_null() {
        return -1;
    }
    unsafe { hew_string_drop(data) };
    0
}
"#;

/// P0 #2 shape. `mkopt` returns a heap-owning `Option<string>`; the `Some(s)`
/// binder is exactly `string` and its ONLY use in the terminator is the call
/// argument, so the two structural conjuncts of the payload-binder exemption
/// hold and only the callee-borrow conjunct can reject the read.
///
/// The sink is a declared extern, so the caller must NOT keep a scope-exit
/// drop for `s`. `spy_retain` models the host taking a reference it keeps: if
/// the composite's `EnumInPlace` drop still ran, the spy would observe one
/// release per iteration of a handle the caller had already handed away.
const ENUM_PAYLOAD_TO_EXTERN_SINK: &str = r#"extern "C" {
    fn spy_retain(s: string) -> i64;
    fn spy_releases() -> i64;
    fn spy_retained() -> i64;
    fn spy_bad_headers() -> i64;
    fn spy_release_one_from_host() -> i64;
}

fn mkopt(i: i64) -> Option<string> {
    Some(f"payload{i}")
}

fn main() -> i64 {
    var i: i64 = 0;
    while i < 8 {
        if let Some(s) = mkopt(i) {
            unsafe { spy_retain(s); }
        }
        i = i + 1;
    }

    let retained = unsafe { spy_retained() };
    let bad = unsafe { spy_bad_headers() };
    let releases = unsafe { spy_releases() };
    println(f"retained={retained}");
    println(f"bad_headers={bad}");
    println(f"releases={releases}");

    // Positive control for the counter itself, run LAST so it cannot perturb
    // the measurement above.
    unsafe { spy_release_one_from_host(); }
    let after = unsafe { spy_releases() };
    println(f"after_host_release={after}");
    0
}
"#;

/// P0 #1 shape: a root-declared `extern "C" -> string` whose result is
/// interpolated, with the host KEEPING its returned pointer alive in a table.
///
/// A declared extern has no lowered body, so the call-result mint used to fall
/// through to `callee_returns_fresh_owner`'s `unwrap_or(true)` cross-ABI
/// fallback and admit it as a fresh string producer.
///
/// See the test's own doc comment for exactly what this fixture does and does
/// not prove — the ratified ABI for a ROOT extern `-> string` is that the host
/// returns a raw malloc-owned C string which codegen ADOPTS into a private
/// header-aware buffer and then `free()`s, so the foreign pointer is released
/// exactly once by design. This fixture pins that "exactly once": a second
/// release of the same malloc block aborts under every libc double-free
/// detector, and the host verifies each block is still intact when it is
/// handed over.
const EXTERN_RETURN_INTERPOLATED: &str = r#"extern "C" {
    fn host_owned_string() -> string;
    fn host_handed_out() -> i64;
}

fn main() -> i64 {
    var i: i64 = 0;
    while i < 8 {
        let v = unsafe { host_owned_string() };
        println(f"v={v}");
        i = i + 1;
    }
    let handed = unsafe { host_handed_out() };
    println(f"handed={handed}");
    0
}
"#;

/// The host allocates each return with `malloc` (the ratified ABI for a root
/// extern `-> string`), records the pointer so it "keeps the handle alive" on
/// its own side, and hands it to Hew. It never frees: codegen's adoption owns
/// that single release, and a second one would abort the process.
const HOST_RETURN_RUST: &str = r#"use std::sync::atomic::{AtomicUsize, Ordering};

static HANDED: AtomicUsize = AtomicUsize::new(0);

extern "C" {
    fn malloc(size: usize) -> *mut u8;
}

#[no_mangle]
pub extern "C" fn host_owned_string() -> *mut std::ffi::c_char {
    let text = b"host-owned\0";
    let buf = unsafe { malloc(text.len()) };
    assert!(!buf.is_null());
    unsafe { std::ptr::copy_nonoverlapping(text.as_ptr(), buf, text.len()) };
    HANDED.fetch_add(1, Ordering::SeqCst);
    buf as *mut std::ffi::c_char
}

#[no_mangle]
pub extern "C" fn host_handed_out() -> i64 {
    HANDED.load(Ordering::SeqCst) as i64
}
"#;

/// The Hew-bodied CONTROL for the same shape: everything about the program is
/// identical except that the payload producer and the consumer are ordinary
/// Hew functions. This must keep working exactly as the leak fix
/// intends -- the extern veto must not have widened into Hew-bodied calls.
const ENUM_PAYLOAD_TO_HEW_SINK: &str = r#"fn mkopt(i: i64) -> Option<string> {
    Some(f"payload{i}")
}

fn consume(s: string) -> i64 {
    s.len()
}

fn main() -> i64 {
    var total: i64 = 0;
    var i: i64 = 0;
    while i < 8 {
        if let Some(s) = mkopt(i) {
            total = total + consume(s);
        }
        i = i + 1;
    }
    println(f"total={total}");
    0
}
"#;

/// P1 shape: an enum that is heap-owning through its `string` arm but whose
/// other payload is a pair of IO handles with SCALAR type arguments. The MIR
/// heap authority answers "owns no heap" for `Stream<i64>` / `Sink<i64>`, so
/// the old `!ty_owns_heap_mir` cap re-admitted this composite to `EnumInPlace`
/// -- and `EnumInPlace` seeds enum clone/drop helper synthesis, which the
/// `IoHandle` class cannot satisfy. Interpolating the `Err(e)` payload is the
/// exact trigger.
const SCALAR_IO_HANDLE_PAYLOAD: &str = r#"fn attempt(i: i64) -> Result<(Stream<i64>, Sink<i64>), string> {
    Err(f"refused{i}")
}

fn main() -> i64 {
    var i: i64 = 0;
    while i < 8 {
        match attempt(i) {
            Ok(_) => {}
            Err(e) => { println(f"err={e}"); }
        }
        i = i + 1;
    }
    0
}
"#;

fn build_staticlib(dir: &Path, name: &str, source: &str) -> Option<PathBuf> {
    let src_path = dir.join(format!("{name}.rs"));
    std::fs::write(&src_path, source).expect("write fixture .rs");
    let archive = dir.join(format!("lib{name}.a"));

    let mut cmd = Command::new("rustc");
    cmd.args([
        "--crate-type",
        "staticlib",
        "--crate-name",
        name,
        "--edition",
        "2021",
        "-C",
        "panic=abort",
        "-C",
        "codegen-units=1",
        "-o",
    ])
    .arg(&archive)
    .arg(&src_path)
    .current_dir(dir);

    let out = match cmd.output() {
        Ok(out) => out,
        Err(error) => {
            eprintln!("SKIP: cannot invoke rustc to build the spy staticlib: {error}");
            return None;
        }
    };
    assert!(
        out.status.success(),
        "rustc failed to build the spy staticlib `{name}`:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
    Some(archive)
}

/// Build `source` (linking `lib` when given) and run it; return stdout.
fn build_and_run(dir: &Path, name: &str, source: &str, lib: Option<&Path>) -> String {
    let prog = dir.join(format!("{name}.hew"));
    std::fs::write(&prog, source).expect("write fixture .hew");
    let bin = dir.join(name);

    let mut compile = Command::new(hew_binary());
    compile.arg("build");
    if let Some(lib) = lib {
        compile.arg("--link-lib").arg(lib);
    }
    compile.arg(&prog).arg("-o").arg(&bin).current_dir(dir);
    let compiled = run_bounded_command(compile, "hew build");
    assert!(
        compiled.status.success(),
        "`hew build` failed for {name}:\n{}",
        describe_output(&compiled),
    );

    let mut cmd = Command::new(&bin);
    cmd.current_dir(dir);
    // Poison freed and fresh allocations so a use-after-free on a released
    // handle is loud rather than silently benign. Harmless where unsupported.
    cmd.env("MallocScribble", "1");
    cmd.env("MallocPreScribble", "1");
    let run = run_bounded_command(cmd, name);
    assert!(
        run.status.success(),
        "{name} did not exit cleanly:\n{}",
        describe_output(&run),
    );
    String::from_utf8_lossy(&run.stdout).into_owned()
}

/// Read `key=<int>` out of the program's stdout.
fn reported(stdout: &str, key: &str) -> i64 {
    let prefix = format!("{key}=");
    stdout
        .lines()
        .find_map(|line| line.trim().strip_prefix(&prefix))
        .unwrap_or_else(|| panic!("program did not report `{key}=`; stdout was:\n{stdout}"))
        .trim()
        .parse()
        .unwrap_or_else(|error| panic!("`{key}` was not an integer: {error}\n{stdout}"))
}

/// A non-copying external SINK that retains the exact `string` handle it is
/// passed must leave the enum payload's scope-exit drop UNRUN.
///
/// Before the fix the `Some(s) => extern_sink(s)` shape cleared all three
/// conjuncts of the payload-binder exemption -- `s` is exactly `string`, the
/// extern is in `module_fn_names`, and its only terminator use is the call
/// argument -- so the composite kept its `EnumInPlace` drop and released a
/// handle it had already handed to the host. The spy reports that as one
/// release per iteration; the fixed compiler reports zero.
///
/// Measured against the pre-fix compiler this fixture reports `releases=8`
/// over eight iterations AND STILL EXITS 0 — which is precisely why an exact
/// count is needed: a released-but-still-reachable buffer does not fault, so
/// the leak-slope oracle (macOS-only) and the clean-exit-under-poisoned-
/// allocator oracle both pass a double release on non-Darwin unix.
#[test]
fn extern_sink_that_retains_a_payload_sees_no_caller_release() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_sink", SPY_RUST) else {
        return;
    };

    let stdout = build_and_run(
        dir.path(),
        "extern_sink",
        ENUM_PAYLOAD_TO_EXTERN_SINK,
        Some(&spy),
    );

    assert_eq!(
        reported(&stdout, "retained"),
        8,
        "guard: the host must have seen all eight payload handles, or the \
         measurement below is vacuous:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "bad_headers"),
        0,
        "guard: every handle the extern received must carry the pinned \
         `HEW_CSTR` header, i.e. the extern really is handed the runtime \
         handle and not a copy — otherwise this test measures nothing:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "releases"),
        0,
        "DOUBLE RELEASE: the enum payload's scope-exit drop released a handle \
         already handed to a non-copying external sink. An extern's ownership \
         behaviour is unknowable, so the caller must decline its drop \
         obligation here (a leak, never a second release):\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "after_host_release"),
        1,
        "the release counter itself must have teeth: one real \
         `hew_string_drop` from the host must read as exactly one release, so \
         the zero above is a measurement rather than a blind probe:\n{stdout}"
    );
}

/// A heap-returning extern's result, interpolated, must be released exactly
/// once — and never by a caller-side owner minted from an unproven freshness
/// answer.
///
/// # What this pins, honestly
///
/// The ratified ABI for a ROOT-declared `extern "C" -> string` is that the host
/// returns a raw malloc-owned C string, and codegen emits an ADOPTION sequence
/// that copies it into a private header-aware buffer and `free()`s the foreign
/// allocation. So on today's pipeline the interpolated value is already a
/// private copy and the foreign pointer has exactly one release, by design.
/// This fixture pins that invariant end to end: the host hands out eight live
/// malloc blocks and never frees them itself, so any release beyond the single
/// adoption `free()` is a double free of a malloc block and aborts the process
/// under every libc double-free detector, with the allocator additionally
/// poisoned.
///
/// It deliberately does NOT claim to reproduce the review's original scenario.
/// That scenario is unreachable through source syntax today for a second
/// reason as well: `unsafe { f() }` lowers to `HirExprKind::Block`, not
/// `HirExprKind::Call`, and a direct extern call without `unsafe` is a checker
/// error — so the mint's `HirExpr` gate never sees the extern callee at all.
/// The gate-level proof that the mint refuses an extern lives in the hew-mir
/// unit regressions (`return_provenance::extern_ownership_opacity` and
/// `lower::facts::analyzed_freshness_strictness`), which assert the authority
/// directly and cannot be routed around by a lowering shape.
#[test]
fn extern_returned_string_interpolation_releases_exactly_once() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_return", HOST_RETURN_RUST) else {
        return;
    };

    let stdout = build_and_run(
        dir.path(),
        "extern_return",
        EXTERN_RETURN_INTERPOLATED,
        Some(&spy),
    );

    assert_eq!(
        reported(&stdout, "handed"),
        8,
        "guard: the host must have handed out all eight allocations, or the \
         run proves nothing:\n{stdout}"
    );
    assert_eq!(
        stdout
            .lines()
            .filter(|l| l.trim() == "v=host-owned")
            .count(),
        8,
        "every interpolation must have read the host's text intact — a \
         premature release would show as poisoned bytes:\n{stdout}"
    );
}

/// The leak fix itself must survive the extern veto: the same payload shape
/// with a Hew-BODIED producer and consumer still lowers, runs, and exits
/// cleanly. `lower_params` ratifies a by-value heap parameter as a borrow, so
/// the caller keeps the drop and the interpolation temp is still freed.
#[test]
fn hew_bodied_payload_consumer_is_unaffected() {
    require_codegen();
    let dir = tempdir();
    let stdout = build_and_run(dir.path(), "hew_sink", ENUM_PAYLOAD_TO_HEW_SINK, None);
    assert_eq!(
        reported(&stdout, "total"),
        8 * 8,
        "the Hew-bodied control must still compute over every payload; the \
         extern veto must not have widened into ordinary calls:\n{stdout}"
    );
}

/// The P1 cap: an enum whose non-string payload is a pair of IO handles with
/// SCALAR type arguments must still compile and run. `Stream<i64>` / `Sink<i64>`
/// read as "owns no heap" to the MIR heap authority, so the old cap admitted
/// them to `EnumInPlace` and re-opened the clone-synthesis refusal.
///
/// Measured against the pre-fix compiler this program does not build at all:
///
/// ```text
/// E_NOT_YET_IMPLEMENTED: fail-closed: Stream/Sink/Generator/CancellationToken
/// handle field reached per-field clone helper; these pointer-backed handles
/// have no dup runtime symbol, so cloning (supervisor restart / aggregate
/// clone) is unsupported. Only the drop direction is wired; move the handle
/// instead of cloning it.
/// ```
///
/// The type arguments are deliberately `i64`, not `string`: with heap type
/// arguments the old `!ty_owns_heap_mir` cap happened to reject the composite
/// for the wrong reason, and the defect would not show.
#[test]
fn scalar_argument_io_handle_payload_still_compiles_and_runs() {
    require_codegen();
    let dir = tempdir();
    let stdout = build_and_run(dir.path(), "io_handle_cap", SCALAR_IO_HANDLE_PAYLOAD, None);
    assert_eq!(
        stdout.lines().filter(|l| l.starts_with("err=")).count(),
        8,
        "every `Err` payload must have been interpolated:\n{stdout}"
    );
}

/// An `@resource` payload alongside an interpolated `string` payload must stay
/// OUT of the exemption, and its `close` must run exactly once per `Ok`.
///
/// `#[resource]` values are affine: they have an implicit `close` drop side
/// effect and no duplication helper at all, so clone totality refuses the
/// class outright. Admitting this composite to `EnumInPlace` would seed a
/// clone/drop helper over a payload that cannot be duplicated. The `Err(e)`
/// arm interpolates, which is what puts the enum in front of the cap in the
/// first place.
///
/// The observable pin is exact and two-sided: three `Ok` frames each close
/// once (no missing close, no double close) and three `Err` frames each read
/// their payload intact.
const RESOURCE_PAYLOAD_WITH_INTERPOLATED_ERR: &str = r#"#[resource] type Conn { fd: i64 }
impl Conn { fn close(self) { println(f"closed-{self.fd}"); } }

fn attempt(i: i64) -> Result<Conn, string> {
    if i % 2 == 0 { Ok(Conn { fd: i }) } else { Err(f"refused{i}") }
}

fn main() -> i64 {
    var i: i64 = 0;
    while i < 6 {
        match attempt(i) {
            Ok(c) => { c.close(); }
            Err(e) => { println(f"err={e}"); }
        }
        i = i + 1;
    }
    0
}
"#;

#[test]
fn resource_payload_beside_an_interpolated_string_closes_exactly_once() {
    require_codegen();
    let dir = tempdir();
    let stdout = build_and_run(
        dir.path(),
        "resource_cap",
        RESOURCE_PAYLOAD_WITH_INTERPOLATED_ERR,
        None,
    );

    let closes: Vec<&str> = stdout
        .lines()
        .map(str::trim)
        .filter(|l| l.starts_with("closed-"))
        .collect();
    assert_eq!(
        closes,
        vec!["closed-0", "closed-2", "closed-4"],
        "each `Ok` payload's affine `close` must run exactly once, in order; a \
         duplicated line is a double close and a missing one is a leaked \
         resource:\n{stdout}"
    );
    let errs: Vec<&str> = stdout
        .lines()
        .map(str::trim)
        .filter(|l| l.starts_with("err="))
        .collect();
    assert_eq!(
        errs,
        vec!["err=refused1", "err=refused3", "err=refused5"],
        "every `Err` payload must have been interpolated intact:\n{stdout}"
    );
}

/// A returned channel pair must NOT be closed by the producer's own scope-exit
/// drop.
///
/// `channel.new` unwraps a `Result<(Sender, Receiver), string>` and MOVES the
/// `Ok` payload into its return value. `Sender` and `Receiver` are resource
/// handles released by `hew_channel_sender_close` / `hew_channel_receiver_close`
/// with no duplication helper, so an `EnumInPlace` drop on that `Result` closes
/// a pair the caller has just been handed. The old `!ty_owns_heap_mir` cap read
/// the pair as "owns no heap" and admitted it; this program then segfaulted
/// unconditionally, with no allocator poisoning required.
///
/// The pin is the whole program: it must run, print the message it sent
/// through the channel, and exit cleanly.
const RETURNED_CHANNEL_PAIR: &str = r#"import std::channel::channel;

fn main() -> i64 {
    let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(1);
    tx.send(f"ready-{1}");
    match rx.try_recv() {
        Some(msg) => { println(f"got={msg}"); }
        None => { println("empty"); }
    }
    0
}
"#;

#[test]
fn a_returned_channel_pair_is_not_closed_by_its_producer() {
    require_codegen();
    let dir = tempdir();
    let stdout = build_and_run(dir.path(), "channel_pair", RETURNED_CHANNEL_PAIR, None);
    assert_eq!(
        stdout.trim(),
        "got=ready-1",
        "the receiver must still be open when the caller reads it; a \
         producer-side in-place drop of the `Result<(Sender, Receiver), string>` \
         closes handles the caller owns:\n{stdout}"
    );
}
