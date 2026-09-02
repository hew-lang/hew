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

use support::{
    describe_output, hew_binary, repo_root, require_codegen, run_bounded_command, tempdir,
};

/// The spy staticlib. Non-copying throughout: it stores and inspects the exact
/// pointer Hew passes, and never clones, duplicates, or re-allocates a handle.
///
/// `spy_retain` is the "non-copying external sink that retains the exact passed
/// handle" the double-release guard needs: after it returns, the host owns a
/// live reference, so any further release by the Hew caller is a second
/// release of a handle the caller no longer owns.
const SPY_RUST: &str = r#"//! Exact release-counting spy over the pinned hew-cabi cstring header.
use std::sync::atomic::{AtomicU32, AtomicU64, AtomicUsize, Ordering};

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

unsafe fn rc_ptr(data: *const u8) -> *const AtomicU32 {
    unsafe { data.offset(-8).cast() }
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
    let rc = unsafe { (&*rc_ptr(data)).fetch_add(BIAS, Ordering::SeqCst) };
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
        let rc = unsafe { (&*rc_ptr(data)).load(Ordering::SeqCst) };
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
        if let .Some(s) = mkopt(i) {
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
        if let .Some(s) = mkopt(i) {
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
            .Ok(_) => {}
            .Err(e) => { println(f"err={e}"); }
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
    let bin = hew_testutil::compiled_binary_path(dir, name);

    let mut compile = Command::new(hew_binary());
    compile.arg("build");
    if let Some(lib) = lib {
        compile.arg("--link-lib").arg(lib);
    }
    compile
        .arg(&prog)
        .arg("-o")
        .arg(&bin)
        .current_dir(dir)
        .env("HEWPATH", repo_root());
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
            .Ok(c) => { c.close(); },
            .Err(e) => { println(f"err={e}"); },
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
const RETURNED_CHANNEL_PAIR: &str = r#"import std.channel.channel;

fn main() -> i64 {
    let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(1);
    tx.send(f"ready-{1}");
    match rx.try_recv() {
        .Some(msg) => { println(f"got={msg}"); }
        .None => { println("empty"); }
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

/// The P0 this revision closes: a Hew WRAPPER around an ownership-opaque
/// `extern "C" -> string`, observed on a non-adopting ABI.
///
/// # Why this fixture can observe what the root-extern one above cannot
///
/// A ROOT-declared extern `-> string` is classified `ForeignAdopt`, so codegen
/// copies the foreign pointer into a private header-aware buffer and `free()`s
/// it. The foreign handle never reaches Hew raw, which is exactly why the
/// root-extern fixture can only pin "released exactly once".
///
/// A `HeaderAware` extern is not copied: the compiler passes the host's pointer
/// straight through as a runtime string handle. That classification is reached
/// through an extern declared in a module with stdlib provenance, so this
/// fixture builds a two-file project whose sibling module is named `std` and
/// declares the extern. This is a real, reachable configuration of the selected
/// ABI, and it is the only one on which a non-adopting foreign result is
/// observable — stated plainly because it is a deliberately narrow hook.
///
/// The host therefore asks the linked runtime to mint a genuine registered
/// handle, biases its pinned `rc` so no release can free it underneath the
/// probe, and keeps every pointer. Each compiler release is then visible as an
/// exact decrement.
///
/// Measured against the pre-fix compiler this fixture reports `releases=8` over
/// eight frames: the wrapper laundered the extern's result into an "analyzed
/// fresh" verdict, `main` minted a synthetic owner over it and dropped it. The
/// fixed compiler reports zero.
const HEADER_AWARE_SPY_RUST: &str = r#"//! Mints registered runtime handles and counts every release of them.
use std::sync::atomic::{AtomicU32, AtomicU64, AtomicUsize, Ordering};

/// `hew-cabi` pins `CSTRING_HEADER_SIZE = 16`, `{ magic: u64, rc: u32, _pad: u32 }`.
const HEADER: isize = 16;
/// `CSTRING_MAGIC` — b"HEW_CSTR" read as a little-endian u64.
const MAGIC: u64 = 0x4845_575F_4353_5452;
/// Large enough that no realistic release count reaches zero, so the observed
/// buffer is never freed underneath us.
const BIAS: u32 = 1_000_000;
const SLOTS: usize = 64;

static COUNT: AtomicUsize = AtomicUsize::new(0);
static HELD: [AtomicU64; SLOTS] = [const { AtomicU64::new(0) }; SLOTS];

extern "C" {
    fn hew_string_concat(
        a: *const std::ffi::c_char,
        b: *const std::ffi::c_char,
    ) -> *mut std::ffi::c_char;
    fn hew_string_drop(s: *mut std::ffi::c_char);
}

unsafe fn rc_ptr(data: *const u8) -> *const AtomicU32 {
    unsafe { data.offset(-8).cast() }
}

/// Ask the linked Hew runtime to allocate the handle so its allocation-
/// provenance registry recognizes the exact pointer.  A raw `{magic, rc}`
/// buffer is deliberately not a managed string anymore.
#[no_mangle]
pub unsafe extern "C" fn spy_make_string() -> *mut std::ffi::c_char {
    let text = b"host-made\0";
    let data = unsafe { hew_string_concat(text.as_ptr().cast(), std::ptr::null()) };
    assert!(!data.is_null());
    let magic = unsafe {
        std::ptr::read_unaligned(data.cast::<u8>().offset(-HEADER).cast::<u64>())
    };
    assert_eq!(magic, MAGIC);
    let previous = unsafe { (&*rc_ptr(data.cast())).fetch_add(BIAS, Ordering::SeqCst) };
    assert_eq!(previous, 1);
    let slot = COUNT.fetch_add(1, Ordering::SeqCst);
    if slot < SLOTS {
        HELD[slot].store(data as u64, Ordering::SeqCst);
    }
    data
}

/// Net releases across every handed-out handle: `sum of (1 + BIAS - rc_now)`.
#[no_mangle]
pub extern "C" fn spy_releases() -> i64 {
    let n = COUNT.load(Ordering::SeqCst).min(SLOTS);
    let mut total: i64 = 0;
    for slot in 0..n {
        let data = HELD[slot].load(Ordering::SeqCst) as *const u8;
        if data.is_null() {
            continue;
        }
        let rc = unsafe { (&*rc_ptr(data)).load(Ordering::SeqCst) };
        total += i64::from(1u32 + BIAS) - i64::from(rc);
    }
    total
}

#[no_mangle]
pub extern "C" fn spy_made() -> i64 {
    COUNT.load(Ordering::SeqCst) as i64
}

/// Positive control for the counter, run LAST: one real `hew_string_drop` from
/// the host must read as exactly one release, so a reported zero is a
/// measurement and not a blind probe.
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

/// The sibling module that declares the non-adopting extern and wraps it in one
/// ordinary Hew frame. `wrapper` is the launderer: body-less externs read as
/// fresh in the coarse freshness summary, so this `-> string` used to inherit a
/// freshness proof it never earned.
const HEADER_AWARE_WRAPPER_MODULE: &str = r#"extern "C" {
    fn spy_make_string() -> string;
}

pub fn wrapper() -> string {
    unsafe { spy_make_string() }
}
"#;

const HEADER_AWARE_WRAPPER_MAIN: &str = r#"import std;

extern "C" {
    fn spy_releases() -> i64;
    fn spy_made() -> i64;
    fn spy_release_one_from_host() -> i64;
}

fn main() -> i64 {
    var i: i64 = 0;
    while i < 8 {
        println(f"v={std.wrapper()}");
        i = i + 1;
    }
    let made = unsafe { spy_made() };
    let releases = unsafe { spy_releases() };
    println(f"made={made}");
    println(f"releases={releases}");

    unsafe { spy_release_one_from_host(); }
    let after = unsafe { spy_releases() };
    println(f"after_host_release={after}");
    0
}
"#;

#[test]
fn a_hew_wrapper_around_an_opaque_extern_sees_no_caller_release() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_header_aware", HEADER_AWARE_SPY_RUST) else {
        return;
    };
    // The wrapper lives in a sibling module resolved from the compiled file's
    // directory; `build_and_run` writes `main.hew` into the same directory.
    std::fs::write(dir.path().join("std.hew"), HEADER_AWARE_WRAPPER_MODULE)
        .expect("write sibling module");

    let stdout = build_and_run(dir.path(), "main", HEADER_AWARE_WRAPPER_MAIN, Some(&spy));

    assert_eq!(
        reported(&stdout, "made"),
        8,
        "guard: the host must have handed out all eight handles, or the \
         measurement below is vacuous:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "releases"),
        0,
        "DOUBLE RELEASE: one Hew frame between the interpolation and an \
         ownership-opaque extern laundered the foreign result into an \
         `analyzed fresh` verdict, and the caller minted and ran a release \
         obligation over a handle the host still owns. The freshness summary \
         must fail closed through an arbitrary chain of Hew frames:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "after_host_release"),
        1,
        "the release counter itself must have teeth: one real \
         `hew_string_drop` from the host must read as exactly one release, so \
         the zero above is a measurement rather than a blind probe:\n{stdout}"
    );
}

/// The Vec-ingress P0 this revision closes, counted exactly.
///
/// `expr_is_materialized_owner` read the coarse freshness map with no
/// opaque-extern veto at all, so a Hew wrapper over an extern was admitted as a
/// materialised owner and `v.push(wrapHolder())` was routed to the MOVE-in
/// `hew_vec_push_owned_move`. The element's heap — here a real header-aware
/// `string` handle the host minted and still holds — was byte-transferred into
/// the buffer with no retain, and the Vec's teardown then released it. That is
/// a release of a handle the caller never owned.
///
/// The fixed routing keeps the push COPY-IN (`hew_vec_push_owned`), which
/// deep-clones the element: the label is RETAINED into the slot and the Vec's
/// teardown releases that retained share, so the host's own reference is
/// untouched and the net count over the observed handles is exactly zero.
///
/// This is the non-string heap class (a record element) observed through the
/// one field the runtime representation lets us count exactly. Measured against
/// the pre-fix compiler it reports `releases=8` over eight frames.
const VEC_INGRESS_RECORD_WRAPPER: &str = r#"type Holder { label: string }

extern "C" {
    fn spy_make_holder() -> Holder;
    fn spy_made() -> i64;
    fn spy_releases() -> i64;
    fn spy_release_one_from_host() -> i64;
}

fn wrapHolder() -> Holder { unsafe { spy_make_holder() } }

fn pushFrames(n: i64) -> i64 {
    var v: Vec<Holder> = Vec.new();
    var i: i64 = 0;
    while i < n {
        v.push(wrapHolder());
        i = i + 1;
    }
    v.len()
}

fn main() -> i64 {
    let pushed = pushFrames(8);
    let made = unsafe { spy_made() };
    let releases = unsafe { spy_releases() };
    println(f"pushed={pushed}");
    println(f"made={made}");
    println(f"releases={releases}");

    // Positive control for the counter, run LAST so it cannot perturb the
    // measurement above.
    unsafe { spy_release_one_from_host(); }
    let after = unsafe { spy_releases() };
    println(f"after_host_release={after}");
    0
}
"#;

/// Mints registered runtime `string` handles, wraps each in a `repr(C)` record,
/// and counts every release of them. Non-copying: the exact pointer handed to
/// Hew is the one whose `rc` is observed, and the `rc` is biased so no release
/// can free the buffer underneath the probe.
const RECORD_SPY_RUST: &str = r#"use std::sync::atomic::{AtomicU32, AtomicU64, AtomicUsize, Ordering};

/// `hew-cabi` pins `CSTRING_HEADER_SIZE = 16`, `{ magic: u64, rc: u32, _pad: u32 }`.
const HEADER: isize = 16;
/// `CSTRING_MAGIC` — b"HEW_CSTR" read as a little-endian u64.
const MAGIC: u64 = 0x4845_575F_4353_5452;
/// Large enough that no realistic release count reaches zero.
const BIAS: u32 = 1_000_000;
const SLOTS: usize = 64;

static COUNT: AtomicUsize = AtomicUsize::new(0);
static HELD: [AtomicU64; SLOTS] = [const { AtomicU64::new(0) }; SLOTS];

extern "C" {
    fn hew_string_concat(
        a: *const std::ffi::c_char,
        b: *const std::ffi::c_char,
    ) -> *mut std::ffi::c_char;
    fn hew_string_drop(s: *mut std::ffi::c_char);
}

unsafe fn rc_ptr(data: *const u8) -> *const AtomicU32 {
    unsafe { data.offset(-8).cast() }
}

/// The record the extern returns, in the C layout the declaration implies.
#[repr(C)]
pub struct Holder {
    label: *mut std::ffi::c_char,
}

unsafe fn make_handle() -> *mut std::ffi::c_char {
    let text = b"host-made\0";
    // Allocation must go through the linked runtime: `hew_string_drop` now
    // rejects raw header lookalikes that are absent from its provenance registry.
    let data = unsafe { hew_string_concat(text.as_ptr().cast(), std::ptr::null()) };
    assert!(!data.is_null());
    let magic = unsafe {
        std::ptr::read_unaligned(data.cast::<u8>().offset(-HEADER).cast::<u64>())
    };
    assert_eq!(magic, MAGIC);
    let previous = unsafe { (&*rc_ptr(data.cast())).fetch_add(BIAS, Ordering::SeqCst) };
    assert_eq!(previous, 1);
    let slot = COUNT.fetch_add(1, Ordering::SeqCst);
    if slot < SLOTS {
        HELD[slot].store(data as u64, Ordering::SeqCst);
    }
    data
}

#[no_mangle]
pub unsafe extern "C" fn spy_make_holder() -> Holder {
    Holder {
        label: unsafe { make_handle() },
    }
}

#[no_mangle]
pub extern "C" fn spy_made() -> i64 {
    COUNT.load(Ordering::SeqCst) as i64
}

/// Net releases across every handed-out handle:
/// `sum over slots of ((1 + BIAS) - rc_now)`.
#[no_mangle]
pub extern "C" fn spy_releases() -> i64 {
    let n = COUNT.load(Ordering::SeqCst).min(SLOTS);
    let mut total: i64 = 0;
    for slot in 0..n {
        let data = HELD[slot].load(Ordering::SeqCst) as *const u8;
        if data.is_null() {
            continue;
        }
        let rc = unsafe { (&*rc_ptr(data)).load(Ordering::SeqCst) };
        total += i64::from(1u32 + BIAS) - i64::from(rc);
    }
    total
}

/// Positive control for the counter, run LAST.
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

#[test]
fn a_vec_push_of_a_wrapped_extern_record_sees_no_caller_release() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_record", RECORD_SPY_RUST) else {
        return;
    };

    let stdout = build_and_run(
        dir.path(),
        "vec_ingress",
        VEC_INGRESS_RECORD_WRAPPER,
        Some(&spy),
    );

    assert_eq!(
        reported(&stdout, "pushed"),
        8,
        "guard: all eight elements must have reached the Vec, or the \
         measurement below is vacuous:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "made"),
        8,
        "guard: the host must have minted all eight handles:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "releases"),
        0,
        "DOUBLE RELEASE at Vec ingress: a Hew wrapper over an ownership-opaque \
         extern was admitted as a materialised owner, the push was routed to \
         `hew_vec_push_owned_move`, and the Vec's teardown released a handle \
         the host still owns. The freshness authority must veto the wrapper at \
         the Vec seam exactly as it does at the string mint:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "after_host_release"),
        1,
        "the release counter itself must have teeth: one real \
         `hew_string_drop` from the host must read as exactly one release, so \
         the zero above is a measurement rather than a blind probe:\n{stdout}"
    );
}

/// The borrowing-callee-argument P0 this revision closes, counted exactly.
///
/// The caller-side temp-arg mint vetoed a DIRECT extern by name and then fell
/// through to the coarse freshness map's `unwrap_or(true)`. A Hew wrapper is
/// not a direct extern, so `borrowHolder(wrapHolder())` minted a synthetic
/// caller-owned temporary over the host's record and scheduled an in-place
/// release of its heap fields — again, a release of a handle the caller never
/// owned. `borrowHolder` only BORROWS its by-value parameter, so nothing else
/// in the program could account for the decrement.
///
/// Measured against the pre-fix compiler this reports `releases=8`.
const BORROWING_CALLEE_RECORD_WRAPPER: &str = r#"type Holder { label: string }

extern "C" {
    fn spy_make_holder() -> Holder;
    fn spy_made() -> i64;
    fn spy_releases() -> i64;
    fn spy_release_one_from_host() -> i64;
}

fn wrapHolder() -> Holder { unsafe { spy_make_holder() } }

fn borrowHolder(h: Holder) -> i64 { h.label.len() }

fn main() -> i64 {
    var total: i64 = 0;
    var i: i64 = 0;
    while i < 8 {
        total = total + borrowHolder(wrapHolder());
        i = i + 1;
    }
    let made = unsafe { spy_made() };
    let releases = unsafe { spy_releases() };
    println(f"total={total}");
    println(f"made={made}");
    println(f"releases={releases}");

    unsafe { spy_release_one_from_host(); }
    let after = unsafe { spy_releases() };
    println(f"after_host_release={after}");
    0
}
"#;

#[test]
fn a_wrapped_extern_record_in_a_borrowing_argument_sees_no_caller_release() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_record_arg", RECORD_SPY_RUST) else {
        return;
    };

    let stdout = build_and_run(
        dir.path(),
        "borrowing_arg",
        BORROWING_CALLEE_RECORD_WRAPPER,
        Some(&spy),
    );

    assert_eq!(
        reported(&stdout, "made"),
        8,
        "guard: the host must have minted all eight handles:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "total"),
        72,
        "guard: every borrowed record must have been readable — `host-made` is \
         nine bytes, eight times:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "releases"),
        0,
        "DOUBLE RELEASE at the temp-arg mint: a Hew wrapper over an \
         ownership-opaque extern is not a direct extern, so the name veto \
         missed it and the coarse `unwrap_or(true)` minted a caller-owned \
         temporary over the host's record. The freshness authority must carry \
         the veto for the WRAPPER, not just the direct callee:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "after_host_release"),
        1,
        "the release counter itself must have teeth: one real \
         `hew_string_drop` from the host must read as exactly one release, so \
         the zero above is a measurement rather than a blind probe:\n{stdout}"
    );
}

/// F1 — the interim fail-open at the call-scrutinee admission, counted exactly.
///
/// `wrap()` returns a heap `Option<Holder>` whose payload the HOST minted. The
/// admission classifier used to route an `OPAQUE`-only module call down an
/// interim branch that COMPILED and MINTED the `__hew_call_scrutinee` owner
/// regardless of provenance — a fail-open annotated as temporary, which is
/// still a fail-open. The scrutinee owner's release then walked the enum and
/// freed the host's `label`.
///
/// Both former fail-open arms now ask the one authority, which vetoes `wrap`
/// through its taint row. Measured against the pre-fix compiler this fixture
/// reports `releases=8` over eight frames AND STILL EXITS 0 — the exact count
/// is what catches it.
const MATCH_WRAPPED_EXTERN_ENUM: &str = r#"type Holder { label: string }

extern "C" {
    fn spy_make_holder() -> Holder;
    fn spy_made() -> i64;
    fn spy_releases() -> i64;
    fn spy_release_one_from_host() -> i64;
}

fn wrap() -> Option<Holder> { Some(unsafe { spy_make_holder() }) }

fn main() -> i64 {
    var total: i64 = 0;
    var i: i64 = 0;
    while i < 8 {
        match wrap() {
            .Some(h) => { total = total + h.label.len(); }
            .None => {}
        }
        i = i + 1;
    }
    let made = unsafe { spy_made() };
    let releases = unsafe { spy_releases() };
    println(f"total={total}");
    println(f"made={made}");
    println(f"releases={releases}");

    unsafe { spy_release_one_from_host(); }
    let after = unsafe { spy_releases() };
    println(f"after_host_release={after}");
    0
}
"#;

#[test]
fn a_match_over_a_wrapped_extern_enum_sees_no_caller_release() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_record_match", RECORD_SPY_RUST) else {
        return;
    };

    let stdout = build_and_run(
        dir.path(),
        "match_wrapped_enum",
        MATCH_WRAPPED_EXTERN_ENUM,
        Some(&spy),
    );

    assert_eq!(
        reported(&stdout, "made"),
        8,
        "guard: the host must have minted all eight handles:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "total"),
        72,
        "guard: every payload must have been readable — `host-made` is nine \
         bytes, eight times:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "releases"),
        0,
        "DOUBLE RELEASE at the call-scrutinee admission: the interim \
         `LegacyModuleCall` arm minted a scrutinee owner over an enum whose \
         payload the host minted and still owns. Admission must consult the \
         freshness authority, with no branch that answers permissively:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "after_host_release"),
        1,
        "the release counter itself must have teeth: one real \
         `hew_string_drop` from the host must read as exactly one release, so \
         the zero above is a measurement rather than a blind probe:\n{stdout}"
    );
}

/// COUNTERFACTUAL for F1: the identical program over a DOMESTIC producer. The
/// scrutinee owner is still minted and still balances to zero net releases of
/// the host's handles, so the assertion above cannot be satisfied by simply
/// switching the scrutinee mint off.
///
/// The spy still mints the eight observed handles — the program hands each one
/// straight back through `spy_keep` — but the matched enum is built by a Hew
/// frame, so the veto must not fire.
const MATCH_OVER_A_DOMESTIC_ENUM: &str = r#"extern "C" {
    fn spy_retained() -> i64;
    fn spy_bad_headers() -> i64;
    fn spy_releases() -> i64;
    fn spy_release_one_from_host() -> i64;
    fn spy_retain(s: string) -> i64;
}

fn mkopt(i: i64) -> Option<string> { Some(f"tok{i}") }

fn main() -> i64 {
    var total: i64 = 0;
    var i: i64 = 0;
    while i < 8 {
        match mkopt(i) {
            .Some(s) => {
                unsafe { spy_retain(s); }
                total = total + 1;
            }
            .None => {}
        }
        i = i + 1;
    }
    let bad = unsafe { spy_bad_headers() };
    println(f"bad={bad}");
    let made = unsafe { spy_retained() };
    let releases = unsafe { spy_releases() };
    println(f"total={total}");
    println(f"made={made}");
    println(f"releases={releases}");

    unsafe { spy_release_one_from_host(); }
    let after = unsafe { spy_releases() };
    println(f"after_host_release={after}");
    0
}
"#;

#[test]
fn a_match_over_a_domestic_enum_keeps_working() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_domestic_match", SPY_RUST) else {
        return;
    };

    let stdout = build_and_run(
        dir.path(),
        "match_domestic_enum",
        MATCH_OVER_A_DOMESTIC_ENUM,
        Some(&spy),
    );

    assert_eq!(
        reported(&stdout, "made"),
        8,
        "guard: all eight domestic handles must have reached the spy:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "releases"),
        0,
        "control: a domestic producer's scrutinee must still balance — the F1 \
         fix is provenance-directed, not a blanket stop on scrutinee \
         ownership:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "after_host_release"),
        1,
        "the counter must have teeth here too:\n{stdout}"
    );
}

/// F2 — the COMPOSITE rule, counted exactly.
///
/// The `Outer` literal genuinely IS fresh: this frame allocated it. The defect
/// was reading that freshness as ownership of every FIELD, so the caller-owned
/// temp minted over `Outer` scheduled a recursive release that reached the
/// `Holder` the host had just returned.
///
/// Measured against the pre-fix compiler this fixture reports `releases=8`.
const RECORD_LITERAL_EMBEDDING_A_DIRECT_EXTERN: &str = r#"type Holder { label: string }
type Outer { inner: Holder, tag: i64 }

extern "C" {
    fn spy_make_holder() -> Holder;
    fn spy_made() -> i64;
    fn spy_releases() -> i64;
    fn spy_release_one_from_host() -> i64;
}

fn borrowOuter(o: Outer) -> i64 { o.inner.label.len() + o.tag }

fn main() -> i64 {
    var total: i64 = 0;
    var i: i64 = 0;
    while i < 8 {
        total = total + borrowOuter(Outer { inner: unsafe { spy_make_holder() }, tag: 0 });
        i = i + 1;
    }
    let made = unsafe { spy_made() };
    let releases = unsafe { spy_releases() };
    println(f"total={total}");
    println(f"made={made}");
    println(f"releases={releases}");

    unsafe { spy_release_one_from_host(); }
    let after = unsafe { spy_releases() };
    println(f"after_host_release={after}");
    0
}
"#;

#[test]
fn a_record_literal_embedding_a_direct_extern_sees_no_caller_release() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_record_embed", RECORD_SPY_RUST) else {
        return;
    };

    let stdout = build_and_run(
        dir.path(),
        "record_embed",
        RECORD_LITERAL_EMBEDDING_A_DIRECT_EXTERN,
        Some(&spy),
    );

    assert_eq!(
        reported(&stdout, "made"),
        8,
        "guard: the host must have minted all eight handles:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "total"),
        72,
        "guard: every embedded record must have been readable:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "releases"),
        0,
        "DOUBLE RELEASE at a composite mint: freshness of the CONTAINER was \
         taken to imply ownership of its FIELDS, so the outer record's \
         recursive release freed the host's handle. A container with an \
         opaque-provenance embed must not be minted at all:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "after_host_release"),
        1,
        "the release counter itself must have teeth: one real \
         `hew_string_drop` from the host must read as exactly one release, so \
         the zero above is a measurement rather than a blind probe:\n{stdout}"
    );
}

/// COUNTERFACTUAL for F2: the identical container built from a DOMESTIC field
/// still gets its mint and still releases exactly once per frame, so the zero
/// above cannot be satisfied by deleting the composite mint. The spy counts
/// only the host's handles, and this program never asks the host for one; the
/// teeth check therefore reads the domestic handle it retained.
const RECORD_LITERAL_OF_A_DOMESTIC_FIELD: &str = r#"extern "C" {
    fn spy_retained() -> i64;
    fn spy_bad_headers() -> i64;
    fn spy_releases() -> i64;
    fn spy_release_one_from_host() -> i64;
    fn spy_retain(s: string) -> i64;
}

type Holder { label: string }
type Outer { inner: Holder, tag: i64 }

fn mkHolder(i: i64) -> Holder { Holder { label: f"tok{i}" } }

fn borrowOuter(o: Outer) -> i64 {
    unsafe { spy_retain(o.inner.label); }
    o.tag + 1
}

fn main() -> i64 {
    var total: i64 = 0;
    var i: i64 = 0;
    while i < 8 {
        total = total + borrowOuter(Outer { inner: mkHolder(i), tag: 0 });
        i = i + 1;
    }
    let bad = unsafe { spy_bad_headers() };
    println(f"bad={bad}");
    let made = unsafe { spy_retained() };
    let releases = unsafe { spy_releases() };
    println(f"total={total}");
    println(f"made={made}");
    println(f"releases={releases}");

    unsafe { spy_release_one_from_host(); }
    let after = unsafe { spy_releases() };
    println(f"after_host_release={after}");
    0
}
"#;

#[test]
fn a_record_literal_of_a_domestic_field_keeps_working() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_domestic_embed", SPY_RUST) else {
        return;
    };

    let stdout = build_and_run(
        dir.path(),
        "domestic_embed",
        RECORD_LITERAL_OF_A_DOMESTIC_FIELD,
        Some(&spy),
    );

    assert_eq!(
        reported(&stdout, "made"),
        8,
        "guard: all eight domestic handles must have reached the spy:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "total"),
        8,
        "guard: every container must have been readable:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "after_host_release"),
        reported(&stdout, "releases") + 1,
        "control: the counter must still have teeth for the domestic shape — \
         one host release reads as exactly one more:\n{stdout}"
    );
}

/// Build `source` and return the compiler's combined output, asserting the
/// build FAILED. Used where the correct answer is a refusal rather than a
/// number: a seam whose ABI offers no safe route must not lower at all.
fn build_expecting_failure(dir: &Path, name: &str, source: &str, lib: Option<&Path>) -> String {
    let prog = dir.join(format!("{name}.hew"));
    std::fs::write(&prog, source).expect("write fixture .hew");
    let bin = hew_testutil::compiled_binary_path(dir, name);

    let mut compile = Command::new(hew_binary());
    compile.arg("build");
    if let Some(lib) = lib {
        compile.arg("--link-lib").arg(lib);
    }
    compile
        .arg(&prog)
        .arg("-o")
        .arg(&bin)
        .current_dir(dir)
        .env("HEWPATH", repo_root());
    let compiled = run_bounded_command(compile, "hew build");
    assert!(
        !compiled.status.success(),
        "`hew build` must REFUSE {name}, but it succeeded:\n{}",
        describe_output(&compiled),
    );
    assert!(
        !bin.exists(),
        "a refused build must emit no binary for {name}"
    );
    describe_output(&compiled)
}

/// F3 — collection ingress. `m.insert(k, wrapHolder())` moved a host-owned
/// record into the map, and the map's teardown released it.
///
/// Unlike the Vec seam there is no copy-in route to fall back to:
/// `hew-runtime`'s hashmap pins ingress as MOVE by ABI and records that copy-in
/// is intentionally absent. Failing CLOSED therefore has to mean refusing the
/// ingress — the alternative is a silent double release, which is what the four
/// preceding rounds shipped.
///
/// This is the only fix in this revision that costs expressiveness. Lifting it
/// needs a copy-in hashmap ingress (a `hew_hashmap_insert_owned` that clones
/// the value the way `hew_vec_push_owned` does), at which point this seam
/// becomes a route choice like the Vec one rather than a refusal.
const HASHMAP_INSERT_OF_A_WRAPPED_EXTERN_RECORD: &str = r#"type Holder { label: string }

extern "C" {
    fn spy_make_holder() -> Holder;
}

fn wrapHolder() -> Holder { unsafe { spy_make_holder() } }

fn main() -> i64 {
    var m: HashMap<i64, Holder> = HashMap.new();
    m.insert(1, wrapHolder());
    m.len()
}
"#;

#[test]
fn a_hashmap_insert_of_a_wrapped_extern_record_is_refused() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_record_map", RECORD_SPY_RUST) else {
        return;
    };

    let out = build_expecting_failure(
        dir.path(),
        "hashmap_ingress",
        HASHMAP_INSERT_OF_A_WRAPPED_EXTERN_RECORD,
        Some(&spy),
    );
    assert!(
        out.contains("ownership-opaque provenance"),
        "the refusal must name the reason, not fail for some unrelated cause:\n{out}"
    );
}

/// COUNTERFACTUAL for F3: the identical program over a DOMESTIC producer must
/// still compile, still move in, and still run clean. Reverting the reject makes
/// the case above compile; widening it into a blanket stop makes this one fail.
const HASHMAP_INSERT_OF_A_DOMESTIC_RECORD: &str = r#"type Holder { label: string }

fn mkHolder(i: i64) -> Holder { Holder { label: f"tok{i}" } }

fn main() -> i64 {
    var m: HashMap<i64, Holder> = HashMap.new();
    var s: HashSet<string> = HashSet.new();
    var i: i64 = 0;
    while i < 8 {
        m.insert(i, mkHolder(i));
        s.insert(f"k{i}");
        i = i + 1;
    }
    println(f"m={m.len()}");
    println(f"s={s.len()}");
    0
}
"#;

#[test]
fn a_hashmap_insert_of_a_domestic_record_still_compiles_and_runs() {
    require_codegen();
    let dir = tempdir();
    let stdout = build_and_run(
        dir.path(),
        "hashmap_domestic",
        HASHMAP_INSERT_OF_A_DOMESTIC_RECORD,
        None,
    );
    assert_eq!(
        reported(&stdout, "m"),
        8,
        "control: domestic collection ingress must be untouched:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "s"),
        8,
        "and so must HashSet's:\n{stdout}"
    );
}

/// The `let` binder. Seeding drop elaboration from the binding's TYPE alone
/// gives a binder over an opaque foreign producer a scope-exit release the
/// program never earned — the same defect as F1/F2, reached through the
/// simplest construct in the language.
const LET_BOUND_DIRECT_EXTERN_RECORD: &str = r#"type Holder { label: string }

extern "C" {
    fn spy_make_holder() -> Holder;
    fn spy_made() -> i64;
    fn spy_releases() -> i64;
    fn spy_release_one_from_host() -> i64;
}

fn main() -> i64 {
    var total: i64 = 0;
    var i: i64 = 0;
    while i < 8 {
        let h = unsafe { spy_make_holder() };
        total = total + h.label.len();
        i = i + 1;
    }
    let made = unsafe { spy_made() };
    let releases = unsafe { spy_releases() };
    println(f"total={total}");
    println(f"made={made}");
    println(f"releases={releases}");

    unsafe { spy_release_one_from_host(); }
    let after = unsafe { spy_releases() };
    println(f"after_host_release={after}");
    0
}
"#;

#[test]
fn a_let_bound_extern_record_sees_no_caller_release() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_letbind", RECORD_SPY_RUST) else {
        return;
    };

    let stdout = build_and_run(
        dir.path(),
        "letbind",
        LET_BOUND_DIRECT_EXTERN_RECORD,
        Some(&spy),
    );

    assert_eq!(
        reported(&stdout, "made"),
        8,
        "guard: the host must have minted all eight handles:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "total"),
        72,
        "guard: every bound record must have been readable:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "releases"),
        0,
        "DOUBLE RELEASE at a `let` binder: the scope-exit drop was seeded from \
         the binding's TYPE without ever asking where the value came from, so \
         leaving the loop body freed the host's handle:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "after_host_release"),
        1,
        "the release counter itself must have teeth:\n{stdout}"
    );
}

/// The same binder, but the foreign value is first placed in a fresh container
/// — the fact has to travel WITH the binder, not just be read off the
/// initializer at the moment of binding.
const LET_BOUND_EXTERN_RECORD_INSIDE_A_CONTAINER: &str = r#"type Holder { label: string }
type Outer { inner: Holder, tag: i64 }

extern "C" {
    fn spy_make_holder() -> Holder;
    fn spy_made() -> i64;
    fn spy_releases() -> i64;
    fn spy_release_one_from_host() -> i64;
}

fn main() -> i64 {
    var total: i64 = 0;
    var i: i64 = 0;
    while i < 8 {
        let h = unsafe { spy_make_holder() };
        let o = Outer { inner: h, tag: 0 };
        total = total + o.inner.label.len() + o.tag;
        i = i + 1;
    }
    let made = unsafe { spy_made() };
    let releases = unsafe { spy_releases() };
    println(f"total={total}");
    println(f"made={made}");
    println(f"releases={releases}");

    unsafe { spy_release_one_from_host(); }
    let after = unsafe { spy_releases() };
    println(f"after_host_release={after}");
    0
}
"#;

#[test]
fn a_container_over_a_let_bound_extern_record_sees_no_caller_release() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_letbind_embed", RECORD_SPY_RUST) else {
        return;
    };

    let stdout = build_and_run(
        dir.path(),
        "letbind_embed",
        LET_BOUND_EXTERN_RECORD_INSIDE_A_CONTAINER,
        Some(&spy),
    );

    assert_eq!(
        reported(&stdout, "made"),
        8,
        "guard: the host must have minted all eight handles:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "total"),
        72,
        "guard: every embedded record must have been readable:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "releases"),
        0,
        "DOUBLE RELEASE: the foreign fact must travel with the BINDER into the \
         container, not be read only off a container's own initializer:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "after_host_release"),
        1,
        "the release counter itself must have teeth:\n{stdout}"
    );
}

/// CONTROL for the binder rule: an identically shaped binder over a DOMESTIC
/// producer still compiles, still runs and still leaves the counter with
/// teeth.
///
/// The release-count COUNTERFACTUAL for this construct lives at the MIR seam
/// (`extern_wrapper_result_opacity::a_let_bound_domestic_record_still_gets_\
/// its_scope_exit_drop`, which pins three `RecordInPlace` drops against the
/// foreign shape's zero). It cannot be taken here: handing a heap field to an
/// extern already releases the caller's obligation for it — an extern's
/// argument disposition is unknowable, so the compiler must assume the host
/// consumed it — which means the spy can never witness a domestic record's own
/// drop. Every existing domestic control in this file is shaped the same way
/// for the same reason.
const LET_BOUND_DOMESTIC_RECORD: &str = r#"extern "C" {
    fn spy_retained() -> i64;
    fn spy_bad_headers() -> i64;
    fn spy_releases() -> i64;
    fn spy_release_one_from_host() -> i64;
    fn spy_retain(s: string) -> i64;
}

type Holder { label: string }

fn mkHolder(i: i64) -> Holder { Holder { label: f"tok{i}" } }

fn main() -> i64 {
    var total: i64 = 0;
    var i: i64 = 0;
    while i < 8 {
        let h = mkHolder(i);
        unsafe { spy_retain(h.label); }
        total = total + 1;
        i = i + 1;
    }
    let retained = unsafe { spy_retained() };
    let bad = unsafe { spy_bad_headers() };
    let releases = unsafe { spy_releases() };
    println(f"total={total}");
    println(f"retained={retained}");
    println(f"bad={bad}");
    println(f"releases={releases}");

    unsafe { spy_release_one_from_host(); }
    let after = unsafe { spy_releases() };
    println(f"after_host_release={after}");
    0
}
"#;

#[test]
fn a_let_bound_domestic_record_still_releases_once_per_frame() {
    require_codegen();
    let dir = tempdir();
    let Some(spy) = build_staticlib(dir.path(), "spy_letbind_domestic", SPY_RUST) else {
        return;
    };

    let stdout = build_and_run(
        dir.path(),
        "letbind_domestic",
        LET_BOUND_DOMESTIC_RECORD,
        Some(&spy),
    );

    assert_eq!(
        reported(&stdout, "retained"),
        8,
        "guard: the spy must have seen all eight domestic labels:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "bad"),
        0,
        "guard: every observed label must have carried a Hew string header:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "total"),
        8,
        "guard: every domestic binder must have been readable:\n{stdout}"
    );
    assert_eq!(
        reported(&stdout, "after_host_release"),
        reported(&stdout, "releases") + 1,
        "control: the counter must still have teeth for the domestic shape — \
         one host release reads as exactly one more:\n{stdout}"
    );
}
