//! State-field overwrite leak oracles for collection, record, and enum
//! actor state fields, plus alias-shape use-after-free pins.
//!
//! Companion of `bytes_drop_leak_oracle.rs` (which pins the bytes/string
//! overwrite release). These oracles pin the remaining overwrite-release
//! kinds in `lower_actor_state_field_store`:
//!
//!   * **Collections** (`Vec` / `HashMap` / `HashSet`): single-pointer
//!     handles released through the `collection_layout_witness` drop
//!     symbol behind the pointer-inequality guard. Pre-fix slope:
//!     ~2.6 leaked nodes per re-store across the three-field fixture.
//!
//!   * **Records / enums**: embedded aggregates released through the
//!     synthesised `__hew_{record,enum}_overwrite_release_<Name>` helper
//!     (collect new heap leaves → neutralise aliased old leaves → run the
//!     in-place drop spine). Pre-fix slope: 1 node per replaced leaf per
//!     re-store.
//!
//! ## Alias-shape UAF pins
//!
//! The record/enum release CANNOT blindly drop the old value: aggregate
//! loads byte-copy at refcount unchanged, so the incoming value may alias
//! old heap leaves. Three shapes pin the leak-over-UAF guard under the
//! poisoned-allocator triple (`MallocScribble` et al.) on every unix
//! host, independent of `leaks(1)`:
//!
//!   * functional update — `cur = Outer { label: cur.label, ... }`
//!     (same-position leaf reuse);
//!   * whole-value self-store — `prof = prof`;
//!   * cross-position swap — `pair = Pair { a: pair.b, b: pair.a }`.
//!
//! An over-eager release frees a buffer the field re-owns and the
//! scribbled read crashes (or returns a poisoned length) before the
//! exit-code assertion passes.
//!
//! ## Slope methodology
//!
//! Identical to `bytes_drop_leak_oracle.rs`: compile each shape at LOW
//! and HIGH frame counts, measure leak NODE counts under `leaks
//! --atExit` with the poisoned-allocator triple, and assert the delta
//! stays within a small constant independent of frames. macOS-only for
//! the `leaks` oracles; the UAF pins run on any unix.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the re-store path at least twice while
/// staying close to the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check (47-frame delta against the
/// +5 tolerance; the pre-fix slopes are ≥1 node/frame).
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Collection overwrite: each message replaces a `Vec<string>`, a
/// `HashMap<string, i64>`, and a `HashSet<i64>` state field with a fresh
/// populated collection. Pre-fix every replaced handle leaked its heap
/// permanently; post-fix each re-store releases the previous collection
/// through the layout-witness drop symbol and `state_drop` releases the
/// final ones exactly once.
fn collection_overwrite_source(frames: usize) -> String {
    format!(
        "actor Cache {{\n\
         \x20   var items: Vec<string>;\n\
         \x20   var index: HashMap<string, i64>;\n\
         \x20   var seen: HashSet<i64>;\n\
         \n\
         \x20   receive fn refresh(round: i64) {{\n\
         \x20       let next: Vec<string> = Vec::new();\n\
         \x20       next.push(\"entry\");\n\
         \x20       items = next;\n\
         \x20       let m: HashMap<string, i64> = HashMap::new();\n\
         \x20       m.insert(\"k\", round);\n\
         \x20       index = m;\n\
         \x20       let s: HashSet<i64> = HashSet::new();\n\
         \x20       s.insert(round);\n\
         \x20       seen = s;\n\
         \x20   }}\n\
         \n\
         \x20   receive fn size() -> i64 {{\n\
         \x20       items.len()\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let c = spawn Cache(items: Vec::new(), index: HashMap::new(), seen: HashSet::new());\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       c.refresh(i);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   sleep(2000ms);\n\
         \x20   match await c.size() {{\n\
         \x20       Ok(v) => v,\n\
         \x20       Err(_) => -1,\n\
         \x20   }}\n\
         }}\n"
    )
}

/// Record overwrite, functional-update shape: each message rebuilds the
/// record reusing the OLD `label` pointer (byte-copied alias — must NOT
/// be released) while replacing `payload` and the nested record's `note`
/// (must be released every frame). Pins both directions at once: a
/// missed release leaks one node per frame; an over-eager release of the
/// aliased `label` is a use-after-free the poisoned-allocator triple
/// crashes on. The durable `label` is built on the HEAP at spawn
/// (`.to_upper()`) so `hew_string_drop` would actually free it — a
/// static literal would make the alias guard vacuous.
fn record_functional_update_source(frames: usize) -> String {
    format!(
        "record Inner {{\n\
         \x20   note: string,\n\
         }}\n\
         \n\
         record Outer {{\n\
         \x20   label: string,\n\
         \x20   payload: bytes,\n\
         \x20   inner: Inner,\n\
         \x20   count: i64,\n\
         }}\n\
         \n\
         actor Keeper {{\n\
         \x20   var cur: Outer;\n\
         \n\
         \x20   receive fn bump() {{\n\
         \x20       cur = Outer {{\n\
         \x20           label: cur.label,\n\
         \x20           payload: \"outer-payload\".to_bytes(),\n\
         \x20           inner: Inner {{ note: \"inner-note\".to_upper() }},\n\
         \x20           count: cur.count + 1,\n\
         \x20       }};\n\
         \x20   }}\n\
         \n\
         \x20   receive fn count() -> i64 {{\n\
         \x20       cur.count\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let k = spawn Keeper(cur: Outer {{\n\
         \x20       label: \"durable-label\".to_upper(),\n\
         \x20       payload: bytes::new(),\n\
         \x20       inner: Inner {{ note: \"first\" }},\n\
         \x20       count: 0,\n\
         \x20   }});\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       k.bump();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   sleep(2000ms);\n\
         \x20   match await k.count() {{\n\
         \x20       Ok(v) => v,\n\
         \x20       Err(_) => -1,\n\
         \x20   }}\n\
         }}\n"
    )
}

/// Enum overwrite: cycles a three-variant enum (unit, one-string, and
/// string+i64 payloads) so every re-store crosses variants. Pre-fix the
/// replaced payload strings leaked one node per heap-bearing frame.
fn enum_overwrite_source(frames: usize) -> String {
    format!(
        "enum Status {{\n\
         \x20   Idle;\n\
         \x20   Working(string);\n\
         \x20   Done(string, i64);\n\
         }}\n\
         \n\
         actor Tracker {{\n\
         \x20   var status: Status;\n\
         \n\
         \x20   receive fn advance(n: i64) {{\n\
         \x20       status = match n % 3 {{\n\
         \x20           0 => Status::Idle,\n\
         \x20           1 => Status::Working(\"busy\".to_upper()),\n\
         \x20           _ => Status::Done(\"finished\".to_upper(), n),\n\
         \x20       }};\n\
         \x20   }}\n\
         \n\
         \x20   receive fn code() -> i64 {{\n\
         \x20       match status {{\n\
         \x20           Status::Idle => 0,\n\
         \x20           Status::Working(_) => 1,\n\
         \x20           Status::Done(_, n) => n,\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let t = spawn Tracker(status: Status::Idle);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       t.advance(i);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   sleep(2000ms);\n\
         \x20   match await t.code() {{\n\
         \x20       Ok(v) => v,\n\
         \x20       Err(_) => -1,\n\
         \x20   }}\n\
         }}\n"
    )
}

/// String inspect-then-overwrite: each frame READS the `string` state field
/// through a borrowing string call (`name.len()`) and then overwrites it
/// (`name = name + "x"`). The load feeding `hew_string_length` borrows the
/// field — it must NOT emit a `hew_string_clone` rc-bump. Pre-#2432-classifier
/// the string-receiver borrow was not recognised (`string_args` is a separate
/// contract axis from the Vec/bytes receiver axis), so the load classified
/// `Owned` and leaked one string buffer per frame (`string_slope`: 2 @3 → 49
/// @50 frames; merge-base 0/0). The store-side overwrite release of the OLD
/// buffer is orthogonal and already correct.
fn string_inspect_overwrite_source(frames: usize) -> String {
    format!(
        "actor Namer {{\n\
         \x20   var name: string;\n\
         \n\
         \x20   receive fn tick() {{\n\
         \x20       if name.len() < 1000000 {{\n\
         \x20           name = name + \"x\";\n\
         \x20       }}\n\
         \x20   }}\n\
         \n\
         \x20   receive fn size() -> i64 {{\n\
         \x20       name.len()\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let h = spawn Namer(name: \"seed\".to_upper());\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       h.tick();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   sleep(2000ms);\n\
         \x20   match await h.size() {{\n\
         \x20       Ok(v) => v,\n\
         \x20       Err(_) => -1,\n\
         \x20   }}\n\
         }}\n"
    )
}

/// Collection iteration (`for v in items`): each frame iterates a `Vec<i64>`
/// state field. The `for x in <collection>` desugar lowers the field load into
/// a synthetic `VecIter {{ vec: <load>, idx: 0 }}` cursor. Pre-#2432-classifier
/// the `RecordInit VecIter` fell through the wildcard as a whole-value escape,
/// so the load classified `Owned` and deep-cloned a Vec handle the cursor
/// never frees — a per-frame leak on THE most common actor-collection read
/// shape (`forin_slope`: 6 @3 → 100 @50 frames; merge-base 0/0). The cursor
/// BORROWS the source handle (reads via `len()`/`get(i)`, never frees it), so
/// the load must classify `Borrowed` and alias the actor's still-owned Vec.
fn collection_iterate_source(frames: usize) -> String {
    format!(
        "actor Summer {{\n\
         \x20   var items: Vec<i64>;\n\
         \n\
         \x20   receive fn spin() {{\n\
         \x20       var s: i64 = 0;\n\
         \x20       for v in items {{\n\
         \x20           s = s + v;\n\
         \x20       }}\n\
         \x20   }}\n\
         \n\
         \x20   receive fn size() -> i64 {{\n\
         \x20       items.len()\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let xs: Vec<i64> = Vec::new();\n\
         \x20   xs.push(1);\n\
         \x20   xs.push(2);\n\
         \x20   xs.push(3);\n\
         \x20   let h = spawn Summer(items: xs);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       h.spin();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   sleep(2000ms);\n\
         \x20   match await h.size() {{\n\
         \x20       Ok(v) => v,\n\
         \x20       Err(_) => -1,\n\
         \x20   }}\n\
         }}\n"
    )
}

/// Whole-value self-store: `prof = prof` byte-copies every leaf, so the
/// release must neutralise them ALL (drop nothing). The aliased `name`
/// leaf is HEAP-allocated at spawn (`.to_upper()`) so an over-eager
/// release would actually free it; the trailing read then crashes under
/// the poisoned triple (a static literal would never be freed, making
/// the pin vacuous). `"self-store-name".to_upper()` stays length 15.
const RECORD_SELF_STORE_SOURCE: &str = "\
record Profile {
    name: string,
    hits: i64,
}

actor Keeper {
    var prof: Profile;

    receive fn refresh() {
        prof = prof;
    }

    receive fn name_len() -> i64 {
        prof.name.len()
    }
}

fn main() -> i64 {
    let k = spawn Keeper(prof: Profile { name: \"self-store-name\".to_upper(), hits: 1 });
    var i: i64 = 0;
    while i < 25 {
        k.refresh();
        i = i + 1;
    }
    sleep(1000ms);
    match await k.name_len() {
        Ok(v) => v,
        Err(_) => -1,
    }
}
";

/// Cross-position swap: the new value reuses BOTH old leaves at swapped
/// positions. A per-position-only guard would free `a` while the new `b`
/// still points at it; the full-set neutralise keeps both alive. Both
/// leaves are HEAP-allocated at spawn (`.to_upper()`) so a wrongful
/// release actually frees them. After an odd number of swaps `a` holds
/// the original `b` string (`"RIGHT-SIDE"`, length 10).
const RECORD_SWAP_SOURCE: &str = "\
record Pair {
    a: string,
    b: string,
}

actor Swapper {
    var pair: Pair;

    receive fn swap() {
        pair = Pair { a: pair.b, b: pair.a };
    }

    receive fn len_a() -> i64 {
        pair.a.len()
    }
}

fn main() -> i64 {
    let s = spawn Swapper(pair: Pair { a: \"left\".to_upper(), b: \"right-side\".to_upper() });
    var i: i64 = 0;
    while i < 25 {
        s.swap();
        i = i + 1;
    }
    sleep(1000ms);
    match await s.len_a() {
        Ok(v) => v,
        Err(_) => -1,
    }
}
";

/// Temp-bridged two-field swap (#2432): `let t = x; x = y; y = t;` reads
/// `x` through a THIRD binding (`t`) before either field is overwritten —
/// two separate `ActorStateFieldLoad`s and two separate
/// `ActorStateFieldStore`s, unlike `RECORD_SWAP_SOURCE`'s single
/// self-referential store (`pair = Pair { a: pair.b, b: pair.a }`). The
/// second store's per-store alias guard has no visibility into `t`, a
/// still-live binding outside its own `(old, new)` pair: pre-fix,
/// `ActorStateFieldLoad` never retains, so `t` bit-aliases the field-`x`
/// buffer, the first store frees it out from under `t`, and the second
/// store either double-frees (Vec/record) or corrupts the string header —
/// deterministic SIGABRT / corrupted-header abort under the
/// poisoned-allocator triple. After an odd swap count `x` holds the
/// ORIGINAL `y` and vice versa — pinned by exact swapped value, not just
/// a clean exit code.
const VEC_SWAP_SOURCE: &str = "\
actor TwoVecs {
    var x: Vec<i64>;
    var y: Vec<i64>;

    receive fn flip() {
        let t = x;
        x = y;
        y = t;
    }

    receive fn sum_x() -> i64 {
        var s: i64 = 0;
        for v in x { s = s + v; }
        s
    }
}

fn main() -> i64 {
    let xs: Vec<i64> = Vec::new();
    xs.push(1); xs.push(2); xs.push(3);
    let ys: Vec<i64> = Vec::new();
    ys.push(10); ys.push(20); ys.push(30); ys.push(40);
    let a = spawn TwoVecs(x: xs, y: ys);
    var i: i64 = 0;
    while i < 25 {
        a.flip();
        i = i + 1;
    }
    sleep(1000ms);
    match await a.sum_x() {
        Ok(v) => v,
        Err(_) => -1,
    }
}
";

/// Temp-bridged two-field swap — string kind. Both leaves are
/// HEAP-allocated at spawn (`.to_upper()`), so a wrongful free or a
/// corrupted refcount header (the string-specific crash signature)
/// actually manifests. After an odd swap count `x` holds the ORIGINAL
/// `y` (`"RIGHT-SIDE"`, len 10).
const STRING_SWAP_SOURCE: &str = "\
actor TwoStrings {
    var x: string;
    var y: string;

    receive fn flip() {
        let t = x;
        x = y;
        y = t;
    }

    receive fn len_x() -> i64 {
        x.len()
    }
}

fn main() -> i64 {
    let a = spawn TwoStrings(x: \"left\".to_upper(), y: \"right-side\".to_upper());
    var i: i64 = 0;
    while i < 25 {
        a.flip();
        i = i + 1;
    }
    sleep(1000ms);
    match await a.len_x() {
        Ok(v) => v,
        Err(_) => -1,
    }
}
";

/// Temp-bridged two-field swap — record kind (`Box { v: Vec<i64> }`), the
/// nested-aggregate analogue of `VEC_SWAP_SOURCE`. After an odd swap count
/// `x` holds the ORIGINAL `y` (sum 100).
const RECORD_TEMP_SWAP_SOURCE: &str = "\
type Box {
    v: Vec<i64>,
}

actor TwoBoxes {
    var x: Box;
    var y: Box;

    receive fn flip() {
        let t = x;
        x = y;
        y = t;
    }

    receive fn sum_x() -> i64 {
        var s: i64 = 0;
        for v in x.v { s = s + v; }
        s
    }
}

fn main() -> i64 {
    let xv: Vec<i64> = Vec::new();
    xv.push(1); xv.push(2); xv.push(3);
    let yv: Vec<i64> = Vec::new();
    yv.push(10); yv.push(20); yv.push(30); yv.push(40);
    let a = spawn TwoBoxes(x: Box { v: xv }, y: Box { v: yv });
    var i: i64 = 0;
    while i < 25 {
        a.flip();
        i = i + 1;
    }
    sleep(1000ms);
    match await a.sum_x() {
        Ok(v) => v,
        Err(_) => -1,
    }
}
";

// ── plumbing (same shape as bytes_drop_leak_oracle) ───────────────────────

/// Compile `source` to a native binary via `hew compile --emit-dir` and
/// return the binary path.
fn compile_to_native(source: &str, dir: &std::path::Path, name: &str) -> PathBuf {
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

/// Run `bin` under the poisoned-allocator triple + `leaks --atExit` and
/// return `Some(leak_count)` when `leaks` produced a usable report.
fn measure_leaks(bin: &std::path::Path) -> Option<usize> {
    let output = Command::new("leaks")
        .arg("--atExit")
        .arg("--")
        .arg(bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .ok()?;
    if !output.status.success() && output.stdout.is_empty() {
        eprintln!(
            "skip: leaks declined to attach to {}: {}",
            bin.display(),
            String::from_utf8_lossy(&output.stderr)
        );
        return None;
    }
    let report = String::from_utf8_lossy(&output.stdout);
    let mut parsed: Option<usize> = None;
    for line in report.lines() {
        if !line.contains(" leaks for ") && !line.contains(" leak for ") {
            continue;
        }
        if let Some(rest) = line.strip_prefix("Process ") {
            if !rest.chars().next().is_some_and(|c| c.is_ascii_digit()) {
                continue;
            }
            if let Some(after_colon) = rest.split_once(": ").map(|(_, s)| s) {
                if let Some(n) = after_colon.split_whitespace().next() {
                    if let Ok(n) = n.parse::<usize>() {
                        eprintln!("  parsed leak count from line: {line}");
                        parsed = Some(n);
                        break;
                    }
                }
            }
        }
    }
    if parsed.is_none() {
        eprintln!(
            "skip: leaks did not emit a `Process <pid>: N leak(s) ...` summary for {}: \
             stderr=\n{}",
            bin.display(),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    parsed
}

/// Build the shape at LOW and HIGH frame counts, measure leak NODE
/// counts, and assert the delta stays within `SLOPE_TOLERANCE`.
fn assert_frame_slope_below_tolerance(shape_name: &str, source_fn: fn(usize) -> String) {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: {shape_name}: leaks(1) is macOS-only");
        return;
    }
    let leaks_avail = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !leaks_avail {
        eprintln!("skip: {shape_name}: `leaks` binary not on PATH");
        return;
    }

    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("overwrite-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let bin_low = compile_to_native(
        &source_fn(LOW_FRAMES),
        dir.path(),
        &format!("{shape_name}_low"),
    );
    let bin_high = compile_to_native(
        &source_fn(HIGH_FRAMES),
        dir.path(),
        &format!("{shape_name}_high"),
    );

    let Some(low_leaks) = measure_leaks(&bin_low) else {
        return;
    };
    let Some(high_leaks) = measure_leaks(&bin_high) else {
        return;
    };

    eprintln!(
        "{shape_name}: low_frames={LOW_FRAMES} low_leaks={low_leaks} \
         high_frames={HIGH_FRAMES} high_leaks={high_leaks} \
         tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "{shape_name}: per-frame leak SLOPE — low_frames={LOW_FRAMES} low_leaks={low_leaks}, \
         high_frames={HIGH_FRAMES} high_leaks={high_leaks}. Excess of {} NODES over the \
         tolerance of {SLOPE_TOLERANCE} indicates a per-re-store allocation is not being \
         released. Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see which \
         stack the leaked block came from.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
    assert!(
        high_leaks + SLOPE_TOLERANCE >= low_leaks,
        "{shape_name}: HIGH leak count is more than {SLOPE_TOLERANCE} below LOW \
         (low={low_leaks}, high={high_leaks}) — the actor likely did not finish \
         draining {HIGH_FRAMES} messages before `leaks --atExit` snapshotted. Increase \
         the `sleep_ms(...)` budget in the shape source."
    );
}

/// Compile `source` and run the binary under the poisoned-allocator
/// triple (no `leaks` dependency — works on any unix). Asserts the
/// process exits cleanly with `expected_exit` — an over-eager old-value
/// release turns the trailing state read into a scribbled-memory access
/// that crashes or corrupts the returned length.
fn assert_scribbled_run_exit(shape_name: &str, source: &str, expected_exit: i32) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("overwrite-uaf-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run fixture binary");
    assert_eq!(
        output.status.code(),
        Some(expected_exit),
        "{shape_name}: expected clean exit {expected_exit} under the poisoned-allocator \
         triple; an over-eager overwrite release frees a leaf the field re-owns \
         (use-after-free). Output:\n{}",
        describe_output(&output)
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// Collections: re-storing Vec/HashMap/HashSet state fields must release
/// the previous handles (pre-fix slope ~2.6 nodes/frame across the three
/// fields).
#[test]
fn collection_state_overwrite_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("collection_overwrite", collection_overwrite_source);
}

/// Records: the functional-update shape must release the replaced leaves
/// (payload, nested note) every frame while keeping the aliased `label`
/// alive (pre-fix slope ~3 nodes/frame; over-release crashes the run).
#[test]
fn record_state_overwrite_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("record_functional_update", record_functional_update_source);
}

/// Enums: cycling variants must release each replaced payload through
/// the tag-dispatched release (pre-fix slope ~0.7 nodes/frame).
#[test]
fn enum_state_overwrite_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("enum_overwrite", enum_overwrite_source);
}

/// #2432 — a per-frame handler that INSPECTS a `string` state field through a
/// borrowing string call (`name.len()`) and overwrites it must not clone the
/// field on the borrowing load. The own/borrow classifier recognises the
/// `string_args` borrow axis, so the load classifies `Borrowed` and emits no
/// unbalanced `hew_string_clone` (pre-classifier slope ~1 buffer/frame).
#[test]
fn string_state_inspect_overwrite_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("string_inspect_overwrite", string_inspect_overwrite_source);
}

/// #2432 — a per-frame handler that ITERATES a `Vec` state field
/// (`for v in items`) must borrow, not deep-clone, the collection: the
/// synthetic `VecIter` cursor borrows the source handle it never frees, so the
/// load classifies `Borrowed` (pre-classifier slope ~2 nodes/frame — the
/// cloned handle plus buffer the cursor never released).
#[test]
fn collection_state_iterate_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("collection_iterate", collection_iterate_source);
}

/// UAF pin — whole-value self-store: every leaf of the incoming value
/// aliases the old value; the release must free nothing and the final
/// read must see the heap-built name (`"SELF-STORE-NAME"`, len 15).
#[test]
fn record_self_store_keeps_aliased_leaves_alive() {
    assert_scribbled_run_exit("record_self_store", RECORD_SELF_STORE_SOURCE, 15);
}

/// UAF pin — cross-position swap: both old leaves reappear at swapped
/// positions. After an odd swap count `a` holds the original `b`
/// ("RIGHT-SIDE", len 10). A per-position-only alias guard frees them.
#[test]
fn record_swap_keeps_cross_position_leaves_alive() {
    assert_scribbled_run_exit("record_swap", RECORD_SWAP_SOURCE, 10);
}

/// UAF pin — functional update at HIGH frames: the aliased `label` leaf
/// survives 50 rebuilds and the final count is exact.
#[test]
fn record_functional_update_keeps_aliased_label_alive() {
    assert_scribbled_run_exit(
        "record_functional_update_uaf",
        &record_functional_update_source(HIGH_FRAMES),
        i32::try_from(HIGH_FRAMES).expect("frame count fits in i32"),
    );
}

/// UAF pin — #2432 temp-bridged swap, Vec kind: `let t = x; x = y; y = t;`
/// reads `x` through a third binding the per-store alias guard cannot see.
/// After 25 (odd) flips `x` holds the ORIGINAL `y` ([10,20,30,40], sum 100).
#[test]
fn vec_temp_bridged_swap_keeps_third_alias_alive() {
    assert_scribbled_run_exit("vec_temp_swap", VEC_SWAP_SOURCE, 100);
}

/// UAF pin — #2432 temp-bridged swap, string kind. After 25 (odd) flips
/// `x` holds the ORIGINAL `y` (`"RIGHT-SIDE"`, len 10).
#[test]
fn string_temp_bridged_swap_keeps_third_alias_alive() {
    assert_scribbled_run_exit("string_temp_swap", STRING_SWAP_SOURCE, 10);
}

/// UAF pin — #2432 temp-bridged swap, record kind (nested `Vec<i64>`
/// field). After 25 (odd) flips `x` holds the ORIGINAL `y` (sum 100).
#[test]
fn record_temp_bridged_swap_keeps_third_alias_alive() {
    assert_scribbled_run_exit("record_temp_swap", RECORD_TEMP_SWAP_SOURCE, 100);
}
