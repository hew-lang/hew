//! Double-free / independent-owner oracle for array-repeat `[x; N]` of an
//! owned (heap-owning) record element.
//!
//! ## The defect this pins (GitHub #2724)
//!
//! `[x; N]` where `x` is a record with a heap-owning field (`string`, or a
//! transitive `Vec`/`HashMap`/`HashSet`) desugared in HIR to an intermediate
//! `__hew_repeat_value` temp bound to a `Read`-load of the source place. For an
//! owned record a `Read`-load is a shallow struct memcpy: it copies the record's
//! inline bytes — including its field heap pointers — WITHOUT deep-cloning the
//! heap, and the checker never marked the source place moved. So the source
//! binding and the temp were two independent owners aliasing the same field
//! heap, and scope-exit drop elaboration ran `record_in_place` on BOTH:
//!
//!   * `Bag { items: Vec<i64> }`  → the field buffer is freed twice → SIGABRT
//!     (poisoned allocator / double-free).
//!   * `Person { name: string }`  → the field's C-string is released twice →
//!     the `hew-cabi` `free_cstring` header sentinel fires and aborts (rc 134)
//!     the moment the second drop runs — even without `MallocScribble`.
//!
//! The per-slot `.push` clones were already sound (`hew_vec_push_owned` deep-
//! copies each slot). The ONLY defective owner was the intermediate aliasing
//! temp. The fix (hew-hir/src/lower.rs `lower_array_repeat`) gives a place
//! source no temp at all: each loop iteration pushes a fresh `Read`-load of the
//! source place, which the sound per-slot clone deep-copies into an independent
//! owner; the source place drops exactly once at its own scope. `[Bag; N]` was
//! additionally fail-closed by the checker gate (`array_repeat_owned_element_
//! clonable`); the fix collapses that gate onto `vec_owned_element_admissible`,
//! so a `Vec`-field record is admitted (`RcFree`) while an `Rc`-field record stays
//! rejected.
//!
//! ## Teeth
//!
//! The defect is a HARD double-free ABORT (rc 134), not a silent leak, so the
//! primary pin is the exact-output run under the poisoned-allocator triple: a
//! double-free crashes before the sentinel prints. Each fixture concatenates its
//! string field on the heap (`"a" + "b"`) so the source really owns a heap
//! allocation the temp would alias — a bare interned literal would not bite.
//! Neutering the fix (restoring the aliasing temp) returns the abort. The
//! `[Person; N]` pin is RED on base today (the shape compiles pre-fix); the
//! `[Bag; N]` pins require the checker gate lift to compile.
//!
//! Pattern mirrors `vec_record_local_drop_leak_oracle.rs`.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// -- compile plumbing (shape shared with the sibling oracles) ----------------

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

/// Compile `source`, run the native binary under the poisoned-allocator triple,
/// and assert it exits 0 with EXACTLY `expected_stdout`. A double-free of a
/// record field heap crashes (rc 134, or a scribbled-value corruption) before
/// the sentinel prints.
fn assert_scribble_clean_exact(shape: &str, source: &str, expected_stdout: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("array-repeat-owned-{shape}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape);

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .unwrap_or_else(|e| panic!("run {shape} binary: {e}"));

    assert!(
        output.status.success(),
        "{shape}: array-repeat of an owned record must release each field heap EXACTLY once -- \
         a crash here is the #2724 double-free: the intermediate `__hew_repeat_value` temp \
         aliased the still-live source place and both dropped the same field heap;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected_stdout,
        "{shape}: output must be untouched -- a differing value indicates an over-drop corrupted \
         a live value or a slot did not receive an independent deep clone;\n{}",
        describe_output(&output)
    );
}

// -- fixtures ----------------------------------------------------------------

/// `[Person; 3]` where `Person { name: string }` and the name is heap-
/// concatenated. Pre-fix the shape COMPILES (string-field records are admitted)
/// and double-frees the C-string on the first `build()` scope exit -> rc 134
/// via the `free_cstring` sentinel before "OK" prints. Post-fix each slot is an
/// independent clone and the source drops once: prints "24OK". RED on base.
fn string_field_record_repeat_source() -> String {
    "\
type Person { name: string }

fn build() -> i64 {
    let p = Person { name: \"al\" + \"pha\" };
    let ps = [p; 3];
    ps.len()
}

fn main() {
    var total: i64 = 0;
    for i in 0..8 {
        total = total + build();
    }
    print(total);
    print(\"OK\");
}
"
    .to_string()
}

/// `[Bag; 3]` where `Bag { items: Vec<i64> }`. Pre-fix the checker gate
/// (`array_repeat_owned_element_clonable`) fail-closes this shape, so it does
/// not compile until the gate lift. Post-lift the Vec-field buffer is deep-
/// cloned per slot and the source drops once: prints "24OK". A double-free of
/// the `Vec` buffer would SIGABRT under the poisoned allocator.
fn vec_field_record_repeat_source() -> String {
    "\
type Bag { items: Vec<i64> }

fn build() -> i64 {
    let b = Bag { items: [1, 2, 3] };
    let bs = [b; 3];
    bs.len()
}

fn main() {
    var total: i64 = 0;
    for i in 0..8 {
        total = total + build();
    }
    print(total);
    print(\"OK\");
}
"
    .to_string()
}

/// Slot independence: `[src; 3]` of a `Vec`-field record. Each of the 3 slots
/// must be an INDEPENDENT deep clone that holds the full source contents, and
/// the source place must stay live and usable afterwards (the fix keeps `x`
/// usable -- no accept-set regression to use-of-moved-value). Any aliasing or
/// over-drop corrupts a slot's contents or crashes.
///
/// acc = 3 slots * (len 3 + first elem 10) + source len 3
///     = 3 * 13 + 3 = 42
fn slot_independence_source() -> String {
    "\
type Bag { items: Vec<i64> }

fn main() {
    let src = Bag { items: [10, 20, 30] };
    let bs = [src; 3];
    var acc: i64 = 0;
    for i in 0..3 {
        acc = acc + bs[i].items.len();
        acc = acc + bs[i].items[0];
    }
    acc = acc + src.items.len();
    print(acc);
    print(\"OK\");
}
"
    .to_string()
}

/// Zero-count control: `[f(); 0]` must evaluate a value-producing source once
/// (the eval-once temp is retained for non-place sources) and drop it once even
/// with zero iterations -- no double-free, empty Vec.
fn zero_count_value_source() -> String {
    "\
type Person { name: string }

fn make() -> Person {
    Person { name: \"z\" + \"z\" }
}

fn main() {
    let ps = [make(); 0];
    print(ps.len());
    print(\"OK\");
}
"
    .to_string()
}

// -- oracles -----------------------------------------------------------------

/// `[Person; 3]` (string-field record). RED on base (double-free abort), GREEN
/// post HIR fix. Neutering the fix (restoring the aliasing temp) returns the
/// rc-134 abort here.
#[test]
fn string_field_record_array_repeat_no_double_free() {
    assert_scribble_clean_exact(
        "person_string_field",
        &string_field_record_repeat_source(),
        "24OK",
    );
}

/// `[Bag; 3]` (Vec-field record). Requires the checker gate lift to compile;
/// GREEN post lift. A double-free of the Vec buffer SIGABRTs under the poisoned
/// allocator.
#[test]
fn vec_field_record_array_repeat_no_double_free() {
    assert_scribble_clean_exact("bag_vec_field", &vec_field_record_repeat_source(), "24OK");
}

/// Each repeat slot is an independent deep clone and the source stays live.
#[test]
fn array_repeat_slots_are_independent_owners() {
    assert_scribble_clean_exact("slot_independence", &slot_independence_source(), "42OK");
}

/// `[f(); 0]` value-producing source: evaluated + dropped exactly once even at
/// zero iterations; empty Vec.
#[test]
fn array_repeat_zero_count_value_source_drops_once() {
    assert_scribble_clean_exact("zero_count_value", &zero_count_value_source(), "0OK");
}
