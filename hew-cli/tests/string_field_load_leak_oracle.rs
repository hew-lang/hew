//! Constant-leak / double-free oracle for reading a `string` field out of an
//! owned aggregate (`r.field`, `t.0`).
//!
//! ## The bug this pins (issue #37 covered producers, not reads)
//!
//! A `string` loaded from a record field (`RecordFieldLoad`) or tuple element
//! (`TupleFieldLoad`) was emitted as a RAW `build_load` of the field pointer
//! with no `hew_string_clone` retain (`hew-codegen-rs/src/llvm.rs`). The loaded
//! local was therefore a bit-ALIAS of the parent aggregate's field buffer,
//! refcount un-bumped. When that alias was then CONSUMED — moved into a
//! container (`hew_hashmap_insert` takes possession of the key on a vacant
//! slot) or returned in two positions — both the consumer's drop AND the
//! parent's own drop freed the SAME buffer: a double-free.
//!
//! Issue #37 fixed fresh-PRODUCED string temporaries (`a + b`,
//! `hew_vec_get_str` for `xs.get(i)`). A field READ out of a still-live owned
//! aggregate is a different ownership shape its producer set never covered.
//!
//! ## The two-sided fix (why a pure codegen retain would invert to a leak)
//!
//! The MIR ownership derivation already SUPPRESSED any scope-exit drop for a
//! projection-alias field-load dest (`projection_alias_dest` seeds the taint;
//! `derive_cow_sole_owner` / `derive_cow_fresh_borrowed_owner` exclude every
//! tainted local). So emitting the codegen retain ALONE would convert the
//! double-free into a silent LEAK — the new `+1` owner would have no balancing
//! drop. The fix is two-sided, mirroring the already-shipped `hew_vec_get_str`
//! (`xs.get(i)`) precedent:
//!   - codegen emits `hew_string_clone` at the string field-load site, and
//!   - the now-retained field-load dest is registered as a fresh `+1` string
//!     producer (`fresh_string_producer_dest`) and excluded from the
//!     projection-alias taint, so the existing drop machinery balances it for
//!     exactly one scope-exit `hew_string_drop`.
//!
//! ## Methodology: no-double-free pins under the poisoned-allocator triple
//!
//! All three fixtures are pinned by running the compiled binary under
//! `MallocScribble` / `MallocPreScribble` / `MallocGuardEdges`, where ANY
//! over-free of a string buffer aborts via the
//! `hew-cabi: free_cstring: ... double-free. Aborting` sentinel. The binding
//! teeth is "runs clean (no abort) and prints its `OK` sentinel" on any unix
//! host — no macOS `leaks(1)` dependency.
//!
//! - `field_load_into_consume` and `dup_field_read_into_return` are the two
//!   ESCAPE shapes: pre-fix the un-retained field alias is consumed by two sinks
//!   (a `HashMap` insert pair; a duplicate tuple-element read) while the parent
//!   aggregate also owns the buffer, so both ends free it — the double-free the
//!   sentinel catches. Post-fix each load is an independent retained `+1`,
//!   released exactly once.
//! - `transient_field_read_no_over_drop` is the no-OVER-drop pin for the
//!   universal retain: a string field is read transiently while the parent
//!   aggregate's own scope-exit drop ALSO fires on the (refcount-shared) buffer.
//!   The retain's matching `hew_string_drop` and the parent's drop must net to
//!   exactly the right refcount — a mis-balanced retain/drop would over-decrement
//!   and double-free the shared buffer (sentinel abort) or scribble a live
//!   string (corrupted output). This is the `assert-distinguishes-garbage`
//!   teeth: the assertion is the exact-output `==` check, not a happy-path
//!   smoke run.
//!
//! ## Why no `leaks --atExit` floor fixture (the confounder)
//!
//! A `leaks` floor-equality probe CANNOT isolate this retain's balance. Reading a
//! `string` field out of an aggregate SUPPRESSES that aggregate's own in-place
//! field drop (the read marks the parent a live operand), so the parent's
//! original refcount on the buffer is never released — a pre-existing leak in the
//! managed-aggregate-field-drop spine (D2, OUT of scope; equal pre- and post-fix,
//! confirmed by measuring `0d42b5b0` baseline vs the fix: both leak the same
//! count). Crucially the retained field-load value and the parent's field SHARE
//! one refcounted buffer, so an UN-balanced retain does NOT add a distinct leaked
//! node — it just leaves the shared buffer at a higher refcount, which `leaks`
//! counts as the SAME one leaked buffer. Balanced vs un-balanced are therefore
//! indistinguishable by node count for the no-escape shape. The retain/drop
//! balance is instead proven structurally (one retain emitted; one
//! `hew_string_drop` scheduled per field load on every exit + loop-back edge,
//! verified via `hew compile --dump-mir elab`) and dynamically by the
//! no-over-drop pin above.

#![cfg(unix)]

mod support;

use std::fmt::Write as _;
use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Iteration count for the transient no-over-drop fixture. Each iteration loads
/// a fresh string field and the parent record drops it; running many iterations
/// makes a per-call over-decrement of the shared refcount overwhelmingly likely
/// to abort before the checksum prints.
const FIELD_LOAD_COUNT: i64 = 8;

// -- fixture sources ---------------------------------------------------------

/// Transient no-over-drop fixture: a string field read into a NAMED local and
/// only borrow-used (`.len()`), while the parent record's own scope-exit drop
/// ALSO fires on the (refcount-shared) field buffer. The named binding is the
/// load-bearing shape: the inline bare-temp release
/// (`apply_nested_fresh_string_temp_drops`) covers `r.field.len()` as one
/// expression, but `let c = r.field; c.len();` is a real binding whose drop is
/// owned by the function-scope derivation — exactly where the codegen retain must
/// earn its balancing `hew_string_drop` (the MIR fresh-producer admission).
///
/// The teeth is no-OVER-drop under the poisoned allocator: the retain's matching
/// drop plus the parent record's drop both act on one shared refcounted buffer,
/// so a mis-balanced retain/drop over-decrements and double-frees (sentinel
/// abort) or scribbles a still-live string (corrupted checksum). A record local
/// (not a `Vec<Row>`) is used so the parent's own field drop genuinely fires;
/// the literal field source keeps every buffer header-aware. The printed
/// checksum is the exact-value assertion (`assert-distinguishes-garbage`).
fn transient_field_read_source() -> String {
    let mut body = String::new();
    body.push_str("type Box { key: string; n: i64; }\n");
    body.push_str("fn main() {\n");
    body.push_str("    var total: i64 = 0;\n");
    body.push_str("    var i: i64 = 0;\n");
    let _ = writeln!(body, "    while i < {FIELD_LOAD_COUNT} {{");
    // Distinct heap key per iteration (concat is a fresh-allocation producer, so
    // each `key` is a real owned buffer the record owns and must release).
    body.push_str("        let key = \"k\" + \"ey\";\n");
    body.push_str("        let b = Box { key: key, n: i };\n");
    // Named-local rebind of the string field + a borrowing `.len()` read. This is
    // the field load whose retain must be balanced. The parent `b`'s drop fires
    // at the end of the iteration on the same shared buffer.
    body.push_str("        let c = b.key;\n");
    body.push_str("        total = total + c.len() + b.n;\n");
    body.push_str("        i = i + 1;\n");
    body.push_str("    }\n");
    // The checksum (`FIELD_LOAD_COUNT * len("key") + sum(0..FIELD_LOAD_COUNT)`)
    // is recomputed and asserted exactly by the test; a scribbled `c` or `b.n`
    // would shift it.
    body.push_str("    print(total);\n");
    body.push_str("    print(\"OK\");\n");
    body.push_str("}\n");
    body
}

/// F1.2 escape fixture (HashMap-insert path): load `row.category` and CONSUME it
/// as the key of TWO distinct `insert`s while `row` is a borrow of a still-live
/// `Vec<Row>`. Pre-fix each field alias was consumed (vacant insert takes
/// possession with no retain) by two maps AND owned by the Vec record — a
/// double/triple-free at teardown. The `MallocScribble` sentinel aborts on it.
const FIELD_LOAD_INTO_CONSUME_SOURCE: &str = "\
import std::string;\n\
type Row { category: string; score: i64; }\n\
fn parse_row(line: string, sc: i64) -> Row {\n\
\x20   let parts: Vec<string> = line.split(\",\");\n\
\x20   Row { category: parts.get(0), score: sc }\n\
}\n\
fn main() {\n\
\x20   let lines: Vec<string> = Vec::new();\n\
\x20   lines.push(\"math,x\"); lines.push(\"science,x\"); lines.push(\"math,x\");\n\
\x20   lines.push(\"science,x\"); lines.push(\"math,x\"); lines.push(\"science,x\");\n\
\x20   let rows: Vec<Row> = Vec::new();\n\
\x20   let n = lines.len();\n\
\x20   for i in 0 .. n { let line = lines.get(i); let row = parse_row(line, i); rows.push(row); }\n\
\x20   let totals: HashMap<string, i64> = HashMap::new();\n\
\x20   let counts: HashMap<string, i64> = HashMap::new();\n\
\x20   let m = rows.len();\n\
\x20   for i in 0 .. m {\n\
\x20       let row = rows.get(i);\n\
\x20       totals.insert(row.category, row.score);\n\
\x20       counts.insert(row.category, 1);\n\
\x20   }\n\
\x20   println(\"OK\");\n\
}\n";

/// F1.1 escape fixture (`scanner.next_line` path): a tuple whose `string` element
/// is read into TWO positions — a `match` arm that prints it AND a value the
/// caller keeps. Pre-fix the same field buffer was aliased into both, and both
/// drops freed it. The `MallocScribble` sentinel aborts on the double-free.
const DUP_FIELD_READ_INTO_RETURN_SOURCE: &str = "\
import std::string;\n\
fn first_field(line: string) -> (string, i64) {\n\
\x20   let parts: Vec<string> = line.split(\",\");\n\
\x20   (parts.get(0), 1)\n\
}\n\
fn main() {\n\
\x20   let lines: Vec<string> = Vec::new();\n\
\x20   lines.push(\"alpha,x\"); lines.push(\"beta,x\"); lines.push(\"gamma,x\");\n\
\x20   lines.push(\"delta,x\"); lines.push(\"epsilon,x\"); lines.push(\"zeta,x\");\n\
\x20   let totals: HashMap<string, i64> = HashMap::new();\n\
\x20   let counts: HashMap<string, i64> = HashMap::new();\n\
\x20   let n = lines.len();\n\
\x20   for i in 0 .. n {\n\
\x20       let p = first_field(lines.get(i));\n\
\x20       totals.insert(p.0, p.1);\n\
\x20       counts.insert(p.0, 1);\n\
\x20   }\n\
\x20   println(\"OK\");\n\
}\n";

// -- leak measurement plumbing (same shape as vec_string_index_compare) ------

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

/// Compile `source` and run it under the poisoned-allocator triple, asserting it
/// exits cleanly (no double-free abort) and its stdout EXACTLY equals
/// `expected_stdout`. Any over-free of a string buffer aborts via the
/// `hew-cabi: free_cstring: ... double-free` sentinel (non-zero exit); any
/// over-drop that scribbles a live string corrupts the output. The exact-output
/// check is the `assert-distinguishes-garbage` teeth — not a happy-path
/// substring smoke test.
fn assert_clean_under_malloc_scribble(shape_name: &str, source: &str, expected_stdout: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("str-field-load-df-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run fixture binary");

    assert!(
        output.status.success(),
        "{shape_name}: a `string` field load must be a retained `+1` owner \
         released EXACTLY once -- a crash here is an over-free of a field buffer \
         (the parent aggregate AND a field-load owner both freed it, or a \
         transient retain over-decremented the shared refcount). The fix is the \
         two-sided codegen retain + MIR fresh-producer admission for string \
         `*FieldLoad`;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected_stdout,
        "{shape_name}: output must be exact -- a scribbled or truncated value \
         indicates an over-drop corrupted a live string buffer;\n{}",
        describe_output(&output)
    );
}

// -- oracles -----------------------------------------------------------------

/// F1.2: `row.category` (a string record field) consumed as a `HashMap` key in
/// two `insert`s while the parent `Row` lives in a `Vec<Row>`. Pre-fix this
/// double/triple-frees the field buffer (the two maps AND the Vec each free it).
/// Post-fix each insert gets an independent retained `+1` and the run is clean.
#[test]
fn field_load_into_consume_no_double_free() {
    assert_clean_under_malloc_scribble(
        "field_load_into_consume",
        FIELD_LOAD_INTO_CONSUME_SOURCE,
        "OK\n",
    );
}

/// F1.1: a tuple `string` element read into two consume positions (the
/// `scanner.next_line` shape, here `(parts.get(0), _).0` keyed into two maps).
/// Pre-fix the same buffer was aliased into both inserts and double-freed.
/// Post-fix each read is an independent retained `+1`.
#[test]
fn dup_field_read_into_return_no_double_free() {
    assert_clean_under_malloc_scribble(
        "dup_field_read_into_return",
        DUP_FIELD_READ_INTO_RETURN_SOURCE,
        "OK\n",
    );
}

/// No-OVER-drop: a string field read transiently into a named local while the
/// parent record's own drop fires on the shared refcounted buffer. The retain's
/// matching `hew_string_drop` plus the parent record drop must net to exactly the
/// right refcount; a mis-balanced retain/drop over-decrements and double-frees
/// (sentinel abort) or scribbles the live string (corrupted checksum). See the
/// module doc for why a `leaks` floor probe cannot isolate this shape (the
/// parent-field-drop leak is refcount-shared and out of scope).
#[test]
fn transient_field_read_no_over_drop() {
    // total = FIELD_LOAD_COUNT * len("key" = 3) + sum(0..FIELD_LOAD_COUNT)
    let expected_total: i64 = FIELD_LOAD_COUNT * 3 + (0..FIELD_LOAD_COUNT).sum::<i64>();
    let expected_stdout = format!("{expected_total}OK");
    assert_clean_under_malloc_scribble(
        "transient_field_read",
        &transient_field_read_source(),
        &expected_stdout,
    );
}
