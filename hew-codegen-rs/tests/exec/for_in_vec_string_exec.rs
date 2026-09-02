//! End-to-end execution tests for `for line in vec_of_strings` over a
//! `Vec<string>`.
//!
//! `hew_vec_get_clone` returns a FRESH, solely-owned retained owner of the
//! element (a descriptor-backed semantic clone — NOT a borrow of the Vec's live
//! buffer slot). The for-in lowering therefore owes the iteration binding exactly one
//! `hew_string_drop` on EVERY path out of the loop body: the fall-through
//! back-edge, each branch-arm join, and the `break`/`continue` edges. An early
//! `return line` moves the single retained reference to the caller, so the
//! body-end drop is suppressed on that path (leak-not-double-free; the move
//! checker / function-scope machinery owns the escaped reference).
//!
//! These tests prove the real program behaviour, not just the lowered shape:
//! they compile + link + run each body shape and assert the exact stdout. The
//! MIR-shape companions (drop count / escape suppression / no-NYI) live in
//! `hew-mir/tests/cstring_container_domain_canary.rs`; the no-double-free /
//! no-extra-leak property is validated out-of-band under the macOS malloc
//! debugger (`MallocScribble`) and `leaks`, where the for-in path is byte-for-
//! byte equivalent to the established `for i in 0..len { let line = xs.get(i) }`
//! path.

#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-codegen-rs has a workspace parent")
        .to_path_buf()
}

fn target_dir(repo: &Path) -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR").map_or_else(
        || repo.join("target"),
        |dir| {
            let path = PathBuf::from(dir);
            if path.is_absolute() {
                path
            } else {
                repo.join(path)
            }
        },
    )
}

fn hew_bin(repo: &Path) -> PathBuf {
    target_dir(repo).join("debug").join("hew")
}

fn hew_command(repo: &Path) -> Command {
    let bin = hew_bin(repo);
    if bin.exists() {
        return Command::new(bin);
    }
    // Cold `target/`: build `hew` once under the shared serialized build
    // lock, OUTSIDE any per-test deadline, so a concurrent build-lock holder
    // cannot make a `cargo run` fallback burn the bounded budget and produce
    // a false timeout (hew-lang/hew#1887).
    Command::new(hew_testutil::ensure_hew_bin_built().expect("build hew binary"))
}

fn ensure_hew_runtime_lib(repo: &Path) {
    let _ = repo;
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        hew_testutil::ensure_hew_lib_built().expect("build libhew.a");
    });
}

fn run_hew_source(repo: &Path, stem: &str, source: &str) -> String {
    ensure_hew_runtime_lib(repo);
    let dir = std::env::temp_dir().join(format!(
        "hew-for-in-vecstring-{}-{stem}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");

    let mut command = hew_command(repo);
    command.arg("run").arg(&path);
    let output = super::run_hew_command(&mut command, format!("hew run {}", path.display()));
    assert!(
        output.status.success(),
        "hew run {} exited non-zero (status={:?}); stderr:\n{}",
        path.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).expect("stdout is utf-8")
}

#[test]
fn vec_iter_owned_yields_and_first_class_cursors_balance_exact_counts() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "owned_yield_counts",
        r#"
        fn first(values: Vec<Rc<i64>>) -> Rc<i64> {
            for value in values.iter() {
                return value;
            }
            panic("missing Rc value")
        }

        fn returned_roundtrip(values: Vec<Rc<i64>>, root: Rc<i64>) -> i64 {
            let returned = first(values);
            if returned.get() != 7 { panic("Rc returned value"); }
            if root.strong_count() != 3 { panic("Rc return transfer count"); }
            returned.get()
        }

        fn manual_roundtrip(values: Vec<Rc<i64>>, root: Rc<i64>) {
            var cursor = values.iter();
            if root.strong_count() != 3 { panic("Rc manual snapshot count"); }
            let _ = cursor.next();
            if root.strong_count() != 3 { panic("Rc ignored next release count"); }
        }

        fn tuple_total() -> i64 {
            let pairs: Vec<(string, string)> = Vec.new();
            pairs.push(("alpha" + "-left", "alpha" + "-right"));
            pairs.push(("beta" + "-left", "beta" + "-right"));
            var total = 0;
            for pair in pairs.iter() {
                total = total + pair.0.len() + pair.1.len();
            }
            total
        }

        fn main() {
            let root = Rc.new(7);
            let values: Vec<Rc<i64>> = Vec.new();
            values.push(root);
            if root.strong_count() != 2 { panic("Rc source count"); }

            for value in values.iter() {
                if root.strong_count() != 4 { panic("Rc normal yield count"); }
                if value.get() != 7 { panic("Rc normal yield value"); }
            }
            if root.strong_count() != 2 { panic("Rc normal exit count"); }

            for value in values.iter() {
                if root.strong_count() != 4 { panic("Rc break yield count"); }
                break;
            }
            if root.strong_count() != 2 { panic("Rc break exit count"); }

            if returned_roundtrip(values, root) != 7 { panic("Rc return result"); }
            if root.strong_count() != 2 { panic("Rc returned owner release count"); }

            manual_roundtrip(values, root);
            if root.strong_count() != 2 { panic("Rc manual cursor exit count"); }

            let edge = root.downgrade();
            let weak_values: Vec<Weak<i64>> = Vec.new();
            weak_values.push(edge);
            if root.weak_count() != 2 { panic("Weak source count"); }
            for weak in weak_values.iter() {
                if root.weak_count() != 4 { panic("Weak yield count"); }
                match weak.upgrade() {
                    .Some(owner) => {
                        if owner.get() != 7 { panic("Weak upgraded value"); }
                    },
                    .None => panic("Weak unexpectedly expired"),
                }
            }
            if root.weak_count() != 2 { panic("Weak loop exit count"); }

            if tuple_total() != 40 { panic("tuple recursive release result"); }
            print("balanced");
        }
        "#,
    );
    assert_eq!(stdout, "balanced");
}

#[test]
fn vec_iter_cursor_transfers_are_path_sensitive_and_overwrite_safe() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "cursor_path_sensitive",
        r#"
        fn conditional_let(values: Vec<Rc<i64>>, take: bool) {
            var cursor = values.iter();
            if take {
                let moved = cursor;
                let _ = moved;
            }
        }

        fn conditional_var(values: Vec<Rc<i64>>, take: bool) {
            var cursor = values.iter();
            if take {
                var moved = cursor;
                let _ = moved.next();
            }
        }

        fn match_move(values: Vec<Rc<i64>>, choice: i64) {
            var cursor = values.iter();
            match choice {
                0 => {
                    let moved = cursor;
                    let _ = moved;
                },
                _ => {},
            }
        }

        fn match_value_move(values: Vec<Rc<i64>>, choice: i64) {
            var first = values.iter();
            var second = values.iter();
            var selected = match choice {
                0 => first,
                _ => second,
            };
            let _ = selected.next();
        }

        fn fresh_reassign(values: Vec<Rc<i64>>, root: Rc<i64>) {
            var cursor = values.iter();
            if root.strong_count() != 3 { panic("first fresh snapshot"); }
            cursor = values.iter();
            if root.strong_count() != 3 { panic("overwritten snapshot release"); }
        }

        fn binding_reassign(
            first_values: Vec<Rc<i64>>,
            second_values: Vec<Rc<i64>>,
            first_root: Rc<i64>,
            second_root: Rc<i64>,
        ) {
            var first = first_values.iter();
            var second = second_values.iter();
            if first_root.strong_count() != 3 { panic("first assignment snapshot"); }
            if second_root.strong_count() != 3 { panic("second assignment snapshot"); }
            first = second;
            if first_root.strong_count() != 2 { panic("assignment old destination release"); }
            if second_root.strong_count() != 3 { panic("assignment source transfer"); }
        }

        fn conditional_reassign(values: Vec<Rc<i64>>, root: Rc<i64>, take: bool) {
            var first = values.iter();
            var second = values.iter();
            if take { first = second; }
            let expected = if take { 3 } else { 4 };
            if root.strong_count() != expected { panic("conditional assignment state"); }
        }

        fn self_reassign(values: Vec<Rc<i64>>, root: Rc<i64>) {
            var cursor = values.iter();
            cursor = cursor;
            if root.strong_count() != 3 { panic("self assignment state"); }
        }

        fn returned_cursor(values: Vec<Rc<i64>>) -> VecIter<Rc<i64>> {
            let cursor = values.iter();
            cursor
        }

        fn match_returned_cursor(
            values: Vec<Rc<i64>>,
            choice: i64,
        ) -> VecIter<Rc<i64>> {
            var first = values.iter();
            var second = values.iter();
            match choice {
                0 => first,
                _ => second,
            }
        }

        fn consume_returned(values: Vec<Rc<i64>>, root: Rc<i64>) {
            var cursor = returned_cursor(values);
            if root.strong_count() != 3 { panic("returned cursor transfer"); }
            let _ = cursor.next();
            if root.strong_count() != 3 { panic("returned cursor ignored next"); }
        }

        fn consume_match_returned(
            values: Vec<Rc<i64>>,
            root: Rc<i64>,
            choice: i64,
        ) {
            var cursor = match_returned_cursor(values, choice);
            if root.strong_count() != 3 { panic("match returned cursor transfer"); }
            let _ = cursor.next();
        }

        fn block_returned_cursor(values: Vec<Rc<i64>>) -> VecIter<Rc<i64>> {
            var inspected = values.iter();
            var returned = values.iter();
            {
                let _ = inspected.next();
                returned
            }
        }

        fn consume_block_returned(values: Vec<Rc<i64>>, root: Rc<i64>) {
            var cursor = block_returned_cursor(values);
            if root.strong_count() != 3 { panic("block returned cursor transfer"); }
            let _ = cursor.next();
        }

        fn early_return(values: Vec<Rc<i64>>, take: bool) {
            var cursor = values.iter();
            if take { return; }
            let _ = cursor.next();
        }

        fn loop_edges(values: Vec<Rc<i64>>) {
            var i = 0;
            while i < 2 {
                var cursor = values.iter();
                if i == 0 {
                    let moved = cursor;
                    let _ = moved;
                    i = i + 1;
                    continue;
                }
                break;
            }
        }

        fn main() {
            let root = Rc.new(11);
            let values: Vec<Rc<i64>> = Vec.new();
            values.push(root);
            print(root.strong_count());

            conditional_let(values, false);
            conditional_var(values, false);
            match_move(values, 1);
            match_value_move(values, 0);
            match_value_move(values, 1);
            print(root.strong_count());

            fresh_reassign(values, root);
            print(root.strong_count());

            conditional_let(values, true);
            conditional_var(values, true);
            match_move(values, 0);
            print(root.strong_count());

            conditional_reassign(values, root, false);
            conditional_reassign(values, root, true);
            self_reassign(values, root);
            consume_returned(values, root);
            consume_match_returned(values, root, 0);
            consume_match_returned(values, root, 1);
            consume_block_returned(values, root);
            early_return(values, true);
            early_return(values, false);
            loop_edges(values);
            print(root.strong_count());

            let other_root = Rc.new(22);
            let other_values: Vec<Rc<i64>> = Vec.new();
            other_values.push(other_root);
            binding_reassign(values, other_values, root, other_root);
            if root.strong_count() != 2 { panic("assignment first final count"); }
            if other_root.strong_count() != 2 { panic("assignment second final count"); }
        }
        "#,
    );
    assert_eq!(
        stdout, "22222",
        "untaken moves, overwrites, sibling branches, returns, and loop edges \
         must leave the source Rc counts unchanged"
    );
}

#[test]
fn vec_iter_non_owning_values_and_call_carriers_release_exactly_once() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "cursor_non_owning_values",
        r#"
        fn consume_cursor(cursor: VecIter<Rc<i64>>) {
            let moved = cursor;
            let _ = moved;
        }

        fn main() {
            let root = Rc.new(11);
            let values: Vec<Rc<i64>> = Vec.new();
            values.push(root);

            {
                var cursor = values.iter();
                consume_cursor(cursor);
            }
            if root.strong_count() != 2 { panic("call-carrier local transfer"); }

            {
                var cursor = values.iter();
                { cursor };
                let _ = cursor.next();
            }
            if root.strong_count() != 2 { panic("discarded block tail"); }

            {
                var first = values.iter();
                var second = values.iter();
                let _ = if true { first } else { second };
                let _ = first.next();
                let _ = second.next();
            }
            if root.strong_count() != 2 { panic("discarded if value"); }

            {
                var first = values.iter();
                var second = values.iter();
                let _ = match 0 {
                    0 => first,
                    _ => second,
                };
                let _ = first.next();
                let _ = second.next();
            }
            if root.strong_count() != 2 { panic("discarded match value"); }

            {
                var cursor = values.iter();
                let inspect = |incoming: VecIter<Rc<i64>>| {
                    let _ = incoming;
                };
                inspect(cursor);
                let _ = cursor.next();
                inspect(values.iter());
            }
            if root.strong_count() != 2 { panic("borrow-only closure argument"); }

            print("balanced");
        }
        "#,
    );
    assert_eq!(stdout, "balanced");
}

/// The headline demo shape: a branched body that prefixes non-empty lines. Both
/// arms read `line` by-value via string concat (a borrow), so the retained
/// element is released once at the branch-join per iteration. `split("\n")` on
/// `"a\nb\n"` yields `["a", "b", ""]`, so the empty trailing segment takes the
/// else arm.
#[test]
fn for_in_branched_transform_body_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "branched",
        r#"
        fn transform_body(body: string) -> string {
            var out = "";
            for line in body.split("\n") {
                if line.len() > 0 { out = out + "PROXY> " + line + "\n"; }
                else { out = out + "\n"; }
            }
            out
        }
        fn main() { print(transform_body("a\nb\n")); }
        "#,
    );
    assert_eq!(stdout, "PROXY> a\nPROXY> b\n\n");
}

/// A simple linear body that concatenates every element.
#[test]
fn for_in_simple_concat_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "simple",
        r#"
        fn join_all(lines: Vec<string>) -> string {
            var out = "";
            for line in lines { out = out + line; }
            out
        }
        fn main() {
            let v: Vec<string> = Vec.new();
            v.push("foo"); v.push("bar"); v.push("baz");
            print(join_all(v));
        }
        "#,
    );
    assert_eq!(stdout, "foobarbaz");
}

/// `continue` skips empty elements; the continued iteration's retained element
/// is freed on the continue edge.
#[test]
fn for_in_continue_skips_empty_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "continue",
        r#"
        fn keep_nonempty(lines: Vec<string>) -> string {
            var out = "";
            for line in lines {
                if line.len() == 0 { continue; }
                out = out + line + ";";
            }
            out
        }
        fn main() {
            let v: Vec<string> = Vec.new();
            v.push("a"); v.push(""); v.push("b"); v.push(""); v.push("c");
            print(keep_nonempty(v));
        }
        "#,
    );
    assert_eq!(stdout, "a;b;c;");
}

/// `break` stops at a sentinel; the breaking iteration's retained element is
/// freed on the break edge before the loop-exit goto.
#[test]
fn for_in_break_at_sentinel_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "break",
        r#"
        fn until_stop(lines: Vec<string>) -> string {
            var out = "";
            for line in lines {
                if line == "STOP" { break; }
                out = out + line + ",";
            }
            out
        }
        fn main() {
            let v: Vec<string> = Vec.new();
            v.push("one"); v.push("two"); v.push("STOP"); v.push("three");
            print(until_stop(v));
        }
        "#,
    );
    assert_eq!(stdout, "one,two,");
}

/// An unused binding still drops each retained element; the count is correct.
#[test]
fn for_in_unused_binding_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "unused",
        r#"
        fn count_iters(lines: Vec<string>) -> i64 {
            var n = 0;
            for line in lines { n = n + 1; }
            n
        }
        fn main() {
            let v: Vec<string> = Vec.new();
            v.push("x"); v.push("y"); v.push("z");
            print(f"{count_iters(v)}");
        }
        "#,
    );
    assert_eq!(stdout, "3");
}

/// Ownership escape: `return line` hands the retained reference to the caller.
/// The body-end drop is suppressed; the returned value is printed intact (no
/// use-after-free of a wrongly-dropped buffer).
#[test]
fn for_in_returned_binding_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "return_escape",
        r#"
        fn first_match(lines: Vec<string>, needle: string) -> string {
            for line in lines {
                if line == needle { return line; }
            }
            "none"
        }
        fn main() {
            let v: Vec<string> = Vec.new();
            v.push("alpha"); v.push("beta"); v.push("gamma");
            print(first_match(v, "beta"));
        }
        "#,
    );
    assert_eq!(stdout, "beta");
}

/// High-iteration branched body (10000 retained-then-dropped elements). A
/// double-free of any element would corrupt the accumulated result or abort
/// under the malloc debugger; a leaked element would show as a growing RSS but
/// the result stays exact. 200 rounds x 50 lines x 7 chars per line = 70000.
#[test]
fn for_in_stress_branched_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "stress",
        r#"
        fn build(n: i64) -> Vec<string> {
            var v: Vec<string> = Vec.new();
            for i in 0..n {
                if i % 3 == 0 { v.push("fizz"); } else { v.push("buzz"); }
            }
            v
        }
        fn transform(lines: Vec<string>) -> string {
            var out = "";
            for line in lines {
                if line.len() > 3 { out = out + "L:" + line + "\n"; }
                else { out = out + "S:" + line + "\n"; }
            }
            out
        }
        fn main() {
            var total = 0;
            for round in 0..200 {
                let lines = build(50);
                let s = transform(lines);
                total = total + s.len();
            }
            print(f"{total}");
        }
        "#,
    );
    assert_eq!(stdout, "70000");
}
