//! End-to-end execution tests for generator construction + consumption.
//!
//! Exercises the full pipeline: checker `.next()` rewrite → HIR `GeneratorNext`
//! → MIR `Instr::GeneratorNext` + `Terminator::MakeGenerator` → codegen
//! (`hew_gen_ctx_create`, `hew_gen_next` + Option unbox, `hew_gen_free` on drop)
//! → thread-based runtime → native execution. These are value oracles: they
//! assert the printed value, not exit-0.
//!
//! The cross-yield-live-local case is the grounding probe for whether the
//! thread-based runtime alone preserves a local across a suspension (it does):
//! `let k = 10; yield 1; yield k` must yield `10` on the second `.next()`.

#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;
use std::time::Duration;

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
    let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let mut command = Command::new(cargo);
    command
        .current_dir(repo)
        .args(["run", "--quiet", "-p", "hew-cli", "--bin", "hew", "--"]);
    command
}

fn ensure_hew_runtime_lib(repo: &Path) {
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        let lib = target_dir(repo).join("debug").join("libhew.a");
        // Always ask cargo to (re)build libhew.a rather than short-circuiting on
        // its mere presence. Cargo's own fingerprint makes this a fast no-op when
        // the archive is current and regenerates it when the toolchain (rustc /
        // bundled LLVM) or the runtime/stdlib sources changed. A bare
        // `lib.exists()` early-return reused a stale archive after a toolchain
        // upgrade, linking a freshly-rebuilt `hew` against old object code and
        // failing the link.
        let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
        let status = Command::new(cargo)
            .current_dir(repo)
            .args(["build", "--quiet", "-p", "hew-lib"])
            .status()
            .expect("spawn cargo build -p hew-lib");
        assert!(
            status.success(),
            "cargo build -p hew-lib failed: {status:?}"
        );
        assert!(
            lib.exists(),
            "libhew.a missing after build: {}",
            lib.display()
        );
    });
}

/// Compile and run a Hew snippet; return trimmed stdout.
fn run_hew_source(repo: &Path, stem: &str, source: &str) -> String {
    ensure_hew_runtime_lib(repo);
    let dir =
        std::env::temp_dir().join(format!("hew-generator-exec-{}-{stem}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");

    let mut cmd = hew_command(repo);
    cmd.arg("run").arg(&path);
    let output = hew_testutil::run_command_bounded(
        &mut cmd,
        format!("hew run {}", path.display()),
        Duration::from_secs(20),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(
        output.status.success(),
        "hew run {} exited non-zero (status={:?}); stderr:\n{}",
        path.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout)
        .expect("stdout is utf-8")
        .trim()
        .to_string()
}

/// `gen { yield 7; 0 }; g.next()` returns `Some(7)`. The deepest construction
/// + single-consume seam.
#[test]
fn gen_block_next_yields_some_value() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "gen_block_next",
        r"fn main() {
    let g = gen { yield 7; 0 };
    match g.next() {
        Some(v) => println(v),
        None => println(-1),
    }
}
",
    );
    assert_eq!(
        stdout, "7",
        "gen block .next(): expected '7', got {stdout:?}"
    );
}

/// A `gen fn`'s first `.next()` yields the first value. Exercises Slice 0's
/// HIR threading composed with the construction + consumption seam.
#[test]
fn gen_fn_first_next_yields_first_value() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "gen_fn_next",
        r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

fn main() {
    let g = count();
    match g.next() {
        Some(v) => println(v),
        None => println(-1),
    }
}
",
    );
    assert_eq!(stdout, "1", "gen fn .next(): expected '1', got {stdout:?}");
}

/// Repeated `.next()` calls advance generator state and surface `None` at
/// exhaustion. Drives the generator to completion so `hew_gen_free` runs on a
/// fully-consumed generator (drop on the exhausted handle must not double-free).
#[test]
fn gen_repeated_next_advances_then_none() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "gen_repeated_next",
        r"fn main() {
    let g = gen { yield 1; yield 2; 0 };
    let a = match g.next() { Some(v) => v, None => -1 };
    let b = match g.next() { Some(v) => v, None => -1 };
    let c = match g.next() { Some(v) => v, None => -9 };
    println(a);
    println(b);
    println(c);
}
",
    );
    assert_eq!(
        stdout, "1\n2\n-9",
        "repeated .next(): expected '1\\n2\\n-9' (third call exhausted → None), got {stdout:?}"
    );
}

/// The ratified proof-of-life: `for n in count()` over a `gen fn` sums the
/// yielded values. Exercises every slice composed — Slice 0 (`gen fn` HIR
/// threading), Slice 1 (construction), Slice 2 (`.next()` consumption +
/// free-on-loop-exit), Slice 3 (`for`-in desugar to a `.next()` loop).
#[test]
fn for_in_generator_sums_yielded_values() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "gen_for_sum",
        r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

fn main() {
    var total = 0;
    for n in count() {
        total = total + n;
    }
    println(total);
}
",
    );
    assert_eq!(
        stdout, "6",
        "for-in generator sum: expected '6' (1+2+3), got {stdout:?}"
    );
}

/// `for x in gen { ... }` over a block-expression generator (no `gen fn`).
/// Confirms the desugar handles the inline gen-block iterable too.
#[test]
fn for_in_gen_block_sums_yielded_values() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "gen_for_block_sum",
        r"fn main() {
    var total = 0;
    for n in gen { yield 4; yield 5; 0 } {
        total = total + n;
    }
    println(total);
}
",
    );
    assert_eq!(
        stdout, "9",
        "for-in gen-block sum: expected '9' (4+5), got {stdout:?}"
    );
}

/// GROUNDING PROBE (plan Risk 4): a local live ACROSS a yield. `k = 10` is
/// defined before `yield 1` and read by `yield k` after the suspension. If the
/// thread model preserves the stack frame across the yield, the second
/// `.next()` returns `Some(10)`. A wrong value here would mean the
/// state-machine switch-prologue (Slice 4) is required for cross-yield local
/// liveness — it is not.
#[test]
fn gen_cross_yield_live_local_is_preserved() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "gen_cross_yield",
        r"fn main() {
    let g = gen { let k = 10; yield 1; yield k; };
    let _first = g.next();
    match g.next() {
        Some(v) => println(v),
        None => println(-1),
    }
}
",
    );
    assert_eq!(
        stdout, "10",
        "cross-yield-live local: expected '10' (thread model preserves the \
         frame across the yield), got {stdout:?}"
    );
}

// ---------------------------------------------------------------------------
// Generator resource-leak regression suite (W3.053 leak fixes).
//
// The portable exactly-once ORACLE is the dump-mir drop count (the same oracle
// the W5.021 returned-aggregate tests use): `leaks --atExit` cannot see a live
// blocked generator thread's still-reachable context, and exit-success does not
// prove no-double-free, so the count of release symbols in the elaborated MIR
// is the authoritative assertion. Each is paired with a `hew run` value-oracle
// as the runtime negative-control (a wrong drop count usually SIGSEGVs or hangs
// the run). The `leaks --atExit` evidence is recorded in the lane report.
// ---------------------------------------------------------------------------

/// Compile `source` with `--dump-mir <stage>` and return the dump.
///
/// Inline `Instr::Drop`s (the consumer-side yielded-value release and the
/// per-scope-exit `hew_gen_free`) live in the instruction stream, dumped by the
/// `checked` stage. The composite member drops (the tuple's `TupleInPlace` and
/// its `hew_gen_free` thunk) live in the `drop_plans`, dumped by `elab`. Each
/// test picks the stage that carries its release symbol.
fn dump_mir(repo: &Path, stem: &str, stage: &str, source: &str) -> String {
    let dir = std::env::temp_dir().join(format!("hew-gen-leak-{}-{stem}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");

    let mut cmd = hew_command(repo);
    cmd.arg("compile").arg("--dump-mir").arg(stage).arg(&path);
    let output = hew_testutil::run_command_bounded(
        &mut cmd,
        format!("hew compile --dump-mir {stage} {}", path.display()),
        Duration::from_secs(20),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(
        output.status.success(),
        "dump-mir {stage} {} exited non-zero (status={:?}); stderr:\n{}",
        path.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).expect("stdout is utf-8")
}

/// Compile `source` to native artefacts and return the emitted LLVM IR text
/// (`<stem>.ll`). Used to assert codegen-synthesized drop thunks (e.g. the
/// tuple's `__hew_tuple_drop_inplace_*` body, which is emitted in LLVM IR, not
/// the MIR dump).
fn emit_llvm_ir(repo: &Path, stem: &str, source: &str) -> String {
    ensure_hew_runtime_lib(repo);
    let dir = std::env::temp_dir().join(format!("hew-gen-leak-ir-{}-{stem}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");

    let mut cmd = hew_command(repo);
    cmd.arg("compile").arg("--emit-dir").arg(&dir).arg(&path);
    let output = hew_testutil::run_command_bounded(
        &mut cmd,
        format!("hew compile --emit-dir {}", dir.display()),
        Duration::from_secs(60),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(
        output.status.success(),
        "hew compile {} exited non-zero (status={:?}); stderr:\n{}",
        path.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let ll = dir.join(format!("{stem}.ll"));
    std::fs::read_to_string(&ll).unwrap_or_else(|e| panic!("read emitted IR {}: {e}", ll.display()))
}

/// Count direct `call void @<symbol>(` sites for a release symbol in the
/// `main` function body of emitted LLVM IR. The duplicate-free regression
/// (Defect 1) is invisible to a presence oracle (`contains`) because the
/// second free is masked by a runtime null-guard — only the COUNT of emitted
/// call sites distinguishes exactly-once from a double-free. Scoped to `@main`
/// so the per-symbol release helper thunks (e.g. the tuple drop thunk's own
/// single call) are counted via `count_release_calls_in_module` instead.
fn count_main_release_calls(ir: &str, symbol: &str) -> usize {
    let needle = format!("call void @{symbol}(");
    let mut in_main = false;
    let mut count = 0;
    for line in ir.lines() {
        if line.starts_with("define ") && line.contains("@main(") {
            in_main = true;
            continue;
        }
        if in_main {
            if line == "}" {
                break;
            }
            if line.contains(&needle) {
                count += 1;
            }
        }
    }
    count
}

/// Count direct `call void @<symbol>(` sites across the WHOLE emitted module.
/// Used where the release fires through a drop thunk (the generator-in-tuple
/// member drop): the module-wide count must be exactly one (the thunk's single
/// recursing call) with no second direct `main`-body call aliasing the same
/// handle.
fn count_release_calls_in_module(ir: &str, symbol: &str) -> usize {
    let needle = format!("call void @{symbol}(");
    ir.lines().filter(|line| line.contains(&needle)).count()
}

/// Leak 1 (consumer side): `for v in lists()` over a `Vec<i64>` yield must
/// release each yielded Vec in the loop body — exactly one `hew_vec_free` and
/// (since the generator is consumed to completion + dropped on loop exit) one
/// `hew_gen_free`. Before the fix there was no `hew_vec_free` and the yielded
/// buffers leaked (verified: 2 leaks → 0 under `leaks --atExit`).
#[test]
fn for_in_vec_yield_releases_each_yielded_vec() {
    let repo = repo_root();
    let source = r"gen fn lists() -> Vec<i64> { yield [1, 2, 3]; yield [4, 5] }

fn main() {
    var total = 0;
    for v in lists() {
        total = total + v.len();
    }
    println(total);
}
";
    let dump = dump_mir(&repo, "leak1_vec", "checked", source);
    assert!(
        dump.contains("hew_vec_free"),
        "yielded Vec must be released with hew_vec_free in the consuming body; \
         none found in MIR:\n{dump}"
    );
    assert!(
        dump.contains("hew_gen_free"),
        "the generator handle must still be released with hew_gen_free:\n{dump}"
    );
    // Runtime negative-control: 3 + 2 element lengths summed.
    let stdout = run_hew_source(&repo, "leak1_vec_run", source);
    assert_eq!(
        stdout, "5",
        "for-in Vec yield sum: expected '5', got {stdout:?}"
    );
}

/// Leak 1 (consumer side): a heap-BUILT string yield (`"a" + "b"`, not a static
/// literal) must release each yielded string with `hew_string_drop` in the
/// consuming body (verified leak-clean under `leaks --atExit`).
#[test]
fn for_in_string_yield_releases_each_yielded_string() {
    let repo = repo_root();
    let source = r#"gen fn rows() -> string {
    var i = 0;
    while i < 3 {
        yield "row-" + "data";
        i = i + 1;
    }
}

fn main() {
    var count = 0;
    for s in rows() {
        count = count + s.len();
    }
    println(count);
}
"#;
    let dump = dump_mir(&repo, "leak1_str", "checked", source);
    assert!(
        dump.contains("hew_string_drop"),
        "yielded string must be released with hew_string_drop in the consuming \
         body; none found in MIR:\n{dump}"
    );
    let stdout = run_hew_source(&repo, "leak1_str_run", source);
    assert_eq!(
        stdout, "24",
        "for-in string yield: 3 rows of len 8, expected '24', got {stdout:?}"
    );
}

/// Leak 2: a generator embedded in a returned tuple must be freed by the
/// caller's tuple member-drop. `ty_contains_heap_owning` now classifies a
/// `(Generator<i64,()>, i64)` as heap-owning, so the caller emits a
/// `TupleInPlace` drop whose thunk recurses to `hew_gen_free` (verified
/// leak-clean for the otherwise value-class-dependent i64 case).
#[test]
fn generator_in_returned_tuple_is_freed_by_caller() {
    let repo = repo_root();
    let source = r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

fn make() -> (Generator<i64, ()>, i64) {
    (count(), 7)
}

fn main() {
    let pair = make();
    println(pair.1);
}
";
    // The MIR proof: the tuple earns a `TupleInPlace` member-drop (before the
    // fix it was classified non-heap-owning and got none).
    let dump = dump_mir(&repo, "leak2_tuple", "elab", source);
    assert!(
        dump.contains("TupleInPlace"),
        "the (Generator, i64) tuple must earn a TupleInPlace member-drop:\n{dump}"
    );
    // The codegen proof: the synthesized `__hew_tuple_drop_inplace_*` thunk
    // recurses to `hew_gen_free` for the generator member (the thunk body is
    // emitted in LLVM IR, not the MIR dump). COUNT, not presence: the generator
    // is read only by `.1`, never extracted, so the tuple thunk's single call is
    // the ONLY release — a duplicate (the Defect 1 double-free) would show as a
    // second call site that the runtime null-guard would mask at runtime.
    let ir = emit_llvm_ir(&repo, "leak2_tuple_ir", source);
    assert!(
        ir.contains("__hew_tuple_drop_inplace_"),
        "the (Generator, i64) tuple must synthesize a drop-in-place thunk; IR:\n{ir}"
    );
    assert_eq!(
        count_release_calls_in_module(&ir, "hew_gen_free"),
        1,
        "the generator ctx must be freed exactly once (the tuple thunk's single \
         recursing call); a duplicate is a masked double-free; IR:\n{ir}"
    );
    assert_eq!(
        count_main_release_calls(&ir, "hew_gen_free"),
        0,
        "main must not also free the tuple's generator member directly (it is \
         freed by the tuple drop thunk); IR:\n{ir}"
    );
    let stdout = run_hew_source(&repo, "leak2_tuple_run", source);
    assert_eq!(
        stdout, "7",
        "tuple second field: expected '7', got {stdout:?}"
    );
}

/// Leak 3: `for n in count()` nested inside a `while` must free one generator
/// context per OUTER iteration, on the block-scope-exit edge — not accumulate
/// one leaked context + thread per iteration. The fix emits a per-scope-exit
/// `hew_gen_free`; verified 23 leaks → 0 under `leaks --atExit`.
#[test]
fn for_in_generator_nested_in_loop_frees_each_iteration() {
    let repo = repo_root();
    let source = r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

fn main() {
    var total = 0;
    var i = 0;
    while i < 3 {
        for n in count() {
            total = total + n;
        }
        i = i + 1;
    }
    println(total);
}
";
    let dump = dump_mir(&repo, "leak3_nested", "checked", source);
    assert!(
        dump.contains("hew_gen_free"),
        "the nested for-iter generator must be released with hew_gen_free on the \
         enclosing loop's re-entry edge:\n{dump}"
    );
    let stdout = run_hew_source(&repo, "leak3_nested_run", source);
    assert_eq!(
        stdout, "18",
        "nested for-in sum: 3 outer * (1+2+3), expected '18', got {stdout:?}"
    );
}

/// A generator created but never iterated inside a loop (`let g = count()` in a
/// `while`) must still free the context per iteration via the same
/// per-scope-exit `hew_gen_free` (verified leak-clean).
#[test]
fn unconsumed_generator_in_loop_is_freed_each_iteration() {
    let repo = repo_root();
    let source = r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

fn main() {
    var i = 0;
    while i < 5 {
        let g = count();
        i = i + 1;
    }
    println(i);
}
";
    let dump = dump_mir(&repo, "leak3_unconsumed", "checked", source);
    assert!(
        dump.contains("hew_gen_free"),
        "the created-but-unconsumed generator must be released with hew_gen_free \
         each iteration:\n{dump}"
    );
    let stdout = run_hew_source(&repo, "leak3_unconsumed_run", source);
    assert_eq!(
        stdout, "5",
        "unconsumed-generator loop: expected '5', got {stdout:?}"
    );
}

/// for-in early `break`: must run cleanly and free BOTH the generator context
/// AND the break-iteration's yielded value on the break edge. The break edge
/// gets an inline `hew_vec_free` for the iteration's yielded Vec (the value-side
/// companion to the handle release) plus the `hew_gen_free` handle release —
/// verified 2 leaks → 0 under `leaks --atExit`.
#[test]
fn for_in_generator_early_break_runs_clean() {
    let repo = repo_root();
    let source = r"gen fn lists() -> Vec<i64> { yield [1, 2, 3]; yield [4, 5]; yield [6, 7, 8, 9] }

fn main() {
    var total = 0;
    for v in lists() {
        total = total + v.len();
        if total > 2 {
            break;
        }
    }
    println(total);
}
";
    let dump = dump_mir(&repo, "leak_break", "checked", source);
    assert!(
        dump.contains("hew_gen_free"),
        "the generator must be freed on the break/exit path:\n{dump}"
    );
    assert!(
        dump.contains("hew_vec_free"),
        "the break-iteration's yielded Vec must be freed on the break edge \
         (value-side companion to the handle release):\n{dump}"
    );
    let stdout = run_hew_source(&repo, "leak_break_run", source);
    assert_eq!(stdout, "3", "early break: expected '3', got {stdout:?}");
}

/// Defect 2 (the unbounded leak): a `for v in gen()` whose body contains a
/// `continue` must NOT suppress the per-iteration body-end Vec drop. Before the
/// fix the back-edge `Goto` (added by `continue`) made the body-shape drop scan
/// return conservative `false`, suppressing the body-end `hew_vec_free` for the
/// WHOLE binding — leaking every yielded Vec (verified 50 iters → 100 leaks).
/// The body-end `hew_vec_free` must be present even with a `continue` in scope.
#[test]
fn for_in_continue_preserves_per_iteration_value_drop() {
    let repo = repo_root();
    let source = r"gen fn lists() -> Vec<i64> {
    var i = 0;
    while i < 50 {
        yield [i, i + 1, i + 2, i + 3];
        i = i + 1;
    }
}

fn main() {
    var total = 0;
    for v in lists() {
        if v.len() == 0 {
            continue;
        }
        total = total + v.len();
    }
    println(total);
}
";
    let dump = dump_mir(&repo, "defect2_continue", "checked", source);
    assert!(
        dump.contains("hew_vec_free"),
        "a continue in the body must NOT suppress the per-iteration body-end \
         hew_vec_free (Defect 2 unbounded leak):\n{dump}"
    );
    let stdout = run_hew_source(&repo, "defect2_continue_run", source);
    assert_eq!(
        stdout, "200",
        "continue loop sum: 50 iters * 4 elems, expected '200', got {stdout:?}"
    );
}

/// Defect 2 break-edge value release: a `for v in gen()` with an early `break`
/// must free the break-iteration's yielded Vec on the break edge AND the
/// non-break iterations' Vecs at the body-end — the break edge and the
/// fall-through body-end are mutually exclusive CFG paths, so `@main` carries
/// exactly TWO `hew_vec_free` sites (one per path) and exactly ONE
/// `hew_gen_free` (the handle, freed once on loop exit). A double-free would
/// show as extra call sites; the value-leak (pre-fix) showed as only the
/// body-end site with the break iteration leaking.
#[test]
fn for_in_break_frees_value_on_both_paths_exactly_once_each() {
    let repo = repo_root();
    let source = r"gen fn lists() -> Vec<i64> { yield [1, 2, 3]; yield [4, 5]; yield [6, 7, 8, 9] }

fn main() {
    var total = 0;
    for v in lists() {
        total = total + v.len();
        if total > 2 {
            break;
        }
    }
    println(total);
}
";
    let ir = emit_llvm_ir(&repo, "defect2_break_ir", source);
    assert_eq!(
        count_main_release_calls(&ir, "hew_vec_free"),
        2,
        "break loop must free the yielded Vec on BOTH the break edge and the \
         body-end fall-through (mutually exclusive paths), exactly one each; \
         IR:\n{ir}"
    );
    assert_eq!(
        count_main_release_calls(&ir, "hew_gen_free"),
        1,
        "the generator handle must be freed exactly once on loop exit; IR:\n{ir}"
    );
}

/// Defect 1 (hard double-free): a generator extracted out of a live tuple via a
/// standalone `let g = pair.0` binding must free its context EXACTLY ONCE. The
/// extracted binding's standalone `hew_gen_free` is the sole owner; the tuple's
/// `TupleInPlace` member-drop MUST be excluded so it does not free the same
/// aliased ctx a second time. Count-based oracle: a presence check passes even
/// on the double-free (the runtime null-guard masks the second free), so the
/// module must carry exactly ONE `hew_gen_free` call site.
#[test]
fn generator_extracted_from_tuple_is_freed_exactly_once() {
    let repo = repo_root();
    let source = r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

fn make() -> (Generator<i64, ()>, i64) {
    let g = count();
    return (g, 0);
}

fn main() {
    let pair = make();
    let g = pair.0;
    println(pair.1);
}
";
    let ir = emit_llvm_ir(&repo, "defect1_extract_ir", source);
    assert_eq!(
        count_release_calls_in_module(&ir, "hew_gen_free"),
        1,
        "extracting g = pair.0 must free the ctx exactly once (the standalone \
         binding's drop); the tuple member-drop must be excluded; IR:\n{ir}"
    );
    let stdout = run_hew_source(&repo, "defect1_extract_run", source);
    assert_eq!(
        stdout, "0",
        "extract-from-tuple: expected '0' (pair.1), got {stdout:?}"
    );
}

/// Defect 1: a destructured `let (g, n) = make()` where `g` is never consumed
/// must also free the generator exactly once. The destructure temp's element
/// binder takes ownership; the temp tuple must be excluded from the spine.
#[test]
fn generator_destructured_from_tuple_is_freed_exactly_once() {
    let repo = repo_root();
    let source = r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

fn make() -> (Generator<i64, ()>, i64) {
    let g = count();
    return (g, 0);
}

fn main() {
    let (g, n) = make();
    println(n);
}
";
    let ir = emit_llvm_ir(&repo, "defect1_destruct_ir", source);
    assert_eq!(
        count_release_calls_in_module(&ir, "hew_gen_free"),
        1,
        "destructured g must free the ctx exactly once; IR:\n{ir}"
    );
    let stdout = run_hew_source(&repo, "defect1_destruct_run", source);
    assert_eq!(
        stdout, "0",
        "destructure-from-tuple: expected '0' (n), got {stdout:?}"
    );
}

/// Defect 1: a generator extracted from a tuple THEN consumed by for-in
/// (`let g = pair.0; for n in g`) is released by the for-iter binding's
/// per-loop-exit `hew_gen_free`; the still-live tuple's member-drop must be
/// excluded so the aliased ctx is not freed a second time.
#[test]
fn generator_extracted_then_consumed_is_freed_exactly_once() {
    let repo = repo_root();
    let source = r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

fn make() -> (Generator<i64, ()>, i64) {
    let g = count();
    return (g, 0);
}

fn main() {
    let pair = make();
    let g = pair.0;
    var total = 0;
    for n in g {
        total = total + n;
    }
    println(total);
}
";
    let ir = emit_llvm_ir(&repo, "defect1_extract_consume_ir", source);
    assert_eq!(
        count_release_calls_in_module(&ir, "hew_gen_free"),
        1,
        "extract-then-for-in must free the ctx exactly once (the for-iter \
         binding's release); the tuple member-drop must be excluded; IR:\n{ir}"
    );
    let stdout = run_hew_source(&repo, "defect1_extract_consume_run", source);
    assert_eq!(
        stdout, "6",
        "extract-then-consume sum 1+2+3: expected '6', got {stdout:?}"
    );
}

/// W3.053: a generator created LOCALLY, moved into a local tuple, and consumed
/// out of that tuple's field by for-in (`let g = count(); let packed = (g, 99);
/// for n in packed.0`). The for-in iterator binding takes ownership of the
/// generator field and releases it via its per-loop-exit `hew_gen_free`; both the
/// source `g` binding's standalone drop AND the local tuple's `TupleInPlace`
/// member-drop must be excluded so the same aliased ctx is not freed again.
/// Before the fix this emitted seven `hew_gen_free` call sites and SIGSEGV'd
/// (exit 139); after, exactly one. Count-based oracle (the runtime null-guard
/// masks extra frees, so a presence check would pass on the over-free).
#[test]
fn generator_in_local_tuple_consumed_by_for_in_is_freed_exactly_once() {
    let repo = repo_root();
    let source = r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

fn main() {
    let g = count();
    let packed = (g, 99);
    var total = 0;
    for n in packed.0 {
        total = total + n;
    }
    println(total);
}
";
    let ir = emit_llvm_ir(&repo, "w3053_local_tuple_forin_ir", source);
    assert_eq!(
        count_release_calls_in_module(&ir, "hew_gen_free"),
        1,
        "for-in over a local tuple's generator field must free the ctx exactly \
         once (the for-iter binding's release); the source binding and the tuple \
         member-drop must both be excluded; IR:\n{ir}"
    );
    let stdout = run_hew_source(&repo, "w3053_local_tuple_forin_run", source);
    assert_eq!(
        stdout, "6",
        "local-tuple for-in field sum 1+2+3: expected '6', got {stdout:?}"
    );
}

/// W3.053: the bind-then-for-in variant — the generator field is first bound to
/// a `let` (`let extracted = packed.0`) and the for-in iterates that binding
/// (`for n in extracted`). The chain handle → local tuple → field extraction →
/// rebind → for-iter binding must still resolve to a single owner: the for-iter
/// binding's release. The source `g` binding's drop is excluded.
#[test]
fn generator_bound_from_local_tuple_then_for_in_is_freed_exactly_once() {
    let repo = repo_root();
    let source = r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

fn main() {
    let g = count();
    let packed = (g, 99);
    let extracted = packed.0;
    var total = 0;
    for n in extracted {
        total = total + n;
    }
    println(total);
}
";
    let ir = emit_llvm_ir(&repo, "w3053_bind_then_forin_ir", source);
    assert_eq!(
        count_release_calls_in_module(&ir, "hew_gen_free"),
        1,
        "bind-then-for-in over a local tuple's generator field must free the ctx \
         exactly once; IR:\n{ir}"
    );
    let stdout = run_hew_source(&repo, "w3053_bind_then_forin_run", source);
    assert_eq!(
        stdout, "6",
        "bind-then-for-in field sum 1+2+3: expected '6', got {stdout:?}"
    );
}

/// W3.053: extract a generator out of a returned aggregate, RE-STORE it into a
/// second local aggregate, then consume that aggregate's field by for-in
/// (`let g = pair.0; let repacked = (g, 99); for n in repacked.0`). The value
/// flow crosses two aggregates (the returned `pair`, the local `repacked`) and a
/// rebind before reaching the for-iter consumer; the exclusion must follow the
/// handle through both so still exactly one `hew_gen_free` fires.
#[test]
fn generator_extracted_then_restored_into_local_aggregate_is_freed_exactly_once() {
    let repo = repo_root();
    let source = r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

fn make() -> (Generator<i64, ()>, i64) {
    let g = count();
    return (g, 0);
}

fn main() {
    let pair = make();
    let g = pair.0;
    let repacked = (g, 99);
    var total = 0;
    for n in repacked.0 {
        total = total + n;
    }
    println(total);
}
";
    let ir = emit_llvm_ir(&repo, "w3053_extract_then_restore_ir", source);
    assert_eq!(
        count_release_calls_in_module(&ir, "hew_gen_free"),
        1,
        "extract-then-restore-then-for-in must free the ctx exactly once across \
         both aggregates; IR:\n{ir}"
    );
    let stdout = run_hew_source(&repo, "w3053_extract_then_restore_run", source);
    assert_eq!(
        stdout, "6",
        "extract-then-restore field sum 1+2+3: expected '6', got {stdout:?}"
    );
}

/// W3.053 (negative / no-regression): a generator moved into a local tuple whose
/// generator field is NEVER extracted (`let repacked = (g, 99);
/// println(repacked.1)`) keeps the source binding's OWN standalone drop as the
/// sole free. The field-precise exclusion must NOT fire here (no extraction
/// consumer), so still exactly one `hew_gen_free` — proving the fix does not
/// over-exclude the no-consume sibling into a leak.
#[test]
fn generator_in_local_tuple_not_extracted_keeps_source_drop() {
    let repo = repo_root();
    let source = r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

fn make() -> (Generator<i64, ()>, i64) {
    let g = count();
    return (g, 0);
}

fn main() {
    let pair = make();
    let g = pair.0;
    let repacked = (g, 99);
    println(repacked.1);
}
";
    let ir = emit_llvm_ir(&repo, "w3053_no_consume_ir", source);
    assert_eq!(
        count_release_calls_in_module(&ir, "hew_gen_free"),
        1,
        "no extraction consumer: the source binding's own drop is the sole free; \
         the field-precise exclusion must NOT fire (no leak, no double-free); \
         IR:\n{ir}"
    );
    let stdout = run_hew_source(&repo, "w3053_no_consume_run", source);
    assert_eq!(
        stdout, "99",
        "no-consume: expected '99' (repacked.1), got {stdout:?}"
    );
}

/// W3.053 (record path): the local-RECORD analogue — a generator created
/// locally, placed into a local record field, and consumed by for-in over that
/// field (`let h = Holder { inner: g, tag: 7 }; for n in h.inner`). Exercises the
/// `RecordInit` / `RecordFieldLoad` arms of the consumed-member derivation (the
/// tuple cases above exercise `TupleConstruct` / `TupleFieldLoad`). Still exactly
/// one `hew_gen_free`: the for-iter binding's release.
#[test]
fn generator_in_local_record_consumed_by_for_in_is_freed_exactly_once() {
    let repo = repo_root();
    let source = r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }

type Holder {
    inner: Generator<i64, ()>;
    tag: i64;
}

fn main() {
    let g = count();
    let h = Holder { inner: g, tag: 7 };
    var total = 0;
    for n in h.inner {
        total = total + n;
    }
    println(total);
}
";
    let ir = emit_llvm_ir(&repo, "w3053_local_record_forin_ir", source);
    assert_eq!(
        count_release_calls_in_module(&ir, "hew_gen_free"),
        1,
        "for-in over a local record's generator field must free the ctx exactly \
         once; the source binding and the record member-drop must both be \
         excluded; IR:\n{ir}"
    );
    let stdout = run_hew_source(&repo, "w3053_local_record_forin_run", source);
    assert_eq!(
        stdout, "6",
        "local-record for-in field sum 1+2+3: expected '6', got {stdout:?}"
    );
}

/// Unit-yield exec: `gen { yield (); 0 }` must construct, yield unit, and
/// surface `Some(())` — exercises the empty-tuple (`()`) value lowering the
/// yield path depends on.
#[test]
fn gen_unit_yield_next_is_some() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "gen_unit_yield",
        r"fn main() {
    let g = gen { yield (); 0 };
    match g.next() {
        Some(_) => println(42),
        None => println(-1),
    }
}
",
    );
    assert_eq!(
        stdout, "42",
        "unit yield .next(): expected '42' (Some(())), got {stdout:?}"
    );
}

/// Compile a snippet expecting the W3.053 fail-closed gate to REFUSE it. Returns
/// the combined stderr. Asserts the compile exited non-zero and the diagnostic is
/// the owned-handle aggregate-extraction gate (never a crash / unrelated error).
fn compile_expect_w3053_refusal(repo: &Path, stem: &str, source: &str) -> String {
    let dir = std::env::temp_dir().join(format!("hew-w3053-refuse-{}-{stem}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");

    let mut cmd = hew_command(repo);
    cmd.arg("compile").arg("--emit-dir").arg(&dir).arg(&path);
    let output = hew_testutil::run_command_bounded(
        &mut cmd,
        format!("hew compile --emit-dir {}", dir.display()),
        Duration::from_secs(60),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(
        !output.status.success(),
        "expected W3.053 fail-closed refusal but compile SUCCEEDED for {stem} \
         (a double-free reached codegen)"
    );
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    assert!(
        stderr.contains("OwnedHandleAggregateExtractionUnsupported")
            || stderr.contains("not yet supported in v0.5"),
        "expected the owned-handle aggregate-extraction gate diagnostic for {stem}; got:\n{stderr}"
    );
    stderr
}

/// W3.053 fail-closed gate (Shape A — re-aggregation): a generator moved into a
/// tuple, the field extracted into a SECOND tuple, then consumed by for-in
/// (`let a = (g, 1); let b = (a.0, 2); for n in b.0`) is the residual double-free
/// the precise exclusion analysis cannot prove exactly-once. The compiler must
/// REFUSE it (clean compile error) rather than emit the use-after-free (exit 139
/// under MallocScribble) that tip c1f5ffd4 produced.
#[test]
fn w3053_reaggregated_generator_extraction_is_refused() {
    let repo = repo_root();
    let stderr = compile_expect_w3053_refusal(
        &repo,
        "w3053_reaggregate",
        r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }
fn main() {
    let g = count();
    let a = (g, 1);
    let b = (a.0, 2);
    for n in b.0 {
        println(n);
    }
}
",
    );
    assert!(
        stderr.contains("Generator"),
        "the diagnostic names the owned handle type; got:\n{stderr}"
    );
}

/// W3.053 fail-closed gate (nested aggregate sibling): a generator nested two
/// tuple levels deep (`((g, 1), 2)`) then extracted out is the same unprovable
/// double-free class as Shape A. Must be REFUSED, never a UAF.
#[test]
fn w3053_nested_aggregate_generator_extraction_is_refused() {
    let repo = repo_root();
    compile_expect_w3053_refusal(
        &repo,
        "w3053_nested",
        r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }
fn main() {
    let g = count();
    let nested = ((g, 1), 2);
    let inner = nested.0;
    for n in inner.0 {
        println(n);
    }
}
",
    );
}

/// W3.053 fail-closed gate (record re-aggregation sibling): a generator placed
/// into a record field, that field extracted into a SECOND record, then consumed
/// — the `RecordInit`/`RecordFieldLoad` analogue of Shape A. Must be REFUSED.
#[test]
fn w3053_reaggregated_record_generator_extraction_is_refused() {
    let repo = repo_root();
    compile_expect_w3053_refusal(
        &repo,
        "w3053_record_reaggregate",
        r"type Holder { gen_field: Generator<i64, ()>; tag: i64; }
gen fn count() -> i64 { yield 1; yield 2; yield 3 }
fn main() {
    let g = count();
    let h = Holder { gen_field: g, tag: 1 };
    let h2 = Holder { gen_field: h.gen_field, tag: 2 };
    for n in h2.gen_field {
        println(n);
    }
}
",
    );
}

/// W3.053 fail-closed gate (no-extraction double-free): a generator moved into a
/// LOCAL tuple created in the same scope whose generator field is never extracted
/// (`let g = count(); let r = (g, 99); println(r.1)`) is freed BOTH by the
/// tuple's member drop AND the source binding's drop — a pre-existing double-free
/// (verified exit 139 on tip c1f5ffd4). The gate must REFUSE it. (Distinct from
/// the SAFE `generator_in_local_tuple_not_extracted_keeps_source_drop` exec test,
/// where the generator is extracted from a RETURNED tuple first, so the member
/// drop is suppressed and the IR frees exactly once.)
#[test]
fn w3053_local_tuple_no_extraction_double_free_is_refused() {
    let repo = repo_root();
    compile_expect_w3053_refusal(
        &repo,
        "w3053_no_extract_doublefree",
        r"gen fn count() -> i64 { yield 1; yield 2; yield 3 }
fn main() {
    let g = count();
    let repacked = (g, 99);
    println(repacked.1);
}
",
    );
}
