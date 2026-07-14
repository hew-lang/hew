mod support;

use std::process::Command;

use support::{hew_binary, require_codegen, strip_ansi};

fn write_fixture(content: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = support::tempdir();
    let path = dir.path().join("fixture.hew");
    std::fs::write(&path, content).expect("cannot write fixture");
    (dir, path)
}

/// A supervisor child declared with named init args propagates the seeded value
/// to the actor at runtime. The oracle uses a non-zero literal (7) with an i64
/// field to avoid the unrelated i32 literal-coercion confound.
///
/// Green condition: exit 0 AND stdout contains "worker id=7".
/// A zero output (worker id=0) would mean the state template was not seeded,
/// which is the exact bug this feature fixes.
#[test]
fn supervisor_child_init_args_seed_actor_state() {
    require_codegen();

    let source = r#"actor Worker {
    let id: i64;
    receive fn report() { print("worker id="); println(id); }
}
supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w1: Worker(id: 7);
}
fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w = sup.w1;
    w.report();
    sleep(50ms);
}
"#;

    let (_dir, path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args(["run", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "supervisor child init args oracle must exit 0; stderr: {stderr}"
    );
    assert!(
        stdout.contains("worker id=7"),
        "supervisor child init args oracle must print 'worker id=7' (non-zero \
         seeded value); got stdout: {stdout}"
    );
}

/// Two i32 state fields seeded in reversed declaration order must both carry
/// the correct value at runtime.
///
/// This is the regression test for the wrong-code bug where MIR lowering
/// emitted `ChildInitArg::I64` for every integer literal regardless of the
/// declared field width.  An 8-byte i64 store into a 4-byte i32 slot clobbers
/// the adjacent field when the higher-offset field is written first.
///
/// The test exercises BOTH argument orders — natural (`a: 7, b: 99`) and
/// reversed (`b: 99, a: 7`) — to confirm neither clobbers the other.
///
/// Green condition: both runs exit 0 AND stdout contains "a=7 b=99".
#[test]
fn supervisor_child_i32_fields_reversed_arg_order() {
    require_codegen();

    let source_natural = r#"actor Worker {
    let a: i32;
    let b: i32;
    receive fn report() { print("a="); print(a); print(" b="); println(b); }
}
supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w: Worker(a: 7, b: 99);
}
fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w = sup.w;
    w.report();
    sleep(50ms);
}
"#;

    let source_reversed = r#"actor Worker {
    let a: i32;
    let b: i32;
    receive fn report() { print("a="); print(a); print(" b="); println(b); }
}
supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w: Worker(b: 99, a: 7);
}
fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w = sup.w;
    w.report();
    sleep(50ms);
}
"#;

    for (label, source) in [("natural", source_natural), ("reversed", source_reversed)] {
        let (_dir, path) = write_fixture(source);

        let output = Command::new(hew_binary())
            .args(["run", path.to_str().unwrap()])
            .output()
            .expect("hew binary must run");

        let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
        let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

        assert!(
            output.status.success(),
            "i32 two-field oracle ({label} order) must exit 0; stderr: {stderr}"
        );
        assert!(
            stdout.contains("a=7 b=99"),
            "i32 two-field oracle ({label} order) must print 'a=7 b=99'; \
             got stdout: {stdout}"
        );
    }
}

/// Two i32 fields plus an i64 and a bool, seeded in fully reversed
/// declaration order, must all carry correct values.
///
/// Exercises mixed-width struct layout: the 4-byte fields must not be
/// widened to 8-byte stores, and the i64 and bool stores must be
/// width-exact as well.
///
/// Green condition: exit 0 AND stdout contains "x=42 y=1234567890123 z=true".
#[test]
fn supervisor_child_mixed_width_fields_reversed_arg_order() {
    require_codegen();

    let source = r#"actor Worker {
    let x: i32;
    let y: i64;
    let z: bool;
    receive fn report() {
        print("x="); print(x);
        print(" y="); print(y);
        print(" z="); println(z);
    }
}
supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w: Worker(z: true, x: 42, y: 1234567890123);
}
fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w = sup.w;
    w.report();
    sleep(50ms);
}
"#;

    let (_dir, path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args(["run", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "mixed-width init-args oracle must exit 0; stderr: {stderr}"
    );
    assert!(
        stdout.contains("x=42 y=1234567890123 z=true"),
        "mixed-width init-args oracle must print 'x=42 y=1234567890123 z=true'; \
         got stdout: {stdout}"
    );
}

/// Every narrow and unsigned integer width is seeded width-exact: i8, i16,
/// u8, u16, u32, u64 fields packed in reversed declaration order must each
/// carry the correct value at runtime.
///
/// This is the regression test for NYI #4 (narrow/unsigned init-arg widths).
/// Before the fix, the MIR post-loop pass emitted `NotYetImplemented` for any
/// integer literal against a sub-i32 or unsigned field. The values are chosen
/// to use the high bit of each width (u8=200, u16=60000, u32=4000000000,
/// u64=18000000000) so a wrong-width store, or a sign-extension instead of a
/// zero-extension, would corrupt the value or clobber an adjacent field.
///
/// The actor compares each field against its expected value and prints a
/// match-count: narrow integer widths are not `Display`-printable directly, so
/// an equality oracle is the width-exact assertion (each `==` is evaluated at
/// the field's own width). `ok=6` means every field round-tripped exactly.
///
/// Green condition: exit 0 AND stdout contains "ok=6".
#[test]
fn supervisor_child_narrow_and_unsigned_widths_reversed_arg_order() {
    require_codegen();

    let source = r#"actor Worker {
    let a: i8;
    let b: i16;
    let c: u8;
    let d: u16;
    let e: u32;
    let f: u64;
    receive fn report() {
        var ok: i64 = 0;
        if a == 120 { ok = ok + 1; }
        if b == 30000 { ok = ok + 1; }
        if c == 200 { ok = ok + 1; }
        if d == 60000 { ok = ok + 1; }
        if e == 4000000000 { ok = ok + 1; }
        if f == 18000000000 { ok = ok + 1; }
        print("ok="); println(ok);
    }
}
supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w: Worker(f: 18000000000, e: 4000000000, d: 60000, c: 200, b: 30000, a: 120);
}
fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w = sup.w;
    w.report();
    sleep(50ms);
}
"#;

    let (_dir, path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args(["run", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "narrow/unsigned width init-args oracle must exit 0; stderr: {stderr}"
    );
    assert!(
        stdout.contains("ok=6"),
        "every narrow/unsigned init arg must be seeded width-exact (ok=6); \
         got stdout: {stdout}"
    );
}

/// An init-arg literal that overflows its declared narrow field is a compile
/// error, not a silent truncation: u8 cannot hold 300.
///
/// Green condition: `hew check` exits non-zero.
#[test]
fn supervisor_child_narrow_width_overflow_is_compile_error() {
    require_codegen();

    let source = r#"actor Worker {
    let a: u8;
    receive fn report() { print("a="); println(a); }
}
supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w: Worker(a: 300);
}
fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w = sup.w;
    w.report();
    sleep(50ms);
}
"#;

    let (_dir, path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args(["check", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    assert!(
        !output.status.success(),
        "an init-arg literal that overflows its declared narrow field (u8 = 300) \
         must be a compile error, not a silent truncation"
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("does not fit in `u8`"),
        "the overflow diagnostic must name the field width; stderr: {stderr}"
    );
}

/// A supervisor with a stateful child actor but no init args must fail at
/// compile time (`CodegenError::FailClosed`), not at runtime with a SIGSEGV.
///
/// This is the fail-closed gate: emitting a null `init_state` for a stateful
/// actor is forbidden at the codegen boundary.
#[test]
fn supervisor_stateful_child_without_init_args_fails_at_compile_time() {
    require_codegen();

    // Worker has a state field `id: i64` but the child declaration provides
    // no init args. This must fail at compile time, not reach the runtime.
    let source = r#"actor Worker {
    let id: i64;
    receive fn report() { print("worker id="); println(id); }
}
supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w1: Worker;
}
fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w = sup.w1;
    w.report();
    sleep(50ms);
}
"#;

    let (_dir, path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args(["check", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    // Must fail to compile — not a runtime crash.
    assert!(
        !output.status.success(),
        "a stateful child with no init args must be a compile error, not a \
         silent null spawn; check should exit non-zero"
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    // The fail-closed diagnostic should appear somewhere in stderr.
    // We check for the actor name and the failure marker — exact message
    // wording is not load-bearing for this test.
    assert!(
        stderr.contains("Worker") || stderr.contains("missing field"),
        "compile error for missing init args should mention the actor type or \
         missing field; stderr: {stderr}"
    );
}

/// A supervisor child whose actor has a declared-default field (R310) that is
/// OMITTED from the child init args must receive the declared default value at
/// runtime — not stack garbage from an unwritten alloca.
///
/// This is the primary regression test for the defaulted-field bug: the MIR
/// post-loop pass previously only emitted `ChildInitArgs` for explicitly-supplied
/// fields, leaving defaulted fields unwritten in the state template alloca.
///
/// Green condition: exit 0 AND stdout contains "a=7 b=100".
#[test]
fn supervisor_child_declared_default_fills_omitted_field() {
    require_codegen();

    let source = r#"actor Worker {
    let a: i64;
    let b: i64 = 100;
    receive fn report() { print("a="); print(a); print(" b="); println(b); }
}
supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w: Worker(a: 7);
}
fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w = sup.w;
    w.report();
    sleep(50ms);
}
"#;

    let (_dir, path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args(["run", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "default+explicit oracle must exit 0; stderr: {stderr}"
    );
    assert!(
        stdout.contains("a=7 b=100"),
        "declared default b=100 must be applied when field is omitted from child init args; \
         got stdout: {stdout}"
    );
}

/// A supervisor child declared with NO init args, where the actor declares ALL
/// fields with defaults, must receive all default values at runtime.
///
/// Green condition: exit 0 AND stdout contains "x=5 y=9".
#[test]
fn supervisor_child_all_declared_defaults_no_explicit_args() {
    require_codegen();

    let source = r#"actor Worker {
    let x: i64 = 5;
    let y: i64 = 9;
    receive fn report() { print("x="); print(x); print(" y="); println(y); }
}
supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w: Worker;
}
fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w = sup.w;
    w.report();
    sleep(50ms);
}
"#;

    let (_dir, path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args(["run", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "all-defaults oracle must exit 0; stderr: {stderr}"
    );
    assert!(
        stdout.contains("x=5 y=9"),
        "all declared defaults must be applied when no explicit init args are supplied; \
         got stdout: {stdout}"
    );
}

/// An explicit init arg that matches the name of a defaulted field must WIN over
/// the declared default — the explicit value is the final value at runtime.
///
/// Green condition: exit 0 AND stdout contains "a=1 b=50".
/// (a=1 from the declared default; b=50 from the explicit override of b=100)
#[test]
fn supervisor_child_explicit_arg_overrides_declared_default() {
    require_codegen();

    let source = r#"actor Worker {
    let a: i64 = 1;
    let b: i64 = 100;
    receive fn report() { print("a="); print(a); print(" b="); println(b); }
}
supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w: Worker(b: 50);
}
fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w = sup.w;
    w.report();
    sleep(50ms);
}
"#;

    let (_dir, path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args(["run", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "explicit-overrides-default oracle must exit 0; stderr: {stderr}"
    );
    assert!(
        stdout.contains("a=1 b=50"),
        "explicit arg must override declared default (expected a=1 b=50); \
         got stdout: {stdout}"
    );
}

/// A non-defaulted field that is omitted from child init args must be a
/// compile-time error — not runtime garbage or a silent zero.
///
/// This confirms the fail-closed gate still operates correctly when only SOME
/// fields have defaults; an un-supplied field with no default must diagnose.
///
/// Green condition: `hew check` exits non-zero AND stderr mentions the actor
/// or missing field.
#[test]
fn supervisor_child_required_field_omitted_is_compile_error() {
    require_codegen();

    // `a` has no default and is not supplied — must be a compile error.
    let source = r#"actor Worker {
    let a: i64;
    let b: i64 = 100;
    receive fn report() { print("a="); print(a); print(" b="); println(b); }
}
supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w: Worker;
}
fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w = sup.w;
    w.report();
    sleep(50ms);
}
"#;

    let (_dir, path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args(["check", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    assert!(
        !output.status.success(),
        "omitting a required (non-defaulted) field must be a compile error, not garbage; \
         check should exit non-zero"
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("Worker") || stderr.contains("missing") || stderr.contains("required"),
        "compile error for missing required field should mention actor, 'missing', or \
         'required'; stderr: {stderr}"
    );
}

/// An i32 state field WITH a declared default (exercises both width-correctness
/// and default-filling together).
///
/// Green condition: exit 0 AND stdout contains "a=42 b=99".
/// (a=42 from the declared i32 default; b=99 from the explicit i32 arg)
#[test]
fn supervisor_child_i32_field_with_declared_default() {
    require_codegen();

    let source = r#"actor Worker {
    let a: i32 = 42;
    let b: i32;
    receive fn report() { print("a="); print(a); print(" b="); println(b); }
}
supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w: Worker(b: 99);
}
fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w = sup.w;
    w.report();
    sleep(50ms);
}
"#;

    let (_dir, path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args(["run", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "i32 default+explicit oracle must exit 0; stderr: {stderr}"
    );
    assert!(
        stdout.contains("a=42 b=99"),
        "i32 declared default must be applied at correct width (a=42 b=99); \
         got stdout: {stdout}"
    );
}

/// `supervisor_stop` on a supervisor with an arg-initialized stateful child
/// must tear down cleanly. Regression pin for the state-drop double free:
/// the synthesized `__hew_state_drop_<Actor>` callback freed the state
/// wrapper itself, but every runtime consumer (`free_actor_resources_with_options`,
/// `InternalChildSpec::drop`) frees the wrapper AFTER invoking the callback —
/// the stop path crashed with SIGTRAP (`pointer being freed was not
/// allocated`) after printing correct output.
///
/// Green condition: exit 0 AND the post-stop marker reaches stdout. The
/// poisoned-allocator triple makes a survived double free deterministic on
/// macOS instead of silent; the env vars are inert elsewhere.
#[test]
fn supervisor_stop_with_stateful_child_exits_cleanly() {
    require_codegen();

    let source = r#"actor Counter {
    var count: i64 = 0;
    receive fn increment() {
        count = count + 1;
        println(f"Count: {count}");
    }
}
supervisor CounterGroup {
    strategy: one_for_one;
    intensity: 5 within 60s;
    child c1: Counter(count: 0) restart: permanent;
    child c2: Counter(count: 0) restart: permanent;
}
fn main() {
    let sup = spawn CounterGroup;
    sup.c1.increment();
    sup.c2.increment();
    sleep(50ms);
    supervisor_stop(sup);
    println("Stopped");
}
"#;

    let (_dir, path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args(["run", path.to_str().unwrap()])
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("hew binary must run");

    let stdout = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "supervisor_stop with stateful children must exit 0 (state-drop \
         double-free regression); stderr: {stderr}"
    );
    assert!(
        stdout.contains("Stopped"),
        "post-stop marker must reach stdout (process must survive teardown); \
         got stdout: {stdout}"
    );
}

/// A supervisor child's `MissingActorSpawnArgument` diagnostic must carry the
/// real source location of the *child declaration* — never a sentinel
/// `SiteId(0)` and never a numeric collision with an unrelated site. Before
/// this fix, supervisor children were never registered in the verifier's
/// site-span table, so the diagnostic either rendered with no location or
/// (worse) aliased whatever site happened to claim the same numeric ID —
/// observed live pointing at an unrelated `receive fn`'s parameter.
///
/// The fixture places the child declaration on a distinct line from the
/// unrelated actor receive handler's parameter (`x`) so a collision would be
/// unambiguous: `--format=json`'s `span.start_line` must match the child
/// declaration's line, not the receive handler's.
#[test]
fn supervisor_missing_field_diagnostic_points_at_child_declaration() {
    require_codegen();

    let source = r"actor Worker {
    let id: i64;
    receive fn work(x: i64) -> i64 { x }
}

supervisor Pool {
    strategy: one_for_one;
    intensity: 3 within 60s;
    child w1: Worker;
}
";

    let (_dir, path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args(["check", "--format=json", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    assert!(
        !output.status.success(),
        "a stateful child with no init args must be a compile error"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let diagnostics: serde_json::Value = serde_json::from_str(&stdout).unwrap_or_else(|error| {
        panic!("stdout must be a parseable JSON array; parse error: {error}\nstdout:\n{stdout}")
    });
    let diagnostics = diagnostics
        .as_array()
        .unwrap_or_else(|| panic!("top-level JSON must be an array; got:\n{stdout}"));

    let missing = diagnostics
        .iter()
        .find(|d| d["code"] == "MissingActorSpawnArgument")
        .unwrap_or_else(|| {
            panic!("expected a MissingActorSpawnArgument diagnostic; got: {diagnostics:#?}")
        });

    // `child w1: Worker;` is on source line 9 (1-based). The unrelated
    // `receive fn work(x: i64)` parameter that a SiteId collision previously
    // rendered a caret on lives on line 3 — asserting an exact match (not
    // merely "not line 3") pins the fix to the real declaration rather than
    // any other incidentally-different line.
    assert_eq!(
        missing["span"]["start_line"], 9,
        "MissingActorSpawnArgument must carry the child declaration's real \
         source line, not a sentinel or an unrelated colliding site; \
         diagnostic: {missing:#?}"
    );
}
