//! End-to-end coverage for the compiler lint pass surfaced through `hew check`:
//! lints render as warnings by default, the `--allow` / `--warn` / `--deny`
//! flags re-level them, an in-source `// hew:allow(...)` directive suppresses
//! them, and an unknown lint name fails closed at the CLI boundary. Covers the
//! `needless_range_loop` and `len_zero_comparison` checker lints plus the
//! `dead_code` warning now routed through the same registry.

mod support;

use std::process::{Command, Output};

use support::{hew_binary, repo_root, strip_ansi, tempdir};

/// A program whose only diagnostic is the `needless_range_loop` lint: the loop
/// indexes `xs` with `i` and does nothing else with `i`, so iterating the
/// collection directly is exactly equivalent.
const NEEDLESS: &str = "fn main() {\n\
     let xs: Vec<i64> = Vec::new();\n\
     for i in 0..xs.len() {\n\
     let _ = xs[i];\n\
     }\n\
     }\n";

/// The same program with an in-source allow directive on the line above.
const NEEDLESS_SUPPRESSED: &str = "fn main() {\n\
     let xs: Vec<i64> = Vec::new();\n\
     // hew:allow(needless_range_loop)\n\
     for i in 0..xs.len() {\n\
     let _ = xs[i];\n\
     }\n\
     }\n";

const LINT_MESSAGE: &str = "the loop variable `i` is only used to index `xs`";

/// Run `hew check <fixture>` with `source` written to a temp file plus any
/// extra args (e.g. `--deny needless_range_loop`) inserted before the path.
fn run_check(source: &str, extra_args: &[&str]) -> Output {
    let dir = tempdir();
    let path = dir.path().join("prog.hew");
    std::fs::write(&path, source).unwrap();

    let mut command = Command::new(hew_binary());
    command.arg("check");
    command.args(extra_args);
    command.arg(&path);
    command
        .current_dir(repo_root())
        .output()
        .expect("failed to spawn hew check")
}

fn stderr_of(output: &Output) -> String {
    strip_ansi(&String::from_utf8_lossy(&output.stderr))
}

#[test]
fn lint_warning_renders_by_default() {
    let output = run_check(NEEDLESS, &[]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "a lint warning must not fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("warning:") && stderr.contains(LINT_MESSAGE),
        "expected the needless_range_loop warning to render:\n{stderr}"
    );
}

#[test]
fn allow_flag_suppresses_lint() {
    let output = run_check(NEEDLESS, &["--allow", "needless_range_loop"]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(LINT_MESSAGE),
        "--allow must suppress the lint:\n{stderr}"
    );
}

#[test]
fn allow_all_wildcard_suppresses_lint() {
    let output = run_check(NEEDLESS, &["-A", "all"]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(LINT_MESSAGE),
        "`-A all` must suppress every lint:\n{stderr}"
    );
}

#[test]
fn warn_flag_keeps_lint_as_warning() {
    let output = run_check(NEEDLESS, &["--warn", "needless_range_loop"]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "a warning must not fail:\n{stderr}"
    );
    assert!(
        stderr.contains("warning:") && stderr.contains(LINT_MESSAGE),
        "--warn must keep the lint at warning severity:\n{stderr}"
    );
}

#[test]
fn deny_flag_promotes_lint_to_error() {
    let output = run_check(NEEDLESS, &["--deny", "needless_range_loop"]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "--deny must fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("error:") && stderr.contains(LINT_MESSAGE),
        "--deny must render the lint as an error:\n{stderr}"
    );
}

#[test]
fn inline_directive_suppresses_lint() {
    let output = run_check(NEEDLESS_SUPPRESSED, &[]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(LINT_MESSAGE),
        "an in-source `// hew:allow(...)` directive must suppress the lint:\n{stderr}"
    );
}

#[test]
fn inline_directive_overrides_deny() {
    // A local allow wins even over a command-line --deny: the lint is dropped,
    // so the build still succeeds.
    let output = run_check(NEEDLESS_SUPPRESSED, &["--deny", "needless_range_loop"]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "an in-source allow must override --deny:\n{stderr}"
    );
    assert!(
        !stderr.contains(LINT_MESSAGE),
        "the suppressed lint must not surface under --deny:\n{stderr}"
    );
}

#[test]
fn unknown_lint_name_is_rejected() {
    let output = run_check(NEEDLESS, &["--allow", "no_such_lint"]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "an unknown lint name must fail closed:\n{stderr}"
    );
    assert!(
        stderr.contains("unknown lint `no_such_lint`"),
        "expected a clear unknown-lint error:\n{stderr}"
    );
}

// ── checker-stage lint: len_zero_comparison ──────────────────────────

/// A program whose only diagnostic is `len_zero_comparison`: `xs.len() == 0`
/// is exactly `xs.is_empty()`.
const LEN_ZERO: &str = "fn main() {\n\
     let xs: Vec<i64> = Vec::new();\n\
     let _ = xs.len() == 0;\n\
     }\n";

/// The same program with an in-source allow directive on the line above.
const LEN_ZERO_SUPPRESSED: &str = "fn main() {\n\
     let xs: Vec<i64> = Vec::new();\n\
     // hew:allow(len_zero_comparison)\n\
     let _ = xs.len() == 0;\n\
     }\n";

const LEN_ZERO_MESSAGE: &str = "is exactly `is_empty()`";

#[test]
fn len_zero_comparison_renders_by_default() {
    let output = run_check(LEN_ZERO, &[]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "a lint warning must not fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("warning:") && stderr.contains(LEN_ZERO_MESSAGE),
        "expected the len_zero_comparison warning to render:\n{stderr}"
    );
}

#[test]
fn len_zero_comparison_deny_promotes_to_error() {
    let output = run_check(LEN_ZERO, &["-D", "len_zero_comparison"]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "-D must fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("error:") && stderr.contains(LEN_ZERO_MESSAGE),
        "-D must render the lint as an error:\n{stderr}"
    );
}

#[test]
fn len_zero_comparison_inline_directive_suppresses() {
    let output = run_check(LEN_ZERO_SUPPRESSED, &[]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(LEN_ZERO_MESSAGE),
        "an in-source `// hew:allow(...)` directive must suppress the lint:\n{stderr}"
    );
}

// ── migrated warning: dead_code is now registry-controlled ───────────

/// `helper` is never called, so the migrated `dead_code` lint flags it.
const DEAD_CODE: &str = "fn helper() {}\nfn main() {}\n";

const DEAD_CODE_MESSAGE: &str = "function `helper` is never called";

#[test]
fn dead_code_warns_by_default_but_is_allowable() {
    // The migration preserves the default warning while making it suppressible
    // through the same `--allow` path as every other registry lint.
    let default = run_check(DEAD_CODE, &[]);
    let default_stderr = stderr_of(&default);
    assert!(
        default_stderr.contains("warning:") && default_stderr.contains(DEAD_CODE_MESSAGE),
        "dead_code must still warn by default:\n{default_stderr}"
    );

    let allowed = run_check(DEAD_CODE, &["--allow", "dead_code"]);
    let allowed_stderr = stderr_of(&allowed);
    assert!(
        !allowed_stderr.contains(DEAD_CODE_MESSAGE),
        "--allow dead_code must suppress the migrated warning:\n{allowed_stderr}"
    );
}

#[test]
fn dead_code_deny_promotes_to_error() {
    let output = run_check(DEAD_CODE, &["-D", "dead_code"]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "-D dead_code must fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("error:") && stderr.contains(DEAD_CODE_MESSAGE),
        "-D must render the migrated lint as an error:\n{stderr}"
    );
}

/// A `re"..."` literal triggers implicit injection of `import std::text::regex`,
/// whose method bodies compare `values.len() == 0` internally. The lint sweep
/// runs over user-authored bodies only, so that standard-library comparison must
/// NOT surface as a `len_zero_comparison` warning against the user's program.
/// The literal is used through the regex API so the library's method bodies are
/// genuinely pulled into the compilation (the guard is not vacuous), and the
/// program compiles cleanly — successful `check` is the witness that the implicit
/// module resolved and lowered.
const REGEX_LITERAL: &str = "fn main() {\n\
     let r = re\"hello\";\n\
     if r.is_match(\"hello world\") {\n\
     println(\"match\");\n\
     }\n\
     }\n";

#[test]
fn stdlib_lints_do_not_leak_through_implicit_import() {
    let output = run_check(REGEX_LITERAL, &[]);
    let stderr = stderr_of(&output);
    // Sanity: the implicit `std::text::regex` import was actually resolved,
    // compiled, and lowered (otherwise this test would pass vacuously). A clean
    // `check` over a program that uses the regex API is the witness.
    assert!(
        output.status.success(),
        "the implicit std::text::regex import must resolve and lower cleanly:\n{stderr}"
    );
    // The actual guard: no lint finding from the library's own source.
    assert!(
        !stderr.contains(LEN_ZERO_MESSAGE),
        "a len_zero_comparison finding must not leak from stdlib bodies:\n{stderr}"
    );
}

// ── MIR-stage lint: dead_store ───────────────────────────────────────
//
// `dead_store` is the first lint that rides the MIR liveness pass rather than
// the HIR checker sweep, so it exercises the separate CLI surfacing seam
// (`render_pipeline_mir_lints`). It must honour the exact same registry
// controls — default warning, `-A/-W/-D`, and `// hew:allow(...)` — as every
// checker-stage lint above.

/// `x` is assigned `5`, then unconditionally overwritten by `6` before the
/// first value is ever read: the `var x = 5` store is dead.
const DEAD_STORE: &str = "fn f() -> i64 {\n\
     var x = 5;\n\
     x = 6;\n\
     x\n\
     }\n\
     fn main() {\n\
     let _ = f();\n\
     }\n";

/// The same program with an in-source allow directive on the line above the
/// dead store.
const DEAD_STORE_SUPPRESSED: &str = "fn f() -> i64 {\n\
     // hew:allow(dead_store)\n\
     var x = 5;\n\
     x = 6;\n\
     x\n\
     }\n\
     fn main() {\n\
     let _ = f();\n\
     }\n";

/// A textbook `for i in 0..n` accumulator loop: `i` and `total` are both read
/// normally, so the precision guards must keep `dead_store` silent. This is the
/// regression that proves the loop-counter / accumulator machinery does not
/// misfire.
const FOR_RANGE_CLEAN: &str = "fn sum(n: i64) -> i64 {\n\
     var total = 0;\n\
     for i in 0..n {\n\
     total = total + i;\n\
     }\n\
     total\n\
     }\n\
     fn main() {\n\
     let _ = sum(5);\n\
     }\n";

const DEAD_STORE_MESSAGE: &str = "is never read before it is overwritten";

#[test]
fn dead_store_warns_by_default() {
    let output = run_check(DEAD_STORE, &[]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "a dead_store warning must not fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("warning:") && stderr.contains(DEAD_STORE_MESSAGE),
        "expected the dead_store warning to render by default:\n{stderr}"
    );
}

#[test]
fn dead_store_deny_promotes_to_error() {
    let output = run_check(DEAD_STORE, &["-D", "dead_store"]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "-D dead_store must fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("error:") && stderr.contains(DEAD_STORE_MESSAGE),
        "-D must render the dead_store lint as an error:\n{stderr}"
    );
}

#[test]
fn dead_store_allow_suppresses() {
    let output = run_check(DEAD_STORE, &["--allow", "dead_store"]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(DEAD_STORE_MESSAGE),
        "--allow dead_store must suppress the lint:\n{stderr}"
    );
}

#[test]
fn dead_store_inline_directive_suppresses() {
    let output = run_check(DEAD_STORE_SUPPRESSED, &[]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(DEAD_STORE_MESSAGE),
        "an in-source `// hew:allow(dead_store)` directive must suppress the MIR lint:\n{stderr}"
    );
}

#[test]
fn dead_store_inline_directive_overrides_deny() {
    // The in-source allow wins even over `-D`: parity with the checker lints.
    let output = run_check(DEAD_STORE_SUPPRESSED, &["-D", "dead_store"]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "an in-source allow must override -D for a MIR lint:\n{stderr}"
    );
    assert!(
        !stderr.contains(DEAD_STORE_MESSAGE),
        "the suppressed MIR lint must not surface under -D:\n{stderr}"
    );
}

#[test]
fn for_range_loop_does_not_trip_dead_store() {
    // The canonical precision guard: a normal counting loop with a read counter
    // and a read accumulator must produce no dead_store finding, even under -D.
    let output = run_check(FOR_RANGE_CLEAN, &["-D", "dead_store"]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "`for i in 0..n` with used variables must not trip dead_store:\n{stderr}"
    );
    assert!(
        !stderr.contains(DEAD_STORE_MESSAGE),
        "dead_store must not misfire on a normal counting loop:\n{stderr}"
    );
}

#[test]
fn clean_counter_is_unregistered_and_fails_closed() {
    // `clean_counter` was deliberately NOT registered (it has no emission code
    // yet). Selecting it must fail closed at the CLI boundary as an unknown
    // lint — never silently no-op — so the registry's fail-closed contract
    // holds. Mirrors `unknown_lint_name_is_rejected`.
    let output = run_check(DEAD_STORE, &["-D", "clean_counter"]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "-D clean_counter must fail closed (unregistered lint):\n{stderr}"
    );
    assert!(
        stderr.contains("unknown lint `clean_counter`"),
        "expected a clear unknown-lint error naming clean_counter:\n{stderr}"
    );
}

// ── checker-stage lint: must_use ─────────────────────────────────────

/// A program whose only diagnostic is `must_use`: the `Result<(), WriteError>`
/// returned by `w()` is discarded in statement position, so a write error is
/// silently dropped. `main` and `w` are both reachable (no `dead_code` noise).
const MUST_USE_DISCARD: &str = "enum WriteError { Disconnected(i64); }\n\
     fn w() -> Result<(), WriteError> { Ok(()) }\n\
     fn main() {\n\
     w();\n\
     }\n";

/// The same program with an in-source allow directive on the line above.
const MUST_USE_SUPPRESSED: &str = "enum WriteError { Disconnected(i64); }\n\
     fn w() -> Result<(), WriteError> { Ok(()) }\n\
     fn main() {\n\
     // hew:allow(must_use)\n\
     w();\n\
     }\n";

const MUST_USE_MESSAGE: &str = "an ignored write/send error fails open";

#[test]
fn must_use_warning_renders_by_default() {
    let output = run_check(MUST_USE_DISCARD, &[]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "a must_use warning must not fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("warning:") && stderr.contains(MUST_USE_MESSAGE),
        "expected the must_use warning to render:\n{stderr}"
    );
}

#[test]
fn must_use_allow_flag_suppresses() {
    let output = run_check(MUST_USE_DISCARD, &["--allow", "must_use"]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(MUST_USE_MESSAGE),
        "--allow must_use must suppress the lint:\n{stderr}"
    );
}

#[test]
fn must_use_deny_flag_promotes_to_error() {
    let output = run_check(MUST_USE_DISCARD, &["--deny", "must_use"]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "--deny must_use must fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("error:") && stderr.contains(MUST_USE_MESSAGE),
        "--deny must render must_use as an error:\n{stderr}"
    );
}

#[test]
fn must_use_inline_directive_suppresses() {
    let output = run_check(MUST_USE_SUPPRESSED, &[]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(MUST_USE_MESSAGE),
        "an in-source `// hew:allow(must_use)` must suppress the lint:\n{stderr}"
    );
}
