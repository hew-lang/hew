//! End-to-end coverage for the compiler lint pass surfaced through `hew check`:
//! lints render at their default levels, the `--allow` / `--warn` / `--deny`
//! flags re-level them, an in-source `// hew:allow(...)` directive suppresses
//! them, and an unknown lint name fails closed at the CLI boundary. Covers the
//! `needless_range_loop`, `len_zero_comparison`, and comment Unicode checker
//! lints plus the `dead_code` warning now routed through the same registry.

mod support;

use std::process::{Command, Output};

use support::{hew_binary, repo_root, strip_ansi, tempdir};

/// A program whose only diagnostic is the `needless_range_loop` lint: the loop
/// indexes `xs` with `i` and does nothing else with `i`, so iterating the
/// collection directly is exactly equivalent.
const NEEDLESS: &str = "fn main() {\n\
     let xs: Vec<i64> = Vec.new();\n\
     for i in 0..xs.len() {\n\
     let _ = xs[i];\n\
     }\n\
     }\n";

/// A needless range loop followed by another statement. The later statement
/// is a negative control for the diagnostic span: it must not be rendered as
/// the source of the loop warning.
const NEEDLESS_FOLLOWED_BY_STATEMENT: &str = "fn main() {\n    let xs: Vec<i64> = Vec.new();\n    for i in 0..xs.len() {\n        let _ = xs[i];\n    }\n    println(\"unrelated after loop\");\n}\n";

/// The same program with an in-source allow directive on the line above.
const NEEDLESS_SUPPRESSED: &str = "fn main() {\n\
     let xs: Vec<i64> = Vec.new();\n\
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

/// Run `hew check` on a program importing a directory module assembled from
/// `journal.hew` and `recovery.hew`. Only the peer file contains a lint.
fn run_directory_module_check() -> Output {
    let dir = tempdir();
    let journal_dir = dir.path().join("journal");
    std::fs::create_dir(&journal_dir).unwrap();
    std::fs::write(
        dir.path().join("main.hew"),
        "import journal;\n\nfn main() {}\n",
    )
    .unwrap();
    std::fs::write(
        journal_dir.join("journal.hew"),
        "pub fn open() {\n    println(\"journal primary\");\n}\n",
    )
    .unwrap();
    std::fs::write(
        journal_dir.join("recovery.hew"),
        "pub fn recover() {\n    let names: Vec<string> = Vec.new();\n    for i in 0..names.len() {\n        let _ = names[i];\n    }\n}\n",
    )
    .unwrap();

    Command::new(hew_binary())
        .arg("check")
        .arg(dir.path().join("main.hew"))
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
fn needless_range_loop_warning_points_at_loop_header() {
    let output = run_check(NEEDLESS_FOLLOWED_BY_STATEMENT, &[]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "a lint warning must not fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("prog.hew:3:9:")
            && stderr.contains("3 |     for i in 0..xs.len() {")
            && stderr.contains("|         ^^^^^^^^^^^^^^^^^\n")
            && stderr.contains(LINT_MESSAGE),
        "the needless_range_loop warning must point at the loop header:\n{stderr}"
    );
    assert!(
        !stderr.contains("unrelated after loop"),
        "the warning must not point at the following statement:\n{stderr}"
    );
}

#[test]
fn directory_module_lint_points_at_peer_file() {
    let output = run_directory_module_check();
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "a lint warning must not fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("recovery.hew:3:9:")
            && stderr.contains("3 |     for i in 0..names.len() {")
            && stderr.contains("the loop variable `i` is only used to index `names`"),
        "the lint must render against the peer file that owns its span:\n{stderr}"
    );
    assert!(
        !stderr.contains("journal.hew:") && !stderr.contains("journal primary"),
        "the primary module file is a negative control for attribution:\n{stderr}"
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
     let xs: Vec<i64> = Vec.new();\n\
     let _ = xs.len() == 0;\n\
     }\n";

/// The same program with an in-source allow directive on the line above.
const LEN_ZERO_SUPPRESSED: &str = "fn main() {\n\
     let xs: Vec<i64> = Vec.new();\n\
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

// ── receive-handler lint: sleep_loop_blocks_mailbox ───────────────────

const SLEEP_LOOP_BLOCKS_MAILBOX: &str = "actor Worker {\n\
     var running: bool = true,\n\
     receive fn run() { while running { sleep(10ms); } }\n\
     receive fn stop() { running = false; }\n\
     }\n";

const SLEEP_LOOP_BLOCKS_MAILBOX_SUPPRESSED: &str = "actor Worker {\n\
     var running: bool = true,\n\
     receive fn run() {\n\
     // hew:allow(sleep_loop_blocks_mailbox)\n\
     while running { sleep(10ms); }\n\
     }\n\
     receive fn stop() { running = false; }\n\
     }\n";

const SLEEP_LOOP_BLOCKS_MAILBOX_MESSAGE: &str = "actor's mailbox is never observed";

#[test]
fn sleep_loop_blocks_mailbox_renders_by_default() {
    let output = run_check(SLEEP_LOOP_BLOCKS_MAILBOX, &[]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "a lint warning must not fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("warning:")
            && stderr.contains(SLEEP_LOOP_BLOCKS_MAILBOX_MESSAGE)
            && stderr.contains("#[every"),
        "expected the sleep_loop_blocks_mailbox warning and suggestion:\n{stderr}"
    );
}

#[test]
fn sleep_loop_blocks_mailbox_deny_promotes_to_error() {
    let output = run_check(
        SLEEP_LOOP_BLOCKS_MAILBOX,
        &["--deny", "sleep_loop_blocks_mailbox"],
    );
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "--deny must fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("error:") && stderr.contains(SLEEP_LOOP_BLOCKS_MAILBOX_MESSAGE),
        "--deny must render the lint as an error:\n{stderr}"
    );
}

#[test]
fn sleep_loop_blocks_mailbox_inline_directive_suppresses() {
    let output = run_check(SLEEP_LOOP_BLOCKS_MAILBOX_SUPPRESSED, &[]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(SLEEP_LOOP_BLOCKS_MAILBOX_MESSAGE),
        "an in-source `// hew:allow(...)` directive must suppress the lint:\n{stderr}"
    );
}

// ── receive-handler lint: actor_handle_builtin_shadow ─────────────────

const ACTOR_HANDLE_BUILTIN_SHADOW: &str = "actor Counter {\n\
     var count: i64,\n\
     receive fn send(n: i64) { count = count + n; }\n\
     }\n\
     fn main() {\n\
     let counter = spawn Counter(count: 0);\n\
     let _ = counter.send(1);\n\
     }\n";

const ACTOR_HANDLE_BUILTIN_SHADOW_SUPPRESSED: &str = "actor Counter {\n\
     var count: i64,\n\
     // hew:allow(actor_handle_builtin_shadow)\n\
     receive fn send(n: i64) { count = count + n; }\n\
     }\n\
     fn main() {\n\
     let counter = spawn Counter(count: 0);\n\
     let _ = counter.send(1);\n\
     }\n";

const ACTOR_HANDLE_BUILTIN_SHADOW_MESSAGE: &str =
    "`receive fn send` shadows builtin actor-handle method";

#[test]
fn actor_handle_builtin_shadow_warns_by_default() {
    let output = run_check(ACTOR_HANDLE_BUILTIN_SHADOW, &[]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "a shadow warning must not fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("warning:")
            && stderr.contains(ACTOR_HANDLE_BUILTIN_SHADOW_MESSAGE)
            && stderr.contains("generic `P: Pid` contexts use builtin `Pid.send` semantics"),
        "expected the actor_handle_builtin_shadow warning to render:\n{stderr}"
    );
}

#[test]
fn actor_handle_builtin_shadow_inline_directive_suppresses() {
    let output = run_check(ACTOR_HANDLE_BUILTIN_SHADOW_SUPPRESSED, &[]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(ACTOR_HANDLE_BUILTIN_SHADOW_MESSAGE),
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

// ── checker-stage source lint: Trojan-Source comments ──────────────────

const COMMENT_TEXT_DIRECTION: &str = "/// \u{202E}\nfn main() {}\n";
const COMMENT_TEXT_DIRECTION_SUPPRESSED: &str =
    "// hew:allow(text_direction_codepoint_in_comment)\n/// \u{202E}\nfn main() {}\n";
const COMMENT_INVISIBLE: &str = "// hidden\u{200B}gap\nfn main() {}\n";
const TEXT_DIRECTION_LINT: &str = "text_direction_codepoint_in_comment";
const INVISIBLE_LINT: &str = "invisible_codepoint_in_comment";

#[test]
fn comment_text_direction_denies_by_default() {
    let output = run_check(COMMENT_TEXT_DIRECTION, &[]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "the deny-default text-direction lint must fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("error:") && stderr.contains(TEXT_DIRECTION_LINT),
        "expected the text-direction lint to render as an error:\n{stderr}"
    );
}

#[test]
fn comment_text_direction_allow_flag_suppresses() {
    let output = run_check(COMMENT_TEXT_DIRECTION, &["--allow", TEXT_DIRECTION_LINT]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(TEXT_DIRECTION_LINT),
        "--allow must suppress the deny-default lint:\n{stderr}"
    );
}

#[test]
fn comment_text_direction_warn_flag_downgrades_to_warning() {
    let output = run_check(COMMENT_TEXT_DIRECTION, &["--warn", TEXT_DIRECTION_LINT]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "--warn must downgrade the deny-default lint:\n{stderr}"
    );
    assert!(
        stderr.contains("warning:") && stderr.contains(TEXT_DIRECTION_LINT),
        "--warn must render the text-direction lint as a warning:\n{stderr}"
    );
}

#[test]
fn comment_text_direction_inline_directive_suppresses() {
    let output = run_check(COMMENT_TEXT_DIRECTION_SUPPRESSED, &[]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(TEXT_DIRECTION_LINT),
        "an in-source `// hew:allow(...)` directive must suppress the lint:\n{stderr}"
    );
}

#[test]
fn comment_invisible_warns_by_default_and_deny_promotes() {
    let default = run_check(COMMENT_INVISIBLE, &[]);
    let default_stderr = stderr_of(&default);
    assert!(
        default.status.success(),
        "the invisible-codepoint lint is warning-tier by default:\n{default_stderr}"
    );
    assert!(
        default_stderr.contains("warning:") && default_stderr.contains(INVISIBLE_LINT),
        "expected the invisible-codepoint lint warning:\n{default_stderr}"
    );

    let denied = run_check(COMMENT_INVISIBLE, &["--deny", INVISIBLE_LINT]);
    let denied_stderr = stderr_of(&denied);
    assert!(
        !denied.status.success(),
        "--deny must promote the invisible-codepoint lint:\n{denied_stderr}"
    );
    assert!(
        denied_stderr.contains("error:") && denied_stderr.contains(INVISIBLE_LINT),
        "--deny must render the invisible-codepoint lint as an error:\n{denied_stderr}"
    );
}

// ── checker-stage lint: must_use ─────────────────────────────────────

/// A program that triggers `must_use`: `c.write(...)` returns
/// `Result<(), std.net.WriteError>` and the call is discarded in statement
/// position, so a backpressure/disconnect signal is silently dropped. The
/// lint matches `std.net.WriteError` by its exact canonical owner (a
/// same-named local enum no longer spoofs it — see `must_use.rs`), so the
/// fixture must exercise the real stdlib type, not a look-alike local one.
/// `w` is never called, so `dead_code` can fire here too; the assertions
/// below match the `must_use` message specifically rather than asserting a
/// single diagnostic.
const MUST_USE_DISCARD: &str = "import std.net.{Connection};\n\
     fn w(c: Connection) {\n\
     c.write(b\"hi\");\n\
     }\n\
     fn main() {\n\
     }\n";

/// The same program with an in-source allow directive on the line above.
const MUST_USE_SUPPRESSED: &str = "import std.net.{Connection};\n\
     fn w(c: Connection) {\n\
     // hew:allow(must_use)\n\
     c.write(b\"hi\");\n\
     }\n\
     fn main() {\n\
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

// ── checker-stage lint: must_use on a discarded `actor.msg()` result ───

/// A program whose only diagnostic is `E_SEND_RESULT_DROPPED`: `d.process(5)`
/// is discarded in statement position, dropping the `Result<i64, ActorError>`
/// the completion call returns — a silently lost timeout / full-mailbox /
/// stopped-actor signal. This is a compile error, not a lint.
const ASK_MUST_USE_DISCARD: &str = "actor Doubler {\n\
     receive fn process(n: i64) -> i64 { n * 2 }\n\
     }\n\
     fn main() {\n\
     let d = spawn Doubler;\n\
     d.process(5);\n\
     }\n";

/// The same program, but the call result is explicitly discarded with `let _`,
/// the deliberate drop the fix-it names (silent).
const ASK_MUST_USE_HANDLED: &str = "actor Doubler {\n\
     receive fn process(n: i64) -> i64 { n * 2 }\n\
     }\n\
     fn main() {\n\
     let d = spawn Doubler;\n\
     let _ = d.process(5);\n\
     }\n";

const ASK_MUST_USE_MESSAGE: &str = "E_SEND_RESULT_DROPPED";

#[test]
fn discarded_call_result_is_refused_by_default() {
    let output = run_check(ASK_MUST_USE_DISCARD, &[]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "a dropped delivery outcome must fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("error:") && stderr.contains(ASK_MUST_USE_MESSAGE),
        "expected the discarded-result refusal to render:\n{stderr}"
    );
    assert!(
        stderr.contains("ActorError"),
        "the error should name the ActorError type:\n{stderr}"
    );
    assert!(
        stderr.contains("let _ = <expr>;"),
        "the error should carry the explicit-discard fix-it:\n{stderr}"
    );
}

#[test]
fn must_use_call_result_handled_is_silent() {
    let output = run_check(ASK_MUST_USE_HANDLED, &[]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(ASK_MUST_USE_MESSAGE),
        "`let _ = …` must be accepted:\n{stderr}"
    );
}
