//! Obligation under-release (leak) is an ADVISORY warning, not a build error,
//! is never silently suppressed, and surfaces on EVERY output path.
//!
//! RED-FIRST regression teeth for the removed forgeable under-release
//! allowlist. Before this change the S1 obligation-balance gate hard-errored on
//! an under-release UNLESS the `(function, local, local_ty)` triple matched a
//! per-function allowlist keyed on the mangled symbol. That key was forgeable:
//! the compiler mangles a user module `base64`'s `decode` to the same
//! `base64$decode` a tracked stdlib entry used, so a genuine user leak that
//! collided with a stdlib triple (a `decode`-shaped fn leaking a local
//! `out: bytes`) was SILENTLY SUPPRESSED — no error, no warning.
//!
//! The fix removes the allowlist entirely and demotes under-release to an
//! advisory: the lite MIR tier cannot SOUNDLY suppress a leak without
//! un-forgeable defining-module provenance, so it WARNS on every under-release
//! (leaving the build green) rather than hard-gate behind a forgeable key.
//! Over-release (double-free, memory-unsafe) stays a hard error with no escape.
//!
//! These teeth pin three properties so no future allowlist can silently
//! re-suppress a leak:
//!   1. a root-unit user leak (`count_items` / owned-element `for` cursor)
//!      surfaces as a warning, build stays green;
//!   2. a `$`-mangled symbol (the generic monomorphisation `count_iter$$Item`)
//!      surfaces as a warning (proves a `$`-containing symbol is not
//!      special-cased back into suppression);
//!   3. the advisory is flushed on EVERY output path, including
//!      `--format json --dump-mir`, where an early return once dropped it.
//!
//! The fixtures originally pinned the `base64::decode` / `semver::try_parse`
//! branch-around guard-return leak (a value returned on the tail path and
//! removed from the drop plan globally, leaking on the guard early-returns).
//! That family is now fixed by path-sensitive returned-member drop re-admission,
//! so the fixtures were repointed to the remaining genuine under-release: a
//! `for _ in v.iter()` over an OWNED-element `Vec` clones a receiver snapshot
//! (`hew_vec_clone_owned`) whose owned elements the `VecIter` cursor never frees
//! — a distinct MIR-seam leak (the cursor is misclassified as a borrower, not
//! the owner of its cloned snapshot). Both fixtures still fire the advisory. (If
//! that leak is ever fixed this test fails loudly at the lift event — repoint it
//! to the next genuine under-release then.)

#![cfg(unix)]

mod support;

use std::path::Path;
use std::process::{Command, Output};

use support::{describe_output, hew_binary, repo_root, require_codegen, strip_ansi, tempdir};

/// A self-contained user program that iterates an OWNED-element `Vec<Item>` with
/// `for _ in xs.iter()`. `.iter()` clones the receiver into a `VecIter` snapshot
/// (`hew_vec_clone_owned`) deep-copying each owned `Item`; the cursor is
/// classified as a borrower of its source (the place-source `VecIter` model), so
/// its cloned snapshot's owned elements are never released — the S1 gate flags
/// the under-release on the loop-exit `Return`. The root-unit symbol is
/// `count_items` and the leaked local renders as `_0` (the discarded `for`
/// cursor element).
const USER_ITER_LEAK_SOURCE: &str = "\
type Item { name: string; n: i64; }\n\
\n\
fn count_items(xs: Vec<Item>) -> i64 {\n\
\x20   let it = xs.iter();\n\
\x20   var total = 0;\n\
\x20   for _ in it {\n\
\x20       total = total + 1;\n\
\x20   }\n\
\x20   total\n\
}\n\
\n\
fn main() {\n\
\x20   let xs: Vec<Item> = Vec::new();\n\
\x20   xs.push(Item { name: \"a\", n: 1 });\n\
\x20   print(count_items(xs));\n\
}\n";

/// A program whose GENERIC `count_iter<T>` iterates an owned-element `Vec<T>`,
/// instantiated at the owned record `Item`. The monomorphised symbol is exactly
/// `count_iter$$Item` — a `$`-containing symbol. This pins that a `$`-mangled
/// (monomorphisation-qualified) symbol is not re-suppressed by a future
/// allowlist. The same owned-element `VecIter` snapshot leak fires here.
const GENERIC_ITER_LEAK_SOURCE: &str = "\
type Item { name: string; n: i64; }\n\
\n\
fn count_iter<T>(xs: Vec<T>) -> i64 {\n\
\x20   let it = xs.iter();\n\
\x20   var total = 0;\n\
\x20   for _ in it {\n\
\x20       total = total + 1;\n\
\x20   }\n\
\x20   total\n\
}\n\
\n\
fn main() {\n\
\x20   let xs: Vec<Item> = Vec::new();\n\
\x20   xs.push(Item { name: \"a\", n: 1 });\n\
\x20   print(count_iter(xs));\n\
}\n";

fn compile(dir: &Path, source: &str, name: &str, extra_args: &[&str]) -> Output {
    let src = dir.join(format!("{name}.hew"));
    std::fs::write(&src, source).expect("write hew source");
    let src_str = src.to_str().expect("hew src utf-8").to_string();
    let emit_dir = dir.to_str().expect("emit-dir utf-8").to_string();
    let mut args = vec!["compile", "--emit-dir", &emit_dir];
    args.extend_from_slice(extra_args);
    args.push(&src_str);
    Command::new(hew_binary())
        .args(&args)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile")
}

/// Extract the `ObligationUnderReleased` entries from a JSON diagnostic array on
/// stdout, returning their severities. Fails the calling test if stdout is not a
/// parseable JSON array — the whole point of the `--format json` fix is that the
/// MIR text dump must not corrupt the JSON contract.
fn json_under_release_severities(output: &Output) -> Vec<String> {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: serde_json::Value = serde_json::from_str(&stdout).unwrap_or_else(|e| {
        panic!(
            "stdout must be a parseable JSON diagnostic array (the MIR dump must \
             not corrupt it): {e}\n{}",
            describe_output(output)
        )
    });
    value
        .as_array()
        .expect("JSON diagnostics is an array")
        .iter()
        .filter(|d| d.get("code").and_then(|c| c.as_str()) == Some("ObligationUnderReleased"))
        .map(|d| {
            d.get("severity")
                .and_then(|s| s.as_str())
                .unwrap_or("<missing>")
                .to_string()
        })
        .collect()
}

/// (1) A root-unit user leak surfaces as a warning and the build stays green.
#[test]
fn user_under_release_leak_warns_but_builds_green() {
    require_codegen();
    let dir = tempdir();
    let output = compile(dir.path(), USER_ITER_LEAK_SOURCE, "user_iter_leak", &[]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "an under-release leak must be advisory (warning), not a build error;\n{}",
        describe_output(&output)
    );
    let names_the_leak = stderr.contains("obligation balance in `count_items`")
        && stderr.contains("`_0`")
        && stderr.contains("never released")
        && stderr.contains("ObligationUnderReleased");
    assert!(
        names_the_leak,
        "the user under-release leak must surface (not be swallowed);\n{}",
        describe_output(&output)
    );
    for line in stderr
        .lines()
        .filter(|l| l.contains("obligation balance in"))
    {
        assert!(
            line.trim_start().starts_with("warning:"),
            "the under-release advisory must render as `warning:`, never `error:` \
             — offending line: {line:?}\n{}",
            describe_output(&output)
        );
    }
}

/// (2) A `$`-mangled symbol (the generic monomorphisation `count_iter$$Item`)
/// surfaces as a warning at exit 0 — a `$`-containing symbol is NOT re-suppressed.
#[test]
fn mangled_generic_iter_leak_is_not_suppressed() {
    require_codegen();
    let dir = tempdir();
    let output = compile(
        dir.path(),
        GENERIC_ITER_LEAK_SOURCE,
        "uses_generic_iter",
        &[],
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "the generic count_iter$$Item under-release is advisory — the build must \
         stay green;\n{}",
        describe_output(&output)
    );

    let mangled_lines: Vec<&str> = stderr
        .lines()
        .filter(|l| l.contains("obligation balance in `count_iter$$Item`"))
        .collect();
    assert!(
        !mangled_lines.is_empty(),
        "the `$`-mangled `count_iter$$Item` under-release must surface as a \
         diagnostic, never be suppressed;\n{}",
        describe_output(&output)
    );
    // The `$` proves this is the monomorphisation-mangled symbol, not a
    // root-unit name.
    assert!(
        mangled_lines.iter().all(|l| l.contains('$')),
        "the surfaced symbol must be the `$`-mangled `count_iter$$Item`;\n{}",
        describe_output(&output)
    );
    for line in &mangled_lines {
        assert!(
            line.trim_start().starts_with("warning:"),
            "the `count_iter$$Item` advisory must render as `warning:`, never \
             `error:` — offending line: {line:?}\n{}",
            describe_output(&output)
        );
    }
}

/// (3) MATRIX — the JSON advisory must survive on EVERY `--format json`
/// termination path, whatever the exit code. This guards the flush-completeness
/// CLASS, not one instance: each round of review found one more unflushed exit
/// (clean, then `--dump-mir`, then the unsupported `--target`). Every leaking
/// compile below — clean-exit, dump-mir early-return, and the unsupported-target
/// exit-2 — must emit a parseable JSON diagnostic array on stdout carrying the
/// under-release at `severity=warning`. A future exit path that drops the flush
/// fails here regardless of its exit code.
#[test]
fn json_advisory_survives_every_exit_path() {
    require_codegen();
    // (label, extra compile args, expected exit code).
    let cases: &[(&str, &[&str], i32)] = &[
        ("json_clean_exit", &["--format", "json"], 0),
        (
            "json_dump_mir_early_return",
            &["--format", "json", "--dump-mir", "elab"],
            0,
        ),
        // The reported round-4 hole: lowering accumulates the advisory, then the
        // unsupported --target exits 2 — the flush must still run. `hew compile`
        // supports only the native host and wasm32-unknown-unknown, so an
        // explicit foreign triple takes the unsupported-target exit.
        (
            "json_unsupported_target_exit2",
            &["--format", "json", "--target", "aarch64-apple-darwin"],
            2,
        ),
    ];

    for (label, args, expected_code) in cases {
        let dir = tempdir();
        let output = compile(dir.path(), USER_ITER_LEAK_SOURCE, label, args);

        assert_eq!(
            output.status.code(),
            Some(*expected_code),
            "[{label}] expected exit {expected_code};\n{}",
            describe_output(&output)
        );
        let severities = json_under_release_severities(&output);
        assert!(
            !severities.is_empty() && severities.iter().all(|s| s == "warning"),
            "[{label}] the under-release advisory must be flushed as a \
             severity=warning JSON entry on THIS exit path (a dropped flush here \
             is the fail-open the guard closes), got {severities:?};\n{}",
            describe_output(&output)
        );
    }
}

/// (3b) `hew check --format json` reaches the MIR obligation gate and must carry
/// the advisory on its clean-exit path too (a different command sharing the sink).
#[test]
fn check_json_carries_under_release_advisory() {
    require_codegen();
    let dir = tempdir();
    let src = dir.path().join("check_leak.hew");
    std::fs::write(&src, USER_ITER_LEAK_SOURCE).expect("write hew source");
    let output = Command::new(hew_binary())
        .args(["check", "--format", "json", src.to_str().expect("utf-8")])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        output.status.success(),
        "hew check with an advisory leak must exit 0;\n{}",
        describe_output(&output)
    );
    let severities = json_under_release_severities(&output);
    assert!(
        !severities.is_empty() && severities.iter().all(|s| s == "warning"),
        "hew check --format json must carry the under-release advisory as \
         severity=warning, got {severities:?};\n{}",
        describe_output(&output)
    );
}

/// (3c) Text `--dump-mir` alone: the warning renders to STDERR during lowering
/// and the MIR dump is the stdout payload — build stays green.
#[test]
fn text_dump_mir_still_warns_on_stderr() {
    require_codegen();
    let dir = tempdir();
    let output = compile(
        dir.path(),
        USER_ITER_LEAK_SOURCE,
        "user_iter_leak_textdump",
        &["--dump-mir", "elab"],
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(
        output.status.success(),
        "text dump-mir with an advisory leak must exit 0;\n{}",
        describe_output(&output)
    );
    assert!(
        stderr.contains("obligation balance in `count_items`")
            && stderr
                .lines()
                .filter(|l| l.contains("obligation balance in"))
                .all(|l| l.trim_start().starts_with("warning:")),
        "the advisory must render as a `warning:` on stderr in text dump mode;\n{}",
        describe_output(&output)
    );
    assert!(
        stdout.contains("return["),
        "the MIR text dump must be the stdout payload in text mode;\n{}",
        describe_output(&output)
    );
}
