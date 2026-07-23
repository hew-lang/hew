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
//!   1. a root-unit user leak (`decode` / `out: bytes`) surfaces as a warning,
//!      build stays green;
//!   2. the EXACT module-mangled `base64$decode` symbol the deleted allowlist
//!      keyed on surfaces as a warning (proves a `$`-mangled symbol is not
//!      special-cased back into suppression);
//!   3. the advisory is flushed on EVERY output path, including
//!      `--format json --dump-mir`, where an early return once dropped it.

#![cfg(unix)]

mod support;

use std::path::Path;
use std::process::{Command, Output};

use support::{describe_output, hew_binary, repo_root, require_codegen, strip_ansi, tempdir};

/// A self-contained user program whose `decode` builds a local `out: bytes` and
/// returns it on the tail path while two guard paths return a fresh
/// `bytes::new()`. The elaborator removes `out`'s terminal drop globally
/// (because `out` is consumed on the tail return), so the guard early-returns
/// reach a `Return` exit with `out` live and undischarged — the branch-around
/// under-release the S1 gate flags. This is the user-code analogue of the
/// former-allowlisted stdlib `base64$decode` / `out` / `bytes` triple; the
/// root-unit symbol here is `decode`.
const USER_DECODE_LEAK_SOURCE: &str = "\
fn decode(s: string) -> bytes {\n\
\x20   let slen = s.len();\n\
\x20   let out: bytes = bytes::new();\n\
\x20   if slen == 0 {\n\
\x20       return out;\n\
\x20   }\n\
\x20   let vals: bytes = bytes::new();\n\
\x20   for i in 0 .. slen {\n\
\x20       let v = s.slice(i, i + 1).len();\n\
\x20       if v < 0 {\n\
\x20           return bytes::new();\n\
\x20       }\n\
\x20       vals.push(v as u8);\n\
\x20   }\n\
\x20   let nvals = vals.len();\n\
\x20   var i = 0;\n\
\x20   while i + 4 <= nvals {\n\
\x20       out.push(vals[i]);\n\
\x20       i = i + 4;\n\
\x20   }\n\
\x20   let remaining = nvals - i;\n\
\x20   if remaining == 1 {\n\
\x20       return bytes::new();\n\
\x20   }\n\
\x20   out\n\
}\n\
\n\
fn main() {\n\
\x20   let b = decode(\"hello\");\n\
\x20   print(b.len());\n\
}\n";

/// A program that imports the stdlib `base64` module and calls `decode`. The
/// stdlib `base64.decode` builds a local `out: bytes` and under-releases it on
/// its guard-return paths — a tracked pre-existing leak. Its mangled symbol is
/// exactly `base64$decode`, the EXACT `$`-mangled triple the deleted allowlist
/// keyed on. This pins that a module-qualified (`$`-containing) symbol is not
/// re-suppressed by a future allowlist. (If the stdlib leak is ever fixed this
/// test fails loudly at precisely the lift event — update it then.)
const STDLIB_BASE64_LEAK_SOURCE: &str = "\
import std::encoding::base64;\n\
\n\
fn main() {\n\
\x20   let back = base64.decode(\"SGk=\");\n\
\x20   print(back.len());\n\
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
    let output = compile(dir.path(), USER_DECODE_LEAK_SOURCE, "user_decode_leak", &[]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "an under-release leak must be advisory (warning), not a build error;\n{}",
        describe_output(&output)
    );
    let names_the_leak = stderr.contains("obligation balance in `decode`")
        && stderr.contains("`out`")
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

/// (2) The EXACT module-mangled `base64$decode` symbol the deleted allowlist
/// keyed on surfaces as a warning at exit 0 — a `$`-containing (module-scoped)
/// symbol is NOT re-suppressed.
#[test]
fn module_mangled_base64_decode_leak_is_not_suppressed() {
    require_codegen();
    let dir = tempdir();
    let output = compile(dir.path(), STDLIB_BASE64_LEAK_SOURCE, "uses_base64", &[]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        output.status.success(),
        "the stdlib base64$decode under-release is advisory — the build must stay \
         green;\n{}",
        describe_output(&output)
    );

    let mangled_lines: Vec<&str> = stderr
        .lines()
        .filter(|l| l.contains("obligation balance in `base64$decode`"))
        .collect();
    assert!(
        !mangled_lines.is_empty(),
        "the exact module-mangled `base64$decode` under-release (the former \
         forgeable-allowlist triple) must surface as a diagnostic, never be \
         suppressed;\n{}",
        describe_output(&output)
    );
    // The `$` proves this is the module-mangled symbol, not a root-unit name.
    assert!(
        mangled_lines.iter().all(|l| l.contains('$')),
        "the surfaced symbol must be the `$`-mangled `base64$decode`;\n{}",
        describe_output(&output)
    );
    for line in &mangled_lines {
        assert!(
            line.trim_start().starts_with("warning:"),
            "the `base64$decode` advisory must render as `warning:`, never \
             `error:` — offending line: {line:?}\n{}",
            describe_output(&output)
        );
    }
}

/// (3) The advisory is flushed on the `--format json --dump-mir` path — the
/// early-return that once dropped it. stdout stays a parseable JSON array
/// carrying the `warning`-severity under-release; exit 0.
#[test]
fn json_dump_mir_flushes_under_release_advisory() {
    require_codegen();
    let dir = tempdir();
    let output = compile(
        dir.path(),
        USER_DECODE_LEAK_SOURCE,
        "user_decode_leak_jsondump",
        &["--format", "json", "--dump-mir", "elab"],
    );

    assert!(
        output.status.success(),
        "json + dump-mir with an advisory leak must exit 0;\n{}",
        describe_output(&output)
    );
    let severities = json_under_release_severities(&output);
    assert!(
        !severities.is_empty(),
        "the under-release advisory must be present in the JSON diagnostics even \
         with --dump-mir (it was silently dropped on this exit path before the \
         flush fix);\n{}",
        describe_output(&output)
    );
    assert!(
        severities.iter().all(|s| s == "warning"),
        "every under-release JSON diagnostic must carry severity=warning, got \
         {severities:?};\n{}",
        describe_output(&output)
    );
}

/// (3b) The plain `--format json` path (no dump) also carries the advisory as a
/// `warning`-severity entry at exit 0 — the flush-on-clean-exit contract.
#[test]
fn json_format_alone_carries_under_release_advisory() {
    require_codegen();
    let dir = tempdir();
    let output = compile(
        dir.path(),
        USER_DECODE_LEAK_SOURCE,
        "user_decode_leak_json",
        &["--format", "json"],
    );

    assert!(
        output.status.success(),
        "json-format compile with an advisory leak must exit 0;\n{}",
        describe_output(&output)
    );
    let severities = json_under_release_severities(&output);
    assert!(
        !severities.is_empty() && severities.iter().all(|s| s == "warning"),
        "plain --format json must carry the under-release advisory as \
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
        USER_DECODE_LEAK_SOURCE,
        "user_decode_leak_textdump",
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
        stderr.contains("obligation balance in `decode`")
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
