//! Obligation under-release (leak) is an ADVISORY warning, not a build error,
//! and is never silently suppressed.
//!
//! RED-FIRST regression tooth for the removed forgeable under-release
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
//! This test proves the former-wildcard user leak is no longer swallowed: a
//! user `decode` that under-releases a local `out: bytes` (the same shape and
//! local name the deleted `base64$decode`/`out`/`bytes` allowlist entry
//! covered) compiles GREEN and surfaces a compile-time `warning:` naming the
//! leak — neither a hard error nor silence.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen, strip_ansi, tempdir};

/// A self-contained user program whose `decode` builds a local `out: bytes` and
/// returns it on the tail path while two guard paths return a fresh
/// `bytes::new()`. The elaborator removes `out`'s terminal drop globally
/// (because `out` is consumed on the tail return), so the guard early-returns
/// reach a `Return` exit with `out` live and undischarged — the branch-around
/// under-release the S1 gate flags. This is the user-code analogue of the
/// former-allowlisted stdlib `base64$decode` / `out` / `bytes` triple.
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

#[test]
fn user_under_release_leak_warns_but_builds_green() {
    require_codegen();

    let dir = tempdir();
    let src = dir.path().join("user_decode_leak.hew");
    std::fs::write(&src, USER_DECODE_LEAK_SOURCE).expect("write hew source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.path().to_str().expect("emit-dir utf-8"),
            src.to_str().expect("hew src utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // 1. The advisory does NOT fail the build — a leak is not a hard error.
    assert!(
        output.status.success(),
        "an under-release leak must be advisory (warning), not a build error — \
         the build must stay green;\n{}",
        describe_output(&output)
    );

    // 2. The leak is NOT silently swallowed: the gate surfaces the exact
    //    under-release finding for the user `decode`'s `out` local. This is the
    //    finding the deleted forgeable allowlist would have suppressed.
    let names_the_leak = stderr.contains("obligation balance in `decode`")
        && stderr.contains("`out`")
        && stderr.contains("never released")
        && stderr.contains("ObligationUnderReleased");
    assert!(
        names_the_leak,
        "the user under-release leak must surface (not be swallowed) — expected \
         an obligation-balance finding naming `decode` and `out`;\n{}",
        describe_output(&output)
    );

    // 3. It is surfaced as a WARNING, not a hard error. The obligation finding
    //    line must carry the `warning:` severity; no `error:` obligation line.
    let obligation_lines: Vec<&str> = stderr
        .lines()
        .filter(|line| line.contains("obligation balance in"))
        .collect();
    assert!(
        !obligation_lines.is_empty(),
        "expected at least one rendered obligation-balance line;\n{}",
        describe_output(&output)
    );
    for line in &obligation_lines {
        assert!(
            line.trim_start().starts_with("warning:"),
            "the under-release advisory must render as `warning:`, never \
             `error:` — offending line: {line:?}\n{}",
            describe_output(&output)
        );
    }
}
