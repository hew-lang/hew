//! One body-finalization seam, held by IDENTITY.
//!
//! HIR-to-MIR lowering finishes EIGHT different body kinds: free functions,
//! closure invoke shims, named-fn invoke shims, lambda-actor handler bodies,
//! generator bodies, fork trampolines, the synthesized task-entry adapter, and
//! the synthesized machine-`step` dispatch. Each one used to seal or construct
//! its own blocks and then run its own hand-rolled subset of the ownership
//! splice pipeline — several with a comment saying they "mirror
//! `lower_function`'s call site".
//!
//! That shape makes every new ownership pass an eight-way registration problem,
//! and a missed registration is SILENT: the body still compiles, and the absent
//! splice surfaces only as a runtime leak. The divergent-arm selection release
//! was added to `lower_function` alone, and a selection inside a closure or a
//! `gen fn` leaked a whole `Vec` per call until the ramps were routed through
//! the shared seam.
//!
//! `Builder::seal_body_blocks` is only half of finishing a body — it seals the
//! cursor and returns blocks that still owe the pipeline. `finalize_body` is
//! the one place that seals and then runs it. These tests hold the property
//! that makes "add the pass once" true: the sealing primitive is called from
//! `finalize_body` and from nothing else, and the set of functions that reach
//! the seam is exactly the eight named ramps.
//!
//! A source check is the right instrument. The property is about WHICH
//! FUNCTION a call sits in, which no signature expresses — the ramps are child
//! modules of `lower`, so Rust visibility cannot bar them — and it must fail
//! the moment a ninth ramp is written, not when a leak oracle happens to cover
//! its shape.
//!
//! The check resolves the ENCLOSING FUNCTION of each call rather than counting
//! textual occurrences: it scrubs comments and literals, then tracks `fn` items
//! against brace depth. Moving a call out of `finalize_body`, adding a second
//! caller of the sealing primitive, or bypassing the seam in a new ramp each
//! change the resolved SET, so each fails here.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

/// The sealing primitive that may be called from the seam and nowhere else.
const SEAL: &str = "seal_body_blocks";

/// The seam itself.
const SEAM: &str = "finalize_body";

/// Every body ramp that must finish through the seam. A ninth body kind is
/// fine — routing it through `finalize_body` and naming it here is the whole
/// ask. Removing a name without deleting its ramp means a ramp stopped
/// finishing through the seam.
const RAMPS: [&str; 8] = [
    "lower_closure_shim",
    "lower_function",
    "lower_gen_block",
    "lower_named_fn_invoke_shim",
    "lower_spawn_lambda_actor",
    "synthesize_fork_entry_shim",
    "synthesize_machine_step_fn",
    "synthesize_task_entry_adapter",
];

/// A resolved call site: the function whose body contains it.
#[derive(Debug)]
struct CallSite {
    file: String,
    line: usize,
    enclosing: Option<String>,
}

fn lower_sources() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/lower");
    let mut sources = Vec::new();
    let mut pending = vec![root];
    while let Some(current) = pending.pop() {
        let entries = std::fs::read_dir(&current)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", current.display()));
        for entry in entries {
            let path = entry
                .unwrap_or_else(|e| panic!("failed to read entry under {}: {e}", current.display()))
                .path();
            if path.is_dir() {
                pending.push(path);
            } else if path.extension().is_some_and(|extension| extension == "rs") {
                sources.push(path);
            }
        }
    }
    sources.sort();
    sources
}

/// Replace the contents of comments and literals with spaces, byte for byte.
///
/// Byte offsets and line numbers therefore still line up with the original
/// source, while a brace or an identifier inside a doc comment or a `panic!`
/// format string can no longer be read as code. Without this the brace-depth
/// walk desynchronises on the first `"{sites:#?}"`.
fn scrub(source: &str) -> Vec<u8> {
    let src = source.as_bytes();
    let mut out = src.to_vec();
    let mut i = 0;
    let blank = |out: &mut Vec<u8>, from: usize, to: usize| {
        for byte in &mut out[from..to] {
            if *byte != b'\n' {
                *byte = b' ';
            }
        }
    };
    while i < src.len() {
        match src[i] {
            b'/' if src.get(i + 1) == Some(&b'/') => {
                let end = src[i..]
                    .iter()
                    .position(|b| *b == b'\n')
                    .map_or(src.len(), |offset| i + offset);
                blank(&mut out, i, end);
                i = end;
            }
            b'/' if src.get(i + 1) == Some(&b'*') => {
                let mut depth = 1usize;
                let start = i;
                i += 2;
                while i < src.len() && depth > 0 {
                    if src[i] == b'/' && src.get(i + 1) == Some(&b'*') {
                        depth += 1;
                        i += 2;
                    } else if src[i] == b'*' && src.get(i + 1) == Some(&b'/') {
                        depth -= 1;
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                blank(&mut out, start, i);
            }
            b'r' if matches!(src.get(i + 1), Some(b'"' | b'#')) => {
                let mut hashes = 0usize;
                let mut cursor = i + 1;
                while src.get(cursor) == Some(&b'#') {
                    hashes += 1;
                    cursor += 1;
                }
                if src.get(cursor) == Some(&b'"') {
                    let mut close = vec![b'"'];
                    close.extend(std::iter::repeat_n(b'#', hashes));
                    let start = i;
                    cursor += 1;
                    while cursor < src.len() && !src[cursor..].starts_with(&close) {
                        cursor += 1;
                    }
                    let end = (cursor + close.len()).min(src.len());
                    blank(&mut out, start, end);
                    i = end;
                } else {
                    i += 1;
                }
            }
            b'"' => {
                let start = i;
                i += 1;
                while i < src.len() {
                    match src[i] {
                        b'\\' => i += 2,
                        b'"' => {
                            i += 1;
                            break;
                        }
                        _ => i += 1,
                    }
                }
                blank(&mut out, start, i.min(src.len()));
            }
            // A char literal — `'a` in the same position is a lifetime, which
            // is code and must survive.
            b'\'' => {
                if src.get(i + 1) == Some(&b'\\') {
                    let mut cursor = i + 2;
                    while cursor < src.len() && src[cursor] != b'\'' {
                        cursor += 1;
                    }
                    let end = (cursor + 1).min(src.len());
                    blank(&mut out, i, end);
                    i = end;
                } else if src.get(i + 2) == Some(&b'\'') {
                    blank(&mut out, i, i + 3);
                    i += 3;
                } else {
                    i += 1;
                }
            }
            _ => i += 1,
        }
    }
    out
}

/// Whether the byte at `index` starts an identifier occurrence of `name`
/// rather than sitting inside a longer identifier.
fn is_word_at(code: &[u8], index: usize, name: &str) -> bool {
    let bytes = name.as_bytes();
    if !code[index..].starts_with(bytes) {
        return false;
    }
    let before_ok = index == 0 || !is_ident_byte(code[index - 1]);
    let after = code.get(index + bytes.len()).copied();
    before_ok && after.is_none_or(|byte| !is_ident_byte(byte))
}

fn is_ident_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}

/// Resolve every CALL of `name` in the lowering sources to the function whose
/// body contains it. The `fn name(...)` declaration is not a call.
fn call_sites(name: &str) -> Vec<CallSite> {
    let mut sites = Vec::new();
    for path in lower_sources() {
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));
        let code = scrub(&source);
        // The `fn` item whose body we are inside, innermost last. `None` marks
        // a brace that is not a function body (an `impl`, a block, a struct
        // literal), which keeps the innermost NAMED entry correct.
        let mut stack: Vec<Option<String>> = Vec::new();
        let mut pending: Option<String> = None;
        // Offset of the identifier in `fn <name>` — that occurrence is the
        // declaration, not a call of itself.
        let mut declaration_at: Option<usize> = None;
        let mut line = 1usize;
        let mut index = 0usize;
        while index < code.len() {
            let byte = code[index];
            if byte == b'\n' {
                line += 1;
            }
            if is_word_at(&code, index, "fn") {
                let mut cursor = index + 2;
                while cursor < code.len() && code[cursor].is_ascii_whitespace() {
                    cursor += 1;
                }
                let start = cursor;
                while cursor < code.len() && is_ident_byte(code[cursor]) {
                    cursor += 1;
                }
                if cursor > start {
                    let ident = String::from_utf8_lossy(&code[start..cursor]).into_owned();
                    if ident == name {
                        declaration_at = Some(start);
                    }
                    pending = Some(ident);
                }
            } else if is_word_at(&code, index, name)
                && code.get(index + name.len()) == Some(&b'(')
                && declaration_at != Some(index)
            {
                sites.push(CallSite {
                    file: path_name(&path),
                    line,
                    enclosing: stack.iter().rev().find_map(Clone::clone),
                });
            }
            match byte {
                b'{' => {
                    stack.push(pending.take());
                }
                b'}' => {
                    stack.pop();
                }
                _ => {}
            }
            index += 1;
        }
    }
    sites
}

fn path_name(path: &Path) -> String {
    path.file_name()
        .and_then(|name| name.to_str())
        .unwrap_or_default()
        .to_string()
}

fn enclosing_set(sites: &[CallSite]) -> BTreeSet<String> {
    sites
        .iter()
        .map(|site| {
            site.enclosing.clone().unwrap_or_else(|| {
                panic!(
                    "call at {}:{} resolved to no enclosing function",
                    site.file, site.line
                )
            })
        })
        .collect()
}

/// The sealing primitive is called from `finalize_body` and from nothing else.
///
/// A ramp that seals its own blocks trips this immediately, with the remedy in
/// the failure text: route it through the seam so every ownership pass reaches
/// it. Unlike a positional check, moving the call OUT of `finalize_body` into a
/// helper beside it fails here too.
#[test]
fn the_sealing_primitive_is_called_only_from_inside_the_seam() {
    let sites = call_sites(SEAL);
    let callers = enclosing_set(&sites);
    let expected: BTreeSet<String> = [SEAM.to_string()].into_iter().collect();
    assert_eq!(
        callers, expected,
        "`{SEAL}` must be called from `{SEAM}` and from nothing else — the seam seals and then \
         runs the shared ownership splice pipeline, so a body sealed anywhere else looks finished \
         and silently skips every splice (the closure / generator divergent-arm leak). Resolved \
         call sites: {sites:#?}"
    );
}

/// Exactly the named ramps finish through the seam.
///
/// Set equality in both directions: a ramp that stops calling `finalize_body`
/// drops out, and a new body kind (or a stray caller) shows up. Either way the
/// author has to come here and say which it is.
#[test]
fn exactly_the_named_body_ramps_finish_through_the_seam() {
    let sites = call_sites(SEAM);
    let callers = enclosing_set(&sites);
    let expected: BTreeSet<String> = RAMPS.iter().map(|name| (*name).to_string()).collect();
    assert_eq!(
        callers, expected,
        "the functions calling `{SEAM}` must be exactly the eight body ramps. Missing names are \
         ramps that stopped finishing through the seam (their ownership splices silently vanish); \
         extra names are a new body kind that must be added to `RAMPS` deliberately. Resolved \
         call sites: {sites:#?}"
    );
}

/// The scrubber does not hide code.
///
/// Both assertions above are set comparisons, so a scrubber that blanked the
/// whole file would fail them loudly rather than silently pass. This pins the
/// narrower property the scrubber exists for: braces and identifiers inside
/// comments and literals are not read as code, and everything else survives.
#[test]
fn the_scrubber_blanks_only_comments_and_literals() {
    let source = r##"
fn outer() {
    // fn decoy() { seal_body_blocks(
    let text = "fn decoy() { seal_body_blocks(";
    let raw = r#"} } }"#;
    let ch = '}';
    seal_body_blocks(x);
}
"##;
    let code = scrub(source);
    assert_eq!(
        code.len(),
        source.len(),
        "the scrubber must preserve byte offsets"
    );
    let scrubbed = String::from_utf8_lossy(&code);
    assert_eq!(
        scrubbed.matches("seal_body_blocks").count(),
        1,
        "only the real call survives scrubbing: {scrubbed}"
    );
    assert_eq!(
        scrubbed.matches('{').count(),
        1,
        "only the real body brace survives scrubbing: {scrubbed}"
    );
    assert!(
        scrubbed.contains("fn outer()"),
        "code outside comments and literals must survive: {scrubbed}"
    );
}
