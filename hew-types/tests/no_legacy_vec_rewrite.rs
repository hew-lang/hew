//! Structural invariant: after the Vec dispatch cutover the resolver is the
//! sole admission authority for Vec method dispatch. Any
//! `record_runtime_method_call_rewrite` call re-introduced inside
//! `check_vec_method` would silently restore the legacy dual-emit shape and
//! break the `lifecycle-symmetry` (P0) invariant — keep this test load-bearing.
//!
//! The test reads `hew-types/src/check/methods.rs`, isolates
//! `check_vec_method`'s body (between its signature and the first column-4
//! closing brace that follows), and counts whitespace-tolerant matches of
//! `record_runtime_method_call_rewrite`.  Zero matches is the only passing
//! outcome.

use std::fs;
use std::path::PathBuf;

fn methods_source() -> String {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("src/check/methods.rs");
    fs::read_to_string(&path).unwrap_or_else(|err| panic!("read {}: {err}", path.display()))
}

fn extract_check_vec_method_body(source: &str) -> &str {
    // Locate the function signature.  The signature is intentionally written
    // with `pub(super) fn check_vec_method(` on a single line.
    let signature = "pub(super) fn check_vec_method(";
    let sig_idx = source
        .find(signature)
        .expect("could not locate `pub(super) fn check_vec_method(` in methods.rs");
    let after_sig = &source[sig_idx..];
    // Function bodies in this file are indented one level inside an `impl`;
    // the closing brace therefore sits at column 4 (`"    }"`).  Match the
    // first such line after the signature start as the end-of-function.
    let mut search_from = 0usize;
    while let Some(rel) = after_sig[search_from..].find("\n    }") {
        let end = search_from + rel + "\n    }".len();
        // Guard: the matched closing must terminate a line (LF or EOF).
        let tail = &after_sig[end..];
        if tail.is_empty() || tail.starts_with('\n') {
            return &after_sig[..end];
        }
        search_from = end;
    }
    panic!("could not locate end of `check_vec_method` body in methods.rs");
}

#[test]
fn check_vec_method_contains_no_legacy_rewrite_calls() {
    let source = methods_source();
    let body = extract_check_vec_method_body(&source);

    // Whitespace-tolerant scan: any occurrence of the identifier inside
    // `check_vec_method` is a regression, regardless of formatter wrapping
    // around `self.` or argument layout.  Substring search is sufficient
    // because the identifier is a single token.
    let needle = "record_runtime_method_call_rewrite";
    let hits = body.matches(needle).count();
    assert_eq!(
        hits, 0,
        "Vec dispatch must remain on the resolved-call kernel: \
         `check_vec_method` contains {hits} `{needle}` call(s); \
         the resolver is the sole admission authority for Vec method dispatch \
         since the W4.027 Stage 3 cutover."
    );
}
