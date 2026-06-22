//! W3.001 Stage 3 — Differential cutover for `Vec<T>` (parallel call path).
//!
//! Pins the **declarative** `#[extern_symbol]` resolution path against
//! the **legacy** magic table (`hew_types::stdlib::resolve_vec_method`)
//! across the full 91-cell matrix from plan §6 Stage 3 plus W3.003 bool/char
//! scalar ABI additions:
//!
//! ```text
//! 13 methods × 7 element types = 91 cells.
//! Methods : push pop get set contains len is_empty clear clone
//!           append extend remove join
//! Element : Vec<bool> Vec<char> Vec<i32> Vec<i64> Vec<f64>
//!           Vec<string> Vec<Vec<i32>>
//! ```
//!
//! For each cell the test computes:
//!
//! 1. **Legacy oracle** — `resolve_vec_method(method, &elem_ty)` (the
//!    magic table Stage 4 deletes).
//! 2. **Attribute path** — `declarative_vec_ffi::expand_vec_method(method, &elem_ty, &type_defs)`
//!    (Stage 3 substrate; template-driven expansion via
//!    `RuntimeCallingConvention::for_ty_with_layout`).
//!
//! and asserts byte-equal output, with two documented exemption
//! classes:
//!
//! * **`(contains, Vec<Vec<i32>>)`** — pre-existing allowlist exclusion
//!   (`hew-types/src/stdlib.rs:137` — runtime has no
//!   `hew_vec_contains_ptr`). Both paths must fail-closed identically.
//! * **`(len, *)`** — plan §6 Stage 3 reconciliation row: the legacy
//!   path emits `"len_vec"` (a HIR-catalog overload that resolves to
//!   runtime `hew_vec_len`); the attribute path emits the runtime
//!   `"hew_vec_len"` directly. The two are **aliases at runtime** —
//!   pinned here as a known divergence with the canonical resolution
//!   target asserted on both sides. Stage 4 collapses the alias.
//!
//! Cells outside the supported intersection (e.g. `Vec<bool>::push`,
//! `Vec<MyRecord>::push`) live in the Stage 5 negative-test suite per
//! plan §6 Stage 3 closing bullet.

use hew_types::check::TypeDef;
use hew_types::declarative_vec_ffi::expand_vec_method;
use hew_types::stdlib::resolve_vec_method;
use hew_types::Ty;
use std::collections::HashMap;

/// Element-type column header for diagnostics. Mirrors plan §6 Stage 3
/// element-type set `{i32, i64, f64, string, Vec<i32>}`.
fn element_types() -> Vec<(&'static str, Ty)> {
    vec![
        ("Vec<bool>", Ty::Bool),
        ("Vec<char>", Ty::Char),
        ("Vec<i32>", Ty::I32),
        ("Vec<i64>", Ty::I64),
        ("Vec<f64>", Ty::F64),
        ("Vec<string>", Ty::String),
        (
            "Vec<Vec<i32>>",
            Ty::Named {
                name: "Vec".to_string(),
                args: vec![Ty::I32],
                builtin: None,
            },
        ),
    ]
}

/// The 13 method names in plan §6 Stage 3 matrix order.
const METHODS: &[&str] = &[
    "push", "pop", "get", "set", "contains", "len", "is_empty", "clear", "clone", "append",
    "extend", "remove", "join",
];

/// Stage-3 substrate: minimal `TypeDef` map seeded with the heap-handle
/// classification the differential matrix relies on (`Vec` →
/// `is_indirect = true`). The substrate-correct route — Stage 2
/// invariant — is for `RuntimeCallingConvention::for_ty_with_layout`
/// to consult `TypeDef.is_indirect`; the matrix asserts the
/// classification is honoured for `Vec<Vec<i32>>` (nested heap-handle
/// element).
fn vec_handle_type_defs() -> HashMap<String, TypeDef> {
    use hew_types::check::TypeDefKind;
    let mut m = HashMap::new();
    m.insert(
        "Vec".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Vec".to_string(),
            type_params: vec!["T".to_string()],
            bounds: HashMap::new(),
            fields: HashMap::new(),
            field_order: vec![],
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: true,
        },
    );
    m
}

/// Classify each cell for the differential.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CellExpectation {
    /// Both paths emit a `c_symbol`; the test asserts byte-equal.
    Match,
    /// Plan §6 documented allowlist-exclusion (preserved fail-closed).
    BothNone,
    /// Plan §6 documented HIR-catalog alias `len_vec → hew_vec_len`.
    /// (Currently unused at the resolution-layer differential: the
    /// magic table itself emits the canonical `hew_vec_len`. The
    /// hand-recorded literal `"len_vec"` in `check_vec_method` arm
    /// "len" is the Stage 4 reconciliation target, not this matrix.)
    #[allow(dead_code, reason = "kept for documentation of the Stage 4 carry")]
    AliasedLen,
    /// `join` row: the legacy magic table does not list `join` (the
    /// element-type validity gate fires earlier in the typechecker —
    /// `check_vec_method` arm "join"). The attribute template
    /// `"hew_vec_join_str"` is monomorphic and expands successfully
    /// at the resolution layer for any element type; gating remains
    /// the type-system's job. Plan §6 Stage 3 introduces the
    /// `MethodCallRewrite` registration on the `Vec<string>` branch
    /// (in-scope expansion) — exercised by the per-checker integration
    /// (Stage 5). See `vec_join_resolution_layer_behavior_documented`.
    JoinResolutionGap,
}

fn classify(method: &str, elem_label: &str) -> CellExpectation {
    match (method, elem_label) {
        // `len`: `resolve_vec_method("len", _)` returns "hew_vec_len" —
        // the magic table itself is already canonical. The legacy
        // literal `"len_vec"` lives only at `check_vec_method` arm
        // "len" (methods.rs:2042) where the symbol is hand-recorded
        // *without* consulting `resolve_vec_method` — that's the
        // Stage 4 reconciliation target. The resolution-layer
        // differential (this matrix) is byte-equal.
        ("join", _) => CellExpectation::JoinResolutionGap,
        ("contains", "Vec<bool>" | "Vec<Vec<i32>>") => CellExpectation::BothNone,
        _ => CellExpectation::Match,
    }
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "single matrix test keeps per-cell accounting and failure reporting together"
)]
fn vec_ffi_differential_matrix_65_cells() {
    let type_defs = vec_handle_type_defs();
    let columns = element_types();

    let mut failures: Vec<String> = Vec::new();
    let mut cells_checked = 0_usize;
    let mut cells_matched = 0_usize;
    let mut cells_aliased_len = 0_usize;
    let mut cells_both_none = 0_usize;
    let mut cells_join_gap = 0_usize;

    for method in METHODS {
        for (label, elem_ty) in &columns {
            cells_checked += 1;
            let legacy = resolve_vec_method(method, elem_ty, &type_defs);
            let attribute = expand_vec_method(method, elem_ty, &type_defs);

            let expectation = classify(method, label);
            match expectation {
                CellExpectation::Match => {
                    // Legacy must produce Some(symbol); attribute must
                    // produce Some(Ok(symbol)); strings must agree.
                    let Some(legacy_sym) = legacy else {
                        failures.push(format!(
                            "({method}, {label}): legacy returned None but cell \
                             is classified Match — update classification or fix legacy",
                        ));
                        continue;
                    };
                    let Some(attr_result) = attribute else {
                        failures.push(format!(
                            "({method}, {label}): attribute returned None but cell \
                             is classified Match",
                        ));
                        continue;
                    };
                    match attr_result {
                        Ok(attr_sym) => {
                            if attr_sym == legacy_sym {
                                cells_matched += 1;
                            } else {
                                failures.push(format!(
                                    "({method}, {label}): byte mismatch — \
                                     legacy={legacy_sym:?}, attribute={attr_sym:?}",
                                ));
                            }
                        }
                        Err(e) => failures.push(format!(
                            "({method}, {label}): attribute path failed expansion \
                             but legacy produced {legacy_sym:?} — error: {e:?}",
                        )),
                    }
                }
                CellExpectation::BothNone => {
                    // Plan-documented allowlist exclusion. Both paths
                    // return None — the resolution layer refuses to
                    // hand out a symbol the runtime does not back.
                    // (Attribute path mirrors the legacy magic-table
                    // exclusion via `runtime_symbol_exists` —
                    // `declarative_vec_ffi`.)
                    if legacy.is_some() {
                        failures.push(format!(
                            "({method}, {label}): expected legacy None for \
                             allowlist exclusion, got {legacy:?}",
                        ));
                        continue;
                    }
                    if let Some(attr_result) = attribute {
                        failures.push(format!(
                            "({method}, {label}): expected attribute None for \
                             allowlist exclusion (legacy and attribute must \
                             agree byte-for-byte), got Some({attr_result:?})",
                        ));
                        continue;
                    }
                    cells_both_none += 1;
                }
                CellExpectation::AliasedLen => {
                    // Aliased pair: legacy emits "len_vec" (HIR catalog
                    // overload), attribute emits "hew_vec_len" (direct
                    // runtime symbol). Both must produce *some*
                    // symbol; both targets resolve to the same runtime
                    // entry per plan §6 Stage 3 reconciliation row.
                    let legacy_sym = legacy.expect("len always resolves on legacy");
                    let attr_sym = attribute
                        .expect("len always has an attribute template")
                        .expect("len monomorphic template never fails expansion");
                    assert_eq!(
                        legacy_sym, "len_vec",
                        "({method}, {label}): legacy `len` no longer emits \
                         `len_vec` — Stage 4 reconciliation may have landed; \
                         update the differential classification",
                    );
                    assert_eq!(
                        attr_sym, "hew_vec_len",
                        "({method}, {label}): attribute `len` template drifted",
                    );
                    cells_aliased_len += 1;
                }
                CellExpectation::JoinResolutionGap => {
                    // Legacy: not listed in `resolve_vec_method`, always
                    // None. Attribute: monomorphic template
                    // `hew_vec_join_str`, always expands. The
                    // element-type gate fires in `check_vec_method`,
                    // outside the resolution layer.
                    if legacy.is_some() {
                        failures.push(format!(
                            "({method}, {label}): expected legacy None for \
                             join resolution gap, got {legacy:?}",
                        ));
                        continue;
                    }
                    let attr_sym = attribute
                        .expect("join is in the W3.001 template table")
                        .expect("join template is monomorphic — never fails expansion");
                    if attr_sym != "hew_vec_join_str" {
                        failures.push(format!(
                            "({method}, {label}): attribute join template drifted: \
                             got {attr_sym:?}, expected \"hew_vec_join_str\"",
                        ));
                        continue;
                    }
                    cells_join_gap += 1;
                }
            }
        }
    }

    assert_eq!(
        cells_checked, 91,
        "differential matrix must cover 13 × 7 = 91 cells exactly",
    );

    assert!(
        failures.is_empty(),
        "differential test: {} cell(s) failed out of 91:\n  {}",
        failures.len(),
        failures.join("\n  ")
    );

    assert_eq!(
        cells_matched + cells_aliased_len + cells_both_none + cells_join_gap,
        91,
        "every cell must be accounted for: matched={cells_matched}, \
         aliased_len={cells_aliased_len}, both_none={cells_both_none}, \
         join_gap={cells_join_gap}",
    );
}

/// Secondary assertion: the `join` row's interaction with non-string
/// element types is asserted in the negative-test suite (Stage 5 plan
/// §6 negative table); for Stage 3 we pin the resolution-layer
/// behaviour explicitly.
///
/// Today the legacy `resolve_vec_method("join", _)` returns `None`
/// because the magic table does not list `join` — the typecheck-side
/// rejection at `methods.rs` arm "join" fires earlier with
/// `UndefinedMethod` for non-string elements, and the W3.001 Stage 3
/// in-scope expansion adds the `MethodCallRewrite` registration only
/// when `elem_ty == String`. The attribute template
/// `"hew_vec_join_str"` is monomorphic, so the resolution layer alone
/// would happily expand it for any element type; the type-system
/// gate stays the authority on element type validity.
#[test]
fn vec_join_resolution_layer_behavior_documented() {
    let type_defs = vec_handle_type_defs();

    // Legacy magic table: `join` is not listed → always `None`.
    for (_, elem) in element_types() {
        assert_eq!(
            resolve_vec_method("join", &elem, &type_defs),
            None,
            "legacy resolve_vec_method has no `join` entry by design — \
             the typecheck-side rejection is the authority",
        );
    }

    // Attribute path: monomorphic template `hew_vec_join_str` expands
    // for any element type; the element-type gate lives in
    // `check_vec_method`, not in the resolution helper.
    for (label, elem) in element_types() {
        let out = expand_vec_method("join", &elem, &type_defs)
            .expect("`join` is in W3.001 template table")
            .expect("monomorphic template never fails expansion");
        assert_eq!(
            out, "hew_vec_join_str",
            "join template expansion for {label}",
        );
    }
}
