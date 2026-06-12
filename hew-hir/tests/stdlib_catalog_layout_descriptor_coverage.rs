//! W4.001 Stage C0b — stdlib catalog layout-descriptor coverage gate.
//!
//! Every scalar / string / bytes / unit type in C0a's scope must have a
//! `BuiltinLinkage::LayoutDescriptorSymbol` catalog entry naming the
//! `#[no_mangle] pub static` runtime symbol that exports its descriptor.
//! Coverage gap = test fail.
//!
//! Named-record descriptors are intentionally *not* enumerated here.
//! Per plan §4 Stage C0b ("descriptors materialized on demand via the
//! existing Named-record Layout machinery"), they are not shipped as
//! fixed statics — Stage C-1c synthesises per-record descriptors at
//! call-site materialisation time. The coverage check is therefore
//! restricted to the fixed-set primitive types.

use std::collections::BTreeSet;

use hew_hir::stdlib_catalog::{entries, BuiltinLinkage, LayoutDescriptorRole};

/// Types that must have a `hew_layout_key_<type>` catalog entry.
///
/// Float K descriptors are shipped (with `hash_fn = None` / `eq_fn = None`
/// for DI-003 belt-and-suspenders fail-closed) so they are counted here.
/// `unit` is *not* a valid K (zero-size keys are rejected by the kernel's
/// `validate_key_layout`) so it is absent.
const REQUIRED_KEY_TYPES: &[&str] = &[
    "i32", "i64", "u32", "u64", "f32", "f64", "bool", "char", "string", "bytes",
];

/// Types that must have a `hew_layout_val_<type>` catalog entry.
///
/// `unit` is included for the `HashSet<T>` = `HashMap<T, ()>` ZST pattern.
const REQUIRED_VAL_TYPES: &[&str] = &[
    "i32", "i64", "u32", "u64", "f32", "f64", "bool", "char", "string", "bytes", "unit",
];

fn collect_descriptor_symbols(role: LayoutDescriptorRole) -> BTreeSet<String> {
    entries()
        .iter()
        .filter_map(|e| match e.linkage {
            BuiltinLinkage::LayoutDescriptorSymbol {
                symbol,
                role: entry_role,
            } if entry_role == role => Some(symbol.to_string()),
            _ => None,
        })
        .collect()
}

#[test]
fn every_required_key_type_has_a_layout_descriptor_entry() {
    let actual = collect_descriptor_symbols(LayoutDescriptorRole::Key);
    let mut missing: Vec<String> = Vec::new();
    for ty in REQUIRED_KEY_TYPES {
        let expected = format!("hew_layout_key_{ty}");
        if !actual.contains(&expected) {
            missing.push(expected);
        }
    }
    assert!(
        missing.is_empty(),
        "stdlib_catalog is missing required Key-role descriptor entries: {missing:?}\n\
         Saw: {actual:?}",
    );
}

#[test]
fn every_required_val_type_has_a_layout_descriptor_entry() {
    let actual = collect_descriptor_symbols(LayoutDescriptorRole::Value);
    let mut missing: Vec<String> = Vec::new();
    for ty in REQUIRED_VAL_TYPES {
        let expected = format!("hew_layout_val_{ty}");
        if !actual.contains(&expected) {
            missing.push(expected);
        }
    }
    assert!(
        missing.is_empty(),
        "stdlib_catalog is missing required Value-role descriptor entries: {missing:?}\n\
         Saw: {actual:?}",
    );
}

#[test]
fn descriptor_entry_name_matches_symbol() {
    // The catalog entry's `name` and its `LayoutDescriptorSymbol.symbol`
    // must agree. Drift here would mean a Stage C consumer looking up by
    // entry name would get a different string than the linker resolves.
    for entry in entries() {
        if let BuiltinLinkage::LayoutDescriptorSymbol { symbol, .. } = entry.linkage {
            assert_eq!(
                entry.name, symbol,
                "catalog row `{}` has linkage symbol `{symbol}` — must match",
                entry.name,
            );
        }
    }
}

#[test]
fn descriptor_entries_have_the_expected_total_count() {
    // Lower-bound guard: 10 key + 11 value = 21 descriptor rows in C0b's
    // fixed scope. A drift below this means a row was silently removed;
    // the gate forces an explicit reckoning. (Above is fine — Stage C
    // may add named-record materialiser rows or further fixed types.)
    let total = entries()
        .iter()
        .filter(|e| matches!(e.linkage, BuiltinLinkage::LayoutDescriptorSymbol { .. }))
        .count();
    assert!(
        total >= 21,
        "expected ≥ 21 LayoutDescriptorSymbol catalog entries (10 K + 11 V), saw {total}",
    );
}
