//! W4.001 Stage C0b — permanent gate: every `MethodTarget.symbol_name`
//! produced by the Stage B `collection_dispatch_registry` must name a real
//! `#[no_mangle] extern "C"` symbol exported by `hew-runtime`.
//!
//! This is the test that would have caught Stage B's bare-name vs
//! linkable-symbol regression (the registry seeded `"hew_hashmap_insert"`
//! while the only linkable runtime symbol was `hew_hashmap_insert_layout`).
//! Per plan §4 Stage C0b ("permanent gate") and brief §"Mandatory tests".
//!
//! ## How linkage is verified — *link-time*, not `dlsym`
//!
//! macOS's `dlsym(RTLD_DEFAULT, ...)` only consults the dynamic symbol
//! table, which for a `cargo test` binary on Darwin excludes statically
//! linked archive symbols unless the host re-links with `-rdynamic` /
//! `-Wl,-export_dynamic`. We sidestep that whole class of build-config
//! fragility by binding the check at *link time*:
//!
//! 1. The `extern "C"` declarations below are resolved at link time. If
//!    `hew-runtime` ever stops exporting any of them, this test binary
//!    fails to link — a stronger and earlier signal than a runtime
//!    `assert!`.
//! 2. The runtime check then walks `collection_dispatch_registry_for_tests`
//!    and asserts every `symbol_name` is present in the link-verified
//!    set built from the same `extern "C"` decls. A registry that drifts
//!    to a fresh symbol name not added here fails loudly with the
//!    offending coordinate.
//!
//! This combination is the bare-name regression gate: a registry that
//! spells `"hew_hashmap_insert"` will fail because that symbol is not in
//! the link-verified set, and adding it to the set would force the
//! gate-maintainer to also add a matching `extern "C"` decl — which
//! would then fail to link unless hew-runtime really did export it.

use std::collections::{BTreeSet, HashMap};
use std::ffi::c_void;

use hew_types::check::collection_dispatch_registry_for_tests;

// Force the linker to pull the hew-runtime archive (and its
// `#[no_mangle]` layout descriptor statics + kernel `_layout` fns) into
// this test binary. The dev-dependency alone is not enough — Rust elides
// unreferenced rlibs from the link line. Calling a real fn from
// hew-runtime forces inclusion of the whole archive.
fn link_anchor_pull_runtime() {
    // SAFETY: trivially safe — no FFI, no pointer math, no unsafe ops.
    let _capacity: u64 = hew_runtime::hashmap::layout_init_capacity_for_link_anchor();
}

// ---------------------------------------------------------------------------
// Link-time symbol witnesses
// ---------------------------------------------------------------------------
//
// Each `extern "C" fn` decl below is resolved at *link* time. Signatures
// are deliberately type-erased (`*const c_void`) — the gate only cares
// about the *symbol*, not the ABI shape (the latter is covered by the
// declarative_*_ffi_differential.rs gate family).

extern "C" {
    fn hew_hashmap_insert_layout(m: *mut c_void, key: *const c_void, val: *const c_void) -> bool;
    fn hew_hashmap_get_layout(m: *const c_void, key: *const c_void) -> *const c_void;
    fn hew_hashmap_contains_key_layout(m: *const c_void, key: *const c_void) -> bool;
    fn hew_hashmap_remove_layout(m: *mut c_void, key: *const c_void) -> bool;
    fn hew_hashmap_len_layout(m: *const c_void) -> i64;
    fn hew_hashmap_keys_layout(m: *const c_void) -> *mut c_void;
    fn hew_hashmap_values_layout(m: *const c_void) -> *mut c_void;
    fn hew_hashmap_clone_layout(m: *const c_void) -> *mut c_void;
    fn hew_hashset_insert_layout(s: *mut c_void, elem: *const c_void) -> bool;
    fn hew_hashset_contains_layout(s: *const c_void, elem: *const c_void) -> bool;
    fn hew_hashset_remove_layout(s: *mut c_void, elem: *const c_void) -> bool;
    fn hew_hashset_len_layout(s: *const c_void) -> i64;
    fn hew_hashset_is_empty_layout(s: *const c_void) -> bool;
    fn hew_hashset_clone_layout(s: *const c_void) -> *mut c_void;
}

/// Statically-known set of kernel symbols this gate has *proved linkable*.
///
/// Adding a new Stage B symbol requires (a) an `extern "C"` decl above so
/// the linker resolves it, and (b) an entry here. Both edits are
/// necessary — half the wiring is not a green test.
fn known_linked_kernel_symbols() -> HashMap<&'static str, *const ()> {
    let mut m: HashMap<&'static str, *const ()> = HashMap::new();
    m.insert(
        "hew_hashmap_insert_layout",
        hew_hashmap_insert_layout as *const (),
    );
    m.insert(
        "hew_hashmap_get_layout",
        hew_hashmap_get_layout as *const (),
    );
    m.insert(
        "hew_hashmap_contains_key_layout",
        hew_hashmap_contains_key_layout as *const (),
    );
    m.insert(
        "hew_hashmap_remove_layout",
        hew_hashmap_remove_layout as *const (),
    );
    m.insert(
        "hew_hashmap_len_layout",
        hew_hashmap_len_layout as *const (),
    );
    m.insert(
        "hew_hashmap_keys_layout",
        hew_hashmap_keys_layout as *const (),
    );
    m.insert(
        "hew_hashmap_values_layout",
        hew_hashmap_values_layout as *const (),
    );
    m.insert(
        "hew_hashmap_clone_layout",
        hew_hashmap_clone_layout as *const (),
    );
    m.insert(
        "hew_hashset_insert_layout",
        hew_hashset_insert_layout as *const (),
    );
    m.insert(
        "hew_hashset_contains_layout",
        hew_hashset_contains_layout as *const (),
    );
    m.insert(
        "hew_hashset_remove_layout",
        hew_hashset_remove_layout as *const (),
    );
    m.insert(
        "hew_hashset_len_layout",
        hew_hashset_len_layout as *const (),
    );
    m.insert(
        "hew_hashset_is_empty_layout",
        hew_hashset_is_empty_layout as *const (),
    );
    m.insert(
        "hew_hashset_clone_layout",
        hew_hashset_clone_layout as *const (),
    );
    m
}

// ---------------------------------------------------------------------------
// Layout descriptor link witnesses (C0b — surface 1 + 2)
// ---------------------------------------------------------------------------

#[allow(
    dead_code,
    reason = "addresses returned for their link-time side-effect; never dereferenced"
)]
fn descriptor_addresses() -> Vec<(&'static str, *const ())> {
    vec![
        (
            "hew_layout_key_i32",
            (&raw const hew_cabi::map::hew_layout_key_i32).cast(),
        ),
        (
            "hew_layout_key_i64",
            (&raw const hew_cabi::map::hew_layout_key_i64).cast(),
        ),
        (
            "hew_layout_key_u32",
            (&raw const hew_cabi::map::hew_layout_key_u32).cast(),
        ),
        (
            "hew_layout_key_u64",
            (&raw const hew_cabi::map::hew_layout_key_u64).cast(),
        ),
        (
            "hew_layout_key_f32",
            (&raw const hew_cabi::map::hew_layout_key_f32).cast(),
        ),
        (
            "hew_layout_key_f64",
            (&raw const hew_cabi::map::hew_layout_key_f64).cast(),
        ),
        (
            "hew_layout_key_bool",
            (&raw const hew_cabi::map::hew_layout_key_bool).cast(),
        ),
        (
            "hew_layout_key_char",
            (&raw const hew_cabi::map::hew_layout_key_char).cast(),
        ),
        (
            "hew_layout_key_string",
            (&raw const hew_cabi::map::hew_layout_key_string).cast(),
        ),
        (
            "hew_layout_key_bytes",
            (&raw const hew_cabi::map::hew_layout_key_bytes).cast(),
        ),
        (
            "hew_layout_val_i32",
            (&raw const hew_cabi::map::hew_layout_val_i32).cast(),
        ),
        (
            "hew_layout_val_i64",
            (&raw const hew_cabi::map::hew_layout_val_i64).cast(),
        ),
        (
            "hew_layout_val_u32",
            (&raw const hew_cabi::map::hew_layout_val_u32).cast(),
        ),
        (
            "hew_layout_val_u64",
            (&raw const hew_cabi::map::hew_layout_val_u64).cast(),
        ),
        (
            "hew_layout_val_f32",
            (&raw const hew_cabi::map::hew_layout_val_f32).cast(),
        ),
        (
            "hew_layout_val_f64",
            (&raw const hew_cabi::map::hew_layout_val_f64).cast(),
        ),
        (
            "hew_layout_val_bool",
            (&raw const hew_cabi::map::hew_layout_val_bool).cast(),
        ),
        (
            "hew_layout_val_char",
            (&raw const hew_cabi::map::hew_layout_val_char).cast(),
        ),
        (
            "hew_layout_val_string",
            (&raw const hew_cabi::map::hew_layout_val_string).cast(),
        ),
        (
            "hew_layout_val_bytes",
            (&raw const hew_cabi::map::hew_layout_val_bytes).cast(),
        ),
        (
            "hew_layout_val_unit",
            (&raw const hew_cabi::map::hew_layout_val_unit).cast(),
        ),
    ]
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[test]
fn every_stage_b_method_target_symbol_is_link_resolved() {
    link_anchor_pull_runtime();
    let linked = known_linked_kernel_symbols();
    let registry = collection_dispatch_registry_for_tests();

    let mut missing: Vec<String> = Vec::new();
    let mut checked: BTreeSet<String> = BTreeSet::new();

    for (_impl_id, def) in registry.iter() {
        for (method_name, target) in &def.methods {
            let symbol = target.symbol_name.as_str();
            if def.trait_name == "Seq" && symbol.ends_with("_FAMILY") {
                // Vec keeps per-element scalar/layout symbol selection in
                // `resolve_vec_runtime_symbol`; the registry row is only the
                // resolved-call admission substrate until that duality is
                // collapsed. Placeholder family roots are intentionally not
                // runtime exports.
                continue;
            }
            checked.insert(symbol.to_string());
            let resolved = linked.get(symbol).copied().filter(|p| !p.is_null());
            if resolved.is_none() {
                missing.push(format!(
                    "registry entry `{trait_name}::{method_name}` references symbol \
                     `{symbol}` but no link-verified witness exists for it. \
                     Add an `extern \"C\"` decl + `known_linked_kernel_symbols` entry, \
                     and ensure hew-runtime exports it.",
                    trait_name = def.trait_name,
                ));
            }
        }
    }

    assert!(
        missing.is_empty(),
        "Stage B registry references kernel symbols that lack a link-verified witness.\n\
         This is the bare-name regression class — see W4.001 plan §4 Stage C0b.\n\
         Missing:\n  - {}\nChecked total: {}",
        missing.join("\n  - "),
        checked.len(),
    );

    // Defensive lower-bound: the Stage B registry seeds 8 HashMap methods
    // (insert, get, contains_key, remove, len, keys, values, clone)
    // + 6 HashSet methods (insert, contains, remove, len, is_empty, clone)
    // = 14 symbols. A drift below this means the registry shrank silently;
    // the gate forces an explicit reckoning.
    assert!(
        checked.len() >= 14,
        "expected ≥ 14 distinct kernel symbols in the Stage B registry, \
         saw {} ({:?})",
        checked.len(),
        checked,
    );
}

#[test]
fn every_stage_b_symbol_uses_canonical_kernel_suffix() {
    // Belt-and-suspenders against the bare-name regression: every
    // symbol the Stage B registry hands out must end in `_layout` (the
    // canonical kernel-ABI suffix). The runtime's bare `hew_hashmap_*`
    // family is the legacy per-V scalar-shim ABI and dies at Stage C.
    let registry = collection_dispatch_registry_for_tests();
    let mut violations: Vec<String> = Vec::new();
    for (_impl_id, def) in registry.iter() {
        for (method_name, target) in &def.methods {
            if def.trait_name == "Seq" && target.symbol_name.ends_with("_FAMILY") {
                // Vec placeholder roots are overwritten per call site by
                // `resolve_vec_runtime_symbol`; only concrete HashMap/HashSet
                // kernel symbols participate in this suffix invariant.
                continue;
            }
            if !target.symbol_name.ends_with("_layout") {
                violations.push(format!(
                    "{trait_name}::{method_name} -> `{symbol}` (must end in `_layout`)",
                    trait_name = def.trait_name,
                    symbol = target.symbol_name,
                ));
            }
        }
    }
    assert!(
        violations.is_empty(),
        "Stage B registry seeds non-canonical (non-`_layout`) symbols:\n  - {}",
        violations.join("\n  - "),
    );
}

#[test]
fn cabi_descriptor_symbols_link_resolve_in_runtime() {
    link_anchor_pull_runtime();
    // Each address below is bound at link time. If any cabi-declared
    // descriptor static were not exported by hew-runtime, this test
    // binary would not have linked — `cargo test` would fail at build.
    // The runtime check below proves the addresses are not silently null
    // (and that the cabi declaration list stayed in sync with the
    // hew-cabi public surface).
    let addrs = descriptor_addresses();
    let mut nulls: Vec<&str> = Vec::new();
    for (name, addr) in &addrs {
        if addr.is_null() {
            nulls.push(*name);
        }
    }
    assert!(
        nulls.is_empty(),
        "cabi-declared layout descriptor statics resolved to NULL — \
         hew-runtime did not export: {nulls:?}",
    );

    // Cross-check with the cabi-exported symbol lists. A drift between
    // `descriptor_addresses` (the link-verified set in this test) and
    // `KEY_LAYOUT_DESCRIPTOR_SYMBOLS` / `VAL_LAYOUT_DESCRIPTOR_SYMBOLS`
    // (the cabi public manifest) would mean Stage C consumers and this
    // gate disagree about scope.
    let linked_names: BTreeSet<&str> = addrs.iter().map(|(n, _)| *n).collect();
    let cabi_names: BTreeSet<&str> = hew_cabi::map::KEY_LAYOUT_DESCRIPTOR_SYMBOLS
        .iter()
        .copied()
        .chain(hew_cabi::map::VAL_LAYOUT_DESCRIPTOR_SYMBOLS.iter().copied())
        .collect();
    assert_eq!(
        linked_names, cabi_names,
        "descriptor link-witness set drifted from cabi public manifest"
    );
}
