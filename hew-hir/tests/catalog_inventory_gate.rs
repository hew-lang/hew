//! Inventory gate: every `stdlib_catalog` builtin with a runtime symbol must
//! appear in the `stable` section of `scripts/jit-symbol-classification.toml`.
//!
//! This test prevents silent drift where a catalog row names a symbol that is
//! either misspelled, removed from the runtime, or never classified.
//!
//! Authority: the `stable` list in `scripts/jit-symbol-classification.toml`
//! covers every `#[no_mangle] extern "C"` symbol that JIT modules may
//! reference directly. This is a superset of `MIR_EMITTER_RUNTIME_SYMBOLS` (which is the
//! MIR-emitter subset). We validate against `stable` here because:
//!
//! - `RuntimeFfiShim`, `ToStringShim`, `StringCloneShim`, and `PrintIntercept`
//!   entries name real C-ABI symbols the codegen will link against.
//! - `MIR_EMITTER_RUNTIME_SYMBOLS` is not exhaustive: for example `hew_int_to_string` and
//!   `hew_assert_eq_*` are in the catalog but not in M2. Using M2 would produce
//!   false failures for legitimately classified symbols.
//! - `CompilerIntrinsic` entries do not name a C-ABI symbol; they are handled
//!   entirely inside the codegen backend (MLIR/LLVM ops). They always pass.
//!
//! When a symbol is missing from `stable`, it either needs to be added to
//! `scripts/jit-symbol-classification.toml` or the catalog row's linkage
//! is wrong.

use hew_hir::stdlib_catalog::{entries, BuiltinLinkage};
use std::collections::HashSet;

/// Parse the `stable = [ ... ]` block from the TOML classification file using
/// simple line-based extraction. The block format is well-defined (one quoted
/// string per line inside `stable = [` ... `]`) and does not require a full
/// TOML parser.
///
/// WHY: avoids adding a toml crate test-dep; the format has been stable since
/// the file was introduced and is enforced by the verifier (scripts/verify-jit-symbols.sh).
fn parse_stable_symbols() -> HashSet<String> {
    let raw = include_str!("../../scripts/jit-symbol-classification.toml");
    let mut inside = false;
    let mut symbols = HashSet::new();

    for line in raw.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("stable = [") {
            inside = true;
            continue;
        }
        // The internal list starts after stable; stop at the next `]` that
        // closes the stable section (it's always on its own line).
        if inside && trimmed == "]" {
            break;
        }
        // A data line looks like: `  "hew_something",` or `  "hew_something"`
        if inside {
            if let Some(rest) = trimmed.strip_prefix('"') {
                if let Some(sym) = rest.split('"').next() {
                    if !sym.is_empty() {
                        symbols.insert(sym.to_string());
                    }
                }
            }
        }
    }

    symbols
}

#[test]
fn catalog_runtime_symbols_are_classified() {
    let stable = parse_stable_symbols();
    assert!(
        !stable.is_empty(),
        "failed to parse any symbols from scripts/jit-symbol-classification.toml — \
         check that the file exists and the stable = [ ] block is intact"
    );

    let mut failures: Vec<String> = Vec::new();

    for entry in entries() {
        let symbol = match entry.linkage {
            BuiltinLinkage::RuntimeFfiShim { symbol }
            | BuiltinLinkage::ToStringShim { symbol }
            | BuiltinLinkage::StringCloneShim { symbol } => symbol,
            BuiltinLinkage::PrintIntercept { runtime_symbol, .. } => runtime_symbol,
            // CompilerIntrinsic entries do not name a C-ABI symbol; they map to
            // MLIR/LLVM backend ops. CalleeNameDispatchOnly entries are
            // intercepted in codegen by callee name and never declare an LLVM
            // extern of their own. Always considered classified.
            // CompilerIntrinsic / CalleeNameDispatchOnly entries do not name a
            // C-ABI symbol exposed at the JIT host boundary; LayoutDescriptorSymbol
            // entries name `#[no_mangle] pub static` descriptors in
            // `hew-runtime/src/layout_intrinsics.rs`, not extern "C" fns, and the
            // JIT-symbol-classification gate only enumerates fn exports (see
            // `scripts/verify-ffi-symbols.py:4`). All three are out of scope here.
            BuiltinLinkage::CompilerIntrinsic { .. }
            | BuiltinLinkage::CalleeNameDispatchOnly
            | BuiltinLinkage::LayoutDescriptorSymbol { .. } => {
                continue;
            }
            // NodeRegisterByPid declares two C-ABI symbols; check both.
            BuiltinLinkage::NodeRegisterByPid {
                register_symbol,
                pid_accessor,
            } => {
                for sym in [register_symbol, pid_accessor] {
                    if !stable.contains(sym) {
                        failures.push(format!(
                            "catalog row `{}` (linkage symbol `{sym}`) is not in the \
                             `stable` section of scripts/jit-symbol-classification.toml",
                            entry.name,
                        ));
                    }
                }
                continue;
            }
        };

        if !stable.contains(symbol) {
            failures.push(format!(
                "catalog row `{}` (linkage symbol `{}`) is not in the \
                 `stable` section of scripts/jit-symbol-classification.toml",
                entry.name, symbol
            ));
        }
    }

    if !failures.is_empty() {
        let list = failures.join("\n  ");
        panic!(
            "{} catalog row(s) name a runtime symbol absent from the classification table:\n  {}\n\n\
             To fix: add each missing symbol to the `stable` list in \
             scripts/jit-symbol-classification.toml, or correct the catalog linkage.",
            failures.len(),
            list
        );
    }
}

#[test]
fn stable_symbol_parser_is_non_empty() {
    // A regression guard: if the include_str! path is wrong or the parser
    // silently produces nothing, catch it here rather than letting the
    // coverage test pass vacuously.
    let stable = parse_stable_symbols();
    assert!(
        stable.len() > 100,
        "expected > 100 stable symbols, got {} — parser may be broken",
        stable.len()
    );
    // Spot-check a few well-known entries.
    assert!(
        stable.contains("hew_actor_send"),
        "hew_actor_send missing from stable"
    );
    assert!(
        stable.contains("hew_duplex_send"),
        "hew_duplex_send missing from stable"
    );
    assert!(
        stable.contains("hew_sleep_ms"),
        "hew_sleep_ms missing from stable"
    );
}

#[test]
fn catalog_contains_bytes_constructor_and_method_targets() {
    let mut names: HashSet<&str> = HashSet::new();
    for entry in entries() {
        names.insert(entry.name);
        if let Some(symbol) = entry.linkage.runtime_symbol() {
            names.insert(symbol);
        }
    }
    for name in [
        "bytes::new",
        "hew_bytes_push",
        "hew_vec_pop_i32",
        "hew_vec_len",
        "hew_vec_get_i32",
        "hew_vec_set_i32",
        "hew_vec_is_empty",
        "hew_vec_clear",
        "hew_vec_contains_i32",
        "hew_bytes_to_string",
        "hew_vec_append",
    ] {
        assert!(names.contains(name), "missing bytes catalog row `{name}`");
    }
}
