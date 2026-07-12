//! Compile-time access to the classified Hew runtime and stdlib FFI symbols.

use std::collections::HashSet;
use std::sync::OnceLock;

const JIT_CLASSIFICATION_TOML: &str = include_str!("../../scripts/jit-symbol-classification.toml");

fn parse_symbol_blocks(headers: &[&str]) -> HashSet<&'static str> {
    let mut set = HashSet::new();
    for header in headers {
        let mut inside = false;
        for line in JIT_CLASSIFICATION_TOML.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with(header) {
                inside = true;
                continue;
            }
            if inside && trimmed == "]" {
                break;
            }
            if inside {
                if let Some(rest) = trimmed.strip_prefix('"') {
                    if let Some(symbol) = rest.split('"').next() {
                        if !symbol.is_empty() {
                            set.insert(symbol);
                        }
                    }
                }
            }
        }
    }
    set
}

/// Symbols user code may declare through `extern "rt"`.
#[must_use]
pub fn stable_symbols() -> &'static HashSet<&'static str> {
    static SET: OnceLock<HashSet<&'static str>> = OnceLock::new();
    SET.get_or_init(|| parse_symbol_blocks(&["stable = [", "stable-stdlib = ["]))
}

/// Whether `symbol` belongs to a classified Hew runtime or stdlib FFI tier.
///
/// Classified string-returning symbols already produce Hew header-aware
/// strings. Unclassified raw `extern "C"` package symbols retain the legacy
/// malloc-owned return contract and require codegen adoption.
#[must_use]
pub fn is_classified_hew_ffi_symbol(symbol: &str) -> bool {
    static SET: OnceLock<HashSet<&'static str>> = OnceLock::new();
    SET.get_or_init(|| {
        parse_symbol_blocks(&[
            "stable = [",
            "stable-stdlib = [",
            "codegen-stable = [",
            "internal = [",
        ])
    })
    .contains(symbol)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stable_and_internal_tiers_are_distinguished() {
        assert!(stable_symbols().contains("hew_env_get"));
        assert!(is_classified_hew_ffi_symbol("hew_actor_cooperate"));
        assert!(!stable_symbols().contains("hew_actor_cooperate"));
        assert!(!is_classified_hew_ffi_symbol("hew_testffi_name"));
    }
}
