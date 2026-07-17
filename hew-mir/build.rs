use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;

fn quoted_value(line: &str) -> Option<&str> {
    let (_, value) = line.split_once('=')?;
    let value = value.trim();
    value.strip_prefix('"')?.strip_suffix('"')
}

fn toml_results(path: &Path) -> BTreeMap<String, String> {
    let source = fs::read_to_string(path).expect("read FFI classification TOML");
    let mut results = BTreeMap::new();
    let mut symbol: Option<String> = None;
    let mut in_contract = false;

    for line in source.lines() {
        let line = line.trim();
        if line == "[[ownership.contracts]]" {
            in_contract = true;
            symbol = None;
            continue;
        }
        if !in_contract {
            continue;
        }
        if line.starts_with("symbol =") {
            symbol = quoted_value(line).map(str::to_owned);
        } else if line.starts_with("result =") {
            let result = quoted_value(line).expect("ownership result must be quoted");
            let symbol = symbol
                .take()
                .expect("ownership result must follow its symbol");
            assert!(
                results.insert(symbol.clone(), result.to_owned()).is_none(),
                "duplicate TOML ownership contract for {symbol}"
            );
        }
    }
    results
}

fn checked_runtime_results(path: &Path) -> BTreeMap<String, String> {
    let source = fs::read_to_string(path).expect("read runtime_symbols.rs");
    let marker = "const TOML_RESULT_CONSISTENCY:";
    let start = source
        .find(marker)
        .expect("runtime_symbols.rs must declare TOML_RESULT_CONSISTENCY");
    let mut results = BTreeMap::new();

    for line in source[start..].lines().skip(1) {
        let line = line.trim();
        if line == "];" {
            break;
        }
        if !line.starts_with("(\"") {
            continue;
        }
        let quoted = line.split('"').collect::<Vec<_>>();
        assert!(quoted.len() >= 4, "malformed consistency row: {line}");
        let symbol = quoted[1].to_owned();
        let result = quoted[3].to_owned();
        assert!(
            results.insert(symbol.clone(), result).is_none(),
            "duplicate runtime consistency row for {symbol}"
        );
    }
    assert!(
        !results.is_empty(),
        "TOML_RESULT_CONSISTENCY must not be empty"
    );
    results
}

fn toml_symbol_tier(source: &str, tier: &str) -> BTreeSet<String> {
    let header = format!("{tier} = [");
    let mut lines = source.lines();
    lines
        .find(|line| line.trim() == header)
        .unwrap_or_else(|| panic!("classification TOML must declare `{header}`"));

    let mut symbols = BTreeSet::new();
    for line in lines {
        let line = line.trim();
        if line == "]" {
            return symbols;
        }
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let symbol = line
            .strip_prefix('"')
            .and_then(|line| line.strip_suffix("\","))
            .unwrap_or_else(|| panic!("malformed `{tier}` symbol row: {line}"));
        assert!(
            symbols.insert(symbol.to_owned()),
            "duplicate `{tier}` TOML symbol: {symbol}"
        );
    }

    panic!("unterminated `{tier}` TOML symbol list");
}

fn runtime_backed_mir_symbols(path: &Path) -> BTreeSet<String> {
    let source = fs::read_to_string(path).expect("read runtime_symbols.rs");
    let marker = "const RUNTIME_BACKED_MIR_SYMBOLS:";
    let start = source
        .find(marker)
        .expect("runtime_symbols.rs must declare RUNTIME_BACKED_MIR_SYMBOLS");
    let mut symbols = BTreeSet::new();

    for line in source[start..].lines().skip(1) {
        let line = line.trim();
        if line == "];" {
            assert!(
                !symbols.is_empty(),
                "RUNTIME_BACKED_MIR_SYMBOLS must not be empty"
            );
            return symbols;
        }
        if !line.starts_with('"') {
            continue;
        }
        let symbol = line
            .strip_prefix('"')
            .and_then(|line| line.strip_suffix("\","))
            .unwrap_or_else(|| panic!("malformed RUNTIME_BACKED_MIR_SYMBOLS row: {line}"));
        assert!(
            symbols.insert(symbol.to_owned()),
            "duplicate runtime-backed MIR symbol: {symbol}"
        );
    }

    panic!("unterminated RUNTIME_BACKED_MIR_SYMBOLS");
}

fn main() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let classification = manifest_dir.join("../scripts/jit-symbol-classification.toml");
    let runtime_symbols = manifest_dir.join("src/runtime_symbols.rs");

    println!("cargo:rerun-if-changed={}", classification.display());
    println!("cargo:rerun-if-changed={}", runtime_symbols.display());

    let classification_source =
        fs::read_to_string(&classification).expect("read FFI classification TOML");
    let mut jit_symbols = toml_symbol_tier(&classification_source, "stable");
    jit_symbols.extend(toml_symbol_tier(&classification_source, "codegen-stable"));
    let missing: Vec<_> = runtime_backed_mir_symbols(&runtime_symbols)
        .difference(&jit_symbols)
        .cloned()
        .collect();
    assert!(
        missing.is_empty(),
        "runtime-backed MIR symbols missing from TOML `stable` or \
         `codegen-stable` classification: {}",
        missing.join(", ")
    );

    let toml = toml_results(&classification);
    for (symbol, expected) in checked_runtime_results(&runtime_symbols) {
        let actual = toml.get(&symbol).unwrap_or_else(|| {
            panic!("{symbol} is runtime-checked but has no TOML ownership contract")
        });
        assert_eq!(
            actual, &expected,
            "TOML/runtime result ownership mismatch for {symbol}"
        );
    }
}
