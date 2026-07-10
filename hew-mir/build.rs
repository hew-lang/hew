use std::collections::BTreeMap;
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

fn main() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let classification = manifest_dir.join("../scripts/jit-symbol-classification.toml");
    let runtime_symbols = manifest_dir.join("src/runtime_symbols.rs");

    println!("cargo:rerun-if-changed={}", classification.display());
    println!("cargo:rerun-if-changed={}", runtime_symbols.display());

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
