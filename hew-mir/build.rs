use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};

use hew_types::runtime_call::{all_runtime_call_families, RuntimeCallFamily};

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

/// One `[[ownership.contracts]]` row from the FFI classification TOML.
struct ContractRow {
    result: String,
    params: Vec<String>,
    release_symbol: String,
    discharge_depth: String,
}

fn quoted_list(line: &str) -> Vec<String> {
    let (_, value) = line.split_once('=').expect("params line must carry `=`");
    let value = value.trim();
    let inner = value
        .strip_prefix('[')
        .and_then(|v| v.strip_suffix(']'))
        .expect("params must be a single-line array");
    inner
        .split(',')
        .map(str::trim)
        .filter(|part| !part.is_empty())
        .map(|part| {
            part.strip_prefix('"')
                .and_then(|p| p.strip_suffix('"'))
                .expect("params entries must be quoted")
                .to_owned()
        })
        .collect()
}

/// Parse the full `[[ownership.contracts]]` table. Every axis is validated
/// against the closed schema vocabularies here (fail-closed: an unknown
/// spelling aborts the build rather than degrading to a default), and the
/// owned-result/release-symbol coupling from `verify-ffi-symbols.py` is
/// re-checked so the generated compiler table can never carry a row the
/// out-of-band validator would reject.
fn toml_ownership_contracts(path: &Path) -> BTreeMap<String, ContractRow> {
    let source = fs::read_to_string(path).expect("read FFI classification TOML");
    let mut contracts = BTreeMap::new();
    let mut current: Option<(Option<String>, ContractRow)> = None;

    let finish = |entry: Option<(Option<String>, ContractRow)>,
                  contracts: &mut BTreeMap<String, ContractRow>| {
        if let Some((symbol, row)) = entry {
            let symbol = symbol.expect("ownership contract missing `symbol`");
            assert!(
                ["fresh", "retained", "borrowed", "none"].contains(&row.result.as_str()),
                "unknown ownership result for {symbol}: {}",
                row.result
            );
            for param in &row.params {
                assert!(
                    ["borrow", "consume", "retain"].contains(&param.as_str()),
                    "unknown param ownership for {symbol}: {param}"
                );
            }
            assert!(
                ["shallow", "deep", "none"].contains(&row.discharge_depth.as_str()),
                "unknown discharge depth for {symbol}: {}",
                row.discharge_depth
            );
            if matches!(row.result.as_str(), "fresh" | "retained") {
                assert!(
                    !row.release_symbol.is_empty() && row.discharge_depth != "none",
                    "owned result for {symbol} requires release-symbol and discharge depth"
                );
            } else {
                assert!(
                    row.release_symbol.is_empty() && row.discharge_depth == "none",
                    "borrowed/none result for {symbol} must carry no release axis"
                );
            }
            assert!(
                contracts.insert(symbol.clone(), row).is_none(),
                "duplicate TOML ownership contract for {symbol}"
            );
        }
    };

    for line in source.lines() {
        let line = line.trim();
        if line == "[[ownership.contracts]]" {
            finish(current.take(), &mut contracts);
            current = Some((
                None,
                ContractRow {
                    result: String::new(),
                    params: Vec::new(),
                    release_symbol: String::new(),
                    discharge_depth: String::new(),
                },
            ));
            continue;
        }
        let Some((symbol, row)) = current.as_mut() else {
            continue;
        };
        if line.starts_with("symbol =") {
            *symbol = Some(
                quoted_value(line)
                    .expect("contract symbol must be quoted")
                    .to_owned(),
            );
        } else if line.starts_with("result =") {
            quoted_value(line)
                .expect("contract result must be quoted")
                .clone_into(&mut row.result);
        } else if line.starts_with("params =") {
            row.params = quoted_list(line);
        } else if line.starts_with("release-symbol =") {
            quoted_value(line)
                .expect("contract release-symbol must be quoted")
                .clone_into(&mut row.release_symbol);
        } else if line.starts_with("discharge-depth =") {
            quoted_value(line)
                .expect("contract discharge-depth must be quoted")
                .clone_into(&mut row.discharge_depth);
        }
    }
    finish(current.take(), &mut contracts);
    assert!(
        !contracts.is_empty(),
        "classification TOML must declare ownership contracts"
    );
    contracts
}

/// Generate the static, sorted extern-ownership table consumed by
/// `hew_mir::ffi_contracts` via `include!`. The TOML is the single source of
/// truth; this table is a build-time projection of it, so the two carriers
/// cannot drift without a rebuild (a 1:1 drift test re-checks with an
/// independent TOML parser).
fn generate_ffi_ownership_table(contracts: &BTreeMap<String, ContractRow>) {
    let mut generated = String::from(
        "// Generated by hew-mir/build.rs from scripts/jit-symbol-classification.toml.\n\
         // Do not edit; edit the TOML `[[ownership.contracts]]` rows instead.\n\
         /// Sorted (by symbol) extern-ownership contract rows projected from the\n\
         /// machine-checked TOML classification. Sorted order is the binary-search\n\
         /// invariant of [`extern_ownership_contract`].\n\
         pub static FFI_OWNERSHIP_CONTRACTS: &[(&str, ExternOwnershipContract)] = &[\n",
    );
    for (symbol, row) in contracts {
        let params = row
            .params
            .iter()
            .map(|param| match param.as_str() {
                "borrow" => "ExternParamOwnership::Borrow",
                "consume" => "ExternParamOwnership::Consume",
                "retain" => "ExternParamOwnership::Retain",
                other => panic!("unmapped param ownership: {other}"),
            })
            .collect::<Vec<_>>()
            .join(", ");
        let result = match row.result.as_str() {
            "fresh" => "ExternResultOwnership::Fresh",
            "retained" => "ExternResultOwnership::Retained",
            "borrowed" => "ExternResultOwnership::Borrowed",
            "none" => "ExternResultOwnership::None",
            other => panic!("unmapped result ownership: {other}"),
        };
        let depth = match row.discharge_depth.as_str() {
            "shallow" => "ReleaseDischargeDepth::Shallow",
            "deep" => "ReleaseDischargeDepth::Deep",
            "none" => "ReleaseDischargeDepth::None",
            other => panic!("unmapped discharge depth: {other}"),
        };
        writeln!(
            generated,
            "    (\"{symbol}\", ExternOwnershipContract {{ params: &[{params}], \
             result: {result}, release_symbol: \"{}\", discharge_depth: {depth} }}),",
            row.release_symbol
        )
        .expect("write generated contract row");
    }
    generated.push_str("];\n");

    let out_dir = PathBuf::from(std::env::var("OUT_DIR").expect("OUT_DIR set by cargo"));
    fs::write(out_dir.join("ffi_ownership_contracts.rs"), generated)
        .expect("write generated FFI ownership table");
}

fn runtime_backed_mir_symbols() -> BTreeSet<String> {
    all_runtime_call_families()
        .into_iter()
        .filter(|family| family.is_runtime_backed_mir_family())
        .map(RuntimeCallFamily::c_symbol)
        .map(str::to_owned)
        .collect()
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
    let missing: Vec<_> = runtime_backed_mir_symbols()
        .difference(&jit_symbols)
        .cloned()
        .collect();
    assert!(
        missing.is_empty(),
        "runtime-backed MIR symbols missing from TOML `stable` or \
         `codegen-stable` classification: {}",
        missing.join(", ")
    );

    generate_ffi_ownership_table(&toml_ownership_contracts(&classification));

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
