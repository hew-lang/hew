//! 1:1 drift proof between the two ownership-contract carriers.
//!
//! `scripts/jit-symbol-classification.toml` `[[ownership.contracts]]` is the
//! authority; `hew-mir/build.rs` projects it into the static
//! `FFI_OWNERSHIP_CONTRACTS` table with a hand-rolled line parser. This test
//! re-parses the TOML with the independent `toml` crate and asserts exact
//! row-for-row equality, so neither a build-script parser bug nor a stale
//! generated table can put the compiler's facts out of sync with the
//! machine-checked contracts.

#![cfg(not(target_arch = "wasm32"))]

use std::collections::BTreeMap;
use std::path::PathBuf;

use hew_mir::ffi_contracts::{
    ExternParamOwnership, ExternResultOwnership, ReleaseDischargeDepth, FFI_OWNERSHIP_CONTRACTS,
};

#[derive(Debug, serde::Deserialize)]
struct Document {
    ownership: Ownership,
}

#[derive(Debug, serde::Deserialize)]
struct Ownership {
    contracts: Vec<Contract>,
}

#[derive(Debug, serde::Deserialize)]
struct Contract {
    symbol: String,
    result: String,
    params: Vec<String>,
    #[serde(rename = "release-symbol")]
    release_symbol: String,
    #[serde(rename = "discharge-depth")]
    discharge_depth: String,
}

fn classification_toml() -> Document {
    let path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../scripts/jit-symbol-classification.toml");
    let source = std::fs::read_to_string(&path).expect("read FFI classification TOML");
    toml::from_str(&source).expect("parse FFI classification TOML")
}

fn param_spelling(param: ExternParamOwnership) -> &'static str {
    match param {
        ExternParamOwnership::Borrow => "borrow",
        ExternParamOwnership::Consume => "consume",
        ExternParamOwnership::Retain => "retain",
    }
}

fn result_spelling(result: ExternResultOwnership) -> &'static str {
    match result {
        ExternResultOwnership::Fresh => "fresh",
        ExternResultOwnership::Retained => "retained",
        ExternResultOwnership::Borrowed => "borrowed",
        ExternResultOwnership::None => "none",
    }
}

fn depth_spelling(depth: ReleaseDischargeDepth) -> &'static str {
    match depth {
        ReleaseDischargeDepth::Shallow => "shallow",
        ReleaseDischargeDepth::Deep => "deep",
        ReleaseDischargeDepth::None => "none",
    }
}

#[test]
fn compiler_table_matches_toml_one_to_one() {
    let document = classification_toml();
    let mut toml_rows = BTreeMap::new();
    for contract in document.ownership.contracts {
        let symbol = contract.symbol.clone();
        assert!(
            toml_rows.insert(symbol.clone(), contract).is_none(),
            "duplicate TOML contract for {symbol}"
        );
    }

    assert_eq!(
        FFI_OWNERSHIP_CONTRACTS.len(),
        toml_rows.len(),
        "compiler table and TOML carry different contract counts"
    );

    for (symbol, compiled) in FFI_OWNERSHIP_CONTRACTS {
        let expected = toml_rows
            .get(*symbol)
            .unwrap_or_else(|| panic!("{symbol} is compiled but absent from the TOML"));
        let compiled_params: Vec<&str> = compiled
            .params
            .iter()
            .map(|param| param_spelling(*param))
            .collect();
        assert_eq!(compiled_params, expected.params, "{symbol}: params drift");
        assert_eq!(
            result_spelling(compiled.result),
            expected.result,
            "{symbol}: result drift"
        );
        assert_eq!(
            compiled.release_symbol, expected.release_symbol,
            "{symbol}: release-symbol drift"
        );
        assert_eq!(
            depth_spelling(compiled.discharge_depth),
            expected.discharge_depth,
            "{symbol}: discharge-depth drift"
        );
    }
}
