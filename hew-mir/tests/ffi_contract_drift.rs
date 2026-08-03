//! 1:1 drift proof between the two ownership-contract carriers.
//!
//! `scripts/jit-symbol-classification.toml` `[[ownership.contracts]]` is the
//! authority; `hew-types/build.rs` projects it into the static
//! `FFI_OWNERSHIP_CONTRACTS` table with a hand-rolled line parser. This test
//! re-parses the TOML with the independent `toml` crate and asserts exact
//! row-for-row equality, so neither a build-script parser bug nor a stale
//! generated table can put the compiler's facts out of sync with the
//! machine-checked contracts.

#![cfg(not(target_arch = "wasm32"))]

use std::collections::BTreeMap;
use std::path::PathBuf;

use hew_mir::ffi_contracts::{
    ExternParamOwnership, ExternResultOwnership, ExternResultRetention, ReleaseDischargeDepth,
    FFI_OWNERSHIP_CONTRACTS,
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
    #[serde(rename = "resource-param-types", default)]
    resource_param_types: Vec<String>,
    #[serde(rename = "resource-result-type", default)]
    resource_result_type: Option<String>,
    #[serde(rename = "release-symbol")]
    release_symbol: String,
    #[serde(rename = "discharge-depth")]
    discharge_depth: String,
    #[serde(rename = "result-retention", default)]
    result_retention: String,
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

fn retention_spelling(retention: ExternResultRetention) -> &'static str {
    match retention {
        ExternResultRetention::Transferred => "transferred",
        ExternResultRetention::SharedRefcount => "shared-refcount",
        ExternResultRetention::Unspecified => "",
    }
}

#[test]
fn string_clone_records_an_independently_balanced_shared_owner() {
    let document = classification_toml();
    let source = document
        .ownership
        .contracts
        .iter()
        .find(|row| row.symbol == "hew_string_clone")
        .expect("TOML string clone ownership row");
    assert_eq!(source.result, "retained");
    assert_eq!(source.result_retention, "shared-refcount");

    let (_, compiled) = FFI_OWNERSHIP_CONTRACTS
        .iter()
        .find(|(symbol, _)| *symbol == "hew_string_clone")
        .expect("compiled string clone ownership row");
    assert_eq!(compiled.result, ExternResultOwnership::Retained);
    assert_eq!(
        compiled.result_retention,
        ExternResultRetention::SharedRefcount
    );
    assert!(compiled.result_retention.authorizes_caller_release());
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
            compiled.resource_param_types, expected.resource_param_types,
            "{symbol}: resource-param-types drift"
        );
        assert_eq!(
            compiled.resource_result_type,
            expected.resource_result_type.as_deref(),
            "{symbol}: resource-result-type drift"
        );
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
        assert_eq!(
            retention_spelling(compiled.result_retention),
            expected.result_retention,
            "{symbol}: result-retention drift"
        );
    }
}

#[test]
fn string_to_bytes_transfer_row_does_not_drift() {
    let document = classification_toml();
    let source = document
        .ownership
        .contracts
        .iter()
        .find(|row| row.symbol == "hew_string_to_bytes")
        .expect("TOML string-to-bytes ownership row");
    assert_eq!(source.result, "fresh");
    assert_eq!(source.params, ["borrow"]);
    assert_eq!(source.release_symbol, "hew_bytes_drop");
    assert_eq!(source.discharge_depth, "shallow");
    assert_eq!(source.result_retention, "transferred");

    let (_, compiled) = FFI_OWNERSHIP_CONTRACTS
        .iter()
        .find(|(symbol, _)| *symbol == "hew_string_to_bytes")
        .expect("compiled string-to-bytes ownership row");
    assert_eq!(compiled.result, ExternResultOwnership::Fresh);
    assert_eq!(compiled.params, &[ExternParamOwnership::Borrow]);
    assert_eq!(compiled.release_symbol, "hew_bytes_drop");
    assert_eq!(compiled.discharge_depth, ReleaseDischargeDepth::Shallow);
    assert_eq!(
        compiled.result_retention,
        ExternResultRetention::Transferred
    );
}
