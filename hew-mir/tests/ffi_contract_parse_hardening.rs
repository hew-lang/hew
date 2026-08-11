//! Hardening proof for the build-script `[[ownership.contracts]]` parser.
//!
//! Includes the EXACT parser `hew-types/build.rs` compiles
//! (`build_support/ownership_contract_parse.rs`) and proves that a trailing
//! foreign table cannot pollute the final contract: any `[header]` /
//! `[[header]]` line other than `[[ownership.contracts]]` closes out the
//! contract being accumulated and enters a skip state.

#![allow(dead_code, reason = "the included parser carries build-only helpers")]

include!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/build_support/ownership_contract_parse.rs"
));

const CONTRACT_THEN_FOREIGN_TABLE: &str = r#"
[[ownership.contracts]]
symbol = "hew_alpha"
result = "none"
params = ["consume"]
release-symbol = ""
discharge-depth = "none"

# A trailing foreign table reusing the contract key spellings. Nothing below
# may reach hew_alpha or mint a new contract.
[foreign.table]
symbol = "hew_polluter"
result = "fresh"
params = ["borrow"]
release-symbol = "hew_string_drop"
discharge-depth = "shallow"
"#;

#[test]
fn trailing_foreign_table_cannot_alter_the_last_contract() {
    let contracts = parse_ownership_contracts(CONTRACT_THEN_FOREIGN_TABLE);
    assert_eq!(contracts.len(), 1, "foreign table must not mint a contract");
    let row = contracts
        .get("hew_alpha")
        .expect("hew_alpha contract parsed");
    // The open hew_alpha entry was closed out AT the foreign header with its
    // own values intact — none of the foreign keys were absorbed.
    assert_eq!(row.result, "none");
    assert_eq!(row.params, ["consume"]);
    assert_eq!(row.release_symbol, "");
    assert_eq!(row.discharge_depth, "none");
    assert!(!contracts.contains_key("hew_polluter"));
}

const FOREIGN_TABLE_BETWEEN_CONTRACTS: &str = r#"
[[ownership.contracts]]
symbol = "hew_alpha"
result = "none"
params = []
release-symbol = ""
discharge-depth = "none"

[interloper]
result = "fresh"

[[ownership.contracts]]
symbol = "hew_beta"
result = "fresh"
params = [
  "borrow",
  "borrow",
]
release-symbol = "hew_string_drop"
discharge-depth = "shallow"
"#;

#[test]
fn foreign_table_between_contracts_is_skipped_and_wrapped_params_parse() {
    let contracts = parse_ownership_contracts(FOREIGN_TABLE_BETWEEN_CONTRACTS);
    assert_eq!(contracts.len(), 2);
    assert_eq!(contracts["hew_alpha"].result, "none");
    // The interloper's `result = "fresh"` did not leak into hew_alpha, and
    // parsing resumed cleanly at the next contract header — including the
    // formatter-wrapped multi-line params array.
    assert_eq!(contracts["hew_beta"].result, "fresh");
    assert_eq!(contracts["hew_beta"].params, ["borrow", "borrow"]);
}

#[test]
#[should_panic(expected = "resource-param-types for hew_short must have one entry per parameter")]
fn short_typed_resource_row_fails_closed() {
    let _ = parse_ownership_contracts(
        r#"
[[ownership.contracts]]
symbol = "hew_short"
result = "none"
params = ["borrow", "borrow"]
resource-param-types = ["std.net.Connection"]
release-symbol = ""
discharge-depth = "none"
"#,
    );
}

#[test]
#[should_panic(expected = "unknown result-retention for hew_wrong_retention")]
fn wrong_result_retention_fails_closed() {
    let _ = parse_ownership_contracts(
        r#"
[[ownership.contracts]]
symbol = "hew_wrong_retention"
result = "fresh"
params = []
release-symbol = "hew_string_drop"
discharge-depth = "shallow"
result-retention = "retained"
"#,
    );
}

#[test]
fn shared_refcount_retention_requires_a_retained_result() {
    let rows = parse_ownership_contracts(
        r#"
[[ownership.contracts]]
symbol = "hew_clone"
result = "retained"
params = ["borrow"]
release-symbol = "hew_string_drop"
discharge-depth = "shallow"
result-retention = "shared-refcount"
"#,
    );
    assert_eq!(rows["hew_clone"].result_retention, "shared-refcount");
}

#[test]
#[should_panic(expected = "requires a retained result")]
fn shared_refcount_retention_rejects_a_fresh_result() {
    let _ = parse_ownership_contracts(
        r#"
[[ownership.contracts]]
symbol = "hew_fresh"
result = "fresh"
params = []
release-symbol = "hew_string_drop"
discharge-depth = "shallow"
result-retention = "shared-refcount"
"#,
    );
}
