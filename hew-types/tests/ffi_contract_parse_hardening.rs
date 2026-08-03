//! Fail-closed tests for typed owned-resource result contracts.
//!
//! The included source is the exact parser used by `hew-types/build.rs`, so
//! these tests exercise the generated-table admission boundary rather than a
//! parallel test parser.

#![allow(dead_code, reason = "the included parser carries build-only helpers")]

include!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../hew-mir/build_support/ownership_contract_parse.rs"
));

const VALID_SYNTHETIC_RESOURCE: &str = r#"
[[ownership.contracts]]
symbol = "example_socket_close"
result = "none"
params = ["consume"]
resource-param-types = ["example.io.Socket"]
release-symbol = ""
discharge-depth = "none"

[[ownership.contracts]]
symbol = "example_socket_open"
result = "fresh"
params = []
resource-result-type = "example.io.Socket"
release-symbol = "example_socket_close"
discharge-depth = "shallow"
result-retention = "resource-transfer"
result-retention-basis = "synthetic constructor returns one close token"
"#;

const VALID_SYNTHETIC_BYTES: &str = r#"
[[ownership.contracts]]
symbol = "example_string_to_bytes"
result = "fresh"
params = ["borrow"]
release-symbol = "example_bytes_drop"
discharge-depth = "shallow"
result-retention = "transferred"
"#;

#[test]
fn qualified_resource_result_joins_exact_consuming_release() {
    let rows = parse_ownership_contracts(VALID_SYNTHETIC_RESOURCE);
    assert_eq!(
        rows["example_socket_open"].resource_result_type.as_deref(),
        Some("example.io.Socket")
    );
    assert_eq!(
        rows["example_socket_open"].release_symbol,
        "example_socket_close"
    );
}

#[test]
#[should_panic(expected = "must be a qualified nominal")]
fn unqualified_resource_result_fails_closed() {
    let _ =
        parse_ownership_contracts(&VALID_SYNTHETIC_RESOURCE.replace("example.io.Socket", "Socket"));
}

#[test]
#[should_panic(expected = "resource-result-type must be quoted")]
fn malformed_resource_result_fails_closed() {
    let _ = parse_ownership_contracts(&VALID_SYNTHETIC_RESOURCE.replace(
        "resource-result-type = \"example.io.Socket\"",
        "resource-result-type = 42",
    ));
}

#[test]
#[should_panic(expected = "requires an owned result")]
fn resource_result_on_borrowed_row_fails_closed() {
    let _ = parse_ownership_contracts(&VALID_SYNTHETIC_RESOURCE.replace(
        "symbol = \"example_socket_open\"\nresult = \"fresh\"",
        "symbol = \"example_socket_open\"\nresult = \"borrowed\"",
    ));
}

#[test]
#[should_panic(expected = "names missing release contract")]
fn resource_result_with_missing_release_row_fails_closed() {
    let _ = parse_ownership_contracts(&VALID_SYNTHETIC_RESOURCE.replace(
        "release-symbol = \"example_socket_close\"\ndischarge-depth = \"shallow\"",
        "release-symbol = \"example_socket_drop\"\ndischarge-depth = \"shallow\"",
    ));
}

#[test]
#[should_panic(expected = "must consume exactly one example.io.Socket")]
fn resource_result_with_mismatched_release_nominal_fails_closed() {
    let _ = parse_ownership_contracts(&VALID_SYNTHETIC_RESOURCE.replacen(
        "resource-param-types = [\"example.io.Socket\"]",
        "resource-param-types = [\"example.io.Pipe\"]",
        1,
    ));
}

#[test]
fn resource_result_without_retention_proof_stays_unmeasured() {
    let rows = parse_ownership_contracts(
        &VALID_SYNTHETIC_RESOURCE
            .replace("result-retention = \"resource-transfer\"\n", "")
            .replace(
                "result-retention-basis = \"synthetic constructor returns one close token\"\n",
                "",
            ),
    );
    assert!(rows["example_socket_open"].result_retention.is_empty());
}

#[test]
#[should_panic(expected = "requires a non-empty basis")]
fn resource_transfer_without_body_basis_fails_closed() {
    let _ = parse_ownership_contracts(&VALID_SYNTHETIC_RESOURCE.replace(
        "result-retention-basis = \"synthetic constructor returns one close token\"\n",
        "",
    ));
}

#[test]
fn transferred_non_resource_result_is_preserved_by_the_build_parser() {
    let rows = parse_ownership_contracts(VALID_SYNTHETIC_BYTES);
    assert_eq!(
        rows["example_string_to_bytes"].result_retention,
        "transferred"
    );
}

#[test]
#[should_panic(expected = "unknown result-retention")]
fn malformed_non_resource_retention_fails_closed() {
    let _ = parse_ownership_contracts(&VALID_SYNTHETIC_BYTES.replace(
        "result-retention = \"transferred\"",
        "result-retention = \"callee-keeps-alias\"",
    ));
}
