#[test]
fn supervisor_wiring_rejects_duplicate_keys_and_clauses() {
    for clause in [
        "wired_to: { backend: ledger, backend: spare }",
        "wired_to: {} wired_to: { backend: ledger }",
    ] {
        let source = format!("supervisor Shop {{ child checkout: Checkout {clause}; }}");
        let parsed = hew_parser::parse(&source);
        assert!(!parsed.errors.is_empty(), "{:#?}", parsed.errors);
    }
}
