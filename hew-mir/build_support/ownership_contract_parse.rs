// `[[ownership.contracts]]` hand parser, shared between `hew-types/build.rs`
// (via `include!`) and the parser-hardening test in
// `tests/ffi_contract_parse_hardening.rs` so the exact build-time code is
// what the test exercises. Types are referred to fully-qualified (no `use`
// items) because this file is textually included into hosts that carry their
// own imports.

/// One `[[ownership.contracts]]` row from the FFI classification TOML.
struct ContractRow {
    result: String,
    params: Vec<String>,
    /// For a resource-valued ABI parameter, the one nominal handle this row
    /// audits. Empty entries denote non-resource positions.  The field is
    /// deliberately optional for the existing non-resource ABI surface; an
    /// absent typed fact is never interpreted as a resource borrow.
    resource_param_types: Vec<String>,
    /// Fully-qualified source nominal for an independently-owned opaque
    /// result. Absence means this row does not mint a source resource
    /// lifecycle candidate.
    resource_result_type: Option<String>,
    release_symbol: String,
    discharge_depth: String,
    /// The RETENTION answer for an owned result: `"transferred"` for an
    /// exclusive allocation handoff, `"shared-refcount"` for an independently
    /// balanced retained alias, `"resource-transfer"` for an opaque close
    /// authority, and empty when the question has not been answered.
    /// Empty is the fail-closed default — see the `result-retention` section
    /// of `scripts/jit-symbol-classification.toml`.
    result_retention: String,
    /// Runtime-body evidence for an opaque `resource-transfer`. Kept in the
    /// source table for auditability; it is validated but need not enter the
    /// generated compiler table.
    result_retention_basis: String,
}

fn quoted_value(line: &str) -> Option<&str> {
    let (_, value) = line.split_once('=')?;
    let value = value.trim();
    value.strip_prefix('"')?.strip_suffix('"')
}

/// Parse a `params = [...]` array body. The TOML formatter wraps long arrays
/// across lines, so the caller accumulates lines until the closing `]` and
/// hands the joined body here.
fn quoted_list(body: &str) -> Vec<String> {
    let (_, value) = body.split_once('=').expect("params line must carry `=`");
    let value = value.trim();
    let inner = value
        .strip_prefix('[')
        .and_then(|v| v.strip_suffix(']'))
        .expect("params must be a bracketed array");
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

/// Fail-closed schema check for one accumulated row.
///
/// Every axis is validated against a closed vocabulary — an unknown spelling
/// aborts the build rather than degrading to a default — and the
/// owned-result couplings from `verify-ffi-symbols.py` are re-checked here so
/// the generated compiler table can never carry a row the out-of-band
/// validator would reject.
fn validate_contract_row(symbol: &str, row: &ContractRow) {
    assert!(
        ["fresh", "retained", "borrowed", "none"].contains(&row.result.as_str()),
        "unknown ownership result for {symbol}: {}",
        row.result
    );
    // The generated table interpolates these three fields straight into
    // `"..."` Rust string literals (`generate_ffi_ownership_table`); a `"` or
    // `\` in an interpolated field would close the literal early or start an
    // escape sequence, corrupting the generated source rather than failing
    // the build cleanly. Reject them here instead.
    for resource_type in &row.resource_param_types {
        assert!(
            !resource_type.contains('"') && !resource_type.contains('\\'),
            "resource-param-types for {symbol} must not contain `\"` or `\\`: {resource_type}"
        );
    }
    if let Some(resource_type) = &row.resource_result_type {
        assert!(
            !resource_type.contains('"') && !resource_type.contains('\\'),
            "resource-result-type for {symbol} must not contain `\"` or `\\`: {resource_type}"
        );
    }
    assert!(
        !row.release_symbol.contains('"') && !row.release_symbol.contains('\\'),
        "release-symbol for {symbol} must not contain `\"` or `\\`: {}",
        row.release_symbol
    );
    for param in &row.params {
        assert!(
            ["borrow", "consume", "retain"].contains(&param.as_str()),
            "unknown param ownership for {symbol}: {param}"
        );
    }
    if !row.resource_param_types.is_empty() {
        assert_eq!(
            row.resource_param_types.len(),
            row.params.len(),
            "resource-param-types for {symbol} must have one entry per parameter"
        );
        for (index, resource_type) in row.resource_param_types.iter().enumerate() {
            if !resource_type.is_empty() {
                assert!(
                    ["borrow", "consume"].contains(&row.params[index].as_str()),
                    "typed resource parameter {index} for {symbol} must be an audited borrow or consume"
                );
                assert!(
                    resource_type.contains('.'),
                    "resource parameter type for {symbol} must be a qualified nominal: {resource_type}"
                );
            }
        }
    }
    if let Some(resource_type) = &row.resource_result_type {
        assert!(
            !resource_type.is_empty() && resource_type.contains('.'),
            "resource result type for {symbol} must be a qualified nominal: {resource_type}"
        );
        assert!(
            matches!(row.result.as_str(), "fresh" | "retained"),
            "resource result type for {symbol} requires an owned result"
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
    // The RETENTION axis. Absence is the fail-closed answer "not established",
    // so only measured positive spellings are allowed, and they are meaningful
    // only about an allocation the caller was actually given.
    assert!(
        ["", "resource-transfer", "shared-refcount", "transferred"]
            .contains(&row.result_retention.as_str()),
        "unknown result-retention for {symbol}: {}",
        row.result_retention
    );
    assert!(
        row.result_retention.is_empty() || matches!(row.result.as_str(), "fresh" | "retained"),
        "result-retention for {symbol} is meaningless without an owned result"
    );
    assert!(
        row.result_retention != "shared-refcount" || row.result == "retained",
        "shared-refcount result-retention for {symbol} requires a retained result"
    );
    assert!(
        row.result_retention != "resource-transfer" || row.resource_result_type.is_some(),
        "resource-transfer result-retention for {symbol} requires a resource result type"
    );
    assert!(
        row.result_retention != "resource-transfer"
            || !row.result_retention_basis.trim().is_empty(),
        "resource-transfer result-retention for {symbol} requires a non-empty basis"
    );
    assert!(
        row.result_retention == "resource-transfer" || row.result_retention_basis.is_empty(),
        "result-retention basis for {symbol} is only meaningful for resource-transfer"
    );
}

/// Validate the cross-row disposer edge for every typed resource result.
///
/// This is deliberately a graph check over the same contract table rather
/// than a second disposer registry: the producer's `release-symbol` must name
/// a row that consumes exactly one position of the same qualified nominal and
/// produces no owner.
fn validate_contract_graph(contracts: &std::collections::BTreeMap<String, ContractRow>) {
    for (symbol, row) in contracts {
        let Some(resource_type) = row.resource_result_type.as_deref() else {
            continue;
        };
        let release = contracts.get(&row.release_symbol).unwrap_or_else(|| {
            panic!(
                "resource result type for {symbol} names missing release contract {}",
                row.release_symbol
            )
        });
        assert_eq!(
            release.result, "none",
            "release contract {} for {symbol} must produce no owned result",
            row.release_symbol
        );
        let matching_positions = release
            .resource_param_types
            .iter()
            .enumerate()
            .filter(|(index, candidate)| {
                candidate.as_str() == resource_type
                    && release.params.get(*index).is_some_and(|mode| mode == "consume")
            })
            .count();
        assert_eq!(
            matching_positions, 1,
            "release contract {} for {symbol} must consume exactly one {resource_type}",
            row.release_symbol
        );
    }
}

/// Parse the full `[[ownership.contracts]]` table from TOML source. Every
/// axis is validated against the closed schema vocabularies here
/// (fail-closed: an unknown spelling aborts rather than degrading to a
/// default), and the owned-result/release-symbol coupling from
/// `verify-ffi-symbols.py` is re-checked so the generated compiler table can
/// never carry a row the out-of-band validator would reject.
///
/// Any table header other than `[[ownership.contracts]]` closes out the
/// contract being accumulated and enters a skip state: keys inside a foreign
/// trailing table — even ones spelled `symbol =` / `result =` — must never
/// pollute the final contract.
#[expect(
    clippy::too_many_lines,
    reason = "the fail-closed hand parser keeps every accepted contract key in one visible dispatch"
)]
fn parse_ownership_contracts(
    source: &str,
) -> std::collections::BTreeMap<String, ContractRow> {
    let mut contracts = std::collections::BTreeMap::new();
    let mut current: Option<(Option<String>, ContractRow)> = None;

    let finish = |entry: Option<(Option<String>, ContractRow)>,
                  contracts: &mut std::collections::BTreeMap<String, ContractRow>| {
        if let Some((symbol, row)) = entry {
            let symbol = symbol.expect("ownership contract missing `symbol`");
            validate_contract_row(&symbol, &row);
            assert!(
                contracts.insert(symbol.clone(), row).is_none(),
                "duplicate TOML ownership contract for {symbol}"
            );
        }
    };

    let mut lines = source.lines();
    while let Some(line) = lines.next() {
        let line = line.trim();
        if line == "[[ownership.contracts]]" {
            finish(current.take(), &mut contracts);
            current = Some((
                None,
                ContractRow {
                    result: String::new(),
                    params: Vec::new(),
                    resource_param_types: Vec::new(),
                    resource_result_type: None,
                    release_symbol: String::new(),
                    discharge_depth: String::new(),
                    result_retention: String::new(),
                    result_retention_basis: String::new(),
                },
            ));
            continue;
        }
        if line.starts_with('[') {
            // A DIFFERENT table header ([header] or [[header]]): the contract
            // section is over for the current entry. Close it out and skip
            // until the next [[ownership.contracts]] header, so a foreign
            // table's keys cannot be absorbed into the last contract.
            finish(current.take(), &mut contracts);
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
            // Accumulate a formatter-wrapped multi-line array until its `]`.
            let mut body = line.to_owned();
            while !body.trim_end().ends_with(']') {
                let continuation = lines
                    .next()
                    .expect("unterminated params array in ownership contract");
                body.push(' ');
                body.push_str(continuation.trim());
            }
            row.params = quoted_list(&body);
        } else if line.starts_with("resource-param-types =") {
            // Keep this parallel to `params`: the empty-string slots pin the
            // non-resource ABI positions, so an index can never be shifted
            // into a neighbouring resource fact.
            let mut body = line.to_owned();
            while !body.trim_end().ends_with(']') {
                let continuation = lines
                    .next()
                    .expect("unterminated resource-param-types array in ownership contract");
                body.push(' ');
                body.push_str(continuation.trim());
            }
            row.resource_param_types = quoted_list(&body);
        } else if line.starts_with("resource-result-type =") {
            row.resource_result_type = Some(
                quoted_value(line)
                    .expect("contract resource-result-type must be quoted")
                    .to_owned(),
            );
        } else if line.starts_with("release-symbol =") {
            quoted_value(line)
                .expect("contract release-symbol must be quoted")
                .clone_into(&mut row.release_symbol);
        } else if line.starts_with("discharge-depth =") {
            quoted_value(line)
                .expect("contract discharge-depth must be quoted")
                .clone_into(&mut row.discharge_depth);
        } else if line.starts_with("result-retention =") {
            quoted_value(line)
                .expect("contract result-retention must be quoted")
                .clone_into(&mut row.result_retention);
        } else if line.starts_with("result-retention-basis =") {
            quoted_value(line)
                .expect("contract result-retention-basis must be quoted")
                .clone_into(&mut row.result_retention_basis);
        }
    }
    finish(current.take(), &mut contracts);
    assert!(
        !contracts.is_empty(),
        "classification TOML must declare ownership contracts"
    );
    validate_contract_graph(&contracts);
    contracts
}
