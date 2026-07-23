// `[[ownership.contracts]]` hand parser, shared between `hew-mir/build.rs`
// (via `include!`) and the parser-hardening test in
// `tests/ffi_contract_parse_hardening.rs` so the exact build-time code is
// what the test exercises. Types are referred to fully-qualified (no `use`
// items) because this file is textually included into hosts that carry their
// own imports.

/// One `[[ownership.contracts]]` row from the FFI classification TOML.
struct ContractRow {
    result: String,
    params: Vec<String>,
    release_symbol: String,
    discharge_depth: String,
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
fn parse_ownership_contracts(
    source: &str,
) -> std::collections::BTreeMap<String, ContractRow> {
    let mut contracts = std::collections::BTreeMap::new();
    let mut current: Option<(Option<String>, ContractRow)> = None;

    let finish = |entry: Option<(Option<String>, ContractRow)>,
                  contracts: &mut std::collections::BTreeMap<String, ContractRow>| {
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
                    release_symbol: String::new(),
                    discharge_depth: String::new(),
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
