//! The one ratchet ledger and the one checker that reads it.
//!
//! `tests/expected-failures.tsv` is the single expected-failure ledger for
//! every ratcheted suite (nextest, core-acceptance, the compiled-Hew suite
//! and the corpus). One row: `suite  platforms  id  expect  issue  reason`,
//! sorted by `(suite, id)`, no section headers. `ratchet check` compares a
//! suite's observed failures against its rows and refuses a new unlisted
//! failure, a row whose id no longer exists, or a row with the wrong
//! `expect` kind. A row whose test now passes is reported for ledger
//! cleanup, never a blocking failure (D555 amendment): a lane that fixes a
//! test deletes its row in the same change, but the ratchet itself never
//! goes red over a recovery. `ratchet issues` checks every `#N` against
//! GitHub.
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use quick_xml::events::{BytesStart, Event};
use quick_xml::reader::Reader;
use serde::Deserialize;

use crate::Result;

pub(crate) const LEDGER_PATH: &str = "tests/expected-failures.tsv";
const PLATFORMS: [&str; 4] = ["linux", "macos", "windows", "freebsd"];

/// The kind of non-pass outcome a row pins. `nextest` rows use `Failure`
/// and `Timeout` only; `hew-suite` and `corpus` rows use the rest.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum Expect {
    Failure,
    Timeout,
    Compile,
    Runtime,
    Diagnostic(String),
}

impl Expect {
    fn parse(value: &str) -> Result<Self> {
        Ok(match value {
            "failure" => Self::Failure,
            "timeout" => Self::Timeout,
            "compile" => Self::Compile,
            "runtime" => Self::Runtime,
            other => match other.strip_prefix("diagnostic:") {
                Some(code) if !code.is_empty() && code.starts_with("E_") => {
                    Self::Diagnostic(code.to_string())
                }
                _ => return Err(format!("unknown expect kind {other:?}")),
            },
        })
    }

    fn label(&self) -> String {
        match self {
            Self::Failure => "failure".to_string(),
            Self::Timeout => "timeout".to_string(),
            Self::Compile => "compile".to_string(),
            Self::Runtime => "runtime".to_string(),
            Self::Diagnostic(code) => format!("diagnostic:{code}"),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Row {
    pub suite: String,
    pub platforms: Vec<String>,
    pub id: String,
    pub expect: Expect,
    pub issue: String,
    pub reason: String,
}

impl Row {
    fn applies_to(&self, platform: &str) -> bool {
        self.platforms
            .iter()
            .any(|item| item == "*" || item == platform)
    }
}

/// Parse the whole unified ledger: one file for every suite, sorted
/// globally by `(suite, id)`, no section headers.
pub(crate) fn parse_ledger(text: &str) -> Result<Vec<Row>> {
    let mut rows = Vec::new();
    let mut selected: std::collections::BTreeSet<(String, &'static str, String)> =
        std::collections::BTreeSet::new();
    let mut last: Option<(String, String)> = None;
    for (index, line) in text.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let fields: Vec<&str> = line.split('\t').collect();
        if fields.len() != 6 || fields.iter().any(|field| field.trim().is_empty()) {
            return Err(format!(
                "{LEDGER_PATH}:{}: row must have six non-empty tab-separated fields",
                index + 1
            ));
        }
        let suite = fields[0].to_string();
        let platforms: Vec<String> = fields[1].split(',').map(str::to_string).collect();
        if platforms.iter().any(std::string::String::is_empty) {
            return Err(format!(
                "{LEDGER_PATH}:{}: row has an empty platform",
                index + 1
            ));
        }
        if platforms
            .iter()
            .any(|item| item != "*" && !PLATFORMS.contains(&item.as_str()))
        {
            return Err(format!(
                "{LEDGER_PATH}:{}: row has an unknown platform",
                index + 1
            ));
        }
        let id = fields[2].to_string();
        let expect = Expect::parse(fields[3])
            .map_err(|err| format!("{LEDGER_PATH}:{}: {err}", index + 1))?;
        let issue = fields[4].to_string();
        let valid_issue = issue
            .strip_prefix('#')
            .is_some_and(|number| !number.is_empty() && number.chars().all(|c| c.is_ascii_digit()));
        if !valid_issue {
            return Err(format!(
                "{LEDGER_PATH}:{}: issue must be '#<number>', got {issue:?}",
                index + 1
            ));
        }
        let reason = fields[5].to_string();

        let key = (suite.clone(), id.clone());
        if let Some(previous) = &last {
            if key < *previous {
                return Err(format!(
                    "{LEDGER_PATH}:{}: not sorted by (suite, id): {suite:?}/{id:?} follows {previous:?}",
                    index + 1
                ));
            }
        }
        last = Some(key);

        for candidate in PLATFORMS {
            if (platforms.iter().any(|item| item == "*")
                || platforms.iter().any(|item| item == candidate))
                && !selected.insert((suite.clone(), candidate, id.clone()))
            {
                return Err(format!(
                    "{LEDGER_PATH}:{}: {suite}/{id} is already selected for {candidate}",
                    index + 1
                ));
            }
        }

        rows.push(Row {
            suite,
            platforms,
            id,
            expect,
            issue,
            reason,
        });
    }
    Ok(rows)
}

fn load_ledger(root: &Path) -> Result<Vec<Row>> {
    let path = root.join(LEDGER_PATH);
    let text =
        fs::read_to_string(&path).map_err(|err| format!("read {}: {err}", path.display()))?;
    parse_ledger(&text)
}

/// Rows for one suite on one platform, keyed by id. `core-acceptance` calls
/// this in process; nothing here re-implements the parser.
pub(crate) fn rows_for(root: &Path, suite: &str, platform: &str) -> Result<BTreeMap<String, Row>> {
    if !PLATFORMS.contains(&platform) {
        return Err(format!("unsupported platform {platform:?}"));
    }
    let mut out = BTreeMap::new();
    for row in load_ledger(root)? {
        if row.suite == suite
            && row.applies_to(platform)
            && out.insert(row.id.clone(), row).is_some()
        {
            return Err(format!(
                "{LEDGER_PATH}: duplicate row for {suite}/{platform}"
            ));
        }
    }
    Ok(out)
}

pub(super) fn run(args: &[String]) -> Result<()> {
    match args.first().map(String::as_str) {
        Some("check") => run_check(&args[1..]),
        Some("issues") => run_issues(&args[1..]),
        Some("rows") => run_rows(&args[1..]),
        Some("--help" | "-h") | None => {
            println!("{}", usage());
            Ok(())
        }
        Some(other) => Err(format!(
            "unknown ratchet subcommand: {other}\n\n{}",
            usage()
        )),
    }
}

fn usage() -> &'static str {
    "usage: cargo run -p xtask -- ratchet <check|issues|rows> [options]\n\n\
     check --suite <nextest|hew-suite|corpus> --platform <p> --output <report.xml>\n\
       nextest:            --junit <raw.xml> --runner-exit <code> \\\n\
                            [--full-inventory <f> --selected-inventory <f>]\n\
       hew-suite | corpus: --results <file>   (lines: id<TAB>outcome[<TAB>code])\n\
     issues                verify every #N in the ledger resolves on GitHub\n\
     rows --suite <s> --platform <p>\n\
       print that suite's rows in the shape its shell/python driver already\n\
       parses: hew-suite prints '<id> <kind>', corpus prints '<path>[ <code>]'"
}

/// Print a suite's rows in the legacy shape `corpus-ratchet.sh` and
/// `compiled-hew-shards.py` already parse, so those drivers keep their own
/// comparison logic and read the one ledger through this single adapter.
fn run_rows(args: &[String]) -> Result<()> {
    let mut suite = None;
    let mut platform = None;
    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "--suite" => {
                suite = args.get(index + 1);
                index += 2;
            }
            "--platform" => {
                platform = args.get(index + 1);
                index += 2;
            }
            other => return Err(format!("unknown ratchet rows option: {other}")),
        }
    }
    let suite = suite.ok_or("rows requires --suite")?;
    let platform = platform.ok_or("rows requires --platform")?;
    let root = crate::workspace_root()?;
    let rows = rows_for(&root, suite, platform)?;
    for (id, row) in rows {
        match suite.as_str() {
            "hew-suite" => println!("{id} {}", row.expect.label()),
            "corpus" => match &row.expect {
                Expect::Diagnostic(code) => println!("{id} {code}"),
                _ => println!("{id}"),
            },
            other => return Err(format!("rows has no legacy shape for suite {other:?}")),
        }
    }
    Ok(())
}

// ── check ──────────────────────────────────────────────────────────────

#[derive(Debug)]
struct CheckOptions {
    suite: String,
    platform: String,
    output: PathBuf,
    junit: Option<PathBuf>,
    runner_exit: Option<i32>,
    full_inventory: Option<PathBuf>,
    selected_inventory: Option<PathBuf>,
    results: Option<PathBuf>,
}

fn parse_check_options(args: &[String]) -> Result<CheckOptions> {
    let mut values: BTreeMap<&str, &str> = BTreeMap::new();
    let mut index = 0;
    while index < args.len() {
        let flag = args[index].as_str();
        if !matches!(
            flag,
            "--suite"
                | "--platform"
                | "--output"
                | "--junit"
                | "--runner-exit"
                | "--full-inventory"
                | "--selected-inventory"
                | "--results"
        ) {
            return Err(format!("unknown ratchet check option: {flag}"));
        }
        let value = args
            .get(index + 1)
            .ok_or_else(|| format!("{flag} requires a value"))?;
        if values.insert(flag, value.as_str()).is_some() {
            return Err(format!("duplicate option: {flag}"));
        }
        index += 2;
    }
    let get = |flag| {
        values
            .get(flag)
            .copied()
            .ok_or_else(|| format!("missing {flag}"))
    };
    let suite = get("--suite")?.to_string();
    let runner_exit = values
        .get("--runner-exit")
        .map(|value| {
            value
                .parse()
                .map_err(|_| "--runner-exit must be an integer".to_string())
        })
        .transpose()?;
    Ok(CheckOptions {
        suite,
        platform: get("--platform")?.to_string(),
        output: get("--output")?.into(),
        junit: values.get("--junit").map(|value| PathBuf::from(*value)),
        runner_exit,
        full_inventory: values
            .get("--full-inventory")
            .map(|value| PathBuf::from(*value)),
        selected_inventory: values
            .get("--selected-inventory")
            .map(|value| PathBuf::from(*value)),
        results: values.get("--results").map(|value| PathBuf::from(*value)),
    })
}

fn run_check(args: &[String]) -> Result<()> {
    let options = parse_check_options(args)?;
    let root = crate::workspace_root()?;
    let expected = rows_for(&root, &options.suite, &options.platform)?;

    let evaluation = if options.suite == "nextest" {
        let junit = options
            .junit
            .as_ref()
            .ok_or("suite nextest requires --junit")?;
        let runner_exit = options
            .runner_exit
            .ok_or("suite nextest requires --runner-exit")?;
        if same_path(junit, &options.output) {
            return Err("ratchet output must differ from raw nextest JUnit".to_string());
        }
        evaluate_nextest(junit, &expected, runner_exit, &options)?
    } else {
        let results = options
            .results
            .as_ref()
            .ok_or_else(|| format!("suite {} requires --results", options.suite))?;
        evaluate_results(results, &expected)?
    };

    write_report(&options.output, &evaluation)?;
    if evaluation.blocking() {
        println!(
            "ratchet {}: {} known non-pass outcome(s) matched",
            options.suite,
            evaluation.matched.len()
        );
        for recovery in &evaluation.recoveries {
            println!("ratchet recovery (ledger cleanup, non-blocking): {recovery}");
        }
        Ok(())
    } else {
        Err(evaluation
            .failures
            .iter()
            .chain(&evaluation.errors)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n"))
    }
}

#[derive(Debug, Default)]
struct Evaluation {
    matched: Vec<String>,
    /// A listed row whose test now passes. Reported only: the D555
    /// amendment makes a recovery non-blocking everywhere. The row still
    /// needs deleting by hand.
    recoveries: Vec<String>,
    failures: Vec<String>,
    errors: Vec<String>,
}

impl Evaluation {
    fn blocking(&self) -> bool {
        self.failures.is_empty() && self.errors.is_empty()
    }
}

fn same_path(left: &Path, right: &Path) -> bool {
    left == right
        || fs::canonicalize(left)
            .ok()
            .zip(fs::canonicalize(right).ok())
            .is_some_and(|(left, right)| left == right)
}

// ── results-file suites (hew-suite, corpus) ───────────────────────────────
//
// A results file lists only the ids that currently fail, one per line:
// `id<TAB>outcome[<TAB>code]`. An id absent from the file is assumed to
// have passed. This mirrors the set-difference model corpus-ratchet.sh and
// compiled-hew-shards.py already run.

fn parse_results(text: &str) -> Result<BTreeMap<String, Expect>> {
    let mut out = BTreeMap::new();
    for (index, line) in text.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let fields: Vec<&str> = line.split('\t').collect();
        let (id, outcome, code) = match fields.as_slice() {
            [id, outcome] => (*id, *outcome, None),
            [id, outcome, code] => (*id, *outcome, Some(*code)),
            _ => {
                return Err(format!(
                    "results line {}: expected id\\toutcome[\\tcode]",
                    index + 1
                ))
            }
        };
        let expect = match outcome {
            "failure" => Expect::Failure,
            "timeout" => Expect::Timeout,
            "compile" => Expect::Compile,
            "runtime" => Expect::Runtime,
            "diagnostic" => {
                let code = code.ok_or_else(|| {
                    format!(
                        "results line {}: diagnostic outcome requires a code",
                        index + 1
                    )
                })?;
                Expect::Diagnostic(code.to_string())
            }
            other => {
                return Err(format!(
                    "results line {}: unknown outcome {other:?}",
                    index + 1
                ))
            }
        };
        if out.insert(id.to_string(), expect).is_some() {
            return Err(format!("results line {}: duplicate id {id:?}", index + 1));
        }
    }
    Ok(out)
}

fn evaluate_results(results_path: &Path, expected: &BTreeMap<String, Row>) -> Result<Evaluation> {
    let text = fs::read_to_string(results_path)
        .map_err(|err| format!("read {}: {err}", results_path.display()))?;
    let observed = parse_results(&text)?;

    let mut result = Evaluation::default();
    for (id, actual) in &observed {
        match expected.get(id) {
            None => result
                .failures
                .push(format!("unexpected {}: {id}", actual.label())),
            Some(row) if row.expect == *actual => {
                result.matched.push(format!("{id} ({})", row.reason));
            }
            Some(row) => result.failures.push(format!(
                "{id} changed from {} to {}",
                row.expect.label(),
                actual.label()
            )),
        }
    }
    for (id, row) in expected {
        if !observed.contains_key(id) {
            result.recoveries.push(format!(
                "tracked {} now passes: {id} ({})",
                row.expect.label(),
                row.reason
            ));
        }
    }
    Ok(result)
}

// ── nextest (JUnit) ────────────────────────────────────────────────────

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct Identity(String, String);

impl Identity {
    fn label(&self) -> String {
        format!("{} :: {}", self.0, self.1)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Outcome {
    Passed,
    Skipped,
    Failure,
    Timeout,
    Signal,
    Error,
}

impl Outcome {
    fn expect(self) -> Option<Expect> {
        match self {
            Self::Failure => Some(Expect::Failure),
            Self::Timeout => Some(Expect::Timeout),
            Self::Passed | Self::Skipped | Self::Signal | Self::Error => None,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct Counts {
    tests: usize,
    failures: usize,
    errors: usize,
    skipped: usize,
}

impl Counts {
    fn add(&mut self, outcome: Outcome) {
        self.tests += 1;
        match outcome {
            Outcome::Failure | Outcome::Timeout | Outcome::Signal => self.failures += 1,
            Outcome::Error => self.errors += 1,
            Outcome::Skipped => self.skipped += 1,
            Outcome::Passed => {}
        }
    }

    fn merge(&mut self, other: Self) {
        self.tests += other.tests;
        self.failures += other.failures;
        self.errors += other.errors;
        self.skipped += other.skipped;
    }
}

#[derive(Debug)]
struct Declared {
    counts: Counts,
    has_skipped: bool,
}

#[derive(Debug)]
struct Case {
    outcome: Outcome,
    infrastructure: Option<String>,
}

#[derive(Debug)]
struct Report {
    cases: BTreeMap<Identity, Case>,
    counts: Counts,
}

#[derive(Debug, Deserialize)]
struct InventoryDocument {
    #[serde(rename = "test-count")]
    test_count: usize,
    #[serde(rename = "rust-suites")]
    rust_suites: BTreeMap<String, InventorySuite>,
}

#[derive(Debug, Deserialize)]
struct InventorySuite {
    #[serde(rename = "binary-id")]
    binary_id: String,
    status: String,
    testcases: BTreeMap<String, InventoryCase>,
}

#[derive(Debug, Deserialize)]
struct InventoryCase {
    ignored: bool,
    #[serde(rename = "filter-match")]
    filter_match: InventoryFilter,
}

#[derive(Debug, Deserialize)]
struct InventoryFilter {
    status: String,
}

#[derive(Debug)]
struct FilteredInventories {
    full: std::collections::BTreeSet<Identity>,
    selected: std::collections::BTreeSet<Identity>,
}

impl FilteredInventories {
    fn new(
        full: std::collections::BTreeSet<Identity>,
        selected: std::collections::BTreeSet<Identity>,
    ) -> Result<Self> {
        if let Some(unexpected) = selected.difference(&full).next() {
            return Err(format!(
                "selected nextest inventory contains a test absent from the full inventory: {}",
                unexpected.label()
            ));
        }
        Ok(Self { full, selected })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Role {
    Root,
    Suite,
    Case,
    Status,
    Other,
}

#[derive(Debug)]
struct Suite {
    name: String,
    declared: Declared,
    actual: Counts,
}

#[derive(Debug)]
struct PendingCase {
    identity: Identity,
    outcome: Outcome,
    infrastructure: Option<String>,
    has_status: bool,
}

#[derive(Debug, Default)]
struct Scanner {
    stack: Vec<Role>,
    root: Option<Declared>,
    root_closed: bool,
    suite: Option<Suite>,
    case: Option<PendingCase>,
    cases: BTreeMap<Identity, Case>,
    actual: Counts,
}

fn evaluate_nextest(
    junit: &Path,
    expected: &BTreeMap<String, Row>,
    runner_exit: i32,
    options: &CheckOptions,
) -> Result<Evaluation> {
    let xml =
        fs::read_to_string(junit).map_err(|err| format!("read {}: {err}", junit.display()))?;
    let report = scan(&xml)?;
    let mut by_identity: BTreeMap<Identity, &Row> = BTreeMap::new();
    for row in expected.values() {
        let Some((suite, test)) = row.id.rsplit_once("::") else {
            return Err(format!(
                "{LEDGER_PATH}: nextest id {:?} has no '::' separator between suite and test",
                row.id
            ));
        };
        by_identity.insert(Identity(suite.to_string(), test.to_string()), row);
    }

    let filtered_inventories = match (&options.full_inventory, &options.selected_inventory) {
        (None, None) => None,
        (Some(full_path), Some(selected_path)) => {
            let full_json = fs::read_to_string(full_path)
                .map_err(|err| format!("read {}: {err}", full_path.display()))?;
            let full = parse_inventory(&full_json, &full_path.display().to_string())?;
            let selected_json = fs::read_to_string(selected_path)
                .map_err(|err| format!("read {}: {err}", selected_path.display()))?;
            let selected = parse_inventory(&selected_json, &selected_path.display().to_string())?;
            Some(FilteredInventories::new(full, selected)?)
        }
        _ => {
            return Err(
                "--full-inventory and --selected-inventory must be provided together".to_string(),
            )
        }
    };

    let mut result = Evaluation::default();
    let mut remaining = by_identity.clone();
    for (identity, case) in &report.cases {
        let wanted = remaining.remove(identity);
        if let Some(reason) = &case.infrastructure {
            result
                .errors
                .push(format!("{}: {reason}", identity.label()));
            continue;
        }
        let Some(actual) = case.outcome.expect() else {
            if let Some(row) = wanted {
                if case.outcome == Outcome::Passed {
                    result.recoveries.push(format!(
                        "tracked {} now passes: {} ({})",
                        row.expect.label(),
                        identity.label(),
                        row.reason
                    ));
                } else {
                    result.failures.push(format!(
                        "expected {} {} ({}) but it was skipped",
                        row.expect.label(),
                        identity.label(),
                        row.reason
                    ));
                }
            }
            continue;
        };
        match wanted {
            Some(row) if row.expect == actual => {
                result
                    .matched
                    .push(format!("{} ({})", identity.label(), row.reason));
            }
            Some(row) => result.failures.push(format!(
                "{} changed from {} to {}",
                identity.label(),
                row.expect.label(),
                actual.label()
            )),
            None => result.failures.push(format!(
                "unexpected {}: {}",
                actual.label(),
                identity.label()
            )),
        }
    }
    for (identity, row) in remaining {
        if filtered_inventories.as_ref().is_some_and(|inventories| {
            inventories.full.contains(&identity) && !inventories.selected.contains(&identity)
        }) {
            continue;
        }
        result.failures.push(format!(
            "expected {} is absent: {} ({})",
            row.expect.label(),
            identity.label(),
            row.reason
        ));
    }
    let has_nonpass = report.counts.failures + report.counts.errors > 0;
    if !matches!((runner_exit, has_nonpass), (0, false) | (100, true)) {
        result.errors.push(format!(
            "runner exit {runner_exit} is incoherent with {} failure/error outcome(s)",
            report.counts.failures + report.counts.errors
        ));
    }
    Ok(result)
}

fn scan(xml: &str) -> Result<Report> {
    if xml.trim().is_empty() {
        return Err("nextest JUnit is empty".to_string());
    }
    let mut reader = Reader::from_str(xml);
    reader.config_mut().expand_empty_elements = false;
    let mut scanner = Scanner::default();
    loop {
        match reader.read_event() {
            Ok(Event::Start(event)) => scanner.open(&event, false)?,
            Ok(Event::Empty(event)) => scanner.open(&event, true)?,
            Ok(Event::End(_)) => scanner.close()?,
            Ok(Event::Text(event)) => {
                let bytes: &[u8] = event.as_ref();
                if !bytes.iter().all(u8::is_ascii_whitespace)
                    && !matches!(
                        scanner.stack.last().copied(),
                        Some(Role::Status | Role::Other)
                    )
                {
                    return Err("text is not allowed at this JUnit level".to_string());
                }
            }
            Ok(Event::CData(_) | Event::GeneralRef(_)) => {
                if !matches!(
                    scanner.stack.last().copied(),
                    Some(Role::Status | Role::Other)
                ) {
                    return Err("content is not allowed at this JUnit level".to_string());
                }
            }
            Ok(Event::Decl(_) | Event::Comment(_)) => {}
            Ok(Event::PI(_) | Event::DocType(_)) => {
                return Err("processing instructions and DTDs are not accepted".to_string());
            }
            Ok(Event::Eof) => break,
            Err(error) => return Err(format!("parse nextest JUnit: {error}")),
        }
    }
    scanner.finish()
}

impl Scanner {
    fn open(&mut self, event: &BytesStart<'_>, empty: bool) -> Result<()> {
        let tag = str_value(event.name().as_ref(), "element name")?;
        let parent = self.stack.last().copied();
        let role = match parent {
            None if tag == "testsuites" && self.root.is_none() => {
                self.root = Some(declared(event)?);
                Role::Root
            }
            Some(Role::Root) if tag == "testsuite" => {
                if self.suite.is_some() {
                    return Err("nested test suites are not supported".to_string());
                }
                self.suite = Some(Suite {
                    name: required_attr(event, "name")?,
                    declared: declared(event)?,
                    actual: Counts::default(),
                });
                Role::Suite
            }
            Some(Role::Suite) if tag == "testcase" => {
                let suite = self.suite.as_ref().ok_or("testcase has no suite")?;
                self.case = Some(PendingCase {
                    identity: Identity(suite.name.clone(), required_attr(event, "name")?),
                    outcome: Outcome::Passed,
                    infrastructure: None,
                    has_status: false,
                });
                Role::Case
            }
            Some(Role::Suite) if tag == "properties" => Role::Other,
            Some(Role::Case) if matches!(tag.as_str(), "failure" | "error" | "skipped") => {
                self.set_status(event, &tag)?;
                Role::Status
            }
            Some(Role::Case) if matches!(tag.as_str(), "flakyFailure" | "flakyError") => {
                let case = self.case.as_mut().ok_or("flaky result has no testcase")?;
                case.infrastructure = Some("nextest reported a flaky test attempt".to_string());
                Role::Other
            }
            Some(Role::Case)
                if matches!(
                    tag.as_str(),
                    "system-out" | "system-err" | "rerunFailure" | "rerunError"
                ) =>
            {
                Role::Other
            }
            Some(Role::Status | Role::Other) => Role::Other,
            _ => return Err(format!("unexpected <{tag}> in nextest JUnit")),
        };
        if empty {
            self.finish_role(role)?;
        } else {
            self.stack.push(role);
        }
        Ok(())
    }

    fn close(&mut self) -> Result<()> {
        let role = self.stack.pop().ok_or("unexpected closing element")?;
        self.finish_role(role)
    }

    fn set_status(&mut self, event: &BytesStart<'_>, tag: &str) -> Result<()> {
        let case = self.case.as_mut().ok_or("test status has no testcase")?;
        if case.has_status {
            return Err(format!(
                "{} has multiple terminal outcomes",
                case.identity.label()
            ));
        }
        case.has_status = true;
        let kind = attr(event, "type")?.unwrap_or_default();
        let message = attr(event, "message")?.unwrap_or_default();
        let detail = format!("{kind} {message}").to_ascii_lowercase();
        case.outcome = match tag {
            "skipped" => Outcome::Skipped,
            "error" => Outcome::Error,
            "failure" if kind == "test timeout" => Outcome::Timeout,
            "failure" if kind.starts_with("test abort") => Outcome::Signal,
            "failure"
                if kind == "test failure" || kind.starts_with("test failure with exit code ") =>
            {
                Outcome::Failure
            }
            "failure" => {
                case.infrastructure = Some(format!("unknown nextest failure type {kind:?}"));
                Outcome::Failure
            }
            _ => unreachable!(),
        };
        if tag == "error" {
            case.infrastructure = Some(format!("nextest execution error: {kind} {message}"));
        } else if case.outcome == Outcome::Signal {
            case.infrastructure = Some(format!("test terminated by signal: {message}"));
        } else if detail.contains("leaked handles") {
            case.infrastructure = Some("nextest reported leaked handles".to_string());
        }
        Ok(())
    }

    fn finish_role(&mut self, role: Role) -> Result<()> {
        match role {
            Role::Case => {
                let case = self.case.take().ok_or("finished testcase is missing")?;
                let suite = self
                    .suite
                    .as_mut()
                    .ok_or("finished testcase has no suite")?;
                suite.actual.add(case.outcome);
                let infrastructure = if suite.name.starts_with("@setup-script:")
                    && case.outcome != Outcome::Passed
                {
                    Some("setup script did not pass".to_string())
                } else {
                    case.infrastructure
                };
                if self
                    .cases
                    .insert(
                        case.identity.clone(),
                        Case {
                            outcome: case.outcome,
                            infrastructure,
                        },
                    )
                    .is_some()
                {
                    return Err(format!("duplicate testcase {}", case.identity.label()));
                }
            }
            Role::Suite => {
                let suite = self.suite.take().ok_or("finished suite is missing")?;
                check_counts("suite", &suite.name, &suite.declared, suite.actual)?;
                self.actual.merge(suite.actual);
            }
            Role::Root => self.root_closed = true,
            Role::Status | Role::Other => {}
        }
        Ok(())
    }

    fn finish(self) -> Result<Report> {
        if !self.stack.is_empty() || !self.root_closed {
            return Err("nextest JUnit has no complete <testsuites> root".to_string());
        }
        let root = self.root.ok_or("nextest JUnit has no <testsuites> root")?;
        check_counts("root", "testsuites", &root, self.actual)?;
        if self.actual.tests == 0 {
            return Err("nextest JUnit contains zero tests".to_string());
        }
        Ok(Report {
            cases: self.cases,
            counts: self.actual,
        })
    }
}

fn declared(event: &BytesStart<'_>) -> Result<Declared> {
    let skipped = attr(event, "skipped")?.or(attr(event, "disabled")?);
    Ok(Declared {
        counts: Counts {
            tests: count_attr(event, "tests")?,
            failures: count_attr(event, "failures")?,
            errors: count_attr(event, "errors")?,
            skipped: skipped
                .as_deref()
                .map(parse_count)
                .transpose()?
                .unwrap_or(0),
        },
        has_skipped: skipped.is_some(),
    })
}

fn check_counts(kind: &str, name: &str, declared: &Declared, actual: Counts) -> Result<()> {
    if declared.counts.tests != actual.tests
        || declared.counts.failures != actual.failures
        || declared.counts.errors != actual.errors
        || (declared.has_skipped && declared.counts.skipped != actual.skipped)
    {
        return Err(format!(
            "{kind} {name:?} count mismatch: declared {:?}, found {:?}",
            declared.counts, actual
        ));
    }
    Ok(())
}

fn count_attr(event: &BytesStart<'_>, name: &str) -> Result<usize> {
    parse_count(&required_attr(event, name)?)
}

fn parse_count(value: &str) -> Result<usize> {
    value
        .parse()
        .map_err(|_| format!("invalid JUnit count {value:?}"))
}

fn required_attr(event: &BytesStart<'_>, name: &str) -> Result<String> {
    attr(event, name)?.ok_or_else(|| {
        format!(
            "<{}> requires {name:?}",
            String::from_utf8_lossy(event.name().as_ref())
        )
    })
}

fn attr(event: &BytesStart<'_>, wanted: &str) -> Result<Option<String>> {
    let mut found = None;
    for attribute in event.attributes() {
        let attribute = attribute.map_err(|error| format!("invalid XML attribute: {error}"))?;
        if attribute.key.as_ref() == wanted.as_bytes() {
            if found.is_some() {
                return Err(format!("duplicate XML attribute {wanted:?}"));
            }
            let value = attribute
                .normalized_value(quick_xml::XmlVersion::Implicit1_0)
                .map_err(|error| format!("invalid {wanted:?} attribute: {error}"))?;
            found = Some(value.into_owned());
        }
    }
    Ok(found)
}

fn str_value(bytes: &[u8], what: &str) -> Result<String> {
    std::str::from_utf8(bytes)
        .map(str::to_string)
        .map_err(|_| format!("{what} is not UTF-8"))
}

fn parse_inventory(text: &str, source: &str) -> Result<std::collections::BTreeSet<Identity>> {
    if text.trim().is_empty() {
        return Err(format!("nextest inventory {source} is empty"));
    }
    let inventory: InventoryDocument = serde_json::from_str(text)
        .map_err(|error| format!("parse nextest inventory {source}: {error}"))?;
    let mut enumerated = 0usize;
    let mut runnable = std::collections::BTreeSet::new();
    for (suite_key, suite) in inventory.rust_suites {
        if suite.binary_id != suite_key || suite.binary_id.is_empty() {
            return Err(format!(
                "nextest inventory {source} suite key {suite_key:?} does not match binary-id {:?}",
                suite.binary_id
            ));
        }
        if suite.status != "listed" && !suite.status.starts_with("skipped") {
            return Err(format!(
                "nextest inventory {source} suite {suite_key:?} has unknown status {:?}",
                suite.status
            ));
        }
        if suite.status.starts_with("skipped") && !suite.testcases.is_empty() {
            return Err(format!(
                "nextest inventory {source} skipped suite {suite_key:?} contains testcases"
            ));
        }
        for (test_name, testcase) in suite.testcases {
            enumerated = enumerated
                .checked_add(1)
                .ok_or_else(|| format!("nextest inventory {source} testcase count overflow"))?;
            if test_name.is_empty() {
                return Err(format!(
                    "nextest inventory {source} suite {suite_key:?} contains an empty test name"
                ));
            }
            let filter_status = testcase.filter_match.status;
            let matches = match filter_status.as_str() {
                "matches" => true,
                "mismatch" => false,
                _ => {
                    return Err(format!(
                        "nextest inventory {source} testcase {suite_key} :: {test_name} has unknown filter-match status {filter_status:?}"
                    ))
                }
            };
            if !testcase.ignored && matches {
                runnable.insert(Identity(suite.binary_id.clone(), test_name));
            }
        }
    }
    if inventory.test_count != enumerated {
        return Err(format!(
            "nextest inventory {source} count mismatch: declared {}, found {enumerated}",
            inventory.test_count
        ));
    }
    Ok(runnable)
}

fn write_report(path: &Path, evaluation: &Evaluation) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .map_err(|error| format!("create {}: {error}", parent.display()))?;
    }
    let failures = usize::from(!evaluation.failures.is_empty() && evaluation.errors.is_empty());
    let errors = usize::from(!evaluation.errors.is_empty());
    let skipped = evaluation.matched.len() + evaluation.recoveries.len();
    let tests = 1 + skipped;
    let mut xml = format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<testsuites name=\"ratchet\" tests=\"{tests}\" failures=\"{failures}\" errors=\"{errors}\" skipped=\"{skipped}\">\n  <testsuite name=\"ratchet\" tests=\"{tests}\" failures=\"{failures}\" errors=\"{errors}\" skipped=\"{skipped}\">\n"
    );
    xml.push_str("    <testcase name=\"ratchet summary\" classname=\"ratchet\">");
    let problems = if errors > 0 {
        let mut messages = evaluation.errors.clone();
        messages.extend(evaluation.failures.iter().cloned());
        Some(("error", "ratchet error", messages.join("\n")))
    } else if failures > 0 {
        Some((
            "failure",
            "ratchet mismatch",
            evaluation.failures.join("\n"),
        ))
    } else {
        None
    };
    if let Some((tag, kind, message)) = problems {
        let _ = write!(
            xml,
            "<{tag} type=\"{kind}\" message=\"{}\"/>",
            escape(&message)
        );
    }
    xml.push_str("</testcase>\n");
    for matched in &evaluation.matched {
        let _ = writeln!(
            xml,
            "    <testcase name=\"{}\" classname=\"ratchet\"><skipped message=\"{}\"/></testcase>",
            escape(matched),
            escape(matched)
        );
    }
    for recovery in &evaluation.recoveries {
        let _ = writeln!(
            xml,
            "    <testcase name=\"recovered ledger entry\" classname=\"ratchet\"><skipped message=\"{}\"/></testcase>",
            escape(recovery)
        );
    }
    xml.push_str("  </testsuite>\n</testsuites>\n");
    fs::write(path, xml).map_err(|error| format!("write {}: {error}", path.display()))
}

fn escape(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

// ── issues ─────────────────────────────────────────────────────────────

/// Verify every `#N` in the ledger resolves on GitHub. Nightly-only: if
/// `gh` is not on PATH, this reports and passes rather than blocking a
/// local run that has no token.
fn run_issues(args: &[String]) -> Result<()> {
    if !args.is_empty() {
        return Err(format!("ratchet issues takes no options\n\n{}", usage()));
    }
    let root = crate::workspace_root()?;
    let rows = load_ledger(&root)?;
    let mut numbers: std::collections::BTreeSet<u64> = std::collections::BTreeSet::new();
    for row in &rows {
        let number: u64 = row.issue[1..]
            .parse()
            .map_err(|_| format!("{LEDGER_PATH}: malformed issue {:?}", row.issue))?;
        numbers.insert(number);
    }
    if numbers.is_empty() {
        println!("ratchet issues: ledger is empty, nothing to check");
        return Ok(());
    }
    if Command::new("gh").arg("--version").output().is_err() {
        println!("ratchet issues: gh is not available, skipping (nightly-only check)");
        return Ok(());
    }
    let mut missing = Vec::new();
    for number in &numbers {
        let status = Command::new("gh")
            .args(["issue", "view", &number.to_string(), "-R", "hew-lang/hew"])
            .output()
            .map_err(|err| format!("run gh issue view {number}: {err}"))?;
        if !status.status.success() {
            missing.push(*number);
        }
    }
    if missing.is_empty() {
        println!("ratchet issues: {} issue(s) resolved", numbers.len());
        Ok(())
    } else {
        Err(format!(
            "{} ledger issue(s) do not resolve on GitHub: {}",
            missing.len(),
            missing
                .iter()
                .map(|n| format!("#{n}"))
                .collect::<Vec<_>>()
                .join(", ")
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn xml(cases: &str, tests: usize, failures: usize, errors: usize, skipped: usize) -> String {
        format!("<testsuites tests=\"{tests}\" failures=\"{failures}\" errors=\"{errors}\" skipped=\"{skipped}\"><testsuite name=\"bin\" tests=\"{tests}\" failures=\"{failures}\" errors=\"{errors}\" skipped=\"{skipped}\">{cases}</testsuite></testsuites>")
    }

    fn one_row(suite: &str, platform: &str, id: &str, expect: &str) -> BTreeMap<String, Row> {
        let mut out = BTreeMap::new();
        out.insert(
            id.to_string(),
            Row {
                suite: suite.to_string(),
                platforms: vec![platform.to_string()],
                id: id.to_string(),
                expect: Expect::parse(expect).unwrap(),
                issue: "#1".to_string(),
                reason: "tracked bug".to_string(),
            },
        );
        out
    }

    fn write_temp(name: &str, contents: &str) -> PathBuf {
        let path = std::env::temp_dir().join(format!("hew-ratchet-{name}-{}", std::process::id()));
        fs::write(&path, contents).unwrap();
        path
    }

    #[test]
    fn parses_the_unified_ledger_sorted_by_suite_then_id() {
        let text = "corpus\t*\ta.hew\tcompile\t#1\tone\n\
                     nextest\tlinux\tbin::t\tfailure\t#2\ttwo\n";
        let rows = parse_ledger(text).unwrap();
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].suite, "corpus");
        assert_eq!(rows[1].suite, "nextest");
    }

    #[test]
    fn refuses_a_row_out_of_suite_id_order() {
        let text = "nextest\t*\tb\tfailure\t#1\tone\n\
                     nextest\t*\ta\tfailure\t#2\ttwo\n";
        let error = parse_ledger(text).unwrap_err();
        assert!(error.contains("not sorted"), "unexpected error: {error}");
    }

    #[test]
    fn refuses_a_row_with_a_malformed_issue() {
        let text = "corpus\t*\ta.hew\tcompile\tN123\tone\n";
        let error = parse_ledger(text).unwrap_err();
        assert!(error.contains("issue must be"), "unexpected error: {error}");
    }

    #[test]
    fn refuses_a_duplicate_selection_for_one_platform() {
        let text = "corpus\tlinux,macos\ta.hew\tcompile\t#1\tone\n\
                     corpus\tlinux\ta.hew\tcompile\t#2\ttwo\n";
        let error = parse_ledger(text).unwrap_err();
        assert!(
            error.contains("already selected"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn results_negative_control_an_unlisted_failure_fails_the_check() {
        let expected = one_row("corpus", "linux", "a.hew", "compile");
        let path = write_temp("unlisted", "b.hew\tcompile\n");
        let evaluation = evaluate_results(&path, &expected).unwrap();
        fs::remove_file(&path).unwrap();
        assert!(!evaluation.blocking());
        assert!(evaluation.failures[0].contains("unexpected compile: b.hew"));
    }

    #[test]
    fn results_a_recovered_row_reports_and_stays_green() {
        let expected = one_row("corpus", "linux", "a.hew", "compile");
        let path = write_temp("recovered", "");
        let evaluation = evaluate_results(&path, &expected).unwrap();
        fs::remove_file(&path).unwrap();
        assert!(
            evaluation.blocking(),
            "a recovery must never block the check"
        );
        assert_eq!(evaluation.recoveries.len(), 1);
        assert!(evaluation.recoveries[0].contains("a.hew"));
    }

    #[test]
    fn results_a_matched_row_stays_green() {
        let expected = one_row("corpus", "linux", "a.hew", "diagnostic:E_SIR_UNSUPPORTED");
        let path = write_temp("matched", "a.hew\tdiagnostic\tE_SIR_UNSUPPORTED\n");
        let evaluation = evaluate_results(&path, &expected).unwrap();
        fs::remove_file(&path).unwrap();
        assert!(evaluation.blocking());
        assert_eq!(evaluation.matched.len(), 1);
    }

    #[test]
    fn results_a_changed_outcome_kind_fails() {
        let expected = one_row("hew-suite", "linux", "t", "compile");
        let path = write_temp("changed", "t\truntime\n");
        let evaluation = evaluate_results(&path, &expected).unwrap();
        fs::remove_file(&path).unwrap();
        assert!(!evaluation.blocking());
        assert!(evaluation.failures[0].contains("changed from compile to runtime"));
    }

    #[test]
    fn nextest_negative_control_an_unlisted_junit_failure_fails() {
        let junit = write_temp(
            "junit-unlisted",
            &xml(
                "<testcase name=\"test\"><failure type=\"test failure with exit code 1\"/></testcase>",
                1,
                1,
                0,
                0,
            ),
        );
        let output =
            std::env::temp_dir().join(format!("hew-ratchet-report-{}.xml", std::process::id()));
        let options = CheckOptions {
            suite: "nextest".to_string(),
            platform: "linux".to_string(),
            output: output.clone(),
            junit: Some(junit.clone()),
            runner_exit: Some(100),
            full_inventory: None,
            selected_inventory: None,
            results: None,
        };
        let evaluation = evaluate_nextest(&junit, &BTreeMap::new(), 100, &options).unwrap();
        fs::remove_file(&junit).unwrap();
        assert!(!evaluation.blocking());
    }

    #[test]
    fn nextest_recovery_is_reported_and_never_blocks() {
        let junit = write_temp(
            "junit-recovered",
            &xml("<testcase name=\"test\"/>", 1, 0, 0, 0),
        );
        let expected = one_row("nextest", "linux", "bin::test", "failure");
        let options = CheckOptions {
            suite: "nextest".to_string(),
            platform: "linux".to_string(),
            output: PathBuf::from("/dev/null"),
            junit: Some(junit.clone()),
            runner_exit: Some(0),
            full_inventory: None,
            selected_inventory: None,
            results: None,
        };
        let evaluation = evaluate_nextest(&junit, &expected, 0, &options).unwrap();
        fs::remove_file(&junit).unwrap();
        assert!(
            evaluation.blocking(),
            "a recovery must never block the check"
        );
        assert_eq!(evaluation.recoveries.len(), 1);
    }

    #[test]
    fn nextest_exact_match_stays_green() {
        let junit = write_temp(
            "junit-matched",
            &xml(
                "<testcase name=\"test\"><failure type=\"test timeout\"/></testcase>",
                1,
                1,
                0,
                0,
            ),
        );
        let expected = one_row("nextest", "linux", "bin::test", "timeout");
        let options = CheckOptions {
            suite: "nextest".to_string(),
            platform: "linux".to_string(),
            output: PathBuf::from("/dev/null"),
            junit: Some(junit.clone()),
            runner_exit: Some(100),
            full_inventory: None,
            selected_inventory: None,
            results: None,
        };
        let evaluation = evaluate_nextest(&junit, &expected, 100, &options).unwrap();
        fs::remove_file(&junit).unwrap();
        assert!(evaluation.blocking());
        assert_eq!(evaluation.matched.len(), 1);
    }

    #[test]
    fn compact_report_is_valid_xml() {
        let evaluation = Evaluation {
            matched: vec!["a & b".to_string()],
            ..Evaluation::default()
        };
        let path =
            std::env::temp_dir().join(format!("hew-ratchet-report-{}.xml", std::process::id()));
        write_report(&path, &evaluation).unwrap();
        let report = scan(&fs::read_to_string(&path).unwrap()).unwrap();
        fs::remove_file(&path).unwrap();
        assert_eq!(report.counts.tests, 2);
        assert_eq!(report.counts.skipped, 1);
    }
}
