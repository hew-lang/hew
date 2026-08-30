use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};

use quick_xml::events::{BytesStart, Event};
use quick_xml::reader::Reader;
use serde::Deserialize;

use crate::Result;

#[derive(Debug)]
struct Options {
    junit: PathBuf,
    ledger: PathBuf,
    output: PathBuf,
    platform: String,
    runner_exit: i32,
    full_inventory: Option<PathBuf>,
    selected_inventory: Option<PathBuf>,
}

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
    fn ledger_name(self) -> Option<&'static str> {
        match self {
            Self::Failure => Some("failure"),
            Self::Timeout => Some("timeout"),
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
    full: BTreeSet<Identity>,
    selected: BTreeSet<Identity>,
}

impl FilteredInventories {
    fn new(full: BTreeSet<Identity>, selected: BTreeSet<Identity>) -> Result<Self> {
        if let Some(unexpected) = selected.difference(&full).next() {
            return Err(format!(
                "selected nextest inventory contains a test absent from the full inventory: {}",
                unexpected.label()
            ));
        }
        Ok(Self { full, selected })
    }
}

type Expected = (Outcome, String);

#[derive(Debug, Default)]
struct Evaluation {
    matched: Vec<(Identity, String)>,
    failures: Vec<String>,
    errors: Vec<String>,
}

impl Evaluation {
    fn exact(&self) -> bool {
        self.failures.is_empty() && self.errors.is_empty()
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

pub(super) fn run(args: &[String]) -> Result<()> {
    if args.iter().any(|arg| arg == "--help" || arg == "-h") {
        println!("{}", usage());
        return Ok(());
    }
    let options = parse_options(args)?;
    if same_path(&options.junit, &options.output) {
        return Err("ratchet output must differ from raw nextest JUnit".to_string());
    }

    let result: Result<Evaluation> = (|| {
        let xml = fs::read_to_string(&options.junit)
            .map_err(|err| format!("read {}: {err}", options.junit.display()))?;
        let report = scan(&xml)?;
        let ledger = fs::read_to_string(&options.ledger)
            .map_err(|err| format!("read {}: {err}", options.ledger.display()))?;
        let expected = parse_ledger(&ledger, &options.platform)?;
        let filtered_inventories = match (
            options.full_inventory.as_ref(),
            options.selected_inventory.as_ref(),
        ) {
            (None, None) => None,
            (Some(full_path), Some(selected_path)) => {
                let full_json = fs::read_to_string(full_path)
                    .map_err(|err| format!("read {}: {err}", full_path.display()))?;
                let full = parse_inventory(&full_json, &full_path.display().to_string())?;
                let selected_json = fs::read_to_string(selected_path)
                    .map_err(|err| format!("read {}: {err}", selected_path.display()))?;
                let selected =
                    parse_inventory(&selected_json, &selected_path.display().to_string())?;
                Some(FilteredInventories::new(full, selected)?)
            }
            _ => {
                return Err(
                    "--full-inventory and --selected-inventory must be provided together"
                        .to_string(),
                )
            }
        };
        Ok(evaluate(
            &report,
            expected,
            options.runner_exit,
            filtered_inventories.as_ref(),
        ))
    })();

    let evaluation = match result {
        Ok(evaluation) => evaluation,
        Err(error) => Evaluation {
            errors: vec![error.clone()],
            ..Evaluation::default()
        },
    };
    write_report(&options.output, &evaluation)?;
    if evaluation.exact() {
        println!(
            "nextest ratchet: {} known non-pass outcome(s) matched",
            evaluation.matched.len()
        );
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

fn usage() -> &'static str {
    "usage: cargo run -p xtask -- nextest-ratchet --junit <raw.xml> --ledger <ledger.tsv> --output <ratchet.xml> --platform <name> --runner-exit <code> [--full-inventory <nextest-list.json> --selected-inventory <nextest-list.json>]"
}

fn parse_options(args: &[String]) -> Result<Options> {
    let mut values = BTreeMap::new();
    let mut index = 0;
    while index < args.len() {
        let flag = args[index].as_str();
        if !matches!(
            flag,
            "--junit"
                | "--ledger"
                | "--output"
                | "--platform"
                | "--runner-exit"
                | "--full-inventory"
                | "--selected-inventory"
        ) {
            return Err(format!("unknown nextest-ratchet option: {flag}"));
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
    Ok(Options {
        junit: get("--junit")?.into(),
        ledger: get("--ledger")?.into(),
        output: get("--output")?.into(),
        platform: get("--platform")?.to_string(),
        runner_exit: get("--runner-exit")?
            .parse()
            .map_err(|_| "--runner-exit must be an integer".to_string())?,
        full_inventory: values
            .get("--full-inventory")
            .map(|value| PathBuf::from(*value)),
        selected_inventory: values
            .get("--selected-inventory")
            .map(|value| PathBuf::from(*value)),
    })
}

fn same_path(left: &Path, right: &Path) -> bool {
    left == right
        || fs::canonicalize(left)
            .ok()
            .zip(fs::canonicalize(right).ok())
            .is_some_and(|(left, right)| left == right)
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
            Ok(Event::Text(event)) if !event.as_ref().iter().all(u8::is_ascii_whitespace) => {
                if !matches!(
                    scanner.stack.last().copied(),
                    Some(Role::Status | Role::Other)
                ) {
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
            Ok(Event::Text(_) | Event::Decl(_) | Event::Comment(_)) => {}
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

fn parse_ledger(text: &str, platform: &str) -> Result<BTreeMap<Identity, Expected>> {
    const PLATFORMS: [&str; 4] = ["linux", "macos", "windows", "freebsd"];
    if !PLATFORMS.contains(&platform) {
        return Err(format!("unsupported test platform {platform:?}"));
    }
    let mut selected = BTreeMap::new();
    let mut seen = BTreeSet::new();
    for (index, line) in text.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let fields = line.split('\t').collect::<Vec<_>>();
        if fields.len() != 5 || fields.iter().any(|field| field.trim().is_empty()) {
            return Err(format!(
                "ledger line {} must have five non-empty tab-separated fields",
                index + 1
            ));
        }
        let platforms = fields[0].split(',').collect::<Vec<_>>();
        if platforms.iter().any(|item| item.is_empty()) {
            return Err(format!("ledger line {} has an empty platform", index + 1));
        }
        if platforms
            .iter()
            .any(|item| !matches!(*item, "*" | "linux" | "macos" | "windows" | "freebsd"))
        {
            return Err(format!("ledger line {} has an unknown platform", index + 1));
        }
        let outcome = match fields[1] {
            "failure" => Outcome::Failure,
            "timeout" => Outcome::Timeout,
            other => {
                return Err(format!(
                    "ledger line {} has unknown outcome {other:?}",
                    index + 1
                ))
            }
        };
        let identity = Identity(fields[2].into(), fields[3].into());
        for candidate in PLATFORMS {
            if (platforms.contains(&"*") || platforms.contains(&candidate))
                && !seen.insert((candidate, identity.clone()))
            {
                return Err(format!(
                    "ledger selects {} more than once for {candidate}",
                    identity.label()
                ));
            }
        }
        if (platforms.contains(&"*") || platforms.contains(&platform))
            && selected
                .insert(identity.clone(), (outcome, fields[4].into()))
                .is_some()
        {
            return Err(format!(
                "ledger selects {} more than once for {platform}",
                identity.label()
            ));
        }
    }
    Ok(selected)
}

fn parse_inventory(text: &str, source: &str) -> Result<BTreeSet<Identity>> {
    if text.trim().is_empty() {
        return Err(format!("nextest inventory {source} is empty"));
    }
    let inventory: InventoryDocument = serde_json::from_str(text)
        .map_err(|error| format!("parse nextest inventory {source}: {error}"))?;
    let mut enumerated = 0usize;
    let mut runnable = BTreeSet::new();
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
                _ => return Err(format!(
                    "nextest inventory {source} testcase {suite_key} :: {test_name} has unknown filter-match status {filter_status:?}"
                )),
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

fn evaluate(
    report: &Report,
    mut expected: BTreeMap<Identity, Expected>,
    runner_exit: i32,
    filtered_inventories: Option<&FilteredInventories>,
) -> Evaluation {
    let mut result = Evaluation::default();
    for (identity, case) in &report.cases {
        let wanted = expected.remove(identity);
        if let Some(reason) = &case.infrastructure {
            result
                .errors
                .push(format!("{}: {reason}", identity.label()));
            continue;
        }
        let Some(actual) = case.outcome.ledger_name() else {
            if let Some((outcome, reason)) = wanted {
                result.failures.push(format!(
                    "expected {} {} ({}) but it {}",
                    outcome.ledger_name().unwrap(),
                    identity.label(),
                    reason,
                    if case.outcome == Outcome::Skipped {
                        "was skipped"
                    } else {
                        "passed"
                    }
                ));
            }
            continue;
        };
        if let Some((outcome, reason)) = wanted {
            if outcome == case.outcome {
                result.matched.push((identity.clone(), reason));
            } else {
                result.failures.push(format!(
                    "{} changed from {} to {actual}",
                    identity.label(),
                    outcome.ledger_name().unwrap()
                ));
            }
        } else {
            result
                .failures
                .push(format!("unexpected {actual}: {}", identity.label()));
        }
    }
    for (identity, (outcome, reason)) in expected {
        if filtered_inventories.is_some_and(|inventories| {
            inventories.full.contains(&identity) && !inventories.selected.contains(&identity)
        }) {
            continue;
        }
        result.failures.push(format!(
            "expected {} is absent: {} ({})",
            outcome.ledger_name().unwrap(),
            identity.label(),
            reason
        ));
    }
    let has_nonpass = report.counts.failures + report.counts.errors > 0;
    if !matches!((runner_exit, has_nonpass), (0, false) | (100, true)) {
        result.errors.push(format!(
            "runner exit {runner_exit} is incoherent with {} failure/error outcome(s)",
            report.counts.failures + report.counts.errors
        ));
    }
    result
}

fn write_report(path: &Path, evaluation: &Evaluation) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .map_err(|error| format!("create {}: {error}", parent.display()))?;
    }
    let failures = usize::from(!evaluation.failures.is_empty() && evaluation.errors.is_empty());
    let errors = usize::from(!evaluation.errors.is_empty());
    let skipped = evaluation.matched.len();
    let tests = 1 + skipped;
    let mut xml = format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<testsuites name=\"nextest-ratchet\" tests=\"{tests}\" failures=\"{failures}\" errors=\"{errors}\" skipped=\"{skipped}\">\n  <testsuite name=\"nextest-ratchet\" tests=\"{tests}\" failures=\"{failures}\" errors=\"{errors}\" skipped=\"{skipped}\">\n"
    );
    xml.push_str("    <testcase name=\"ratchet summary\" classname=\"nextest-ratchet\">");
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
    for (identity, reason) in &evaluation.matched {
        let _ = writeln!(
            xml,
            "    <testcase name=\"{}\" classname=\"nextest-ratchet\"><skipped message=\"{}\"/></testcase>",
            escape(&identity.label()), escape(reason)
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

#[cfg(test)]
mod tests {
    use super::*;

    fn xml(cases: &str, tests: usize, failures: usize, errors: usize, skipped: usize) -> String {
        format!("<testsuites tests=\"{tests}\" failures=\"{failures}\" errors=\"{errors}\" skipped=\"{skipped}\"><testsuite name=\"bin\" tests=\"{tests}\" failures=\"{failures}\" errors=\"{errors}\" skipped=\"{skipped}\">{cases}</testsuite></testsuites>")
    }

    fn ledger(outcome: &str) -> BTreeMap<Identity, Expected> {
        parse_ledger(
            &format!("linux\t{outcome}\tbin\ttest\ttracked bug"),
            "linux",
        )
        .unwrap()
    }

    fn inventory(cases: &[(&str, bool, &str)], count: usize) -> String {
        let testcases = cases
            .iter()
            .map(|(name, ignored, filter_status)| {
                (
                    (*name).to_string(),
                    serde_json::json!({
                        "ignored": ignored,
                        "filter-match": { "status": filter_status },
                    }),
                )
            })
            .collect::<serde_json::Map<_, _>>();
        serde_json::json!({
            "test-count": count,
            "rust-suites": {
                "bin": {
                    "binary-id": "bin",
                    "status": "listed",
                    "testcases": testcases,
                },
            },
        })
        .to_string()
    }

    #[test]
    fn exact_failure_matches_and_unexpected_pass_does_not() {
        let report = scan(&xml(
            "<testcase name=\"test\"><failure type=\"test failure with exit code 1\"/></testcase>",
            1,
            1,
            0,
            0,
        ))
        .unwrap();
        let exact = evaluate(&report, ledger("failure"), 100, None);
        assert!(exact.exact());
        assert_eq!(exact.matched.len(), 1);

        let pass = scan(&xml("<testcase name=\"test\"/>", 1, 0, 0, 0)).unwrap();
        assert!(!evaluate(&pass, ledger("failure"), 0, None).exact());
    }

    #[test]
    fn compares_both_directions_and_outcome_classes() {
        let report = scan(&xml(
            "<testcase name=\"test\"><failure type=\"test timeout\"/></testcase>",
            1,
            1,
            0,
            0,
        ))
        .unwrap();
        let changed = evaluate(&report, ledger("failure"), 100, None);
        assert!(changed.failures[0].contains("changed from failure to timeout"));
        assert!(!evaluate(&report, BTreeMap::new(), 100, None).exact());
        assert!(!evaluate(&report, ledger("timeout"), 0, None).exact());
    }

    #[test]
    fn rejects_infrastructure_outcomes_even_when_ledgered() {
        for failure in [
            "<failure type=\"test abort\" message=\"signal 11\"/>",
            "<failure type=\"future status\"/>",
            "<error type=\"execution failure\"/>",
        ] {
            let errors = usize::from(failure.starts_with("<error"));
            let report = scan(&xml(
                &format!("<testcase name=\"test\">{failure}</testcase>"),
                1,
                1 - errors,
                errors,
                0,
            ))
            .unwrap();
            assert!(!evaluate(&report, ledger("failure"), 100, None)
                .errors
                .is_empty());
        }

        let report = scan(&xml(
            "<testcase name=\"test\"><flakyFailure type=\"test failure\"/></testcase>",
            1,
            0,
            0,
            0,
        ))
        .unwrap();
        assert!(!evaluate(&report, BTreeMap::new(), 0, None)
            .errors
            .is_empty());
    }

    #[test]
    fn validates_structure_counts_duplicates_and_zero_tests() {
        assert!(scan("").is_err());
        assert!(scan(&xml("", 0, 0, 0, 0)).is_err());
        assert!(scan(&xml("<testcase name=\"a\"/>", 2, 0, 0, 0)).is_err());
        assert!(scan(&xml(
            "<testcase name=\"a\"/><testcase name=\"a\"/>",
            2,
            0,
            0,
            0
        ))
        .is_err());
        assert!(scan("<testsuites>").is_err());
    }

    #[test]
    fn ledger_is_platform_scoped_and_rejects_overlap() {
        let text = "linux,macos\tfailure\tbin\ta\tone\nwindows\ttimeout\tbin\tb\ttwo";
        assert_eq!(parse_ledger(text, "linux").unwrap().len(), 1);
        assert!(parse_ledger(
            "*\tfailure\tbin\ta\tone\nlinux\tfailure\tbin\ta\ntwo",
            "linux"
        )
        .is_err());
        assert!(parse_ledger(
            "windows\tfailure\tbin\ta\tone\nwindows\tfailure\tbin\ta\ntwo",
            "linux"
        )
        .is_err());
        assert!(parse_ledger("linx\tfailure\tbin\ta\tone", "linux").is_err());
        assert!(parse_ledger("", "solaris").is_err());
    }

    #[test]
    fn filtered_run_ignores_only_known_runnable_tests_absent_from_junit() {
        let report = scan(&xml("<testcase name=\"selected\"/>", 1, 0, 0, 0)).unwrap();
        let full = parse_inventory(
            &inventory(
                &[("test", false, "matches"), ("selected", false, "matches")],
                2,
            ),
            "counterfactual",
        )
        .unwrap();
        let selected = parse_inventory(
            &inventory(
                &[("test", false, "mismatch"), ("selected", false, "matches")],
                2,
            ),
            "counterfactual",
        )
        .unwrap();
        let filtered = FilteredInventories::new(full.clone(), selected).unwrap();
        assert!(evaluate(&report, ledger("failure"), 0, Some(&filtered)).exact());

        let stale_inventories = FilteredInventories::new(BTreeSet::new(), BTreeSet::new()).unwrap();
        let stale = evaluate(&report, ledger("failure"), 0, Some(&stale_inventories));
        assert!(stale.failures[0].contains("expected failure is absent"));

        let selected_but_missing = FilteredInventories::new(full.clone(), full).unwrap();
        let missing = evaluate(&report, ledger("failure"), 0, Some(&selected_but_missing));
        assert!(
            missing.failures[0].contains("expected failure is absent"),
            "a selected test missing from JUnit must not be mistaken for a filter exclusion"
        );
        assert!(!evaluate(&report, ledger("failure"), 0, None).exact());
    }

    #[test]
    fn inventory_cannot_hide_an_expected_test_that_ran_or_is_not_runnable() {
        let pass = scan(&xml("<testcase name=\"test\"/>", 1, 0, 0, 0)).unwrap();
        let full = parse_inventory(
            &inventory(&[("test", false, "matches")], 1),
            "counterfactual",
        )
        .unwrap();
        let available = FilteredInventories::new(full.clone(), full).unwrap();
        assert!(!evaluate(&pass, ledger("failure"), 0, Some(&available)).exact());

        let unrelated = scan(&xml("<testcase name=\"selected\"/>", 1, 0, 0, 0)).unwrap();
        for testcase in [("test", true, "matches"), ("test", false, "mismatch")] {
            let unavailable =
                parse_inventory(&inventory(&[testcase], 1), "counterfactual").unwrap();
            let inventories = FilteredInventories::new(unavailable.clone(), unavailable).unwrap();
            assert!(!evaluate(&unrelated, ledger("failure"), 0, Some(&inventories)).exact());
        }
    }

    #[test]
    fn selected_inventory_must_be_a_subset_of_the_full_inventory() {
        let selected = BTreeSet::from([Identity("bin".into(), "test".into())]);
        assert!(FilteredInventories::new(BTreeSet::new(), selected).is_err());
    }

    #[test]
    fn inventory_parser_fails_closed_on_schema_and_count_drift() {
        assert!(parse_inventory("", "counterfactual").is_err());
        assert!(parse_inventory(
            &inventory(&[("test", false, "matches")], 2),
            "counterfactual"
        )
        .is_err());
        assert!(parse_inventory(
            &inventory(&[("test", false, "future")], 1),
            "counterfactual"
        )
        .is_err());
        assert!(parse_inventory(
            r#"{"test-count":0,"rust-suites":{"wrong":{"binary-id":"bin","status":"listed","testcases":{}}}}"#,
            "counterfactual"
        )
        .is_err());
        assert!(parse_inventory(
            r#"{"test-count":0,"rust-suites":{"bin":{"binary-id":"bin","status":"skipped-default-filter","testcases":{}}}}"#,
            "counterfactual"
        )
        .unwrap()
        .is_empty());
        assert!(parse_inventory(
            r#"{"test-count":1,"rust-suites":{"bin":{"binary-id":"bin","status":"skipped-default-filter","testcases":{"test":{"ignored":false,"filter-match":{"status":"matches"}}}}}}"#,
            "counterfactual"
        )
        .is_err());
    }

    #[test]
    fn compact_report_is_valid_and_contains_only_ratchet_cases() {
        let evaluation = Evaluation {
            matched: vec![(Identity("bin".into(), "a&b".into()), "known".into())],
            ..Evaluation::default()
        };
        let path = std::env::temp_dir().join(format!("hew-ratchet-{}.xml", std::process::id()));
        write_report(&path, &evaluation).unwrap();
        let report = scan(&fs::read_to_string(&path).unwrap()).unwrap();
        fs::remove_file(path).unwrap();
        assert_eq!(report.counts.tests, 2);
        assert_eq!(report.counts.skipped, 1);
    }
}
