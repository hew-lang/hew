use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;
use std::time::{Duration, Instant};

use serde::Deserialize;

type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Deserialize)]
struct Manifest {
    #[serde(rename = "case")]
    cases: Vec<Case>,
}

#[derive(Debug, Clone, Deserialize)]
struct Case {
    id: String,
    intent: String,
    /// The `.hew` (or, for `kind = "doc"`, the documentation file or
    /// directory) the case observes, resolved against
    /// `tests/core-acceptance/`. A case may point outside that directory —
    /// `../vertical-slice/accept/x.hew`, `../../examples/y.hew` — so a case
    /// names an existing source instead of duplicating it.
    source: PathBuf,
    /// Flat input directory copied independently for each execution profile.
    fixtures: Option<PathBuf>,
    suites: Vec<String>,
    timeout_seconds: u64,
    /// A case without a `kind` is `run`: compile and execute at O0 and O2.
    #[serde(default)]
    kind: CaseKind,
    /// `kind = "run"` only: environment the compiled binary runs under. The
    /// migrated vertical-slice fixtures carry `HEW_WORKERS` here because
    /// their observed output depends on the scheduler's worker count.
    #[serde(default)]
    env: BTreeMap<String, String>,
    /// `kind = "doc"` only: how fences are extracted from `source` and how
    /// each fence's case id is spelled.
    #[serde(default)]
    fences: Option<FenceSource>,
    #[serde(default)]
    expected: ExpectedOutcome,
}

/// What a case proves.
///
/// - `Run` (the default) compiles and executes the source at O0 and O2
///   against an exact stdout/stderr/exit expectation.
/// - `Check` runs `hew check` once and asserts the *exact* set of
///   diagnostics it reports: nothing extra, nothing missing.
/// - `Reject` runs `hew check` once and asserts the compile was refused and
///   that the named diagnostics are among the ones reported. This is the
///   vertical-slice reject oracle: it pins the diagnostic that matters
///   without freezing every unrelated cascade line around it.
/// - `Doc` is a fence source, not a single observation: the runner extracts
///   every fenced hew block from `source` and expands each into its own
///   case, named by the fence's content, and `hew check`s it.
///
/// Safety (ASan/LSan) stays a suite selected by `suites`, not a case kind —
/// only a `run` case is ever sanitizer-compiled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
enum CaseKind {
    #[default]
    Run,
    Check,
    Reject,
    Doc,
}

impl CaseKind {
    fn label(self) -> &'static str {
        match self {
            Self::Run => "run",
            Self::Check => "check",
            Self::Reject => "reject",
            Self::Doc => "doc",
        }
    }

    fn parse(text: &str) -> Result<Self> {
        match text {
            "run" => Ok(Self::Run),
            "check" => Ok(Self::Check),
            "reject" => Ok(Self::Reject),
            "doc" => Ok(Self::Doc),
            other => Err(format!(
                "unknown core-acceptance kind {other:?}; expected run, check, reject or doc"
            )),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize)]
struct ExpectedOutcome {
    /// `kind = "run"` only, and required there: the exact stdout the
    /// program must produce. `Option` (not a default-empty `String`) so a
    /// run case that forgets it is a validation error, not a silent pass
    /// against an empty expectation.
    #[serde(default)]
    stdout: Option<String>,
    #[serde(default)]
    stderr: String,
    /// `kind = "run"` only, and required there: see `stdout` for why this is
    /// `Option` rather than default-zero.
    #[serde(default)]
    exit: Option<i32>,
    /// `kind = "check"` only: the exact diagnostics `hew check --format json`
    /// must report, matched as a set on `(code, line, column)` with an
    /// optional message substring.
    #[serde(default)]
    diagnostics: Vec<ExpectedDiagnostic>,
}

impl ExpectedOutcome {
    /// The `(stdout, exit)` a `run` case expects. `validate_manifest`
    /// requires both to be present before a `run` case ever reaches
    /// `execute`, so the `expect`s here document an already-checked
    /// invariant rather than a fallible lookup.
    fn run_expectation(&self) -> (&str, i32) {
        (
            self.stdout
                .as_deref()
                .expect("run case validated to have expected stdout"),
            self.exit
                .expect("run case validated to have an expected exit"),
        )
    }
}

/// One expected diagnostic for a `check` case.
///
/// `code` is the JSON diagnostic's `code` field verbatim — the stable `kind`
/// discriminant `hew check --format json` emits (see
/// `hew-cli/src/diagnostic_json.rs`). For a diagnostic whose specific error
/// lives under a generic discriminant (`InvalidOperation` covers several
/// distinct `E_*` checks today), `code` is that generic string and `message`
/// is what actually pins down which one: the `E_*`/`W_*` token is embedded in
/// the message text, not the JSON code, until each such check earns its own
/// discriminant.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct ExpectedDiagnostic {
    code: String,
    line: usize,
    column: usize,
    #[serde(default)]
    message: Option<String>,
    /// The source file the diagnostic must be reported against, matched as a
    /// path suffix so a case never pins the checkout's absolute location. A
    /// multi-file case uses it to prove provenance: an imported file's spans
    /// are its own byte offsets, so an untagged diagnostic renders against the
    /// root source at whatever text shares the offset.
    #[serde(default)]
    file: Option<String>,
}

#[derive(Debug)]
struct Options {
    suite: String,
    cases: Vec<String>,
    /// Empty means every kind. `--kind` exists so the transitional
    /// `make test-doc-examples` alias can run the doc fences without also
    /// compiling every native case.
    kinds: Vec<CaseKind>,
    hew_bin: PathBuf,
    timeout_seconds: Option<u64>,
    jobs: usize,
}

#[derive(Debug, Clone, Copy)]
enum Profile {
    O0,
    O2,
}

impl Profile {
    fn cli_level(self) -> &'static str {
        match self {
            Self::O0 => "0",
            Self::O2 => "2",
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::O0 => "O0",
            Self::O2 => "O2",
        }
    }
}

#[derive(Debug)]
enum CommandResult {
    Completed {
        status: ExitStatus,
        stdout: String,
        stderr: String,
    },
    TimedOut {
        stdout: String,
        stderr: String,
    },
}

pub(crate) fn run(args: &[String]) -> Result<()> {
    if matches!(args, [flag] if flag == "--help" || flag == "-h") {
        println!("{}", usage());
        return Ok(());
    }
    let options = parse_options(args)?;
    let root = workspace_root()?;
    let run_dir = tempfile::tempdir()
        .map_err(|err| format!("create core acceptance temporary directory: {err}"))?;
    let mut manifest = load_manifest(&root)?;
    expand_doc_cases(&mut manifest, &root, run_dir.path())?;
    validate_manifest(&manifest, &root)?;
    let ratchet = load_expected_failures(&root, &manifest)?;
    let selected = select_cases(&manifest, &options.suite, &options.cases, &options.kinds)?;
    let fingerprint = compiler_fingerprint(&options.hew_bin)?;
    let instrumentation_request = if options.suite == "safety" {
        if !cfg!(target_os = "linux") {
            return Err("environment failure: paired ASan/LSan safety requires Linux".to_string());
        }
        "address"
    } else {
        "none"
    };

    println!(
        "core-acceptance compiler={} version={fingerprint:?} host={}-{} instrumentation-requested={instrumentation_request}",
        options.hew_bin.display(),
        std::env::consts::OS,
        std::env::consts::ARCH,
    );
    println!(
        "core-acceptance suite={} selected={} jobs={} build=prebuilt",
        options.suite,
        selected.len(),
        options.jobs,
    );

    let runner = Runner {
        options: &options,
        root: &root,
        run_dir: run_dir.path(),
        instrumentation_request,
    };
    let verdicts = run_selected(&runner, &selected, options.jobs);
    let Ratchet {
        failed,
        known,
        now_passing,
    } = classify(&verdicts, &ratchet);

    println!(
        "core-acceptance results: {} passed, {} known-failing, {} failed",
        verdicts.len() - failed.len() - known.len() - now_passing.len(),
        known.len(),
        failed.len(),
    );
    for id in &known {
        println!("KNOWN {id}  # {}", ratchet[*id]);
    }

    if !now_passing.is_empty() {
        println!("core-acceptance: these cases pass but are listed in {EXPECTED_FAILURES_PATH}:");
        for id in &now_passing {
            println!("  {id}");
        }
        println!(
            "  Delete those rows. Ratchets only shrink; never restore a row to keep a run green."
        );
    }
    if !failed.is_empty() {
        println!("core-acceptance: unlisted failing cases:");
        for id in &failed {
            println!("  {id}");
        }
    }

    if failed.is_empty() && now_passing.is_empty() {
        println!("core-acceptance: PASS");
        Ok(())
    } else {
        Err(format!(
            "core-acceptance: {} unlisted failure(s), {} stale expected-failure row(s)",
            failed.len(),
            now_passing.len()
        ))
    }
}

/// Run the selected cases across `jobs` threads, reporting each as it
/// finishes, and return the verdicts.
///
/// Each case buffers its own report and prints it whole under a lock, so two
/// cases never interleave their lines and a long run says what it is doing
/// while it does it.
fn run_selected<'a>(
    runner: &Runner<'_>,
    selected: &[&'a Case],
    jobs: usize,
) -> Vec<(&'a str, bool)> {
    let next = AtomicUsize::new(0);
    let verdicts = std::sync::Mutex::new(Vec::with_capacity(selected.len()));
    thread::scope(|scope| {
        for _ in 0..jobs.min(selected.len().max(1)) {
            scope.spawn(|| loop {
                let index = next.fetch_add(1, Ordering::Relaxed);
                let Some(case) = selected.get(index) else {
                    return;
                };
                let mut log = String::new();
                let passed = runner.run_case(case, &mut log);
                let mut reported = verdicts.lock().expect("verdict lock");
                print!("{log}");
                reported.push((case.id.as_str(), passed));
            });
        }
    });
    let mut reported = verdicts.into_inner().expect("verdict lock");
    reported.sort_unstable();
    reported
}

/// How a run's verdicts land against the expected-failure ledger.
#[derive(Debug, Default, PartialEq, Eq)]
struct Ratchet<'a> {
    /// Failed with no row: red.
    failed: Vec<&'a str>,
    /// Failed with a row: reported, not red.
    known: Vec<&'a str>,
    /// Passed with a row: red, because a ratchet only shrinks and a stale row
    /// hides the next real failure of that case.
    now_passing: Vec<&'a str>,
}

fn classify<'a>(verdicts: &[(&'a str, bool)], ratchet: &BTreeMap<String, String>) -> Ratchet<'a> {
    let mut out = Ratchet::default();
    for (id, passed) in verdicts {
        match (passed, ratchet.contains_key(*id)) {
            (true, false) => {}
            (true, true) => out.now_passing.push(id),
            (false, true) => out.known.push(id),
            (false, false) => out.failed.push(id),
        }
    }
    out
}

const EXPECTED_FAILURES_PATH: &str = "tests/core-acceptance/expected-failures.txt";

/// The one expected-failure ledger for the acceptance runner: a case id per
/// row, with an issue or a one-line reason after `#`.
///
/// A row naming a case that no longer exists is refused here rather than
/// silently ignored — a renamed or deleted case must take its row with it,
/// otherwise the ledger accumulates rows that can never be retired.
fn load_expected_failures(root: &Path, manifest: &Manifest) -> Result<BTreeMap<String, String>> {
    let path = root.join(EXPECTED_FAILURES_PATH);
    let contents = match fs::read_to_string(&path) {
        Ok(contents) => contents,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(BTreeMap::new()),
        Err(err) => return Err(format!("read {}: {err}", path.display())),
    };
    let ids: std::collections::BTreeSet<&str> =
        manifest.cases.iter().map(|case| case.id.as_str()).collect();
    let mut rows = BTreeMap::new();
    for (number, line) in contents.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let (id, reason) = match line.split_once('#') {
            Some((id, reason)) => (id.trim(), reason.trim()),
            None => (line, ""),
        };
        if reason.is_empty() {
            return Err(format!(
                "{}:{}: {id} has no reason; give an issue or a one-line reason after #",
                path.display(),
                number + 1
            ));
        }
        if !ids.contains(id) {
            return Err(format!(
                "{}:{}: {id} is not a core-acceptance case; delete the row with the case",
                path.display(),
                number + 1
            ));
        }
        if rows.insert(id.to_string(), reason.to_string()).is_some() {
            return Err(format!(
                "{}:{}: duplicate row for {id}",
                path.display(),
                number + 1
            ));
        }
    }
    Ok(rows)
}

fn parse_options(args: &[String]) -> Result<Options> {
    let root = workspace_root()?;
    let mut suite = "acceptance".to_string();
    let mut cases = Vec::new();
    let mut hew_bin = root.join("target/debug/hew");
    let mut timeout_seconds = None;
    let mut kinds = Vec::new();
    let mut jobs = None;
    let mut index = 0;

    while index < args.len() {
        let value = &args[index];
        match value.as_str() {
            "--suite" => {
                suite = required_value(args, &mut index, "--suite")?.to_string();
            }
            "--case" => {
                cases.push(required_value(args, &mut index, "--case")?.to_string());
            }
            "--hew-bin" => {
                hew_bin = PathBuf::from(required_value(args, &mut index, "--hew-bin")?);
            }
            "--kind" => {
                for name in required_value(args, &mut index, "--kind")?.split(',') {
                    let kind = CaseKind::parse(name.trim())?;
                    if !kinds.contains(&kind) {
                        kinds.push(kind);
                    }
                }
            }
            "--jobs" => {
                let value = required_value(args, &mut index, "--jobs")?;
                let requested: usize = value
                    .parse()
                    .map_err(|_| format!("--jobs must be a positive integer, got {value:?}"))?;
                if requested == 0 {
                    return Err("--jobs must be positive".to_string());
                }
                jobs = Some(requested);
            }
            "--timeout-seconds" => {
                let value = required_value(args, &mut index, "--timeout-seconds")?;
                timeout_seconds = Some(value.parse().map_err(|_| {
                    format!("--timeout-seconds must be a positive integer, got {value:?}")
                })?);
            }
            "--help" | "-h" => return Err("--help must be used on its own".to_string()),
            _ => {
                return Err(format!(
                    "unknown core-acceptance option: {value}\n\n{}",
                    usage()
                ));
            }
        }
        index += 1;
    }

    if !matches!(suite.as_str(), "acceptance" | "safety") {
        return Err(format!(
            "unknown core-acceptance suite {suite:?}; expected acceptance or safety"
        ));
    }
    if timeout_seconds == Some(0) {
        return Err("--timeout-seconds must be positive".to_string());
    }
    // Safety runs sanitizer-instrumented binaries whose faults and scheduler
    // interleavings are the subject; running them alongside 31 others changes
    // what is being observed, and each one is memory-heavy besides.
    let jobs = jobs.unwrap_or_else(|| if suite == "safety" { 1 } else { default_jobs() });
    Ok(Options {
        suite,
        cases,
        kinds,
        hew_bin,
        timeout_seconds,
        jobs,
    })
}

/// One case at a time per core, capped. Every case shells out to the compiler
/// and then to the compiled program, so the runner is almost entirely waiting
/// on child processes and the migrated suite is unusable serially — but each
/// compile is an LLVM pass and a link against a multi-hundred-MB archive, and
/// past a point more of those at once only produces timeouts on a shared box.
const MAX_DEFAULT_JOBS: usize = 16;

fn default_jobs() -> usize {
    thread::available_parallelism()
        .map_or(1, std::num::NonZero::get)
        .min(MAX_DEFAULT_JOBS)
}

fn required_value<'a>(args: &'a [String], index: &mut usize, flag: &str) -> Result<&'a str> {
    *index += 1;
    args.get(*index)
        .map(String::as_str)
        .ok_or_else(|| format!("{flag} requires a value"))
}

fn usage() -> String {
    [
        "usage: cargo run -p xtask -- core-acceptance [options]",
        "",
        "options:",
        "  --suite acceptance|safety              select a manifest suite (default: acceptance)",
        "  --case ID                             run one named manifest case",
        "  --hew-bin PATH                        use a prebuilt compiler binary",
        "  --kind run,check,reject,doc           run only cases of these kinds",
        "  --jobs N                              cases to run concurrently (default: cores; 1 for safety)",
        "  --timeout-seconds N                   override each case timeout",
    ]
    .join("\n")
}

fn workspace_root() -> Result<PathBuf> {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .map(Path::to_path_buf)
        .ok_or_else(|| "xtask manifest should have a workspace parent".to_string())
}

fn load_manifest(root: &Path) -> Result<Manifest> {
    let cases_dir = root.join("tests/core-acceptance/cases");
    let mut paths: Vec<PathBuf> = fs::read_dir(&cases_dir)
        .map_err(|err| format!("read core acceptance cases {}: {err}", cases_dir.display()))?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<std::result::Result<_, _>>()
        .map_err(|err| format!("read core acceptance case entry: {err}"))?;
    paths.retain(|path| path.extension().is_some_and(|ext| ext == "toml"));
    paths.sort();

    let mut cases = Vec::with_capacity(paths.len());
    for path in paths {
        let contents = fs::read_to_string(&path)
            .map_err(|err| format!("read core acceptance case {}: {err}", path.display()))?;
        let mut parsed: Manifest = toml::from_str(&contents)
            .map_err(|err| format!("parse core acceptance case {}: {err}", path.display()))?;
        if parsed.cases.len() != 1 {
            return Err(format!(
                "core acceptance case file must contain exactly one [[case]]: {}",
                path.display()
            ));
        }
        let case = parsed.cases.remove(0);
        let file_stem = path
            .file_stem()
            .map(|stem| stem.to_string_lossy().into_owned());
        if file_stem.as_deref() != Some(case.id.as_str()) {
            return Err(format!(
                "core acceptance case id {:?} does not match its file name {}",
                case.id,
                path.display()
            ));
        }
        cases.push(case);
    }
    Ok(Manifest { cases })
}

fn validate_manifest(manifest: &Manifest, root: &Path) -> Result<()> {
    if manifest.cases.is_empty() {
        return Err("core acceptance manifest has no cases".to_string());
    }
    let mut ids = std::collections::BTreeSet::new();
    for case in &manifest.cases {
        if case.id.is_empty() {
            return Err("core acceptance case id must not be empty".to_string());
        }
        if case.intent.is_empty() {
            return Err(format!("{} must state its semantic intent", case.id));
        }
        if !ids.insert(&case.id) {
            return Err(format!("duplicate core acceptance case id: {}", case.id));
        }
        if case.timeout_seconds == 0 {
            return Err(format!("{} has a zero timeout", case.id));
        }
        if case.suites.is_empty()
            || case
                .suites
                .iter()
                .any(|suite| !matches!(suite.as_str(), "acceptance" | "safety"))
        {
            return Err(format!("{} has invalid suite membership", case.id));
        }
        validate_case(case, root)?;
    }
    Ok(())
}

/// The checks that belong to one case: what its kind requires of its
/// expectation, and that its source exists.
fn validate_case(case: &Case, root: &Path) -> Result<()> {
    match case.kind {
        CaseKind::Run => {
            if !case.expected.diagnostics.is_empty() {
                return Err(format!(
                    "{} has kind run but declares expected diagnostics (that shape is check-only)",
                    case.id
                ));
            }
            if case.expected.stdout.is_none() || case.expected.exit.is_none() {
                return Err(format!(
                    "{} has kind run and must declare both expected stdout and exit",
                    case.id
                ));
            }
        }
        CaseKind::Check | CaseKind::Reject => {
            if case.expected.diagnostics.is_empty() {
                return Err(format!(
                    "{} has kind {} but declares no expected diagnostics",
                    case.id,
                    case.kind.label()
                ));
            }
            if case.suites.iter().any(|suite| suite == "safety") {
                return Err(format!(
                    "{} has kind {} but belongs to the safety suite; safety stays a suite, not a kind",
                    case.id,
                    case.kind.label()
                ));
            }
            if case.expected.stdout.is_some()
                || case.expected.exit.is_some()
                || !case.expected.stderr.is_empty()
            {
                return Err(format!(
                    "{} has kind {} but declares run-only expected stdout/stderr/exit",
                    case.id,
                    case.kind.label()
                ));
            }
        }
        CaseKind::Doc => {
            // Every doc case in the manifest was expanded into one case
            // per extracted fence before validation, so what survives
            // here is a fence: it proves a clean `hew check`, and has no
            // expectation of its own to declare.
            if !case.expected.diagnostics.is_empty()
                || case.expected.stdout.is_some()
                || case.expected.exit.is_some()
                || !case.expected.stderr.is_empty()
            {
                return Err(format!(
                    "{} is a doc fence and must not declare an expectation; a fence proves a clean check",
                    case.id
                ));
            }
            if case.suites.iter().any(|suite| suite == "safety") {
                return Err(format!(
                    "{} has kind doc but belongs to the safety suite",
                    case.id
                ));
            }
        }
    }
    if !case.env.is_empty() && case.kind != CaseKind::Run {
        return Err(format!(
            "{} has kind {} but declares run-only environment",
            case.id,
            case.kind.label()
        ));
    }
    let source = case_source(root, case);
    if !source.is_file() {
        return Err(format!(
            "{} source does not exist: {}",
            case.id,
            source.display()
        ));
    }
    Ok(())
}

/// Resolve a case's `source` against `tests/core-acceptance/`. A migrated
/// case points at the fixture where it already lives
/// (`../vertical-slice/accept/x.hew`, `../../examples/y.hew`) instead of
/// carrying a second copy of it, and an expanded doc fence carries an
/// absolute path into the run directory — `Path::join` honours both.
fn case_source(root: &Path, case: &Case) -> PathBuf {
    root.join("tests/core-acceptance").join(&case.source)
}

fn select_cases<'a>(
    manifest: &'a Manifest,
    suite: &str,
    selected_ids: &[String],
    kinds: &[CaseKind],
) -> Result<Vec<&'a Case>> {
    if !selected_ids.is_empty() {
        let mut selected = Vec::with_capacity(selected_ids.len());
        for selected_id in selected_ids {
            let case = manifest
                .cases
                .iter()
                .find(|case| case.id == *selected_id)
                .ok_or_else(|| format!("unknown core acceptance case: {selected_id}"))?;
            if !case.suites.iter().any(|member| member == suite) {
                return Err(format!(
                    "core acceptance case {selected_id:?} does not belong to suite {suite:?}"
                ));
            }
            selected.push(case);
        }
        return Ok(selected);
    }
    let selected = manifest
        .cases
        .iter()
        .filter(|case| case.suites.iter().any(|member| member == suite))
        .filter(|case| kinds.is_empty() || kinds.contains(&case.kind))
        .collect::<Vec<_>>();
    if selected.is_empty() {
        return Err(format!(
            "core acceptance suite {suite:?} has no cases{}",
            if kinds.is_empty() {
                String::new()
            } else {
                format!(
                    " of kind {:?}",
                    kinds.iter().map(|kind| kind.label()).collect::<Vec<_>>()
                )
            }
        ));
    }
    Ok(selected)
}

fn compiler_fingerprint(hew_bin: &Path) -> Result<String> {
    if !hew_bin.is_file() {
        return Err(format!(
            "environment failure: compiler binary not found: {}",
            hew_bin.display()
        ));
    }
    match run_command(
        Command::new(hew_bin).arg("--version"),
        Duration::from_secs(10),
    )? {
        CommandResult::Completed {
            status,
            stdout,
            stderr: _,
        } if status.success() => Ok(stdout.trim().to_string()),
        CommandResult::Completed {
            status,
            stdout,
            stderr,
        } => Err(format!(
            "environment failure: compiler fingerprint exited {:?}: {}{}",
            status.code(),
            summarise(&stdout),
            summarise(&stderr)
        )),
        CommandResult::TimedOut { stdout, stderr } => Err(format!(
            "environment failure: compiler fingerprint timed out: {}{}",
            summarise(&stdout),
            summarise(&stderr)
        )),
    }
}

struct Runner<'a> {
    options: &'a Options,
    root: &'a Path,
    run_dir: &'a Path,
    instrumentation_request: &'a str,
}

impl Runner<'_> {
    fn run_case(&self, case: &Case, log: &mut String) -> bool {
        match case.kind {
            CaseKind::Run => {
                let mut passed = true;
                for profile in [Profile::O0, Profile::O2] {
                    if !self.run_profile(case, profile, log) {
                        passed = false;
                    }
                }
                passed
            }
            CaseKind::Check | CaseKind::Reject => self.run_check(case, false, log),
            CaseKind::Doc => self.run_check(case, true, log),
        }
    }

    /// Run one `hew check --format json` against the case's source: no
    /// build, no execution.
    ///
    /// `expect_clean` is the doc-fence shape — the check must exit 0, which
    /// is the whole observation a fence makes. Otherwise the check must be
    /// refused (exit 1) and its diagnostics compared against the case:
    /// `check` demands the exact set, `reject` demands the named ones are
    /// present and says nothing about the rest.
    fn run_check(&self, case: &Case, expect_clean: bool, log: &mut String) -> bool {
        let source = case_source(self.root, case);
        let mut command = Command::new(&self.options.hew_bin);
        command
            .arg("check")
            .arg(&source)
            .arg("--format")
            .arg("json")
            .current_dir(self.root);
        let result = match run_command(&mut command, self.timeout(case)) {
            Ok(result) => result,
            Err(err) => {
                let _ = writeln!(
                    log,
                    "FAIL {} profile={} class=environment-failure detail={err}",
                    case.id,
                    case.kind.label()
                );
                return false;
            }
        };
        match result {
            CommandResult::TimedOut { stdout, stderr } => {
                let _ = writeln!(
                    log,
                    "FAIL {} profile={} class=timeout timeout_seconds={}{}{}",
                    case.id,
                    case.kind.label(),
                    self.timeout(case).as_secs(),
                    summarise(&stdout),
                    summarise(&stderr)
                );
                false
            }
            CommandResult::Completed {
                status,
                stdout,
                stderr,
            } => {
                let Some(actual_exit) = status.code() else {
                    let _ = writeln!(
                        log,
                        "FAIL {} profile={} class=compiler-crash{}{}",
                        case.id,
                        case.kind.label(),
                        summarise(&stdout),
                        summarise(&stderr)
                    );
                    return false;
                };
                Self::check_reported(case, expect_clean, actual_exit, &stdout, &stderr, log)
            }
        }
    }

    /// Compare one finished `hew check` against the case.
    fn check_reported(
        case: &Case,
        expect_clean: bool,
        actual_exit: i32,
        stdout: &str,
        stderr: &str,
        log: &mut String,
    ) -> bool {
        // A clean check exits 0 and a refused one does not: that is
        // the contract the kind verifies, not a per-case expectation.
        // `check` pins the exact refusal shape and so pins exit 1;
        // `reject` only proves the compile was refused, and a refusal
        // by a compiler limitation exits 3.
        let wrong_exit = match case.kind {
            CaseKind::Doc => actual_exit != 0,
            CaseKind::Reject => actual_exit == 0,
            _ => actual_exit != 1,
        };
        if wrong_exit {
            let expected_exit = match case.kind {
                CaseKind::Doc => "0",
                CaseKind::Reject => "non-zero",
                _ => "1",
            };
            let _ = writeln!(
                log,
                "FAIL {} profile={} class=wrong-exit expected={expected_exit} actual={actual_exit}{}{}",
                case.id,
                case.kind.label(),
                summarise(stdout),
                summarise(stderr)
            );
            return false;
        }
        if !expect_clean {
            let actual: Vec<ActualDiagnostic> = match serde_json::from_str(stdout) {
                Ok(diagnostics) => diagnostics,
                Err(err) => {
                    let _ = writeln!(
                        log,
                        "FAIL {} profile={} class=environment-failure detail=parse diagnostics json: {err}{}",
                        case.id,
                        case.kind.label(),
                        summarise(stdout)
                    );
                    return false;
                }
            };
            let exact = case.kind == CaseKind::Check;
            if let Err(detail) = diagnostics_match(&case.expected.diagnostics, &actual, exact) {
                let _ = writeln!(
                    log,
                    "FAIL {} profile={} class=wrong-diagnostics detail={detail}{}",
                    case.id,
                    case.kind.label(),
                    summarise(stderr)
                );
                return false;
            }
        }
        let _ = writeln!(
            log,
            "PASS {} profile={} exit={actual_exit}",
            case.id,
            case.kind.label()
        );
        true
    }

    fn run_profile(&self, case: &Case, profile: Profile, log: &mut String) -> bool {
        let source = case_source(self.root, case);
        let emit_dir = self.run_dir.join(&case.id).join(profile.label());
        let Some(binary) = self.compile(case, profile, &source, &emit_dir, log) else {
            return false;
        };
        self.execute(case, profile, &binary, log)
    }

    fn timeout(&self, case: &Case) -> Duration {
        Duration::from_secs(self.options.timeout_seconds.unwrap_or(case.timeout_seconds))
    }

    fn compile(
        &self,
        case: &Case,
        profile: Profile,
        source: &Path,
        emit_dir: &Path,
        log: &mut String,
    ) -> Option<PathBuf> {
        if let Err(err) = fs::create_dir_all(emit_dir) {
            let _ = writeln!(
                log,
                "FAIL {} profile={} class=environment-failure detail=create emit directory: {err}",
                case.id,
                profile.label()
            );
            return None;
        }
        let mut command = Command::new(&self.options.hew_bin);
        command
            .arg("compile")
            .arg("--emit-dir")
            .arg(emit_dir)
            .arg("--opt-level")
            .arg(profile.cli_level())
            .arg(source)
            .current_dir(self.root);
        if self.instrumentation_request == "address" {
            command.arg("--emit-llvm").env("HEW_SANITIZE_ADDRESS", "1");
            configure_safety_environment(&mut command);
        } else {
            command.env_remove("HEW_SANITIZE_ADDRESS");
        }
        let result = match run_command(&mut command, self.timeout(case)) {
            Ok(result) => result,
            Err(err) => {
                let _ = writeln!(
                    log,
                    "FAIL {} profile={} class=environment-failure detail={err}",
                    case.id,
                    profile.label()
                );
                return None;
            }
        };
        match result {
            CommandResult::TimedOut { stdout, stderr } => {
                let _ = writeln!(
                    log,
                    "FAIL {} profile={} class=timeout phase=compile timeout_seconds={}{}{}",
                    case.id,
                    profile.label(),
                    self.timeout(case).as_secs(),
                    summarise(&stdout),
                    summarise(&stderr)
                );
                None
            }
            CommandResult::Completed {
                status,
                stdout,
                stderr,
            } if !status.success() => {
                let class = if status.code().is_some() {
                    "source-diagnostic"
                } else {
                    "compiler-crash"
                };
                let _ = writeln!(
                    log,
                    "FAIL {} profile={} class={} exit={:?}{}{}",
                    case.id,
                    profile.label(),
                    class,
                    status.code(),
                    summarise(&stdout),
                    summarise(&stderr)
                );
                None
            }
            CommandResult::Completed { .. } => {
                let binary = executable_path(emit_dir, source);
                if self.instrumentation_request == "address" {
                    if let Err(error) = verify_address_instrumentation(&binary.with_extension("ll"))
                    {
                        let _ = writeln!(
                            log,
                            "FAIL {} profile={} class=environment-failure detail={error}",
                            case.id,
                            profile.label()
                        );
                        return None;
                    }
                }
                if binary.is_file() {
                    Some(binary)
                } else {
                    let _ = writeln!(
                    log,
                        "FAIL {} profile={} class=environment-failure detail=compiler returned success without {}",
                        case.id,
                        profile.label(),
                        binary.display()
                    );
                    None
                }
            }
        }
    }

    fn prepare_inputs(&self, case: &Case, profile: Profile) -> Result<PathBuf> {
        let Some(fixtures) = &case.fixtures else {
            return Ok(self.root.to_path_buf());
        };
        let source = self.root.join("tests/core-acceptance").join(fixtures);
        let destination = self
            .run_dir
            .join(&case.id)
            .join(profile.label())
            .join("inputs");
        fs::create_dir_all(&destination)
            .map_err(|err| format!("create fixture directory: {err}"))?;
        for entry in fs::read_dir(&source)
            .map_err(|err| format!("read fixture directory {}: {err}", source.display()))?
        {
            let entry = entry.map_err(|err| format!("read fixture entry: {err}"))?;
            if !entry
                .file_type()
                .map_err(|err| format!("inspect fixture: {err}"))?
                .is_file()
            {
                return Err(format!(
                    "fixture must be a regular file: {}",
                    entry.path().display()
                ));
            }
            fs::copy(entry.path(), destination.join(entry.file_name()))
                .map_err(|err| format!("copy fixture {}: {err}", entry.path().display()))?;
        }
        Ok(destination)
    }

    fn execute(&self, case: &Case, profile: Profile, binary: &Path, log: &mut String) -> bool {
        let mut command = Command::new(binary);
        for (name, value) in &case.env {
            command.env(name, value);
        }
        let working_dir = match self.prepare_inputs(case, profile) {
            Ok(directory) => directory,
            Err(err) => {
                let _ = writeln!(
                    log,
                    "FAIL {} profile={} class=environment-failure detail={err}",
                    case.id,
                    profile.label()
                );
                return false;
            }
        };
        command.current_dir(working_dir);
        if self.instrumentation_request == "address" {
            configure_safety_environment(&mut command);
        }
        let executed = match run_command(&mut command, self.timeout(case)) {
            Ok(result) => result,
            Err(err) => {
                let _ = writeln!(
                    log,
                    "FAIL {} profile={} class=environment-failure detail={err}",
                    case.id,
                    profile.label()
                );
                return false;
            }
        };
        match executed {
            CommandResult::TimedOut { stdout, stderr } => {
                let _ = writeln!(
                    log,
                    "FAIL {} profile={} class=timeout phase=run timeout_seconds={}{}{}",
                    case.id,
                    profile.label(),
                    self.timeout(case).as_secs(),
                    summarise(&stdout),
                    summarise(&stderr)
                );
                false
            }
            CommandResult::Completed {
                status,
                stdout,
                stderr,
            } if status.code().is_none() => {
                let _ = writeln!(
                    log,
                    "FAIL {} profile={} class=program-crash{}{}",
                    case.id,
                    profile.label(),
                    summarise(&stdout),
                    summarise(&stderr)
                );
                false
            }
            CommandResult::Completed {
                status,
                stdout,
                stderr,
            } => {
                let actual_exit = status.code().expect("checked above");
                self.report_run(case, profile, actual_exit, &stdout, &stderr, log)
            }
        }
    }

    /// Compare one finished fixture run against the case.
    fn report_run(
        &self,
        case: &Case,
        profile: Profile,
        actual_exit: i32,
        stdout: &str,
        stderr: &str,
        log: &mut String,
    ) -> bool {
        let (expected_stdout, expected_exit) = case.expected.run_expectation();
        if actual_exit != expected_exit {
            let _ = writeln!(
                log,
                "FAIL {} profile={} class=wrong-exit expected={} actual={}{}{}",
                case.id,
                profile.label(),
                expected_exit,
                actual_exit,
                summarise(stdout),
                summarise(stderr)
            );
            return false;
        }
        if stdout != expected_stdout || stderr != case.expected.stderr {
            let _ = writeln!(
                log,
                "FAIL {} profile={} class=wrong-output expected={:?} actual={:?}{}",
                case.id,
                profile.label(),
                expected_stdout,
                stdout,
                summarise(stderr)
            );
            return false;
        }
        let _ = writeln!(
            log,
            "PASS {} profile={} exit={} instrumentation-requested={}",
            case.id,
            profile.label(),
            actual_exit,
            self.instrumentation_request
        );
        true
    }
}

fn executable_path(emit_dir: &Path, source: &Path) -> PathBuf {
    let stem = source
        .file_stem()
        .expect("manifest source has a filename")
        .to_string_lossy();
    let name = if cfg!(windows) {
        format!("{stem}.exe")
    } else {
        stem.into_owned()
    };
    emit_dir.join(name)
}

fn verify_address_instrumentation(path: &Path) -> Result<()> {
    let ir = fs::read_to_string(path).map_err(|error| {
        format!(
            "environment failure: cannot read generated safety IR {}: {error}",
            path.display()
        )
    })?;
    if !ir.contains("sanitize_address") || !ir.contains("call void @__asan_init()") {
        return Err(format!(
            "environment failure: generated IR has no AddressSanitizer instrumentation: {}",
            path.display()
        ));
    }
    Ok(())
}

fn configure_safety_environment(command: &mut Command) {
    command
        .env(
            "ASAN_OPTIONS",
            "detect_leaks=1:use_sigaltstack=0:halt_on_error=1:exitcode=99",
        )
        .env("LSAN_OPTIONS", "exitcode=99");
}

fn run_command(command: &mut Command, timeout: Duration) -> Result<CommandResult> {
    let output_dir =
        tempfile::tempdir().map_err(|err| format!("create command output directory: {err}"))?;
    let stdout_path = output_dir.path().join("stdout");
    let stderr_path = output_dir.path().join("stderr");
    let stdout_file = fs::File::create(&stdout_path)
        .map_err(|err| format!("create command stdout capture: {err}"))?;
    let stderr_file = fs::File::create(&stderr_path)
        .map_err(|err| format!("create command stderr capture: {err}"))?;
    command.stdout(Stdio::from(stdout_file));
    command.stderr(Stdio::from(stderr_file));
    command.stdin(Stdio::null());
    let mut child = command
        .spawn()
        .map_err(|err| format!("start command: {err}"))?;
    let started = Instant::now();
    let status = loop {
        if let Some(status) = child
            .try_wait()
            .map_err(|err| format!("wait for command: {err}"))?
        {
            break Some(status);
        }
        if started.elapsed() >= timeout {
            child
                .kill()
                .map_err(|err| format!("kill timed out command: {err}"))?;
            child
                .wait()
                .map_err(|err| format!("wait for timed out command: {err}"))?;
            break None;
        }
        thread::sleep(Duration::from_millis(10));
    };
    let stdout = read_capture(&stdout_path, "stdout")?;
    let stderr = read_capture(&stderr_path, "stderr")?;
    Ok(match status {
        Some(status) => CommandResult::Completed {
            status,
            stdout,
            stderr,
        },
        None => CommandResult::TimedOut { stdout, stderr },
    })
}

fn read_capture(path: &Path, stream: &str) -> Result<String> {
    fs::read_to_string(path)
        .map_err(|err| format!("read command {stream} capture {}: {err}", path.display()))
}

/// The slice of `hew check --format json`'s `JsonDiagnostic` this runner
/// needs. Serde ignores the rest of the object (severity, channel, source,
/// notes, fixes) — the JSON diagnostics array is the one authority, read
/// directly, never re-derived from the text renderer.
#[derive(Debug, Deserialize)]
struct ActualDiagnostic {
    code: String,
    span: ActualSpan,
    message: String,
    #[serde(default)]
    file: Option<String>,
}

#[derive(Debug, Deserialize)]
struct ActualSpan {
    start_line: usize,
    start_col: usize,
}

/// Match a case's expected diagnostics against what `hew check` actually
/// reported, as a set keyed on `(code, line, column)` with an optional
/// message substring per match.
///
/// A missing diagnostic always fails. `exact` decides what an unnamed extra
/// diagnostic means: a `check` case proves the exact set and fails on one, a
/// `reject` case proves only that the named diagnostics are reported and
/// ignores the rest.
fn diagnostics_match(
    expected: &[ExpectedDiagnostic],
    actual: &[ActualDiagnostic],
    exact: bool,
) -> Result<()> {
    let mut remaining: Vec<&ActualDiagnostic> = actual.iter().collect();
    let mut missing = Vec::new();
    for want in expected {
        let position = remaining.iter().position(|got| {
            got.code == want.code
                && got.span.start_line == want.line
                && got.span.start_col == want.column
                && match &want.message {
                    Some(substring) => got.message.contains(substring.as_str()),
                    None => true,
                }
                && match &want.file {
                    Some(suffix) => got
                        .file
                        .as_deref()
                        .is_some_and(|file| file.ends_with(suffix.as_str())),
                    None => true,
                }
        });
        match position {
            Some(index) => {
                remaining.remove(index);
            }
            None => missing.push(format!(
                "{}@{}{}:{}{}",
                want.code,
                want.file
                    .as_deref()
                    .map(|f| format!("{f}:"))
                    .unwrap_or_default(),
                want.line,
                want.column,
                want.message
                    .as_deref()
                    .map(|m| format!(" ({m:?})"))
                    .unwrap_or_default()
            )),
        }
    }
    if missing.is_empty() && (!exact || remaining.is_empty()) {
        return Ok(());
    }
    if !exact {
        return Err(format!("missing={missing:?}"));
    }
    let extra: Vec<String> = remaining
        .iter()
        .map(|got| {
            format!(
                "{}@{}{}:{} ({:?})",
                got.code,
                got.file
                    .as_deref()
                    .map(|f| format!("{f}:"))
                    .unwrap_or_default(),
                got.span.start_line,
                got.span.start_col,
                got.message
            )
        })
        .collect();
    Err(format!("missing={missing:?} extra={extra:?}"))
}

fn summarise(text: &str) -> String {
    if text.is_empty() {
        String::new()
    } else {
        let limit = 2_000;
        let mut end = text.len().min(limit);
        while !text.is_char_boundary(end) {
            end -= 1;
        }
        let truncated = &text[..end];
        format!(" output={truncated:?}")
    }
}

// ---------------------------------------------------------------------------
// Documentation fences
// ---------------------------------------------------------------------------

/// How a `kind = "doc"` case's fences are found and named.
///
/// Fence identity is the fence's own content, not its position in the file:
/// a `<prefix>-<cksum>` id is unaffected by an unrelated fence inserted or
/// removed earlier in the same document, and a fence whose text genuinely
/// changes gets a new id and shows up as an ordinary new case. That is the
/// same identity the doc-fence ratchet has always used, so an expected-failure
/// row survives this migration verbatim.
#[derive(Debug, Clone, Deserialize)]
struct FenceSource {
    prefix: String,
    style: FenceStyle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
enum FenceStyle {
    /// One Markdown file, whose fences carry an explicit hew tag.
    Markdown,
    /// A directory of `.hew` documentation modules. Only `//!` lines carry
    /// prose, and their fences carry an explicit hew tag. Ids are
    /// `<prefix>-<stem>-<cksum>`.
    Module,
    /// The standard library, walked recursively. Both `//!` and `///` carry
    /// prose, an untagged fence is hew (the only language std writes in),
    /// and an explicitly tagged fence is skipped. Ids are
    /// `<prefix>-<slug>-<cksum>`.
    Std,
}

/// Substrings that, in the five lines before a fence, mark it as documenting
/// a surface that is not implemented yet. Spec ahead of implementation is not
/// drift when it is declared.
const FENCE_SKIP_MARKERS: [&str; 3] = ["Not yet implemented", "doctest: skip", "doctest:skip"];

/// Replace every `kind = "doc"` case with one case per extracted fence.
///
/// A doc case in the manifest is a fence *source*, not an observation. After
/// this pass every case in the manifest is a single observation with its own
/// id, so selection, the expected-failure ledger and the reporting all see
/// one flat key space.
fn expand_doc_cases(manifest: &mut Manifest, root: &Path, run_dir: &Path) -> Result<()> {
    if !manifest.cases.iter().any(|case| case.kind == CaseKind::Doc) {
        return Ok(());
    }
    let fence_dir = run_dir.join("doc-fences");
    fs::create_dir_all(&fence_dir).map_err(|err| format!("create doc fence directory: {err}"))?;
    let mut minted: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    let mut expanded = Vec::with_capacity(manifest.cases.len());

    for case in std::mem::take(&mut manifest.cases) {
        if case.kind != CaseKind::Doc {
            expanded.push(case);
            continue;
        }
        let Some(fences) = case.fences.clone() else {
            return Err(format!(
                "{} has kind doc but declares no [case.fences]",
                case.id
            ));
        };
        let source = root.join("tests/core-acceptance").join(&case.source);
        let mut found = 0usize;
        for (path, prefix) in fence_documents(&source, &fences)? {
            let text = fs::read_to_string(&path)
                .map_err(|err| format!("read doc source {}: {err}", path.display()))?;
            let extracted = match fences.style {
                FenceStyle::Std => extract_std_fences(&text),
                FenceStyle::Markdown => extract_fences(&text, false),
                FenceStyle::Module => extract_fences(&text, true),
            };
            for (content, skip) in extracted {
                found += 1;
                if skip {
                    continue;
                }
                let id = mint_fence_id(&prefix, &content, &mut minted);
                let fence_path = fence_dir.join(format!("{id}.hew"));
                fs::write(&fence_path, &content)
                    .map_err(|err| format!("write fence {}: {err}", fence_path.display()))?;
                expanded.push(Case {
                    id,
                    intent: format!(
                        "documentation fence from {}: the surface it teaches must type-check",
                        path.strip_prefix(root).unwrap_or(&path).display()
                    ),
                    source: fence_path,
                    fixtures: None,
                    suites: case.suites.clone(),
                    timeout_seconds: case.timeout_seconds,
                    kind: CaseKind::Doc,
                    env: BTreeMap::new(),
                    fences: None,
                    expected: ExpectedOutcome::default(),
                });
            }
        }
        // A source that yields nothing (a renamed doc, a changed fence
        // marker) would make the ledger trivially agree with an empty run.
        if found == 0 {
            return Err(format!(
                "{} extracted no fences from {}",
                case.id,
                source.display()
            ));
        }
    }
    manifest.cases = expanded;
    Ok(())
}

/// The `(file, id prefix)` pairs one doc case covers: a single file for
/// `markdown`, every module in a directory for `module`, the whole tree for
/// `std`.
fn fence_documents(source: &Path, fences: &FenceSource) -> Result<Vec<(PathBuf, String)>> {
    if fences.style == FenceStyle::Markdown {
        if !source.is_file() {
            return Err(format!("doc source is not a file: {}", source.display()));
        }
        return Ok(vec![(source.to_path_buf(), fences.prefix.clone())]);
    }
    if !source.is_dir() {
        return Err(format!(
            "doc source is not a directory: {}",
            source.display()
        ));
    }
    let recursive = fences.style == FenceStyle::Std;
    let mut files = Vec::new();
    collect_hew_files(source, recursive, &mut files)?;
    files.sort();
    let documents = files
        .into_iter()
        .map(|path| {
            let slug = match fences.style {
                FenceStyle::Std => path
                    .strip_prefix(source)
                    .unwrap_or(&path)
                    .with_extension("")
                    .to_string_lossy()
                    .replace(std::path::MAIN_SEPARATOR, "-"),
                _ => path
                    .file_stem()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .into_owned(),
            };
            let prefix = format!("{}-{slug}", fences.prefix);
            (path, prefix)
        })
        .collect();
    Ok(documents)
}

fn collect_hew_files(directory: &Path, recursive: bool, out: &mut Vec<PathBuf>) -> Result<()> {
    for entry in
        fs::read_dir(directory).map_err(|err| format!("read {}: {err}", directory.display()))?
    {
        let entry = entry.map_err(|err| format!("read directory entry: {err}"))?;
        let path = entry.path();
        if path.is_dir() {
            if recursive && path.file_name().is_some_and(|name| name != "target") {
                collect_hew_files(&path, recursive, out)?;
            }
        } else if path.extension().is_some_and(|ext| ext == "hew") {
            out.push(path);
        }
    }
    Ok(())
}

/// `<prefix>-<cksum>`, with a numeric suffix when two fences in the same
/// document are byte-identical, so both are still checked independently.
fn mint_fence_id(
    prefix: &str,
    content: &str,
    minted: &mut std::collections::BTreeSet<String>,
) -> String {
    let base = format!("{prefix}-{}", posix_cksum(content.as_bytes()));
    let mut candidate = base.clone();
    let mut suffix = 1u32;
    while minted.contains(&candidate) {
        suffix += 1;
        candidate = format!("{base}-{suffix}");
    }
    minted.insert(candidate.clone());
    candidate
}

/// The POSIX `cksum` CRC. The doc-fence ratchet's ids were minted by
/// `cksum`, so the runner must produce the same number or every existing
/// expected-failure row would be orphaned by this migration.
fn posix_cksum(data: &[u8]) -> u32 {
    fn step(crc: u32, byte: u8) -> u32 {
        let mut crc = crc ^ (u32::from(byte) << 24);
        for _ in 0..8 {
            crc = if crc & 0x8000_0000 != 0 {
                (crc << 1) ^ 0x04C1_1DB7
            } else {
                crc << 1
            };
        }
        crc
    }
    let mut crc = 0u32;
    for byte in data {
        crc = step(crc, *byte);
    }
    let mut length = data.len();
    while length > 0 {
        let byte = u8::try_from(length & 0xFF).expect("masked to one byte");
        crc = step(crc, byte);
        length >>= 8;
    }
    !crc
}

/// Split a document the way `while IFS= read -r line` does: on newlines, with
/// a final unterminated line dropped. Carriage returns stay in the line so a
/// fence's content — and therefore its id — is byte-identical to what the
/// shell extractor produced.
fn document_lines(text: &str) -> Vec<&str> {
    let mut lines: Vec<&str> = text.split('\n').collect();
    lines.pop();
    lines
}

fn without_carriage_return(line: &str) -> &str {
    line.strip_suffix('\r').unwrap_or(line)
}

fn fence_is_skipped(lines: &[String], fence_index: usize) -> bool {
    let start = fence_index.saturating_sub(5);
    lines[start..fence_index].iter().any(|line| {
        FENCE_SKIP_MARKERS
            .iter()
            .any(|marker| line.contains(marker))
    })
}

/// Extract the hew-tagged fences from a Markdown document, or from the `//!`
/// prose of a `.hew` documentation module when `strip_module_prefix` is set.
fn extract_fences(text: &str, strip_module_prefix: bool) -> Vec<(String, bool)> {
    let lines: Vec<String> = document_lines(text)
        .into_iter()
        .map(|line| {
            if !strip_module_prefix {
                return line.to_string();
            }
            match line.strip_prefix("//!") {
                Some(rest) => rest.strip_prefix(' ').unwrap_or(rest).to_string(),
                None => String::new(),
            }
        })
        .collect();

    let mut fences = Vec::new();
    let mut index = 0;
    while index < lines.len() {
        if without_carriage_return(&lines[index]) != "```hew" {
            index += 1;
            continue;
        }
        let skip = fence_is_skipped(&lines, index);
        index += 1;
        let mut content = String::new();
        while index < lines.len() {
            if without_carriage_return(&lines[index]) == "```" {
                index += 1;
                break;
            }
            content.push_str(&lines[index]);
            content.push('\n');
            index += 1;
        }
        fences.push((content, skip));
    }
    fences
}

/// The standard library's fences: `///` and `//!` both carry prose, an
/// untagged ``` opens an implicit hew fence, and an explicitly tagged fence
/// (```text) is stepped over rather than mistaken for one.
fn extract_std_fences(text: &str) -> Vec<(String, bool)> {
    let lines: Vec<String> = document_lines(text)
        .into_iter()
        .map(|line| {
            let trimmed = line.trim_start();
            for marker in ["//!", "///"] {
                if let Some(rest) = trimmed.strip_prefix(marker) {
                    return rest.strip_prefix(' ').unwrap_or(rest).to_string();
                }
            }
            String::new()
        })
        .collect();

    let mut fences = Vec::new();
    let mut index = 0;
    let mut inside_other_language = false;
    while index < lines.len() {
        let stripped = without_carriage_return(&lines[index]);
        if inside_other_language {
            if stripped == "```" {
                inside_other_language = false;
            }
            index += 1;
            continue;
        }
        if stripped != "```" && stripped != "```hew" {
            if stripped.starts_with("```") {
                inside_other_language = true;
            }
            index += 1;
            continue;
        }
        let skip = fence_is_skipped(&lines, index);
        index += 1;
        let mut content = String::new();
        while index < lines.len() {
            if without_carriage_return(&lines[index]) == "```" {
                index += 1;
                break;
            }
            content.push_str(&lines[index]);
            content.push('\n');
            index += 1;
        }
        fences.push((content, skip));
    }
    fences
}

#[cfg(test)]
mod tests {
    use super::*;

    fn manifest() -> Manifest {
        toml::from_str(
            r#"
            [[case]]
            id = "acceptance-case"
            intent = "selection test"
            source = "cases/acceptance-case.hew"
            suites = ["acceptance"]
            timeout_seconds = 1
            [case.expected]
            stdout = ""
            exit = 0
            [[case]]
            id = "safety-case"
            intent = "selection test"
            source = "cases/safety-case.hew"
            suites = ["safety"]
            timeout_seconds = 1
            [case.expected]
            stdout = ""
            exit = 0
            "#,
        )
        .expect("test manifest parses")
    }

    #[test]
    fn repeated_case_flags_select_each_case_in_order() {
        let mut manifest = manifest();
        manifest.cases[1].suites = vec!["acceptance".into()];
        let selected = select_cases(
            &manifest,
            "acceptance",
            &["safety-case".to_string(), "acceptance-case".to_string()],
            &[],
        )
        .expect("both cases belong to the acceptance suite");
        let selected_ids: Vec<&str> = selected.iter().map(|case| case.id.as_str()).collect();
        assert_eq!(selected_ids, ["safety-case", "acceptance-case"]);
    }

    #[test]
    fn selected_case_must_exist() {
        let error = select_cases(&manifest(), "acceptance", &["missing".to_string()], &[])
            .expect_err("unknown focused case must fail rather than silently running a suite");
        assert!(error.contains("unknown core acceptance case"));
    }

    #[test]
    fn profile_inputs_are_isolated_and_missing_inputs_fail() {
        let directory = tempfile::tempdir().unwrap();
        let fixture = directory.path().join("tests/core-acceptance/fixtures");
        fs::create_dir_all(&fixture).unwrap();
        let bytes = b"a\0b\xff";
        fs::write(fixture.join("input.txt"), bytes).unwrap();
        let mut case = manifest().cases.remove(0);
        case.fixtures = Some("fixtures".into());
        let options = Options {
            suite: "acceptance".into(),
            cases: Vec::new(),
            kinds: Vec::new(),
            jobs: 1,
            hew_bin: directory.path().join("hew"),
            timeout_seconds: None,
        };
        let runner = Runner {
            options: &options,
            root: directory.path(),
            run_dir: directory.path(),
            instrumentation_request: "none",
        };
        let first = runner.prepare_inputs(&case, Profile::O0).unwrap();
        assert_eq!(fs::read(first.join("input.txt")).unwrap(), bytes);
        fs::write(first.join("input.txt"), "changed by program").unwrap();
        let second = runner.prepare_inputs(&case, Profile::O2).unwrap();
        assert_eq!(fs::read(second.join("input.txt")).unwrap(), bytes);
        assert_eq!(fs::read(fixture.join("input.txt")).unwrap(), bytes);
        case.fixtures = Some("missing".into());
        assert!(runner.prepare_inputs(&case, Profile::O0).is_err());
    }

    #[test]
    fn safety_requires_instrumented_ir_not_just_an_executable() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("program.ll");
        assert!(verify_address_instrumentation(&path).is_err());
        fs::write(&path, "define i32 @main() { ret i32 0 }").unwrap();
        assert!(verify_address_instrumentation(&path).is_err());
        fs::write(
            &path,
            "define void @body() sanitize_address { ret void }\n\
             declare void @__asan_init()\n\
             define void @asan.module_ctor() { call void @__asan_init()\nret void }",
        )
        .unwrap();
        verify_address_instrumentation(&path).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn sanitizer_report_fails_even_when_exit_matches_the_program() {
        use std::os::unix::fs::PermissionsExt;
        let directory = tempfile::tempdir().unwrap();
        let binary = directory.path().join("report");
        fs::write(
            &binary,
            "#!/bin/sh\nprintf 'ERROR: LeakSanitizer: detected memory leaks\\n' >&2\nexit 23\n",
        )
        .unwrap();
        fs::set_permissions(&binary, fs::Permissions::from_mode(0o700)).unwrap();
        let mut case = manifest().cases.remove(0);
        case.expected.exit = Some(23);
        let options = Options {
            suite: "safety".to_string(),
            cases: Vec::new(),
            kinds: Vec::new(),
            jobs: 1,
            hew_bin: binary.clone(),
            timeout_seconds: None,
        };
        let runner = Runner {
            options: &options,
            root: directory.path(),
            run_dir: directory.path(),
            instrumentation_request: "address",
        };
        assert!(!runner.execute(&case, Profile::O0, &binary, &mut String::new()));
    }

    #[test]
    fn a_case_can_require_both_native_acceptance_and_safety() {
        let manifest: Manifest = toml::from_str(
            r#"[[case]]
            id = "owned"
            intent = "independent value copy"
            source = "owned.hew"
            suites = ["acceptance", "safety"]
            timeout_seconds = 1
            [case.expected]
            stdout = ""
            exit = 0
            "#,
        )
        .unwrap();
        for suite in ["acceptance", "safety"] {
            assert_eq!(
                select_cases(&manifest, suite, &[], &[]).unwrap()[0].id,
                "owned"
            );
        }
    }

    #[test]
    fn focused_case_cannot_replace_the_requested_suite() {
        let manifest = manifest();
        let error = select_cases(&manifest, "safety", &["acceptance-case".to_string()], &[])
            .expect_err("ordinary execution cannot substitute for safety validation");
        assert!(error.contains("does not belong to suite"));
        let selected = select_cases(&manifest, "safety", &["safety-case".to_string()], &[])
            .expect("a focused case within its suite remains selectable");
        assert_eq!(selected[0].id, "safety-case");
    }

    #[test]
    fn unicode_output_summary_never_slices_a_character() {
        let text = format!("{}étail", "x".repeat(1_999));
        assert_eq!(summarise(&text), format!(" output={:?}", "x".repeat(1_999)));
    }

    fn write_case(dir: &Path, file_name: &str, id: &str) {
        let cases_dir = dir.join("tests/core-acceptance/cases");
        fs::create_dir_all(&cases_dir).unwrap();
        fs::write(
            cases_dir.join(file_name),
            format!(
                r#"[[case]]
                id = "{id}"
                intent = "selection test"
                source = "cases/{id}.hew"
                suites = ["acceptance"]
                timeout_seconds = 1
                [case.expected]
                stdout = ""
                exit = 0
                "#
            ),
        )
        .unwrap();
    }

    #[test]
    fn case_id_must_match_its_file_name() {
        let directory = tempfile::tempdir().unwrap();
        write_case(directory.path(), "wrong-name.toml", "actual-id");
        let error =
            load_manifest(directory.path()).expect_err("mismatched id and file name must fail");
        assert!(error.contains("does not match its file name"));
    }

    #[test]
    fn cases_load_in_sorted_file_order() {
        let directory = tempfile::tempdir().unwrap();
        write_case(directory.path(), "b-case.toml", "b-case");
        write_case(directory.path(), "a-case.toml", "a-case");
        let manifest = load_manifest(directory.path()).expect("both cases load");
        let ids: Vec<&str> = manifest.cases.iter().map(|case| case.id.as_str()).collect();
        assert_eq!(ids, ["a-case", "b-case"]);
    }

    #[test]
    fn missing_capture_is_an_environment_error() {
        let directory = tempfile::tempdir().expect("temporary directory");
        let error = read_capture(&directory.path().join("missing"), "stdout")
            .expect_err("missing command output must not masquerade as program output");
        assert!(error.contains("read command stdout capture"));
    }

    // -----------------------------------------------------------------
    // Expectation kind: `doc`, and the expected-failure ledger
    // -----------------------------------------------------------------

    #[test]
    fn fence_ids_use_the_posix_cksum_the_ratchet_was_minted_with() {
        // `printf '%s' <content> | cksum` — the ids in every existing
        // expected-failure row were minted this way, so a different checksum
        // would orphan the whole ledger.
        assert_eq!(posix_cksum(b""), 4_294_967_295);
        assert_eq!(posix_cksum(b"fn main() {}\n"), 3_257_837_729);
        assert_eq!(posix_cksum(b"let x = 1\n"), 2_119_607_129);
    }

    #[test]
    fn markdown_fences_carry_content_and_honour_a_skip_marker() {
        let document = concat!(
            "prose\n",
            "```hew\n",
            "fn main() {}\n",
            "```\n",
            "<!-- doctest: skip -->\n",
            "```hew\n",
            "aspirational\n",
            "```\n",
        );
        let fences = extract_fences(document, false);
        assert_eq!(
            fences,
            vec![
                ("fn main() {}\n".to_string(), false),
                ("aspirational\n".to_string(), true),
            ]
        );
    }

    #[test]
    fn module_fences_read_only_the_module_doc_prose() {
        let module = concat!(
            "//! prose\n",
            "//! ```hew\n",
            "//! fn main() {}\n",
            "//! ```\n",
            "fn actual_code() {}\n",
            "```hew\n",
            "not a doc fence\n",
            "```\n",
        );
        assert_eq!(
            extract_fences(module, true),
            vec![("fn main() {}\n".to_string(), false)]
        );
    }

    #[test]
    fn std_fences_are_implicit_hew_and_step_over_a_tagged_block() {
        let module = concat!(
            "//! module prose\n",
            "    /// ```text\n",
            "    /// not hew at all\n",
            "    /// ```\n",
            "    /// ```\n",
            "    /// let value = 1\n",
            "    /// ```\n",
        );
        assert_eq!(
            extract_std_fences(module),
            vec![("let value = 1\n".to_string(), false)]
        );
    }

    #[test]
    fn identical_fences_in_one_document_are_still_checked_independently() {
        let mut minted = std::collections::BTreeSet::new();
        let first = mint_fence_id("guide", "same\n", &mut minted);
        let second = mint_fence_id("guide", "same\n", &mut minted);
        assert_ne!(first, second);
        assert_eq!(second, format!("{first}-2"));
    }

    fn ledger(rows: &[(&str, &str)]) -> BTreeMap<String, String> {
        rows.iter()
            .map(|(id, reason)| (id.to_string(), reason.to_string()))
            .collect()
    }

    #[test]
    fn a_listed_case_that_now_passes_is_red_because_ratchets_only_shrink() {
        let ledger = ledger(&[("listed", "issue #1")]);
        let verdicts = [("listed", true), ("unlisted", true)];
        let result = classify(&verdicts, &ledger);
        assert_eq!(result.now_passing, ["listed"]);
        assert!(result.failed.is_empty() && result.known.is_empty());
    }

    #[test]
    fn an_unlisted_failure_is_red_and_a_listed_one_is_known() {
        let ledger = ledger(&[("listed", "issue #1")]);
        let verdicts = [("listed", false), ("unlisted", false)];
        let result = classify(&verdicts, &ledger);
        assert_eq!(result.failed, ["unlisted"]);
        assert_eq!(result.known, ["listed"]);
        assert!(result.now_passing.is_empty());
    }

    fn ledger_root(body: &str) -> tempfile::TempDir {
        let directory = tempfile::tempdir().unwrap();
        fs::create_dir_all(directory.path().join("tests/core-acceptance")).unwrap();
        fs::write(directory.path().join(EXPECTED_FAILURES_PATH), body).unwrap();
        directory
    }

    #[test]
    fn a_row_naming_no_case_is_refused_rather_than_ignored() {
        let directory = ledger_root("ghost  # issue #1\n");
        let error = load_expected_failures(directory.path(), &manifest())
            .expect_err("a row for a case that no longer exists must be refused");
        assert!(error.contains("is not a core-acceptance case"), "{error}");
    }

    #[test]
    fn a_row_without_a_reason_is_refused() {
        let directory = ledger_root("acceptance-case\n");
        let error = load_expected_failures(directory.path(), &manifest())
            .expect_err("a row with no reason must be refused");
        assert!(error.contains("has no reason"), "{error}");
    }

    #[test]
    fn a_ledger_row_is_read_with_its_reason() {
        let directory = ledger_root("# a comment\n\nacceptance-case  # issue #3001\n");
        let rows = load_expected_failures(directory.path(), &manifest()).unwrap();
        assert_eq!(rows["acceptance-case"], "issue #3001");
    }

    // -----------------------------------------------------------------
    // Expectation kind: `check`
    // -----------------------------------------------------------------

    fn make_case(
        kind: CaseKind,
        diagnostics: Vec<ExpectedDiagnostic>,
        suites: &[&str],
        source_rel: &str,
    ) -> Case {
        Case {
            id: "case-under-test".to_string(),
            intent: "unit test".to_string(),
            source: PathBuf::from(source_rel),
            fixtures: None,
            suites: suites.iter().map(ToString::to_string).collect(),
            timeout_seconds: 1,
            kind,
            env: BTreeMap::new(),
            fences: None,
            expected: ExpectedOutcome {
                stdout: None,
                stderr: String::new(),
                exit: None,
                diagnostics,
            },
        }
    }

    fn manifest_root_with_source(source_rel: &str) -> tempfile::TempDir {
        let directory = tempfile::tempdir().expect("temporary directory");
        let source = directory
            .path()
            .join("tests/core-acceptance")
            .join(source_rel);
        fs::create_dir_all(source.parent().expect("source has a parent")).unwrap();
        fs::write(&source, "fn main() {}\n").unwrap();
        directory
    }

    #[test]
    fn case_without_kind_defaults_to_run() {
        assert_eq!(manifest().cases[0].kind, CaseKind::Run);
    }

    #[test]
    fn run_case_rejects_declared_expected_diagnostics() {
        let directory = manifest_root_with_source("cases/case.hew");
        let case = make_case(
            CaseKind::Run,
            vec![ExpectedDiagnostic {
                code: "InvalidOperation".to_string(),
                line: 1,
                column: 1,
                message: None,
                file: None,
            }],
            &["acceptance"],
            "cases/case.hew",
        );
        let manifest = Manifest { cases: vec![case] };
        let error = validate_manifest(&manifest, directory.path())
            .expect_err("a run case declaring diagnostics is a check-only shape");
        assert!(error.contains("check-only"));
    }

    #[test]
    fn run_case_requires_expected_stdout_and_exit() {
        // `stdout`/`exit` are `Option`, not default-empty/default-zero:
        // a run case that forgets one must fail validation rather than
        // silently comparing against "" / 0 and passing when the program
        // happens to match by coincidence.
        let directory = manifest_root_with_source("cases/case.hew");
        let case = make_case(CaseKind::Run, Vec::new(), &["acceptance"], "cases/case.hew");
        let manifest = Manifest { cases: vec![case] };
        let error = validate_manifest(&manifest, directory.path())
            .expect_err("a run case missing stdout/exit must fail validation");
        assert!(error.contains("must declare both expected stdout and exit"));
    }

    #[test]
    fn check_case_requires_expected_diagnostics() {
        let directory = manifest_root_with_source("cases/case.hew");
        let case = make_case(
            CaseKind::Check,
            Vec::new(),
            &["acceptance"],
            "cases/case.hew",
        );
        let manifest = Manifest { cases: vec![case] };
        let error = validate_manifest(&manifest, directory.path())
            .expect_err("a check case with no expected diagnostics must fail validation");
        assert!(error.contains("declares no expected diagnostics"));
    }

    #[test]
    fn check_case_cannot_join_the_safety_suite() {
        let directory = manifest_root_with_source("cases/case.hew");
        let case = make_case(
            CaseKind::Check,
            vec![ExpectedDiagnostic {
                code: "InvalidOperation".to_string(),
                line: 1,
                column: 1,
                message: None,
                file: None,
            }],
            &["acceptance", "safety"],
            "cases/case.hew",
        );
        let manifest = Manifest { cases: vec![case] };
        let error = validate_manifest(&manifest, directory.path())
            .expect_err("safety stays a suite, not a case kind");
        assert!(error.contains("safety stays a suite"));
    }

    #[test]
    fn diagnostics_match_accepts_an_exact_set() {
        let actual = vec![ActualDiagnostic {
            code: "InvalidOperation".to_string(),
            span: ActualSpan {
                start_line: 10,
                start_col: 9,
            },
            message: "a select needs at least one arm: a source arm".to_string(),
            file: Some("cases/probe.hew".to_string()),
        }];
        let expected = vec![ExpectedDiagnostic {
            code: "InvalidOperation".to_string(),
            line: 10,
            column: 9,
            message: Some("needs at least one arm".to_string()),
            file: Some("probe.hew".to_string()),
        }];
        assert!(diagnostics_match(&expected, &actual, true).is_ok());
    }

    #[test]
    fn diagnostics_match_rejects_wrong_position() {
        let actual = vec![ActualDiagnostic {
            code: "InvalidOperation".to_string(),
            span: ActualSpan {
                start_line: 10,
                start_col: 9,
            },
            message: "a select needs at least one arm".to_string(),
            file: None,
        }];
        let wrong_line = vec![ExpectedDiagnostic {
            code: "InvalidOperation".to_string(),
            line: 11,
            column: 9,
            message: None,
            file: None,
        }];
        let error = diagnostics_match(&wrong_line, &actual, true)
            .expect_err("naming the wrong line must fail rather than silently pass");
        assert!(error.contains("missing"));
        assert!(error.contains("extra"));
    }

    /// A multi-file case pins provenance, so naming the wrong file must fail
    /// even when the code, line and column all match.
    #[test]
    fn diagnostics_match_rejects_the_wrong_file() {
        let actual = vec![ActualDiagnostic {
            code: "ResourceBoundaryParamMustConsume".to_string(),
            span: ActualSpan {
                start_line: 13,
                start_col: 5,
            },
            message: "must pin its disposition".to_string(),
            file: Some("/checkout/cases/main.hew".to_string()),
        }];
        let expected = vec![ExpectedDiagnostic {
            code: "ResourceBoundaryParamMustConsume".to_string(),
            line: 13,
            column: 5,
            message: None,
            file: Some("cases/token.hew".to_string()),
        }];
        let error = diagnostics_match(&expected, &actual, true)
            .expect_err("a diagnostic reported against another file must fail the case");
        assert!(error.contains("cases/token.hew"));
    }

    #[test]
    fn diagnostics_match_rejects_an_unexpected_extra_diagnostic() {
        let actual = vec![ActualDiagnostic {
            code: "UnusedVariable".to_string(),
            span: ActualSpan {
                start_line: 4,
                start_col: 13,
            },
            message: "unused variable seen".to_string(),
            file: None,
        }];
        let error = diagnostics_match(&[], &actual, true)
            .expect_err("an undeclared extra diagnostic must fail the case");
        assert!(error.contains("extra"));
    }

    #[cfg(unix)]
    #[test]
    fn check_case_with_a_wrong_position_fails_the_runner() {
        use std::os::unix::fs::PermissionsExt;
        let directory = manifest_root_with_source("cases/probe.hew");
        let fake_hew = directory.path().join("fake-hew");
        let script = concat!(
            "#!/bin/sh\n",
            "cat <<'JSON'\n",
            "[{",
            r#""code":"InvalidOperation","severity":"error","channel":"user","#,
            r#""source":"hew-types","file":"probe.hew","#,
            r#""span":{"start_line":10,"start_col":9,"end_line":10,"end_col":18,"start_byte":0,"end_byte":0},"#,
            r#""message":"a select needs at least one arm","notes":[],"fixes":[]"#,
            "}]\n",
            "JSON\n",
            "exit 1\n",
        );
        fs::write(&fake_hew, script).unwrap();
        fs::set_permissions(&fake_hew, fs::Permissions::from_mode(0o700)).unwrap();

        let options = Options {
            suite: "acceptance".to_string(),
            cases: Vec::new(),
            kinds: Vec::new(),
            jobs: 1,
            hew_bin: fake_hew,
            timeout_seconds: None,
        };
        let runner = Runner {
            options: &options,
            root: directory.path(),
            run_dir: directory.path(),
            instrumentation_request: "none",
        };

        let correct_case = make_case(
            CaseKind::Check,
            vec![ExpectedDiagnostic {
                code: "InvalidOperation".to_string(),
                line: 10,
                column: 9,
                message: None,
                file: None,
            }],
            &["acceptance"],
            "cases/probe.hew",
        );
        assert!(
            runner.run_check(&correct_case, false, &mut String::new()),
            "a check case whose expectation matches the real diagnostic position must pass"
        );

        let wrong_position_case = make_case(
            CaseKind::Check,
            vec![ExpectedDiagnostic {
                code: "InvalidOperation".to_string(),
                line: 99,
                column: 9,
                message: None,
                file: None,
            }],
            &["acceptance"],
            "cases/probe.hew",
        );
        assert!(
            !runner.run_check(&wrong_position_case, false, &mut String::new()),
            "a check case naming the wrong position must fail the runner, not pass it"
        );
    }
}
