use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use serde::Deserialize;
use serde_json::Value;

const SANDBOX_PROFILE: &str = "sandbox-vm-export";
const DEFERRED_BLOCK_START: &str = "```json sandbox-fixtures-deferred";
const DEFERRED_BLOCK_END: &str = "```";

type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Mode {
    Update,
    Check,
    Probe,
}

#[derive(Debug, Clone)]
struct Options {
    mode: Mode,
    fixtures_dir: PathBuf,
}

#[derive(Debug, Deserialize)]
struct DeferredManifest {
    deferred: Vec<DeferredFixture>,
}

#[derive(Debug, Clone, Deserialize)]
struct DeferredFixture {
    fixture: String,
    feature: String,
    #[serde(default)]
    compiler_version: Option<String>,
    reason: String,
}

#[derive(Debug)]
struct FixtureDir {
    name: String,
    path: PathBuf,
    has_main: bool,
    has_bytecode: bool,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("error: {err}");
        std::process::exit(1);
    }
}

fn run() -> Result<()> {
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    let options = parse_options(&args)?;
    let repo_root = workspace_root()?;
    std::env::set_var("HEWPATH", &repo_root);

    let deferred = if options.mode == Mode::Probe {
        BTreeMap::new()
    } else {
        load_deferred_manifest(&options.fixtures_dir)?
    };
    let fixtures = discover_fixtures(&options.fixtures_dir)?;
    validate_deferred_entries(&fixtures, &deferred)?;

    let mut failures = Vec::new();
    let mut compiled = 0usize;
    let mut deferred_count = 0usize;
    let mut diagnostics_only = 0usize;

    for fixture in fixtures {
        match process_fixture(&fixture, &options.mode, deferred.get(&fixture.name)) {
            Ok(status) => match status {
                FixtureStatus::Compiled => compiled += 1,
                FixtureStatus::Deferred => deferred_count += 1,
                FixtureStatus::DiagnosticsOnly => diagnostics_only += 1,
                FixtureStatus::Ignored => {}
            },
            Err(err) => failures.push(format!("{}: {err}", fixture.name)),
        }
    }

    println!(
        "sandbox fixtures: {compiled} compiled, {deferred_count} deferred, {diagnostics_only} diagnostics-only"
    );

    if failures.is_empty() {
        Ok(())
    } else {
        Err(format!(
            "{} fixture error(s):\n{}",
            failures.len(),
            failures
                .iter()
                .map(|failure| format!("  - {failure}"))
                .collect::<Vec<_>>()
                .join("\n")
        ))
    }
}

fn parse_options(args: &[String]) -> Result<Options> {
    if args.first().map(String::as_str) != Some("sandbox-fixtures") {
        return Err("usage: cargo run -p xtask -- sandbox-fixtures [--check|--probe] [--fixtures-dir <path>]".to_string());
    }

    let mut mode = Mode::Update;
    let mut fixtures_dir = std::env::var_os("HEW_SANDBOX_FIXTURES_DIR").map(PathBuf::from);
    let mut index = 1usize;
    while index < args.len() {
        match args[index].as_str() {
            "--check" => {
                mode = Mode::Check;
                index += 1;
            }
            "--probe" => {
                mode = Mode::Probe;
                index += 1;
            }
            "--fixtures-dir" => {
                let Some(path) = args.get(index + 1) else {
                    return Err("--fixtures-dir requires a path".to_string());
                };
                fixtures_dir = Some(PathBuf::from(path));
                index += 2;
            }
            "--help" | "-h" => {
                println!(
                    "usage: cargo run -p xtask -- sandbox-fixtures [--check|--probe] [--fixtures-dir <path>]"
                );
                std::process::exit(0);
            }
            arg => return Err(format!("unknown sandbox-fixtures option: {arg}")),
        }
    }

    let fixtures_dir = fixtures_dir.unwrap_or(workspace_root()?.join("hew-sandbox-vm/fixtures"));
    Ok(Options { mode, fixtures_dir })
}

fn workspace_root() -> Result<PathBuf> {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .map(Path::to_path_buf)
        .ok_or_else(|| "xtask manifest should have a workspace parent".to_string())
}

fn load_deferred_manifest(fixtures_dir: &Path) -> Result<BTreeMap<String, DeferredFixture>> {
    let readme_path = fixtures_dir.join("README.md");
    let readme = fs::read_to_string(&readme_path)
        .map_err(|err| format!("read {}: {err}", readme_path.display()))?;
    let json = extract_deferred_json(&readme).ok_or_else(|| {
        format!(
            "{} is missing the {DEFERRED_BLOCK_START:?} fenced block",
            readme_path.display()
        )
    })?;
    let manifest: DeferredManifest = serde_json::from_str(&json)
        .map_err(|err| format!("parse deferred fixture block: {err}"))?;

    let mut deferred = BTreeMap::new();
    for entry in manifest.deferred {
        if entry.fixture.is_empty() {
            return Err("deferred fixture entries require a fixture id".to_string());
        }
        if entry.feature.is_empty() || entry.reason.is_empty() {
            return Err(format!(
                "deferred fixture {} requires feature and reason",
                entry.fixture
            ));
        }
        if let Some(version) = &entry.compiler_version {
            if !version.starts_with("handcrafted-pending-") {
                return Err(format!(
                    "deferred fixture {} marker must start with handcrafted-pending-",
                    entry.fixture
                ));
            }
        }
        if deferred.insert(entry.fixture.clone(), entry).is_some() {
            return Err("duplicate deferred fixture entry".to_string());
        }
    }
    Ok(deferred)
}

fn extract_deferred_json(readme: &str) -> Option<String> {
    let mut in_block = false;
    let mut lines = Vec::new();
    for line in readme.lines() {
        if in_block {
            if line.trim() == DEFERRED_BLOCK_END {
                return Some(lines.join("\n"));
            }
            lines.push(line);
        } else if line.trim() == DEFERRED_BLOCK_START {
            in_block = true;
        }
    }
    None
}

fn discover_fixtures(fixtures_dir: &Path) -> Result<Vec<FixtureDir>> {
    let mut fixtures = Vec::new();
    for entry in fs::read_dir(fixtures_dir)
        .map_err(|err| format!("read fixtures dir {}: {err}", fixtures_dir.display()))?
    {
        let entry = entry.map_err(|err| format!("read fixtures dir entry: {err}"))?;
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let Some(name) = path.file_name().and_then(OsStr::to_str) else {
            continue;
        };
        fixtures.push(FixtureDir {
            name: name.to_string(),
            has_main: path.join("main.hew").is_file(),
            has_bytecode: path.join("bytecode.json").is_file(),
            path,
        });
    }
    fixtures.sort_by(|left, right| left.name.cmp(&right.name));
    Ok(fixtures)
}

fn validate_deferred_entries(
    fixtures: &[FixtureDir],
    deferred: &BTreeMap<String, DeferredFixture>,
) -> Result<()> {
    let fixture_names: BTreeSet<_> = fixtures
        .iter()
        .map(|fixture| fixture.name.as_str())
        .collect();
    for name in deferred.keys() {
        if !fixture_names.contains(name.as_str()) {
            return Err(format!("deferred fixture {name} does not exist"));
        }
    }
    Ok(())
}

#[derive(Debug, Clone, Copy)]
enum FixtureStatus {
    Compiled,
    Deferred,
    DiagnosticsOnly,
    Ignored,
}

fn process_fixture(
    fixture: &FixtureDir,
    mode: &Mode,
    deferred: Option<&DeferredFixture>,
) -> Result<FixtureStatus> {
    if *mode != Mode::Probe {
        if let Some(entry) = deferred {
            return process_deferred_fixture(fixture, mode, entry);
        }
    }

    if !fixture.has_main {
        if fixture.has_bytecode {
            return Err(
                "bytecode fixture has no main.hew and is not listed as deferred".to_string(),
            );
        }
        return Ok(FixtureStatus::Ignored);
    }

    let source_path = fixture.path.join("main.hew");
    let source = fs::read_to_string(&source_path)
        .map_err(|err| format!("read {}: {err}", source_path.display()))?;
    let output = hew_sandbox_wasm::compile_to_sandbox_bytecode(&source, Some(SANDBOX_PROFILE))
        .map_err(|err| format!("compile threw: {err}"))?;

    if output
        .diagnostics
        .iter()
        .any(|diag| diag.severity == "error")
    {
        let diagnostics = output
            .diagnostics
            .iter()
            .filter(|diag| diag.severity == "error")
            .map(|diag| format!("{}:{}: {}", diag.phase, diag.kind, diag.message))
            .collect::<Vec<_>>()
            .join("; ");
        if fixture.has_bytecode || *mode == Mode::Probe {
            return Err(format!("compiler produced diagnostics: {diagnostics}"));
        }
        println!("DIAGNOSTIC {} {diagnostics}", fixture.name);
        return Ok(FixtureStatus::DiagnosticsOnly);
    }

    let Some(bytecode) = output.bytecode else {
        if fixture.has_bytecode || *mode == Mode::Probe {
            return Err("compiler produced no diagnostics and no bytecode".to_string());
        }
        println!("DIAGNOSTIC {} compiler produced no bytecode", fixture.name);
        return Ok(FixtureStatus::DiagnosticsOnly);
    };

    let expected =
        serde_json::to_value(&bytecode).map_err(|err| format!("serialize bytecode: {err}"))?;
    let bytecode_path = fixture.path.join("bytecode.json");

    match mode {
        Mode::Probe => {
            println!("COMPILES {} {}", fixture.name, bytecode.compiler_version);
        }
        Mode::Check => {
            let current = read_json(&bytecode_path)?;
            if current != expected {
                return Err("bytecode.json is out of date; run make sandbox-fixtures".to_string());
            }
            println!("OK {}", fixture.name);
        }
        Mode::Update => {
            if write_json_if_changed(&bytecode_path, &expected)? {
                println!("WROTE {}", fixture.name);
            } else {
                println!("OK {}", fixture.name);
            }
        }
    }

    Ok(FixtureStatus::Compiled)
}

fn process_deferred_fixture(
    fixture: &FixtureDir,
    mode: &Mode,
    entry: &DeferredFixture,
) -> Result<FixtureStatus> {
    if !fixture.has_bytecode {
        println!("DEFERRED {} {} (no bytecode)", fixture.name, entry.feature);
        return Ok(FixtureStatus::Deferred);
    }

    let marker = entry.compiler_version.as_ref().ok_or_else(|| {
        format!(
            "deferred fixture {} has bytecode but no compiler_version marker",
            fixture.name
        )
    })?;
    let bytecode_path = fixture.path.join("bytecode.json");
    let mut current = read_json(&bytecode_path)?;
    let Some(object) = current.as_object_mut() else {
        return Err("bytecode.json root must be an object".to_string());
    };

    match object.get("compiler_version").and_then(Value::as_str) {
        Some(version) if version == marker => {}
        Some(version) if mode == &Mode::Update && version.contains("-fixture") => {
            object.insert(
                "compiler_version".to_string(),
                Value::String(marker.clone()),
            );
            write_json_if_changed(&bytecode_path, &current)?;
        }
        Some(version) => {
            return Err(format!(
                "deferred compiler_version must be {marker:?}, got {version:?}"
            ));
        }
        None => return Err("bytecode.json is missing compiler_version".to_string()),
    }

    println!("DEFERRED {} {}", fixture.name, entry.feature);
    Ok(FixtureStatus::Deferred)
}

fn read_json(path: &Path) -> Result<Value> {
    let contents =
        fs::read_to_string(path).map_err(|err| format!("read {}: {err}", path.display()))?;
    serde_json::from_str(&contents).map_err(|err| format!("parse {}: {err}", path.display()))
}

fn write_json_if_changed(path: &Path, value: &Value) -> Result<bool> {
    let next = format!(
        "{}\n",
        serde_json::to_string_pretty(value)
            .map_err(|err| format!("serialize {}: {err}", path.display()))?
    );
    match fs::read_to_string(path) {
        Ok(current) if current == next => Ok(false),
        Ok(_) | Err(_) => {
            fs::write(path, next).map_err(|err| format!("write {}: {err}", path.display()))?;
            Ok(true)
        }
    }
}
