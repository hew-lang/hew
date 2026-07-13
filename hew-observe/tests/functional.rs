//! Functional profiler/observe HTTP harness for `hew observe`.
//!
//! `hew-observe` is intentionally TUI-only in non-demo mode: it has no batch or
//! JSON output flag and exits when stdout is not a TTY. This harness therefore
//! exercises the documented profiler HTTP endpoints that the TUI consumes.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use serde::Deserialize;

const OBSERVE_SCHEMA_VERSION: &str = "v0.5";
const FIXTURE_COUNTER_INCREMENTS: u64 = 7;
const EXPECTED_COUNTER_MESSAGES: u64 = FIXTURE_COUNTER_INCREMENTS + 1;
const EXPECTED_PINGER_MESSAGES: u64 = 1;
const EXPECTED_ACTORS_LIVE: u64 = 2;
const EXPECTED_ACTOR_TURNS: u64 = EXPECTED_COUNTER_MESSAGES + EXPECTED_PINGER_MESSAGES;
const EXPECTED_TASKS_SPAWNED: u64 = EXPECTED_ACTORS_LIVE;

#[derive(Debug, Deserialize)]
struct Envelope<T> {
    schema_version: String,
    data: T,
}

#[derive(Debug, Default, Deserialize)]
struct Metrics {
    #[serde(default)]
    tasks_spawned: u64,
    #[serde(default)]
    tasks_completed: u64,
    #[serde(default)]
    messages_sent: u64,
    #[serde(default)]
    messages_received: u64,
    #[serde(default)]
    alloc_count: u64,
    #[serde(default)]
    dealloc_count: u64,
    #[serde(default)]
    bytes_allocated: u64,
    #[serde(default)]
    bytes_freed: u64,
    #[serde(default)]
    bytes_live: u64,
    #[serde(default)]
    peak_bytes_live: u64,
}

#[derive(Debug, Deserialize)]
struct ActorInfo {
    #[serde(default)]
    actor_type: String,
    #[serde(default)]
    msgs: u64,
}

#[derive(Debug, Default)]
struct Snapshot {
    metrics: Metrics,
    actors: Vec<ActorInfo>,
    scrape: String,
}

struct ChildGuard {
    child: Option<Child>,
}

struct BuiltFixture {
    _dir: tempfile::TempDir,
    binary: PathBuf,
}

impl ChildGuard {
    fn new(child: Child) -> Self {
        Self { child: Some(child) }
    }

    fn output_after_kill(mut self) -> String {
        let Some(mut child) = self.child.take() else {
            return "child process already collected".to_owned();
        };
        let _ = child.kill();
        match child.wait_with_output() {
            Ok(output) => format!(
                "stdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            ),
            Err(error) => format!("failed to collect child output after kill: {error}"),
        }
    }
}

impl Drop for ChildGuard {
    fn drop(&mut self) {
        if let Some(child) = &mut self.child {
            if matches!(child.try_wait(), Ok(None)) {
                let _ = child.kill();
                let _ = child.wait();
            }
        }
    }
}

#[test]
#[ignore = "run by `make observe-functional-test` with built compiler artifacts"]
fn profiler_endpoint_captures_fixture_observability_data() {
    let fixture = build_fixture_binary();
    let port = reserve_local_port();
    let base_url = format!("http://127.0.0.1:{port}");
    let child = ChildGuard::new(
        Command::new(&fixture.binary)
            .env("HEW_PPROF", format!("127.0.0.1:{port}"))
            .env("HEW_OBSERVE", "hot")
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap_or_else(|error| {
                panic!("failed to start {}: {error}", fixture.binary.display())
            }),
    );

    let client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_millis(750))
        .build()
        .expect("build HTTP client");

    match wait_for_valid_snapshot(&client, &base_url) {
        Ok(snapshot) => assert_snapshot_has_expected_data(&snapshot),
        Err(error) => {
            panic!("{error}\n{}", child.output_after_kill());
        }
    }
}

#[test]
#[ignore = "run by `make observe-functional-test` with the positive endpoint test"]
fn observe_snapshot_validation_rejects_no_data() {
    let snapshot = Snapshot {
        scrape: "actors_live 0\nactors_turns_total 0\nheap_live_bytes 0\n".to_owned(),
        ..Snapshot::default()
    };

    let error = validate_snapshot(&snapshot).expect_err("empty observe data must be rejected");
    assert_eq!(error, "expected exactly 1 Counter actor, observed 0");
}

fn build_fixture_binary() -> BuiltFixture {
    ensure_compiler_artifacts();

    let out_dir = tempfile::tempdir().expect("create fixture output directory");
    let binary = out_dir.path().join(format!(
        "observe-test-fixture{}",
        std::env::consts::EXE_SUFFIX
    ));
    let fixture = repo_root().join("examples/observe-test-fixture.hew");
    let hew = hew_binary_path();
    let output = Command::new(&hew)
        .args(["build"])
        .arg(&fixture)
        .args(["-o"])
        .arg(&binary)
        .current_dir(repo_root())
        .output()
        .unwrap_or_else(|error| panic!("failed to run {} build: {error}", hew.display()));

    assert!(
        output.status.success(),
        "failed to build observe fixture with {}\nstdout:\n{}\nstderr:\n{}",
        hew.display(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    BuiltFixture {
        _dir: out_dir,
        binary,
    }
}

fn ensure_compiler_artifacts() {
    let hew = hew_binary_path();
    if !hew.is_file() {
        let mut command = Command::new("cargo");
        command
            .args(["build", "-p", "hew-cli"])
            .current_dir(repo_root());
        if !cfg!(debug_assertions) {
            command.arg("--release");
        }
        let output = command
            .output()
            .expect("failed to run cargo build for hew compiler artifacts");
        assert!(
            output.status.success(),
            "failed to bootstrap hew compiler artifacts\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            hew.is_file(),
            "cargo build succeeded but {} was not created",
            hew.display()
        );
    }
    hew_testutil::ensure_hew_lib_built().expect("build libhew.a");
}

fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-observe crate should live under the repository root")
}

fn target_dir() -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR").map_or_else(|| repo_root().join("target"), PathBuf::from)
}

fn profile_dir() -> &'static str {
    if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    }
}

fn hew_binary_path() -> PathBuf {
    if let Some(path) = std::env::var_os("CARGO_BIN_EXE_hew") {
        return PathBuf::from(path);
    }
    target_dir()
        .join(profile_dir())
        .join(format!("hew{}", std::env::consts::EXE_SUFFIX))
}

fn reserve_local_port() -> u16 {
    std::net::TcpListener::bind("127.0.0.1:0")
        .expect("reserve local profiler port")
        .local_addr()
        .expect("read reserved profiler port")
        .port()
}

fn wait_for_valid_snapshot(
    client: &reqwest::blocking::Client,
    base_url: &str,
) -> Result<Snapshot, String> {
    let deadline = Instant::now() + Duration::from_secs(10);
    let mut last_error = "profiler endpoint was not contacted".to_owned();
    while Instant::now() < deadline {
        match fetch_snapshot(client, base_url).and_then(|snapshot| {
            validate_snapshot(&snapshot)?;
            Ok(snapshot)
        }) {
            Ok(snapshot) => return Ok(snapshot),
            Err(error) => last_error = error,
        }
        std::thread::sleep(Duration::from_millis(100));
    }
    Err(format!(
        "timed out waiting for valid observe data from {base_url}: {last_error}"
    ))
}

fn fetch_snapshot(client: &reqwest::blocking::Client, base_url: &str) -> Result<Snapshot, String> {
    Ok(Snapshot {
        metrics: fetch_json(client, base_url, "/api/metrics")?,
        actors: fetch_json(client, base_url, "/api/actors")?,
        scrape: fetch_text(client, base_url, "/api/observe/scrape")?,
    })
}

fn fetch_json<T: serde::de::DeserializeOwned>(
    client: &reqwest::blocking::Client,
    base_url: &str,
    path: &str,
) -> Result<T, String> {
    let url = format!("{base_url}{path}");
    let response = client
        .get(&url)
        .send()
        .map_err(|error| format!("GET {url} failed: {error}"))?
        .error_for_status()
        .map_err(|error| format!("GET {url} returned non-success status: {error}"))?;
    let envelope: Envelope<T> = response
        .json()
        .map_err(|error| format!("GET {url} returned invalid envelope JSON: {error}"))?;
    if envelope.schema_version != OBSERVE_SCHEMA_VERSION {
        return Err(format!(
            "GET {url} schema_version mismatch: expected {OBSERVE_SCHEMA_VERSION}, got {}",
            envelope.schema_version
        ));
    }
    Ok(envelope.data)
}

fn fetch_text(
    client: &reqwest::blocking::Client,
    base_url: &str,
    path: &str,
) -> Result<String, String> {
    let url = format!("{base_url}{path}");
    client
        .get(&url)
        .send()
        .map_err(|error| format!("GET {url} failed: {error}"))?
        .error_for_status()
        .map_err(|error| format!("GET {url} returned non-success status: {error}"))?
        .text()
        .map_err(|error| format!("GET {url} returned invalid text: {error}"))
}

fn validate_snapshot(snapshot: &Snapshot) -> Result<(), String> {
    let counter = exactly_one_actor(&snapshot.actors, "Counter")?;
    if counter.msgs != EXPECTED_COUNTER_MESSAGES {
        return Err(format!(
            "Counter processed {} messages, expected exactly {EXPECTED_COUNTER_MESSAGES}",
            counter.msgs
        ));
    }

    let pinger = exactly_one_actor(&snapshot.actors, "Pinger")?;
    if pinger.msgs != EXPECTED_PINGER_MESSAGES {
        return Err(format!(
            "Pinger processed {} messages, expected exactly {EXPECTED_PINGER_MESSAGES}",
            pinger.msgs
        ));
    }

    validate_scheduler_metrics(&snapshot.metrics)?;
    validate_memory_metrics(&snapshot.metrics)?;
    validate_scrape_metrics(&snapshot.scrape)?;
    Ok(())
}

fn exactly_one_actor<'a>(
    actors: &'a [ActorInfo],
    actor_type: &str,
) -> Result<&'a ActorInfo, String> {
    let matches = actors
        .iter()
        .filter(|actor| actor.actor_type == actor_type)
        .collect::<Vec<_>>();
    if matches.len() != 1 {
        return Err(format!(
            "expected exactly 1 {actor_type} actor, observed {}",
            matches.len()
        ));
    }
    Ok(matches[0])
}

fn validate_scheduler_metrics(metrics: &Metrics) -> Result<(), String> {
    if metrics.messages_sent < EXPECTED_ACTOR_TURNS {
        return Err(format!(
            "scheduler messages_sent={} below fixture boundary {EXPECTED_ACTOR_TURNS}",
            metrics.messages_sent
        ));
    }
    if metrics.messages_received < EXPECTED_ACTOR_TURNS {
        return Err(format!(
            "scheduler messages_received={} below fixture boundary {EXPECTED_ACTOR_TURNS}",
            metrics.messages_received
        ));
    }
    if metrics.tasks_spawned < EXPECTED_TASKS_SPAWNED {
        return Err(format!(
            "scheduler tasks_spawned={} below fixture boundary {EXPECTED_TASKS_SPAWNED}",
            metrics.tasks_spawned
        ));
    }
    if metrics.tasks_completed > metrics.tasks_spawned {
        return Err(format!(
            "scheduler tasks_completed={} exceeded tasks_spawned={}",
            metrics.tasks_completed, metrics.tasks_spawned
        ));
    }
    Ok(())
}

fn validate_memory_metrics(metrics: &Metrics) -> Result<(), String> {
    if metrics.peak_bytes_live == 0 {
        return Err("memory peak_bytes_live stayed at 0".to_owned());
    }
    if metrics.bytes_allocated < metrics.bytes_freed {
        return Err(format!(
            "memory bytes_allocated={} below bytes_freed={}",
            metrics.bytes_allocated, metrics.bytes_freed
        ));
    }
    if metrics.alloc_count < metrics.dealloc_count {
        return Err(format!(
            "memory alloc_count={} below dealloc_count={}",
            metrics.alloc_count, metrics.dealloc_count
        ));
    }
    if metrics.peak_bytes_live < metrics.bytes_live {
        return Err(format!(
            "memory peak_bytes_live={} below bytes_live={}",
            metrics.peak_bytes_live, metrics.bytes_live
        ));
    }
    Ok(())
}

fn validate_scrape_metrics(scrape: &str) -> Result<(), String> {
    let values = scrape_values(scrape);
    expect_scrape_value(&values, "actors_live", EXPECTED_ACTORS_LIVE)?;
    expect_scrape_value(&values, "actors_turns_total", EXPECTED_ACTOR_TURNS)?;
    expect_scrape_value(
        &values,
        "actors_attributed_turns_total",
        EXPECTED_ACTOR_TURNS,
    )?;
    expect_handler_turns(scrape, "Counter::increment", FIXTURE_COUNTER_INCREMENTS)?;
    expect_handler_turns(scrape, "Counter::total", 1)?;
    expect_handler_turns(scrape, "Pinger::ping", 1)?;
    Ok(())
}

fn scrape_values(scrape: &str) -> BTreeMap<String, u64> {
    scrape
        .lines()
        .filter_map(|line| {
            if line.starts_with('#') || line.contains('{') {
                return None;
            }
            let (name, value) = line.split_once(' ')?;
            let parsed = value.parse::<u64>().ok()?;
            Some((name.to_owned(), parsed))
        })
        .collect()
}

fn expect_scrape_value(
    values: &BTreeMap<String, u64>,
    name: &str,
    expected: u64,
) -> Result<(), String> {
    match values.get(name) {
        Some(actual) if *actual == expected => Ok(()),
        Some(actual) => Err(format!(
            "scrape metric {name}={actual}, expected exactly {expected}"
        )),
        None => Err(format!("scrape metric {name} missing")),
    }
}

fn expect_handler_turns(scrape: &str, handler: &str, expected: u64) -> Result<(), String> {
    let prefix = "actors_attributed_turns_by_handler_total{";
    let needle = format!("handler=\"{handler}\"");
    let mut observed = Vec::new();
    for line in scrape.lines() {
        if line.starts_with(prefix) && line.contains(&needle) {
            let (_, value) = line
                .rsplit_once(' ')
                .ok_or_else(|| format!("handler line has no value: {line}"))?;
            observed.push(
                value
                    .parse::<u64>()
                    .map_err(|error| format!("handler line value is not u64: {line}: {error}"))?,
            );
        }
    }
    match observed.as_slice() {
        [actual] if *actual == expected => Ok(()),
        [actual] => Err(format!(
            "handler {handler} turns={actual}, expected exactly {expected}"
        )),
        [] => Err(format!("handler {handler} turn series missing")),
        many => Err(format!(
            "handler {handler} emitted {} turn series, expected exactly 1",
            many.len()
        )),
    }
}

fn assert_snapshot_has_expected_data(snapshot: &Snapshot) {
    validate_snapshot(snapshot).expect("snapshot should already have been validated");
}
