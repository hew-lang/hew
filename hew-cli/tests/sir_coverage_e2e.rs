//! `hew tool sir-coverage` contracts: the dev-only inventory of which root
//! items the SIR route admits and which the legacy lowerer still owns.
//!
//! The inventory asks every admitted header for its body (not only what the
//! entry reaches), names the refusal for everything else, counts every
//! non-function item as legacy, and can hold a percentage ratchet.

mod support;

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use support::{describe_output, hew_binary, repo_root};

/// Two admitted bodies, one refused body the entry never calls, and one
/// refused header. `stranded` is deliberately unreachable from `main`: under
/// entry demand it would be "not reached", so its refusal line proves the
/// tool asks for every body.
const SCALAR_MIX: &str = r"
fn main() -> i64 {
    if twice(20) == 40 {
        0
    } else {
        1
    }
}

fn twice(value: i64) -> i64 {
    value + value
}

fn stranded(value: i64) -> i64 {
    var total = value;
    total = total + 1;
    total
}

fn greet(name: string) -> i64 {
    name.len()
}
";

const ACTOR_PROGRAM: &str = r"
actor Counter {
    var count: i64 = 0;

    receive fn bump() {
        count = count + 1;
    }
}

fn main() -> i64 {
    let counter = spawn Counter;
    counter.bump();
    0
}
";

const UNPARSEABLE: &str = r"
fn main( -> i64 {
";

fn coverage(args: &[&str], current_dir: &Path) -> Output {
    let mut command = Command::new(hew_binary());
    command
        .arg("tool")
        .arg("sir-coverage")
        .args(args)
        .current_dir(current_dir);
    support::run_bounded_command(command, format!("hew tool sir-coverage {}", args.join(" ")))
}

fn lines_for<'a>(stdout: &'a str, file: &str) -> Vec<&'a str> {
    stdout
        .lines()
        .filter(|line| line.starts_with(file))
        .collect()
}

#[test]
fn admitted_bodies_report_sir_and_every_refusal_names_its_reason() {
    let dir = support::tempdir();
    fs::write(dir.path().join("mix.hew"), SCALAR_MIX).expect("write fixture");

    let output = coverage(&["mix.hew"], dir.path());
    assert!(
        output.status.success(),
        "an inventory over an admissible file must succeed:\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines = lines_for(&stdout, "mix.hew ");
    assert!(
        lines.contains(&"mix.hew main sir") && lines.contains(&"mix.hew twice sir"),
        "admitted scalar bodies must be reported `sir`:\n{stdout}"
    );
    let stranded = lines
        .iter()
        .find(|line| line.starts_with("mix.hew stranded "))
        .unwrap_or_else(|| panic!("the unreachable body must still be inventoried:\n{stdout}"));
    assert!(
        stranded.starts_with("mix.hew stranded legacy: ") && stranded.contains("mutable"),
        "a body the entry never reaches must be demanded and refused with its reason:\n{stdout}"
    );
    let greet = lines
        .iter()
        .find(|line| line.starts_with("mix.hew greet "))
        .unwrap_or_else(|| panic!("the refused header must be inventoried:\n{stdout}"));
    assert!(
        greet.starts_with("mix.hew greet legacy: ") && greet.contains("string"),
        "a refused header must name the offending type, not read as unreached:\n{stdout}"
    );
    assert!(
        stdout
            .lines()
            .last()
            .is_some_and(|line| line == "sir-coverage: 2/4 functions (50.00%)"),
        "the summary must count admitted over inventoried items:\n{stdout}"
    );
}

#[test]
fn an_actor_item_is_counted_as_legacy_by_item_kind() {
    let dir = support::tempdir();
    fs::write(dir.path().join("counter.hew"), ACTOR_PROGRAM).expect("write fixture");

    let output = coverage(&["counter.hew"], dir.path());
    assert!(
        output.status.success(),
        "an inventory over an actor program must succeed:\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout
            .lines()
            .any(|line| line == "counter.hew Counter legacy: item-kind:actor"),
        "an actor has no SIR route and must be counted as legacy, never omitted:\n{stdout}"
    );
    assert!(
        stdout
            .lines()
            .any(|line| line.starts_with("counter.hew main legacy: ")),
        "the entry that spawns the actor is outside the SIR surface:\n{stdout}"
    );
    assert!(
        stdout
            .lines()
            .last()
            .is_some_and(|line| line.starts_with("sir-coverage: 0/")),
        "nothing in an actor program is admitted today:\n{stdout}"
    );
}

#[test]
fn json_output_carries_status_and_reason_per_item() {
    let dir = support::tempdir();
    fs::write(dir.path().join("mix.hew"), SCALAR_MIX).expect("write fixture");

    let output = coverage(&["--json", "mix.hew"], dir.path());
    assert!(output.status.success(), "{}", describe_output(&output));
    let report: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("--json must emit one JSON document");
    let items = report["files"][0]["items"]
        .as_array()
        .expect("one file with an item array");
    let item = |name: &str| {
        items
            .iter()
            .find(|item| item["name"] == name)
            .unwrap_or_else(|| panic!("item `{name}` missing from {report:#}"))
    };
    assert_eq!(item("main")["status"], "sir");
    assert!(item("main").get("reason").is_none());
    assert_eq!(item("greet")["status"], "legacy");
    assert!(item("greet")["reason"]
        .as_str()
        .is_some_and(|reason| reason.contains("string")));
    assert_eq!(report["admitted"], 2);
    assert_eq!(report["total"], 4);
    assert_eq!(report["files_failed"], 0);
}

#[test]
fn a_file_the_frontend_rejects_is_reported_and_excluded_from_the_total() {
    let dir = support::tempdir();
    fs::write(dir.path().join("mix.hew"), SCALAR_MIX).expect("write fixture");
    fs::write(dir.path().join("broken.hew"), UNPARSEABLE).expect("write fixture");

    let output = coverage(&["."], dir.path());
    assert!(
        output.status.success(),
        "one unparseable file must not abort the inventory:\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout
            .lines()
            // The prefix is `.` joined with the file name and renders with
            // the platform separator, so match the tail only.
            .any(|line| line.ends_with("broken.hew - frontend-failed")),
        "the failed file must be named in the inventory:\n{stdout}"
    );
    assert!(
        stdout
            .lines()
            .last()
            .is_some_and(|line| line == "sir-coverage: 2/4 functions (50.00%)"),
        "a failed file contributes no items to either side of the total:\n{stdout}"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("1 file(s) failed the frontend"),
        "the failure count must be announced:\n{stderr}"
    );
}

#[test]
fn ratchet_fails_when_the_recorded_percentage_is_higher_than_measured() {
    let dir = support::tempdir();
    fs::write(dir.path().join("mix.hew"), SCALAR_MIX).expect("write fixture");
    fs::write(dir.path().join("ratchet.txt"), "60.0000\n").expect("write ratchet");

    let output = coverage(&["--ratchet", "ratchet.txt", "mix.hew"], dir.path());
    assert_eq!(
        output.status.code(),
        Some(1),
        "a drop below the ratchet must fail:\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("ratchet dropped")
            && stderr.contains("records 60.0000%")
            && stderr.contains("measured 50.0000%"),
        "the failure must quote both sides:\n{stderr}"
    );
}

#[test]
fn ratchet_holds_when_equal_and_reports_a_rise_without_failing() {
    let dir = support::tempdir();
    fs::write(dir.path().join("mix.hew"), SCALAR_MIX).expect("write fixture");

    fs::write(dir.path().join("ratchet.txt"), "50.0000\n").expect("write ratchet");
    let equal = coverage(&["--ratchet", "ratchet.txt", "mix.hew"], dir.path());
    assert!(
        equal.status.success(),
        "an equal ratchet must hold:\n{}",
        describe_output(&equal)
    );
    assert!(
        String::from_utf8_lossy(&equal.stderr).contains("ratchet holds at 50.0000%"),
        "{}",
        describe_output(&equal)
    );

    fs::write(dir.path().join("ratchet.txt"), "25.0000\n").expect("write ratchet");
    let mut rise = Command::new(hew_binary());
    rise.arg("tool")
        .arg("sir-coverage")
        .arg("--ratchet")
        .arg("ratchet.txt")
        .arg("mix.hew")
        .env_remove("RATCHET_STRICT_RECOVERIES")
        .current_dir(dir.path());
    let rise = support::run_bounded_command(rise, "sir-coverage ratchet rise");
    assert!(
        rise.status.success(),
        "a rise is a recovery to record, never a failure by default:\n{}",
        describe_output(&rise)
    );
    let stderr = String::from_utf8_lossy(&rise.stderr);
    assert!(
        stderr.contains("ratchet can rise") && stderr.contains("update the file to 50.0000"),
        "the rise must say what to record:\n{stderr}"
    );
}

#[test]
fn the_committed_ratchet_is_a_decimal_percentage() {
    let ratchet = repo_root().join("scripts/sir-coverage-ratchet.txt");
    let text = fs::read_to_string(&ratchet).expect("the committed ratchet must exist");
    let value: f64 = text
        .trim()
        .parse()
        .unwrap_or_else(|error| panic!("`{}` must hold one decimal: {error}", ratchet.display()));
    assert!(
        (0.0..=100.0).contains(&value),
        "a percentage must lie in 0..=100, got {value}"
    );
}
