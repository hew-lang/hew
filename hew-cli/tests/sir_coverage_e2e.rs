//! `hew tool sir-coverage` contracts: the dev-only inventory of which
//! function bodies the SIR route admits and which the legacy lowerer still
//! owns.
//!
//! The inventory asks every admitted header for its body (not only what the
//! entry reaches), names the refusal for everything else, counts every
//! function body (free fns, impl methods, actor/machine handlers) toward the
//! ratchet, and leaves bodiless declarations as uncounted inventory lines.

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

/// Two receive handlers and a plain method, so the body enumeration must
/// report three actor-body rows, not one row for the whole actor.
const ACTOR_PROGRAM: &str = r"
actor Counter {
    var count: i64 = 0;

    receive fn bump() {
        count = count + 1;
    }

    receive fn reset() {
        count = 0;
    }

    fn peek() -> i64 {
        count
    }
}

fn main() -> i64 {
    let counter = spawn Counter;
    counter.bump();
    0
}
";

/// A bodiless type declaration and nothing else: the inventory has no
/// function body to count, so the run must fail closed rather than report a
/// vacuous 0/0.
const TYPE_ONLY: &str = r"
type Pair {
    left: i64,
    right: i64,
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
fn every_actor_handler_body_is_its_own_counted_row() {
    let dir = support::tempdir();
    fs::write(dir.path().join("counter.hew"), ACTOR_PROGRAM).expect("write fixture");

    let output = coverage(&["counter.hew"], dir.path());
    assert!(
        output.status.success(),
        "an inventory over an actor program must succeed:\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    for handler in [
        "Counter::receive fn bump",
        "Counter::receive fn reset",
        "Counter::peek",
    ] {
        assert!(
            stdout
                .lines()
                .any(|line| line == format!("counter.hew {handler} legacy: no-sir-route:actor-body")),
            "each actor body must be its own row, not collapsed into the actor's item-kind line:\n{stdout}"
        );
    }
    assert!(
        stdout
            .lines()
            .any(|line| line == "counter.hew Counter legacy: item-kind:actor"),
        "the actor declaration itself stays an uncounted inventory line:\n{stdout}"
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
            .is_some_and(|line| line == "sir-coverage: 0/4 functions (0.00%)"),
        "main plus three actor bodies must be counted, the actor's own item-kind line must not:\n{stdout}"
    );
}

#[test]
fn a_bodiless_corpus_fails_closed_instead_of_reporting_zero_of_zero() {
    let dir = support::tempdir();
    fs::write(dir.path().join("pair.hew"), TYPE_ONLY).expect("write fixture");

    let output = coverage(&["pair.hew"], dir.path());
    assert_eq!(
        output.status.code(),
        Some(2),
        "a corpus with no function body proves nothing and must fail closed:\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("no function bodies were inventoried"),
        "{stderr}"
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
    assert_eq!(item("main")["counted"], true);
    assert_eq!(item("greet")["status"], "legacy");
    assert!(item("greet")["reason"]
        .as_str()
        .is_some_and(|reason| reason.contains("string")));
    assert_eq!(report["admitted"], 2);
    assert_eq!(report["total"], 4);
    assert_eq!(report["files_failed"], 0);
}

#[test]
fn an_impl_block_header_is_an_uncounted_inventory_line() {
    let dir = support::tempdir();
    fs::write(
        dir.path().join("wrap.hew"),
        r"
type Wrapper { value: i64 }

impl Wrapper {
    fn value(self) -> i64 {
        self.value
    }
}

fn main() -> i64 {
    Wrapper { value: 7 }.value()
}
",
    )
    .expect("write fixture");

    let output = coverage(&["--json", "wrap.hew"], dir.path());
    assert!(output.status.success(), "{}", describe_output(&output));
    let report: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("--json must emit one JSON document");
    let items = report["files"][0]["items"]
        .as_array()
        .expect("one file with an item array");
    let impl_header = items
        .iter()
        .find(|item| item["name"] == "impl Wrapper")
        .unwrap_or_else(|| panic!("the impl-block header row is missing from {report:#}"));
    assert_eq!(impl_header["counted"], false);
    let method = items
        .iter()
        .find(|item| {
            item["name"]
                .as_str()
                .is_some_and(|name| name.ends_with("::value") && name.contains("Wrapper"))
        })
        .unwrap_or_else(|| panic!("the impl method row is missing from {report:#}"));
    assert_eq!(method["counted"], true);
    // The impl-block header must not add a second entry to `total` for a
    // method already counted through its own `HirItem::Function` row.
    assert_eq!(
        report["total"], 2,
        "main plus the one method, not the header too"
    );
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
fn ratchet_fails_when_the_recorded_count_is_higher_than_measured() {
    let dir = support::tempdir();
    fs::write(dir.path().join("mix.hew"), SCALAR_MIX).expect("write fixture");
    // SCALAR_MIX admits 2 of 4 bodies; recording a higher admitted count
    // than what this run measured is exactly a regression.
    fs::write(dir.path().join("ratchet.txt"), "3\n").expect("write ratchet");

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
            && stderr.contains("records 3")
            && stderr.contains("measured 2"),
        "the failure must quote both sides:\n{stderr}"
    );
}

#[test]
fn ratchet_holds_when_equal_and_reports_a_rise_without_failing() {
    let dir = support::tempdir();
    fs::write(dir.path().join("mix.hew"), SCALAR_MIX).expect("write fixture");

    fs::write(dir.path().join("ratchet.txt"), "2\n").expect("write ratchet");
    let equal = coverage(&["--ratchet", "ratchet.txt", "mix.hew"], dir.path());
    assert!(
        equal.status.success(),
        "an equal ratchet must hold:\n{}",
        describe_output(&equal)
    );
    assert!(
        String::from_utf8_lossy(&equal.stderr).contains("ratchet holds at 2"),
        "{}",
        describe_output(&equal)
    );

    fs::write(dir.path().join("ratchet.txt"), "1\n").expect("write ratchet");
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
        stderr.contains("ratchet can rise") && stderr.contains("update the file to 2"),
        "the rise must say what to record:\n{stderr}"
    );

    // The same rise fails under strict-recoveries accounting: an unrecorded
    // rise is treated the same as an unrecorded drop.
    let mut strict = Command::new(hew_binary());
    strict
        .arg("tool")
        .arg("sir-coverage")
        .arg("--ratchet")
        .arg("ratchet.txt")
        .arg("mix.hew")
        .env("RATCHET_STRICT_RECOVERIES", "1")
        .current_dir(dir.path());
    let strict = support::run_bounded_command(strict, "sir-coverage ratchet strict rise");
    assert_eq!(
        strict.status.code(),
        Some(1),
        "an unrecorded rise must fail under strict-recoveries accounting:\n{}",
        describe_output(&strict)
    );
}

#[test]
fn a_non_numeric_ratchet_file_fails_closed() {
    let dir = support::tempdir();
    fs::write(dir.path().join("mix.hew"), SCALAR_MIX).expect("write fixture");
    fs::write(dir.path().join("ratchet.txt"), "60.0000\n").expect("write ratchet");

    let output = coverage(&["--ratchet", "ratchet.txt", "mix.hew"], dir.path());
    assert_eq!(
        output.status.code(),
        Some(2),
        "a percentage left over from the old format is not a valid count:\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("must hold one non-negative integer"),
        "{stderr}"
    );
}

#[test]
fn the_committed_ratchet_is_a_non_negative_integer() {
    let ratchet = repo_root().join("scripts/sir-coverage-ratchet.txt");
    let text = fs::read_to_string(&ratchet).expect("the committed ratchet must exist");
    text.trim().parse::<usize>().unwrap_or_else(|error| {
        panic!(
            "`{}` must hold one non-negative integer count, not a percentage: {error}",
            ratchet.display()
        )
    });
}
