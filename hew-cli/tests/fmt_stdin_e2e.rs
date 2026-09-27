mod support;

use std::io::Write;
use std::process::{Command, Output, Stdio};

use support::{hew_binary, strip_ansi};

fn run_fmt(args: &[&str], input: &str) -> Output {
    let mut child = Command::new(hew_binary())
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    {
        let mut stdin = child.stdin.take().expect("stdin should be piped");
        stdin.write_all(input.as_bytes()).unwrap();
    }

    child.wait_with_output().unwrap()
}

#[test]
fn fmt_no_args_exits_one_with_usage_message() {
    let output = Command::new(hew_binary()).args(["fmt"]).output().unwrap();

    assert!(!output.status.success(), "expected non-zero exit");
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Usage: hew fmt"), "stderr: {stderr}");
    assert!(stderr.contains("--stdin | <path>"), "stderr: {stderr}");
}

#[test]
fn fmt_stdin_writes_formatted_source_to_stdout() {
    let input = "fn main() { let x = 1; }\n";
    let output = run_fmt(&["fmt", "--stdin"], input);

    assert!(
        output.status.success(),
        "hew fmt --stdin failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_ne!(stdout, input);
    assert!(stdout.contains("fn main() {\n"), "stdout: {stdout}");
    assert!(stdout.contains("    let x = 1;\n"), "stdout: {stdout}");
    assert!(stdout.contains("}\n"), "stdout: {stdout}");
    assert!(
        String::from_utf8_lossy(&output.stderr).is_empty(),
        "unexpected stderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn fmt_stdin_handles_regex_records_and_is_operator() {
    let input = concat!(
        r#"type Point {
    x: i32;
    y: i32;
}

fn main() -> i32 {
    let pattern = re"^hew[0-9]+$";
    let base = Point { x: 1, y: 2 };
    let updated = Point { ..base, x: 3 };
    if updated.x is i32 {
        pattern;
    }
    updated.x
}
"#,
        "\n"
    );
    let output = run_fmt(&["fmt", "--stdin"], input);

    assert!(
        output.status.success(),
        "hew fmt --stdin failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("type Point {\n"), "stdout: {stdout}");
    assert!(
        stdout.contains(r#"let pattern = re"^hew[0-9]+$";"#),
        "stdout: {stdout}"
    );
    assert!(
        stdout.contains("let updated = Point { ..base, x: 3 };"),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("if updated.x is i32 {"), "stdout: {stdout}");
}

#[test]
fn fmt_check_stdin_succeeds_for_formatted_source() {
    let input = "fn main() {\n    let x = 1;\n}\n";
    let output = run_fmt(&["fmt", "--check", "--stdin"], input);

    assert!(
        output.status.success(),
        "hew fmt --check --stdin failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );
    assert!(
        output.stderr.is_empty(),
        "expected no stderr, got: {}",
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn fmt_check_stdin_fails_for_unformatted_source() {
    let output = run_fmt(&["fmt", "--check", "--stdin"], "fn main() { let x = 1; }\n");

    assert!(!output.status.success(), "expected non-zero exit");
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("<stdin>: needs formatting"),
        "stderr: {stderr}"
    );
}

#[test]
fn fmt_stdin_rejects_file_arguments() {
    let output = Command::new(hew_binary())
        .args(["fmt", "--stdin", "main.hew"])
        .output()
        .unwrap();

    assert!(!output.status.success(), "expected non-zero exit");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("--stdin"), "stderr: {stderr}");
    assert!(stderr.contains("cannot be used with"), "stderr: {stderr}");
}

#[test]
fn fmt_stdin_parse_errors_render_cli_diagnostics() {
    let output = run_fmt(&["fmt", "--stdin"], "fn main( {\n");

    assert!(!output.status.success(), "expected non-zero exit");
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<stdin>:1:"), "stderr: {stderr}");
    assert!(stderr.contains("error:"), "stderr: {stderr}");
    assert!(stderr.contains("1 | fn main( {"), "stderr: {stderr}");
    assert!(stderr.contains('^'), "stderr: {stderr}");
    assert!(!stderr.contains("ParseError {"), "stderr: {stderr}");
}

#[test]
fn fmt_inplace_reports_formatted_to_stderr() {
    let dir = support::tempdir();
    let path = dir.path().join("needs_fmt.hew");
    std::fs::write(&path, "fn main() { let x = 1; }\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("fmt")
        .arg(&path)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew fmt in-place failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    let expected = format!("Formatted {}", path.display());
    assert!(stderr.contains(&expected), "stderr: {stderr}");

    let rewritten = std::fs::read_to_string(&path).unwrap();
    assert!(
        rewritten.contains("fn main() {\n"),
        "file not rewritten: {rewritten}"
    );
}

#[test]
fn fmt_inplace_already_formatted_is_silent() {
    let dir = support::tempdir();
    let path = dir.path().join("already_fmt.hew");
    std::fs::write(&path, "fn main() {\n    let x = 1;\n}\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("fmt")
        .arg(&path)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew fmt in-place failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );
    assert!(
        output.stderr.is_empty(),
        "expected no stderr for already-formatted file, got: {}",
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn fmt_file_parse_errors_render_cli_diagnostics() {
    let dir = support::tempdir();
    let path = dir.path().join("broken.hew");
    std::fs::write(&path, "fn main( {\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("fmt")
        .arg(&path)
        .output()
        .unwrap();

    assert!(!output.status.success(), "expected non-zero exit");

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    let header = format!("{}:1:", path.display());
    assert!(stderr.contains(&header), "stderr: {stderr}");
    assert!(stderr.contains("error:"), "stderr: {stderr}");
    assert!(stderr.contains("1 | fn main( {"), "stderr: {stderr}");
    assert!(stderr.contains('^'), "stderr: {stderr}");
    assert!(!stderr.contains("ParseError {"), "stderr: {stderr}");
}

#[test]
fn fmt_migrate_uses_checker_resolved_variant_owners_and_check_is_non_destructive() {
    let dir = support::tempdir();
    let path = dir.path().join("legacy.hew");
    let source = concat!(
        "enum Choice {\n    Present(i64);\n}\n",
        "fn contextual() -> Choice { Present(42) }\n",
        "fn inferred() { let value = Present(7); }\n"
    );
    std::fs::write(&path, source).unwrap();

    let check = Command::new(hew_binary())
        .args(["fmt", "--migrate", "--check"])
        .arg(&path)
        .output()
        .unwrap();
    assert!(
        !check.status.success(),
        "legacy syntax must fail migration check"
    );
    assert_eq!(std::fs::read_to_string(&path).unwrap(), source);

    let migrate = Command::new(hew_binary())
        .args(["fmt", "--migrate"])
        .arg(&path)
        .output()
        .unwrap();
    assert!(
        migrate.status.success(),
        "migration failed: {}",
        String::from_utf8_lossy(&migrate.stderr)
    );
    let migrated = std::fs::read_to_string(&path).unwrap();
    assert!(migrated.contains(".Present(42)"), "migrated: {migrated}");
    assert!(
        migrated.contains("Choice.Present(7)"),
        "migrated: {migrated}"
    );

    let final_check = Command::new(hew_binary())
        .args(["fmt", "--migrate", "--check"])
        .arg(&path)
        .output()
        .unwrap();
    assert!(
        final_check.status.success(),
        "migrated source must be a fixed point: {}",
        String::from_utf8_lossy(&final_check.stderr)
    );
}

#[test]
fn fmt_migrate_rewrites_declared_machine_event_aliases() {
    let dir = support::tempdir();
    let path = dir.path().join("machine-events.hew");
    let source = "import std.machines.toggle.{Toggle, ToggleEvent};\n\nmachine Tank {\n    events {\n        Tick;\n    }\n    state Idle;\n    state Filled;\n    on Tick: Idle => Filled;\n    default { state }\n}\n\nfn main() {\n    var tank = Tank.Idle;\n    let _ = tank.step(TankEvent.Tick);\n    var toggle: Toggle = .Off;\n    let _ = toggle.step(ToggleEvent.Flip);\n    println(\"TankEvent\");\n}\n";
    std::fs::write(&path, source).unwrap();

    let migration = Command::new(hew_binary())
        .args(["fmt", "--migrate"])
        .arg(&path)
        .output()
        .unwrap();
    assert!(
        migration.status.success(),
        "migration failed: {}",
        String::from_utf8_lossy(&migration.stderr)
    );
    let migrated = std::fs::read_to_string(&path).unwrap();
    assert!(migrated.contains("Tank.Event.Tick"), "{migrated}");
    assert!(migrated.contains("Toggle.Event.Flip"), "{migrated}");
    assert!(
        migrated.contains("import std.machines.toggle.{Toggle}"),
        "{migrated}"
    );
    assert!(migrated.contains("\"TankEvent\""), "{migrated}");
    let check = Command::new(hew_binary())
        .arg("check")
        .arg(&path)
        .output()
        .unwrap();
    assert!(
        check.status.success(),
        "migrated source did not type-check: {}",
        String::from_utf8_lossy(&check.stderr)
    );
    let fixed_point = Command::new(hew_binary())
        .args(["fmt", "--migrate", "--check"])
        .arg(&path)
        .env("TMPDIR", dir.path())
        .output()
        .unwrap();
    assert!(
        fixed_point.status.success(),
        "migration snapshot inside the source parent failed: {}",
        String::from_utf8_lossy(&fixed_point.stderr)
    );
}

#[test]
fn fmt_migrate_selects_actor_lifecycle_without_touching_resource_close() {
    let dir = support::tempdir();
    let path = dir.path().join("actor-lifecycle.hew");
    let source = r"
        #[resource] type Gate { value: i64, }
        impl Gate { fn close(consume self) {} }
        actor Worker { receive fn ping() {} }
        supervisor Team { child after: Worker, }
        fn main() {
            let team = spawn Team;
            let _ = await_restart team.after;
            close(team);
            let gate = Gate { value: 1 };
            gate.close();
        }
    ";
    std::fs::write(&path, source).unwrap();

    let migrate = Command::new(hew_binary())
        .args(["fmt", "--migrate"])
        .arg(&path)
        .output()
        .unwrap();
    assert!(
        migrate.status.success(),
        "migration failed: {}",
        String::from_utf8_lossy(&migrate.stderr)
    );
    let migrated = std::fs::read_to_string(&path).unwrap();
    assert!(migrated.contains("restarted("), "{migrated}");
    assert!(migrated.contains("stop("), "{migrated}");
    assert!(migrated.contains("stopped("), "{migrated}");
    assert!(migrated.contains("gate.close();"), "{migrated}");
    assert!(!migrated.contains("await_restart"), "{migrated}");
    let second = Command::new(hew_binary())
        .args(["fmt", "--migrate", "--check"])
        .arg(&path)
        .output()
        .unwrap();
    assert!(
        second.status.success(),
        "migration changed on a second pass: {}",
        String::from_utf8_lossy(&second.stderr)
    );
}

#[test]
fn fmt_migrate_root_discovers_nested_hew_sources() {
    let dir = support::tempdir();
    let nested = dir.path().join("nested");
    std::fs::create_dir(&nested).unwrap();
    let path = nested.join("legacy.hew");
    std::fs::write(
        &path,
        "enum Choice {\n    Present(i64);\n}\n\nfn pick() -> Choice {\n    Present(42)\n}\n\nfn main() {\n    let _ = pick();\n}\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .args(["fmt", "--migrate", "--root"])
        .arg(dir.path())
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "root migration failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(std::fs::read_to_string(path)
        .unwrap()
        .contains(".Present(42)"));
}

#[test]
fn fmt_migrate_root_typechecks_legacy_paths_across_imports() {
    let dir = support::tempdir();
    let helper = dir.path().join("helper.hew");
    let main = dir.path().join("main.hew");
    std::fs::write(&helper, "pub fn empty() -> Vec<i64> { Vec::new() }\n").unwrap();
    std::fs::write(
        &main,
        "import helper::{empty};\nfn main() { let values = empty(); }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .args(["fmt", "--migrate", "--root"])
        .arg(dir.path())
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "root migration failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(std::fs::read_to_string(helper)
        .unwrap()
        .contains("Vec.new()"));
    assert!(std::fs::read_to_string(main)
        .unwrap()
        .contains("import helper.{empty};"));
}

#[test]
fn fmt_migrate_root_typechecks_legacy_directory_module_peers() {
    let dir = support::tempdir();
    let module_dir = dir.path().join("greeting");
    std::fs::create_dir(&module_dir).unwrap();
    let entry = module_dir.join("greeting.hew");
    let peer = module_dir.join("dog.hew");
    std::fs::write(
        &entry,
        concat!(
            "pub trait Greeter {\n",
            "    fn name(self) -> string;\n",
            "    fn greet(self) -> string { self.name() }\n",
            "}\n",
            "pub fn empty_labels() -> Vec<string> { Vec::new() }\n",
        ),
    )
    .unwrap();
    std::fs::write(
        &peer,
        concat!(
            "pub type Dog {\n    label: string;\n}\n",
            "impl Greeter for Dog {\n",
            "    fn name(self) -> string { self.label }\n",
            "}\n",
            "pub fn describe(d: Dog) -> string { d.greet() }\n",
            "pub fn empty_dogs() -> Vec<Dog> { Vec::new() }\n",
        ),
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .args(["fmt", "--migrate", "--root"])
        .arg(dir.path())
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "directory-module migration failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(std::fs::read_to_string(entry)
        .unwrap()
        .contains("Vec.new()"));
    assert!(std::fs::read_to_string(peer).unwrap().contains("Vec.new()"));
}

#[test]
fn fmt_migrate_root_refuses_nonrewritable_directory_peer_transactionally() {
    let dir = support::tempdir();
    let module_dir = dir.path().join("greeting");
    std::fs::create_dir(&module_dir).unwrap();
    let entry = module_dir.join("greeting.hew");
    let peer = module_dir.join("bad.hew");
    let entry_source = "pub fn empty_labels() -> Vec<string> { Vec::new() }\n";
    let peer_source = "import std::*;\npub fn broken() {}\n";
    std::fs::write(&entry, entry_source).unwrap();
    std::fs::write(&peer, peer_source).unwrap();

    let output = Command::new(hew_binary())
        .args(["fmt", "--migrate", "--root"])
        .arg(dir.path())
        .output()
        .unwrap();
    assert!(!output.status.success(), "removed glob must remain fatal");
    assert_eq!(std::fs::read_to_string(entry).unwrap(), entry_source);
    assert_eq!(std::fs::read_to_string(peer).unwrap(), peer_source);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("E_IMPORT_GLOB_REMOVED"), "stderr: {stderr}");
}

#[test]
fn fmt_migrate_combines_punctuation_and_actor_lifecycle_atomically() {
    let dir = support::tempdir();
    let actor = dir.path().join("actor.hew");
    let peer = dir.path().join("peer.hew");
    let legacy = "actor Worker { var count: i64 = 0, receive fn ping() {} }\nsupervisor Team { child worker: Worker, }\nfn main() { let team = spawn Team; let _ = await_restart team.worker; }\n";
    let invalid = "fn broken( {\n";
    std::fs::write(&actor, legacy).unwrap();
    std::fs::write(&peer, invalid).unwrap();

    let migrate = || {
        Command::new(hew_binary())
            .args(["fmt", "--migrate", "--root"])
            .arg(dir.path())
            .output()
            .unwrap()
    };
    let refused = migrate();
    assert!(!refused.status.success());
    assert_eq!(std::fs::read_to_string(&actor).unwrap(), legacy);
    assert_eq!(std::fs::read_to_string(&peer).unwrap(), invalid);

    std::fs::write(&peer, "fn helper() {}\n").unwrap();
    let migrated = migrate();
    assert!(
        migrated.status.success(),
        "{}",
        String::from_utf8_lossy(&migrated.stderr)
    );
    let once = std::fs::read_to_string(&actor).unwrap();
    assert!(once.contains("count: i64 = 0;"), "{once}");
    assert!(once.contains("child worker: Worker;"), "{once}");
    assert!(once.contains("restarted("), "{once}");
    assert!(!once.contains("await_restart"), "{once}");
    let checked = Command::new(hew_binary())
        .args(["fmt", "--migrate", "--check", "--root"])
        .arg(dir.path())
        .output()
        .unwrap();
    assert!(
        checked.status.success(),
        "{}",
        String::from_utf8_lossy(&checked.stderr)
    );
    assert_eq!(std::fs::read_to_string(actor).unwrap(), once);
}

#[test]
fn fmt_migrate_lists_typecheck_failure_sites_instead_of_succeeding() {
    let dir = support::tempdir();
    let path = dir.path().join("invalid.hew");
    let source = "enum Choice {\n    Present;\n}\n\nfn main() {\n    let value = Choice;\n}\n";
    std::fs::write(&path, source).unwrap();

    let output = Command::new(hew_binary())
        .args(["fmt", "--migrate"])
        .arg(&path)
        .output()
        .unwrap();

    assert!(!output.status.success(), "migration must fail closed");
    assert_eq!(std::fs::read_to_string(&path).unwrap(), source);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("migration refused")
            && stderr.contains("type checking failed: type `Choice` cannot be used as a value"),
        "stderr: {stderr}"
    );
}

#[test]
fn fmt_migrate_refuses_checker_variants_missing_migration_warnings() {
    let dir = support::tempdir();
    let path = dir.path().join("missing-warning.hew");
    let source = concat!(
        "enum Shape {\n    Box { w: i64; h: i64;  }\n}\n",
        "fn make() -> Shape { Box { w: 3, h: 4 } }\n"
    );
    std::fs::write(&path, source).unwrap();

    let output = Command::new(hew_binary())
        .args(["fmt", "--migrate"])
        .arg(&path)
        .output()
        .unwrap();

    assert!(!output.status.success(), "migration must fail closed");
    assert_eq!(std::fs::read_to_string(&path).unwrap(), source);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("checker resolved bare variant `Box` without a migration warning"),
        "stderr: {stderr}"
    );
}

// ---------------------------------------------------------------------------
// Multi-file --check batch behavior
// ---------------------------------------------------------------------------

/// All files already formatted → exit 0, no output on stdout or stderr.
#[test]
fn fmt_check_multi_file_all_formatted_exits_zero() {
    let dir = support::tempdir();
    let a = dir.path().join("a.hew");
    let b = dir.path().join("b.hew");
    std::fs::write(&a, "fn foo() {\n    1\n}\n").unwrap();
    std::fs::write(&b, "fn bar() {\n    2\n}\n").unwrap();

    let output = Command::new(hew_binary())
        .args(["fmt", "--check"])
        .arg(&a)
        .arg(&b)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "expected exit 0 when all files are formatted\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );
    assert!(
        output.stderr.is_empty(),
        "expected no stderr, got: {}",
        String::from_utf8_lossy(&output.stderr),
    );
}

/// Some files need formatting → per-file message on stderr, aggregate exit 1,
/// no final summary count line.
#[test]
fn fmt_check_multi_file_some_unformatted_reports_per_file_and_exits_one() {
    let dir = support::tempdir();
    let good = dir.path().join("good.hew");
    let bad = dir.path().join("bad.hew");
    // already formatted
    std::fs::write(&good, "fn foo() {\n    1\n}\n").unwrap();
    // needs formatting (single-line)
    std::fs::write(&bad, "fn bar() { 2 }\n").unwrap();

    let output = Command::new(hew_binary())
        .args(["fmt", "--check"])
        .arg(&good)
        .arg(&bad)
        .output()
        .unwrap();

    assert!(!output.status.success(), "expected non-zero exit");
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    // only the unformatted file is reported
    let bad_name = bad.display().to_string();
    assert!(
        stderr.contains(&format!("{bad_name}: needs formatting")),
        "expected '{bad_name}: needs formatting' in stderr: {stderr}",
    );
    // the already-formatted file must not appear
    let good_name = good.display().to_string();
    assert!(
        !stderr.contains(&good_name),
        "good file should not appear in stderr: {stderr}",
    );
    // no summary count line (e.g. "2 files checked, 1 needs formatting")
    assert!(
        !stderr.contains("files checked"),
        "unexpected summary line in stderr: {stderr}",
    );
}

/// All files need formatting → each gets its own line, exit 1, no summary.
#[test]
fn fmt_check_multi_file_all_unformatted_reports_each_file() {
    let dir = support::tempdir();
    let a = dir.path().join("a.hew");
    let b = dir.path().join("b.hew");
    std::fs::write(&a, "fn foo() { 1 }\n").unwrap();
    std::fs::write(&b, "fn bar() { 2 }\n").unwrap();

    let output = Command::new(hew_binary())
        .args(["fmt", "--check"])
        .arg(&a)
        .arg(&b)
        .output()
        .unwrap();

    assert!(!output.status.success(), "expected non-zero exit");
    assert!(
        output.stdout.is_empty(),
        "expected no stdout, got: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    let a_name = a.display().to_string();
    let b_name = b.display().to_string();
    assert!(
        stderr.contains(&format!("{a_name}: needs formatting")),
        "expected '{a_name}: needs formatting' in stderr: {stderr}",
    );
    assert!(
        stderr.contains(&format!("{b_name}: needs formatting")),
        "expected '{b_name}: needs formatting' in stderr: {stderr}",
    );
    assert!(
        !stderr.contains("files checked"),
        "unexpected summary line in stderr: {stderr}",
    );
}
