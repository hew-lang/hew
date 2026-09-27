mod support;

use std::fmt::Write as _;
use std::path::Path;
use std::process::Command;
use support::{hew_binary, repo_root, require_codegen, run_hew_in};

fn write_file(root: &Path, relative_path: &str, contents: &str) {
    let path = root.join(relative_path);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).unwrap();
    }
    std::fs::write(path, contents).unwrap();
}

fn has_test_line(output: &str, name: &str, status: &str) -> bool {
    output
        .lines()
        .any(|line| line.starts_with(&format!("{status}  ")) && line.contains(&format!("::{name}")))
}

fn run_suite(files: &[(&str, &str)], extra_args: &[&str]) -> std::process::Output {
    let dir = support::tempdir();
    for (path, contents) in files {
        write_file(dir.path(), path, contents);
    }

    let mut args = vec!["test", "."];
    args.extend_from_slice(extra_args);
    run_hew_in(dir.path(), &args)
}

#[test]
fn doc_output_mismatch_names_the_fence() {
    require_codegen();
    let dir = support::tempdir();
    write_file(
        dir.path(),
        "guide.md",
        "```hew\nprintln(3);\n// Output:\n// 4\n```\n",
    );
    let output = run_hew_in(
        dir.path(),
        &["test", "--doc", "guide.md", "--format", "json"],
    );
    assert!(!output.status.success());
    let events = String::from_utf8_lossy(&output.stdout);
    assert!(events.contains("guide.md::doc(module)#1"), "{events}");
    assert!(events.contains("doc output differs"), "{events}");
}

#[test]
fn doc_no_run_still_rejects_a_type_error() {
    require_codegen();
    let dir = support::tempdir();
    write_file(
        dir.path(),
        "guide.md",
        "```hew,no_run\nlet count: i64 = \"wrong\";\n```\n",
    );
    let output = run_hew_in(
        dir.path(),
        &["test", "--doc", "guide.md", "--format", "json"],
    );
    assert!(!output.status.success());
    let events = String::from_utf8_lossy(&output.stdout);
    assert!(events.contains("guide.md::doc(module)#1"), "{events}");
    assert!(events.contains("\"ok\":false"), "{events}");
}

#[test]
fn doc_no_run_compiles_host_operations_without_running_them() {
    require_codegen();
    let dir = support::tempdir();
    write_file(
        dir.path(),
        "guide.md",
        "```hew,no_run\nimport std.fs;\nlet _ = fs.read(\"missing-file\");\n```\n",
    );
    let output = run_hew_in(
        dir.path(),
        &["test", "--doc", "guide.md", "--format", "json"],
    );
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let events = String::from_utf8_lossy(&output.stdout);
    assert!(events.contains("\"passed\":1"), "{events}");
}

#[test]
fn doc_fence_selects_generated_entry_after_existing_test() {
    require_codegen();
    let dir = support::tempdir();
    write_file(
        dir.path(),
        "module.hew",
        "#[test]\nfn earlier() { println(\"earlier test\"); }\n\n/// Example.\n/// ```hew\n/// println(\"doc fence\");\n/// // Output:\n/// // doc fence\n/// ```\npub fn example() {}\n",
    );
    let output = run_hew_in(
        dir.path(),
        &[
            "test",
            "--doc",
            "module.hew::doc(example)#1",
            "--format",
            "json",
        ],
    );
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let events = String::from_utf8_lossy(&output.stdout);
    assert!(events.contains("module.hew::doc(example)#1"), "{events}");
    assert!(events.contains("\"output\":\"doc fence\\n\""), "{events}");
    assert!(!events.contains("earlier test"), "{events}");
}

#[test]
fn doc_imports_keep_source_items_in_scope() {
    require_codegen();
    let dir = support::tempdir();
    write_file(
        dir.path(),
        "module.hew",
        "#[test]\nfn ordinary() { println(\"ordinary\"); }\n/// Example.\n/// ```hew\n/// import std.math;\n/// println(math.abs(-value()));\n/// // Output:\n/// // 7\n/// ```\npub fn value() -> i64 { 7 }\n",
    );
    write_file(
        dir.path(),
        "guide.md",
        "```hew\nimport std.math;\nprintln(math.abs(-7 as i64));\n// Output:\n// 7\n```\n",
    );
    let output = run_hew_in(dir.path(), &["test", "--doc", ".", "--format", "json"]);
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let events = String::from_utf8_lossy(&output.stdout);
    assert!(events.contains("module.hew::doc(value)#1"), "{events}");
    assert!(events.contains("guide.md::doc(module)#1"), "{events}");
    assert!(events.contains("module.hew::ordinary"), "{events}");
    assert!(events.contains("\"passed\":3"), "{events}");
}

#[test]
fn doc_declarations_compile_and_can_be_called_in_the_same_fence() {
    require_codegen();
    let dir = support::tempdir();
    write_file(
        dir.path(),
        "guide.md",
        "```hew\nfn grade(n: i64) -> string { if n >= 90 { \"A\" } else { \"B\" } }\n```\n```hew\nenum Level { High; Low; }\nfn grade(n: i64) -> string { if n >= 90 { \"A\" } else { \"B\" } }\nprintln(grade(92));\n// Output:\n// A\n```\n",
    );
    let output = run_hew_in(
        dir.path(),
        &["test", "--doc", "guide.md", "--format", "json"],
    );
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let events = String::from_utf8_lossy(&output.stdout);
    assert!(events.contains("\"passed\":2"), "{events}");
}

#[test]
fn should_panic_matches_checked_trap_and_fragment() {
    require_codegen();
    let output = run_suite(
        &[(
            "fault_test.hew",
            "#[test]\n#[should_panic(\"IndexOutOfBounds\")]\nfn bounds() {\n    let values: Vec<i64> = [1, 2];\n    println(values[7]);\n}\n#[test]\n#[should_panic(\"known fragment\")]\nfn panic_with_text() { panic(\"known fragment present\"); }\n",
        )],
        &["--format", "json"],
    );
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let events = String::from_utf8_lossy(&output.stdout);
    assert!(events.contains("\"passed\":2"), "{events}");
}

#[test]
fn absolute_selectors_distinguish_same_named_tests_across_roots() {
    require_codegen();
    let dir = support::tempdir();
    for root in ["first", "second"] {
        write_file(
            dir.path(),
            &format!("{root}/cart_test.hew"),
            "#[test]\nfn totals() { assert(true); }\n",
        );
    }
    let first = format!(
        "{}::totals",
        dir.path().join("first/cart_test.hew").display()
    );
    let second = format!(
        "{}::totals",
        dir.path().join("second/cart_test.hew").display()
    );
    let output = run_hew_in(
        &dir.path().join("first"),
        &["test", &first, &second, "--format", "json"],
    );
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let mut selectors = String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter_map(|line| serde_json::from_str::<serde_json::Value>(line).ok())
        .filter(|event| event["event"] == "test_finished")
        .filter_map(|event| event["selector"].as_str().map(str::to_owned))
        .collect::<Vec<_>>();
    assert_eq!(selectors.len(), 2);
    selectors.sort();
    let mut expected = [first, second];
    expected.sort();
    assert_eq!(selectors, expected);
}

#[test]
fn package_native_ffi_is_built_and_linked() {
    require_codegen();

    let dir = support::tempdir();
    for (path, contents) in [
        (
            "hew.toml",
            include_str!("fixtures/test_ffi_package/hew.toml"),
        ),
        (
            "Cargo.toml",
            include_str!("fixtures/test_ffi_package/Cargo.toml"),
        ),
        (
            "src/lib.rs",
            include_str!("fixtures/test_ffi_package/src/lib.rs"),
        ),
        (
            "ffi_test.hew",
            include_str!("fixtures/test_ffi_package/ffi_test.hew"),
        ),
    ] {
        write_file(dir.path(), path, contents);
    }

    let output = Command::new(hew_binary())
        .args(["test", "ffi_test.hew", "--color=never", "--jobs", "1"])
        .env("CARGO_TARGET_DIR", dir.path().join("target"))
        .current_dir(dir.path())
        .output()
        .expect("run package FFI test");

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(has_test_line(&stdout, "native_ffi_is_linked", "ok"));
    assert!(stdout.contains("1 passed, 0 failed, 0 ignored"));
    assert!(dir.path().join("target/release-lib").is_dir());
    let archive = if cfg!(target_os = "windows") {
        dir.path()
            .join("target/release-lib/hew_test_ffi_package.lib")
    } else {
        dir.path()
            .join("target/release-lib/libhew_test_ffi_package.a")
    };
    assert!(
        archive.is_file(),
        "declared native library was not built at {}",
        archive.display()
    );
}

#[test]
fn passing_suite_exits_zero() {
    require_codegen();

    let output = run_suite(
        &[(
            "passing_test.hew",
            "#[test]\nfn passes() {\n    assert(true);\n}\n",
        )],
        &["--color=never"],
    );

    assert!(output.status.success());
    assert!(
        output.stderr.is_empty(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(has_test_line(&stdout, "passes", "ok"));
    assert!(stdout.contains("1 passed, 0 failed, 0 ignored"));
}

/// Selected test declarations retain their direct helper calls.
#[test]
fn selected_unit_test_calls_its_helper() {
    require_codegen();

    let dir = support::tempdir();
    write_file(
        dir.path(),
        "sir_unit_test.hew",
        "fn helper() {}\n\n#[test]\nfn unit_test() {\n    helper();\n}\n",
    );
    let mut command = Command::new(hew_binary());
    command
        .args(["test", ".", "--color=never", "--jobs", "1"])
        // Integration builds may place the compiler in an SSD target directory
        // outside the checkout, where its dev-layout stdlib discovery cannot
        // infer this source tree from a temporary test project.
        .env("HEW_STD", repo_root().join("std"))
        .current_dir(dir.path());
    let output = support::run_bounded_command(command, "run strict SIR unit test wrapper");

    assert!(
        output.status.success(),
        "strict SIR test wrapper must compile and execute successfully:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        has_test_line(&stdout, "unit_test", "ok"),
        "stdout: {stdout}"
    );
    assert!(
        stdout.contains("1 passed, 0 failed, 0 ignored"),
        "stdout: {stdout}"
    );
}

#[test]
fn project_test_imports_source_module_from_project_root() {
    require_codegen();

    let project = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/test_project_root_fixture");
    let output = run_hew_in(&project, &["test", ".", "--color=never"]);

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(has_test_line(&stdout, "imports_source_module", "ok"));
    assert!(stdout.contains("1 passed, 0 failed, 0 ignored"));
}

#[test]
fn project_test_fixture_check_uses_enclosing_manifest_for_source_imports() {
    require_codegen();

    let project = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/test_project_root_fixture");
    let output = run_hew_in(&project, &["check", "tests/project_root_test.hew"]);

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn relative_string_import_resolves_from_test_file() {
    require_codegen();

    let output = run_suite(
        &[
            ("support.hew", "pub fn expected() -> i64 { 42 }\n"),
            (
                "relative_test.hew",
                "import \"support.hew\";\n\n#[test]\nfn imports_relative_file() {\n    assert(expected() == 42);\n}\n",
            ),
        ],
        &["--color=never"],
    );

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(has_test_line(
        &String::from_utf8_lossy(&output.stdout),
        "imports_relative_file",
        "ok"
    ));
}

#[test]
fn failing_suite_exits_non_zero() {
    require_codegen();

    let output = run_suite(
        &[(
            "failing_test.hew",
            "#[test]\nfn fails() {\n    panic(\"expected failure\");\n}\n",
        )],
        &["--color=never"],
    );

    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(has_test_line(&stdout, "fails", "FAIL"));
    assert!(stdout.contains("expected failure"));
}

#[test]
fn test_processes_get_isolated_scratch_and_keep_failed_run() {
    require_codegen();

    let project = support::tempdir();
    let pool = project.path().join("scratch-pool");
    std::fs::create_dir(&pool).expect("create test scratch pool");
    write_file(
        project.path(),
        "scratch_test.hew",
        "import std.os;\n\
         #[test]\nfn passes() { assert(os.temp_dir() != \"\"); }\n\
         #[test]\nfn fails() { panic(os.temp_dir()); }\n",
    );
    let output = Command::new(hew_binary())
        .args(["test", "scratch_test.hew", "--color=never", "--jobs", "1"])
        .env("HEW_STD", repo_root().join("std"))
        .env("TMPDIR", &pool)
        .env("TMP", &pool)
        .env("TEMP", &pool)
        .current_dir(project.path())
        .output()
        .expect("run isolated scratch test");
    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(has_test_line(&stdout, "passes", "ok"), "{stdout}");
    assert!(has_test_line(&stdout, "fails", "FAIL"), "{stdout}");
    let kept = stdout
        .split_once("test scratch: ")
        .and_then(|(_, rest)| rest.lines().next())
        .map(Path::new)
        .expect("failed test reports its scratch directory");
    assert!(kept.starts_with(&pool), "{stdout}");
    assert!(kept.is_dir(), "failed test scratch must remain inspectable");
    assert!(stdout.contains(&format!("UserPanic (212): {}", kept.display())));
    let retained: Vec<_> = std::fs::read_dir(&pool)
        .expect("read test scratch pool")
        .map(|entry| entry.expect("read scratch entry").path())
        .filter(|path| {
            path.file_name()
                .is_some_and(|name| name.to_string_lossy().starts_with("hew_test_run_"))
        })
        .collect();
    assert_eq!(retained.len(), 1, "passing test scratch must be removed");
}

#[test]
fn mixed_suite_reports_each_test_and_exits_non_zero() {
    require_codegen();

    let output = run_suite(
        &[
            (
                "alpha_test.hew",
                "#[test]\nfn alpha() {\n    assert(true);\n}\n",
            ),
            (
                "nested/beta_test.hew",
                "#[test]\nfn beta() {\n    panic(\"boom\");\n}\n",
            ),
        ],
        &["--color=never"],
    );

    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(has_test_line(&stdout, "alpha", "ok"));
    assert!(has_test_line(&stdout, "beta", "FAIL"));
    assert!(stdout.contains("1 passed, 1 failed, 0 ignored"));
}

#[test]
fn parallel_suite_reports_in_discovery_order() {
    require_codegen();

    let output = run_suite(
        &[(
            "ordered_test.hew",
            "#[test]\nfn slow_first() {\n    sleep(100ms);\n}\n\n#[test]\nfn fast_second() {\n    assert(true);\n}\n",
        )],
        &["--color=never", "--jobs", "2"],
    );

    assert!(output.status.success());
    assert!(
        output.stderr.is_empty(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let first = stdout.find("::slow_first").unwrap();
    let second = stdout.find("::fast_second").unwrap();
    assert!(first < second, "stdout: {stdout}");
}

#[test]
fn parallel_csv_test_compilation_uses_compiler_stack_budget() {
    require_codegen();

    // Compiling this standard-library consumer exhausts the platform-default
    // stack used by a scoped test worker, while the same compiler pipeline is
    // healthy on the explicit compiler stack used by `hew-main`.
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli should have a workspace parent");
    let output = run_hew_in(
        workspace_root,
        &[
            "test",
            "tests/hew/csv_test.hew",
            "--color=never",
            "--jobs",
            "2",
            "--filter",
            "test_parse_get_by_name",
        ],
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stdout.contains("::test_parse_get_by_name"),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stdout.contains("::test_parse_get_by_name_missing_column_returns_empty"),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stdout.contains("passed,") || stdout.contains("failed,"),
        "runner must render a complete result instead of aborting\nstdout: {stdout}\nstderr: {stderr}"
    );
    assert!(stderr.is_empty(), "stderr: {stderr}");
}

#[test]
fn serial_tests_do_not_overlap() {
    require_codegen();

    let port = std::net::TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port();
    let source = format!(
        "import std.net;\n\n\
         fn hold_port() {{\n\
             match net.listen(\"127.0.0.1:{port}\") {{\n\
                 .Ok(listener) => {{\n\
                     sleep(200ms);\n\
                     listener.close();\n\
                 }}\n\
                 .Err(_) => panic(\"serial tests overlapped\"),\n\
             }}\n\
         }}\n\n\
         #[test]\n#[serial]\n#[real_time]\nfn serial_one() {{ hold_port(); }}\n\n\
         #[test]\n#[serial]\n#[real_time]\nfn serial_two() {{ hold_port(); }}\n"
    );

    let output = run_suite(
        &[("serial_test.hew", source.as_str())],
        &["--color=never", "--jobs", "2"],
    );

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn zero_jobs_is_rejected() {
    let output = run_suite(
        &[("ignored_test.hew", "#[test]\n#[ignore]\nfn ignored() {}\n")],
        &["--jobs", "0"],
    );

    assert_eq!(output.status.code(), Some(2));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("invalid value '0'"), "stderr: {stderr}");
}

#[test]
fn ignored_test_is_skipped_and_counted() {
    require_codegen();

    let output = run_suite(
        &[(
            "ignored_test.hew",
            "#[test]\n#[ignore]\nfn skipped() {\n    panic(\"ignored tests should not run\");\n}\n",
        )],
        &["--color=never"],
    );

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(has_test_line(&stdout, "skipped", "skip"));
    assert!(stdout.contains("0 passed, 0 failed, 1 ignored"));
}

#[test]
fn include_ignored_flag_runs_skipped_tests() {
    require_codegen();

    let output = run_suite(
        &[(
            "ignored_test.hew",
            "#[test]\n#[ignore]\nfn skipped() {\n    panic(\"ignored test ran\");\n}\n",
        )],
        &["--color=never", "--include-ignored"],
    );

    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(has_test_line(&stdout, "skipped", "FAIL"));
    assert!(stdout.contains("ignored test ran"));
    assert!(stdout.contains("0 passed, 1 failed, 0 ignored"));
}

#[test]
fn filter_narrows_to_matching_tests() {
    require_codegen();

    let output = run_suite(
        &[
            (
                "alpha_test.hew",
                "#[test]\nfn keeps_me() {\n    assert(true);\n}\n",
            ),
            (
                "beta_test.hew",
                "#[test]\nfn skip_me() {\n    panic(\"filtered test should not run\");\n}\n",
            ),
        ],
        &["--color=never", "--filter", "keeps"],
    );

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("hew test (1 tests)"));
    assert!(has_test_line(&stdout, "keeps_me", "ok"));
    assert!(!stdout.contains("skip_me"));
    assert!(stdout.contains("1 passed, 0 failed, 0 ignored"));
}

#[test]
fn list_reports_stable_relative_identities_without_compiling() {
    let output = run_suite(
        &[
            (
                "alpha_test.hew",
                "#[test]\nfn alpha() {\n    assert(true);\n}\n",
            ),
            (
                "nested/beta_test.hew",
                "#[test]\nfn beta() {\n    assert(true);\n}\n",
            ),
        ],
        &["--list"],
    );

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout.lines().collect::<Vec<_>>(),
        ["alpha_test.hew::alpha", "nested/beta_test.hew::beta"]
    );
}

#[test]
fn hash_partition_inventories_are_disjoint_and_cover_the_full_list() {
    let dir = support::tempdir();
    let mut source = String::new();
    for index in 0..20 {
        writeln!(
            source,
            "#[test]\nfn case_{index}() {{\n    assert(true);\n}}"
        )
        .unwrap();
    }
    write_file(dir.path(), "partition_test.hew", &source);

    let full = run_hew_in(dir.path(), &["test", ".", "--list"]);
    assert!(full.status.success());
    let full: std::collections::BTreeSet<_> = String::from_utf8(full.stdout)
        .unwrap()
        .lines()
        .map(str::to_owned)
        .collect();

    let mut union = std::collections::BTreeSet::new();
    for shard in 1..=4 {
        let partition = format!("hash:{shard}/4");
        let output = run_hew_in(
            dir.path(),
            &["test", ".", "--list", "--partition", &partition],
        );
        assert!(output.status.success());
        for identity in String::from_utf8(output.stdout).unwrap().lines() {
            assert!(
                union.insert(identity.to_owned()),
                "duplicate partition identity: {identity}"
            );
        }
    }
    assert_eq!(union, full);
}

#[test]
fn malformed_hash_partition_is_rejected() {
    let output = run_suite(
        &[(
            "partition_test.hew",
            "#[test]\nfn alpha() {\n    assert(true);\n}\n",
        )],
        &["--list", "--partition", "hash:0/4"],
    );

    assert_eq!(output.status.code(), Some(2));
    assert!(String::from_utf8_lossy(&output.stderr).contains("1 <= SHARD <= TOTAL"));
}

#[test]
fn should_panic_test_passes_when_it_panics() {
    require_codegen();

    let output = run_suite(
        &[(
            "should_panic_test.hew",
            "#[test]\n#[should_panic]\nfn expected_panic() {\n    panic(\"boom\");\n}\n",
        )],
        &["--color=never"],
    );

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(has_test_line(&stdout, "expected_panic", "ok"));
    assert!(stdout.contains("1 passed, 0 failed, 0 ignored"));
}

#[test]
fn should_panic_test_fails_when_it_does_not_panic() {
    require_codegen();

    let output = run_suite(
        &[(
            "should_panic_test.hew",
            "#[test]\n#[should_panic]\nfn expected_panic() {\n    assert(true);\n}\n",
        )],
        &["--color=never"],
    );

    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(has_test_line(&stdout, "expected_panic", "FAIL"));
    assert!(stdout.contains("expected a Hew fault, but test completed successfully"));
    assert!(stdout.contains("0 passed, 1 failed, 0 ignored"));
}

#[test]
fn should_panic_rejects_an_explicit_nonzero_exit() {
    require_codegen();

    let output = run_suite(
        &[(
            "should_panic_exit_test.hew",
            "#[test]\n#[should_panic]\nfn exits() {\n    exit(7);\n}\n",
        )],
        &["--color=never"],
    );

    assert_eq!(output.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(has_test_line(&stdout, "exits", "FAIL"), "{stdout}");
    assert!(
        stdout.contains("expected a Hew fault, got another failure"),
        "{stdout}"
    );
}

#[test]
fn failed_assertion_reports_its_source_site() {
    require_codegen();

    let output = run_suite(
        &[(
            "assert_site_test.hew",
            "#[test]\nfn fails() {\n    assert(1 == 2);\n}\n",
        )],
        &["--color=never"],
    );

    assert_eq!(output.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("assert_site_test.hew:3:"), "{stdout}");
}

#[test]
fn should_panic_rejects_a_mismatched_checked_trap_with_its_site() {
    require_codegen();

    let output = run_suite(
        &[(
            "trap_site_test.hew",
            "#[test]\n#[should_panic(\"DivideByZero\")]\nfn traps() {\n    let values = [1, 2];\n    var index = 3;\n    let _ = values[index];\n}\n",
        )],
        &["--color=never"],
    );

    assert_eq!(output.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("expected fault containing \"DivideByZero\""),
        "{stdout}"
    );
    assert!(stdout.contains("IndexOutOfBounds"), "{stdout}");
    assert!(stdout.contains("trap_site_test.hew:6:"), "{stdout}");
}

#[test]
fn no_test_files_in_directory_exits_non_zero() {
    let dir = support::tempdir();
    write_file(dir.path(), "notes/readme.txt", "not a Hew test\n");

    let output = Command::new(hew_binary())
        .arg("test")
        .arg(".")
        .arg("--color=never")
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert_eq!(output.status.code(), Some(1));
    assert!(
        output.stdout.is_empty(),
        "stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("No test files found."), "stderr: {stderr}");
}

#[test]
fn no_test_files_allow_empty_exits_zero() {
    let dir = support::tempdir();
    write_file(dir.path(), "notes/readme.txt", "not a Hew test\n");

    let output = Command::new(hew_binary())
        .arg("test")
        .arg(".")
        .arg("--color=never")
        .arg("--allow-empty")
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert_eq!(output.status.code(), Some(0));
    assert!(
        output.stdout.is_empty(),
        "stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("No test files found."), "stderr: {stderr}");
}

#[test]
fn test_zero_functions_exits_nonzero() {
    let output = run_suite(
        &[("helpers_test.hew", "fn helper() -> i64 {\n    42\n}\n")],
        &["--color=never"],
    );

    assert_eq!(output.status.code(), Some(1));
    assert!(
        output.stdout.is_empty(),
        "stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("No test functions found."),
        "stderr: {stderr}"
    );
}

#[test]
fn test_zero_functions_allow_empty_exits_zero() {
    let output = run_suite(
        &[("helpers_test.hew", "fn helper() -> i64 {\n    42\n}\n")],
        &["--color=never", "--allow-empty"],
    );

    assert_eq!(output.status.code(), Some(0));
    assert!(
        output.stdout.is_empty(),
        "stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("No test functions found."),
        "stderr: {stderr}"
    );
}

#[test]
fn multi_path_invocation_aggregates_results() {
    require_codegen();

    let dir = support::tempdir();
    write_file(
        dir.path(),
        "suite_a/alpha_test.hew",
        "#[test]\nfn alpha() {\n    assert(true);\n}\n",
    );
    write_file(
        dir.path(),
        "suite_b/beta_test.hew",
        "#[test]\nfn beta() {\n    assert(true);\n}\n",
    );

    let output = Command::new(hew_binary())
        .arg("test")
        .arg("suite_a")
        .arg("suite_b")
        .arg("--color=never")
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(output.status.success());
    assert_eq!(output.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("hew test (2 tests)"), "stdout: {stdout}");
    assert!(has_test_line(&stdout, "alpha", "ok"), "stdout: {stdout}");
    assert!(has_test_line(&stdout, "beta", "ok"), "stdout: {stdout}");
    assert!(
        stdout.contains("2 passed, 0 failed, 0 ignored"),
        "stdout: {stdout}"
    );
}

#[test]
fn test_runner_relative_path_invocation_discovers_same_tests_as_absolute_path() {
    let dir = support::tempdir();
    write_file(
        dir.path(),
        "suite/relative_discovery_test.hew",
        "#[test]\n#[ignore]\nfn before_import() {\n    assert(true);\n}\n\nimport std.math;\n\n#[test]\n#[ignore]\nfn after_import() {\n    assert(true);\n}\n",
    );
    let absolute_path = dir.path().join("suite").join("relative_discovery_test.hew");

    let relative = Command::new(hew_binary())
        .arg("test")
        .arg("suite/relative_discovery_test.hew")
        .arg("--color=never")
        .current_dir(dir.path())
        .output()
        .unwrap();
    let absolute = Command::new(hew_binary())
        .arg("test")
        .arg(&absolute_path)
        .arg("--color=never")
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(relative.status.success());
    assert!(absolute.status.success());
    let relative_stdout = String::from_utf8_lossy(&relative.stdout);
    let absolute_stdout = String::from_utf8_lossy(&absolute.stdout);
    assert!(
        relative_stdout.contains("hew test (2 tests)"),
        "stdout: {relative_stdout}"
    );
    assert!(
        absolute_stdout.contains("hew test (2 tests)"),
        "stdout: {absolute_stdout}"
    );
    assert!(has_test_line(&relative_stdout, "before_import", "skip"));
    assert!(has_test_line(&relative_stdout, "after_import", "skip"));
    assert_eq!(relative_stdout, absolute_stdout);
}

#[test]
fn parse_errors_fail_the_suite() {
    let output = run_suite(
        &[(
            "broken_test.hew",
            "#[test]\nfn broken( {\n    assert(true);\n}\n",
        )],
        &["--color=never"],
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("expected"));
}

#[test]
fn timeout_exit_code_is_non_zero() {
    require_codegen();

    let output = run_suite(
        &[(
            "timeout_test.hew",
            "#[test]\nfn forever() {\n    loop {\n        println(\"spin\");\n    }\n}\n",
        )],
        &["--color=never", "--timeout", "1"],
    );

    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(has_test_line(&stdout, "forever", "FAIL"));
    assert!(stdout.contains("test timed out after 1s"));
}

#[test]
fn missing_path_exits_non_zero() {
    let dir = support::tempdir();
    let output = Command::new(hew_binary())
        .arg("test")
        .arg(dir.path().join("missing"))
        .arg("--color=never")
        .output()
        .unwrap();

    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("path not found"));
}

#[test]
fn test_nonexistent_path_still_exits_one() {
    let output = Command::new(hew_binary())
        .arg("test")
        .arg("/no/such/path")
        .arg("--color=never")
        .output()
        .unwrap();

    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("path not found"), "stderr: {stderr}");
}

#[test]
fn junit_passing_suite_emits_xml_on_stdout() {
    require_codegen();

    let output = run_suite(
        &[(
            "passing_junit_test.hew",
            "#[test]\nfn ok() {\n    assert(true);\n}\n",
        )],
        &["--format", "junit"],
    );

    assert!(
        output.status.success(),
        "stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    assert_eq!(output.status.code(), Some(0));
    assert!(
        output.stderr.is_empty(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.starts_with("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("<testsuites"), "stdout: {stdout}");
    assert!(stdout.contains("<testsuite"), "stdout: {stdout}");
    assert!(
        stdout.contains(r#"<testcase name="ok""#),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("</testsuites>"), "stdout: {stdout}");
}

#[test]
fn junit_failing_suite_emits_failure_element_and_exits_one() {
    require_codegen();

    let output = run_suite(
        &[(
            "failing_junit_test.hew",
            "#[test]\nfn bad() {\n    panic(\"boom\");\n}\n",
        )],
        &["--format", "junit"],
    );

    assert!(!output.status.success());
    assert_eq!(output.status.code(), Some(1));

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.starts_with("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"),
        "stdout: {stdout}"
    );
    assert!(
        stdout.contains(r#"<testcase name="bad""#),
        "stdout: {stdout}"
    );
    assert!(
        stdout.contains(r#"<failure type="panic" message="#),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("boom"), "stdout: {stdout}");
}

#[test]
fn junit_ignored_suite_emits_skipped_element() {
    require_codegen();

    let output = run_suite(
        &[(
            "ignored_junit_test.hew",
            "#[test]\n#[ignore]\nfn skip_me() {\n    assert(false);\n}\n",
        )],
        &["--format", "junit"],
    );

    assert!(
        output.status.success(),
        "stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    assert_eq!(output.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains(r#"<testcase name="skip_me""#),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("<skipped/>"), "stdout: {stdout}");
}

#[test]
fn filter_with_no_matching_tests_reports_empty_selection() {
    require_codegen();

    let output = run_suite(
        &[(
            "filter_target_test.hew",
            "#[test]\nfn alpha() {\n    assert(true);\n}\n",
        )],
        &["--color=never", "--filter", "this_pattern_matches_nothing"],
    );

    assert_eq!(output.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&output.stderr)
        .contains("No tests matched the requested selection."));
}
