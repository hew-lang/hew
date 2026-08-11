//! Test runner for the Hew programming language.
//!
//! Discovers `#[test]` functions in `.hew` source files, compiles each as an
//! isolated program via the native compilation pipeline, and reports results
//! with coloured output.

pub mod discovery;
pub mod output;
pub mod runner;

use std::collections::HashSet;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TestPartition {
    shard: u64,
    total: u64,
}

impl TestPartition {
    fn parse(value: &str) -> Result<Self, String> {
        let Some(specification) = value.strip_prefix("hash:") else {
            return Err(format!(
                "unsupported partition `{value}`; expected hash:SHARD/TOTAL"
            ));
        };
        let Some((shard, total)) = specification.split_once('/') else {
            return Err(format!(
                "invalid partition `{value}`; expected hash:SHARD/TOTAL"
            ));
        };
        let shard = shard
            .parse::<u64>()
            .map_err(|_| format!("invalid partition shard in `{value}`"))?;
        let total = total
            .parse::<u64>()
            .map_err(|_| format!("invalid partition total in `{value}`"))?;
        if total == 0 || shard == 0 || shard > total {
            return Err(format!(
                "partition `{value}` must satisfy 1 <= SHARD <= TOTAL"
            ));
        }
        Ok(Self { shard, total })
    }

    fn contains(self, identity: &str) -> bool {
        stable_hash(identity) % self.total + 1 == self.shard
    }
}

fn stable_hash(value: &str) -> u64 {
    // FNV-1a is deliberately fixed here: DefaultHasher makes no stability
    // promise, while CI partitions must not reshuffle between Rust releases.
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for byte in value.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash
}

fn test_identity(test: &discovery::TestCase, root: &Path) -> String {
    let file = Path::new(&test.file);
    let relative = file.strip_prefix(root).unwrap_or(file);
    format!(
        "{}::{}",
        relative.to_string_lossy().replace('\\', "/"),
        test.name
    )
}

fn parse_partition_argument(value: Option<&str>) -> Option<TestPartition> {
    value
        .map(TestPartition::parse)
        .transpose()
        .unwrap_or_else(|error| {
            eprintln!("Error: {error}");
            std::process::exit(2);
        })
}

fn output_test_list(tests: &[discovery::TestCase], filter: Option<&str>, root: &Path) {
    let mut identities: Vec<_> = tests
        .iter()
        .filter(|test| filter.is_none_or(|pattern| test.name.contains(pattern)))
        .map(|test| test_identity(test, root))
        .collect();
    identities.sort();
    for identity in identities {
        println!("{identity}");
    }
}

fn requested_test_paths(args: &crate::args::TestArgs) -> Vec<String> {
    let paths = if args.paths.is_empty() {
        vec![PathBuf::from(".")]
    } else {
        args.paths.clone()
    };
    canonicalize_test_paths(&paths).unwrap_or_else(|error| {
        eprintln!("Error: {error}");
        std::process::exit(1);
    })
}

pub fn cmd_test(args: &crate::args::TestArgs) {
    let filter = args.filter.as_deref();
    let partition = parse_partition_argument(args.partition.as_deref());
    let use_color = !args.no_color;
    let include_ignored = args.include_ignored;
    let format = match args.format {
        crate::args::TestFormat::Text => output::OutputFormat::Text,
        crate::args::TestFormat::Junit => output::OutputFormat::Junit,
    };
    let timeout = crate::util::parse_timeout(&args.timeout).unwrap_or_else(|e| {
        eprintln!("Error: {e}");
        std::process::exit(1);
    });
    let paths = requested_test_paths(args);

    // Discover test files and test cases.
    let mut all_tests = Vec::new();
    let mut discovered_files = 0usize;
    let mut had_parse_errors = false;
    let mut seen_files = HashSet::new();
    for path in &paths {
        let p = Path::new(path);
        if p.is_file() {
            if !seen_files.insert(path.clone()) {
                continue;
            }
            match discovery::discover_tests_in_file(path) {
                Ok(discovered) => {
                    discovered_files += 1;
                    had_parse_errors |= handle_discovered_file(&discovered);
                    all_tests.extend(discovered.tests);
                }
                Err(e) => {
                    eprintln!("Error: {e}");
                    std::process::exit(1);
                }
            }
        } else {
            match discovery::discover_test_files(path) {
                Ok(files) => {
                    for file in files {
                        if !seen_files.insert(file.clone()) {
                            continue;
                        }
                        match discovery::discover_tests_in_file(&file) {
                            Ok(discovered) => {
                                discovered_files += 1;
                                had_parse_errors |= handle_discovered_file(&discovered);
                                all_tests.extend(discovered.tests);
                            }
                            Err(e) => eprintln!("Warning: {e}"),
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Error: {e}");
                    std::process::exit(1);
                }
            }
        }
    }

    if had_parse_errors {
        std::process::exit(1);
    }

    if discovered_files == 0 {
        eprintln!("No test files found.");
        std::process::exit(i32::from(!args.allow_empty));
    }

    if all_tests.is_empty() {
        eprintln!("No test functions found.");
        std::process::exit(i32::from(!args.allow_empty));
    }

    let root = std::env::current_dir()
        .and_then(|path| path.canonicalize())
        .unwrap_or_else(|_| PathBuf::from("."));
    if let Some(partition) = partition {
        all_tests.retain(|test| partition.contains(&test_identity(test, &root)));
    }

    if args.list {
        output_test_list(&all_tests, filter, &root);
        return;
    }

    // Detect and build FFI staticlib if this is an ecosystem package with
    // a Cargo.toml (e.g. db/sqlite, image/magick).
    let cwd = root;
    let ffi_lib = match detect_and_build_ffi_lib(&cwd) {
        Ok(lib) => lib,
        Err(e) => {
            eprintln!("Error building FFI library: {e}");
            std::process::exit(1);
        }
    };

    let summary = runner::run_tests(
        &all_tests,
        filter,
        include_ignored,
        ffi_lib.as_deref(),
        timeout,
    );
    output::output_results(&summary, use_color, format);

    if summary.failed > 0 {
        std::process::exit(1);
    }
}

fn canonicalize_test_paths(paths: &[PathBuf]) -> Result<Vec<String>, String> {
    paths
        .iter()
        .map(|path| {
            if !path.exists() {
                return Err(format!("path not found: {}", path.display()));
            }
            path.canonicalize()
                .map_err(|error| format!("cannot canonicalize {}: {error}", path.display()))
                .map(|path| path.display().to_string())
        })
        .collect()
}

fn handle_discovered_file(file: &discovery::DiscoveredTestFile) -> bool {
    let mut had_errors = false;
    for error in &file.parse_errors {
        let hints: Vec<String> = error.hint.iter().cloned().collect();
        match error.severity {
            hew_parser::Severity::Warning => crate::diagnostic::render_warning(
                &file.source,
                &file.path,
                &error.span,
                &error.message,
                &[],
                &hints,
            ),
            hew_parser::Severity::Error => {
                had_errors = true;
                crate::diagnostic::render_diagnostic(
                    &file.source,
                    &file.path,
                    &error.span,
                    &error.message,
                    &[],
                    &hints,
                );
            }
        }
    }
    had_errors
}

/// Detect whether the current directory (or a close ancestor) is an FFI-backed
/// ecosystem package — i.e. has both `hew.toml` and `Cargo.toml` — and if so,
/// build the Rust staticlib with `cargo build --release` and return the path to
/// the resulting `.a` file.
fn detect_and_build_ffi_lib(start_dir: &std::path::Path) -> Result<Option<String>, String> {
    // Walk start_dir and up to 2 ancestors looking for a directory with both files.
    let mut dir = start_dir.to_path_buf();
    let mut found = false;
    for _ in 0..3 {
        if dir.join("hew.toml").exists() && dir.join("Cargo.toml").exists() {
            found = true;
            break;
        }
        if let Some(parent) = dir.parent() {
            dir = parent.to_path_buf();
        } else {
            break;
        }
    }

    if !found {
        return Ok(None);
    }

    // Parse Cargo.toml to get the crate name.
    let cargo_toml_path = dir.join("Cargo.toml");
    let cargo_toml_content = std::fs::read_to_string(&cargo_toml_path)
        .map_err(|e| format!("cannot read {}: {e}", cargo_toml_path.display()))?;
    let cargo_toml: toml::Value = toml::from_str(&cargo_toml_content)
        .map_err(|e| format!("cannot parse {}: {e}", cargo_toml_path.display()))?;
    let crate_name = cargo_toml
        .get("package")
        .and_then(|p| p.get("name"))
        .and_then(toml::Value::as_str)
        .ok_or_else(|| {
            format!(
                "cannot find [package].name in {}",
                cargo_toml_path.display()
            )
        })?;

    // Build the staticlib.
    eprintln!("Building FFI library: {crate_name}");
    let status = std::process::Command::new("cargo")
        .args(["build", "--release"])
        .current_dir(&dir)
        .status()
        .map_err(|e| format!("cannot run cargo build: {e}"))?;
    if !status.success() {
        return Err("cargo build --release failed".into());
    }

    // Find the workspace target directory.
    let target_dir = find_cargo_target_dir(&dir);

    // Crate names use underscores in library filenames.
    let lib_name = crate_name.replace('-', "_");
    let lib_path = target_dir.join("release").join(format!("lib{lib_name}.a"));

    if !lib_path.exists() {
        return Err(format!(
            "expected staticlib not found: {}",
            lib_path.display()
        ));
    }

    let canonical = lib_path
        .canonicalize()
        .unwrap_or(lib_path)
        .display()
        .to_string();
    Ok(Some(canonical))
}

/// Find the Cargo target directory for a package by walking up to find a
/// workspace `Cargo.toml` (one containing `[workspace]`).
fn find_cargo_target_dir(package_dir: &std::path::Path) -> std::path::PathBuf {
    let mut dir = package_dir.to_path_buf();
    while let Some(parent) = dir.parent() {
        let candidate = parent.join("Cargo.toml");
        if candidate.exists() {
            if let Ok(content) = std::fs::read_to_string(&candidate) {
                if let Ok(parsed) = toml::from_str::<toml::Value>(&content) {
                    if parsed.get("workspace").is_some() {
                        return parent.join("target");
                    }
                }
            }
        }
        dir = parent.to_path_buf();
    }
    // Fallback: use the package's own target directory.
    package_dir.join("target")
}

#[cfg(test)]
mod partition_tests {
    use super::*;

    #[test]
    fn partition_parser_accepts_nextest_hash_syntax() {
        assert_eq!(
            TestPartition::parse("hash:2/4").unwrap(),
            TestPartition { shard: 2, total: 4 }
        );
    }

    #[test]
    fn partition_parser_rejects_missing_and_out_of_range_shards() {
        for invalid in ["2/4", "hash:0/4", "hash:5/4", "hash:1/0", "hash:a/4"] {
            assert!(TestPartition::parse(invalid).is_err(), "accepted {invalid}");
        }
    }

    #[test]
    fn every_identity_belongs_to_exactly_one_partition() {
        for identity in [
            "tests/hew/actor_test.hew::actor_starts",
            "tests/hew/path_test.hew::glob_is_sorted",
            "tests/hew/vec_test.hew::vec_pushes",
        ] {
            let owners = (1..=4)
                .filter(|shard| {
                    TestPartition {
                        shard: *shard,
                        total: 4,
                    }
                    .contains(identity)
                })
                .count();
            assert_eq!(owners, 1, "partition ownership for {identity}");
        }
    }

    #[test]
    fn identity_is_repository_relative_and_path_stable() {
        let test = discovery::TestCase {
            name: "works".into(),
            file: "/repo/tests/hew/sample_test.hew".into(),
            ignored: false,
            should_panic: false,
        };
        assert_eq!(
            test_identity(&test, Path::new("/repo")),
            "tests/hew/sample_test.hew::works"
        );
    }
}
