//! Test runner for the Hew programming language.
//!
//! Discovers `#[test]` functions in `.hew` source files, compiles each as an
//! isolated program via the native compilation pipeline, and reports results
//! with coloured output.

pub mod discovery;
pub mod output;
pub mod runner;

pub fn cmd_test(args: &crate::args::TestArgs) {
    let filter = args.filter.as_deref();
    let use_color = !args.no_color;
    let include_ignored = args.include_ignored;
    let format = match args.format {
        crate::args::TestFormat::Text => output::OutputFormat::Text,
        crate::args::TestFormat::Junit => output::OutputFormat::Junit,
    };
    let timeout = if args.timeout == 0 {
        eprintln!("Error: --timeout must be at least 1 second");
        std::process::exit(1);
    } else {
        std::time::Duration::from_secs(args.timeout)
    };
    let paths: Vec<String> = if args.paths.is_empty() {
        vec![".".to_string()]
    } else {
        args.paths.iter().map(|p| p.display().to_string()).collect()
    };

    // Discover test files and test cases.
    let mut all_tests = Vec::new();
    let mut discovered_files = 0usize;
    let mut had_parse_errors = false;
    for path in &paths {
        let p = std::path::Path::new(path);
        if !p.exists() {
            eprintln!("Error: path not found: {path}");
            std::process::exit(1);
        }
        if p.is_file() {
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
        std::process::exit(0);
    }

    if all_tests.is_empty() {
        eprintln!("No test functions found.");
        std::process::exit(0);
    }

    // Detect and build FFI staticlib if this is an ecosystem package with
    // a Cargo.toml (e.g. db/sqlite, image/magick).
    let cwd = std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("."));
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
