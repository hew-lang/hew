use std::fs;
use std::path::Path;

use hew_parser::ast::Item;

use super::{write_lockfile, write_source, write_toml};
use crate::diagnostics::is_warning_diagnostic;
use crate::manifest::{load_dependencies, load_lockfile, load_package_name};
use crate::module_loader::canonical_direct_stdlib_module_for_source;
use crate::{
    check_file, check_file_with_state, run_file_frontend_to_typecheck, FrontendDiagnosticKind,
    FrontendOptions, Session, SessionTarget,
};

#[test]
fn no_manifest_returns_none() {
    let dir = tempfile::tempdir().expect("create temp dir");
    assert!(load_dependencies(dir.path())
        .expect("missing manifest should not error")
        .is_none());
}

#[test]
fn package_name_loaded() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(dir.path(), "[package]\nname = \"myapp\"\n");
    assert_eq!(
        load_package_name(dir.path()).expect("valid manifest should load"),
        Some("myapp".to_string())
    );
}

#[test]
fn package_name_missing_section() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(dir.path(), "[dependencies]\n");
    assert_eq!(
        load_package_name(dir.path()).expect("valid manifest should load"),
        None
    );
}

#[test]
fn edition_2026_is_accepted() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(
        dir.path(),
        "[package]\nname = \"editpkg\"\nedition = \"2026\"\n",
    );
    assert_eq!(
        load_package_name(dir.path()).expect("edition 2026 should load"),
        Some("editpkg".to_string())
    );
}

#[test]
fn missing_edition_defaults_to_current() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(dir.path(), "[package]\nname = \"defaultpkg\"\n");
    assert_eq!(
        load_package_name(dir.path()).expect("missing edition should default"),
        Some("defaultpkg".to_string())
    );
}

#[test]
fn unsupported_edition_is_rejected() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(
        dir.path(),
        "[package]\nname = \"futurepkg\"\nedition = \"2027\"\n",
    );
    let err = load_package_name(dir.path()).expect_err("edition 2027 must be rejected");
    assert!(
        err.message.contains("E_UNSUPPORTED_EDITION"),
        "missing structured code: {}",
        err.message
    );
    assert!(
        err.message.contains("2027"),
        "missing edition in message: {}",
        err.message
    );
}

#[test]
fn package_name_no_manifest() {
    let dir = tempfile::tempdir().expect("create temp dir");
    assert_eq!(
        load_package_name(dir.path()).expect("missing manifest should not error"),
        None
    );
}

#[test]
fn manifest_no_deps_returns_some_empty() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(dir.path(), "[package]\nname = \"foo\"\n");
    let deps = load_dependencies(dir.path())
        .expect("manifest should load")
        .expect("manifest should be present");
    assert!(deps.is_empty());
}

#[test]
fn manifest_with_deps_returns_keys() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(
        dir.path(),
        "[dependencies]\nstd_utils = \"1.0\"\nmath = \"0.2\"\n",
    );
    let mut deps = load_dependencies(dir.path())
        .expect("manifest should load")
        .expect("manifest should be present");
    deps.sort();
    assert_eq!(deps, vec!["math", "std_utils"]);
}

#[test]
fn manifest_with_table_deps_returns_keys() {
    let dir = tempfile::tempdir().expect("create temp dir");
    // Table / path / feature dependency forms are accepted by the package manager; the
    // compiler must parse them too (it only needs the dependency names).
    write_toml(
        dir.path(),
        "[dependencies]\n\"hew::math::stats\" = { version = \"^0.1.0\" }\nlocal = { version = \"0.1.0\", path = \"../local\" }\nweb = { version = \"1.0\", features = [\"tls\"], optional = true }\n",
    );
    let mut deps = load_dependencies(dir.path())
        .expect("manifest should load")
        .expect("manifest should be present");
    deps.sort();
    assert_eq!(deps, vec!["hew::math::stats", "local", "web"]);
}

#[test]
fn a_path_dependency_needs_no_version() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(
        dir.path(),
        "[dependencies]\n\"acme.local\" = { path = \"../local\" }\n",
    );
    let deps = load_dependencies(dir.path())
        .expect("manifest should load")
        .expect("manifest should be present");
    assert_eq!(deps, vec!["acme.local"]);
}

#[test]
fn manifest_invalid_toml_returns_err() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(dir.path(), "this is not valid toml {{{\n");
    let err = load_dependencies(dir.path()).expect_err("invalid manifest should error");
    assert!(err.message.contains("cannot parse"), "{}", err.message);
    assert!(err.message.contains("hew.toml"), "{}", err.message);
}

#[test]
fn no_lockfile_returns_none() {
    let dir = tempfile::tempdir().expect("create temp dir");
    assert!(load_lockfile(dir.path())
        .expect("missing lockfile should not error")
        .is_none());
}

#[test]
fn empty_lockfile_returns_some_empty() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_lockfile(dir.path(), "# empty\n");
    let entries = load_lockfile(dir.path())
        .expect("lockfile should parse")
        .expect("lockfile should be present");
    assert!(entries.is_empty());
}

#[test]
fn lockfile_with_packages() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_lockfile(
        dir.path(),
        "[[package]]\nname = \"ecosystem::db::postgres\"\nversion = \"1.0.0\"\n\n\
         [[package]]\nname = \"std::net::http\"\nversion = \"2.1.0\"\n",
    );
    let mut entries = load_lockfile(dir.path())
        .expect("lockfile should parse")
        .expect("lockfile should be present");
    entries.sort();
    assert_eq!(
        entries,
        vec![
            ("ecosystem::db::postgres".to_string(), "1.0.0".to_string()),
            ("std::net::http".to_string(), "2.1.0".to_string()),
        ]
    );
}

#[test]
fn lockfile_ignores_extra_fields() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_lockfile(
        dir.path(),
        "[[package]]\nname = \"mypkg\"\nversion = \"0.1.0\"\nchecksum = \"sha256:abc\"\n",
    );
    let entries = load_lockfile(dir.path())
        .expect("lockfile should parse")
        .expect("lockfile should be present");
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0], ("mypkg".to_string(), "0.1.0".to_string()));
}

#[test]
fn lockfile_invalid_toml_returns_err() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_lockfile(dir.path(), "this is not valid toml {{{\n");
    let err = load_lockfile(dir.path()).expect_err("invalid lockfile should error");
    assert!(err.message.contains("cannot parse"), "{}", err.message);
    assert!(err.message.contains("hew.lock"), "{}", err.message);
}

#[test]
fn check_file_fails_closed_on_invalid_manifest() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(dir.path(), "this is not valid toml {{{\n");
    let input = dir.path().join("main.hew");
    fs::write(&input, "").expect("write main.hew");

    let err = check_file(
        input.to_str().expect("utf-8 path"),
        &FrontendOptions::default(),
    )
    .expect_err("invalid manifest should fail closed");
    assert!(err.message.contains("cannot parse"), "{}", err.message);
    assert!(err.message.contains("hew.toml"), "{}", err.message);
}

#[test]
fn check_file_fails_closed_on_invalid_lockfile() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(dir.path(), "[package]\nname = \"myapp\"\n");
    write_lockfile(dir.path(), "this is not valid toml {{{\n");
    let input = dir.path().join("main.hew");
    fs::write(&input, "").expect("write main.hew");

    let err = check_file(
        input.to_str().expect("utf-8 path"),
        &FrontendOptions::default(),
    )
    .expect_err("invalid lockfile should fail closed");
    assert!(err.message.contains("cannot parse"), "{}", err.message);
    assert!(err.message.contains("hew.lock"), "{}", err.message);
}

#[test]
fn check_file_preserves_warnings_without_werror() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let input = write_source(dir.path(), "main.hew", "fn main() { let unused = 42; }\n");

    let result = check_file(&input, &FrontendOptions::default()).expect("check should succeed");

    assert!(
        result.diagnostics.iter().any(is_warning_diagnostic),
        "expected warning diagnostics, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn check_file_fails_when_warnings_are_errors() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let input = write_source(dir.path(), "main.hew", "fn main() { let unused = 42; }\n");

    let failure = check_file(
        &input,
        &FrontendOptions {
            warnings_as_errors: true,
            ..Default::default()
        },
    )
    .expect_err("warnings should fail when warnings_as_errors is enabled");

    assert_eq!(failure.message, "warnings treated as errors");
    assert!(
        failure.diagnostics.iter().any(is_warning_diagnostic),
        "expected warning diagnostics, got: {:?}",
        failure.diagnostics
    );
}

#[test]
fn check_file_rejects_direct_non_floor_intrinsic_source() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let input = write_source(
        dir.path(),
        "math.hew",
        r#"#[intrinsic("math.abs")] pub fn abs<T: Num>(x: T) -> T;"#,
    );

    let failure = check_file(&input, &FrontendOptions::default())
        .expect_err("non-floor direct file must not declare intrinsics");
    assert!(
        failure.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            FrontendDiagnosticKind::Type(error)
                if matches!(
                    &error.kind,
                    hew_types::error::TypeErrorKind::IntrinsicOutsideFloor {
                        intrinsic_key,
                        ..
                    } if intrinsic_key == "math.abs"
                )
        )),
        "expected IntrinsicOutsideFloor for temp math.hew, got: {:?}",
        failure.diagnostics
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "this provenance integration test keeps positive and spoofed-source controls together"
)]
fn direct_std_stream_provenance_is_exact_to_the_shipped_source() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile lives below the repository root");
    let std_roots = [repo_root.to_path_buf()];
    let shipped = repo_root.join("std/stream.hew");
    assert_eq!(
        canonical_direct_stdlib_module_for_source(&shipped, &std_roots)
            .map(|module| module.dotted()),
        Some("std.stream".to_string()),
        "direct compilation of the shipped stream module must retain std.stream identity"
    );

    let dir = tempfile::tempdir().expect("create temp dir");
    let user_stream = write_source(
        dir.path(),
        "stream.hew",
        "type Sink<T> {\n    value: T;\n}\n\ntype Stream<T> {\n    value: T;\n}\n",
    );
    assert!(
        canonical_direct_stdlib_module_for_source(Path::new(&user_stream), &std_roots).is_none(),
        "a same-named user file must not acquire compiler-owned std.stream provenance"
    );

    let shipped_net = repo_root.join("std/net/net.hew");
    assert_eq!(
        canonical_direct_stdlib_module_for_source(&shipped_net, &std_roots)
            .map(|module| module.dotted()),
        Some("std.net".to_string()),
        "direct compilation of the shipped TCP module must retain std.net identity"
    );

    let shipped_lifecycle = repo_root.join("std/concurrency/lifecycle.hew");
    assert_eq!(
        canonical_direct_stdlib_module_for_source(&shipped_lifecycle, &std_roots)
            .map(|module| module.dotted()),
        Some("std.concurrency.lifecycle".to_string()),
        "a direct check of a shipped nested std module must retain its identity"
    );
    fs::create_dir_all(dir.path().join("concurrency")).expect("create user module dir");
    let user_lifecycle = write_source(
        &dir.path().join("concurrency"),
        "lifecycle.hew",
        "pub type Marker {}\n",
    );
    assert!(
        canonical_direct_stdlib_module_for_source(Path::new(&user_lifecycle), &std_roots).is_none(),
        "a same-named user file must not acquire std.concurrency.lifecycle provenance"
    );
    let user_net = write_source(dir.path(), "net.hew", "fn main() {}\n");
    assert!(
        canonical_direct_stdlib_module_for_source(Path::new(&user_net), &std_roots).is_none(),
        "a same-named user file must not acquire compiler-owned std.net provenance"
    );

    let std_net_state = run_file_frontend_to_typecheck(
        shipped_net.to_str().expect("std/net path is UTF-8"),
        &FrontendOptions::default(),
    )
    .expect("the shipped std.net source should type-check directly");
    let std_net_tco = std_net_state
        .typecheck_result
        .tco
        .as_ref()
        .expect("successful std.net check has type output");
    let std_net_hir = hew_hir::lower_program(
        &std_net_state.program,
        std_net_tco,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        std_net_hir.diagnostics.is_empty(),
        "the canonical direct std.net graph must retain the typed TCP borrow authority: {:#?}",
        std_net_hir.diagnostics
    );

    let spoof = write_source(
        dir.path(),
        "spoof.hew",
        r#"
#[resource]
#[opaque]
type Foo {}
impl Foo { fn close(consume self) {} }
extern "C" { fn hew_tcp_read(foo: Foo); }
"#,
    );
    let Err(spoof) = run_file_frontend_to_typecheck(&spoof, &FrontendOptions::default()) else {
        panic!("a user Foo must not inherit std.net.Connection's borrow row");
    };
    assert!(
        spoof.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            FrontendDiagnosticKind::Type(error)
                if error.kind == hew_types::error::TypeErrorKind::BoundaryResourceMustConsume
                    && error.message.contains("`hew_tcp_read`")
        )),
        "a user Foo must not inherit std.net.Connection's borrow row: {:#?}",
        spoof.diagnostics
    );
}

#[test]
fn root_named_connection_import_survives_transitive_first_registration() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_source(
        dir.path(),
        "helper.hew",
        "import std.net;\n\npub fn marker() -> i64 { 1 }\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import helper;\n\
         import std.net.{Connection};\n\n\
         fn close_connection(consume conn: Connection) { conn.close(); }\n\
         fn main() { let _ = helper.marker(); }\n",
    );

    check_file(&input, &FrontendOptions::default()).expect(
        "the root's named Connection binding and builtin close dispatch must survive when helper registered std::net transitively first",
    );
}

/// Two whole-module imports that publish the same source binding in one
/// scope are genuinely ambiguous and must be rejected. Their canonical
/// module IDs remain distinct; the conflict is solely the unaliased
/// `alpha` surface binding both imports would create.
#[test]
fn check_file_rejects_ambiguous_unaliased_module_binding() {
    let dir = tempfile::tempdir().expect("create temp dir");
    // Two modules whose short name (last path segment) is the same `alpha`:
    // a flat `alpha.hew` and a nested `beta/alpha.hew`.
    write_source(dir.path(), "alpha.hew", "pub fn val() -> i64 { 1 }\n");
    fs::create_dir_all(dir.path().join("beta")).expect("create beta dir");
    write_source(dir.path(), "beta/alpha.hew", "pub fn val() -> i64 { 2 }\n");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import alpha;\nimport beta.alpha;\n\nfn main() -> i64 { 0 }\n",
    );

    let failure = check_file(&input, &FrontendOptions::default())
        .expect_err("an ambiguous unaliased module binding must fail closed");
    assert!(
        failure.message.contains("ambiguous binding"),
        "expected an ambiguous module-binding diagnostic, got: {}",
        failure.message
    );
    assert!(
        failure.message.contains("alpha"),
        "diagnostic should name the colliding binding `alpha`, got: {}",
        failure.message
    );
}

/// Positive control: canonical module IDs may share their final component
/// when the source gives them distinct whole-module aliases.
#[test]
fn check_file_accepts_same_leaf_modules_with_distinct_aliases() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_source(dir.path(), "alpha.hew", "pub fn val() -> i64 { 1 }\n");
    fs::create_dir_all(dir.path().join("beta")).expect("create beta dir");
    write_source(dir.path(), "beta/alpha.hew", "pub fn val() -> i64 { 2 }\n");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import alpha as flat_alpha;\n\
         import beta.alpha as nested_alpha;\n\n\
         fn main() -> i64 { flat_alpha.val() + nested_alpha.val() }\n",
    );

    check_file(&input, &FrontendOptions::default())
        .expect("distinct aliases for same-leaf canonical modules must be accepted");
}

#[test]
fn package_directory_import_excludes_adjacent_test_files_from_public_surface() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let pkg_root = dir.path().join("packages");
    let sqlite_dir = pkg_root.join("db/sqlite");
    fs::create_dir_all(&sqlite_dir).expect("create package directory");
    write_source(&sqlite_dir, "sqlite.hew", "pub fn marker() -> i64 { 1 }\n");
    write_source(
        &sqlite_dir,
        "sqlite_test.hew",
        "import \"sqlite.hew\";\n\npub fn test_marker() -> i64 { sqlite.marker() }\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import hew.db.sqlite;\n\nfn main() -> i64 { sqlite.marker() }\n",
    );

    check_file(
        &input,
        &FrontendOptions {
            pkg_path: Some(pkg_root),
            ..Default::default()
        },
    )
    .expect("package import should ignore adjacent _test.hew imports");
}

#[test]
fn explicit_file_import_of_test_file_still_resolves_relative_imports() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_source(dir.path(), "sqlite.hew", "pub fn marker() -> i64 { 1 }\n");
    write_source(
        dir.path(),
        "sqlite_test.hew",
        "import \"sqlite.hew\";\n\npub fn test_marker() -> i64 { 1 }\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import \"sqlite_test.hew\";\n\nfn main() -> i64 { 0 }\n",
    );

    check_file(&input, &FrontendOptions::default())
        .expect("explicit file import should keep _test.hew semantics");
}

#[test]
fn std_import_does_not_fall_back_to_pkg_path_tail() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let pkg_root = dir.path().join("packages");
    fs::create_dir_all(&pkg_root).expect("create package root");
    write_source(&pkg_root, "bogus.hew", "pub fn marker() -> i64 { 1 }\n");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import std.bogus;\n\nfn main() {}\n",
    );

    let failure = check_file(
        &input,
        &FrontendOptions {
            no_typecheck: true,
            pkg_path: Some(pkg_root.clone()),
            ..Default::default()
        },
    )
    .expect_err("std.bogus must not resolve from --pkg-path/bogus.hew");

    assert!(
        failure.message.contains("module `std.bogus` not found"),
        "expected std.bogus to fail closed, got: {}",
        failure.message
    );
    let stripped_pkg_candidate = pkg_root.join("bogus.hew").display().to_string();
    assert!(
        !failure.message.contains(&stripped_pkg_candidate),
        "std. imports must not try stripped --pkg-path tail candidate `{stripped_pkg_candidate}`: {}",
        failure.message
    );
}

#[test]
fn std_import_does_not_resolve_from_pkg_path_std_root() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let pkg_root = dir.path().join("packages");
    let fake_std_dir = pkg_root.join("std");
    fs::create_dir_all(&fake_std_dir).expect("create fake std package dir");
    write_source(&fake_std_dir, "bogus.hew", "pub fn marker() -> i64 { 1 }\n");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import std.bogus;\n\nfn main() {}\n",
    );

    let failure = check_file(
        &input,
        &FrontendOptions {
            no_typecheck: true,
            pkg_path: Some(pkg_root.clone()),
            ..Default::default()
        },
    )
    .expect_err("std.bogus must not resolve from --pkg-path/std/bogus.hew");

    assert!(
        failure.message.contains("module `std.bogus` not found"),
        "expected std.bogus to fail closed, got: {}",
        failure.message
    );
    let fake_std_candidate = fake_std_dir.join("bogus.hew").display().to_string();
    assert!(
        !failure.message.contains(&fake_std_candidate),
        "std. imports must not try --pkg-path std-root candidate `{fake_std_candidate}`: {}",
        failure.message
    );
}

#[test]
fn std_import_does_not_resolve_from_package_cache_std_root() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let pkg_std_dir = dir.path().join(".hew/packages/std");
    fs::create_dir_all(&pkg_std_dir).expect("create fake .hew std package dir");
    write_source(&pkg_std_dir, "bogus.hew", "pub fn marker() -> i64 { 1 }\n");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import std.bogus;\n\nfn main() {}\n",
    );

    let failure = check_file(
        &input,
        &FrontendOptions {
            no_typecheck: true,
            project_dir: Some(dir.path().to_path_buf()),
            ..Default::default()
        },
    )
    .expect_err("std.bogus must not resolve from .hew/packages/std/bogus.hew");

    assert!(
        failure.message.contains("module `std.bogus` not found"),
        "expected std.bogus to fail closed, got: {}",
        failure.message
    );
    let fake_std_candidate = pkg_std_dir.join("bogus.hew").display().to_string();
    assert!(
        !failure.message.contains(&fake_std_candidate),
        "std. imports must not try .hew std-root candidate `{fake_std_candidate}`: {}",
        failure.message
    );
}

#[test]
fn std_import_prefers_compiler_std_over_pkg_path_tail_collision() {
    let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile lives under repo root");

    let dir = tempfile::tempdir().expect("create temp dir");
    let pkg_root = dir.path().join("packages");
    fs::create_dir_all(&pkg_root).expect("create package root");
    write_source(&pkg_root, "fs.hew", "pub fn marker() -> i64 { 1 }\n");
    let input = write_source(dir.path(), "main.hew", "import std.fs;\n\nfn main() {}\n");

    let (_output, state) = check_file_with_state(
        &input,
        &FrontendOptions {
            no_typecheck: true,
            pkg_path: Some(pkg_root.clone()),
            project_dir: Some(repo_root.to_path_buf()),
            ..Default::default()
        },
    )
    .expect("std::fs must resolve from compiler std without package-tail ambiguity");

    let import = state
        .program
        .items
        .iter()
        .find_map(|item| match &item.0 {
            Item::Import(import) if import.path.to_string() == "std.fs" => Some(import),
            _ => None,
        })
        .expect("std::fs import should remain in the program");
    assert_eq!(
        import.resolved_source_paths.len(),
        1,
        "std::fs should resolve to exactly one source path"
    );
    let resolved = &import.resolved_source_paths[0];
    assert!(
        resolved.ends_with("std/fs.hew"),
        "std::fs should resolve to compiler std/fs.hew, got {}",
        resolved.display()
    );
    assert!(
        !resolved.starts_with(&pkg_root),
        "std::fs must not resolve from colliding --pkg-path file {}",
        resolved.display()
    );
}

fn copy_tree(from: &Path, to: &Path) {
    fs::create_dir_all(to).expect("create copied directory");
    for entry in fs::read_dir(from).expect("read source directory") {
        let entry = entry.expect("read source entry");
        let target = to.join(entry.file_name());
        if entry.file_type().expect("source entry type").is_dir() {
            copy_tree(&entry.path(), &target);
        } else {
            fs::copy(entry.path(), &target).expect("copy source file");
        }
    }
}

/// The std roots a run is given are the canonical std, whatever
/// process-wide default exists (`HEW_STD`, the toolchain's own std). A
/// copy of the std outside the toolchain checks and lowers as the std: its
/// prelude declarations stay the builtin prelude rather than colliding
/// with it as a second, user-owned declaration, and its intrinsic floor
/// and directory modules import as the std.
#[test]
fn given_std_roots_are_the_canonical_std() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let toolchain = dir.path().join("toolchain");
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile lives below the repository root");
    copy_tree(&repo_root.join("std"), &toolchain.join("std"));
    let project_dir = dir.path().join("project");
    fs::create_dir(&project_dir).expect("create project");
    let input = write_source(
        &project_dir,
        "main.hew",
        "import std.math;\nimport std.encoding.json;\n\nfn main() {\n    \
         let values = [1, 2, 3];\n    var total = 0;\n    \
         for value in values {\n        total = total + value;\n    }\n    \
         println(f\"{total} {math.sqrt(16.0)}\");\n    \
         match json.parse(\"[1]\") {\n        .Ok(_) => println(\"parsed\"),\n        \
         .Err(_) => println(\"invalid\"),\n    }\n}\n",
    );
    let options = FrontendOptions {
        project_dir: Some(project_dir),
        module_search_paths: Some(vec![toolchain.clone()]),
        ..Default::default()
    };

    let state = run_file_frontend_to_typecheck(&input, &options)
        .unwrap_or_else(|failure| panic!("{}: {:#?}", failure.message, failure.diagnostics));

    assert_eq!(
        state.typecheck_result.module_registry.std_roots(),
        [toolchain]
    );
    let tco = state
        .typecheck_result
        .tco
        .as_ref()
        .expect("checked program");
    assert!(
        tco.canonical_std_modules
            .contains(&hew_parser::module::ModulePath::new(["std", "builtins"])),
        "the given std's builtins are the canonical builtins"
    );
    for module in [["std", "math"].as_slice(), &["std", "encoding", "json"]] {
        assert!(
            tco.canonical_std_modules
                .contains(&hew_parser::module::ModulePath::new(module.iter().copied())),
            "the given std's {module:?} is canonical"
        );
    }
    Session::from_frontend_options(SessionTarget::native(), &options)
        .lower_program(&state.program, tco)
        .expect("the program lowers against the given std");
}

/// A given std whose `std/builtins.hew` lacks a declaration the prelude
/// publishes is refused with a diagnostic naming that file, not a panic.
/// The control is the same copy with the shipped builtins intact.
#[test]
fn an_incomplete_std_builtins_is_a_diagnostic() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let toolchain = dir.path().join("toolchain");
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile lives below the repository root");
    copy_tree(&repo_root.join("std"), &toolchain.join("std"));
    let project_dir = dir.path().join("project");
    fs::create_dir(&project_dir).expect("create project");
    let input = write_source(
        &project_dir,
        "main.hew",
        "fn main() {\n    println(1);\n}\n",
    );
    let options = FrontendOptions {
        project_dir: Some(project_dir),
        module_search_paths: Some(vec![toolchain.clone()]),
        ..Default::default()
    };
    run_file_frontend_to_typecheck(&input, &options)
        .unwrap_or_else(|failure| panic!("intact std: {:#?}", failure.diagnostics));

    fs::write(toolchain.join("std/builtins.hew"), "pub fn nothing() {}\n")
        .expect("replace builtins");
    let Err(failure) = run_file_frontend_to_typecheck(&input, &options) else {
        panic!("an incomplete std must be refused");
    };
    let missing = failure
        .diagnostics
        .iter()
        .filter_map(|diagnostic| match &diagnostic.kind {
            FrontendDiagnosticKind::Type(error)
                if error.kind == hew_types::error::TypeErrorKind::MalformedStdSource =>
            {
                Some(error)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert!(
        missing.iter().any(
            |error| error.message.contains("does not declare `LinkError`")
                && error.message.contains("builtins.hew")
        ),
        "{:#?}",
        failure.diagnostics
    );
}

#[test]
fn explicit_module_search_paths_do_not_fall_back_to_process_layout() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let stdlib_root = dir.path().join("compiler-resources");
    let stdlib_dir = stdlib_root.join("std");
    fs::create_dir_all(&stdlib_dir).expect("create explicit stdlib root");
    // Supply only the frontend's declared floor; no process-layout source
    // may fill a missing module behind this explicit search root.
    for leaf in hew_types::stdlib_authority::IMPLICIT_PRELUDE_STD_SOURCES
        .iter()
        .map(|(leaf, _)| *leaf)
        .chain(hew_types::stdlib_authority::IMPLICIT_PRELUDE_MODULE_LOADS)
    {
        write_source(
            &stdlib_dir,
            &format!("{leaf}.hew"),
            "// explicit prelude marker\n",
        );
    }
    let expected = Path::new(&write_source(
        &stdlib_dir,
        "fs.hew",
        "pub fn explicit_marker() -> i64 { 1 }\n",
    ))
    .canonicalize()
    .expect("canonical explicit stdlib module");
    let project_dir = dir.path().join("external-project");
    fs::create_dir(&project_dir).expect("create external project");
    let input = write_source(&project_dir, "main.hew", "import std.fs;\n\nfn main() {}\n");

    let (_output, state) = check_file_with_state(
        &input,
        &FrontendOptions {
            no_typecheck: true,
            project_dir: Some(project_dir),
            module_search_paths: Some(vec![stdlib_root]),
            ..Default::default()
        },
    )
    .expect("the explicit stdlib root should resolve independently of cwd");

    let resolved = state
        .program
        .items
        .iter()
        .find_map(|item| match &item.0 {
            Item::Import(import) if import.path.to_string() == "std.fs" => {
                import.resolved_source_paths.first()
            }
            _ => None,
        })
        .expect("std::fs should resolve from the explicit root");
    assert_eq!(resolved, &expected);
}

#[test]
fn non_builtin_import_still_uses_pkg_path_tail_fallback() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let pkg_root = dir.path().join("packages");
    fs::create_dir_all(&pkg_root).expect("create package root");
    let package_file = Path::new(&write_source(
        &pkg_root,
        "fs.hew",
        "pub fn marker() -> i64 { 1 }\n",
    ))
    .canonicalize()
    .expect("canonical package file");
    let input = write_source(dir.path(), "main.hew", "import mypkg.fs;\n\nfn main() {}\n");

    let (_output, state) = check_file_with_state(
        &input,
        &FrontendOptions {
            no_typecheck: true,
            pkg_path: Some(pkg_root),
            ..Default::default()
        },
    )
    .expect("non-builtin package imports should still use stripped tail fallback");

    let import = state
        .program
        .items
        .iter()
        .find_map(|item| match &item.0 {
            Item::Import(import) if import.path.to_string() == "mypkg.fs" => Some(import),
            _ => None,
        })
        .expect("mypkg::fs import should remain in the program");
    assert_eq!(
        import.resolved_source_paths,
        vec![package_file],
        "mypkg::fs should resolve through --pkg-path/fs.hew"
    );
}

#[test]
fn hew_package_layout_still_uses_explicit_pkg_path_tail_fallback() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let pkg_root = dir.path().join("packages");
    let sqlite_dir = pkg_root.join("db/sqlite");
    fs::create_dir_all(&sqlite_dir).expect("create package directory");
    let package_file = Path::new(&write_source(
        &sqlite_dir,
        "sqlite.hew",
        "pub fn marker() -> i64 { 1 }\n",
    ))
    .canonicalize()
    .expect("canonical package file");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import hew.db.sqlite;\n\nfn main() {}\n",
    );

    let (_output, state) = check_file_with_state(
        &input,
        &FrontendOptions {
            no_typecheck: true,
            pkg_path: Some(pkg_root),
            ..Default::default()
        },
    )
    .expect("hew:: package-layout import should keep using its explicit fallback");

    let import = state
        .program
        .items
        .iter()
        .find_map(|item| match &item.0 {
            Item::Import(import) if import.path.to_string() == "hew.db.sqlite" => Some(import),
            _ => None,
        })
        .expect("hew::db::sqlite import should remain in the program");
    assert_eq!(
        import.resolved_source_paths,
        vec![package_file],
        "hew::db::sqlite should resolve through the explicit hew:: package-layout fallback"
    );
}

#[test]
fn ecosystem_package_layout_still_uses_explicit_pkg_path_tail_fallback() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let pkg_root = dir.path().join("packages");
    let postgres_dir = pkg_root.join("db/postgres");
    fs::create_dir_all(&postgres_dir).expect("create package directory");
    let package_file = Path::new(&write_source(
        &postgres_dir,
        "postgres.hew",
        "pub fn marker() -> i64 { 1 }\n",
    ))
    .canonicalize()
    .expect("canonical package file");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import ecosystem.db.postgres;\n\nfn main() {}\n",
    );

    let (_output, state) = check_file_with_state(
        &input,
        &FrontendOptions {
            no_typecheck: true,
            pkg_path: Some(pkg_root),
            ..Default::default()
        },
    )
    .expect("ecosystem:: package-layout import should keep using its explicit fallback");

    let import = state
        .program
        .items
        .iter()
        .find_map(|item| match &item.0 {
            Item::Import(import) if import.path.to_string() == "ecosystem.db.postgres" => {
                Some(import)
            }
            _ => None,
        })
        .expect("ecosystem::db::postgres import should remain in the program");
    assert_eq!(
        import.resolved_source_paths,
        vec![package_file],
        "ecosystem::db::postgres should resolve through the explicit ecosystem:: package-layout fallback"
    );
}
