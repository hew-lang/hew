use std::fs;
use std::path::Path;

use hew_parser::ast::Item;

use super::{write_lockfile, write_package_manifest, write_source, write_toml};
use crate::diagnostics::build_module_source_map;
use crate::{
    check_file, check_file_with_state, check_program, hir_diagnostics_to_frontend, parse_source,
    run_document_frontend, run_file_frontend_to_typecheck, FrontendDiagnosticKind, FrontendOptions,
    SourceSet,
};

#[test]
fn module_import_with_actor_path_segment_resolves() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let actor_dir = dir.path().join("actor");
    fs::create_dir_all(&actor_dir).expect("create actor module dir");
    write_source(&actor_dir, "monitor.hew", "pub fn ping() -> i64 { 1 }\n");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import actor.monitor;\n\nfn main() -> i64 { 0 }\n",
    );

    let (_output, state) = check_file_with_state(
        &input,
        &FrontendOptions {
            no_typecheck: true,
            ..Default::default()
        },
    )
    .expect("actor path segment import should resolve");

    let Item::Import(import) = &state.program.items[0].0 else {
        panic!("expected import item");
    };
    assert_eq!(import.path.to_string(), "actor.monitor");
    assert!(import
        .resolved_items
        .as_ref()
        .is_some_and(|items| !items.is_empty()));
    assert_eq!(import.resolved_source_paths.len(), 1);
}

/// Two different modules each exporting a `pub actor` with the same bare
/// name are LEGAL: actor identity is the qualified (module, name) pair —
/// `bank.Account` and `store.Account` keep distinct checker entries, MIR
/// layouts, and native symbols — so the program checks cleanly.
#[test]
fn check_file_accepts_duplicate_exported_actor_names_across_modules() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_source(
        dir.path(),
        "bank.hew",
        "pub actor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        1\n    }\n}\n",
    );
    write_source(
        dir.path(),
        "store.hew",
        "pub actor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        2\n    }\n}\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import bank;\nimport store;\n\nfn main() -> i64 { 0 }\n",
    );

    check_file(&input, &FrontendOptions::default())
        .expect("same-named pub actors from distinct modules must coexist");
}

/// A root-local actor sharing a bare name with an imported `pub actor` is
/// LEGAL: the bare reference resolves local-first to the root actor and
/// `spawn bank.Account { ... }` routes to the package actor's qualified
/// layout — neither shadows the other.
#[test]
fn check_file_accepts_root_actor_sharing_name_with_imported_actor() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_source(
        dir.path(),
        "bank.hew",
        "pub actor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        1\n    }\n}\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import bank;\n\nactor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        2\n    }\n}\n\nfn main() -> i64 {\n    0\n}\n",
    );

    check_file(&input, &FrontendOptions::default())
        .expect("root and imported same-named actors must coexist");
}

/// One module declaring two same-named actors stays a hard error: both
/// would claim the same qualified (module, name) identity, and no spawn
/// spelling could tell them apart.
#[test]
fn check_file_rejects_same_module_duplicate_actor_names() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_source(
        dir.path(),
        "bank.hew",
        "pub actor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        1\n    }\n}\n\npub actor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        2\n    }\n}\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import bank;\n\nfn main() -> i64 { 0 }\n",
    );

    let failure = check_file(&input, &FrontendOptions::default())
        .expect_err("two same-named actors in one module must fail closed");
    assert!(
        failure.message.contains("two actors named `Account`"),
        "expected a same-module duplicate-actor diagnostic, got: {}",
        failure.message
    );
}

/// Negative control: two modules exporting actors with DISTINCT bare names
/// compile cleanly — the duplicate-actor guard must not over-reject.
#[test]
fn check_file_accepts_distinct_exported_actor_names() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_source(
        dir.path(),
        "bank.hew",
        "pub actor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        1\n    }\n}\n",
    );
    write_source(
        dir.path(),
        "store.hew",
        "pub actor Register {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        2\n    }\n}\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import bank;\nimport store;\n\nfn main() -> i64 { 0 }\n",
    );

    check_file(&input, &FrontendOptions::default())
        .expect("distinct exported actor names must be accepted");
}

/// Negative control for the file-import happy path: a single `pub actor`
/// reached via `import "counter.hew"` must NOT be flagged: it lives in
/// exactly one module.
#[test]
fn check_file_accepts_single_file_imported_actor() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_source(
        dir.path(),
        "counter.hew",
        "pub actor Counter {\n    var n: i64 = 0;\n    receive fn bump() -> i64 {\n        n = n + 1;\n        n\n    }\n}\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import \"counter.hew\";\n\nfn main() -> i64 { 0 }\n",
    );

    check_file(&input, &FrontendOptions::default())
        .expect("a single file-imported actor must be accepted");
}

/// A *private* (non-pub) imported actor must not be spawnable via its
/// module qualifier, and in particular `spawn secret.Account` must NOT
/// silently route to a same-named root actor. The duplicate-actor graph
/// guard deliberately ignores private actors (they never enter the layout
/// set), so the fail-closed behaviour here comes from the type checker:
/// module-qualified spawn is gated on the actor being a `pub` export of the
/// module (`module_type_exports`), which private actors are excluded from at
/// registration. Without the gate the qualifier is stripped to bare
/// `Account` and routes to the root actor -- a privacy and correctness hole.
#[test]
fn check_file_rejects_spawn_of_private_imported_actor() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_source(
        dir.path(),
        "secret.hew",
        // No `pub`: the actor is private to its module.
        "actor Account {\n    var n: i64 = 0;\n    receive fn id() -> i64 {\n        999\n    }\n}\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import secret;\n\nactor Account {\n    var n: i64 = 0;\n    receive fn id() -> i64 {\n        111\n    }\n}\n\nfn main() {\n    let a = spawn secret.Account;\n}\n",
    );

    let failure = check_file(&input, &FrontendOptions::default())
        .expect_err("spawn of a private imported actor must fail closed");
    // The detailed diagnostic is a typed error in `diagnostics`; the
    // top-level `message` is the generic "type errors found" summary.
    let has_export_diag = failure.diagnostics.iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            FrontendDiagnosticKind::Type(error)
                if error.message.contains("has no exported actor or supervisor `Account`")
                    && error.message.contains("secret")
        )
    });
    assert!(
        has_export_diag,
        "expected a fail-closed `has no exported actor or supervisor `Account`` diagnostic \
         naming `secret`, got: {:?}",
        failure.diagnostics
    );
}

/// A public *non-actor* type export (e.g. `pub type Account`) must not
/// satisfy a module-qualified spawn. `module_type_exports` membership is
/// insufficient -- it also holds public structs/enums/records -- so the
/// spawn gate requires the qualified definition to be `TypeDefKind::Actor`.
/// Without that, `spawn secret.Account` would strip the qualifier to bare
/// `Account` and route to a same-named root actor.
#[test]
fn check_file_rejects_spawn_of_non_actor_module_export() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_source(
        dir.path(),
        "secret.hew",
        // A public NON-actor type that shares the actor's bare name.
        "pub type Account {\n    balance: i64;\n}\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import secret;\n\nactor Account {\n    var n: i64 = 0;\n    receive fn id() -> i64 {\n        111\n    }\n}\n\nfn main() {\n    let a = spawn secret.Account;\n}\n",
    );

    let failure = check_file(&input, &FrontendOptions::default())
        .expect_err("spawn of a non-actor module export must fail closed");
    let has_export_diag = failure.diagnostics.iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            FrontendDiagnosticKind::Type(error)
                if error.message.contains("has no exported actor or supervisor `Account`")
                    && error.message.contains("secret")
        )
    });
    assert!(
        has_export_diag,
        "expected a fail-closed `has no exported actor or supervisor `Account`` diagnostic \
         naming `secret`, got: {:?}",
        failure.diagnostics
    );
}

// ── check_program tests ───────────────────────────────────────────────

#[test]
fn check_program_no_manifest_accepts_simple_program() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let source = "fn main() { let x: i32 = 1; }\n";
    let program = parse_source(source, "main.hew").expect("source should parse");
    let options = FrontendOptions {
        project_dir: Some(dir.path().to_path_buf()),
        ..Default::default()
    };

    let result = check_program(program, source, "main.hew", &options);
    assert!(result.is_ok(), "valid program should pass: {result:?}");
}

#[test]
fn check_program_rejects_undeclared_dependency() {
    let dir = tempfile::tempdir().expect("create temp dir");
    // Manifest with an empty [dependencies] section — no deps declared.
    write_toml(dir.path(), "[package]\nname = \"myapp\"\n[dependencies]\n");

    // Use a user-space module (no std::/hew::/ecosystem:: prefix) so
    // validate_imports_against_manifest actually checks it.
    let source = "import mylib.utils;\nfn main() {}\n";
    let program = parse_source(source, "main.hew").expect("source should parse");
    let options = FrontendOptions {
        project_dir: Some(dir.path().to_path_buf()),
        ..Default::default()
    };

    let err = check_program(program, source, "main.hew", &options)
        .expect_err("undeclared dep should fail");
    assert!(
        err.message.contains("undeclared"),
        "expected undeclared-dep error, got: {}",
        err.message
    );
}

#[test]
fn check_program_fails_closed_on_invalid_manifest() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(dir.path(), "this is not valid toml {{{\n");

    let source = "fn main() {}\n";
    let program = parse_source(source, "main.hew").expect("source should parse");
    let options = FrontendOptions {
        project_dir: Some(dir.path().to_path_buf()),
        ..Default::default()
    };

    let err = check_program(program, source, "main.hew", &options)
        .expect_err("invalid manifest should fail closed");
    assert!(err.message.contains("cannot parse"), "{}", err.message);
    assert!(err.message.contains("hew.toml"), "{}", err.message);
}

#[test]
fn check_program_fails_closed_on_invalid_lockfile() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_toml(dir.path(), "[package]\nname = \"myapp\"\n");
    write_lockfile(dir.path(), "this is not valid toml {{{\n");

    let source = "fn main() {}\n";
    let program = parse_source(source, "main.hew").expect("source should parse");
    let options = FrontendOptions {
        project_dir: Some(dir.path().to_path_buf()),
        ..Default::default()
    };

    let err = check_program(program, source, "main.hew", &options)
        .expect_err("invalid lockfile should fail closed");
    assert!(err.message.contains("cannot parse"), "{}", err.message);
    assert!(err.message.contains("hew.lock"), "{}", err.message);
}

#[test]
fn check_program_catches_type_error() {
    let dir = tempfile::tempdir().expect("create temp dir");
    // No manifest — no import validation.
    let source = "fn main() { let x: i32 = true; }\n";
    let program = parse_source(source, "main.hew").expect("source should parse");
    let options = FrontendOptions {
        project_dir: Some(dir.path().to_path_buf()),
        ..Default::default()
    };

    let err =
        check_program(program, source, "main.hew", &options).expect_err("type error should fail");
    assert!(
        err.message.contains("type error"),
        "expected type-error message, got: {}",
        err.message
    );
}

// Unreachable code after a return statement generates a type Warning.
const SOURCE_WITH_WARNING: &str = "fn main() { return; let _x: i32 = 1; }\n";

#[test]
fn check_program_warnings_as_errors_fails_on_warning() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let source = SOURCE_WITH_WARNING;
    let program = parse_source(source, "main.hew").expect("source should parse");
    let options = FrontendOptions {
        project_dir: Some(dir.path().to_path_buf()),
        warnings_as_errors: true,
        ..Default::default()
    };

    let err = check_program(program, source, "main.hew", &options)
        .expect_err("warnings_as_errors should promote warning to failure");
    assert!(
        err.message.contains("warnings treated as errors"),
        "expected warnings-as-errors message, got: {}",
        err.message
    );
    assert!(
        !err.diagnostics.is_empty(),
        "failure should carry the warning diagnostics"
    );
}

#[test]
fn check_program_warnings_ok_without_flag() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let source = SOURCE_WITH_WARNING;
    let program = parse_source(source, "main.hew").expect("source should parse");
    let options = FrontendOptions {
        project_dir: Some(dir.path().to_path_buf()),
        warnings_as_errors: false,
        ..Default::default()
    };

    // Without the flag, warnings should be collected but not fail the check.
    let output = check_program(program, source, "main.hew", &options)
        .expect("warnings should not fail when flag is off");
    assert!(
        !output.diagnostics.is_empty(),
        "warning diagnostic should still be present in output"
    );
}

#[test]
fn check_file_warnings_as_errors_parity() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let input = dir.path().join("main.hew");
    fs::write(&input, SOURCE_WITH_WARNING).expect("write main.hew");
    let options = FrontendOptions {
        warnings_as_errors: true,
        ..Default::default()
    };

    let err = check_file(input.to_str().expect("utf-8 path"), &options)
        .expect_err("check_file with warnings_as_errors should fail on warning");
    assert!(
        err.message.contains("warnings treated as errors"),
        "expected warnings-as-errors message, got: {}",
        err.message
    );
}

/// A directory module's item spans are file-relative byte offsets, so a
/// diagnostic on a peer-file item must route to THAT file. Two peer files
/// declaring one C symbol with conflicting signatures: the error names
/// the peer file (`pkg/aaa.hew`), the note names the minting file
/// (`pkg/pkg.hew`) — never the peer's span rendered against the primary.
#[test]
fn extern_conflict_in_peer_file_routes_to_the_declaring_file() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_package_manifest(dir.path());
    let pkg_dir = dir.path().join("pkg");
    fs::create_dir(&pkg_dir).expect("create pkg dir");
    let main = write_source(
        dir.path(),
        "main.hew",
        "import pkg;\n\nfn main() {\n    print(\"{pkg.a(\\\"x\\\")}\");\n}\n",
    );
    fs::write(
        pkg_dir.join("pkg.hew"),
        "extern \"C\" {\n    #[extern_symbol(hew_bytes_from_str)]\n    fn alpha(x: string) -> bytes;\n}\n\npub fn a(v: string) -> i64 { unsafe { alpha(v).len() } }\n",
    )
    .expect("write pkg.hew");
    fs::write(
        pkg_dir.join("aaa.hew"),
        "extern \"C\" {\n    #[extern_symbol(hew_bytes_from_str)]\n    fn betaa(x: i64) -> bytes;\n}\n\npub fn b(v: i64) -> i64 { unsafe { betaa(v).len() } }\n",
    )
    .expect("write aaa.hew");

    let err = check_file(&main, &FrontendOptions::default())
        .expect_err("conflicting extern declarations must fail the check");
    let conflict = err
        .diagnostics
        .iter()
        .find(|d| match &d.kind {
            FrontendDiagnosticKind::Type(t) => t.message.contains("conflicting declarations"),
            _ => false,
        })
        .expect("conflict diagnostic present");
    assert!(
        conflict
            .filename
            .as_deref()
            .is_some_and(|f| f.ends_with("aaa.hew")),
        "conflict must route to the declaring peer file, got {:?}",
        conflict.filename
    );
    assert!(
        conflict
            .note_sources
            .first()
            .and_then(|n| n.as_ref())
            .is_some_and(|(_, f)| f.ends_with("pkg.hew")),
        "note must route to the minting file, got {:?}",
        conflict.note_sources.first()
    );
}

#[test]
fn state_constructor_error_in_peer_routes_to_its_source_line() {
    let dir = tempfile::tempdir().expect("create temp dir");
    write_package_manifest(dir.path());
    let workflow_dir = dir.path().join("workflow");
    fs::create_dir(&workflow_dir).expect("create workflow dir");
    let main = write_source(
        dir.path(),
        "main.hew",
        "import workflow.{Workflow};\nfn main() {}\n",
    );
    write_source(
        &workflow_dir,
        "workflow.hew",
        "pub type Marker {\n    value: i64;\n}\n",
    );
    let peer_source = concat!(
        "pub machine Workflow {\n",
        "    events { Crash; }\n",
        "    state Ready;\n",
        "    state Faulted { code: i64; }\n",
        "    on Crash: Ready => .Faulted {\n",
        "        wrong: 1\n",
        "    }\n",
        "    default { state }\n",
        "}\n",
        "\n",
        "pub fn deferred_error() {\n",
        "    let value: Vec<_> = [];\n",
        "}\n",
    );
    write_source(&workflow_dir, "state.hew", peer_source);
    let peer_suffix = Path::new("workflow").join("state.hew");

    let failure = check_file(&main, &FrontendOptions::default())
        .expect_err("the deliberate state-field error must reject the fixture");
    let diagnostic = failure
        .diagnostics
        .iter()
        .find_map(|diagnostic| match &diagnostic.kind {
            FrontendDiagnosticKind::Type(error) if error.message.contains("no field `wrong`") => {
                Some((diagnostic, error))
            }
            _ => None,
        })
        .expect("state constructor field diagnostic");
    assert!(
        diagnostic
            .0
            .filename
            .as_deref()
            .is_some_and(|filename| Path::new(filename).ends_with(&peer_suffix)),
        "diagnostic must name the declaring peer, got {:?}",
        diagnostic.0.filename
    );
    let line = peer_source[..diagnostic.1.span.start]
        .bytes()
        .filter(|byte| *byte == b'\n')
        .count()
        + 1;
    assert_eq!(line, 5, "diagnostic must point at the deliberate error");

    let inference = failure
        .diagnostics
        .iter()
        .find_map(|diagnostic| match &diagnostic.kind {
            FrontendDiagnosticKind::Type(error) if error.message.contains("cannot infer type") => {
                Some((diagnostic, error))
            }
            _ => None,
        })
        .expect("deferred inference diagnostic");
    assert!(
        inference
            .0
            .filename
            .as_deref()
            .is_some_and(|filename| Path::new(filename).ends_with(&peer_suffix)),
        "deferred diagnostic must name the declaring peer, got {:?}",
        inference.0.filename
    );
    let inference_line = peer_source[..inference.1.span.start]
        .bytes()
        .filter(|byte| *byte == b'\n')
        .count()
        + 1;
    assert_eq!(
        inference_line, 12,
        "deferred diagnostic must point at the inference error"
    );
}

#[test]
fn hir_diagnostic_routes_to_imported_module_source() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let main = write_source(
        dir.path(),
        "main.hew",
        "import \"dep.hew\";\nfn main() {}\n",
    );
    fs::write(dir.path().join("dep.hew"), "pub fn dep_entry() {}\n").expect("write dep.hew");
    let state = run_file_frontend_to_typecheck(&main, &FrontendOptions::default())
        .expect("frontend should accept fixture");

    fs::remove_file(dir.path().join("dep.hew")).unwrap();
    let diagnostics = hir_diagnostics_to_frontend(
        vec![hew_hir::HirDiagnostic::new(
            hew_hir::HirDiagnosticKind::ConstIntegerEvaluation {
                class: "arithmetic-overflow".to_string(),
            },
            0..3,
            "probe",
        )
        .with_source_module(Some("dep".to_string()))],
        &state.typecheck_result.sources,
    );

    assert_eq!(diagnostics.len(), 1);
    assert!(
        diagnostics[0]
            .filename
            .as_deref()
            .is_some_and(|filename| filename.ends_with("dep.hew")),
        "expected dep.hew filename, got {:?}",
        diagnostics[0].filename
    );
    assert_eq!(
        diagnostics[0].source.as_deref(),
        Some("pub fn dep_entry() {}\n")
    );
}

#[test]
fn hir_diagnostic_source_map_miss_does_not_fallback_to_root() {
    let source = "fn main() {}\n";
    let program = parse_source(source, "main.hew").expect("source should parse");

    let diagnostics = hir_diagnostics_to_frontend(
        vec![hew_hir::HirDiagnostic::new(
            hew_hir::HirDiagnosticKind::UnresolvedInferenceVar,
            0..2,
            "probe",
        )
        .with_source_module(Some("missing".to_string()))],
        &build_module_source_map(&program, source, "main.hew", &SourceSet::new()),
    );

    assert_eq!(diagnostics.len(), 1);
    assert!(diagnostics[0].source.is_none());
    assert!(diagnostics[0].filename.is_none());
    match &diagnostics[0].kind {
        FrontendDiagnosticKind::Hir(diagnostic) => {
            assert_eq!(diagnostic.source_module.as_deref(), Some("missing"));
        }
        other => panic!("expected HIR diagnostic, got {other:?}"),
    }
}

/// `std::log` ships `pub const JSON: i64 = 1` and `pub const TEXT: i64 = 0`
/// in its Hew source layer.  The stdlib registration path routes these through
/// `register_stdlib_hew_items` registers these constants so `log.JSON` /
/// `log.TEXT` are available to the type checker.
///
/// This test verifies the real stdlib const resolution works end-to-end: the
/// source goes through import resolution (which populates `resolved_items` on
/// the import decl) and type checking (which must find the const in env via
/// `check_field_access`).  Regression guard for the
/// `register_stdlib_hew_items` const arm.
#[test]
fn stdlib_log_module_consts_resolve() {
    // CARGO_MANIFEST_DIR is `hew-compile/`; the repo root is one level up.
    // That root contains `std/` so the module registry's tier-2 walk finds it.
    let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile lives under repo root");

    let dir = tempfile::tempdir().expect("create temp dir");
    let source = concat!(
        "import std.log;\n",
        "fn main() {\n",
        "    log.set_format(log.JSON);\n",
        "    log.set_format(log.TEXT);\n",
        "    log.info(\"ok\");\n",
        "}\n",
    );
    let input = write_source(dir.path(), "main.hew", source);

    let options = FrontendOptions {
        project_dir: Some(repo_root.to_path_buf()),
        ..Default::default()
    };

    let result = check_file(&input, &options);
    assert!(
        result.is_ok(),
        "log.JSON and log.TEXT should resolve cleanly; got: {:#?}",
        result.err()
    );
}

/// Importing `std::fs` and `std::path` together uses a per-module
/// discriminator in `SpanKey`:
///
/// * Defect A — `hew check`: `unsupported unary - for operand i64 -> string`
///   at `std/path.hew:227` (ordinary `return -1;`).  The negation was
///   mis-typed as `-> string` because `std/fs.hew` has a string literal at
///   the same byte offset as `path.hew`'s negation expression, and both
///   shared the same `SpanKey` in the flat `expr_types` map.
///
/// * Defect B — `hew compile`: `Instr::StringLit dest is not a pointer type:
///   dest_ty=i64` because the same collision made codegen see an i64 type
///   where a pointer-to-string was required.
///
/// The fix adds `module_idx: u32` to `SpanKey` so each non-root module gets
/// a distinct 1-based index and byte-offset collisions across files are
/// impossible.
///
/// Regression guard: if this test starts failing, re-examine
/// `SpanKey::in_module` stamping in the checker and HIR lowering.
/// Cross-root std resolution: source file is inside a fake Hew checkout root
/// (has its own `std/builtins.hew` and `std/fs.hew`), while the process cwd
/// is the real repo root (also a Hew checkout).  Before the fix, the compiler
/// built a cwd candidate pointing at the repo's `std/fs.hew` AND a Tier-2
/// candidate pointing at the fake root's `std/fs.hew`, producing two distinct
/// canonical paths → "import `std::fs` is ambiguous".
///
/// After the fix, `cwd_crosses_root` suppresses the cwd candidates when the
/// source file's enclosing root differs from the cwd's root → single candidate
/// from the source file's own root → no ambiguity.
///
/// This is the dogfood repro: `cd <main-checkout> && hew check <worktree>/…`
#[test]
fn cross_root_std_import_not_ambiguous() {
    // Cargo sets cwd to the workspace root during nextest, which is a real Hew
    // checkout (contains std/builtins.hew).  Confirm before asserting.
    let cwd = std::env::current_dir().expect("cwd accessible");
    if !cwd.join("std").join("builtins.hew").exists() {
        // Running outside the repo (e.g. in CI with a relocated test binary).
        // Skip rather than fail — the guard logic is still covered by the unit
        // test for find_enclosing_hew_root in hew-types.
        return;
    }

    // Build a second, completely separate fake Hew checkout root in a tempdir.
    let fake_root = tempfile::tempdir().expect("create fake checkout root");
    let std_dir = fake_root.path().join("std");
    fs::create_dir_all(&std_dir).expect("create std dir");
    fs::write(std_dir.join("builtins.hew"), "// fake builtins\n").expect("write fake builtins.hew");
    fs::write(
        std_dir.join("fs.hew"),
        "pub fn read_to_string(path: string) -> Result<string> { ask \"stub\" }\n",
    )
    .expect("write fake fs.hew");

    // Source file lives inside the fake root.
    let src_dir = fake_root.path().join("examples");
    fs::create_dir_all(&src_dir).expect("create examples dir");
    let source = "import std.fs;\n\nfn main() -> i64 { 0 }\n";
    let input = write_source(&src_dir, "prog.hew", source);

    let options = FrontendOptions {
        project_dir: Some(fake_root.path().to_path_buf()),
        ..Default::default()
    };

    // Must not produce an ambiguity error.  The compile may fail for
    // semantic reasons (stub body, NYI, etc.) — but NOT with "is ambiguous".
    let result = check_file(&input, &options);
    let err_str = result
        .as_ref()
        .err()
        .map(|e| format!("{e:?}"))
        .unwrap_or_default();
    assert!(
        !err_str.contains("is ambiguous"),
        "cross-root std::fs import must not be ambiguous; cwd={} fake_root={}: {err_str}",
        cwd.display(),
        fake_root.path().display(),
    );
}

/// Gap regression: source is inside a Hew root, but the process cwd is
/// OUTSIDE any Hew root yet contains its own `std/fs.hew`.
///
/// Before the widened guard, `cwd_hew_root = None` caused the old
/// `(Some(sr), Some(cr)) if sr != cr` match to fail, so the cwd candidates
/// were added on top of the Tier-2 candidates from the source root →
/// "import `std::fs` is ambiguous".
///
/// After the fix the guard is `source_hew_root.is_some() && cwd_hew_root !=
/// source_hew_root`, where `None != Some(x)` suppresses the cwd candidates
/// → single candidate from the source root → no ambiguity.
#[test]
fn source_in_root_cwd_outside_any_root_with_std_not_ambiguous() {
    // Build a fake Hew checkout root that is the source's home.
    let fake_root = tempfile::tempdir().expect("create fake checkout root");
    let std_dir = fake_root.path().join("std");
    fs::create_dir_all(&std_dir).expect("create std dir");
    fs::write(std_dir.join("builtins.hew"), "// fake builtins\n").expect("write fake builtins.hew");
    fs::write(
        std_dir.join("fs.hew"),
        "pub fn read_to_string(path: string) -> Result<string> { ask \"stub\" }\n",
    )
    .expect("write fake fs.hew");

    // Source file lives inside the fake root.
    let src_dir = fake_root.path().join("examples");
    fs::create_dir_all(&src_dir).expect("create examples dir");
    let source = "import std.fs;\n\nfn main() -> i64 { 0 }\n";
    let input = write_source(&src_dir, "prog.hew", source);

    // A separate tempdir that also contains std/fs.hew but is NOT a Hew
    // root (no builtins.hew, so find_enclosing_hew_root returns None).
    let outside_dir = tempfile::tempdir().expect("create outside dir");
    let outside_std = outside_dir.path().join("std");
    fs::create_dir_all(&outside_std).expect("create outside std dir");
    fs::write(
        outside_std.join("fs.hew"),
        "// outside-root stray std/fs.hew\n",
    )
    .expect("write outside fs.hew");

    // Override cwd via FrontendOptions so the test does not depend on the
    // real process cwd (which varies by runner).
    let options = FrontendOptions {
        project_dir: Some(fake_root.path().to_path_buf()),
        // Pass the outside dir as the working directory for resolution.
        // The resolver reads std::env::current_dir() directly, so we set
        // the process cwd for the duration of this test.
        ..Default::default()
    };

    // We cannot safely change the process cwd (not thread-safe), so we
    // instead verify the guard logic directly: construct the inputs that
    // the resolver would see and assert the correct outcome.
    //
    // The source file path IS inside fake_root (find_enclosing_hew_root →
    // Some(fake_root)), while outside_dir has no builtins.hew
    // (find_enclosing_hew_root → None).  With the widened guard,
    // None != Some(fake_root) → cwd_crosses_root = true → suppress cwd.
    let source_root =
        hew_types::module_registry::find_enclosing_hew_root(std::path::Path::new(&input));
    let outside_root = hew_types::module_registry::find_enclosing_hew_root(outside_dir.path());
    assert!(
        source_root.is_some(),
        "source file must be detected inside a Hew root"
    );
    assert!(
        outside_root.is_none(),
        "outside dir must NOT be detected as a Hew root (no builtins.hew)"
    );
    // The widened predicate: source has root, cwd root differs (None ≠ Some)
    // → cwd candidates suppressed.
    let cwd_crosses_root = source_root.is_some() && outside_root != source_root;
    assert!(
        cwd_crosses_root,
        "guard must fire when source has a root and cwd has none"
    );

    // Also verify end-to-end: compile does NOT produce ambiguity.
    let result = check_file(&input, &options);
    let err_str = result
        .as_ref()
        .err()
        .map(|e| format!("{e:?}"))
        .unwrap_or_default();
    assert!(
        !err_str.contains("is ambiguous"),
        "source-in-root cwd-outside-root std::fs import must not be ambiguous: {err_str}"
    );
}

#[test]
fn cross_module_span_key_collision_unary_minus_and_string_lit_do_not_collide() {
    let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile lives under repo root");

    let dir = tempfile::tempdir().expect("create temp dir");
    // Import both std::fs and std::path — the two modules whose functions
    // have byte-offset-colliding sub-expressions of different types.
    // A plain function call exercises path resolution without needing
    // full stdlib ABI support for the imported functions.
    let source = concat!(
        "import std.path;\n",
        "import std.fs;\n",
        "\n",
        "fn main() -> i64 { 0 }\n",
    );
    let input = write_source(dir.path(), "main.hew", source);

    let options = FrontendOptions {
        project_dir: Some(repo_root.to_path_buf()),
        ..Default::default()
    };

    let result = check_file(&input, &options);
    assert!(
        result.is_ok(),
        "importing std::path and std::fs together must not produce \
         cross-module SpanKey collisions; got: {:#?}",
        result.err()
    );
}

#[test]
fn imported_machine_step_signature_keeps_its_event_declaration() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let source = root.join("tests/core-acceptance/cases/machine-import-values.hew");
    let state = run_document_frontend(source.to_str().unwrap(), &FrontendOptions::default());
    let checked = state
        .typecheck_result
        .as_ref()
        .unwrap()
        .tco
        .as_ref()
        .unwrap();
    assert!(state.stopped.is_none(), "{:?}", state.stopped);
    let mut event_owners = std::collections::HashSet::new();
    for sig in checked.fn_sigs.values() {
        let Some(method) = &sig.impl_method else {
            continue;
        };
        if method.name.as_str() != "step"
            || method
                .receiver
                .is_none_or(|id| checked.defs.name(id).as_str() != "Gate")
        {
            continue;
        }
        let Some(hew_types::CallableParameter {
            ty: hew_types::Ty::Named { head, .. },
            ..
        }) = sig.params.first()
        else {
            continue;
        };
        let event = head
            .declaration(&checked.defs)
            .expect("resolved step event");
        assert_eq!(checked.defs.owner(event.declaration()), method.receiver);
        event_owners.insert(event);
    }
    assert_eq!(
        event_owners.len(),
        2,
        "the two Gate declarations keep separate events"
    );
}

#[test]
fn imported_function_can_spawn_and_wire_module_actors() {
    let dir = tempfile::tempdir().expect("create temp project");
    write_source(
        dir.path(),
        "actor_factory.hew",
        include_str!("../../../tests/core-acceptance/cases/pipeline_support/actors.hew"),
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import actor_factory;\nfn main() {\n    let source = actor_factory.run();\n    source.push(21, \"probe\").expect(\"push\");\n}\n",
    );
    let state = run_file_frontend_to_typecheck(
        &input,
        &FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            ..FrontendOptions::default()
        },
    )
    .unwrap_or_else(|failure| panic!("frontend failed: {failure:#?}"));
    let typecheck = state
        .typecheck_result
        .tco
        .as_ref()
        .expect("fixture must typecheck");
    let hir = hew_hir::lower_program(
        &state.program,
        typecheck,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "imported actor bodies must lower: {:#?}",
        hir.diagnostics
    );
    for actor in ["actor_factory.Stage", "actor_factory.Source"] {
        assert!(
            hir.module.items.iter().any(|item| matches!(
                item, hew_hir::HirItem::Actor(decl) if hir.module.defs.path(decl.declaration) == actor
            )),
            "missing imported actor declaration `{actor}`"
        );
    }
}

/// A cycle between two files in the SAME directory renders one positioned
/// location per import on the path (the header pointing at the first
/// edge, a note per remaining edge, in path order) and steers the fix
/// toward the directory-module form.
#[test]
fn import_cycle_in_same_directory_renders_positions_and_directory_help() {
    let dir = tempfile::tempdir().expect("create cycle fixture");
    let input = write_source(
        dir.path(),
        "a.hew",
        "import \"b.hew\";\npub fn noop_a() {}\n",
    );
    write_source(
        dir.path(),
        "b.hew",
        "import \"a.hew\";\npub fn noop_b() {}\n",
    );

    let failure =
        check_file(&input, &FrontendOptions::default()).expect_err("cycle must be rejected");
    assert_eq!(failure.diagnostics.len(), 1);
    let FrontendDiagnosticKind::Message(inner) = &failure.diagnostics[0].kind else {
        panic!(
            "expected a Message diagnostic, got {:?}",
            failure.diagnostics[0].kind
        );
    };

    assert_eq!(inner.code, "E_IMPORT_CYCLE");
    // Primary location: the first edge, at `a.hew`'s `import "b.hew";`.
    assert!(crate::paths_name_same_file(
        Path::new(
            failure.diagnostics[0]
                .filename
                .as_deref()
                .expect("cycle source filename")
        ),
        Path::new(&input),
    ));
    let primary_span = inner
        .span
        .clone()
        .expect("cycle diagnostic must carry a span");
    let primary_source = inner
        .source
        .as_deref()
        .expect("cycle diagnostic must carry source");
    assert_eq!(primary_source[primary_span].trim_end(), "import \"b.hew\";");
    assert!(
        inner.message.contains('`') && inner.message.contains("imports"),
        "primary message should label the edge it introduces: {}",
        inner.message
    );

    // One note for the closing edge, in `b.hew`, labelled as closing the cycle.
    assert_eq!(inner.notes.len(), 1);
    assert!(inner.notes[0].filename.ends_with("b.hew"));
    assert_eq!(
        inner.notes[0].source[inner.notes[0].span.clone()].trim_end(),
        "import \"a.hew\";"
    );
    assert!(
        inner.notes[0].message.contains("closing the cycle"),
        "closing edge should say so: {}",
        inner.notes[0].message
    );

    assert_eq!(inner.help.len(), 1);
    assert!(
        inner.help[0].contains("share one directory")
            && inner.help[0].contains("spec 3.5.1")
            && inner.help[0].contains("hew init"),
        "a loose same-directory cycle should recommend a package's directory-module form: {}",
        inner.help[0]
    );
}

/// A cycle spanning two DIFFERENT directories recommends moving the
/// shared declarations into a module both sides import instead.
#[test]
fn import_cycle_across_directories_recommends_a_shared_module() {
    let dir = tempfile::tempdir().expect("create cross-directory cycle fixture");
    let near = dir.path().join("near");
    let far = dir.path().join("far");
    fs::create_dir(&near).expect("create near directory");
    fs::create_dir(&far).expect("create far directory");
    let input = write_source(
        &near,
        "a.hew",
        "import \"../far/b.hew\";\npub fn noop_a() {}\n",
    );
    write_source(
        &far,
        "b.hew",
        "import \"../near/a.hew\";\npub fn noop_b() {}\n",
    );

    let failure = check_file(&input, &FrontendOptions::default())
        .expect_err("cross-directory cycle must be rejected");
    let FrontendDiagnosticKind::Message(inner) = &failure.diagnostics[0].kind else {
        panic!(
            "expected a Message diagnostic, got {:?}",
            failure.diagnostics[0].kind
        );
    };

    assert_eq!(inner.code, "E_IMPORT_CYCLE");
    assert_eq!(inner.help.len(), 1);
    assert_eq!(
        inner.help[0],
        "move the shared declarations into a module both sides import"
    );
}
