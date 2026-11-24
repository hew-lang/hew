use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

use hew_parser::ast::Item;
use hew_parser::module::ModulePath;

use super::{write_package_manifest, write_source, write_toml};
use crate::diagnostics::retain_user_facing_diagnostics;
use crate::manifest::load_manifest_metadata;
use crate::module_loader::undeclared_imports;
use crate::{
    build_module_graph, check_file, module_membership, parse_source,
    run_file_frontend_to_typecheck, DiagnosticPolicy, FrontendDiagnostic, FrontendDiagnosticKind,
    FrontendOptions, ImportResolutionContext, Session, SessionTarget, SourceSet,
};

#[test]
fn user_surface_removes_imported_stdlib_diagnostics() {
    let dir = tempfile::tempdir().expect("create diagnostic boundary fixture");
    let root = write_source(dir.path(), "main.hew", "fn main() {}\n");
    let std_dir = dir.path().join("toolchain").join("std");
    fs::create_dir_all(&std_dir).expect("create synthetic stdlib directory");
    let std_source = write_source(&std_dir, "arena.hew", "// synthetic stdlib\n");

    let mut user = FrontendDiagnostic::message("user warning");
    user.filename = Some(root.clone());
    let mut stdlib = FrontendDiagnostic::message("stdlib warning");
    stdlib.filename = Some(std_source);
    let mut diagnostics = vec![user, stdlib];

    retain_user_facing_diagnostics(&root, &[std_dir], &mut diagnostics);

    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].filename.as_deref(), Some(root.as_str()));
}

#[test]
fn memory_floor_calls_are_rejected_at_the_source_boundary() {
    let dir = tempfile::tempdir().unwrap();
    let input = write_source(
        dir.path(),
        "main.hew",
        "import std.mem; fn main() { let pointer = mem.alloc(8, 8); mem.dealloc(pointer, 8, 8); }",
    );
    let failure = check_file(&input, &FrontendOptions::default())
        .expect_err("raw memory primitives are not a source-language API");
    assert!(failure.diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        FrontendDiagnosticKind::Type(error)
            if matches!(&error.kind, hew_types::error::TypeErrorKind::IntrinsicOutsideFloor { intrinsic_key, .. }
                if intrinsic_key == "mem.alloc")
    )), "{:?}", failure.diagnostics);
}

#[test]
fn direct_memory_floor_intrinsics_remain_declarations() {
    let input = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("std/mem/mem.hew");
    let state =
        run_file_frontend_to_typecheck(input.to_str().unwrap(), &FrontendOptions::default())
            .expect("memory floor signatures typecheck");
    let output = state.typecheck_result.tco.unwrap();
    Session::new(SessionTarget::native(), DiagnosticPolicy::default())
        .lower_program(&state.program, &output)
        .expect("memory floor declarations do not manufacture executable bodies");
}

#[test]
fn direct_stdlib_check_retains_source_diagnostics() {
    let dir = tempfile::tempdir().expect("create direct stdlib fixture");
    let std_dir = dir.path().join("std");
    fs::create_dir(&std_dir).expect("create stdlib directory");
    let std_source = write_source(&std_dir, "arena.hew", "// synthetic stdlib\n");
    let mut diagnostic = FrontendDiagnostic::message("stdlib warning");
    diagnostic.filename = Some(std_source.clone());
    let mut diagnostics = vec![diagnostic];

    retain_user_facing_diagnostics(&std_source, &[std_dir], &mut diagnostics);

    assert_eq!(diagnostics.len(), 1);
    assert_eq!(
        diagnostics[0].filename.as_deref(),
        Some(std_source.as_str())
    );
}

#[test]
fn user_std_directory_is_not_treated_as_the_configured_stdlib() {
    let dir = tempfile::tempdir().expect("create diagnostic boundary fixture");
    let root = write_source(dir.path(), "main.hew", "fn main() {}\n");
    let configured_std = dir.path().join("toolchain").join("std");
    fs::create_dir_all(&configured_std).expect("create configured stdlib directory");
    let user_std = dir.path().join("project").join("std");
    fs::create_dir_all(&user_std).expect("create user std directory");
    let user_source = write_source(&user_std, "helpers.hew", "// user source\n");
    let mut diagnostic = FrontendDiagnostic::message("user warning");
    diagnostic.filename = Some(user_source.clone());
    let mut diagnostics = vec![diagnostic];

    retain_user_facing_diagnostics(&root, &[configured_std], &mut diagnostics);

    assert_eq!(diagnostics.len(), 1);
    assert_eq!(
        diagnostics[0].filename.as_deref(),
        Some(user_source.as_str())
    );
}

#[test]
fn checking_directory_module_peer_loads_entry_namespace() {
    let dir = tempfile::tempdir().expect("create directory-module fixture");
    write_package_manifest(dir.path());
    let module_dir = dir.path().join("greeting");
    fs::create_dir(&module_dir).expect("create module directory");
    write_source(
        &module_dir,
        "greeting.hew",
        "pub trait Greeter {\n    fn name(self) -> string;\n    fn greet(self) -> string { self.name() }\n}\n",
    );
    let peer = write_source(
        &module_dir,
        "dog.hew",
        "pub type Dog {\n    label: string;\n}\n\nimpl Greeter for Dog {\n    fn name(self) -> string {\n        self.label\n    }\n}\n\npub fn describe(d: Dog) -> string {\n    d.greet()\n}\n",
    );

    let result = check_file(
        &peer,
        &FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            ..FrontendOptions::default()
        },
    );
    assert!(
        result.is_ok(),
        "a directly checked peer must share its directory module entry: {:#?}",
        result.err()
    );
}

/// A package with a `forge` directory module whose entry uses a peer's
/// function and whose peer uses the entry's types, plus a package-local
/// `util` module the peer imports.
fn forge_package(peer_extra: &str) -> (tempfile::TempDir, String, String) {
    let dir = tempfile::tempdir().expect("create package fixture");
    fs::write(
        dir.path().join("hew.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\nedition = \"2026\"\n",
    )
    .expect("write manifest");
    let forge = dir.path().join("forge");
    fs::create_dir_all(&forge).expect("create module directory");
    write_source(
        dir.path(),
        "util.hew",
        "pub fn twice(x: i64) -> i64 {\n    x * 2\n}\n",
    );
    let entry = write_source(
        &forge,
        "forge.hew",
        "pub type ForgeConfig {\n    timeout: i64;\n}\n\npub fn describe(config: ForgeConfig) -> string {\n    ado_name(config)\n}\n",
    );
    let peer = write_source(
        &forge,
        "ado.hew",
        &format!(
            "import app.util;\n\npub fn ado_name(config: ForgeConfig) -> string {{\n    f\"ado:{{util.twice(config.timeout)}}\"\n}}\n{peer_extra}"
        ),
    );
    write_source(&forge, "ado_test.hew", "not a peer, never parsed(\n");
    (dir, entry, peer)
}

#[test]
fn module_membership_assigns_entries_peers_and_tests() {
    use hew_types::module_registry::MembershipRole;
    let (dir, entry, peer) = forge_package("");
    let canonical_entry = Path::new(&entry).canonicalize().expect("entry exists");
    let membership =
        |path: &Path| module_membership(path, &FrontendOptions::default()).expect("valid manifest");
    let of_entry = membership(Path::new(&entry));
    assert_eq!(of_entry.role, MembershipRole::Entry);
    assert_eq!(of_entry.module, ModulePath::new(["app", "forge"]));
    let of_peer = membership(Path::new(&peer));
    assert_eq!(of_peer.role, MembershipRole::Peer);
    assert_eq!(of_peer.entry, canonical_entry);
    let forge = dir.path().join("forge");
    let of_test = membership(&forge.join("ado_test.hew"));
    assert_eq!(of_test.role, MembershipRole::Test);
    assert_eq!(of_test.entry, canonical_entry);
    let of_util = membership(&dir.path().join("util.hew"));
    assert_eq!(of_util.role, MembershipRole::Single);
    assert_eq!(of_util.module, ModulePath::new(["app", "util"]));
}

#[test]
fn virtual_workspaces_preserve_loose_file_membership() {
    let dir = tempfile::tempdir().expect("create loose-file fixture");
    let model = dir.path().join("model");
    fs::create_dir(&model).expect("create same-stem directory");
    let entry = write_source(&model, "model.hew", "pub fn answer() -> i64 { 42 }\n");
    let peer = write_source(&model, "peer.hew", "fn broken() -> i64 { \"wrong\" }\n");
    for manifest in [None, Some("[workspace]\nmembers = []\n")] {
        if let Some(manifest) = manifest {
            write_toml(dir.path(), manifest);
        }
        check_file(&entry, &FrontendOptions::default())
            .expect("loose entry must not include a sibling's type error");
        let failure = check_file(&peer, &FrontendOptions::default())
            .expect_err("checking the loose sibling must report its own type error");
        assert!(failure.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            FrontendDiagnosticKind::Type(error)
                if matches!(error.kind, hew_types::error::TypeErrorKind::Mismatch { .. })
        )));
    }

    write_package_manifest(dir.path());
    let failure = check_file(&entry, &FrontendOptions::default())
        .expect_err("an actual package's entry must include its peer's type error");
    assert!(failure.diagnostics.iter().any(|diagnostic| {
        diagnostic
            .filename
            .as_deref()
            .is_some_and(|file| Path::new(file).ends_with(Path::new("model").join("peer.hew")))
            && matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Type(error)
                    if matches!(error.kind, hew_types::error::TypeErrorKind::Mismatch { .. })
            )
    }));
}

#[test]
fn checking_a_directory_module_file_checks_the_whole_module() {
    let (_dir, entry, peer) = forge_package("");
    for input in [&entry, &peer] {
        let result = check_file(input, &FrontendOptions::default());
        assert!(
            result.is_ok(),
            "{input} must check with its entry, peers and package imports: {:#?}",
            result.err().map(|failure| failure.diagnostics)
        );
    }
}

#[test]
fn checking_a_directory_module_entry_reports_errors_in_its_peers() {
    let (_dir, entry, peer) = forge_package("\nfn broken() -> i64 {\n    \"no\"\n}\n");
    for input in [&entry, &peer] {
        let failure = check_file(input, &FrontendOptions::default())
            .expect_err("the peer's type error must fail the module check");
        let mismatches = failure
            .diagnostics
            .iter()
            .filter(|diagnostic| {
                matches!(&diagnostic.kind, FrontendDiagnosticKind::Type(error)
                    if matches!(error.kind, hew_types::error::TypeErrorKind::Mismatch { .. }))
            })
            .collect::<Vec<_>>();
        assert_eq!(mismatches.len(), 1, "{:#?}", failure.diagnostics);
        let file = mismatches[0].filename.as_deref().expect("routed to a file");
        assert!(
            Path::new(file).ends_with("forge/ado.hew"),
            "the error belongs to the peer, not {file}"
        );
    }
}

/// The std directory module a peer import loads, as source paths below
/// `root`, from a run that resolves the std at `root` through `documents`.
fn std_http_sources(root: &Path, documents: &SourceSet) -> Vec<std::path::PathBuf> {
    let dir = tempfile::tempdir().expect("create importer fixture");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import std.net.http.codec.request;\n\nfn main() {}\n",
    );
    let source = fs::read_to_string(&input).expect("read importer fixture");
    let mut program = parse_source(&source, &input).expect("parse importer fixture");
    let roots = [root.to_path_buf()];
    let mut ctx = ImportResolutionContext {
        in_progress_imports: HashSet::new(),
        resolved_imports: HashMap::new(),
        manifest_deps: None,
        extra_pkg_path: None,
        locked_versions: None,
        package_name: None,
        project_dir: dir.path(),
        std_roots: &roots,
        documents,
    };
    let graph = build_module_graph(
        Path::new(&input),
        &mut program.items,
        program.module_doc.clone(),
        &mut ctx,
    )
    .expect("the std peer import should build a module graph");
    let client = root.join("std/net/http/codec/request.hew");
    let owners = graph
        .modules
        .values()
        .filter(|module| {
            module
                .source_paths
                .iter()
                .any(|path| path.ends_with("std/net/http/codec/request.hew"))
        })
        .collect::<Vec<_>>();
    assert_eq!(owners.len(), 1, "one graph node holds {}", client.display());
    let root = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
    owners[0]
        .source_paths
        .iter()
        .map(|path| {
            path.strip_prefix(&root)
                .expect("a std source is below its root")
                .to_path_buf()
        })
        .collect()
}

/// A host with no filesystem (the browser) lists a std directory module's
/// peers from the sources it was given, so its module graph is the native
/// one: the same entry and peers in the same order.
#[test]
fn std_directory_module_peers_come_from_documents_without_a_filesystem() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile lives below the repository root")
        .to_path_buf();
    let native = std_http_sources(&repo_root, &SourceSet::new());

    let virtual_root = std::env::temp_dir().join("hew-std-without-a-filesystem");
    assert!(
        !virtual_root.exists(),
        "the virtual std root must not exist on disk"
    );
    let mut documents = SourceSet::new();
    let mut pending = vec![repo_root.join("std")];
    while let Some(dir) = pending.pop() {
        for entry in fs::read_dir(&dir).expect("read the std tree").flatten() {
            let path = entry.path();
            if path.is_dir() {
                pending.push(path);
            } else if path.extension().is_some_and(|extension| extension == "hew") {
                let relative = path.strip_prefix(&repo_root).expect("below the repository");
                documents.insert(
                    virtual_root.join(relative),
                    fs::read_to_string(&path).expect("read a std source"),
                );
            }
        }
    }
    let in_memory = std_http_sources(&virtual_root, &documents);

    assert_eq!(
        native.len(),
        2,
        "std.net.http.codec is its entry and request peer: {native:?}"
    );
    assert_eq!(native[0], Path::new("std/net/http/codec/codec.hew"));
    assert_eq!(in_memory, native);
}

#[test]
fn embedded_directory_modules_resolve_peers_from_relative_roots() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("repository root");
    let root = Path::new("./hew-embedded-stdlib");
    assert!(!root.exists(), "the source root must remain virtual");
    let mut documents = SourceSet::in_memory();
    let mut pending = vec![repo.join("std")];
    while let Some(dir) = pending.pop() {
        for entry in fs::read_dir(dir)
            .expect("standard-library sources")
            .flatten()
        {
            let path = entry.path();
            if path.is_dir() {
                pending.push(path);
            } else if path.extension().is_some_and(|extension| extension == "hew") {
                documents.insert(
                    root.join(path.strip_prefix(repo).expect("repository source")),
                    fs::read_to_string(path).expect("source text"),
                );
            }
        }
    }
    for import in ["std.net.http.codec", "std.net.http.codec.request"] {
        let source = format!("import {import}; fn main() {{}}");
        let mut program = parse_source(&source, "playground.hew").expect("import source");
        let roots = [root.to_path_buf()];
        let mut context = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: Path::new("."),
            std_roots: &roots,
            documents: &documents,
        };
        let graph = build_module_graph(
            Path::new("playground.hew"),
            &mut program.items,
            program.module_doc.clone(),
            &mut context,
        )
        .expect("embedded directory module graph");
        let owners = graph
            .modules
            .values()
            .filter(|module| {
                module
                    .source_paths
                    .iter()
                    .any(|path| path.ends_with("std/net/http/codec/codec.hew"))
            })
            .collect::<Vec<_>>();
        assert_eq!(owners.len(), 1, "one owner for the codec entry");
        assert!(
            owners[0]
                .source_paths
                .iter()
                .any(|path| { path.ends_with("std/net/http/codec/request.hew") }),
            "the entry and private peer helpers must share one module: {:?}",
            owners[0].source_paths
        );
    }
}

fn manifest_errors(manifest: &str, files: &[(&str, &str)], main: &str) -> Vec<String> {
    let dir = tempfile::tempdir().expect("create package fixture");
    fs::write(dir.path().join("hew.toml"), manifest).expect("write manifest");
    for (path, source) in files {
        let path = dir.path().join(path);
        fs::create_dir_all(path.parent().expect("a fixture file has a parent"))
            .expect("create fixture directory");
        fs::write(path, source).expect("write fixture source");
    }
    let input = write_source(dir.path(), "main.hew", main);
    let program = parse_source(main, &input).expect("parse package fixture");
    let (manifest_deps, package_name) = load_manifest_metadata(dir.path()).expect("valid manifest");
    let documents = SourceSet::new();
    let ctx = ImportResolutionContext {
        in_progress_imports: HashSet::new(),
        resolved_imports: HashMap::new(),
        manifest_deps: manifest_deps.as_deref(),
        extra_pkg_path: None,
        locked_versions: None,
        package_name: package_name.as_deref(),
        project_dir: dir.path(),
        std_roots: &hew_types::module_registry::stdlib_search_paths(),
        documents: &documents,
    };
    let input = Path::new(&input).canonicalize().expect("main exists");
    undeclared_imports(&input, &program.items, &ctx).expect("valid manifests")
}

#[test]
fn an_undeclared_dependency_is_named_with_its_fix() {
    let errors = manifest_errors(
        "[package]\nname = \"app\"\n",
        &[],
        "import mylib.utils;\nimport mylib.other;\nfn main() {}\n",
    );
    assert_eq!(
        errors.len(),
        2,
        "every undeclared import is reported: {errors:?}"
    );
    assert!(errors[0].contains("`mylib.utils` is not declared in hew.toml"));
    assert!(errors[0].contains("hew add mylib.utils"));
}

#[test]
fn a_declared_dependency_covers_its_modules() {
    let errors = manifest_errors(
        "[package]\nname = \"app\"\n\n[dependencies]\n\"acme.http\" = \"1.0\"\n",
        &[],
        "import acme.http;\nimport acme.http.client;\nimport acme.other;\nfn main() {}\n",
    );
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].contains("`acme.other`"), "{errors:?}");
}

#[test]
fn std_file_and_package_local_imports_need_no_declaration() {
    let errors = manifest_errors(
        "[package]\nname = \"meshcore.broker\"\n",
        &[
            ("wire.hew", "pub fn w() -> i64 { 7 }\n"),
            ("helpers.hew", "pub fn h() -> i64 { 1 }\n"),
            (
                "greeting/greeting.hew",
                "pub fn hello() -> string { \"hi\" }\n",
            ),
            ("lib.hew", "pub fn l() -> i64 { 2 }\n"),
        ],
        "import std.fs;\nimport \"lib.hew\";\nimport meshcore.broker.wire;\nimport helpers;\nimport greeting;\nfn main() {}\n",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn a_dotted_package_imports_itself_but_not_a_shorter_prefix() {
    let errors = manifest_errors(
        "[package]\nname = \"meshcore.broker\"\n",
        &[("wire.hew", "pub fn w() -> i64 { 7 }\n")],
        "import meshcore.broker.wire;\nimport meshcore.wire;\nfn main() {}\n",
    );
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].contains("`meshcore.wire`"), "{errors:?}");
}

#[test]
fn stdlib_directory_peer_imports_share_one_complete_module_owner() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile lives below the repository root")
        .to_path_buf();

    for imports in [
        "import std.net.http.codec;\nimport std.net.http.codec.request;\n",
        "import std.net.http.codec.request;\nimport std.net.http.codec;\n",
        "import std.net.http.codec.request;\n",
    ] {
        let dir = tempfile::tempdir().expect("create module-owner fixture");
        let input = write_source(
            dir.path(),
            "main.hew",
            &format!("{imports}\nfn main() {{}}\n"),
        );
        let source = fs::read_to_string(&input).expect("read module-owner fixture");
        let mut program = parse_source(&source, &input).expect("parse module-owner fixture");
        let documents = SourceSet::new();
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: dir.path(),
            std_roots: std::slice::from_ref(&repo_root),
            documents: &documents,
        };

        let graph = build_module_graph(
            Path::new(&input),
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("stdlib peer imports should build a module graph");
        let http_id = hew_parser::module::ModulePath::new(["std", "net", "http", "codec"]);
        let peer_id =
            hew_parser::module::ModulePath::new(["std", "net", "http", "codec", "request"]);
        let http = graph
            .modules
            .get(&http_id)
            .expect("the canonical std.net.http.codec module should be present");
        assert!(
            !graph.modules.contains_key(&peer_id),
            "the peer must not become a second graph owner: {:?}",
            graph.modules.keys().collect::<Vec<_>>()
        );
        assert!(
            http.source_paths
                .iter()
                .any(|path| path.ends_with("std/net/http/codec/codec.hew")),
            "the canonical module must retain its entry source: {:?}",
            http.source_paths
        );
        assert!(
            http.source_paths
                .iter()
                .any(|path| path.ends_with("std/net/http/codec/request.hew")),
            "the canonical module must retain its peer source: {:?}",
            http.source_paths
        );
        assert!(
            http.items.iter().any(|(item, _)| matches!(
                item,
                Item::TypeDecl(decl) if decl.name.name.as_str() == "Response"
            )),
            "peer-only imports must load the complete package item set"
        );
    }
}

#[test]
fn imported_http_and_json_preserve_checked_return_identity() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile lives below the repository root");
    let dir = tempfile::tempdir().expect("create stdlib return fixture");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import std.net.http.codec;\nimport std.net.http.codec.request;\nimport std.encoding.json;\nfn main() {}\n",
    );
    let state = run_file_frontend_to_typecheck(
        &input,
        &FrontendOptions {
            module_search_paths: Some(vec![repo_root.to_path_buf()]),
            ..FrontendOptions::default()
        },
    )
    .expect("stdlib imports must type-check");
    let hir = hew_hir::lower_program(
        &state.program,
        state.typecheck_result.tco.as_ref().expect("checked output"),
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "imported return expressions must retain their checked identities: {:#?}",
        hir.diagnostics
    );
}

#[test]
fn typecheck_top_level_networking_demos() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("repository root");
    for relative in ["examples/http_server.hew", "examples/static_server.hew"] {
        check_file(
            &root.join(relative).display().to_string(),
            &FrontendOptions::default(),
        )
        .unwrap_or_else(|failure| panic!("{relative}: {failure:#?}"));
    }
}

#[test]
fn direct_stdlib_directory_sources_check_their_complete_owner() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("repository root");
    for relative in [
        "std/net/http/codec/codec.hew",
        "std/net/http/codec/request.hew",
    ] {
        check_file(
            &root.join(relative).display().to_string(),
            &FrontendOptions::default(),
        )
        .unwrap_or_else(|failure| panic!("{relative}: {failure:#?}"));
    }
}

#[test]
fn direct_stdlib_directory_check_reports_invalid_peer_buffer() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("repository root");
    let peer = root.join("std/net/http/codec/request.hew");
    let original = fs::read_to_string(&peer).expect("read peer");
    let mut options = FrontendOptions::default();
    options.documents.insert(
        peer.display().to_string(),
        format!("{original}\npub fn invalid_peer_value() -> bool {{ 1 }}\n"),
    );
    let failure = check_file(
        &root
            .join("std/net/http/codec/codec.hew")
            .display()
            .to_string(),
        &options,
    )
    .expect_err("checking the entry must check its peer's body");
    assert!(failure.diagnostics.iter().any(|diagnostic| {
        matches!(&diagnostic.kind, FrontendDiagnosticKind::Type(error) if matches!(error.kind, hew_types::error::TypeErrorKind::Mismatch { .. }))
            && diagnostic.filename.as_deref().is_some_and(|file| Path::new(file).ends_with("codec/request.hew"))
    }), "{failure:#?}");
}

#[test]
fn check_file_accepts_shipped_directory_peer_reimports() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile lives below the repository root")
        .to_path_buf();
    let dir = tempfile::tempdir().expect("create peer-reimport fixture");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import std.net.http.codec;\n\
         import std.net.http.codec.request;\n\n\
         fn main() {}\n",
    );

    check_file(
        &input,
        &FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            module_search_paths: Some(vec![repo_root]),
            ..FrontendOptions::default()
        },
    )
    .expect("reimporting a shipped directory peer must not duplicate declarations");
}

/// Reverses 10ec5abd6 (`fix(modules): limit peer promotion to shipped
/// stdlib`), which let a user package import one of its own directory
/// peers directly and kept it as an isolated module. That isolated
/// parse has no access to its sibling declarations — spec 3.5.1 merges
/// every peer into the directory module's one namespace — so any
/// reference to a sibling surfaced downstream as a bare "undefined
/// function"/"undefined variable" with no hint that the fix is to
/// import the directory module instead. Refusing the import outright,
/// naming the directory module to use, is the actionable diagnostic;
/// the isolated-parse path is no longer reachable.
#[test]
fn user_directory_peer_import_is_refused() {
    let dir = tempfile::tempdir().expect("create module-owner fixture");
    write_package_manifest(dir.path());
    let module_dir = dir.path().join("greeting");
    fs::create_dir(&module_dir).expect("create module directory");
    write_source(&module_dir, "greeting.hew", "pub fn entry() -> i64 { 1 }\n");
    write_source(&module_dir, "dog.hew", "pub fn bark() -> i64 { 2 }\n");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import greeting.dog;\n\nfn main() {}\n",
    );
    let source = fs::read_to_string(&input).expect("read module-owner fixture");
    let mut program = parse_source(&source, &input).expect("parse module-owner fixture");
    let documents = SourceSet::new();
    let mut ctx = ImportResolutionContext {
        in_progress_imports: HashSet::new(),
        resolved_imports: HashMap::new(),
        manifest_deps: None,
        extra_pkg_path: None,
        locked_versions: None,
        package_name: None,
        project_dir: dir.path(),
        std_roots: &hew_types::module_registry::stdlib_search_paths(),
        documents: &documents,
    };

    let failure = build_module_graph(
        Path::new(&input),
        &mut program.items,
        program.module_doc.clone(),
        &mut ctx,
    )
    .expect_err("importing a directory peer directly must be refused");

    let FrontendDiagnosticKind::Message(inner) = &failure.diagnostics[0].kind else {
        panic!(
            "expected a Message diagnostic, got {:?}",
            failure.diagnostics[0].kind
        );
    };
    assert_eq!(inner.code, "E_PEER_IMPORT");
    assert!(
        inner.message.contains("greeting"),
        "message should name the directory module to import instead: {}",
        inner.message
    );
}

/// Two peer files of one directory module that claim the same assembled
/// name are a duplicate definition. The module graph rejects a duplicate
/// PUB name before checking, but a private one reaches the checker — where
/// registration visits each file on its own and never sees the collision,
/// because only the ASSEMBLED path collides. The identity table is the
/// only place that can report it, and it must: otherwise the user gets the
/// internal "identity table has no declaration" wording from whichever
/// consumer asks for the refused declaration first.
#[test]
fn directory_module_peers_claiming_one_name_report_a_duplicate_definition() {
    let dir = tempfile::tempdir().expect("create directory-module fixture");
    write_package_manifest(dir.path());
    let module_dir = dir.path().join("shapes");
    fs::create_dir(&module_dir).expect("create module directory");
    write_source(
        &module_dir,
        "shapes.hew",
        "type Point {\n    x: i64;\n}\n\npub fn ax() -> i64 {\n    let p = Point { x: 1 };\n    p.x\n}\n",
    );
    write_source(
        &module_dir,
        "circle.hew",
        "type Point {\n    y: i64;\n}\n\npub fn by() -> i64 {\n    let p = Point { y: 2 };\n    p.y\n}\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import shapes;\n\nfn main() { println(shapes.ax() + shapes.by()); }\n",
    );

    let failure = check_file(
        &input,
        &FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            ..FrontendOptions::default()
        },
    )
    .expect_err("two peers claiming `shapes.Point` must not type-check");
    let duplicates: Vec<&FrontendDiagnostic> = failure
        .diagnostics
        .iter()
        .filter(|diagnostic| {
            matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Type(error)
                    if error.kind == hew_types::error::TypeErrorKind::DuplicateDefinition
            )
        })
        .collect();
    assert_eq!(
        duplicates.len(),
        1,
        "the collision must be reported exactly once: {:#?}",
        failure.diagnostics
    );
    let rendered = format!("{duplicates:#?}");
    assert!(
        rendered.contains("`Point` is defined multiple times")
            && rendered.contains("shapes.hew")
            && rendered.contains("circle.hew"),
        "the diagnostic must name both peer files: {rendered}"
    );
    assert!(
        !format!("{:#?}", failure.diagnostics).contains("identity table has no"),
        "the internal refusal wording must not reach the user: {:#?}",
        failure.diagnostics
    );
}

/// A file import is its own module: a private type it declares and a root
/// type of the same name are distinct declarations, not a collision.
#[test]
fn a_root_type_and_a_private_file_import_type_share_a_name() {
    let dir = tempfile::tempdir().expect("create file-import fixture");
    write_source(
        dir.path(),
        "lib.hew",
        "type Point {\n    x: i64;\n}\n\npub fn lib_point() -> i64 {\n    let p = Point { x: 1 };\n    p.x\n}\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import \"lib.hew\";\n\ntype Point {\n    y: i64;\n}\n\nfn main() {\n    let p = Point { y: 2 };\n    println(p.y + lib_point());\n}\n",
    );

    check_file(&input, &FrontendOptions::default())
        .expect("a private file-import type does not collide with a root type");
}

/// Negative control for the rule above: an `extern "C"` symbol declared by
/// two peer files of one directory module is a redeclaration, not a
/// redefinition — the linker binds every call to one implementation and
/// the extern table resolves the second against the established contract.
/// It must keep type-checking, and both declarations must resolve.
#[test]
fn directory_module_peers_may_redeclare_one_extern_symbol() {
    let dir = tempfile::tempdir().expect("create directory-module fixture");
    write_package_manifest(dir.path());
    let module_dir = dir.path().join("clock");
    fs::create_dir(&module_dir).expect("create module directory");
    write_source(
        &module_dir,
        "clock.hew",
        "extern \"C\" {\n    fn hew_time_now_millis() -> i64;\n}\n             pub fn now() -> i64 { unsafe { hew_time_now_millis() } }\n",
    );
    write_source(
        &module_dir,
        "stamp.hew",
        "extern \"C\" {\n    fn hew_time_now_millis() -> i64;\n}\n             pub fn stamp() -> i64 { unsafe { hew_time_now_millis() } }\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import clock;\n\nfn main() { println(clock.now() + clock.stamp()); }\n",
    );

    let result = check_file(
        &input,
        &FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            ..FrontendOptions::default()
        },
    );
    assert!(
        result.is_ok(),
        "one C symbol declared by two peers is a redeclaration: {:#?}",
        result.err()
    );
}
