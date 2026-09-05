//! Type-checker fixtures for actor lifecycle hooks
//! (`#[on(start)]` / `#[on(stop)]` / `#[on(crash)]` / `#[on(exit)]` /
//! `#[on(down)]`). `upgrade` is no longer a hook kind (HEW-SPEC-2026 §12.6);
//! `#[on(upgrade)]` is exercised here only as an unrecognised-kind case.
//!
//! These exercise the §9.1.2 surface defined in
//! `docs/specs/HEW-SPEC-2026.md`. The accept fixtures pin the
//! type-check-clean shape; the reject fixtures pin diagnostic
//! coverage (§3.3 both-path: every diagnostic has an accept twin and
//! a reject twin so the rule is observable from both sides).

use crate::common;

use common::typecheck_isolated as typecheck;
use hew_parser::ast::Item;
use hew_parser::module::{Module, ModuleGraph, ModuleId, ModuleImport};
use hew_types::error::TypeErrorKind;

/// The isolated lifecycle tests intentionally exercise the no-search-path
/// bootstrap.  These authority tests instead attach the shipped std sources to
/// their imports, matching the module-graph path that real programs use.
fn typecheck_with_resolved_std(source: &str) -> hew_types::TypeCheckOutput {
    fn attach_std_sources(items: &mut [hew_parser::ast::Spanned<Item>]) {
        for (item, _) in items {
            let Item::Import(decl) = item else {
                continue;
            };
            let source = match decl
                .path
                .iter()
                .map(String::as_str)
                .collect::<Vec<_>>()
                .as_slice()
            {
                ["std", "failure"] => Some((
                    include_str!("../../../std/failure.hew"),
                    common::repo_root().join("std/failure.hew"),
                )),
                ["std", "link_monitor"] => Some((
                    include_str!("../../../std/link_monitor.hew"),
                    common::repo_root().join("std/link_monitor.hew"),
                )),
                _ => None,
            };
            if let Some((source, source_path)) = source {
                let mut imported = common::parse_program(source).items;
                attach_std_sources(&mut imported);
                decl.resolved_items = Some(imported);
                decl.resolved_source_paths = vec![source_path];
            }
        }
    }

    let mut program = common::parse_program(source);
    attach_std_sources(&mut program.items);
    let mut checker = common::checker();
    checker.check_program(&program)
}

/// Attach a user-backed module that deliberately spells itself like a stdlib
/// lifecycle owner. Its resolved source path is not the shipped file, which is
/// the authority boundary under test.
fn typecheck_with_spoofed_std_import(owner: &str, source: &str) -> hew_types::TypeCheckOutput {
    let module_source = match owner {
        "failure" => "pub type CrashNotification { actor_id: u64, }\npub enum CrashKind { Crashed, }",
        "link_monitor" => {
            "pub type MonitorId { value: u64, }\n\
             pub enum DownTarget { Local(u64), }\n\
             pub enum DownReason { Exited, }\n\
             pub type DownNotification { monitor: MonitorId, target: DownTarget, reason: DownReason, }"
        }
        _ => panic!("unsupported lifecycle owner fixture: {owner}"),
    };
    let mut program = common::parse_program(source);
    for (item, _) in &mut program.items {
        let Item::Import(decl) = item else {
            continue;
        };
        if decl.path == ["std", owner] {
            decl.resolved_items = Some(common::parse_program(module_source).items);
            decl.resolved_source_paths =
                vec![common::repo_root().join(format!("tests/fixtures/spoofed-{owner}.hew"))];
        }
    }
    let mut checker = common::checker();
    checker.check_program(&program)
}

fn typecheck_module_body(module_path: &[&str], source: &str) -> hew_types::TypeCheckOutput {
    let root_id = ModuleId::root();
    let module_id = ModuleId::new(
        module_path
            .iter()
            .map(|segment| (*segment).to_string())
            .collect(),
    );
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: module_id.clone(),
            items: common::parse_program(source).items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("module fixture must be unique");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("root fixture must be unique");
    graph.topo_order = vec![module_id, root_id];
    let program = hew_parser::ast::Program {
        items: vec![],
        module_doc: None,
        module_graph: Some(graph),
    };
    let mut checker = common::isolated_checker();
    checker.check_program(&program)
}

fn typecheck_with_transitive_std(owner: &str, root_source: &str) -> hew_types::TypeCheckOutput {
    let std_source = match owner {
        "failure" => include_str!("../../../std/failure.hew"),
        "link_monitor" => include_str!("../../../std/link_monitor.hew"),
        _ => panic!("unsupported lifecycle owner fixture: {owner}"),
    };
    let mut helper = common::parse_program(&format!("import std.{owner}; pub fn loaded() {{}}"));
    for (item, _) in &mut helper.items {
        let Item::Import(decl) = item else {
            continue;
        };
        if decl.path.last().is_some_and(|segment| segment == owner) {
            let mut imported = common::parse_program(std_source).items;
            if owner == "link_monitor" {
                for (nested, _) in &mut imported {
                    let Item::Import(nested_decl) = nested else {
                        continue;
                    };
                    if nested_decl.path == ["std", "failure"] {
                        nested_decl.resolved_items = Some(
                            common::parse_program(include_str!("../../../std/failure.hew")).items,
                        );
                    }
                }
            }
            decl.resolved_items = Some(imported);
        }
    }

    let mut root = common::parse_program(root_source);
    for (item, _) in &mut root.items {
        let Item::Import(decl) = item else {
            continue;
        };
        if decl.path == ["app", "helper"] {
            decl.resolved_items = Some(helper.items.clone());
        }
    }
    let mut checker = common::checker();
    checker.check_program(&root)
}

fn typecheck_link_monitor_import_edge(
    target_path: &[&str],
    target_source: &str,
) -> hew_types::TypeCheckOutput {
    let root_id = ModuleId::root();
    let target_id = ModuleId::new(
        target_path
            .iter()
            .map(|segment| (*segment).to_string())
            .collect(),
    );
    let consumer_id = ModuleId::new(vec!["std".to_string(), "link_monitor".to_string()]);
    let target_items = common::parse_program(target_source).items;
    let mut consumer = common::parse_program(&format!(
        "import {}.{{CrashKind}};\n\
         pub enum ImportedReason {{ Crashed(CrashKind); }}",
        target_path.join(".")
    ));
    for (item, _) in &mut consumer.items {
        if let Item::Import(decl) = item {
            decl.resolved_items = Some(target_items.clone());
        }
    }

    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: target_id.clone(),
            items: target_items,
            imports: vec![],
            source_paths: if target_path == ["std", "failure"] {
                vec![common::repo_root().join("std/failure.hew")]
            } else {
                vec![]
            },
            doc: None,
        })
        .expect("target fixture must be unique");
    graph
        .add_module(Module {
            id: consumer_id.clone(),
            items: consumer.items,
            imports: vec![ModuleImport {
                target: target_id.clone(),
                spec: None,
                span: 0..0,
            }],
            source_paths: vec![],
            doc: None,
        })
        .expect("consumer fixture must be unique");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("root fixture must be unique");
    graph.topo_order = vec![target_id, consumer_id, root_id];
    let program = hew_parser::ast::Program {
        items: vec![],
        module_doc: None,
        module_graph: Some(graph),
    };
    let mut checker = common::checker();
    checker.check_program(&program)
}

// ── Accept fixtures ──────────────────────────────────────────────────

#[test]
fn named_imported_exit_payload_is_source_authoritative_and_consumes_import() {
    let output = typecheck_with_resolved_std(
        r"
        import std.failure.{CrashNotification};

        actor Watcher {
            #[on(exit)]
            fn on_peer_exit(note: CrashNotification) {
                let _id = note.actor_id;
            }
        }

        fn main() {}
        ",
    );
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
    assert!(
        !output.warnings.iter().any(|warning| {
            warning.kind == TypeErrorKind::UnusedImport && warning.message.contains("failure")
        }),
        "published lifecycle type use must consume its import: {:?}",
        output.warnings
    );
}

#[test]
fn aliased_imported_exit_payload_keeps_canonical_lifecycle_identity() {
    let output = typecheck_with_resolved_std(
        r"
        import std.failure.{CrashNotification as ExitNote};

        actor Watcher {
            #[on(exit)]
            fn on_peer_exit(note: ExitNote) {
                let _id = note.actor_id;
            }
        }

        fn main() {}
        ",
    );
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
}

#[test]
fn whole_module_aliases_keep_canonical_exit_and_down_hook_identity() {
    let output = typecheck_with_resolved_std(
        r"
        import std.failure as f;
        import std.link_monitor as lm;

        actor Watcher {
            #[on(exit)]
            fn on_peer_exit(note: f.CrashNotification) {
                let _id = note.actor_id;
            }

            #[on(down)]
            fn on_down(note: lm.DownNotification) {
                let _id = note.monitor.value;
            }
        }

        fn main() {
            let _kind = f.CrashKind.Crashed;
            let _target = lm.DownTarget.Local(7);
            let _reason = lm.DownReason.Exited;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "whole-module aliases must resolve to canonical lifecycle identities: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|warning| warning.kind == TypeErrorKind::UnusedImport),
        "qualified alias uses must consume both imports: {:?}",
        output.warnings
    );
    assert_eq!(
        output
            .import_type_name_aliases
            .get(&(None, 0, "f.CrashNotification".to_string()))
            .map(String::as_str),
        Some("std.failure.CrashNotification"),
        "the checker must publish its proven whole-module lifecycle identity for HIR"
    );
}

#[test]
fn user_backed_std_module_ids_do_not_grant_lifecycle_authority() {
    for (module, hook_source) in [
        (
            &["std", "failure"][..],
            r"
            actor Watcher {
                #[on(exit)]
                fn on_peer_exit(note: failure.CrashNotification) {}
            }
            ",
        ),
        (
            &["std", "link_monitor"][..],
            r"
            actor Watcher {
                #[on(down)]
                fn on_down(note: link_monitor.DownNotification) {}
            }
            ",
        ),
    ] {
        let output = typecheck_module_body(module, hook_source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| matches!(error.kind, TypeErrorKind::UndefinedType)),
            "a user-backed std.x module ID must not imply lifecycle authority: {:?}",
            output.errors
        );
    }
}

#[test]
fn spoofed_std_owner_imports_cannot_mint_lifecycle_identities() {
    for (owner, qualified, bare, hook) in [
        (
            "failure",
            "failure.CrashNotification",
            "CrashNotification",
            "exit",
        ),
        (
            "link_monitor",
            "link_monitor.DownNotification",
            "DownNotification",
            "down",
        ),
    ] {
        let qualified_import = format!(
            "import std.{owner};\n\
             actor Watcher {{ #[on({hook})] fn callback(note: {qualified}) {{}} }}\n\
             fn main() {{}}"
        );
        let qualified_output = typecheck_with_spoofed_std_import(owner, &qualified_import);
        assert!(
            qualified_output
                .errors
                .iter()
                .any(|error| matches!(error.kind, TypeErrorKind::UndefinedType)),
            "a whole-module import of spoofed std::{owner} must not authorize {qualified}: {:?}",
            qualified_output.errors
        );

        let named_import = format!(
            "import std.{owner}.{{{bare}}};\n\
             actor Watcher {{ #[on({hook})] fn callback(note: {bare}) {{}} }}\n\
             fn main() {{}}"
        );
        let named_output = typecheck_with_spoofed_std_import(owner, &named_import);
        assert!(
            named_output
                .errors
                .iter()
                .any(|error| matches!(error.kind, TypeErrorKind::UndefinedType)),
            "a named/bare import of spoofed std::{owner} must not authorize {bare}: {:?}",
            named_output.errors
        );
    }
}

#[test]
fn single_segment_owner_module_ids_do_not_grant_lifecycle_authority() {
    for (module, hook_source) in [
        (
            &["failure"][..],
            r"
            actor Watcher {
                #[on(exit)]
                fn on_peer_exit(note: failure.CrashNotification) {}
            }
            ",
        ),
        (
            &["link_monitor"][..],
            r"
            actor Watcher {
                #[on(down)]
                fn on_down(note: link_monitor.DownNotification) {}
            }
            ",
        ),
    ] {
        let output = typecheck_module_body(module, hook_source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| matches!(error.kind, TypeErrorKind::UndefinedType)),
            "a same-named single-segment module must not imply lifecycle authority: {:?}",
            output.errors
        );
    }
}

#[test]
fn canonical_std_module_named_import_is_seeded_before_member_resolution() {
    let output = typecheck_link_monitor_import_edge(
        &["std", "failure"],
        include_str!("../../../std/failure.hew"),
    );
    assert!(
        output.errors.is_empty(),
        "a resolved std.link_monitor -> std.failure named import must authorize CrashKind \
         while member types are pre-registered: {:?}",
        output.errors
    );

    let spoof =
        typecheck_link_monitor_import_edge(&["app", "failure"], "pub enum CrashKind { Crashed, }");
    assert!(
        spoof
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::UndefinedType)),
        "a same-final-segment user module edge must not seed canonical lifecycle authority: {:?}",
        spoof.errors
    );
}

#[test]
fn named_imports_keep_requested_qualified_lifecycle_authority() {
    for (source, requested_type) in [
        (
            r"
        import std.failure.{CrashAction};
        fn main() {
            let _kind = failure.CrashKind.Crashed;
        }
        ",
            None,
        ),
        (
            r"
        import std.failure.{CrashKind};
        fn main() {
            let _kind = failure.CrashKind.Crashed;
        }
        ",
            Some("std.failure.CrashKind"),
        ),
        (
            r"
        import std.link_monitor.{MonitorId};
        fn main() {
            let _reason = link_monitor.DownReason.Exited;
        }
        ",
            None,
        ),
        (
            r"
        import std.link_monitor.{DownReason};
        fn main() {
            let _reason = link_monitor.DownReason.Exited;
        }
        ",
            Some("std.link_monitor.DownReason"),
        ),
    ] {
        let output = typecheck_with_resolved_std(source);
        if let Some(requested_type) = requested_type {
            assert!(
                output.errors.is_empty(),
                "a requested named import must remain available as `{requested_type}`: {:?}",
                output.errors
            );
        } else {
            assert!(
                output
                    .errors
                    .iter()
                    .any(|error| matches!(error.kind, TypeErrorKind::UndefinedType)),
                "a named import must not authorize an unrequested qualified type: {:?}",
                output.errors
            );
        }
    }
}

#[test]
fn source_owned_exit_payload_rejects_plain_and_missing_imports() {
    for source in [
        r"
            import std.failure;
            actor Watcher { #[on(exit)] fn on_peer_exit(note: CrashNotification) {} }
            fn main() {}
        ",
        r"
            actor Watcher { #[on(exit)] fn on_peer_exit(note: CrashNotification) {} }
            fn main() {}
        ",
    ] {
        let output = typecheck_with_resolved_std(source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| matches!(error.kind, TypeErrorKind::UndefinedType)),
            "unpublished lifecycle payload must fail at resolution: {:?}",
            output.errors
        );
    }

    let output = typecheck_with_resolved_std(
        r"
        import std.failure;
        actor Watcher {
            #[on(exit)]
            fn on_peer_exit(note: failure.CrashNotification) { let _id = note.actor_id; }
        }
        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "a qualified payload use must consume a plain module import: {:?}",
        output.errors
    );
    assert!(
        !output.warnings.iter().any(|warning| {
            warning.kind == TypeErrorKind::UnusedImport && warning.message.contains("failure")
        }),
        "a qualified lifecycle use must consume its import: {:?}",
        output.warnings
    );
}

#[test]
fn sibling_loading_failure_does_not_authorize_root_qualified_exit_payload() {
    let mut helper = common::parse_program(
        r"
        import std.failure.{CrashNotification};
        pub fn loaded() {}
        ",
    );
    for (item, _) in &mut helper.items {
        let Item::Import(decl) = item else {
            continue;
        };
        if decl.path == ["std", "failure"] {
            decl.resolved_items =
                Some(common::parse_program(include_str!("../../../std/failure.hew")).items);
        }
    }

    let mut root = common::parse_program(
        r"
        import app.helper;

        actor Watcher {
            #[on(exit)]
            fn on_peer_exit(note: failure.CrashNotification) {}
        }

        fn main() {}
        ",
    );
    for (item, _) in &mut root.items {
        let Item::Import(decl) = item else {
            continue;
        };
        if decl.path == ["app", "helper"] {
            decl.resolved_items = Some(helper.items.clone());
        }
    }
    let mut checker = common::checker();
    let output = checker.check_program(&root);
    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::UndefinedType)),
        "a sibling's import must not authorize the root scope: {:?}",
        output.errors
    );
}

#[test]
fn named_imported_down_payload_and_nested_types_consume_import() {
    let output = typecheck_with_resolved_std(
        r"
        import std.link_monitor.{DownNotification, DownReason, DownTarget};

        actor Watcher {
            #[on(down)]
            fn on_down(note: DownNotification) {
                let _id = note.monitor.value;
                match note.target {
                    DownTarget.Local(slot) => { let _slot = slot; }
                    DownTarget.Remote(location) => { let _location = location; }
                }
                match note.reason {
                    DownReason.Exited => {}
                    DownReason.Crashed(kind) => { let _kind = kind; }
                    DownReason.MonitorLost => {}
                    DownReason.LocalShutdown => {}
                }
            }
        }

        fn main() {}
        ",
    );
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
    assert!(
        !output.warnings.iter().any(|warning| {
            warning.kind == TypeErrorKind::UnusedImport && warning.message.contains("link_monitor")
        }),
        "published DOWN types must consume their import: {:?}",
        output.warnings
    );
}

#[test]
fn source_owned_down_payload_requires_direct_import_authority() {
    for source in [
        r"
            import std.link_monitor;
            actor Watcher { #[on(down)] fn on_down(note: DownNotification) {} }
            fn main() {}
        ",
        r"
            actor Watcher { #[on(down)] fn on_down(note: link_monitor.DownNotification) {} }
            fn main() {}
        ",
    ] {
        let output = typecheck_with_resolved_std(source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| matches!(error.kind, TypeErrorKind::UndefinedType)),
            "unpublished DOWN payload must fail at resolution: {:?}",
            output.errors
        );
    }

    let output = typecheck_with_resolved_std(
        r"
        import std.link_monitor;
        actor Watcher {
            #[on(down)]
            fn on_down(note: link_monitor.DownNotification) { let _id = note.monitor.value; }
        }
        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "a qualified DOWN use must consume a plain module import: {:?}",
        output.errors
    );
    assert!(
        !output.warnings.iter().any(|warning| {
            warning.kind == TypeErrorKind::UnusedImport && warning.message.contains("link_monitor")
        }),
        "a qualified DOWN use must consume its import: {:?}",
        output.warnings
    );
}

#[test]
fn sibling_loading_link_monitor_does_not_authorize_root_qualified_down_payload() {
    let mut monitor = common::parse_program(include_str!("../../../std/link_monitor.hew"));
    for (item, _) in &mut monitor.items {
        let Item::Import(decl) = item else {
            continue;
        };
        if decl.path == ["std", "failure"] {
            decl.resolved_items =
                Some(common::parse_program(include_str!("../../../std/failure.hew")).items);
        }
    }

    let mut helper = common::parse_program("import std.link_monitor; pub fn loaded() {}");
    for (item, _) in &mut helper.items {
        let Item::Import(decl) = item else {
            continue;
        };
        if decl.path == ["std", "link_monitor"] {
            decl.resolved_items = Some(monitor.items.clone());
        }
    }

    let mut root = common::parse_program(
        r"
        import app.helper;
        actor Watcher {
            #[on(down)]
            fn on_down(note: link_monitor.DownNotification) {}
        }
        fn main() {}
        ",
    );
    for (item, _) in &mut root.items {
        let Item::Import(decl) = item else {
            continue;
        };
        if decl.path == ["app", "helper"] {
            decl.resolved_items = Some(helper.items.clone());
        }
    }
    let mut checker = common::checker();
    let output = checker.check_program(&root);
    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::UndefinedType)),
        "a sibling's link-monitor import must not authorize the root scope: {:?}",
        output.errors
    );
}

#[test]
fn transitive_std_failure_defs_do_not_authorize_qualified_constructors_or_variants() {
    for (source, expected_message) in [
        (
            r"
        import app.helper;
        fn main() {
            let _note = failure.CrashNotification.Forged {
                actor_id: 1,
            };
        }
        ",
            "undefined type `failure.CrashNotification.Forged`",
        ),
        (
            r"
        import app.helper;
        fn main() {
            let _kind = failure.CrashKind.HeapExceeded;
        }
        ",
            "undefined variable `failure`",
        ),
    ] {
        let output = typecheck_with_transitive_std("failure", source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.message == expected_message),
            "a transitive std.failure definition must not authorize a value path: {:?}",
            output.errors
        );
    }
}

#[test]
fn transitive_std_link_monitor_defs_do_not_authorize_qualified_constructors_or_variants() {
    for (source, expected_message) in [
        (
            r"
        import app.helper;
        fn main() {
            let _note = link_monitor.DownNotification.Forged {
                monitor: 1,
            };
        }
        ",
            "undefined type `link_monitor.DownNotification.Forged`",
        ),
        (
            r"
        import app.helper;
        fn main() {
            let _target = link_monitor.DownTarget.Remote;
        }
        ",
            "undefined variable `link_monitor`",
        ),
        (
            r"
        import app.helper;
        fn main() {
            let _reason = link_monitor.DownReason.MonitorLost;
        }
        ",
            "undefined variable `link_monitor`",
        ),
    ] {
        let output = typecheck_with_transitive_std("link_monitor", source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.message == expected_message),
            "a transitive std.link_monitor definition must not authorize a value path: {:?}",
            output.errors
        );
    }
}

#[test]
fn local_lifecycle_shadow_does_not_forge_imported_payload() {
    let output = typecheck_with_resolved_std(
        r"
        import std.failure.{CrashNotification};

        type CrashNotification { value: i64, }

        actor Watcher {
            #[on(exit)]
            fn on_peer_exit(note: CrashNotification) { let _value = note.value; }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.iter().any(|error| {
            error
                .message
                .contains("must have type `CrashNotification` (from `std.failure`)")
        }),
        "a local shadow must not forge the lifecycle payload: {:?}",
        output.errors
    );
}

#[test]
fn accept_on_start_only() {
    let output = typecheck(
        r"
        actor Cache {
            let entries: i32,

            #[on(start)]
            fn warm() {
                entries
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "actor with only `#[on(start)]` should type-check: {:?}",
        output.errors
    );
}

#[test]
fn accept_on_stop_only() {
    let output = typecheck(
        r"
        actor Cache {
            let entries: i32,

            #[on(stop)]
            fn flush() {
                entries
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "actor with only `#[on(stop)]` should type-check: {:?}",
        output.errors
    );
}

#[test]
fn accept_multiple_on_stop_in_declared_order() {
    // Multiple `#[on(stop)]` hooks are legal; lexical order is the run
    // order (HEW-SPEC-2026 §9.1.2 rule 6). The type-checker accepts
    // them without complaint.
    let output = typecheck(
        r"
        actor Cache {
            let entries: i32,
            let socket: i32,

            #[on(stop)]
            fn flush_cache() {
                entries
            }

            #[on(stop)]
            fn close_socket() {
                socket
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "multiple `#[on(stop)]` hooks should be accepted: {:?}",
        output.errors
    );
}

#[test]
fn accept_typed_on_down_hook() {
    let output = typecheck(
        r"
        actor Watcher {
            #[on(down)]
            fn on_down(note: DownNotification) {
                let _id = note.monitor.value;
                let _target = note.target;
                let _reason = note.reason;
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "canonical `#[on(down)]` should type-check: {:?}",
        output.errors
    );
}

#[test]
fn reject_user_down_notification_collision_in_typed_hook() {
    let output = typecheck(
        r"
        type DownNotification {
            value: i64,
        }

        actor Watcher {
            #[on(down)]
            fn on_down(note: DownNotification) {
                let _value = note.value;
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.iter().any(|error| {
            error
                .message
                .contains("must have type `DownNotification` (from `std.link_monitor`)")
        }),
        "a user nominal that only shares the lifecycle payload's short name must be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_invalid_on_down_shapes() {
    let output = typecheck(
        r"
        actor Watcher {
            #[on(down, extra)]
            fn extra(note: DownNotification) {}

            #[on(down)]
            fn wrong_type(note: i64) {}

            #[on(down)]
            fn wrong_arity() {}

            #[on(down)]
            fn generic<T>(note: DownNotification) where T: Copy {}

            #[on(down)]
            fn wrong_return(note: DownNotification) -> i64 { 0 }
        }

        fn main() {}
        ",
    );
    let messages: Vec<_> = output
        .errors
        .iter()
        .map(|error| error.message.as_str())
        .collect();
    assert!(
        messages
            .iter()
            .any(|message| message.contains("does not accept extra arguments")),
        "extra hook arguments must be rejected: {:?}",
        output.errors
    );
    assert!(
        messages
            .iter()
            .any(|message| message.contains("must have type `DownNotification`")),
        "wrong DOWN payload type must be rejected: {:?}",
        output.errors
    );
    assert!(
        messages
            .iter()
            .any(|message| message.contains("exactly one parameter")),
        "wrong DOWN hook arity must be rejected: {:?}",
        output.errors
    );
    assert!(
        messages
            .iter()
            .any(|message| message.contains("cannot have type parameters"))
            && messages
                .iter()
                .any(|message| message.contains("cannot have a `where` clause")),
        "generic DOWN hooks must be rejected: {:?}",
        output.errors
    );
    assert!(
        messages
            .iter()
            .any(|message| message.contains("must return `()`")),
        "non-unit DOWN hooks must be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_duplicate_on_down_hook() {
    let output = typecheck(
        r"
        actor Watcher {
            #[on(down)]
            fn first(note: DownNotification) {}

            #[on(down)]
            fn second(note: DownNotification) {}
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("more than one `#[on(down)]`")),
        "duplicate DOWN hooks must be rejected: {:?}",
        output.errors
    );
}

// ── Reject fixtures ──────────────────────────────────────────────────

#[test]
fn reject_hook_with_parameters() {
    // Hooks must take no parameters; actor fields are in scope by bare
    // name. A parameter list (e.g. attempting `self`-style receivers,
    // imported from other ecosystems) is rejected.
    let output = typecheck(
        r"
        actor Worker {
            let count: i32,

            #[on(stop)]
            fn shutdown(unused: i32) {
                count
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.iter().any(|e| e.message.contains("must take")
            && e.message.contains("no parameters")
            && e.message.contains("on(stop)")),
        "hook with parameters should be rejected with a hook-specific \
         diagnostic: {:?}",
        output.errors
    );
}

#[test]
fn reject_duplicate_on_start() {
    // `#[on(start)]` is at-most-once per actor (rule 6). Declaring two
    // is a structural error.
    let output = typecheck(
        r"
        actor Worker {
            let count: i32,

            #[on(start)]
            fn first() {
                count
            }

            #[on(start)]
            fn second() {
                count
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("more than one `#[on(start)]`")),
        "duplicate `#[on(start)]` should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_unknown_hook_kind() {
    // `#[on(restart)]` is not a recognised lifecycle hook in this edition;
    // the checker emits a diagnostic listing the valid kinds (start, stop).
    // Uses a plain identifier that is not a reserved keyword.
    let output = typecheck(
        r"
        actor Worker {
            let count: i32,

            #[on(restart)]
            fn setup() {
                count
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("on(restart)")
                && e.message.contains("start")
                && e.message.contains("stop")
                && e.message.contains("crash")
                && !e.message.contains("upgrade")),
        "`#[on(restart)]` should be rejected with valid-kinds list \
         (start, stop, crash, exit, down) and no longer name `upgrade`: {:?}",
        output.errors
    );
}

// ── E1: `#[on(crash)]` recognition / `#[on(upgrade)]` removed ─────────
//
// `#[on(crash)]` remains a live lifecycle hook. `upgrade` left the hook-kind
// list (HEW-SPEC-2026 §12.6): `#[on(upgrade)]` is now an ordinary unrecognised
// hook kind, the same as any other misspelling.

#[test]
fn on_crash_still_works() {
    // `#[on(crash)]` with a diverging body (`panic(...)`) must type-check
    // cleanly.  The hook itself is live — only the non-diverging
    // `CrashAction`-return path is fail-closed (see
    // `reject_crash_action_return_not_yet_wired`).
    let output = typecheck(
        r#"
        actor Worker {
            let count: i32,

            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                panic("handled")
            }
        }

        fn main() {}
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "well-formed `#[on(crash)]` with diverging body should type-check cleanly: {:?}",
        output.errors
    );
}

#[test]
fn on_upgrade_attribute_compile_errors() {
    // `upgrade` left the `#[on(..)]` hook-kind list (HEW-SPEC-2026 §12.6):
    // `#[on(upgrade)]` is rejected the same way as any other unrecognised
    // hook kind, not through a bespoke reserved-attribute diagnostic.
    let source = r"
        actor Worker {
            let count: i32,

            #[on(upgrade)]
            fn on_upgrade() {
                count
            }
        }

        fn main() {}
        ";
    let output = typecheck(source);
    let error = output
        .errors
        .iter()
        .find(|e| {
            matches!(&e.kind, TypeErrorKind::InvalidOperation) && e.message.contains("on(upgrade)")
        })
        .expect("`#[on(upgrade)]` should be rejected as an unrecognised lifecycle hook");
    let attr_start = source
        .find("#[on(upgrade)]")
        .expect("fixture should contain upgrade attribute");
    let attr_span = attr_start..attr_start + "#[on(upgrade)]".len();
    assert_eq!(
        error.span, attr_span,
        "diagnostic should point at the `#[on(upgrade)]` attribute"
    );
    assert!(
        error.message.contains("not a recognised")
            && error.message.contains("start")
            && error.message.contains("stop"),
        "diagnostic should explain the hook kind is unrecognised and list the valid kinds: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_crash_with_extra_args() {
    // `#[on(crash, foo)]` is malformed — the event slot takes exactly
    // one identifier. start/stop reach this through `check_lifecycle_hook`,
    // but crash has event-specific signature checking, so the attribute-shape
    // check lives in the event dispatch itself.
    let output = typecheck(
        r"
        actor Worker {
            #[on(crash, foo)]
            fn on_crash() {
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.iter().any(|e| e.message.contains("on(crash)")
            && e.message.contains("does not accept extra arguments")),
        "`#[on(crash, …)]` with extra args should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_upgrade_with_extra_args() {
    // `upgrade` is no longer a recognised hook kind at all (HEW-SPEC-2026
    // §12.6), so `#[on(upgrade, v2)]` fails the same way `#[on(upgrade)]`
    // does — the unrecognised-kind check runs before any extra-args check.
    let output = typecheck(
        r"
        actor Worker {
            #[on(upgrade, v2)]
            fn on_upgrade() {
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(&e.kind, TypeErrorKind::InvalidOperation)
                && e.message.contains("on(upgrade)")
                && e.message.contains("not a recognised")),
        "`#[on(upgrade, …)]` should be rejected as an unrecognised hook kind: {:?}",
        output.errors
    );
}

// ── E1b: `#[on(crash)]` returns a CrashAction value (M-4) ────────────
//
// The `CrashAction` enum-variant return is now wired end-to-end: the checker
// accepts a CrashAction-returning hook body (the former
// `CrashActionReturnNotYetWired` fail-closed gate is removed), MIR returns the
// CrashAction value by its natural enum-return ABI, and the supervisor honours
// the returned variant. Every return position — tail expression, explicit
// `return`, non-final return, in-branch return — type-checks identically.

#[test]
fn accept_crash_action_tail_return() {
    // A `CrashAction::Restart` tail expression now type-checks cleanly.
    let source = r"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                CrashAction.Restart
            }
        }

        fn main() {}
        ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "`CrashAction::Restart` tail body should type-check cleanly: {:?}",
        output.errors
    );
}

#[test]
fn accept_crash_action_explicit_return_stmt() {
    // The explicit `return CrashAction::Restart;` form also type-checks.
    let source = r"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                return CrashAction.Restart;
            }
        }

        fn main() {}
        ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "`return CrashAction::Restart;` should type-check cleanly: {:?}",
        output.errors
    );
}

#[test]
fn accept_crash_action_nonfinal_return_before_more_stmts() {
    // A non-final `return CrashAction::Restart;` (followed by more code)
    // type-checks: the explicit return is a valid CrashAction return.
    let source = r#"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                return CrashAction.Restart;
                panic("dead code after the early return")
            }
        }

        fn main() {}
        "#;
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "non-final `return CrashAction::Restart;` should type-check cleanly: {:?}",
        output.errors
    );
}

#[test]
fn accept_crash_action_return_inside_if_then_more_code() {
    // A `return CrashAction::Escalate;` inside an `if` branch, with a diverging
    // fallthrough, type-checks: every return position is a valid CrashAction
    // return now that the fail-closed gate is removed.
    let source = r"
        actor Worker {
            let flag: i32,

            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                if flag == 1 {
                    return CrashAction.Escalate;
                }
                CrashAction.Kill
            }
        }

        fn main() {}
        ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "`return CrashAction::Escalate` inside an `if` branch should type-check cleanly: {:?}",
        output.errors
    );
}

#[test]
fn accept_crash_hook_with_if_and_diverging_body() {
    // Accept twin: an `#[on(crash)]` hook that uses an `if` branch with a
    // diverging expression (`panic(...)`) in each arm still type-checks cleanly.
    let output = typecheck(
        r#"
        actor Worker {
            let flag: i32,

            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                if flag == 1 {
                    panic("restart path")
                } else {
                    panic("kill path")
                }
            }
        }

        fn main() {}
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "`#[on(crash)]` hook with diverging `if` branches should type-check cleanly: {:?}",
        output.errors
    );
}

// ── E1d: closure nested inside `#[on(crash)]` must not inherit flag ──
//
// A closure defined inside an `#[on(crash)]` hook body is NOT the hook itself.
// A `return CrashAction::X;` inside such a nested closure is a valid closure
// return statement, independent of the hook's own return path.

#[test]
fn accept_closure_inside_crash_hook_returning_crash_action() {
    // The closure captures context from the hook but has its OWN return type
    // annotation of `CrashAction`. The `return CrashAction::Restart;` inside the
    // closure is a valid closure return statement; the hook body itself diverges
    // via `panic(...)`. Both type-check cleanly.
    let output = typecheck(
        r#"
        actor Worker {
            let flag: i32,

            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                let handler = || -> CrashAction {
                    return CrashAction.Restart;
                };
                panic("diverging hook body")
            }
        }

        fn main() {}
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "a closure inside `#[on(crash)]` that returns CrashAction should type-check cleanly: {:?}",
        output.errors
    );
}

// ── M-7-R: `#[on(exit)]` linked-actor exit hook ──────────────────────
//
// The exit hook fires when an actor THIS actor is linked to crashes/exits,
// delivering a typed `CrashNotification { actor_id, kind }`. Signature:
// `fn on_exit(note: CrashNotification)` (returns `()`).

#[test]
fn accept_on_exit_hook_canonical_shape() {
    let output = typecheck(
        r"
        actor Watcher {
            #[on(exit)]
            fn on_peer_exit(note: CrashNotification) {
                let _id = note.actor_id;
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "`#[on(exit)] fn on_peer_exit(note: CrashNotification)` should type-check cleanly: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_exit_hook_wrong_param_type() {
    let output = typecheck(
        r"
        actor Watcher {
            #[on(exit)]
            fn on_peer_exit(note: CrashInfo) {
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.iter().any(|e| {
            matches!(&e.kind, TypeErrorKind::InvalidOperation)
                && e.message.contains("CrashNotification")
        }),
        "`#[on(exit)]` with a non-CrashNotification param must reject: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_exit_hook_nonunit_return() {
    let output = typecheck(
        r"
        actor Watcher {
            #[on(exit)]
            fn on_peer_exit(note: CrashNotification) -> i64 {
                42
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.iter().any(|e| {
            matches!(&e.kind, TypeErrorKind::InvalidOperation)
                && e.message.contains("must return `()`")
        }),
        "`#[on(exit)]` with a non-unit return must reject: {:?}",
        output.errors
    );
}

// ── E2: `#[on(crash)]` signature pinning ─────────────────────────────
//
// Failure-philosophy slice E2 pins the crash hook signature shape:
// `fn on_crash(info: CrashInfo) -> CrashAction`.  `CrashInfo` and
// `CrashAction` come from `std/failure.hew` and are pre-registered for
// inline tests via `register_builtin_failure_surface`.  Each rejection
// has an accept twin and a reject twin (HEW-SPEC-2026 §3.3 both-path).

#[test]
fn on_crash_signature_pinned() {
    // Accept twin: the canonical shape with a diverging body.
    // The `CrashAction` return type is correctly validated; the body uses
    // `panic(...)` to avoid `CrashActionReturnNotYetWired` (see the
    // `crash_action_variants_recognised_by_type_checker` test for the
    // non-diverging case).
    let output = typecheck(
        r#"
        actor Worker {
            let count: i32,

            #[on(crash)]
            fn on_crash(info: CrashInfo) -> CrashAction {
                panic("crash")
            }
        }

        fn main() {}
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "canonical `#[on(crash)]` shape with diverging body should type-check: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_crash_missing_param() {
    let output = typecheck(
        r"
        actor Worker {
            #[on(crash)]
            fn on_crash() -> CrashAction {
                CrashAction.Restart
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output.errors.iter().any(|e| e.message.contains("on(crash)")
            && e.message.contains("exactly one parameter")),
        "`#[on(crash)]` with no params should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_crash_wrong_param_type() {
    let output = typecheck(
        r"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: i32) -> CrashAction {
                CrashAction.Restart
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("on(crash)")
                && e.message.contains("must have type `CrashInfo`")),
        "`#[on(crash)]` with non-CrashInfo param should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_crash_missing_return_type() {
    let output = typecheck(
        r"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: CrashInfo) {
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("on(crash)")
                && e.message.contains("must declare a return type")),
        "`#[on(crash)]` without an explicit return type should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn reject_on_crash_wrong_return_type() {
    let output = typecheck(
        r"
        actor Worker {
            #[on(crash)]
            fn on_crash(info: CrashInfo) -> i32 {
                0
            }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("on(crash)")
                && e.message.contains("must return `CrashAction`")),
        "`#[on(crash)]` returning non-CrashAction should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn crash_action_variants_recognised_by_type_checker() {
    // The `CrashAction` enum carries three variants per Q46/A23:
    // `Restart | Escalate | Kill`. The type-checker recognises each variant as
    // a valid `CrashAction` expression and (M-4) accepts it as the hook return
    // — no signature mismatch, no fail-closed gate, no error at all.
    for variant in ["Restart", "Escalate", "Kill"] {
        let src = format!(
            "
            actor Worker {{
                #[on(crash)]
                fn on_crash(info: CrashInfo) -> CrashAction {{
                    CrashAction.{variant}
                }}
            }}

            fn main() {{}}
            "
        );
        let output = typecheck(&src);
        assert!(
            output.errors.is_empty(),
            "`CrashAction::{variant}` should type-check cleanly as a hook return: {:?}",
            output.errors
        );
    }
}
