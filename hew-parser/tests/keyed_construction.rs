//! Keyed construction: `spawn`, supervisor children and transition heads take
//! their keys in braces; the retired parenthesised lists recover with a
//! fix-it and migrate to the brace spelling.

use hew_parser::ast::{Expr, Item, Stmt};
use hew_parser::ast_eq::program_eq_ignoring_spans;
use hew_parser::ParseDiagnosticKind;

const DECLS: &str = "actor Pair {\n    let a: i64;\n    let b: i64;\n}\n\nactor Store {\n    var n: i64 = 0;\n}\n\n";

/// The one error `source` reports: its code and hint.
fn single_error(source: &str) -> (ParseDiagnosticKind, &'static str, String) {
    let parsed = hew_parser::parse(source);
    assert_eq!(parsed.errors.len(), 1, "errors: {:?}", parsed.errors);
    let error = &parsed.errors[0];
    (
        error.kind.clone(),
        error.kind.as_kind_str(),
        error.hint.clone().unwrap_or_default(),
    )
}

fn main_with(body: &str) -> String {
    format!("{DECLS}fn main() {{\n    let a = 1;\n    let b = 2;\n{body}}}\n")
}

/// The keys of the spawn in `let p = spawn ..;`, the last statement of `main`.
fn spawn_keys(source: &str) -> Vec<(String, bool)> {
    let parsed = hew_parser::parse(source);
    let Some((Item::Function(main), _)) = parsed.program.items.last() else {
        panic!("no main");
    };
    let Some((
        Stmt::Let {
            value: Some(value), ..
        },
        _,
    )) = main.body.stmts.last()
    else {
        panic!("no let");
    };
    let Expr::Spawn {
        args, arg_labels, ..
    } = &value.0
    else {
        panic!("not a spawn: {:?}", value.0);
    };
    args.iter()
        .zip(arg_labels)
        .map(|((key, _), label)| (key.to_string(), label.shorthand))
        .collect()
}

#[test]
fn spawn_keys_parse_in_braces_with_shorthand() {
    let source = main_with("    let p = spawn Pair { b, a: a + 1 };\n");
    assert!(hew_parser::parse(&source).errors.is_empty());
    assert_eq!(
        spawn_keys(&source),
        vec![("b".to_string(), true), ("a".to_string(), false)]
    );
}

#[test]
fn parenthesised_spawn_is_refused_with_the_brace_fix() {
    let (kind, code, hint) = single_error(&main_with("    let p = spawn Pair(b, a);\n"));
    assert_eq!(kind, ParseDiagnosticKind::LegacySpawnArgs);
    assert_eq!(code, "E_SPAWN_PAREN_ARGS");
    assert_eq!(hint, "write `spawn Pair { b, a }`");
}

#[test]
fn empty_parenthesised_spawn_is_refused_with_the_bare_fix() {
    let (_, code, hint) = single_error(&main_with("    let s = spawn Store();\n"));
    assert_eq!(code, "E_SPAWN_PAREN_ARGS");
    assert_eq!(hint, "write `spawn Store`");
}

#[test]
fn empty_spawn_braces_are_refused() {
    let (_, code, hint) = single_error(&main_with("    let s = spawn Store {};\n"));
    assert_eq!(code, "E_EMPTY_KEY_BRACES");
    assert_eq!(hint, "write `spawn Store`");
}

#[test]
fn spawn_base_is_refused() {
    let (_, code, _) = single_error(&main_with("    let p = spawn Pair { ..a };\n"));
    assert_eq!(code, "E_SPAWN_BASE");
}

#[test]
fn parenthesised_child_is_refused_with_the_brace_fix() {
    let source = format!(
        "{DECLS}supervisor Pool {{\n    strategy: one_for_one;\n    child p: Pair(b: 1, a: 2) restart: transient;\n}}\n"
    );
    let (_, code, hint) = single_error(&source);
    assert_eq!(code, "E_CHILD_PAREN_ARGS");
    assert_eq!(hint, "write `child p: Pair { b: 1, a: 2 }`");
}

#[test]
fn parenthesised_transition_head_is_refused_with_the_brace_fix() {
    let source = "machine Door {\n    events {\n        Open { by: string; }\n    }\n    state Shut;\n    state Ajar { by: string; }\n    on Open(by): Shut => Ajar { by: by }\n    default { state }\n}\n";
    let (_, code, hint) = single_error(source);
    assert_eq!(code, "E_EVENT_HEAD_PARENS");
    assert_eq!(hint, "write `on Open { by }:`");
}

#[test]
fn every_legacy_spelling_recovers_to_the_brace_tree() {
    let legacy = format!(
        "{DECLS}supervisor Pool(a: i64) {{\n    strategy: one_for_one;\n    child p: Pair(b: a, a);\n}}\n\nmachine Door {{\n    events {{\n        Open {{ by: string; }}\n    }}\n    state Shut;\n    state Ajar {{ by: string; }}\n    on Open(by): Shut => Ajar {{ by: by }}\n    default {{ state }}\n}}\n\nfn main() {{\n    let a = 1;\n    let p = spawn Pair(b: 2, a);\n    let s = spawn Store();\n}}\n"
    );
    let current = format!(
        "{DECLS}supervisor Pool(a: i64) {{\n    strategy: one_for_one;\n    child p: Pair {{ b: a, a }};\n}}\n\nmachine Door {{\n    events {{\n        Open {{ by: string; }}\n    }}\n    state Shut;\n    state Ajar {{ by: string; }}\n    on Open {{ by }}: Shut => Ajar {{ by: by }}\n    default {{ state }}\n}}\n\nfn main() {{\n    let a = 1;\n    let p = spawn Pair {{ b: 2, a }};\n    let s = spawn Store;\n}}\n"
    );
    let old = hew_parser::parse(&legacy);
    let new = hew_parser::parse(&current);
    assert!(new.errors.is_empty(), "{:?}", new.errors);
    assert!(program_eq_ignoring_spans(&old.program, &new.program));

    let migrated = hew_parser::fmt::migrate_syntax(&legacy).expect("legacy spellings migrate");
    assert_eq!(migrated, current);
    assert_eq!(
        hew_parser::fmt::migrate_syntax(&migrated).expect("second pass"),
        migrated,
        "migration is idempotent"
    );
}

#[test]
fn spawn_braces_follow_the_block_context_rule() {
    // In a `for` iterable a lone shorthand `{ n }` is the loop body.
    let bare = "actor Gen {\n    let n: i64;\n}\n\nfn main() {\n    let n = 1;\n    for x in spawn Gen { n }\n}\n";
    let parsed = hew_parser::parse(bare);
    let Some((Item::Function(main), _)) = parsed.program.items.last() else {
        panic!("no main");
    };
    let (Stmt::For { iterable, body, .. }, _) = main.body.stmts.last().expect("for") else {
        panic!("not a for loop");
    };
    assert!(matches!(&iterable.0, Expr::Spawn { args, .. } if args.is_empty()));
    assert!(body.trailing_expr.is_some() || !body.stmts.is_empty());

    // Parentheses lift the rule, so the spawn takes the key.
    let wrapped = "actor Gen {\n    let n: i64;\n}\n\nfn main() {\n    let n = 1;\n    for x in (spawn Gen { n }).items() {\n    }\n}\n";
    let parsed = hew_parser::parse(wrapped);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let Some((Item::Function(main), _)) = parsed.program.items.last() else {
        panic!("no main");
    };
    let (Stmt::For { iterable, .. }, _) = main.body.stmts.last().expect("for") else {
        panic!("not a for loop");
    };
    let Expr::MethodCall { receiver, .. } = &iterable.0 else {
        panic!("not a method call: {:?}", iterable.0);
    };
    assert!(matches!(&receiver.0, Expr::Spawn { args, .. } if args.len() == 1));
}

#[test]
fn fmt_keeps_brace_keys_and_shorthand() {
    let source = main_with("    let p = spawn Pair { b, a: a + 1 };\n    let s = spawn Store;\n");
    let parsed = hew_parser::parse(&source);
    assert_eq!(
        hew_parser::fmt::format_source(&source, &parsed.program),
        source
    );
}
