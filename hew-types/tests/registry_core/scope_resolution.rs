//! A written type path resolves through `Scope`: what a module-qualified path
//! names, what an import binds, and how an unresolved path is reported.

use crate::common;
use hew_parser::ast::Item;
use hew_types::error::TypeErrorKind;
use hew_types::TypeCheckOutput;

/// Check `root_source`, whose `import lib…;` declarations name a module with
/// `lib_source`'s items.
fn check_with_lib(lib_source: &str, root_source: &str) -> TypeCheckOutput {
    let lib_items = common::parse_program(lib_source).items;
    let mut program = common::parse_program(root_source);
    for (item, _) in &mut program.items {
        if let Item::Import(decl) = item {
            decl.resolved_items = Some(lib_items.clone().into());
        }
    }
    common::checker().check_program(&program)
}

fn has_error(output: &TypeCheckOutput, kind: &TypeErrorKind, text: &str) -> bool {
    output
        .errors
        .iter()
        .any(|error| &error.kind == kind && error.message.contains(text))
}

const LIB: &str = "pub type Shown {\n    x: i64;\n}\n\npub type Other {\n    y: i64;\n}\n\ntype Hidden {\n    z: i64;\n}\n";

#[test]
fn qualified_unknown_type_is_reported_at_its_path() {
    let output = check_with_lib(LIB, "import lib;\nfn take(v: lib.Missing) -> i64 { 0 }\n");
    assert!(
        has_error(&output, &TypeErrorKind::UndefinedType, "lib.Missing"),
        "{:#?}",
        output.errors
    );

    let control = check_with_lib(LIB, "import lib;\nfn take(v: lib.Shown) -> i64 { v.x }\n");
    assert!(control.errors.is_empty(), "{:#?}", control.errors);
}

#[test]
fn qualified_private_type_is_refused() {
    let output = check_with_lib(LIB, "import lib;\nfn take(v: lib.Hidden) -> i64 { 0 }\n");
    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::VisibilityViolationPrivate { .. })),
        "{:#?}",
        output.errors
    );
}

#[test]
fn selection_binds_its_module_for_qualified_siblings() {
    let output = check_with_lib(
        LIB,
        "import lib.{Shown};\nfn take(v: Shown, w: lib.Other) -> i64 { v.x + w.y }\n",
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);

    let control = common::typecheck("fn take(w: lib.Other) -> i64 { w.y }\n");
    assert!(
        has_error(&control, &TypeErrorKind::UndefinedType, "lib.Other"),
        "a module no import binds names nothing: {:#?}",
        control.errors
    );
}

#[test]
fn catalog_builtin_answers_to_its_qualified_spelling() {
    let output = common::typecheck("fn close_sink(consume tx: stream.Sink<i64>) { tx.close(); }\n");
    assert!(output.errors.is_empty(), "{:#?}", output.errors);

    let control = common::typecheck("fn take(tx: stream.Missing<i64>) {}\n");
    assert!(
        has_error(&control, &TypeErrorKind::UndefinedType, "stream.Missing"),
        "{:#?}",
        control.errors
    );
}
