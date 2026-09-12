//! Tuple patterns preserve the resolved producer and every typed field,
//! including omitted owned values, without relying on synthetic names.

use hew_hir::{
    lower_program_host_target, HirDestructureSelector, HirExprKind, HirItem, HirStmtKind,
    ResolutionCtx, ResolvedRef,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn lower_with_typecheck(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "type errors: {:#?}", facts.errors);
    let output = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );
    output
}

fn function<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a hew_hir::HirFn {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == name => Some(function),
            _ => None,
        })
        .expect("source function must be present")
}

#[test]
fn tuple_let_preserves_the_resolved_call_and_field_types() {
    let output = lower_with_typecheck(
        r#"
        fn pair() -> (i64, string) { (7, "payload") }
        fn main() -> i64 {
            let (number, text) = pair();
            number
        }
    "#,
    );
    let main = function(&output, "main");
    let (source, fields) = main
        .body
        .statements
        .iter()
        .find_map(|statement| match &statement.kind {
            HirStmtKind::Destructure { value, fields } => Some((value, fields)),
            _ => None,
        })
        .expect("tuple call must feed a typed destructure");
    assert!(matches!(&source.kind, HirExprKind::Call {
        target: hew_types::CallTarget::User(declaration), args, ..
    } if declaration == &function(&output, "pair").declaration && args.is_empty()));
    assert_eq!(fields.len(), 2);
    let number = bound(&fields[0]);
    let text = bound(&fields[1]);
    assert_eq!(fields[0].selector, HirDestructureSelector::Tuple(0));
    assert_eq!(number.name, "number");
    assert_eq!(number.ty, ResolvedTy::I64);
    assert_eq!(fields[1].selector, HirDestructureSelector::Tuple(1));
    assert_eq!(text.name, "text");
    assert_eq!(text.ty, ResolvedTy::String);
    let tail = main.body.tail.as_ref().expect("number tail");
    assert!(matches!(&tail.kind, HirExprKind::BindingRef {
        resolved: ResolvedRef::Binding(id), ..
    } if *id == number.id));
}

#[test]
fn tuple_let_wildcard_keeps_its_field_and_binds_nothing() {
    let output = lower_with_typecheck(
        r#"
        fn main() -> i64 {
            let pair = (7, "omitted");
            let (number, _) = pair;
            number
        }
    "#,
    );
    let main = function(&output, "main");
    let original = main
        .body
        .statements
        .iter()
        .find_map(|statement| match &statement.kind {
            HirStmtKind::Let(binding, _) if binding.name == "pair" => Some(binding.id),
            _ => None,
        })
        .expect("original tuple binding");
    let (source, fields) = main
        .body
        .statements
        .iter()
        .find_map(|statement| match &statement.kind {
            HirStmtKind::Destructure { value, fields } => Some((value, fields)),
            _ => None,
        })
        .expect("wildcard tuple must retain a typed destructure");
    assert!(matches!(&source.kind, HirExprKind::BindingRef {
        resolved: ResolvedRef::Binding(id), ..
    } if *id == original));
    assert_eq!(fields.len(), 2, "the wildcard must not erase its field");
    assert_eq!(fields[0].selector, HirDestructureSelector::Tuple(0));
    let number = bound(&fields[0]);
    assert_eq!(number.name, "number");
    assert_eq!(number.ty, ResolvedTy::I64);
    assert_eq!(fields[1].selector, HirDestructureSelector::Tuple(1));
    assert!(
        fields[1].binding.is_none(),
        "a wildcard field names nothing, so it takes nothing out of the source"
    );
}

/// The binding one destructure field introduces.
fn bound(field: &hew_hir::HirDestructureField) -> &hew_hir::HirBinding {
    field
        .binding
        .as_ref()
        .expect("destructure field must bind a name")
}
