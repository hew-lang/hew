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
    assert_eq!(fields[0].selector, HirDestructureSelector::Tuple(0));
    assert_eq!(fields[0].binding.name, "number");
    assert_eq!(fields[0].binding.ty, ResolvedTy::I64);
    assert_eq!(fields[1].selector, HirDestructureSelector::Tuple(1));
    assert_eq!(fields[1].binding.name, "text");
    assert_eq!(fields[1].binding.ty, ResolvedTy::String);
    let tail = main.body.tail.as_ref().expect("number tail");
    assert!(matches!(&tail.kind, HirExprKind::BindingRef {
        resolved: ResolvedRef::Binding(id), ..
    } if *id == fields[0].binding.id));
}

#[test]
fn tuple_let_wildcard_preserves_an_independent_owned_field_binding() {
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
    assert_eq!(fields[0].binding.name, "number");
    assert_eq!(fields[0].binding.ty, ResolvedTy::I64);
    assert_eq!(fields[1].selector, HirDestructureSelector::Tuple(1));
    assert_eq!(fields[1].binding.ty, ResolvedTy::String);
    assert_ne!(fields[0].binding.id, fields[1].binding.id);
}
