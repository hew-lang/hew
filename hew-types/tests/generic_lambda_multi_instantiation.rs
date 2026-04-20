mod common;

use common::parse_and_typecheck_isolated as parse_and_check;
use hew_parser::ast::{Expr, Item, Stmt};
use hew_types::check::SpanKey;
use hew_types::Ty;

fn main_call_spans(program: &hew_parser::ast::Program) -> Vec<hew_parser::ast::Span> {
    let main_fn = program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => Some(fd),
            _ => None,
        })
        .expect("main function should exist");

    main_fn
        .body
        .stmts
        .iter()
        .filter_map(|(stmt, _)| match stmt {
            Stmt::Let {
                value: Some((Expr::Call { .. }, span)),
                ..
            }
            | Stmt::Expression((Expr::Call { .. }, span)) => Some(span.clone()),
            _ => None,
        })
        .collect()
}

#[test]
fn generic_lambda_multi_instantiation_int_and_string() {
    let source = r#"
        fn main() {
            let id = <T>(x: T) -> T => x;
            let a: int = id(42);
            let b: string = id("hello");
        }
    "#;

    let (program, output) = parse_and_check(source);
    let call_spans = main_call_spans(&program);
    assert_eq!(
        call_spans.len(),
        2,
        "expected two generic lambda call sites"
    );
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(output.call_type_args.len(), 2);
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[0])),
        Some(&vec![Ty::I64])
    );
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[1])),
        Some(&vec![Ty::String])
    );
}

#[test]
fn generic_lambda_multi_instantiation_three_calls() {
    let source = r#"
        fn main() {
            let id = <T>(x: T) -> T => x;
            let a: int = id(42);
            let b: bool = id(true);
            let c: string = id("hello");
        }
    "#;

    let (program, output) = parse_and_check(source);
    let call_spans = main_call_spans(&program);
    assert_eq!(
        call_spans.len(),
        3,
        "expected three generic lambda call sites"
    );
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[0])),
        Some(&vec![Ty::I64])
    );
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[1])),
        Some(&vec![Ty::Bool])
    );
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[2])),
        Some(&vec![Ty::String])
    );
}

#[test]
fn generic_lambda_multi_instantiation_var_bound_int_and_string() {
    let source = r#"
        fn main() {
            var id = <T>(x: T) -> T => x;
            let a: int = id(42);
            let b: string = id("hello");
        }
    "#;

    let (program, output) = parse_and_check(source);
    let call_spans = main_call_spans(&program);
    assert_eq!(
        call_spans.len(),
        2,
        "expected two generic lambda call sites"
    );
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(output.call_type_args.len(), 2);
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[0])),
        Some(&vec![Ty::I64])
    );
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[1])),
        Some(&vec![Ty::String])
    );
}

#[test]
fn generic_lambda_multi_instantiation_with_captures() {
    let source = r"
        fn main() {
            let offset: int = 10;
            let flag: bool = false;
            let attach = <T>(x: T) -> (T, int, bool) => (x, offset, flag);
            let a = attach(42);
            let b = attach(true);
        }
    ";

    let (program, output) = parse_and_check(source);
    let call_spans = main_call_spans(&program);
    assert_eq!(
        call_spans.len(),
        2,
        "expected two generic lambda call sites"
    );
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[0])),
        Some(&vec![Ty::I64])
    );
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[1])),
        Some(&vec![Ty::Bool])
    );
}

#[test]
fn generic_lambda_multi_instantiation_explicit_types() {
    let source = r#"
        fn main() {
            let id = <T>(x: T) -> T => x;
            let a: int = id<int>(42);
            let b: string = id<string>("hello");
        }
    "#;

    let (_program, output) = parse_and_check(source);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert!(output.call_type_args.is_empty());
}

#[test]
fn generic_lambda_multi_instantiation_two_params_per_call() {
    let source = r#"
        fn main() {
            let pick = <A, B>(a: A, b: B) -> A => a;
            let first: int = pick(42, true);
            let second: string = pick("hello", 7);
        }
    "#;

    let (program, output) = parse_and_check(source);
    let call_spans = main_call_spans(&program);
    assert_eq!(
        call_spans.len(),
        2,
        "expected two generic lambda call sites"
    );
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[0])),
        Some(&vec![Ty::I64, Ty::Bool])
    );
    assert_eq!(
        output.call_type_args.get(&SpanKey::from(&call_spans[1])),
        Some(&vec![Ty::String, Ty::I64])
    );
}
