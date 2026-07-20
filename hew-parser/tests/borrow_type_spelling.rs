//! `&T` is an immutable, non-owning view spelling reserved for foreign signatures.

use hew_parser::{
    ast::{Item, TypeExpr},
    parse,
};

const OUTSIDE_EXTERN_MESSAGE: &str =
    "`&T` is only allowed in `extern` function signatures; write `T` in ordinary Hew code";

fn extern_functions(src: &str) -> Vec<hew_parser::ast::ExternFnDecl> {
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors for `{src}`: {:#?}",
        result.errors
    );
    let item = result.program.items.into_iter().next().expect("one item").0;
    let Item::ExternBlock(block) = item else {
        panic!("expected extern block, got {item:?}");
    };
    block.functions
}

fn assert_rejected_at_ampersand(src: &str) {
    let ampersand = src.rfind('&').expect("fixture must contain an ampersand");
    let result = parse(src);
    assert_eq!(
        result.errors.len(),
        1,
        "errors for `{src}`: {:#?}",
        result.errors
    );
    let error = &result.errors[0];
    assert_eq!(error.message, OUTSIDE_EXTERN_MESSAGE);
    assert_eq!(error.span, ampersand..ampersand + 1);
}

#[test]
fn extern_borrow_parameter_and_return_parse() {
    let functions =
        extern_functions("extern \"C\" { fn read(value: &i64) -> i64; fn get() -> &i64; }");
    assert!(matches!(functions[0].params[0].ty.0, TypeExpr::Borrow(_)));
    assert!(matches!(
        functions[1].return_type.as_ref().map(|ty| &ty.0),
        Some(TypeExpr::Borrow(_))
    ));
}

#[test]
fn extern_borrow_context_recurses_through_generics_and_callbacks() {
    let functions = extern_functions(
        "extern \"C\" { fn nested(value: &Vec<i64>, callback: fn(&i64) -> i64) -> \
         Option<&i64>; }",
    );
    let params = &functions[0].params;
    assert!(matches!(params[0].ty.0, TypeExpr::Borrow(_)));
    let TypeExpr::Function {
        params: callback_params,
        ..
    } = &params[1].ty.0
    else {
        panic!("expected callback type, got {:?}", params[1].ty.0);
    };
    assert!(matches!(callback_params[0].0, TypeExpr::Borrow(_)));

    let Some((TypeExpr::Named { type_args, .. }, _)) = &functions[0].return_type else {
        panic!("expected generic return type");
    };
    assert!(matches!(
        type_args.as_deref(),
        Some([(TypeExpr::Borrow(_), _)])
    ));
}

#[test]
fn extern_borrow_remains_distinct_from_const_pointer() {
    let functions =
        extern_functions("extern \"C\" { fn borrow(value: &i32); fn pointer(value: *const i32); }");
    assert!(matches!(functions[0].params[0].ty.0, TypeExpr::Borrow(_)));
    assert!(matches!(
        functions[1].params[0].ty.0,
        TypeExpr::Pointer {
            is_mutable: false,
            ..
        }
    ));
}

#[test]
fn ordinary_borrow_positions_are_rejected() {
    for source in [
        "fn f(value: &i64) {}",
        "fn f() -> &i64 {}",
        "type Field { value: &i64 }",
        "enum Payload { Value(&i64) }",
        "type Alias = &i64;",
        "trait Read { fn read(value: &i64); }",
        "type Callback = fn(&i64) -> i64;",
        "type Nested = Option<&i64>;",
    ] {
        assert_rejected_at_ampersand(source);
    }
}

#[test]
fn extern_context_does_not_leak_to_following_item() {
    assert_rejected_at_ampersand(
        "extern \"C\" { fn read(value: &i64); } fn ordinary(value: &i64) {}",
    );
}

#[test]
fn mutable_borrow_spellings_are_rejected_everywhere() {
    for source in [
        "fn f(value: &mut i64) {}",
        "fn f(value: &var i64) {}",
        "extern \"C\" { fn f(value: &mut i64); }",
        "extern \"C\" { fn f(value: &var i64); }",
    ] {
        let result = parse(source);
        assert_eq!(
            result.errors.len(),
            1,
            "errors for `{source}`: {:#?}",
            result.errors
        );
        let message = &result.errors[0].message;
        assert!(message.contains("*mut T"), "message: {message}");
        assert!(message.contains("ordinary Hew code"), "message: {message}");
    }
}
