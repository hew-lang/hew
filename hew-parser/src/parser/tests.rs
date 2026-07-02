//! Parser unit tests.

use super::*;

#[test]
fn parse_simple_function() {
    let source = "fn main() { let x = 1 + 2; }";
    let result = parse(source);
    assert!(result.errors.is_empty());
    assert_eq!(result.program.items.len(), 1);
}

/// Supervisor child declarations accept the module-qualified dotted type
/// (`child a: bank.Account`), carrying the qualified actor identity
/// verbatim; bare child types stay the root/local spelling.
#[test]
fn parse_supervisor_child_dotted_module_qualified_type() {
    let source = "supervisor S {\n\
                      \x20   strategy: one_for_one;\n\
                      \x20   intensity: 1 within 60s;\n\
                      \n\
                      \x20   child a: bank.Account(n: 1);\n\
                      \x20   child b: Local;\n\
                      }\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Supervisor(sd) = &result.program.items[0].0 else {
        panic!("expected supervisor, got {:?}", result.program.items[0].0);
    };
    assert_eq!(sd.children[0].actor_type, "bank.Account");
    assert_eq!(sd.children[0].args.len(), 1);
    assert_eq!(sd.children[1].actor_type, "Local");
}

/// A supervisor takes construction-time config params: `supervisor App(config: T)`.
/// The params carry through so child init-arg exprs can derive from runtime config.
#[test]
fn parse_supervisor_construction_time_config_params() {
    let source = "supervisor App(config: AppConfig) {\n\
                      \x20   strategy: one_for_one;\n\
                      \x20   child cache: Cache(capacity: config.cache_size);\n\
                      }\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Supervisor(sd) = &result.program.items[0].0 else {
        panic!("expected supervisor, got {:?}", result.program.items[0].0);
    };
    assert_eq!(sd.params.len(), 1, "one config param");
    assert_eq!(sd.params[0].name, "config");
    // The child init arg references the config binding (a non-literal expr).
    assert_eq!(sd.children[0].args.len(), 1);
    assert_eq!(sd.children[0].args[0].0, "capacity");
}

/// A supervisor without a `(...)` clause has no params (back-compat).
#[test]
fn parse_supervisor_without_params_has_empty_param_list() {
    let source = "supervisor S {\n\
                      \x20   strategy: one_for_one;\n\
                      \x20   child a: Local;\n\
                      }\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Supervisor(sd) = &result.program.items[0].0 else {
        panic!("expected supervisor");
    };
    assert!(sd.params.is_empty());
}

#[test]
fn parse_doc_comment_on_function() {
    let source = "/// Adds numbers.\nfn add(a: i32, b: i32) -> i32 { a + b }";
    let result = parse(source);
    assert!(result.errors.is_empty());
    assert_eq!(result.program.items.len(), 1);
    if let Item::Function(f) = &result.program.items[0].0 {
        assert_eq!(f.doc_comment.as_deref(), Some("Adds numbers."));
    } else {
        panic!("expected function");
    }
}

#[test]
fn parse_module_doc_comment() {
    let source = "//! Module docs.\n//! Line two.\nfn foo() {}";
    let result = parse(source);
    assert!(result.errors.is_empty());
    assert_eq!(
        result.program.module_doc.as_deref(),
        Some("Module docs.\nLine two.")
    );
}

#[test]
fn parse_no_doc_comment() {
    let source = "fn bare() {}";
    let result = parse(source);
    assert!(result.errors.is_empty());
    if let Item::Function(f) = &result.program.items[0].0 {
        assert!(f.doc_comment.is_none());
    } else {
        panic!("expected function");
    }
}

#[test]
fn parse_struct_decl() {
    let source = "type Point { x: i32; y: i32; }";
    let result = parse(source);
    assert!(result.errors.is_empty());
    assert_eq!(result.program.items.len(), 1);
}

#[test]
fn parse_record_named_fields_with_semicolon_emits_hint() {
    let source = "record Point { x: i32; y: i32 }";
    let result = parse(source);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
    assert_eq!(
        result.errors[0].message,
        "expected `,` or `}` after record field, found `;`"
    );
    assert_eq!(
        result.errors[0].hint.as_deref(),
        Some("record fields use commas; write `field: Type,` instead of `field: Type;`")
    );
    assert_eq!(result.program.items.len(), 1);
    let Item::Record(record) = &result.program.items[0].0 else {
        panic!("expected recovered record item");
    };
    let RecordKind::Named(fields) = &record.kind else {
        panic!("expected named record");
    };
    assert_eq!(fields.len(), 2);
}

#[test]
fn parse_actor_decl() {
    let source =
        "actor Counter { var count: i32 = 0; receive fn increment() { count = count + 1; } }";
    let result = parse(source);
    assert!(result.errors.is_empty());
    assert_eq!(result.program.items.len(), 1);
    if let Item::Actor(actor) = &result.program.items[0].0 {
        assert_eq!(actor.fields.len(), 1);
        assert_eq!(actor.fields[0].name, "count");
        assert_eq!(actor.receive_fns.len(), 1);
    } else {
        panic!("expected actor item");
    }
}

#[test]
fn actor_keyword_still_starts_actor_item() {
    let source = "actor ActorPathRegression { receive fn ping() {} }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    assert!(matches!(&result.program.items[0].0, Item::Actor(_)));
}

// ---------------------------------------------------------------------------
// Reserved-word-as-name diagnostics and recovery
// ---------------------------------------------------------------------------

/// A `receive fn` whose name is a reserved keyword produces exactly ONE
/// error that names the keyword and suggests renaming, instead of a
/// cascade of parse errors for each remaining token in the declaration.
#[test]
fn receive_fn_reserved_keyword_name_emits_single_clear_error() {
    let source = "actor Metrics { receive fn record(n: i64) {} }";
    let result = parse(source);
    assert_eq!(
        result.errors.len(),
        1,
        "expected exactly 1 error, got {}: {:?}",
        result.errors.len(),
        result.errors
    );
    let msg = &result.errors[0].message;
    assert!(
        msg.contains("record"),
        "error message must name the reserved word; got: {msg}"
    );
    assert!(
        msg.contains("reserved") || msg.contains("keyword"),
        "error message must say reserved/keyword; got: {msg}"
    );
}

/// Variant: `match` as the name of a plain `fn` method inside an actor.
#[test]
fn actor_method_reserved_keyword_name_emits_single_clear_error() {
    let source = "actor A { fn match(n: i64) {} }";
    let result = parse(source);
    assert_eq!(
        result.errors.len(),
        1,
        "expected exactly 1 error, got {}: {:?}",
        result.errors.len(),
        result.errors
    );
    let msg = &result.errors[0].message;
    assert!(
        msg.contains("match"),
        "error must name the reserved word; got: {msg}"
    );
}

/// Variant: `record` as the name of a top-level `fn` declaration.
#[test]
fn toplevel_fn_reserved_keyword_name_emits_single_clear_error() {
    let source = "fn record(n: i64) -> i64 { n }";
    let result = parse(source);
    assert_eq!(
        result.errors.len(),
        1,
        "expected exactly 1 error, got {}: {:?}",
        result.errors.len(),
        result.errors
    );
    let msg = &result.errors[0].message;
    assert!(
        msg.contains("record"),
        "error must name the reserved word; got: {msg}"
    );
}

#[test]
fn toplevel_fn_actor_name_still_reports_reserved_word() {
    let source = "fn actor() {}";
    let result = parse(source);
    assert!(
        result.errors.iter().any(|err| err
            .message
            .contains("`actor` is a reserved word and cannot be used as a name")),
        "expected reserved-word diagnostic for actor function name, got: {:?}",
        result.errors
    );
}

/// A valid actor with non-keyword names continues to parse cleanly.
#[test]
fn actor_with_non_keyword_receive_fn_name_parses_ok() {
    let source = "actor Metrics { receive fn emit_record(n: i64) {} fn helper() {} }";
    let result = parse(source);
    assert!(
        result.errors.is_empty(),
        "expected no errors, got: {:?}",
        result.errors
    );
    let Item::Actor(actor) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    assert_eq!(actor.receive_fns.len(), 1);
    assert_eq!(actor.methods.len(), 1);
}

/// Recovery: a second valid receive fn after a reserved-keyword name is
/// still parsed correctly (the error does not swallow the rest of the actor).
#[test]
fn reserved_keyword_name_recovery_continues_parsing_sibling_methods() {
    let source = "actor Metrics { receive fn record(n: i64) {} receive fn add(m: i64) {} }";
    let result = parse(source);
    // One error for the bad name, but the actor and sibling method survive.
    assert_eq!(
        result.errors.len(),
        1,
        "expected 1 error, got {}: {:?}",
        result.errors.len(),
        result.errors
    );
    assert_eq!(
        result.program.items.len(),
        1,
        "actor item must still be produced"
    );
    let Item::Actor(actor) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    // The second (valid) receive fn must be recovered.
    assert_eq!(
        actor.receive_fns.len(),
        1,
        "sibling receive fn `add` must be parsed"
    );
    assert_eq!(actor.receive_fns[0].name, "add");
}

#[test]
fn parse_receive_gen_fn() {
    let source = "actor NumberStream { receive gen fn numbers() -> i32 { yield 1; } }";
    let result = parse(source);
    assert!(result.errors.is_empty());
    if let Item::Actor(actor) = &result.program.items[0].0 {
        assert_eq!(actor.receive_fns.len(), 1);
        assert!(actor.receive_fns[0].is_generator);
    } else {
        panic!("expected actor item");
    }
}

#[test]
fn parse_receive_fn_type_params_and_where_clause() {
    let source = "actor Foo { receive fn bar<T>(x: T) -> T where T: Display { x } }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Actor(actor) = &result.program.items[0].0 {
        assert_eq!(actor.receive_fns.len(), 1);
        let rf = &actor.receive_fns[0];
        assert_eq!(rf.name, "bar");
        let tps = rf.type_params.as_ref().expect("expected type_params");
        assert_eq!(tps.len(), 1);
        assert_eq!(tps[0].name, "T");
        let wc = rf.where_clause.as_ref().expect("expected where_clause");
        assert_eq!(wc.predicates.len(), 1);
        assert_eq!(wc.predicates[0].bounds[0].name, "Display");
    } else {
        panic!("expected actor item");
    }
}

#[test]
fn parse_where_clause_trailing_comma_fn() {
    // A standalone function with a trailing comma after the last predicate.
    let source = "fn foo<T, U>(a: T, b: U) where T: Display, U: Clone, { }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Function(f) = &result.program.items[0].0 {
        let wc = f.where_clause.as_ref().expect("expected where_clause");
        assert_eq!(wc.predicates.len(), 2);
        assert_eq!(wc.predicates[0].bounds[0].name, "Display");
        assert_eq!(wc.predicates[1].bounds[0].name, "Clone");
    } else {
        panic!("expected function item");
    }
}

#[test]
fn parse_where_clause_trailing_comma_receive_fn() {
    // A receive fn with a trailing comma — multiline style inlined here.
    let source = "actor Foo { receive fn bar<T, U>(x: T) -> T where T: Display, U: Clone, { x } }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Actor(actor) = &result.program.items[0].0 {
        let rf = &actor.receive_fns[0];
        let wc = rf.where_clause.as_ref().expect("expected where_clause");
        assert_eq!(wc.predicates.len(), 2);
        assert_eq!(wc.predicates[0].bounds[0].name, "Display");
        assert_eq!(wc.predicates[1].bounds[0].name, "Clone");
    } else {
        panic!("expected actor item");
    }
}

#[test]
fn parse_where_clause_single_trailing_comma() {
    // Single predicate with trailing comma is the minimal reproduction case.
    let source = "fn foo<T>(a: T) where T: Display, { }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Function(f) = &result.program.items[0].0 {
        let wc = f.where_clause.as_ref().expect("expected where_clause");
        assert_eq!(wc.predicates.len(), 1);
        assert_eq!(wc.predicates[0].bounds[0].name, "Display");
    } else {
        panic!("expected function item");
    }
}

/// Regression: `if` in tail position (the last and only item before `}`,
/// with no semicolon) must become the function's `trailing_expr`, not a
/// discarded `Stmt::If`.  The downstream HIR/MIR pipeline reads
/// `trailing_expr` as the return value; a `Stmt::If` leaves the return
/// slot uninitialised.
#[test]
fn if_in_tail_position_is_trailing_expr() {
    let source = "fn f(n: i64) -> i64 { if n <= 1 { n } else { n + 1 } }";
    let result = parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let Item::Function(func) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    assert!(
        func.body.stmts.is_empty(),
        "stmts must be empty when `if` is the trailing expr; got {:?}",
        func.body.stmts
    );
    assert!(
        func.body.trailing_expr.is_some(),
        "trailing_expr must be Some(Expr::If {{ .. }}) for a tail-position if"
    );
    assert!(
        matches!(
            func.body.trailing_expr.as_deref(),
            Some((Expr::If { .. }, _))
        ),
        "trailing_expr must be Expr::If, got {:?}",
        func.body.trailing_expr.as_deref().map(|(e, _)| e)
    );
}

/// Regression: `match` in tail position must become `trailing_expr`, not
/// a discarded `Stmt::Match`.
#[test]
fn match_in_tail_position_is_trailing_expr() {
    let source = "fn f(n: i64) -> i64 { match n { 0 => 0, _ => n } }";
    let result = parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let Item::Function(func) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    assert!(
        func.body.stmts.is_empty(),
        "stmts must be empty when `match` is the trailing expr; got {:?}",
        func.body.stmts
    );
    assert!(
        matches!(
            func.body.trailing_expr.as_deref(),
            Some((Expr::Match { .. }, _))
        ),
        "trailing_expr must be Expr::Match"
    );
}

/// `if` followed by a semicolon must remain a statement, not a trailing
/// expression.  The semicolon suppresses tail-position promotion and
/// causes an "unnecessary semicolon" warning (block-expression forms do
/// not need `;`).
#[test]
fn if_with_semicolon_is_statement_not_trailing() {
    let source = "fn main() { if true { 1 } else { 2 }; }";
    let result = parse(source);
    // The trailing `;` after a block-expression emits an "unnecessary
    // semicolon" warning — that is expected and not a hard error.
    let has_real_error = result.errors.iter().any(|e| e.severity == Severity::Error);
    assert!(
        !has_real_error,
        "unexpected hard errors: {:?}",
        result.errors
    );
    let Item::Function(func) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    assert!(
        func.body.trailing_expr.is_none(),
        "semicoloned if must not become trailing_expr"
    );
    assert_eq!(
        func.body.stmts.len(),
        1,
        "semicoloned if must produce one statement"
    );
}

/// `if` as a non-tail statement (followed by more items) must remain a
/// statement, not a trailing expression.
#[test]
fn if_as_non_tail_statement_stays_statement() {
    let source = "fn main() { if true { 1 } else { 2 } let x = 3; }";
    let result = parse(source);
    let has_real_error = result.errors.iter().any(|e| e.severity == Severity::Error);
    assert!(
        !has_real_error,
        "unexpected hard errors: {:?}",
        result.errors
    );
    let Item::Function(func) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    // `if` not at tail position → Stmt::If stays in stmts
    assert_eq!(
        func.body.stmts.len(),
        2,
        "non-tail if followed by let must produce two statements"
    );
    assert!(
        func.body.trailing_expr.is_none(),
        "non-tail if must not become trailing_expr"
    );
}

#[test]
fn parse_if_expression() {
    let source = "fn main() { let result = if x > 0 { x } else { -x }; }";
    let result = parse(source);
    if !result.errors.is_empty() {
        for error in &result.errors {
            eprintln!("Error: {} at {:?}", error.message, error.span);
        }
    }
    assert!(result.errors.is_empty());
}

#[test]
fn parse_match_expression() {
    let source = "fn main() { match opt { Some(x) => x, None => 0, } }";
    let result = parse(source);
    assert!(result.errors.is_empty());
}

#[test]
fn parse_match_block_arms_without_commas() {
    let source = "fn main() { match opt { Some(x) => { x } None => { 0 } } }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_match_bad_arm_pattern_recovers_to_later_arms() {
    let source = "fn main() { match n { fn => 0, 1 => 1, _ => 2 } }";
    let result = parse(source);
    assert_eq!(
        result.errors.len(),
        1,
        "bad arm pattern should not cascade: {:?}",
        result.errors
    );
    assert!(
        matches!(
            result.errors[0].kind,
            ParseDiagnosticKind::InvalidPattern { .. }
        ),
        "expected invalid-pattern diagnostic, got: {:?}",
        result.errors
    );
    let Item::Function(function) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    let Some((Expr::Match { arms, .. }, _)) = function.body.trailing_expr.as_deref() else {
        panic!("expected trailing match expression");
    };
    assert_eq!(arms.len(), 2, "valid later arms should still parse");
}

#[test]
fn parse_lambda() {
    let source = "fn main() { let f = |x: i32| x * 2; }";
    let result = parse(source);
    if !result.errors.is_empty() {
        for error in &result.errors {
            eprintln!("Error: {} at {:?}", error.message, error.span);
        }
    }
    assert!(result.errors.is_empty());
}

#[test]
fn parse_pipe_closure_forms() {
    for source in [
        "fn main() { let f = |x| x + 1; }",
        "fn main() { let f = |x: i32| x + 1; }",
        "fn main() { let f = |x: i32| -> i32 { x + 1 }; }",
        "fn main() { let f = || 42; }",
        "fn main() { let f = move |x| x; }",
    ] {
        let result = parse(source);
        assert!(
            result.errors.is_empty(),
            "expected pipe closure to parse cleanly: {source}\nerrors: {:?}",
            result.errors
        );
    }
}

#[test]
fn parse_pipe_closure_malformed_surfaces_are_explicit_errors() {
    for source in [
        "fn main() { let f = ||; }",
        "fn main() { let f = |x| -> i32 x + 1; }",
    ] {
        let result = parse(source);
        assert!(
            result.errors.iter().any(|error| matches!(
                error.kind,
                ParseDiagnosticKind::ClosurePipeSyntax
            ) && error.message.contains("E_CLOSURE_PIPE_SYNTAX")),
            "expected typed E_CLOSURE_PIPE_SYNTAX for {source}, got {:?}",
            result.errors
        );
    }
}

#[test]
fn parse_mutable_function_param() {
    let source = "fn f(var x: int) -> int { x }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    match &result.program.items[0].0 {
        Item::Function(f) => {
            assert_eq!(f.params.len(), 1);
            assert_eq!(f.params[0].name, "x");
            assert!(f.params[0].is_mutable);
        }
        other => panic!("expected function item, got: {other:?}"),
    }
}

#[test]
fn parse_lambda_var_param_is_error() {
    let source = "fn main() { let f = (var x: int) => x; }";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for lambda var parameter"
    );
    assert!(
        result
            .errors
            .iter()
            .any(|error| error.message.contains("expected expression")),
        "expected expression parse error, got: {:?}",
        result.errors
    );
}

#[test]
fn parse_fibonacci_example() {
    let source = include_str!("../../../examples/fibonacci.hew");
    let result = parse(source);
    if !result.errors.is_empty() {
        for error in &result.errors {
            eprintln!("Error: {} at {:?}", error.message, error.span);
        }
    }
    assert!(result.errors.is_empty());
}

#[test]
fn parse_match_or_pattern() {
    let source = "fn classify(n: i32) -> i32 { match n { 1 | 2 | 3 => 1, _ => 0, } }";
    let result = parse(source);
    if !result.errors.is_empty() {
        for error in &result.errors {
            eprintln!("Error: {} at {:?}", error.message, error.span);
        }
    }
    assert!(result.errors.is_empty());
}

#[test]
fn parse_labeled_while_break_continue() {
    let source = r"fn main() -> i32 {
            var i = 0;
            @outer: while i < 5 {
                var j = 0;
                while j < 5 {
                    if j == 3 { break @outer; }
                    j = j + 1;
                }
                i = i + 1;
            }
            0
        }";
    let result = parse(source);
    for e in &result.errors {
        eprintln!("Error: {} at {:?}", e.message, e.span);
    }
    assert!(result.errors.is_empty());

    // Verify the label was parsed
    if let Item::Function(ref f) = result.program.items[0].0 {
        // Third statement should be the labeled while
        if let Stmt::While { ref label, .. } = f.body.stmts[1].0 {
            assert_eq!(label.as_deref(), Some("outer"));
        } else {
            panic!("expected While statement");
        }
    } else {
        panic!("expected Function item");
    }
}

#[test]
fn parse_context_reader_as_identifier_expression() {
    let source = "fn main() -> u64 { @actor_id }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    let Item::Function(function) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    let Some((Expr::Identifier(name), _)) = function.body.trailing_expr.as_deref() else {
        panic!("expected context reader identifier tail");
    };
    assert_eq!(name, "@actor_id");
}

#[test]
fn parse_labeled_loop() {
    let source = r"fn main() -> i32 {
            @top: loop {
                break @top;
            }
            0
        }";
    let result = parse(source);
    assert!(result.errors.is_empty());
    if let Item::Function(ref f) = result.program.items[0].0 {
        if let Stmt::Loop { ref label, .. } = f.body.stmts[0].0 {
            assert_eq!(label.as_deref(), Some("top"));
        } else {
            panic!("expected Loop statement");
        }
    }
}

#[test]
fn parse_labeled_continue() {
    let source = r"fn main() -> i32 {
            var i = 0;
            @outer: while i < 5 {
                i = i + 1;
                continue @outer;
            }
            0
        }";
    let result = parse(source);
    for e in &result.errors {
        eprintln!("Error: {} at {:?}", e.message, e.span);
    }
    assert!(result.errors.is_empty());
}

#[test]
fn parse_for_await_loop() {
    let source = r"fn main() {
            for await item in stream {
                println(item);
            }
        }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    if let Item::Function(ref f) = result.program.items[0].0 {
        if let Stmt::For { is_await, .. } = &f.body.stmts[0].0 {
            assert!(*is_await);
        } else {
            panic!("expected For statement");
        }
    } else {
        panic!("expected Function item");
    }
}

#[test]
fn parse_async_fn_is_rejected() {
    // `async fn` has no meaning in Hew — async-ness comes from fork{} context
    // (architecture §4.1, D2 ratification). Only `async gen fn` is accepted.
    // The parser emits a generic "expected 'gen fn' after 'async'" error and
    // returns None, so the item is absent from the parsed program.
    let source = "async fn fetch() -> i32 { 42 }";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected a parse error for bare `async fn`"
    );
    assert!(
        result.errors[0]
            .message
            .contains("expected 'gen fn' after 'async'"),
        "expected rejection diagnostic, got: {:?}",
        result.errors[0].message
    );
}

#[test]
fn parse_async_gen_fn() {
    let source = "async gen fn count_up() -> i32 { yield 1; yield 2; }";
    let result = parse(source);
    for _e in &result.errors {}
    match &result.program.items[0].0 {
        Item::Function(_f) => {}
        _ => panic!("expected Function item"),
    }
}

#[test]
fn parse_pub_async_gen_fn() {
    let source = "pub async gen fn numbers() -> i32 { yield 42; }";
    let result = parse(source);
    match &result.program.items[0].0 {
        Item::Function(_f) => {}
        _ => panic!("expected Function item"),
    }
}

#[test]
fn parse_pattern_underscore_integer() {
    let source = "fn main() { match x { 1_000 => 1, _ => 0, } }";
    let result = parse(source);
    assert!(result.errors.is_empty());
}

#[test]
fn parse_negative_literal_pattern() {
    let source = "fn main() { match x { -1 => 0, _ => 1, } }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::Function(func) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    // A bare `match` as the last item in a block is a trailing expression (value-bearing
    // position), not a `Stmt::Match`.  The block has no `;` after the match and no further
    // items before `}`.
    let Some((Expr::Match { arms, .. }, _)) = func.body.trailing_expr.as_deref() else {
        panic!("expected trailing match expression");
    };
    let (Pattern::Literal(Literal::Integer { value, radix }), _) = &arms[0].pattern else {
        panic!("expected literal integer pattern");
    };
    assert_eq!(*value, -1);
    assert_eq!(*radix, IntRadix::Decimal);
}

#[test]
fn parse_pattern_contextual_keywords() {
    // All contextual keywords that can appear as identifiers in patterns.
    // state/event/on/when/join were previously missing from the inline list.
    let keywords = [
        "after",
        "from",
        "init",
        "child",
        "restart",
        "budget",
        "strategy",
        "permanent",
        "transient",
        "temporary",
        "one_for_one",
        "one_for_all",
        "rest_for_one",
        "wire",
        "optional",
        "deprecated",
        "reserved",
        "state",
        "event",
        "on",
        "when",
        "join",
    ];
    for kw in &keywords {
        let source = format!("fn check(x: i32) -> i32 {{ match x {{ {kw} => 1, _ => 0, }} }}");
        let result = parse(&source);
        assert!(
            result.errors.is_empty(),
            "contextual keyword '{kw}' should be usable as pattern identifier, \
                 but got errors: {:?}",
            result.errors,
        );
        let Item::Function(func) = &result.program.items[0].0 else {
            panic!("expected function for keyword '{kw}'");
        };
        // A bare `match` as the last item in a block is a trailing expression.
        let Some((Expr::Match { arms, .. }, _)) = func.body.trailing_expr.as_deref() else {
            panic!("expected trailing match expression for keyword '{kw}'");
        };
        let (Pattern::Identifier(name), _) = &arms[0].pattern else {
            panic!(
                "expected identifier pattern for '{kw}', got {:?}",
                arms[0].pattern
            );
        };
        assert_eq!(name, *kw, "pattern name mismatch for keyword '{kw}'");
    }
}

#[test]
fn parse_lexer_error_reported() {
    // The backtick is not a valid token; it should produce a parse error
    let source = "fn main() { let x = `; }";
    let result = parse(source);
    assert!(result
        .errors
        .iter()
        .any(|e| e.message.contains("unexpected character")));
}

#[test]
fn parse_string_escape_sequences() {
    let source = r#"fn main() -> i32 { let a = "hello\nworld"; let b = "tab\there"; let c = "quote\"end"; let d = "back\\slash"; 0 }"#;
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let body = match &result.program.items[0].0 {
        Item::Function(f) => &f.body,
        _ => panic!("expected Function item"),
    };
    let stmts = &body.stmts;
    let get_str = |idx: usize| -> &str {
        if let (
            Stmt::Let {
                value: Some((Expr::Literal(Literal::String(s)), _)),
                ..
            },
            _,
        ) = &stmts[idx]
        {
            return s.as_str();
        }
        panic!("expected let with string literal at index {idx}");
    };
    assert_eq!(get_str(0), "hello\nworld");
    assert_eq!(get_str(1), "tab\there");
    assert_eq!(get_str(2), "quote\"end");
    assert_eq!(get_str(3), "back\\slash");
}

#[test]
fn parse_string_literal_rejects_embedded_nul_escape() {
    let source = r#"fn main() { let s = "a\0b"; }"#;
    let result = parse(source);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
    let diag = &result.errors[0];
    assert_eq!(diag.message, EMBEDDED_NUL_STRING_MESSAGE);
    assert_eq!(
        diag.span,
        source.find('"').unwrap()..source.rfind('"').unwrap() + 1
    );
    assert!(matches!(diag.kind, ParseDiagnosticKind::InvalidLiteral));
}

#[test]
fn parse_string_literal_rejects_raw_embedded_nul() {
    let source = format!("fn main() {{ let s = \"a{}b\"; }}", '\0');
    let result = parse(&source);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
    let diag = &result.errors[0];
    assert_eq!(diag.message, EMBEDDED_NUL_STRING_MESSAGE);
    let start = source.find('"').unwrap();
    assert_eq!(diag.span, start..start + 5);
    assert!(matches!(diag.kind, ParseDiagnosticKind::InvalidLiteral));
}

#[test]
fn parse_raw_string_literal_rejects_raw_embedded_nul() {
    let source = format!("fn main() {{ let s = r\"a{}b\"; }}", '\0');
    let result = parse(&source);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
    let diag = &result.errors[0];
    assert_eq!(diag.message, EMBEDDED_NUL_STRING_MESSAGE);
    let start = source.find("r\"").unwrap();
    assert_eq!(diag.span, start..start + 6);
    assert!(matches!(diag.kind, ParseDiagnosticKind::InvalidLiteral));
}

#[test]
fn parse_interpolated_string_literal_rejects_raw_embedded_nul() {
    let source = format!("fn main() {{ let s = f\"a{}b\"; }}", '\0');
    let result = parse(&source);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
    let diag = &result.errors[0];
    assert_eq!(diag.message, EMBEDDED_NUL_STRING_MESSAGE);
    let start = source.find("f\"").unwrap();
    assert_eq!(diag.span, start..start + 6);
    assert!(matches!(diag.kind, ParseDiagnosticKind::InvalidLiteral));
}

#[test]
fn parse_interpolated_string_literal_rejects_embedded_nul_escape() {
    let source = r#"fn main() { let s = f"a\0{name}"; }"#;
    let result = parse(source);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
    let diag = &result.errors[0];
    assert_eq!(diag.message, EMBEDDED_NUL_STRING_MESSAGE);
    let start = source.find("f\"").unwrap();
    assert_eq!(
        diag.span,
        start..source[start..].find("\";").unwrap() + start + 1
    );
    assert!(matches!(diag.kind, ParseDiagnosticKind::InvalidLiteral));
}

#[test]
fn parse_interpolated_string_contains_expr_part() {
    let result = parse(r#"fn main() { let s = f"hello {name}"; }"#);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(f) = &result.program.items[0].0 else {
        panic!("expected function");
    };
    let Stmt::Let {
        value: Some((Expr::InterpolatedString(parts), _)),
        ..
    } = &f.body.stmts[0].0
    else {
        panic!("expected interpolated string");
    };
    assert!(parts.iter().any(|p| matches!(p, StringPart::Expr(_))));
}

#[test]
fn parse_interpolated_string_with_nested_string_literal() {
    let result = parse(r#"fn main() { let s = f"x={func("a")}"; }"#);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(f) = &result.program.items[0].0 else {
        panic!("expected function");
    };
    let Stmt::Let {
        value: Some((Expr::InterpolatedString(parts), _)),
        ..
    } = &f.body.stmts[0].0
    else {
        panic!("expected interpolated string");
    };
    assert!(
        matches!(parts.as_slice(), [StringPart::Literal(prefix), StringPart::Expr(_)] if prefix == "x=")
    );
}

#[test]
fn parse_interpolated_string_shared_escapes_decode_and_escaped_delimiters_stay_literal() {
    let result = parse(r#"fn main() { let s = f"left \{ \} \x41 {name}"; }"#);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(f) = &result.program.items[0].0 else {
        panic!("expected function");
    };
    let Stmt::Let {
        value: Some((Expr::InterpolatedString(parts), _)),
        ..
    } = &f.body.stmts[0].0
    else {
        panic!("expected interpolated string");
    };

    assert_eq!(parts.len(), 2);
    assert_eq!(parts[0], StringPart::Literal("left { } A ".to_string()));
    assert!(matches!(
        parts[1],
        StringPart::Expr((Expr::Identifier(ref name), _)) if name == "name"
    ));
}

#[test]
fn parse_interpolated_string_empty_expr_reports_error() {
    let result = parse(r#"fn main() { let s = f"hello {}"; }"#);
    assert!(
        !result.errors.is_empty(),
        "expected parse errors for malformed interpolation"
    );
}

#[test]
fn parse_deeply_nested_expr_produces_error() {
    // 300 levels of parenthesized nesting exceeds MAX_DEPTH (256).
    // Use a child thread with an explicit stack size to avoid the test
    // runner's own stack limit being hit before our guard triggers.
    let result = std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(|| {
            let open: String = "(".repeat(300);
            let close: String = ")".repeat(300);
            let source = format!("fn main() -> i32 {{ {open}1{close} }}");
            parse(&source)
        })
        .expect("failed to spawn thread")
        .join()
        .expect("thread panicked");

    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("maximum nesting depth exceeded")),
        "expected nesting depth error, got: {:?}",
        result.errors
    );
}

#[test]
fn parse_error_missing_brace() {
    let source = "fn main() -> i32 {\n    let x = 42;\n    0\n";
    let result = parse(source);
    assert!(
        result.errors.iter().any(|e| e.message.contains("`}`")),
        "expected `}}` error, got: {:?}",
        result.errors
    );
}

#[test]
fn parse_error_unexpected_token() {
    let source = "fn main() {\n    let x = 42 + + + ;\n}";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse errors for unexpected tokens"
    );
}

#[test]
fn parse_error_unclosed_string() {
    let source = "fn main() {\n    println(\"hello world);\n}";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse errors for unclosed string"
    );
}

#[test]
fn parse_error_missing_expr() {
    let source = "fn main() {\n    let x = ;\n}";
    let result = parse(source);
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("expected expression")),
        "expected 'expected expression' error, got: {:?}",
        result.errors
    );
}

#[test]
fn parse_error_missing_semicolon() {
    let source = "fn main() {\n    let x = 42\n    let y = 10;\n}";
    let result = parse(source);
    assert!(
        result.errors.iter().any(|e| e.message.contains("expected")),
        "expected error about missing semicolon, got: {:?}",
        result.errors
    );
}

// -----------------------------------------------------------------------
// Edge case tests: nested generics, chained methods, complex expressions
// -----------------------------------------------------------------------

#[test]
fn parse_nested_generic_types() {
    let source = "fn main() { let v: Vec<Vec<i32>> = Vec::new(); }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_deeply_nested_generics() {
    let source = "fn main() { let v: HashMap<string, Vec<Vec<i32>>> = HashMap::new(); }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_chained_method_calls() {
    let source = "fn main() { a.b().c().d(); }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_chained_methods_with_args() {
    let source = "fn main() { x.filter(1).map(2).collect(); }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_operator_precedence_complex() {
    let source = "fn main() -> i32 { x + y * z - w / v }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    // Verify the trailing expression exists (the complex expression)
    if let Item::Function(f) = &result.program.items[0].0 {
        assert!(f.body.trailing_expr.is_some());
    } else {
        panic!("expected function");
    }
}

#[test]
fn parse_mixed_precedence_with_parens() {
    let source = "fn main() -> i32 { (a + b) * (c - d) / e }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_empty_function_body() {
    let source = "fn noop() {}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Function(f) = &result.program.items[0].0 {
        assert!(f.body.stmts.is_empty());
        assert!(f.body.trailing_expr.is_none());
    } else {
        panic!("expected function");
    }
}

#[test]
fn parse_empty_actor_body() {
    let source = "actor Empty {}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Actor(a) = &result.program.items[0].0 {
        assert!(a.fields.is_empty());
        assert!(a.receive_fns.is_empty());
    } else {
        panic!("expected actor");
    }
}

#[test]
fn parse_actor_lifecycle_hook_and_receive_attributes() {
    // The `terminate { }` block surface was removed in favour of the
    // annotation-based hook surface; cleanup logic is expressed as a
    // plain `fn` annotated with `#[on(stop)]` (and `#[on(start)]` for
    // startup logic).
    let source = r"actor Worker {
    #[on(stop)]
    fn shutdown() { stop(); }

    #[every(50ms)]
    receive fn tick() { work(); }
}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Actor(actor) = &result.program.items[0].0 {
        assert_eq!(actor.methods.len(), 1);
        assert_eq!(actor.methods[0].attributes.len(), 1);
        assert_eq!(actor.methods[0].attributes[0].name, "on");

        assert_eq!(actor.receive_fns.len(), 1);
        assert_eq!(actor.receive_fns[0].attributes.len(), 1);
        assert_eq!(actor.receive_fns[0].attributes[0].name, "every");
    } else {
        panic!("expected actor");
    }
}

#[test]
fn parse_actor_on_crash_hook_attaches_to_method() {
    // E1: `#[on(crash)]` parses on an actor method. Signature shape
    // (params/return type) is owned by E2 — parser just attaches
    // the attribute to the FnDecl.
    let source = r"actor Worker {
    #[on(crash)]
    fn on_crash() { }
}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Actor(actor) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    assert_eq!(actor.methods.len(), 1);
    assert_eq!(actor.methods[0].attributes.len(), 1);
    assert_eq!(actor.methods[0].attributes[0].name, "on");
    assert_eq!(actor.methods[0].attributes[0].args.len(), 1);
    assert_eq!(actor.methods[0].attributes[0].args[0].as_str(), "crash");
}

#[test]
fn parse_actor_on_upgrade_hook_attaches_to_method() {
    // E1: `#[on(upgrade)]` parses on an actor method. The parser accepts
    // it; the type-checker rejects it as a reserved, unsupported attribute.
    let source = r"actor Worker {
    #[on(upgrade)]
    fn on_upgrade() { }
}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Actor(actor) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    assert_eq!(actor.methods.len(), 1);
    assert_eq!(actor.methods[0].attributes.len(), 1);
    assert_eq!(actor.methods[0].attributes[0].name, "on");
    assert_eq!(actor.methods[0].attributes[0].args.len(), 1);
    assert_eq!(actor.methods[0].attributes[0].args[0].as_str(), "upgrade");
}

#[test]
fn parse_attribute_key_value_missing_value_emits_error_without_empty_fallback() {
    let source = r"
#[meta(rename = , version = 2)]
fn demo() {}
";
    let result = parse(source);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
    assert_eq!(
        result.errors[0].message,
        "invalid value for attribute `rename`: missing value"
    );
    assert_eq!(
        result.errors[0].hint.as_deref(),
        Some("expected identifier, string literal, or integer literal")
    );

    let Item::Function(func) = &result.program.items[0].0 else {
        panic!("expected function");
    };
    assert_eq!(func.attributes.len(), 1);
    assert_eq!(func.attributes[0].name, "meta");
    assert_eq!(func.attributes[0].args.len(), 1);
    assert!(matches!(
        &func.attributes[0].args[0],
        AttributeArg::KeyValue { key, value } if key == "version" && value == "2"
    ));
}

#[test]
fn parse_unicode_in_string_literal() {
    let source = r#"fn main() { let s = "Hello, 世界! 🦀"; }"#;
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Function(f) = &result.program.items[0].0 {
        if let (
            Stmt::Let {
                value: Some(val), ..
            },
            _,
        ) = &f.body.stmts[0]
        {
            if let (Expr::Literal(Literal::String(s)), _) = val {
                assert!(s.contains("世界"));
                assert!(s.contains("🦀"));
            } else {
                panic!("expected string literal");
            }
        }
    }
}

#[test]
fn parse_empty_string_literal() {
    let source = r#"fn main() { let s = ""; }"#;
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_large_integer_literal() {
    let source = "fn main() -> i64 { 9_223_372_036_854_775_807 }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Function(f) = &result.program.items[0].0 {
        if let Some(boxed) = &f.body.trailing_expr {
            if let (Expr::Literal(Literal::Integer { value: n, .. }), _) = boxed.as_ref() {
                assert_eq!(*n, i64::MAX);
            } else {
                panic!("expected integer literal");
            }
        }
    }
}

#[test]
fn parse_hex_integer_literal() {
    let source = "fn main() -> i64 { 0xFF }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Function(f) = &result.program.items[0].0 {
        if let Some(boxed) = &f.body.trailing_expr {
            if let (Expr::Literal(Literal::Integer { value: n, radix }), _) = boxed.as_ref() {
                assert_eq!(*n, 255);
                assert_eq!(*radix, IntRadix::Hex);
            } else {
                panic!("expected integer literal");
            }
        }
    }
}

#[test]
fn parse_binary_integer_literal() {
    let source = "fn main() -> i64 { 0b1010 }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Function(f) = &result.program.items[0].0 {
        if let Some(boxed) = &f.body.trailing_expr {
            if let (Expr::Literal(Literal::Integer { value: n, radix }), _) = boxed.as_ref() {
                assert_eq!(*n, 10);
                assert_eq!(*radix, IntRadix::Binary);
            } else {
                panic!("expected integer literal");
            }
        }
    }
}

#[test]
fn parse_octal_integer_literal() {
    let source = "fn main() -> i64 { 0o77 }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Function(f) = &result.program.items[0].0 {
        if let Some(boxed) = &f.body.trailing_expr {
            if let (Expr::Literal(Literal::Integer { value: n, radix }), _) = boxed.as_ref() {
                assert_eq!(*n, 63);
                assert_eq!(*radix, IntRadix::Octal);
            } else {
                panic!("expected integer literal");
            }
        }
    }
}

#[test]
fn parse_multiple_items() {
    let source = "fn foo() {} fn bar() {} type Baz {}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    assert_eq!(result.program.items.len(), 3);
}

#[test]
fn parse_empty_program() {
    let result = parse("");
    assert!(result.errors.is_empty());
    assert!(result.program.items.is_empty());
}

#[test]
fn parse_nested_if_else() {
    let source = "fn main() -> i32 { if a > 0 { if b > 0 { 1 } else { 2 } } else { 3 } }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_match_with_struct_pattern() {
    let source = "fn main() { match p { Point { x, y } => x + y, _ => 0, } }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_tuple_expression() {
    let source = "fn main() { let t = (1, 2, 3); }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_array_expression() {
    let source = "fn main() { let a = [1, 2, 3]; }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_unary_operators() {
    let source = "fn main() { let a = -x; let b = !flag; }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

/// Extract the initializer expression of the first `let` in the first
/// function body. Panics if the shape does not match — tests want a loud
/// failure when the AST drifts.
fn first_let_value(result: &ParseResult) -> &Expr {
    let Item::Function(f) = &result.program.items[0].0 else {
        panic!("expected first item to be a function");
    };
    let Stmt::Let { value, .. } = &f.body.stmts[0].0 else {
        panic!("expected first statement to be a `let`");
    };
    &value.as_ref().expect("let initializer present").0
}

#[test]
fn parse_clone_prefix_expression() {
    let source = "fn main() { let a = clone x; }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    match first_let_value(&result) {
        Expr::Clone(operand) => {
            assert!(
                matches!(&operand.0, Expr::Identifier(name) if name == "x"),
                "clone operand should be identifier `x`, got: {:?}",
                operand.0
            );
        }
        other => panic!("expected Expr::Clone, got: {other:?}"),
    }
}

#[test]
fn parse_clone_prefix_takes_whole_postfix_chain() {
    // `clone x.field` must clone the field access, not `(clone x).field`.
    let source = "fn main() { let a = clone x.field; }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    match first_let_value(&result) {
        Expr::Clone(operand) => assert!(
            matches!(&operand.0, Expr::FieldAccess { field, .. } if field == "field"),
            "clone operand should be the field access, got: {:?}",
            operand.0
        ),
        other => panic!("expected Expr::Clone wrapping a field access, got: {other:?}"),
    }
}

#[test]
fn parse_clone_prefix_binds_below_binary() {
    // `clone x + y` is `(clone x) + y`, matching other unary prefixes.
    let source = "fn main() { let a = clone x + y; }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    match first_let_value(&result) {
        Expr::Binary { left, op, .. } => {
            assert_eq!(*op, BinaryOp::Add);
            assert!(
                matches!(&left.0, Expr::Clone(_)),
                "left of `+` should be the clone, got: {:?}",
                left.0
            );
        }
        other => panic!("expected Expr::Binary with a clone on the left, got: {other:?}"),
    }
}

#[test]
fn parse_clone_call_is_not_a_prefix() {
    // `clone(x)` stays a call to a function named `clone`; the contextual
    // prefix only triggers when an operand token (not `(`) follows.
    let source = "fn main() { let a = clone(x); }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    match first_let_value(&result) {
        Expr::Call { function, .. } => assert!(
            matches!(&function.0, Expr::Identifier(name) if name == "clone"),
            "expected a call to `clone`, got: {:?}",
            function.0
        ),
        other => panic!("expected Expr::Call, got: {other:?}"),
    }
}

#[test]
fn parse_clone_as_identifier_still_works() {
    // `clone` is not a reserved word: usable as a binding and in operator
    // position when not followed by an operand token.
    let source = "fn main() { let clone = 5; let y = clone + 1; }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_ampersand_prefix_is_rejected() {
    let source = "fn main() { let y = &x; }";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected a parse error for prefix `&`"
    );
    let err = &result.errors[0];
    assert!(
        err.message.contains("not a prefix operator"),
        "message should explain `&` is not a prefix operator, got: {}",
        err.message
    );
    assert!(
        err.hint.as_deref().is_some_and(|h| h.contains("clone")),
        "hint should point at `clone`, got: {:?}",
        err.hint
    );
}

#[test]
fn parse_comparison_chain() {
    let source = "fn main() -> bool { a < b && b > c || d == e && f != g }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_import_statement() {
    let source = "import std::fs;";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        assert_eq!(imp.path, vec!["std", "fs"]);
    } else {
        panic!("expected import");
    }
}

#[test]
fn parse_import_actor_path_segment() {
    let source = "import std::actor::monitor;";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        assert_eq!(imp.path, vec!["std", "actor", "monitor"]);
    } else {
        panic!("expected import");
    }
}

#[test]
fn parse_trait_declaration() {
    let source = "trait Printable { fn print(self); }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_bare_self_in_free_fn_is_error() {
    let source = "fn print(self) {}";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for bare `self` in free function"
    );
    assert!(
        result.errors[0]
            .message
            .contains("not a valid parameter name"),
        "error message should mention self is not a valid parameter name, got: {}",
        result.errors[0].message,
    );
}

#[test]
fn parse_typed_self_is_error() {
    let source = "type Foo { x: int } impl Foo { fn bar(self: Foo) -> int { 0 } }";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `self: Type` parameter"
    );
    assert!(
        result.errors[0]
            .message
            .contains("not a valid parameter name"),
        "error message should mention self is not a valid parameter name, got: {}",
        result.errors[0].message,
    );
}

#[test]
fn parse_impl_method_bare_self_receiver() {
    let source = "type Foo { x: int } impl Foo { fn bar(self) -> int { self.x } }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_error_duplicate_keyword() {
    let source = "fn fn main() {}";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for duplicate fn keyword"
    );
}

// Duration literal parsing
#[test]
fn parse_duration_literals() {
    let source = "fn main() { let a = 100ms; let b = 5s; let c = 1m; let d = 2h; }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Function(f) = &result.program.items[0].0 {
        let stmts = &f.body.stmts;
        assert_eq!(stmts.len(), 4);
        // 100ms → 100_000_000
        if let Stmt::Let {
            value: Some((Expr::Literal(Literal::Duration(ns)), _)),
            ..
        } = &stmts[0].0
        {
            assert_eq!(*ns, 100_000_000);
        } else {
            panic!("expected Duration literal for 100ms");
        }
        // 5s → 5_000_000_000
        if let Stmt::Let {
            value: Some((Expr::Literal(Literal::Duration(ns)), _)),
            ..
        } = &stmts[1].0
        {
            assert_eq!(*ns, 5_000_000_000);
        } else {
            panic!("expected Duration literal for 5s");
        }
        // 1m → 60_000_000_000
        if let Stmt::Let {
            value: Some((Expr::Literal(Literal::Duration(ns)), _)),
            ..
        } = &stmts[2].0
        {
            assert_eq!(*ns, 60_000_000_000);
        } else {
            panic!("expected Duration literal for 1m");
        }
        // 2h → 7_200_000_000_000
        if let Stmt::Let {
            value: Some((Expr::Literal(Literal::Duration(ns)), _)),
            ..
        } = &stmts[3].0
        {
            assert_eq!(*ns, 7_200_000_000_000);
        } else {
            panic!("expected Duration literal for 2h");
        }
    } else {
        panic!("expected function");
    }
}

#[test]
fn parse_foreign_block() {
    // `foreign` is no longer accepted — only `extern` is valid
    let source = "foreign { fn ext_add(a: i32, b: i32) -> i32; fn ext_print(msg: string); }";
    let result = parse(source);
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("unexpected 'foreign'")
                && e.hint
                    .as_deref()
                    .is_some_and(|h| h.contains("use 'extern'"))),
        "expected foreign rejection error, got: {:?}",
        result.errors
    );

    // Verify that `extern` still works for the same purpose
    let source = "extern \"C\" { fn ext_add(a: i32, b: i32) -> i32; fn ext_print(msg: string); }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    assert_eq!(result.program.items.len(), 1);
}

// ── extern "rt" block tests ──────────────────────────────────────────

#[test]
fn extern_rt_block_empty() {
    let result = parse("extern \"rt\" { }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    assert_eq!(result.program.items.len(), 1);
    let Item::ExternBlock(block) = &result.program.items[0].0 else {
        panic!("expected ExternBlock");
    };
    assert_eq!(block.abi, "rt");
    assert!(block.functions.is_empty());
}

#[test]
fn extern_rt_block_single_fn() {
    let result = parse("extern \"rt\" { fn println(s: string); }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    assert_eq!(result.program.items.len(), 1);
    let Item::ExternBlock(block) = &result.program.items[0].0 else {
        panic!("expected ExternBlock");
    };
    assert_eq!(block.abi, "rt");
    assert_eq!(block.functions.len(), 1);
    assert_eq!(block.functions[0].name, "println");
    assert_eq!(block.functions[0].params.len(), 1);
    assert_eq!(block.functions[0].params[0].name, "s");
}

#[test]
fn extern_rt_block_multiple_fns() {
    let result = parse(
        "extern \"rt\" { fn println(s: string); fn print(s: string); fn assert(cond: bool); }",
    );
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::ExternBlock(block) = &result.program.items[0].0 else {
        panic!("expected ExternBlock");
    };
    assert_eq!(block.abi, "rt");
    assert_eq!(block.functions.len(), 3);
    assert_eq!(block.functions[0].name, "println");
    assert_eq!(block.functions[1].name, "print");
    assert_eq!(block.functions[2].name, "assert");
}

#[test]
fn extern_rt_block_fn_with_body_rejected() {
    // Bodies are forbidden in extern blocks (any ABI): the parser expects `;`
    // after the parameter list. A `{` in its place produces a parse error.
    let result = parse("extern \"rt\" { fn println(s: string) { todo() } }");
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("expected `;`")),
        "expected a 'expected `;`' error, got: {:?}",
        result.errors
    );
}

#[test]
fn extern_unknown_abi_accepted_at_parse_level() {
    // The parser is ABI-agnostic; unknown ABI strings parse successfully.
    // Rejection of unsupported ABIs is deferred to the type-checker.
    let result = parse("extern \"xyz\" { fn foo(); }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::ExternBlock(block) = &result.program.items[0].0 else {
        panic!("expected ExternBlock");
    };
    assert_eq!(block.abi, "xyz");
    assert_eq!(block.functions.len(), 1);
    assert_eq!(block.functions[0].name, "foo");
}

#[test]
fn parse_timeout_combinator() {
    let source = "fn main() { let r = foo() | after 5000; }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    // Check the expression is Timeout wrapping a call
    let stmt = &result.program.items[0];
    if let Item::Function(f) = &stmt.0 {
        if let (
            Stmt::Let {
                value: Some(val), ..
            },
            _,
        ) = &f.body.stmts[0]
        {
            assert!(
                matches!(val.0, Expr::Timeout { .. }),
                "expected Timeout, got {:?}",
                val.0
            );
        } else {
            panic!("expected let binding");
        }
    } else {
        panic!("expected FnDecl");
    }
}
// -- Import aliasing --

#[test]
fn parse_import_alias() {
    let source = r"import std::net::{http as h, websocket as ws};";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        assert_eq!(imp.path, vec!["std", "net"]);
        if let Some(ImportSpec::Names(names)) = &imp.spec {
            assert_eq!(names.len(), 2);
            assert_eq!(names[0].name, "http");
            assert_eq!(names[0].alias.as_deref(), Some("h"));
            assert_eq!(names[1].name, "websocket");
            assert_eq!(names[1].alias.as_deref(), Some("ws"));
        } else {
            panic!("expected ImportSpec::Names, got {:?}", imp.spec);
        }
    } else {
        panic!("expected import item");
    }
}

#[test]
fn parse_import_alias_single() {
    let source = r"import mymod::{foo as bar};";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        if let Some(ImportSpec::Names(names)) = &imp.spec {
            assert_eq!(names.len(), 1);
            assert_eq!(names[0].name, "foo");
            assert_eq!(names[0].alias.as_deref(), Some("bar"));
        } else {
            panic!("expected ImportSpec::Names");
        }
    } else {
        panic!("expected import item");
    }
}

#[test]
fn parse_import_whole_module_alias() {
    // `import path::to::mod as alias;` sets module_alias; spec stays None.
    let source = r"import std::net as n;";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        assert_eq!(imp.path, vec!["std", "net"]);
        assert!(imp.spec.is_none(), "whole-module alias keeps spec None");
        assert_eq!(imp.module_alias.as_deref(), Some("n"));
    } else {
        panic!("expected import item");
    }
}

#[test]
fn parse_import_no_module_alias_is_none() {
    // A plain whole-module import leaves module_alias unset.
    let source = r"import std::net;";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        assert!(imp.module_alias.is_none());
    } else {
        panic!("expected import item");
    }
}

#[test]
fn parse_import_alias_of_brace_rejected() {
    // Aliasing a `::{ }` spec has no meaning and is a parse error.
    let source = r"import mymod::{ Foo } as f;";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for aliasing a brace import"
    );
}

#[test]
fn parse_import_alias_of_glob_rejected() {
    // Aliasing a glob has no meaning and is a parse error.
    let source = r"import mymod::* as g;";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for aliasing a glob import"
    );
}

#[test]
fn parse_import_no_alias_preserves_name() {
    // Names without `as` should have alias = None
    let source = r"import mymod::{foo, bar};";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        if let Some(ImportSpec::Names(names)) = &imp.spec {
            assert_eq!(names.len(), 2);
            assert_eq!(names[0].name, "foo");
            assert!(names[0].alias.is_none());
            assert_eq!(names[1].name, "bar");
            assert!(names[1].alias.is_none());
        } else {
            panic!("expected ImportSpec::Names");
        }
    } else {
        panic!("expected import item");
    }
}

#[test]
fn parse_import_bare_colons_rejected() {
    // `import foo::;` is syntactically invalid — `::` must be followed by `*` or `{`
    let source = r"import foo::;";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `import foo::;`"
    );
}

#[test]
fn parse_import_glob() {
    let source = r"import utils::*;";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        assert_eq!(imp.spec, Some(ImportSpec::Glob));
    } else {
        panic!("expected import item");
    }
}

#[test]
fn parse_float_with_underscore_separators() {
    let source = "fn main() { let x = 1_000.5; }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_raw_string_literal() {
    let source = r#"fn main() { let x = r"hello\nworld"; }"#;
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}
#[test]
fn test_hex_escape_in_string() {
    // \x41 = 'A', \x42 = 'B'
    assert_eq!(unescape_string(r"\x41\x42").0, "AB");
    // Mixed with normal text and other escapes
    assert_eq!(unescape_string(r"hi\x21\n").0, "hi!\n");
    // Invalid hex digits preserved as-is
    assert_eq!(unescape_string(r"\xZZ").0, "\\xZZ");
    // Truncated hex escape (only one char after \x)
    assert_eq!(unescape_string("\\x4").0, "\\x4");
}

#[test]
fn parse_visibility_modifiers() {
    use crate::ast::Visibility;

    // pub fn → Visibility::Pub
    let r = parse("pub fn foo() {}");
    assert!(r.errors.is_empty(), "errors: {:?}", r.errors);
    if let Item::Function(f) = &r.program.items[0].0 {
        assert_eq!(f.visibility, Visibility::Pub);
    } else {
        panic!("expected function");
    }

    // package fn → Visibility::Package
    let r = parse("package fn bar() {}");
    assert!(r.errors.is_empty(), "errors: {:?}", r.errors);
    if let Item::Function(f) = &r.program.items[0].0 {
        assert_eq!(f.visibility, Visibility::Package);
    } else {
        panic!("expected function");
    }

    // fn (no modifier) → Visibility::Private
    let r = parse("fn private() {}");
    assert!(r.errors.is_empty(), "errors: {:?}", r.errors);
    if let Item::Function(f) = &r.program.items[0].0 {
        assert_eq!(f.visibility, Visibility::Private);
    } else {
        panic!("expected function");
    }

    // package type → Visibility::Package
    let r = parse("package type Point { x: i32; y: i32 }");
    assert!(r.errors.is_empty(), "errors: {:?}", r.errors);
    if let Item::TypeDecl(t) = &r.program.items[0].0 {
        assert_eq!(t.visibility, Visibility::Package);
    } else {
        panic!("expected type decl");
    }

    // package const → Visibility::Package
    let r = parse("package const X: i32 = 1;");
    assert!(r.errors.is_empty(), "errors: {:?}", r.errors);
    if let Item::Const(c) = &r.program.items[0].0 {
        assert_eq!(c.visibility, Visibility::Package);
    } else {
        panic!("expected const decl");
    }

    // old pub(package) syntax is no longer accepted
    let r = parse("pub(package) fn old() {}");
    assert!(
        !r.errors.is_empty(),
        "pub(package) should no longer parse cleanly"
    );

    // old pub(super) syntax is no longer accepted
    let r = parse("pub(super) fn old() {}");
    assert!(
        !r.errors.is_empty(),
        "pub(super) should no longer parse cleanly"
    );
}
#[test]
fn parse_generic_lambda_removed_emits_typed_diagnostic() {
    // Generic lambda `<T>(params) => body` was removed in v0.5.
    // The parser must emit a typed E_CLOSURE_PIPE_SYNTAX diagnostic,
    // not silently accept or produce a cryptic error.
    for source in [
        "fn main() { let id = <T>(x: T) => x; }",
        "fn main() { let add = <T: Add>(x: T, y: T) => x + y; }",
        "fn main() { let id = <T>(x: T) -> T => x; }",
    ] {
        let result = parse(source);
        assert!(
            result
                .errors
                .iter()
                .any(|e| matches!(e.kind, ParseDiagnosticKind::ClosurePipeSyntax)
                    && e.message.contains("E_CLOSURE_PIPE_SYNTAX")),
            "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda: {source}\ngot: {:?}",
            result.errors
        );
    }
}

#[test]
fn parse_paren_lambda_removed_emits_typed_diagnostic() {
    // Parenthesized `(params) => body` was removed in v0.5.
    // The parser must emit a typed E_CLOSURE_PIPE_SYNTAX diagnostic.
    for source in [
        "fn main() { let f = (x) => x; }",
        "fn main() { let f = (x: i32) => x; }",
        "fn main() { let f = (x: i32) -> i32 => x; }",
        "fn main() { let f = move (x) => x; }",
    ] {
        let result = parse(source);
        assert!(
            result
                .errors
                .iter()
                .any(|e| matches!(e.kind, ParseDiagnosticKind::ClosurePipeSyntax)
                    && e.message.contains("E_CLOSURE_PIPE_SYNTAX")),
            "expected typed E_CLOSURE_PIPE_SYNTAX for removed paren lambda: {source}\ngot: {:?}",
            result.errors
        );
    }
}

#[test]
fn parse_gen_block_expression() {
    // `gen { yield ...; }` in expression position.
    let source = "fn main() { let g = gen { yield 1; yield 2; }; }";
    let result = parse(source);
    assert!(
        result.errors.is_empty(),
        "expected gen block to parse cleanly: {source}\nerrors: {:?}",
        result.errors
    );
    if let Item::Function(f) = &result.program.items[0].0 {
        if let Stmt::Let {
            value: Some((Expr::GenBlock { .. }, _)),
            ..
        } = &f.body.stmts[0].0
        {
            // Correct: let binding holds a GenBlock
        } else {
            panic!("expected let with GenBlock, got: {:?}", f.body.stmts[0].0);
        }
    } else {
        panic!("expected function item");
    }
}

#[test]
fn parse_gen_block_empty_body() {
    // Empty gen block is syntactically valid (checker will reject it without item type).
    let source = "fn main() { let g = gen {}; }";
    let result = parse(source);
    assert!(
        result.errors.is_empty(),
        "expected empty gen block to parse cleanly: {source}\nerrors: {:?}",
        result.errors
    );
}

#[test]
fn parse_gen_without_block_emits_diagnostic() {
    // `gen` without a following brace must emit a diagnostic.
    let source = "fn main() { let g = gen 42; }";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected error for `gen` without block"
    );
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("E_GEN_BLOCK_SYNTAX")),
        "expected E_GEN_BLOCK_SYNTAX diagnostic, got: {:?}",
        result.errors
    );
}

#[test]
fn parse_wire_struct_preserves_since_modifier() {
    let source = "\
#[wire]
type Msg {
    added: String @2 optional since 2 json(\"added\"),
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    let wire = decl.wire.as_ref().expect("expected wire metadata");
    let meta = &wire.field_meta[0];
    assert_eq!(meta.field_number, 2);
    assert!(meta.is_optional);
    assert_eq!(meta.json_name.as_deref(), Some("added"));
    assert_eq!(meta.since, Some(2));
}

#[test]
fn wire_attr_on_type_declaration_produces_wire_metadata() {
    let source = "\
#[wire]
type Point {
    x: i64;
    y: i64;
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    assert_eq!(decl.kind, TypeDeclKind::Struct);
    let wire = decl.wire.as_ref().expect("expected wire metadata");
    assert_eq!(wire.field_meta.len(), 2);
    assert_eq!(wire.field_meta[0].field_name, "x");
    assert_eq!(wire.field_meta[0].field_number, 1);
    assert_eq!(wire.field_meta[1].field_name, "y");
    assert_eq!(wire.field_meta[1].field_number, 2);
}

#[test]
fn wire_struct_field_metadata_preserves_number_and_outer_naming_cases() {
    let source = "\
#[wire]
#[json(\"camelCase\")]
#[yaml(\"snake_case\")]
type Msg {
    added: String @2 optional yaml(\"added_name\"),
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    let wire = decl.wire.as_ref().expect("expected wire metadata");
    assert_eq!(wire.json_case, Some(NamingCase::CamelCase));
    assert_eq!(wire.yaml_case, Some(NamingCase::SnakeCase));

    let meta = &wire.field_meta[0];
    assert_eq!(meta.field_number, 2);
    assert!(meta.is_optional);
    assert_eq!(meta.yaml_name.as_deref(), Some("added_name"));
}

/// `#[wire] enum E { A; B; C; }` — unit-only variants attach wire metadata
/// at the type level (version / naming-cases) and produce
/// `Item::TypeDecl { kind: Enum, wire: Some(_) }`.  Variants carry no
/// per-field tag numbers (variants are tagged by index, not `@N`).
#[test]
fn parse_wire_enum_unit_variants() {
    let source = "\
#[wire]
enum Command {
    Start;
    Stop;
    Pause;
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    assert_eq!(decl.kind, TypeDeclKind::Enum);
    assert_eq!(decl.name, "Command");
    let wire = decl.wire.as_ref().expect("expected wire metadata on enum");
    assert!(wire.field_meta.is_empty(), "enum variants have no @N tags");
    assert!(wire.reserved_numbers.is_empty());
    assert_eq!(decl.body.len(), 3);
    for item in &decl.body {
        match item {
            TypeBodyItem::Variant(v) => assert!(matches!(v.kind, VariantKind::Unit)),
            _ => panic!("expected variant, got {item:?}"),
        }
    }
}

/// `#[wire] enum E { V1 { x: i64 }; V2 { y: String } }` — struct-payload
/// variants parse through the shared enum-body helper; wire metadata at
/// the type level is attached by `parse_wire_enum`.
#[test]
fn parse_wire_enum_struct_payload_variants() {
    let source = "\
#[wire(version = 2, min_version = 1)]
enum Packet {
    V1 { x: i64 };
    V2 { y: String, z: bool };
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    assert_eq!(decl.kind, TypeDeclKind::Enum);
    let wire = decl.wire.as_ref().expect("expected wire metadata on enum");
    assert_eq!(wire.version, Some(2));
    assert_eq!(wire.min_version, Some(1));
    assert_eq!(decl.body.len(), 2);
    let TypeBodyItem::Variant(v1) = &decl.body[0] else {
        panic!("expected variant V1");
    };
    assert_eq!(v1.name, "V1");
    assert!(matches!(v1.kind, VariantKind::Struct(ref fs) if fs.len() == 1));
    let TypeBodyItem::Variant(v2) = &decl.body[1] else {
        panic!("expected variant V2");
    };
    assert_eq!(v2.name, "V2");
    assert!(matches!(v2.kind, VariantKind::Struct(ref fs) if fs.len() == 2));
}

/// `#[wire] enum E { A(i64); B(String, bool) }` — tuple-payload variants.
#[test]
fn parse_wire_enum_tuple_payload_variants() {
    let source = "\
#[wire]
enum Op {
    Push(i64);
    Pair(String, bool);
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    assert_eq!(decl.kind, TypeDeclKind::Enum);
    let wire = decl.wire.as_ref().expect("expected wire metadata on enum");
    assert!(wire.field_meta.is_empty());
    assert_eq!(decl.body.len(), 2);
    let TypeBodyItem::Variant(push) = &decl.body[0] else {
        panic!("expected variant Push");
    };
    assert!(matches!(push.kind, VariantKind::Tuple(ref ts) if ts.len() == 1));
    let TypeBodyItem::Variant(pair) = &decl.body[1] else {
        panic!("expected variant Pair");
    };
    assert!(matches!(pair.kind, VariantKind::Tuple(ref ts) if ts.len() == 2));
}

/// `#[wire]` is exclusive with `#[resource]` / `#[linear]` on enums (same
/// rule as `#[wire] type`).
#[test]
fn parse_wire_enum_rejects_resource_marker_combo() {
    let source = "\
#[wire]
#[resource]
enum Bad {
    A;
}
";
    let result = parse(source);
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("E_TYPE_MARKER_CONFLICT")),
        "expected E_TYPE_MARKER_CONFLICT, got {:?}",
        result.errors
    );
}

/// `#[wire] #[json("camelCase")] #[yaml("kebab-case")] enum E { … }` —
/// type-level naming attributes flow into `WireMetadata`.
#[test]
fn parse_wire_enum_preserves_naming_cases() {
    let source = "\
#[wire]
#[json(\"camelCase\")]
#[yaml(\"kebab-case\")]
enum Command {
    Start;
    Stop;
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    let wire = decl.wire.as_ref().expect("expected wire metadata on enum");
    assert_eq!(wire.json_case, Some(NamingCase::CamelCase));
    assert_eq!(wire.yaml_case, Some(NamingCase::KebabCase));
}

/// `pub #[wire] enum E { ... }` — the visibility-prefixed dispatch arm
/// (parser.rs `:1491`-area, inside the `Some(Token::Pub)` branch) must
/// route through `parse_wire_enum` and preserve `Visibility::Pub` on the
/// resulting `TypeDecl`.  Exercises the pub-prefixed arm specifically
/// (the existing wire-enum tests all hit the bare arm at `:1581`).
#[test]
fn parses_visibility_prefixed_wire_enum() {
    let source = "\
#[wire]
pub enum Command {
    Start;
    Stop;
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    assert_eq!(decl.kind, TypeDeclKind::Enum);
    assert_eq!(decl.name, "Command");
    assert_eq!(decl.visibility, Visibility::Pub);
    let wire = decl
        .wire
        .as_ref()
        .expect("expected wire metadata on pub enum");
    assert!(wire.field_meta.is_empty());
    assert!(wire.reserved_numbers.is_empty());
    assert_eq!(decl.body.len(), 2);
    for item in &decl.body {
        match item {
            TypeBodyItem::Variant(v) => assert!(matches!(v.kind, VariantKind::Unit)),
            _ => panic!("expected variant, got {item:?}"),
        }
    }
}

/// `#[wire] enum X { A; B(i64); C { x: String, y: i32 } }` — a single
/// enum body mixing unit, tuple, and struct variant payloads.  Verifies
/// each variant kind parses correctly, source order is preserved, and
/// type-level wire metadata is attached.
#[test]
fn parses_mixed_variant_wire_enum() {
    let source = "\
#[wire]
enum Mixed {
    A;
    B(i64);
    C { x: String, y: i32 };
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    assert_eq!(decl.kind, TypeDeclKind::Enum);
    assert_eq!(decl.name, "Mixed");
    assert!(
        decl.wire.is_some(),
        "expected wire metadata on mixed-variant enum"
    );
    assert_eq!(decl.body.len(), 3, "expected 3 variants in source order");

    // Variant 0: A — unit
    let TypeBodyItem::Variant(a) = &decl.body[0] else {
        panic!("expected variant A at index 0");
    };
    assert_eq!(a.name, "A");
    assert!(
        matches!(a.kind, VariantKind::Unit),
        "variant A should be unit, got {:?}",
        a.kind
    );

    // Variant 1: B(i64) — tuple
    let TypeBodyItem::Variant(b) = &decl.body[1] else {
        panic!("expected variant B at index 1");
    };
    assert_eq!(b.name, "B");
    assert!(
        matches!(b.kind, VariantKind::Tuple(ref ts) if ts.len() == 1),
        "variant B should be tuple(1), got {:?}",
        b.kind
    );

    // Variant 2: C { x: String, y: i32 } — struct
    let TypeBodyItem::Variant(c) = &decl.body[2] else {
        panic!("expected variant C at index 2");
    };
    assert_eq!(c.name, "C");
    match &c.kind {
        VariantKind::Struct(fields) => {
            assert_eq!(fields.len(), 2, "variant C should have 2 fields");
            assert_eq!(fields[0].0, "x");
            assert_eq!(fields[1].0, "y");
        }
        other => panic!("variant C should be struct, got {other:?}"),
    }
}

#[test]
fn wire_struct_field_metadata_preserves_since_modifier() {
    let source = "\
#[wire]
type Msg {
    added: String @2 repeated since 3 yaml(\"added\");
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    let wire = decl.wire.as_ref().expect("expected wire metadata");
    let meta = &wire.field_meta[0];
    assert!(meta.is_repeated);
    assert_eq!(meta.yaml_name.as_deref(), Some("added"));
    assert_eq!(meta.since, Some(3));
}

#[test]
fn wire_struct_field_metadata_preserves_explicit_number_and_naming_cases_kebab() {
    let source = "\
#[json(\"camelCase\")]
#[yaml(\"kebab-case\")]
#[wire]
type Msg {
    added: String @4 repeated json(\"added_name\");
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    let wire = decl.wire.as_ref().expect("expected wire metadata");
    assert_eq!(wire.json_case, Some(NamingCase::CamelCase));
    assert_eq!(wire.yaml_case, Some(NamingCase::KebabCase));

    let meta = &wire.field_meta[0];
    assert_eq!(meta.field_number, 4);
    assert!(meta.is_repeated);
    assert_eq!(meta.json_name.as_deref(), Some("added_name"));
}

#[test]
fn wire_struct_since_invalid_version_is_rejected() {
    let source = "\
#[wire]
type Msg {
    added: String @2 since 4294967296;
}
";
    let result = parse(source);
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("invalid version number after 'since'")),
        "expected invalid since error, got {:?}",
        result.errors
    );

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    let wire = decl.wire.as_ref().expect("expected wire metadata");
    assert_eq!(wire.field_meta[0].since, None);
}

/// Helper: parse `fn main() { let x = <source>; }` and return the expression.
fn parse_let_expr(source: &str) -> Expr {
    let full = format!("fn main() {{ let x = {source}; }}");
    let result = parse(&full);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(f) = &result.program.items[0].0 else {
        panic!("expected function");
    };
    let Stmt::Let {
        value: Some((expr, _)),
        ..
    } = &f.body.stmts[0].0
    else {
        panic!("expected let with value");
    };
    expr.clone()
}

fn parse_main_body(source: &str) -> Block {
    let full = format!("fn main() {{ {source} }}");
    let result = parse(&full);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(f) = &result.program.items[0].0 else {
        panic!("expected function");
    };
    f.body.clone()
}

#[test]
fn parse_empty_braces_is_block() {
    // {} is always a block — empty HashMap coercion happens in the type checker
    let expr = parse_let_expr("{}");
    assert!(
        matches!(expr, Expr::Block(_)),
        "expected Block, got {expr:?}"
    );
}

#[test]
fn parse_map_literal_single_entry() {
    let expr = parse_let_expr(r#"{"a": 1}"#);
    assert!(
        matches!(expr, Expr::MapLiteral { ref entries } if entries.len() == 1),
        "expected MapLiteral with 1 entry, got {expr:?}"
    );
}

#[test]
fn parse_map_literal_multiple_entries() {
    let expr = parse_let_expr(r#"{"a": 1, "b": 2, "c": 3}"#);
    assert!(
        matches!(expr, Expr::MapLiteral { ref entries } if entries.len() == 3),
        "expected MapLiteral with 3 entries, got {expr:?}"
    );
}

#[test]
fn parse_map_literal_trailing_comma() {
    let expr = parse_let_expr(r#"{"a": 1, "b": 2,}"#);
    assert!(
        matches!(expr, Expr::MapLiteral { ref entries } if entries.len() == 2),
        "expected MapLiteral with 2 entries, got {expr:?}"
    );
}

#[test]
fn parse_block_still_works() {
    let expr = parse_let_expr("{ let y = 1; y }");
    assert!(
        matches!(expr, Expr::Block(_)),
        "expected Block, got {expr:?}"
    );
}

#[test]
fn scope_keyword_emits_scope_ast_variant() {
    let expr = parse_let_expr("scope { 1 }");
    assert!(
        matches!(expr, Expr::Scope { .. }),
        "expected Scope, got {expr:?}"
    );
}

#[test]
fn parser_scope_block_distinct_from_fork_child() {
    let body = parse_main_body("let block = scope { 1 };\nscope { fork child = run(); };\n");
    let Stmt::Let {
        value: Some((Expr::Scope { .. }, _)),
        ..
    } = &body.stmts[0].0
    else {
        panic!(
            "expected let binding to use scope block: {:?}",
            body.stmts[0]
        );
    };
    let Stmt::Expression((Expr::Scope { body: inner }, _)) = &body.stmts[1].0 else {
        panic!("expected outer scope block: {:?}", body.stmts[1]);
    };
    let Stmt::Expression((Expr::ForkChild { binding, .. }, _)) = &inner.stmts[0].0 else {
        panic!("expected child fork expression: {:?}", inner.stmts[0]);
    };
    assert_eq!(binding.as_deref(), Some("child"));
}

#[test]
fn parse_fork_child_with_binding() {
    let body = parse_main_body("fork child = run();");
    let Stmt::Expression((Expr::ForkChild { binding, expr }, _)) = &body.stmts[0].0 else {
        panic!("expected fork child expression: {:?}", body.stmts[0]);
    };
    assert_eq!(binding.as_deref(), Some("child"));
    assert!(
        matches!(&expr.0, Expr::Call { .. }),
        "expected child expression call, got {:?}",
        expr.0
    );
}

#[test]
fn parse_fork_child_bare() {
    let body = parse_main_body("fork run();");
    let Stmt::Expression((Expr::ForkChild { binding, expr }, _)) = &body.stmts[0].0 else {
        panic!("expected bare fork child expression: {:?}", body.stmts[0]);
    };
    assert!(binding.is_none(), "expected bare fork child binding");
    assert!(
        matches!(&expr.0, Expr::Call { .. }),
        "expected bare child expression call, got {:?}",
        expr.0
    );
}

#[test]
fn parse_nested_scope_block_and_child() {
    let expr = parse_let_expr("scope { fork run(); fork child = work(); child }");
    let Expr::Scope { body } = expr else {
        panic!("expected scope block");
    };
    assert_eq!(body.stmts.len(), 2, "expected two child statements");
    assert!(matches!(
        &body.stmts[0].0,
        Stmt::Expression((Expr::ForkChild { binding: None, .. }, _))
    ));
    assert!(matches!(
        &body.stmts[1].0,
        Stmt::Expression((
            Expr::ForkChild {
                binding: Some(name),
                ..
            },
            _
        )) if name == "child"
    ));
    assert!(matches!(
        body.trailing_expr.as_deref(),
        Some((Expr::Identifier(name), _)) if name == "child"
    ));
}

#[test]
fn parse_scope_fork_block_after_deadline() {
    let expr = parse_let_expr("scope { fork { long_op(); } after(5s) { } }");
    let Expr::Scope { body } = expr else {
        panic!("expected scope block");
    };
    assert_eq!(body.stmts.len(), 2, "expected fork block and deadline");
    let Stmt::Expression((Expr::ForkBlock { body: fork_body }, _)) = &body.stmts[0].0 else {
        panic!("expected fork block: {:?}", body.stmts[0]);
    };
    assert_eq!(fork_body.stmts.len(), 1);
    let Stmt::Expression((Expr::ScopeDeadline { duration, body }, _)) = &body.stmts[1].0 else {
        panic!("expected scope deadline: {:?}", body.stmts[1]);
    };
    assert!(
        matches!(duration.0, Expr::Literal(Literal::Duration(5_000_000_000))),
        "deadline duration should be parsed as 5s duration literal: {:?}",
        duration.0
    );
    assert!(body.stmts.is_empty(), "deadline body should be empty");
}

#[test]
fn parse_unscoped_fork_block_rejects() {
    let result = parse("fn main() { fork { long_op(); } }");
    assert!(
        result
            .errors
            .iter()
            .any(|err| err.message.contains("only valid inside `scope")),
        "unscoped fork block must be rejected: {:?}",
        result.errors
    );
}

#[test]
fn capture_doc_comment_on_trait_method() {
    let source = "trait T {\n    /// First line.\n    /// Second line.\n    fn m();\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Trait(t) = &result.program.items[0].0 else {
        panic!("expected trait");
    };
    let TraitItem::Method(m) = &t.items[0] else {
        panic!("expected method");
    };
    assert_eq!(m.doc_comment.as_deref(), Some("First line.\nSecond line."));
}

#[test]
fn capture_doc_comment_on_enum_variant() {
    let source = "enum E {\n    /// The only variant.\n    A;\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::TypeDecl(t) = &result.program.items[0].0 else {
        panic!("expected type decl");
    };
    let TypeBodyItem::Variant(v) = &t.body[0] else {
        panic!("expected variant");
    };
    assert_eq!(v.doc_comment.as_deref(), Some("The only variant."));
}

#[test]
fn capture_doc_comment_on_struct_field() {
    let source = "type S {\n    /// The x coord.\n    x: i32;\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::TypeDecl(t) = &result.program.items[0].0 else {
        panic!("expected type decl");
    };
    let TypeBodyItem::Field { doc_comment, .. } = &t.body[0] else {
        panic!("expected field");
    };
    assert_eq!(doc_comment.as_deref(), Some("The x coord."));
}

#[test]
fn capture_doc_comment_on_receive_fn_and_actor_field() {
    let source = "actor A {\n    /// The counter.\n    let n: i32;\n    /// Increment handler.\n    receive fn inc() {}\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Actor(a) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    assert_eq!(a.fields[0].doc_comment.as_deref(), Some("The counter."));
    assert_eq!(
        a.receive_fns[0].doc_comment.as_deref(),
        Some("Increment handler."),
    );
}

#[test]
fn capture_doc_comment_on_const_and_type_alias() {
    let source = "/// The answer.\npub const ANSWER: i32 = 42;\n\n/// An id.\npub type Id = i64;\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Const(c) = &result.program.items[0].0 else {
        panic!("expected const");
    };
    assert_eq!(c.doc_comment.as_deref(), Some("The answer."));
    let Item::TypeAlias(ta) = &result.program.items[1].0 else {
        panic!("expected type alias");
    };
    assert_eq!(ta.doc_comment.as_deref(), Some("An id."));
}

#[test]
fn capture_doc_comment_on_actor_method() {
    let source = "actor A {\n    /// Reset the counter.\n    fn reset() {}\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Actor(a) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    assert_eq!(a.methods.len(), 1);
    assert_eq!(
        a.methods[0].doc_comment.as_deref(),
        Some("Reset the counter.")
    );
}

// ── ParseDiagnosticKind serialisation ──────────────────────────────────

#[test]
fn parse_diagnostic_kind_unexpected_token_serialises() {
    let kind = ParseDiagnosticKind::UnexpectedToken {
        expected: ";".to_string(),
        got: "}".to_string(),
    };
    let json = serde_json::to_value(&kind).unwrap();
    assert_eq!(json["kind"], "UnexpectedToken");
    assert_eq!(json["expected"], ";");
    assert_eq!(json["got"], "}");
    assert_eq!(kind.as_kind_str(), "UnexpectedToken");
}

#[test]
fn parse_diagnostic_kind_unexpected_eof_serialises() {
    let kind = ParseDiagnosticKind::UnexpectedEof;
    let json = serde_json::to_value(&kind).unwrap();
    assert_eq!(json["kind"], "UnexpectedEof");
    assert_eq!(kind.as_kind_str(), "UnexpectedEof");
}

#[test]
fn parse_diagnostic_kind_invalid_literal_serialises() {
    let kind = ParseDiagnosticKind::InvalidLiteral;
    let json = serde_json::to_value(&kind).unwrap();
    assert_eq!(json["kind"], "InvalidLiteral");
    assert_eq!(kind.as_kind_str(), "InvalidLiteral");
}

#[test]
fn parse_diagnostic_kind_missing_expression_serialises() {
    let kind = ParseDiagnosticKind::MissingExpression {
        got: "end of file".to_string(),
    };
    let json = serde_json::to_value(&kind).unwrap();
    assert_eq!(json["kind"], "MissingExpression");
    assert_eq!(json["got"], "end of file");
    assert_eq!(kind.as_kind_str(), "MissingExpression");
}

#[test]
fn parse_diagnostic_kind_invalid_pattern_serialises() {
    let kind = ParseDiagnosticKind::InvalidPattern {
        got: "42".to_string(),
    };
    let json = serde_json::to_value(&kind).unwrap();
    assert_eq!(json["kind"], "InvalidPattern");
    assert_eq!(json["got"], "42");
    assert_eq!(kind.as_kind_str(), "InvalidPattern");
}

#[test]
fn parse_diagnostic_kind_other_serialises() {
    let kind = ParseDiagnosticKind::Other;
    let json = serde_json::to_value(&kind).unwrap();
    assert_eq!(json["kind"], "Other");
    assert_eq!(kind.as_kind_str(), "Other");
}

// ── ParseDiagnosticKind round-trip through ParseError ─────────────────

#[test]
fn unexpected_token_kind_set_on_expect_failure() {
    // `expect` should produce UnexpectedToken when a required token is absent.
    let result = parse("fn foo(");
    let missing_paren = result
        .errors
        .iter()
        .find(|e| matches!(&e.kind, ParseDiagnosticKind::UnexpectedToken { .. }));
    assert!(
        missing_paren.is_some(),
        "expected UnexpectedToken in errors, got: {:?}",
        result.errors
    );
}

#[test]
fn invalid_literal_kind_set_on_bad_char_escape() {
    let result = parse("fn f() { let x: char = '\\z'; }");
    let lit_err = result
        .errors
        .iter()
        .find(|e| matches!(&e.kind, ParseDiagnosticKind::InvalidLiteral));
    assert!(
        lit_err.is_some(),
        "expected InvalidLiteral in errors, got: {:?}",
        result.errors
    );
}

#[test]
fn missing_expression_kind_set_at_parse_primary_fallthrough() {
    // `)` cannot start an expression in an expression context.
    let result = parse("fn f() { let x = ); }");
    let expr_err = result.errors.iter().find(|e| {
        matches!(
            &e.kind,
            ParseDiagnosticKind::MissingExpression { .. }
                | ParseDiagnosticKind::UnexpectedToken { .. }
        )
    });
    assert!(
        expr_err.is_some(),
        "expected MissingExpression or UnexpectedToken in errors, got: {:?}",
        result.errors
    );
}

// ── StructInit with explicit type arguments ────────────────────────────

#[test]
fn struct_init_explicit_single_type_arg_parses() {
    let src = r#"
            type Wrapper<T> { value: T }
            fn main() { let w = Wrapper<string> { value: "hello" }; }
        "#;
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    // Drill into the let initialiser and confirm type_args is Some([String]).
    let Item::Function(f) = &result.program.items[1].0 else {
        panic!("expected function");
    };
    let stmt = &f.body.stmts[0];
    let Stmt::Let {
        value: Some((expr, _)),
        ..
    } = &stmt.0
    else {
        panic!("expected let with value");
    };
    let Expr::StructInit {
        name, type_args, ..
    } = expr
    else {
        panic!("expected StructInit, got {expr:?}");
    };
    assert_eq!(name, "Wrapper");
    let args = type_args.as_ref().expect("type_args should be Some");
    assert_eq!(args.len(), 1, "expected one type arg");
    assert!(
        matches!(&args[0].0, TypeExpr::Named { name, .. } if name == "string"),
        "expected string type arg, got {:?}",
        args[0].0
    );
}

#[test]
fn struct_init_without_type_args_leaves_type_args_none() {
    let src = r#"
            type Wrapper<T> { value: T }
            fn main() { let w = Wrapper { value: "hello" }; }
        "#;
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let Item::Function(f) = &result.program.items[1].0 else {
        panic!("expected function");
    };
    let stmt = &f.body.stmts[0];
    let Stmt::Let {
        value: Some((expr, _)),
        ..
    } = &stmt.0
    else {
        panic!("expected let with value");
    };
    let Expr::StructInit { type_args, .. } = expr else {
        panic!("expected StructInit, got {expr:?}");
    };
    assert!(type_args.is_none(), "type_args should be None when omitted");
}

#[test]
fn struct_init_explicit_multi_type_arg_parses() {
    let src = r#"
            type Pair<A, B> { first: A, second: B }
            fn main() { let p = Pair<int, string> { first: 1, second: "x" }; }
        "#;
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let Item::Function(f) = &result.program.items[1].0 else {
        panic!("expected function");
    };
    let stmt = &f.body.stmts[0];
    let Stmt::Let {
        value: Some((expr, _)),
        ..
    } = &stmt.0
    else {
        panic!("expected let with value");
    };
    let Expr::StructInit { type_args, .. } = expr else {
        panic!("expected StructInit, got {expr:?}");
    };
    let args = type_args.as_ref().expect("type_args should be Some");
    assert_eq!(args.len(), 2, "expected two type args");
    assert!(
        matches!(&args[0].0, TypeExpr::Named { name, .. } if name == "int"),
        "first type arg should be int"
    );
    assert!(
        matches!(&args[1].0, TypeExpr::Named { name, .. } if name == "string"),
        "second type arg should be string"
    );
}

#[test]
fn struct_init_explicit_type_arg_empty_struct_parses() {
    // Edge case: struct with type param but no fields at the init site is
    // not something the parser should crash on (it's a checker concern).
    let src = r"
            type Tag<T> {}
            fn main() { let t = Tag<int> {}; }
        ";
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let Item::Function(f) = &result.program.items[1].0 else {
        panic!("expected function");
    };
    let stmt = &f.body.stmts[0];
    let Stmt::Let {
        value: Some((expr, _)),
        ..
    } = &stmt.0
    else {
        panic!("expected let with value");
    };
    let Expr::StructInit { type_args, .. } = expr else {
        panic!("expected StructInit, got {expr:?}");
    };
    let args = type_args.as_ref().expect("type_args should be Some");
    assert_eq!(args.len(), 1);
}

#[test]
fn struct_init_comparison_does_not_consume_lt_as_type_arg() {
    // `if x < y { ... }` must NOT be parsed as a struct init with type arg y.
    let src = r"
            fn main() {
                let x = 1;
                let y = 2;
                if x < y { let _ = x; }
            }
        ";
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "parse errors (regression: `<` in comparison swallowed): {:?}",
        result.errors
    );
}
#[test]
fn parses_resource_marker_and_consuming_method() {
    let source = r"
            #[resource]
            type File {
                fd: int
                fn close(consuming self) -> int { 0 }
            }
        ";
    let result = parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    assert_eq!(result.program.items.len(), 1);
    let (item, _span) = &result.program.items[0];
    let Item::TypeDecl(td) = item else {
        panic!("expected TypeDecl, got {item:?}");
    };
    assert_eq!(td.resource_marker, ResourceMarker::Resource);
    assert_eq!(td.consuming_methods, vec!["close".to_string()]);
    assert_eq!(td.name, "File");
}

#[test]
fn parses_linear_marker_and_multiple_consuming_methods() {
    let source = r"
            #[linear]
            type Txn {
                id: int
                fn commit(consuming self) -> int { 0 }
                fn rollback(consuming self) -> int { 1 }
                fn id(t: Txn) -> int { 0 }
            }
        ";
    let result = parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let (Item::TypeDecl(td), _) = &result.program.items[0] else {
        panic!("expected TypeDecl");
    };
    assert_eq!(td.resource_marker, ResourceMarker::Linear);
    // Only `commit` and `rollback` consume; `id(self: Txn)` does not.
    assert_eq!(
        td.consuming_methods,
        vec!["commit".to_string(), "rollback".to_string()],
    );
}

#[test]
fn unmarked_type_has_no_resource_marker() {
    let source = "type Point { x: int; y: int }";
    let result = parse(source);
    assert!(result.errors.is_empty());
    let (Item::TypeDecl(td), _) = &result.program.items[0] else {
        panic!("expected TypeDecl");
    };
    assert_eq!(td.resource_marker, ResourceMarker::None);
    assert!(td.consuming_methods.is_empty());
}

#[test]
fn resource_and_linear_combined_emits_conflict_diagnostic() {
    let source = r"
            #[resource]
            #[linear]
            type Bad { x: int }
        ";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected a conflict diagnostic but got none"
    );
    assert!(
        result.errors[0].message.contains("E_TYPE_MARKER_CONFLICT"),
        "expected E_TYPE_MARKER_CONFLICT in message, got: {:?}",
        result.errors[0].message
    );
}

#[test]
fn linear_then_resource_combined_emits_conflict_diagnostic() {
    // Order is reversed: linear first, resource second — conflict at resource.
    let source = r"
            #[linear]
            #[resource]
            type Bad { x: int }
        ";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected a conflict diagnostic but got none"
    );
    assert!(
        result.errors[0].message.contains("E_TYPE_MARKER_CONFLICT"),
        "expected E_TYPE_MARKER_CONFLICT in message, got: {:?}",
        result.errors[0].message
    );
}

#[test]
fn unknown_type_marker_emits_diagnostic() {
    let source = r"
            #[both]
            type Bad { x: int }
        ";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected E_UNKNOWN_TYPE_MARKER diagnostic but got none"
    );
    assert!(
        result.errors[0].message.contains("E_UNKNOWN_TYPE_MARKER"),
        "expected E_UNKNOWN_TYPE_MARKER in message, got: {:?}",
        result.errors[0].message
    );
}

#[test]
fn wire_and_resource_combined_emits_conflict_diagnostic() {
    // `#[wire] type` + `#[resource]` is disallowed: wire types are
    // traffic-shape declarations; ownership discipline is separate.
    let source = r"
            #[wire]
            #[resource]
            type Bad { x: int @1 }
        ";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected E_TYPE_MARKER_CONFLICT diagnostic but got none"
    );
    assert!(
        result.errors[0].message.contains("E_TYPE_MARKER_CONFLICT"),
        "expected E_TYPE_MARKER_CONFLICT in message, got: {:?}",
        result.errors[0].message
    );
}

#[test]
fn deprecated_attr_on_type_is_accepted() {
    // `#[deprecated]` is a known-valid type-decl attribute and must not
    // trigger E_UNKNOWN_TYPE_MARKER.
    let source = r"
            #[deprecated]
            type Old { x: int }
        ";
    let result = parse(source);
    // No ownership-marker diagnostics; other errors (deprecation warnings
    // etc.) are out of scope for the parser.
    assert!(
        result
            .errors
            .iter()
            .all(|e| !e.message.contains("E_UNKNOWN_TYPE_MARKER")),
        "deprecated attr triggered unknown-marker diagnostic: {:?}",
        result.errors
    );
}
// ── #[max_heap] attribute tests ──────────────────────────────────────────

#[test]
fn max_heap_attribute_bare_integer_bytes() {
    let source = "#[max_heap(1024)] actor Demo {}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Actor(actor) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    assert_eq!(actor.max_heap_bytes, Some(1024));
}

#[test]
fn max_heap_attribute_kb_suffix() {
    let source = "#[max_heap(2 kb)] actor Demo {}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Actor(actor) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    assert_eq!(actor.max_heap_bytes, Some(2 * 1024));
}

#[test]
fn max_heap_attribute_mb_suffix() {
    let source = "#[max_heap(1 mb)] actor Demo {}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Actor(actor) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    assert_eq!(actor.max_heap_bytes, Some(1024 * 1024));
}

#[test]
fn max_heap_attribute_b_suffix() {
    let source = "#[max_heap(512 b)] actor Demo {}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Actor(actor) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    assert_eq!(actor.max_heap_bytes, Some(512));
}

#[test]
fn max_heap_attribute_zero_accepted_as_unbounded() {
    // cap=0 means unbounded (same as the legacy default); accepted explicitly.
    let source = "#[max_heap(0)] actor Demo {}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Actor(actor) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    assert_eq!(actor.max_heap_bytes, Some(0));
}

#[test]
fn max_heap_attribute_gb_suffix_rejected() {
    let source = "#[max_heap(1 gb)] actor Demo {}";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected error for unsupported `gb` suffix"
    );
    assert!(
        result.errors.iter().any(|e| e.message.contains("gb")),
        "expected error mentioning `gb`, got: {:?}",
        result.errors
    );
}

#[test]
fn max_heap_attribute_on_fn_rejected() {
    let source = "#[max_heap(1024)] fn foo() {}";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected error: #[max_heap] only allowed on actor declarations"
    );
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("max_heap") && e.message.contains("actor")),
        "expected error mentioning max_heap and actor, got: {:?}",
        result.errors
    );
}

#[test]
fn max_heap_attribute_on_type_rejected() {
    let source = "#[max_heap(1 kb)] type Bar { x: i32; }";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected error: #[max_heap] only allowed on actor declarations"
    );
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("max_heap") && e.message.contains("actor")),
        "expected error mentioning max_heap and actor, got: {:?}",
        result.errors
    );
}
// ── is operator tests ──────────────────────────────────────────────

/// Helper: extract the trailing expression from the first statement of the
/// first function in the parse result.
fn first_fn_trailing(source: &str) -> Expr {
    let result = parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let Item::Function(f) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    f.body
        .trailing_expr
        .as_ref()
        .expect("expected trailing expr")
        .0
        .clone()
}

#[test]
fn is_operator_simple_identifiers() {
    let expr = first_fn_trailing("fn f() { x is y }");
    assert!(
        matches!(expr, Expr::Is { .. }),
        "expected Expr::Is, got {expr:?}"
    );
    if let Expr::Is { lhs, rhs } = expr {
        assert!(matches!(lhs.0, Expr::Identifier(ref s) if s == "x"));
        assert!(matches!(rhs.0, Expr::Identifier(ref s) if s == "y"));
    }
}

#[test]
fn is_operator_named_type_rhs() {
    // Parser admits any expression on the rhs; checker rejects scalars (D-2).
    let expr = first_fn_trailing("fn f() { value is Point }");
    assert!(matches!(expr, Expr::Is { .. }));
}

#[test]
fn is_operator_scalar_rhs_parses_ok() {
    // `x is int` — parser admits; checker rejects (slice D-2 / D-3 scope).
    let result = parse("fn f() { let x = value is int; }");
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
}

#[test]
fn is_operator_tuple_rhs_parses_ok() {
    // `x is (a, b)` — parser admits a tuple on the rhs; checker will reject later.
    let result = parse("fn f() { let x = value is (a, b); }");
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
}

#[test]
fn is_operator_chained_left_assoc() {
    // `a is b is c` parses as `(a is b) is c` (left-assoc at equality BP).
    let expr = first_fn_trailing("fn f() { a is b is c }");
    // Outer must be Is; its lhs must also be Is.
    let Expr::Is { lhs, .. } = expr else {
        panic!("expected outer Expr::Is");
    };
    assert!(
        matches!(lhs.0, Expr::Is { .. }),
        "expected inner lhs to be Expr::Is (left-assoc), got {:?}",
        lhs.0
    );
}

#[test]
fn is_operator_equality_precedence_binds_tighter_than_logical_and() {
    // `a is b && c is d` should parse as `(a is b) && (c is d)`
    let expr = first_fn_trailing("fn f() { a is b && c is d }");
    let Expr::Binary { left, op, right } = expr else {
        panic!("expected Binary at top level");
    };
    assert_eq!(op, BinaryOp::And);
    assert!(matches!(left.0, Expr::Is { .. }), "left should be Is");
    assert!(matches!(right.0, Expr::Is { .. }), "right should be Is");
}

#[test]
fn is_operator_equality_precedence_with_eq() {
    // `a is b == true` should parse as `(a is b) == true` (same precedence, left-to-right)
    let expr = first_fn_trailing("fn f() { a is b == true }");
    let Expr::Binary { left, op, right } = expr else {
        panic!("expected Binary at top level");
    };
    assert_eq!(op, BinaryOp::Equal);
    assert!(matches!(left.0, Expr::Is { .. }));
    assert!(matches!(right.0, Expr::Literal(Literal::Bool(true))));
}

#[test]
fn is_keyword_reserved_not_identifier() {
    // `is` must not parse as an identifier (it's a reserved keyword).
    use hew_lexer::{lex, Token};
    let toks: Vec<Token<'_>> = lex("is").into_iter().map(|(t, _)| t).collect();
    assert_eq!(toks, vec![Token::Is]);
    assert_ne!(toks[0], Token::Identifier("is"));
}
// ---------------------------------------------------------------------------
// Wrapping operator parsing
// ---------------------------------------------------------------------------

fn first_fn_expr(source: &str) -> Expr {
    let r = parse(source);
    assert!(r.errors.is_empty(), "parse errors: {:?}", r.errors);
    let Item::Function(f) = &r.program.items[0].0 else {
        panic!("expected function item");
    };
    // FnDecl.body is a Block struct directly (not wrapped in Spanned<Expr>).
    let body = &f.body;
    if let Some(e) = &body.trailing_expr {
        e.0.clone()
    } else {
        panic!("expected trailing expression in function body");
    }
}

#[test]
fn wrapping_binary_ops_parse_correctly() {
    // `a &+ b` should parse as Binary { op: WrappingAdd, .. }
    let expr = first_fn_expr("fn f(a: i64, b: i64) -> i64 { a &+ b }");
    let Expr::Binary { op, .. } = expr else {
        panic!("expected Binary");
    };
    assert_eq!(op, BinaryOp::WrappingAdd);

    let expr = first_fn_expr("fn f(a: i64, b: i64) -> i64 { a &- b }");
    let Expr::Binary { op, .. } = expr else {
        panic!("expected Binary");
    };
    assert_eq!(op, BinaryOp::WrappingSub);

    let expr = first_fn_expr("fn f(a: i64, b: i64) -> i64 { a &* b }");
    let Expr::Binary { op, .. } = expr else {
        panic!("expected Binary");
    };
    assert_eq!(op, BinaryOp::WrappingMul);
}

#[test]
fn wrapping_add_precedence_matches_plain_add() {
    // `a &+ b * c` should parse as `a &+ (b * c)` — `*` binds tighter.
    let expr = first_fn_expr("fn f(a: i64, b: i64, c: i64) -> i64 { a &+ b * c }");
    let Expr::Binary { op, right, .. } = expr else {
        panic!("expected outer Binary");
    };
    assert_eq!(op, BinaryOp::WrappingAdd, "outer op must be &+");
    // The right sub-expression must be `b * c` (plain multiply).
    let Expr::Binary { op: inner_op, .. } = right.0 else {
        panic!("expected inner Binary");
    };
    assert_eq!(inner_op, BinaryOp::Multiply, "inner op must be *");
}

#[test]
fn wrapping_mul_precedence_matches_plain_mul() {
    // `a + b &* c` should parse as `a + (b &* c)` — `&*` binds tighter than `+`.
    let expr = first_fn_expr("fn f(a: i64, b: i64, c: i64) -> i64 { a + b &* c }");
    let Expr::Binary { op, right, .. } = expr else {
        panic!("expected outer Binary");
    };
    assert_eq!(op, BinaryOp::Add, "outer op must be +");
    let Expr::Binary { op: inner_op, .. } = right.0 else {
        panic!("expected inner Binary");
    };
    assert_eq!(inner_op, BinaryOp::WrappingMul, "inner op must be &*");
}

#[test]
fn wrapping_ops_parse_with_no_errors() {
    for src in &[
        "fn f(a: i64, b: i64) -> i64 { a &+ b }",
        "fn f(a: i64, b: i64) -> i64 { a &- b }",
        "fn f(a: i64, b: i64) -> i64 { a &* b }",
    ] {
        let r = parse(src);
        assert!(
            r.errors.is_empty(),
            "parse errors for `{src}`: {:?}",
            r.errors
        );
    }
}

// ── functional_update: `R { x: 5, ..base }` ───────────────────────────

#[test]
fn functional_update_basic_parses() {
    // `Point { x: 1, ..old }` must parse as a StructInit with base = Some(old).
    let src = r"
            record Point { x: int, y: int }
            fn f(old: Point) { let p = Point { x: 1, ..old }; }
        ";
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "functional update must parse without errors; got: {:?}",
        result.errors
    );
    let Item::Function(f) = &result.program.items[1].0 else {
        panic!("expected function");
    };
    let stmt = &f.body.stmts[0];
    let Stmt::Let {
        value: Some((expr, _)),
        ..
    } = &stmt.0
    else {
        panic!("expected let with value");
    };
    let Expr::StructInit { fields, base, .. } = expr else {
        panic!("expected StructInit, got {expr:?}");
    };
    assert_eq!(fields.len(), 1, "one explicit field expected");
    let base = base.as_ref().expect("base should be Some");
    assert!(
        matches!(&base.0, Expr::Identifier(name) if name == "old"),
        "base should be Identifier 'old', got {:?}",
        base.0
    );
}

#[test]
fn functional_update_no_explicit_fields_parses() {
    // `Point { ..old }` (zero explicit fields, only base) must also parse.
    let src = r"
            record Point { x: int, y: int }
            fn f(old: Point) { let p = Point { ..old }; }
        ";
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "functional update with no explicit fields must parse; got: {:?}",
        result.errors
    );
    let Item::Function(f) = &result.program.items[1].0 else {
        panic!("expected function");
    };
    let stmt = &f.body.stmts[0];
    let Stmt::Let {
        value: Some((expr, _)),
        ..
    } = &stmt.0
    else {
        panic!("expected let with value");
    };
    let Expr::StructInit { fields, base, .. } = expr else {
        panic!("expected StructInit, got {expr:?}");
    };
    assert_eq!(fields.len(), 0, "no explicit fields expected");
    assert!(base.is_some(), "base should be Some");
}

#[test]
fn functional_update_mid_list_base_is_rejected() {
    // `Point { ..base, x: 1 }` — base is not last; must produce a parse error.
    let src = r"
            record Point { x: int, y: int }
            fn f(old: Point) { let p = Point { ..old, x: 1 }; }
        ";
    let result = parse(src);
    assert!(
        !result.errors.is_empty(),
        "functional-update base not at end must produce a parse error"
    );
}

#[test]
fn functional_update_base_is_none_for_regular_struct_init() {
    // A plain struct literal must have base = None.
    let src = r"
            record Point { x: int, y: int }
            fn f() { let p = Point { x: 1, y: 2 }; }
        ";
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "plain struct init must parse without errors; got: {:?}",
        result.errors
    );
    let Item::Function(f) = &result.program.items[1].0 else {
        panic!("expected function");
    };
    let stmt = &f.body.stmts[0];
    let Stmt::Let {
        value: Some((expr, _)),
        ..
    } = &stmt.0
    else {
        panic!("expected let with value");
    };
    let Expr::StructInit { base, .. } = expr else {
        panic!("expected StructInit, got {expr:?}");
    };
    assert!(base.is_none(), "plain struct init must have base = None");
}

#[test]
fn functional_update_double_base_is_rejected() {
    // `R { ..a, ..b }` must be a parse error — only one base allowed.
    let src = r"
            record Point { x: int, y: int }
            fn f(a: Point, b: Point) { let p = Point { ..a, ..b }; }
        ";
    let result = parse(src);
    let has_expected_error = result
        .errors
        .iter()
        .any(|e| e.message.contains("must be the last item"));
    assert!(
        has_expected_error,
        "double base `..a, ..b` must produce a 'must be the last item' error; got: {:?}",
        result.errors
    );
}

#[test]
fn intrinsic_attribute_parsed_with_semicolon_body() {
    // A function annotated with `#[intrinsic("key")]` and a `;` terminator
    // must parse without errors and carry the intrinsic key in `FnDecl.intrinsic`.
    let src = r#"#[intrinsic("math.sqrt")] pub fn sqrt(x: f64) -> f64;"#;
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "#[intrinsic] with semicolon body must parse cleanly; got: {:?}",
        result.errors
    );
    let (item, _) = result.program.items.first().expect("expected one item");
    if let crate::ast::Item::Function(fd) = item {
        assert_eq!(
            fd.intrinsic.as_deref(),
            Some("math.sqrt"),
            "FnDecl.intrinsic must carry the catalog key"
        );
    } else {
        panic!("expected Item::Function, got something else");
    }
}

#[test]
fn intrinsic_attribute_key_is_none_for_normal_fns() {
    // Regular functions must have `intrinsic == None`.
    let src = r"pub fn add(a: i64, b: i64) -> i64 { a + b }";
    let result = parse(src);
    assert!(result.errors.is_empty());
    let (item, _) = result.program.items.first().expect("expected one item");
    if let crate::ast::Item::Function(fd) = item {
        assert!(
            fd.intrinsic.is_none(),
            "normal fn must have intrinsic == None, got {:?}",
            fd.intrinsic
        );
    } else {
        panic!("expected Item::Function");
    }
}

// --- #[extern_symbol] attribute --------------------------------------

#[test]
fn extern_symbol_attribute_on_extern_c_fn_is_captured() {
    // The attribute must parse via the existing Attribute infrastructure
    // and ride on `ExternFnDecl.attributes`. The template string is
    // captured verbatim — `{T}` is a literal character sequence inside
    // a `StringLit` and is not yet parsed as a placeholder (Stage 2).
    let src = r#"
            extern "C" {
                #[extern_symbol("hew_vec_push_{T}")]
                fn hew_vec_push(v: ptr, x: ptr);
            }
        "#;
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "expected no parse errors, got: {:?}",
        result.errors
    );
    let crate::ast::Item::ExternBlock(block) = &result.program.items[0].0 else {
        panic!("expected ExternBlock");
    };
    assert_eq!(block.functions.len(), 1);
    let extern_fn = &block.functions[0];
    assert_eq!(extern_fn.name, "hew_vec_push");
    assert_eq!(extern_fn.attributes.len(), 1, "must carry one attribute");
    let attr = &extern_fn.attributes[0];
    assert_eq!(attr.name, "extern_symbol");
    assert_eq!(attr.args.len(), 1);
    assert_eq!(attr.args[0].as_str(), "hew_vec_push_{T}");
}

#[test]
fn extern_symbol_attribute_on_impl_method_is_captured() {
    // The attribute must also flow through `parse_impl_decl`'s body loop
    // onto inherent impl methods.
    let src = r#"
            impl<T> Vec<T> {
                #[extern_symbol("hew_vec_push_{T}")]
                fn push(self, x: T) {}
            }
        "#;
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "expected no parse errors, got: {:?}",
        result.errors
    );
    let crate::ast::Item::Impl(impl_decl) = &result.program.items[0].0 else {
        panic!("expected Impl");
    };
    assert_eq!(impl_decl.methods.len(), 1);
    let method = &impl_decl.methods[0];
    assert_eq!(method.name, "push");
    assert_eq!(method.attributes.len(), 1);
    assert_eq!(method.attributes[0].name, "extern_symbol");
    assert_eq!(method.attributes[0].args[0].as_str(), "hew_vec_push_{T}");
}

#[test]
fn extern_symbol_attribute_on_trait_impl_method_is_captured() {
    let src = r#"
            impl<T> SomeTrait for Vec<T> {
                #[extern_symbol("hew_vec_join_str")]
                fn join(self, sep: string) -> string {}
            }
        "#;
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "expected no parse errors, got: {:?}",
        result.errors
    );
    let crate::ast::Item::Impl(impl_decl) = &result.program.items[0].0 else {
        panic!("expected Impl");
    };
    assert!(impl_decl.trait_bound.is_some(), "must be a trait impl");
    assert_eq!(impl_decl.methods.len(), 1);
    let method = &impl_decl.methods[0];
    assert_eq!(method.attributes.len(), 1);
    assert_eq!(method.attributes[0].name, "extern_symbol");
    assert_eq!(method.attributes[0].args[0].as_str(), "hew_vec_join_str");
}

#[test]
fn extern_symbol_attribute_on_free_fn_is_rejected() {
    // Attachment rule: `#[extern_symbol]` is only valid on
    // `fn` declarations inside `extern "C"` blocks or `impl` blocks.
    // Placement on a free fn must surface as a parser diagnostic.
    let src = r#"
            #[extern_symbol("hew_foo")]
            pub fn foo() {}
        "#;
    let result = parse(src);
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("`#[extern_symbol]`")),
        "expected an `#[extern_symbol]` placement diagnostic, got: {:?}",
        result.errors
    );
}

#[test]
fn extern_symbol_attribute_on_actor_is_rejected() {
    let src = r#"
            #[extern_symbol("hew_actor")]
            actor MyActor {}
        "#;
    let result = parse(src);
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("`#[extern_symbol]`")),
        "expected an `#[extern_symbol]` placement diagnostic, got: {:?}",
        result.errors
    );
}

#[test]
fn extern_symbol_attribute_on_type_decl_method_is_rejected() {
    // Methods declared inline in a type body are not `impl` methods —
    // declare them in an `impl` block instead.
    let src = r#"
            type Foo {
                x: i64,
                #[extern_symbol("hew_foo_bar")]
                fn bar(self) {}
            }
        "#;
    let result = parse(src);
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("`#[extern_symbol]`")),
        "expected an `#[extern_symbol]` placement diagnostic, got: {:?}",
        result.errors
    );
}

#[test]
fn extern_symbol_attribute_on_actor_receive_fn_is_rejected() {
    // Actor `receive fn` (and `receive gen fn`) declarations cannot bind
    // to a runtime C-ABI symbol — extern_symbol is only valid in an
    // `extern "C"` block or an `impl` block.
    let src = r#"
            actor MyActor {
                #[extern_symbol("hew_actor_recv")]
                receive fn ping() {}
            }
        "#;
    let result = parse(src);
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("`#[extern_symbol]`")),
        "expected an `#[extern_symbol]` placement diagnostic, got: {:?}",
        result.errors
    );
}

#[test]
fn extern_symbol_attribute_on_trait_item_fn_is_rejected() {
    // Trait items describe an abstract surface; the C-ABI binding belongs
    // on the concrete `impl` method, not the trait declaration.
    let src = r#"
            trait Pushable<T> {
                #[extern_symbol("hew_vec_push_{T}")]
                fn push(self, value: T);
            }
        "#;
    let result = parse(src);
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("`#[extern_symbol]`")),
        "expected an `#[extern_symbol]` placement diagnostic, got: {:?}",
        result.errors
    );
}

#[test]
fn extern_symbol_attribute_missing_string_arg_is_rejected() {
    // `parse_attributes` enforces value-shape rules; a key-value form
    // here surfaces an "invalid value" diagnostic. (Stage 2 owns
    // semantic template-grammar validation; the parser only relies on the
    // existing attribute syntax gate.)
    let src = r#"
            extern "C" {
                #[extern_symbol(= 5)]
                fn hew_x();
            }
        "#;
    let result = parse(src);
    assert!(
        !result.errors.is_empty(),
        "malformed `#[extern_symbol]` argument list must produce a parser error"
    );
}

// ---------------------------------------------------------------------------
// Leading-dot variant patterns (`.Variant` implicit-enum form)
// ---------------------------------------------------------------------------

/// Extract the arm patterns of the first `match` expression in the first
/// function's body. Panics if the shape does not match.
fn first_match_arm_patterns(source: &str) -> Vec<Pattern> {
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(f) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    // A value-producing `match` lands in the block's `trailing_expr`; a
    // statement-position `match` is a `Stmt::Match`. Check both.
    if let Some(tail) = &f.body.trailing_expr {
        if let Expr::Match { arms, .. } = &tail.0 {
            return arms.iter().map(|a| a.pattern.0.clone()).collect();
        }
    }
    for (stmt, _) in &f.body.stmts {
        if let Stmt::Match { arms, .. } = stmt {
            return arms.iter().map(|a| a.pattern.0.clone()).collect();
        }
        if let Stmt::Expression(e) | Stmt::Return(Some(e)) = stmt {
            if let Expr::Match { arms, .. } = &e.0 {
                return arms.iter().map(|a| a.pattern.0.clone()).collect();
            }
        }
    }
    panic!("no match expression found in function body");
}

/// A leading-dot tuple-payload variant pattern parses to the SAME
/// `Pattern::Constructor` a bare (unqualified) name would, with the short
/// variant name and its payload sub-patterns.
#[test]
fn leading_dot_constructor_pattern_parses_as_bare_constructor() {
    let source = "fn f(e: E) -> i64 { match e { .Some(x) => x, _ => 0 } }";
    let patterns = first_match_arm_patterns(source);
    match &patterns[0] {
        Pattern::Constructor { name, patterns } => {
            assert_eq!(name, "Some");
            assert_eq!(patterns.len(), 1);
            assert!(matches!(&patterns[0].0, Pattern::Identifier(n) if n == "x"));
        }
        other => panic!("expected Pattern::Constructor, got {other:?}"),
    }
}

/// A leading-dot unit variant pattern parses to a bare-name
/// `Pattern::Identifier`, matching the unqualified unit-variant spelling.
#[test]
fn leading_dot_unit_variant_pattern_parses_as_identifier() {
    let source = "fn f(e: E) -> i64 { match e { .None => 0, _ => 1 } }";
    let patterns = first_match_arm_patterns(source);
    assert!(matches!(&patterns[0], Pattern::Identifier(n) if n == "None"));
}

/// Leading-dot variants compose with or-patterns: `.A(x) | .B(x)` parses to
/// a `Pattern::Or` over two bare-name constructor leaves.
#[test]
fn leading_dot_or_pattern_parses() {
    let source = "fn f(e: E) -> i64 { match e { .A(x) | .B(x) => x, _ => 0 } }";
    let patterns = first_match_arm_patterns(source);
    let Pattern::Or(left, right) = &patterns[0] else {
        panic!("expected Pattern::Or, got {:?}", patterns[0]);
    };
    assert!(matches!(&left.0, Pattern::Constructor { name, .. } if name == "A"));
    assert!(matches!(&right.0, Pattern::Constructor { name, .. } if name == "B"));
}

/// Adding the leading-dot arm does not disturb the existing qualified-name
/// (`Type::Variant`) constructor-pattern path: both spellings still parse,
/// and the qualified form keeps its fully-qualified name.
#[test]
fn leading_dot_arm_leaves_qualified_pattern_path_intact() {
    let source = "fn f(e: E) -> i64 { match e { E::Some(x) => x, .None => 0, _ => 1 } }";
    let patterns = first_match_arm_patterns(source);
    match &patterns[0] {
        Pattern::Constructor { name, .. } => assert_eq!(name, "E::Some"),
        other => panic!("expected qualified Pattern::Constructor, got {other:?}"),
    }
    assert!(matches!(&patterns[1], Pattern::Identifier(n) if n == "None"));
}

// ── RecordShorthand pattern (let {a, b} = rec) ───────────────────────────

/// `let { x, y } = p` parses as `Pattern::RecordShorthand` with two plain
/// field bindings and no type name.
#[test]
fn parse_let_record_shorthand_two_fields_no_type_name() {
    let source = "fn main() -> i64 { let p = Point { x: 1, y: 2 }; let { x, y } = p; x + y }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    // Extract the let statement pattern from the function body.
    let crate::ast::Item::Function(func) = &result.program.items[0].0 else {
        panic!("expected function, got {:?}", result.program.items[0].0);
    };
    let stmts = &func.body.stmts;
    // Second statement (index 1) is `let { x, y } = p`.
    let (crate::ast::Stmt::Let { pattern, .. }, _) = &stmts[1] else {
        panic!("expected let statement at index 1, got {:?}", stmts[1]);
    };
    let Pattern::RecordShorthand { fields } = &pattern.0 else {
        panic!("expected Pattern::RecordShorthand, got {:?}", pattern.0);
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name, "x");
    assert!(
        fields[0].pattern.is_none(),
        "shorthand field should have no sub-pattern"
    );
    assert_eq!(fields[1].name, "y");
}

/// `let { x: px, y: py } = p` parses as `Pattern::RecordShorthand` with
/// alias sub-patterns for each field.
#[test]
fn parse_let_record_shorthand_with_alias_subpatterns() {
    let source =
        "fn main() -> i64 { let p = Point { x: 1, y: 2 }; let { x: px, y: py } = p; px + py }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let crate::ast::Item::Function(func) = &result.program.items[0].0 else {
        panic!("expected function, got {:?}", result.program.items[0].0);
    };
    let stmts = &func.body.stmts;
    let (crate::ast::Stmt::Let { pattern, .. }, _) = &stmts[1] else {
        panic!("expected let statement at index 1, got {:?}", stmts[1]);
    };
    let Pattern::RecordShorthand { fields } = &pattern.0 else {
        panic!("expected Pattern::RecordShorthand, got {:?}", pattern.0);
    };
    assert_eq!(fields.len(), 2);
    assert!(
        fields[0].pattern.is_some(),
        "field x should have alias sub-pattern"
    );
    assert!(
        fields[1].pattern.is_some(),
        "field y should have alias sub-pattern"
    );
}

/// `let { x, y } = p` round-trips through the formatter as `let { x, y } = p`
/// (the shorthand form is preserved — not rewritten to the typed form).
#[test]
fn format_let_record_shorthand_round_trips_without_type_name() {
    let source = "fn main() -> i64 {\n    let p = Point { x: 1, y: 2 };\n    let { x, y } = p;\n    x + y\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let formatted = crate::fmt::format_program(&result.program);
    // The formatted output must contain the shorthand `{ x, y }`, not `Point { x, y }`.
    assert!(
        formatted.contains("let { x, y } = p"),
        "formatted output should preserve shorthand form, got:\n{formatted}"
    );
}

// Proving gate: bare `wire` keyword is rejected as a declaration form.
// Item::Wire no longer exists; the identifier "wire" produces a helpful error.
#[test]
fn bare_struct_rejected_with_type_hint() {
    let result = parse("struct Point { x: i64 }");
    assert!(
        !result.errors.is_empty(),
        "expected parse error for bare `struct`"
    );
    assert_eq!(result.errors[0].message, "unexpected 'struct'");
    assert_eq!(
        result.errors[0].hint.as_deref(),
        Some("write `type Name { ... }`")
    );
}

#[test]
fn wire_struct_rejected_with_wire_type_hint() {
    let result = parse("#[wire]\nstruct Point { x: i64 @1 }");
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `#[wire] struct`"
    );
    assert_eq!(result.errors[0].message, "unexpected 'struct'");
    assert_eq!(
        result.errors[0].hint.as_deref(),
        Some("write `#[wire] type Name { ... }`")
    );
}

#[test]
fn bare_wire_keyword_rejected_with_hint() {
    let result = parse("wire Foo {}");
    assert!(
        !result.errors.is_empty(),
        "expected parse error for bare `wire`"
    );
    let msg = &result.errors[0].message;
    assert!(
        msg.contains("wire"),
        "error should mention 'wire', got: {msg}"
    );
    // No Item::Wire variant can be constructed — match ensures the exhaustive check
    for (item, _) in &result.program.items {
        match item {
            Item::TypeDecl(_)
            | Item::TypeAlias(_)
            | Item::Function(_)
            | Item::Actor(_)
            | Item::Machine(_)
            | Item::Record(_)
            | Item::Supervisor(_)
            | Item::Impl(_)
            | Item::Trait(_)
            | Item::Import(_)
            | Item::Const(_)
            | Item::ExternBlock(_) => {}
        }
    }
}

#[test]
fn wire_type_keyword_form_rejected() {
    let result = parse("wire type Foo { @1 x: string }");
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `wire type`"
    );
    let first_msg = &result.errors[0].message;
    assert!(
        first_msg.contains("wire"),
        "first error should mention 'wire', got: {first_msg}"
    );
    let first_hint = result.errors[0].hint.as_deref().unwrap_or_default();
    assert!(
        first_hint.contains("#[wire] type") && !first_hint.contains("struct"),
        "hint should point at #[wire] type, got: {first_hint}"
    );
}

#[test]
fn wire_enum_keyword_form_rejected() {
    let result = parse("wire enum Status { Active; Inactive; }");
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `wire enum`"
    );
    let first_msg = &result.errors[0].message;
    assert!(
        first_msg.contains("wire"),
        "first error should mention 'wire', got: {first_msg}"
    );
    let first_hint = result.errors[0].hint.as_deref().unwrap_or_default();
    assert!(
        first_hint.contains("#[wire] type") && !first_hint.contains("struct"),
        "hint should point at #[wire] type, got: {first_hint}"
    );
}
