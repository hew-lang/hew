//! Parser unit tests.

use super::*;

#[test]
fn parse_simple_function() {
    let source = "fn main() { let x = 1 + 2; }";
    let result = parse(source);
    assert!(result.errors.is_empty());
    assert_eq!(result.program.items.len(), 1);
}

#[test]
fn parse_stdlib_authority_attributes() {
    let source = r#"#[lang_item("option")]
pub enum Maybe<T> {
    Some(T);
    None;
}

#[intrinsic("math.sqrt")]
pub fn sqrt(x: f64) -> f64 {
}

extern "C" {
    #[runtime_capability("metrics")]
    fn hew_metric();
}
"#;
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(type_decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    assert_eq!(type_decl.lang_item.as_deref(), Some("option"));

    let Item::Function(intrinsic_fn) = &result.program.items[1].0 else {
        panic!("expected intrinsic function");
    };
    assert_eq!(intrinsic_fn.intrinsic.as_deref(), Some("math.sqrt"));

    let Item::ExternBlock(extern_block) = &result.program.items[2].0 else {
        panic!("expected extern block");
    };
    let capability = &extern_block.functions[0].attributes[0];
    assert_eq!(capability.name, "runtime_capability");
    assert_eq!(
        capability.args,
        vec![AttributeArg::Positional("metrics".to_string())]
    );
}

#[test]
fn malformed_stdlib_authority_attributes_are_rejected() {
    for source in [
        "#[lang_item] pub trait MissingKey {}",
        "#[intrinsic(key = sqrt)] pub fn sqrt(value: f64) -> f64 {}",
    ] {
        let result = parse(source);
        assert!(!result.errors.is_empty(), "{:?}", result.errors);
    }
}

/// Supervisor child declarations accept the module-qualified dotted type
/// (`child a: bank.Account`), carrying the qualified actor identity
/// verbatim; bare child types stay the root/local spelling.
#[test]
fn parse_supervisor_child_dotted_module_qualified_type() {
    let source = "supervisor S {\n    strategy: one_for_one;\n    intensity: 1 within 60s;\n\n    child a: bank.Account { n: 1 };\n    child b: Local;\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Supervisor(sd) = &result.program.items[0].0 else {
        panic!("expected supervisor, got {:?}", result.program.items[0].0);
    };
    assert_eq!(sd.children[0].actor_type.to_string(), "bank.Account");
    assert_eq!(sd.children[0].args.len(), 1);
    assert_eq!(sd.children[1].actor_type.to_string(), "Local");
}

/// A supervisor takes construction-time config params: `supervisor App(config: T)`.
/// The params carry through so child init-arg exprs can derive from runtime config.
#[test]
fn parse_supervisor_construction_time_config_params() {
    let source = "supervisor App(config: AppConfig) {\n    strategy: one_for_one;\n    child cache: Cache { capacity: config.cache_size };\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Supervisor(sd) = &result.program.items[0].0 else {
        panic!("expected supervisor, got {:?}", result.program.items[0].0);
    };
    assert_eq!(sd.params.len(), 1, "one config param");
    assert_eq!(sd.params[0].name, Ident::new("config"));
    // The child init arg references the config binding (a non-literal expr).
    assert_eq!(sd.children[0].args.len(), 1);
    assert_eq!(sd.children[0].args[0].0, Ident::new("capacity"));
}

/// Pool arity is the per-child `count:` clause, parsed beside `restart:` and
/// `stop:` and kept out of the parenthesised init-arg list.
#[test]
fn parse_pool_count_clause_lands_beside_the_init_args() {
    let source = "supervisor Farm {\n    strategy: simple_one_for_one;\n    intensity: 3 within 60s;\n    pool workers: Worker { value: 7 } count: 2 restart: transient;\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Supervisor(sd) = &result.program.items[0].0 else {
        panic!("expected supervisor, got {:?}", result.program.items[0].0);
    };
    let child = &sd.children[0];
    assert!(child.is_pool);
    assert!(child.count.is_some(), "the arity clause parsed");
    assert_eq!(
        child.args.len(),
        1,
        "the parenthesised list holds only the actor's own fields"
    );
    assert_eq!(child.args[0].0, Ident::new("value"));
    assert_eq!(child.restart, Some(RestartPolicy::Transient));
}

#[test]
fn supervisor_stop_clause_carries_a_duration_expression() {
    let result = parse(
        "actor Worker { receive fn work() {} }\n\
         supervisor Team { child worker: Worker stop: 50ms; }",
    );
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Supervisor(supervisor) = &result.program.items[1].0 else {
        panic!("expected supervisor");
    };
    assert!(matches!(
        supervisor.children[0].stop,
        Some((Expr::Literal(Literal::Duration(50_000_000)), _))
    ));
}

/// A child's `backoff:` clause is an ordinary expression beside `restart:` and
/// `stop:`, in either order, and stays separate from the supervisor's own.
#[test]
fn supervisor_child_backoff_clause_parses_beside_restart() {
    for clauses in [
        "restart: permanent backoff: restart.immediate()",
        "backoff: restart.immediate() restart: permanent",
    ] {
        let source = format!(
            "supervisor Team {{\n    backoff: restart.exponential(initial: 100ms);\n    child worker: Worker {clauses};\n    child other: Worker;\n}}\n"
        );
        let result = parse(&source);
        assert!(result.errors.is_empty(), "{clauses}: {:?}", result.errors);
        let Item::Supervisor(supervisor) = &result.program.items[0].0 else {
            panic!("expected supervisor");
        };
        assert!(supervisor.backoff.is_some());
        let child = &supervisor.children[0];
        assert_eq!(child.restart, Some(RestartPolicy::Permanent));
        assert!(child.backoff.is_some());
        assert!(supervisor.children[1].backoff.is_none());
    }
}

#[test]
fn supervisor_child_backoff_clause_is_declared_once() {
    let result = parse(
        "supervisor Team { child worker: Worker backoff: restart.immediate() backoff: restart.immediate(); }",
    );
    assert!(
        result.errors.iter().any(|error| error
            .message
            .contains("duplicate supervisor child `backoff:`")),
        "{:?}",
        result.errors
    );
}

#[test]
fn supervisor_child_accepts_only_the_stop_clause() {
    for old in ["shutdown: 50ms", "shutdown: brutal_kill"] {
        let source = format!(
            "actor Worker {{ receive fn work() {{}} }}\n\
             supervisor Team {{ child worker: Worker {old}; }}"
        );
        assert!(!parse(&source).errors.is_empty(), "{old} must not parse");
    }
}

/// The clause namespace and the field namespace are separate, so an actor with
/// a field named `count` pools with its field set like any other. This is the
/// collision the clause form removes.
#[test]
fn parse_pool_child_sets_an_actor_field_named_count() {
    let source = "supervisor Farm {\n    strategy: simple_one_for_one;\n    pool tickers: Ticker { count: 9 } count: 2;\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Supervisor(sd) = &result.program.items[0].0 else {
        panic!("expected supervisor");
    };
    let child = &sd.children[0];
    assert_eq!(child.args.len(), 1, "the `count` field stays an init arg");
    assert_eq!(child.args[0].0, Ident::new("count"));
    assert!(child.count.is_some(), "arity comes from the clause");
}

/// `count:` in a pool child's parentheses with no arity clause is the retired
/// spelling. It is refused with the new one rather than passed on as an init
/// field that the checker would then blame on the actor.
#[test]
fn parse_pool_count_as_init_arg_is_refused_with_the_clause_spelling() {
    let source = "supervisor Farm {\n\
                      \x20   strategy: simple_one_for_one;\n\
                      \x20   pool workers: Worker { count: 2, value: 7 };\n\
                      }\n";
    let result = parse(source);
    assert!(!result.errors.is_empty(), "{:?}", result.errors);
}

/// A `child` declaration is one actor, so it has no arity to declare. The
/// clause is refused there rather than accepted and ignored.
#[test]
fn parse_count_clause_on_a_static_child_is_refused() {
    let source = "supervisor Farm {\n\
                      \x20   strategy: one_for_one;\n\
                      \x20   child worker: Worker { value: 7 } count: 2;\n\
                      }\n";
    let result = parse(source);
    assert!(!result.errors.is_empty(), "{:?}", result.errors);
    let Item::Supervisor(sd) = &result.program.items[0].0 else {
        panic!("expected supervisor");
    };
    assert!(
        sd.children[0].count.is_none(),
        "a refused clause carries no arity downstream"
    );
}

/// A static child's `count:` init arg is an ordinary field, untouched by the
/// pool migration.
#[test]
fn parse_static_child_count_init_arg_stays_a_field() {
    let source =
        "supervisor Farm {\n    strategy: one_for_one;\n    child ticker: Ticker { count: 9 };\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Supervisor(sd) = &result.program.items[0].0 else {
        panic!("expected supervisor");
    };
    assert_eq!(sd.children[0].args[0].0, Ident::new("count"));
    assert!(sd.children[0].count.is_none());
}

/// A supervisor without a `(...)` clause has no params (back-compat).
#[test]
fn parse_supervisor_without_params_has_empty_param_list() {
    let source = "supervisor S {\n    strategy: one_for_one;\n    child a: Local;\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Supervisor(sd) = &result.program.items[0].0 else {
        panic!("expected supervisor");
    };
    assert!(sd.params.is_empty());
}

#[test]
fn parse_item_and_member_doc_comments() {
    let mod_res = parse("//! Module docs.\n//! Line two.\nfn foo() {}");
    assert!(mod_res.errors.is_empty());
    assert_eq!(
        mod_res.program.module_doc.as_deref(),
        Some("Module docs.\nLine two.")
    );

    let fn_res = parse("/// Adds numbers.\nfn add(a: i32, b: i32) -> i32 { a + b }\nfn bare() {}");
    assert!(fn_res.errors.is_empty());
    let Item::Function(f0) = &fn_res.program.items[0].0 else {
        panic!("expected fn");
    };
    let Item::Function(f1) = &fn_res.program.items[1].0 else {
        panic!("expected fn");
    };
    assert_eq!(f0.doc_comment.as_deref(), Some("Adds numbers."));
    assert!(f1.doc_comment.is_none());

    let trait_res = parse("trait T {\n    /// First line.\n    /// Second line.\n    fn m();\n}\n");
    assert!(
        trait_res.errors.is_empty(),
        "errors: {:?}",
        trait_res.errors
    );
    let Item::Trait(trait_decl) = &trait_res.program.items[0].0 else {
        panic!("expected trait");
    };
    let TraitItem::Method(method) = &trait_decl.items[0] else {
        panic!("expected method");
    };
    assert_eq!(
        method.doc_comment.as_deref(),
        Some("First line.\nSecond line.")
    );

    let enum_res = parse("enum E {\n    /// The only variant.\n    A;\n}\n");
    assert!(enum_res.errors.is_empty(), "errors: {:?}", enum_res.errors);
    let Item::TypeDecl(td) = &enum_res.program.items[0].0 else {
        panic!("expected enum");
    };
    let TypeBodyItem::Variant(variant) = &td.body[0] else {
        panic!("expected variant");
    };
    assert_eq!(variant.doc_comment.as_deref(), Some("The only variant."));

    let struct_res = parse("type S {\n    /// The x coord.\n    x: i32;\n}\n");
    assert!(
        struct_res.errors.is_empty(),
        "errors: {:?}",
        struct_res.errors
    );
    let Item::TypeDecl(sd) = &struct_res.program.items[0].0 else {
        panic!("expected type");
    };
    let TypeBodyItem::Field { doc_comment, .. } = &sd.body[0] else {
        panic!("expected field");
    };
    assert_eq!(doc_comment.as_deref(), Some("The x coord."));

    let actor_res = parse("actor A {\n    /// The counter.\n    let n: i32;\n    /// Increment handler.\n    receive fn inc() {}\n    /// Reset the counter.\n    fn reset() {}\n}\n");
    assert!(
        actor_res.errors.is_empty(),
        "errors: {:?}",
        actor_res.errors
    );
    let Item::Actor(actor_decl) = &actor_res.program.items[0].0 else {
        panic!("expected actor");
    };
    assert_eq!(
        actor_decl.fields[0].doc_comment.as_deref(),
        Some("The counter.")
    );
    assert_eq!(
        actor_decl.receive_fns[0].doc_comment.as_deref(),
        Some("Increment handler.")
    );
    assert_eq!(
        actor_decl.methods[0].doc_comment.as_deref(),
        Some("Reset the counter.")
    );

    let misc_res =
        parse("/// The answer.\npub const ANSWER: i32 = 42;\n\n/// An id.\npub type Id = i64;\n");
    assert!(misc_res.errors.is_empty(), "errors: {:?}", misc_res.errors);
    let Item::Const(const_decl) = &misc_res.program.items[0].0 else {
        panic!("expected const");
    };
    let Item::TypeAlias(ta) = &misc_res.program.items[1].0 else {
        panic!("expected type alias");
    };
    assert_eq!(const_decl.doc_comment.as_deref(), Some("The answer."));
    assert_eq!(ta.doc_comment.as_deref(), Some("An id."));
}

#[test]
fn record_declaration_emits_type_migration_hint() {
    let source = "record Point { x: i32, y: i32 }";
    let result = parse(source);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
}

#[test]
fn type_tuple_declaration_preserves_positional_constructor_surface() {
    let result = parse("type Pair(i64, string);");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Record(decl) = &result.program.items[0].0 else {
        panic!("expected positional product declaration");
    };
    assert!(matches!(decl.kind, RecordKind::Tuple(ref fields) if fields.len() == 2));
}

#[test]
fn parse_actor_decl() {
    let source =
        "actor Counter {\n    var count: i32 = 0;\n    receive fn increment() {\n        count = count + 1;\n    }\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty());
    assert_eq!(result.program.items.len(), 1);
    if let Item::Actor(actor) = &result.program.items[0].0 {
        assert_eq!(actor.fields.len(), 1);
        assert_eq!(actor.fields[0].name, Ident::new("count"));
        assert_eq!(actor.receive_fns.len(), 1);
    } else {
        panic!("expected actor item");
    }
}

#[test]
fn optional_control_flow_surface_accepts_lazy_default_and_local_handler() {
    for source in [
        "fn f(value: Option<i64>) -> i64 { value ?? 7 }",
        "fn f(value: Result<i64, string>) -> i64 { value handle problem { return 0; } }",
        "fn f(handle: i64) -> i64 { handle + 1 }",
    ] {
        let parsed = parse(source);
        assert!(parsed.errors.is_empty(), "{source}: {:?}", parsed.errors);
    }
}

#[test]
fn optional_default_associates_right_below_logical_operators() {
    let parsed = parse("fn f() { a ?? b ?? c || d }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let Item::Function(function) = &parsed.program.items[0].0 else {
        panic!("function");
    };
    let Expr::Coalesce { right, .. } = &function.body.trailing_expr.as_ref().expect("tail").0
    else {
        panic!("outer default");
    };
    let Expr::Coalesce { right, .. } = &right.0 else {
        panic!("right-associated default");
    };
    assert_eq!(right.0.binary_last_op(), Some(BinaryOp::Or));
}

#[test]
fn local_handler_requires_binder_and_block() {
    for source in [
        "fn f() { value handle { 7 } }",
        "fn f() { value handle problem 7 }",
    ] {
        assert!(!parse(source).errors.is_empty(), "{source}");
    }
}

#[test]
fn local_handler_accepts_wildcard_binding() {
    let parsed = parse("fn f() { value handle _ { 7 } }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
}

#[test]
fn local_recovery_formatting_preserves_nested_expression_meaning() {
    for source in [
        "fn f() { a handle first { b handle second { second } } }",
        "fn f() { a handle _ { b handle _ { 0 } } }",
        "fn f() { (a handle problem { 7 }) + 1 }",
        "fn f() { a ?? (b handle problem { 7 }) }",
        "fn f() { await (a ?? b) }",
        "fn f() { (a ?? b)? }",
    ] {
        let before = parse(source);
        assert!(before.errors.is_empty(), "{source}: {:?}", before.errors);
        let formatted = crate::fmt::format_program(&before.program);
        let after = parse(&formatted);
        assert!(after.errors.is_empty(), "{formatted}: {:?}", after.errors);
        assert!(
            crate::ast_eq::program_eq_ignoring_spans(&before.program, &after.program),
            "{source} -> {formatted}"
        );
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
    assert_eq!(result.errors[0].kind, ParseDiagnosticKind::ReservedName);
    assert_eq!(&source[result.errors[0].span.clone()], "match");
}

/// Variant: `record` as the name of a top-level `fn` declaration.
#[test]
fn toplevel_fn_can_use_record_as_an_identifier() {
    let source = "fn record(n: i64) -> i64 { n }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

/// A reserved word in a BINDING position must be named as reserved. The
/// generic invalid-pattern message only echoes the raw token, which leaves a
/// reserved word indistinguishable from a typo — the reader cannot tell "this
/// word is off-limits" from "the parser did not recognise this token".
#[test]
fn let_binding_reserved_keyword_names_it_as_reserved() {
    let source = "fn f() { let match = 1 }";
    let result = parse(source);
    let diagnostic = result
        .errors
        .iter()
        .find(|err| matches!(err.kind, ParseDiagnosticKind::InvalidPattern { .. }))
        .unwrap_or_else(|| {
            panic!(
                "expected an invalid-pattern error, got: {:?}",
                result.errors
            )
        });
    assert_eq!(&source[diagnostic.span.clone()], "match");
}

/// `let mut x = …` recovers as `var x = …` behind exactly one diagnostic: the
/// message names Hew's `var` spelling, the hint spells out the fix-it, and
/// parsing resumes cleanly — the following statement still parses, which
/// would not hold if `mut` were left unconsumed (the three-error cascade this
/// replaces: `mut` fails as a pattern, then as an expression, then triggers
/// block-level token recovery).
#[test]
fn let_mut_reports_one_error_and_recovers() {
    let result = parse("fn f() { let mut x = 1; x = 2; }");
    assert_eq!(
        result.errors.len(),
        1,
        "expected exactly one diagnostic, got: {:?}",
        result.errors
    );

    let func = match &result.program.items[0].0 {
        Item::Function(f) => f,
        other => panic!("expected a function item, got {other:?}"),
    };
    assert_eq!(
        func.body.stmts.len(),
        2,
        "expected the `let mut` statement plus the following assignment to both parse; got: {:?}",
        func.body.stmts
    );
    assert!(
        matches!(func.body.stmts[0].0, Stmt::Var { .. }),
        "let mut recovers as a mutable (`var`) binding so a later reassignment does not raise a \
         second, unrelated immutability error downstream; got: {:?}",
        func.body.stmts[0].0
    );
    assert!(
        matches!(func.body.stmts[1].0, Stmt::Assign { .. }),
        "the statement after the recovered `let mut` must parse normally, proving `mut` was \
         consumed rather than left for block-level recovery to eat; got: {:?}",
        func.body.stmts[1].0
    );
}

/// The same fact through the other pattern position.
#[test]
fn match_arm_reserved_keyword_names_it_as_reserved() {
    let result = parse("fn f() { match 1 { type => 0 } }");
    assert!(!result.errors.is_empty(), "{:?}", result.errors);
}

/// Ordinary invalid patterns remain distinct from reserved names.
#[test]
fn non_keyword_invalid_pattern_is_rejected() {
    let result = parse("fn f() { match 1 { => 0 } }");
    result
        .errors
        .iter()
        .find(|err| matches!(err.kind, ParseDiagnosticKind::InvalidPattern { .. }))
        .unwrap_or_else(|| {
            panic!(
                "expected an invalid-pattern error, got: {:?}",
                result.errors
            )
        });
}

#[test]
fn toplevel_fn_actor_name_still_reports_reserved_word() {
    let source = "fn actor() {}";
    let result = parse(source);
    let diagnostic = result
        .errors
        .iter()
        .find(|err| err.kind == ParseDiagnosticKind::ReservedName)
        .expect("reserved-name diagnostic");
    assert_eq!(&source[diagnostic.span.clone()], "actor");
}

/// Recovery: a second valid receive fn after a reserved-keyword name is
/// still parsed correctly (the error does not swallow the rest of the actor).
#[test]
fn record_identifier_does_not_disrupt_sibling_methods() {
    let source = "actor Metrics { receive fn record(n: i64) {} receive fn add(m: i64) {} }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    assert_eq!(
        result.program.items.len(),
        1,
        "actor item must still be produced"
    );
    let Item::Actor(actor) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    assert_eq!(
        actor.receive_fns.len(),
        2,
        "both receive functions must be parsed"
    );
    assert_eq!(actor.receive_fns[0].name, Ident::new("record"));
    assert_eq!(actor.receive_fns[1].name, Ident::new("add"));
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
        assert_eq!(rf.name, Ident::new("bar"));
        let tps = rf.type_params.as_ref().expect("expected type_params");
        assert_eq!(tps.len(), 1);
        assert_eq!(tps[0].name, Ident::new("T"));
        let wc = rf.where_clause.as_ref().expect("expected where_clause");
        assert_eq!(wc.predicates.len(), 1);
        assert_eq!(wc.predicates[0].bounds[0].path.to_string(), "Display");
    } else {
        panic!("expected actor item");
    }
}

#[test]
fn parse_where_clause_trailing_commas() {
    for (source, expected_bounds) in [
        ("fn foo<T>(a: T) where T: Display, { }", vec!["Display"]),
        (
            "fn foo<T, U>(a: T, b: U) where T: Display, U: Clone, { }",
            vec!["Display", "Clone"],
        ),
        (
            "actor Foo { receive fn bar<T, U>(x: T) -> T where T: Display, U: Clone, { x } }",
            vec!["Display", "Clone"],
        ),
    ] {
        let result = parse(source);
        assert!(result.errors.is_empty(), "{source}: {:?}", result.errors);
        let wc = match &result.program.items[0].0 {
            Item::Function(f) => f.where_clause.as_ref().expect("expected where_clause"),
            Item::Actor(a) => a.receive_fns[0]
                .where_clause
                .as_ref()
                .expect("expected where_clause"),
            other => panic!("unexpected item: {other:?}"),
        };
        let bounds: Vec<_> = wc
            .predicates
            .iter()
            .map(|p| p.bounds[0].path.to_string())
            .collect();
        assert_eq!(bounds, expected_bounds);
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
fn arm_separator_keeps_expression_and_member_rules() {
    for (source, kind) in [
        (
            "fn f() { match n { 0 => 1 _ => 2 } }",
            ParseDiagnosticKind::ListSeparator,
        ),
        (
            "fn f() { select { x from left => x y from right => y } }",
            ParseDiagnosticKind::ListSeparator,
        ),
        (
            "fn f() { match n { 0 => {},, _ => {} } }",
            ParseDiagnosticKind::SeparatorAfterBody,
        ),
        (
            "fn f() { select { x from left => {}; after 1s => {} } }",
            ParseDiagnosticKind::SeparatorAfterBody,
        ),
        (
            "actor A { receive fn f() {}, }",
            ParseDiagnosticKind::SeparatorAfterBody,
        ),
    ] {
        let result = parse(source);
        assert!(
            result.errors.iter().any(|error| error.kind == kind),
            "{source}: {:?}",
            result.errors,
        );
    }
}

/// A block-bodied arm needs no trailing comma, and a contextual variant
/// pattern opens with `.` — the only spelling of the dotted form. The `.` after
/// the closing `}` must start the next arm rather than read as member access on
/// the block, which used to fail with "expected pattern, found `=>`".
#[test]
fn parse_match_block_arm_precedes_contextual_variant_pattern() {
    let source = "fn main() { match r { .Ok(v) => { v } .Err(e) => { 0 } } }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(function) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    let Some((Expr::Match { arms, .. }, _)) = function.body.trailing_expr.as_deref() else {
        panic!("expected trailing match expression");
    };
    assert_eq!(arms.len(), 2, "the dotted pattern must open a second arm");
    let names = arms
        .iter()
        .map(|arm| match &arm.pattern.0 {
            Pattern::ContextVariant(variant) => variant.name,
            other => panic!("expected contextual variant pattern, got {other:?}"),
        })
        .collect::<Vec<_>>();
    assert_eq!(names, [Ident::new("Ok"), Ident::new("Err")]);
}

/// Negative control for `parse_match_block_arm_precedes_contextual_variant_pattern`:
/// the restriction covers the arm body itself, so a block expression *inside*
/// the arm still takes an ordinary `.method()` postfix.
#[test]
fn block_expression_inside_a_match_arm_keeps_dot_postfix() {
    let source = "fn main() { match r { .Ok(v) => { let n = { v }.len(); n } } }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(function) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    let Some((Expr::Match { arms, .. }, _)) = function.body.trailing_expr.as_deref() else {
        panic!("expected trailing match expression");
    };
    let Expr::Block(block) = &arms[0].body.0 else {
        panic!("expected a block arm body, got {:?}", arms[0].body.0);
    };
    let Some((
        Stmt::Let {
            value: Some(value), ..
        },
        _,
    )) = block.stmts.first()
    else {
        panic!("expected a `let` statement in the arm body");
    };
    assert!(
        matches!(
            &value.0,
            Expr::MethodCall { receiver, method, .. }
                if method.0.name.as_str() == "len" && matches!(receiver.0, Expr::Block(_))
        ),
        "the block inside the arm must still take `.len()`, got {:?}",
        value.0
    );
}

#[test]
fn block_match_arm_stops_before_tuple_and_negative_patterns() {
    let source = "fn main() { match value { 0 => { println(0); } (1, _) => { println(1); } -1 => { println(-1); } _ => {} } }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(function) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    let Some((Expr::Match { arms, .. }, _)) = function.body.trailing_expr.as_deref() else {
        panic!("expected trailing match expression");
    };
    assert_eq!(arms.len(), 4);
    assert!(matches!(&arms[1].pattern.0, Pattern::Tuple(_)));
    assert!(matches!(
        &arms[2].pattern.0,
        Pattern::Literal(Literal::Integer { value: -1, .. })
    ));
}

#[test]
fn block_match_arm_continuations_keep_calls_subtraction_and_members() {
    for source in [
        "fn main() { match value { 0 => { || 1 }() _ => 0 } }",
        "fn main() { match value { 0 => { 3 } - 1 _ => 0 } }",
        "fn main() { match value { 0 => { record }.field _ => 0 } }",
    ] {
        let result = parse(source);
        assert!(
            result.errors.is_empty(),
            "errors: {:?}\n{source}",
            result.errors
        );
    }
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
            assert_eq!(f.params[0].name, Ident::new("x"));
            assert!(f.params[0].is_mutable);
        }
        other => panic!("expected function item, got: {other:?}"),
    }
}

#[test]
fn parse_labeled_while_break_continue() {
    let source = r"fn main() -> i32 {
            var i = 0;
            @outer: while i < 5 {
                var j = 0;
                while j < 5 {
                    if j == 1 { continue @outer; }
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
            assert_eq!(*label, Some(Ident::new("outer")));
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
    let Some((Expr::Ident(name), _)) = function.body.trailing_expr.as_deref() else {
        panic!("expected context reader identifier tail");
    };
    assert_eq!(*name, Ident::new("@actor_id"));
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
            assert_eq!(*label, Some(Ident::new("top")));
        } else {
            panic!("expected Loop statement");
        }
    }
}

/// `for` waits per item on its own (U383): the `for await` spelling is gone.
/// It is one `E_FOR_AWAIT` diagnostic whose fix-it deletes the word, and the
/// loop behind it still parses, labelled or not, so nothing cascades.
#[test]
fn parse_for_await_is_one_diagnostic() {
    for source in [
        r"fn main() {
            for await item in stream {
                println(item);
            }
        }",
        r"fn main() {
            @outer: for await item in stream {
                println(item);
            }
        }",
    ] {
        let result = parse(source);
        assert_eq!(result.errors.len(), 1, "got {:?}", result.errors);
        assert_eq!(result.errors[0].kind.as_kind_str(), "E_FOR_AWAIT");

        let Item::Function(main) = &result.program.items[0].0 else {
            panic!("expected fn main");
        };
        assert!(matches!(main.body.stmts[0].0, Stmt::For { .. }));
    }
}

#[test]
fn parse_for_over_a_stream_loop() {
    let source = r"fn main() {
            for item in stream {
                println(item);
            }
        }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    if let Item::Function(ref f) = result.program.items[0].0 {
        assert!(
            matches!(&f.body.stmts[0].0, Stmt::For { .. }),
            "expected For statement"
        );
    } else {
        panic!("expected Function item");
    }
}

#[test]
fn async_does_not_mark_a_callable() {
    for source in [
        "async fn fetch() -> i32 { 42 }",
        "async gen fn count_up() -> i32 { yield 1; yield 2; }",
    ] {
        assert!(!parse(source).errors.is_empty(), "{source} must not parse");
    }
}

#[test]
fn async_is_accepted_as_an_identifier() {
    let source = "fn async(async: i32) -> i32 { async }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(function) = &result.program.items[0].0 else {
        panic!("expected Function item");
    };
    assert_eq!(function.name, Ident::new("async"));
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
fn parse_negative_i64_min_literal_pattern() {
    // A pattern has no operator position, so `-<digits>` folds into a single
    // signed literal here. The `i128` carrier holds i64::MIN exactly.
    let source = "fn main() { match x { -9223372036854775808 => 0, _ => 1, } }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::Function(func) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    let Some((Expr::Match { arms, .. }, _)) = func.body.trailing_expr.as_deref() else {
        panic!("expected trailing match expression");
    };
    let (Pattern::Literal(Literal::Integer { value, radix }), _) = &arms[0].pattern else {
        panic!("expected literal integer pattern");
    };
    assert_eq!(*value, i128::from(i64::MIN));
    assert_eq!(*radix, IntRadix::Decimal);
}

#[test]
fn parse_pattern_contextual_keywords() {
    // All contextual keywords that can appear as identifiers in patterns.
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
        assert_eq!(
            name.name.as_str(),
            *kw,
            "pattern name mismatch for keyword '{kw}'"
        );
    }
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
fn parse_string_literals_preserve_embedded_nul() {
    for source in [
        r#"fn main() { let s = "a\0b"; }"#.to_string(),
        r#"fn main() { let s = "a\x00b"; }"#.to_string(),
        r#"fn main() { let s = "a\u{0}b"; }"#.to_string(),
        "fn main() { let s = \"a\0b\"; }".to_string(),
        "fn main() { let s = r\"a\0b\"; }".to_string(),
    ] {
        let result = parse(&source);
        assert!(result.errors.is_empty(), "{source:?}: {:?}", result.errors);
        let Item::Function(function) = &result.program.items[0].0 else {
            panic!("expected function");
        };
        let Stmt::Let {
            value: Some((Expr::Literal(Literal::String(value)), _)),
            ..
        } = &function.body.stmts[0].0
        else {
            panic!("expected string binding");
        };
        assert_eq!(value.as_bytes(), b"a\0b");
    }
}

#[test]
fn parse_interpolated_strings_preserve_nul_and_expression_parts() {
    for source in [
        r#"fn main() { let s = f"a\0{name}"; }"#.to_string(),
        "fn main() { let s = f\"a\0{name}\"; }".to_string(),
        r#"fn main() { let s = f"a\x00{name}"; }"#.to_string(),
    ] {
        let result = parse(&source);
        assert!(result.errors.is_empty(), "{source:?}: {:?}", result.errors);
        let Item::Function(function) = &result.program.items[0].0 else {
            panic!("expected function");
        };
        let Stmt::Let {
            value: Some((Expr::InterpolatedString(parts), _)),
            ..
        } = &function.body.stmts[0].0
        else {
            panic!("expected interpolated string binding");
        };
        assert!(
            matches!(&parts[..], [StringPart::Literal(text), StringPart::Expr((Expr::Ident(name), _))]
            if text.as_bytes() == b"a\0" && name.name.as_str() == "name")
        );
    }
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
fn interpolation_rejects_unconsumed_input_at_its_source_span() {
    for (expression, expected_tail) in [
        ("x garbage", "garbage"),
        ("x; y", "; y"),
        ("x )", ")"),
        ("x 7 8", "7 8"),
        ("1e21", "e21"),
        ("x:04", ":04"),
    ] {
        let source = format!("fn main() {{ let s = f\"é {{{expression}}}\"; }}");
        let result = parse(&source);
        assert_eq!(result.errors.len(), 1, "{expression}: {:?}", result.errors);
        assert_eq!(
            &source[result.errors[0].span.clone()],
            expected_tail,
            "the complete tail must retain its byte offset after a Unicode prefix"
        );
    }
    let result = parse(r#"fn main() { let s = f"{1.0e21} {value:?} {call(1 + 2)}"; }"#);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_interpolated_string_structural_format_part() {
    let result = parse(r#"fn main() { let s = f"value={item:?}"; }"#);
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
    assert!(matches!(
        parts.as_slice(),
        [StringPart::Literal(prefix), StringPart::StructuralExpr((Expr::Ident(name), _))]
            if prefix == "value=" && name.name.as_str() == "item"
    ));
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
        StringPart::Expr((Expr::Ident(ref name), _)) if name.name.as_str() == "name"
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

fn interpolation_errors(source: &str) -> Vec<String> {
    parse(source)
        .errors
        .into_iter()
        .map(|error| source[error.span].to_string())
        .collect()
}

#[test]
fn parse_interpolated_string_refuses_format_specifiers_other_than_debug() {
    for (spec, expected) in [
        ("x:<8", ":<8"),
        ("x:08", ":08"),
        ("f:.2", ":.2"),
        ("x:x", ":x"),
        ("x :<8", ":<8"),
    ] {
        let source = format!(r#"fn main() {{ let s = f"[{{{spec}}}]"; }}"#);
        assert_eq!(
            interpolation_errors(&source),
            [expected.to_string()],
            "{spec}"
        );
    }
}

#[test]
fn parse_interpolated_string_keeps_colons_inside_the_expression() {
    for inner in [
        " x ",
        "x :?",
        "xs.iter().map(|k: i64| k + 1).count()",
        "Point { x: 1, y: 2 }:?",
        r#"label("a:b")"#,
        r#"if x > 1 { "a" } else { "b" }"#,
    ] {
        let source = format!(r#"fn main() {{ let s = f"[{{{inner}}}]"; }}"#);
        assert!(interpolation_errors(&source).is_empty(), "{inner}");
    }
}

#[test]
fn parse_nested_blocks_past_limit_terminates_with_bounded_errors() {
    // Past MAX_DEPTH a statement-start block is refused without consuming
    // input; the block loop must skip it rather than retry it forever.
    let errors = std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(|| {
            let source = format!("fn main() {}", "{".repeat(400));
            parse(&source).errors
        })
        .expect("failed to spawn thread")
        .join()
        .expect("thread panicked");
    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("maximum nesting depth exceeded")),
        "{errors:?}"
    );
    assert!(errors.len() < 2_000, "{} errors", errors.len());
}

#[test]
fn long_prefix_operator_runs_hit_the_nesting_limit() {
    // Each prefix operator nests its operand one level, like a parenthesis,
    // so a run of them is refused at MAX_DEPTH instead of recursing once per
    // operator until the stack runs out.
    for prefix in ["-", "!", "~", "& ", "clone "] {
        let errors = std::thread::Builder::new()
            .stack_size(16 * 1024 * 1024)
            .spawn(move || {
                let source = format!("fn main() {{ let _x = {}1; }}", prefix.repeat(100_000));
                parse(&source).errors
            })
            .expect("failed to spawn thread")
            .join()
            .expect("thread panicked");
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("maximum nesting depth exceeded")),
            "{prefix}: {errors:?}"
        );
    }
}

#[test]
fn an_overlong_integer_literal_is_quoted_by_its_start() {
    let digits = "1".repeat(100_000);
    let errors = parse(&format!("fn main() {{ let _x = {digits}; }}")).errors;
    let message = &errors
        .first()
        .expect("an invalid literal is refused")
        .message;
    assert!(
        message.starts_with("invalid integer literal '1111"),
        "{message}"
    );
    assert!(message.len() < 100, "{message}");
    let errors = parse("fn main() { let _x = 99999999999999999999999999999999999999999; }").errors;
    assert_eq!(errors[0].kind, ParseDiagnosticKind::InvalidLiteral);
    assert!(errors[0].message.contains('…'), "{}", errors[0].message);
    assert!(errors[0].message.len() < 100, "{}", errors[0].message);
}

#[test]
fn prefix_operator_runs_below_the_limit_still_parse() {
    let errors = std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(|| parse(&format!("fn main() {{ let _x = {}1; }}", "- ".repeat(48))).errors)
        .expect("failed to spawn thread")
        .join()
        .expect("thread panicked");
    assert!(errors.is_empty(), "errors: {errors:?}");
}

#[test]
fn expression_nesting_below_limit_still_parses() {
    let errors = std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(|| {
            let open = "(".repeat(48);
            let close = ")".repeat(48);
            let source = format!("fn main() -> i32 {{ {open}1{close} }}");
            parse(&source).errors
        })
        .expect("failed to spawn thread")
        .join()
        .expect("thread panicked");
    assert!(errors.is_empty(), "errors: {errors:?}");
}

#[test]
fn parse_syntax_error_smoke() {
    for source in [
        "fn main() { let x = `; }",
        "fn main() -> i32 {\n    let x = 42;\n    0\n",
        "fn main() {\n    let x = 42 + + + ;\n}",
        "fn main() {\n    println(\"hello world);\n}",
        "fn main() {\n    let x = ;\n}",
        "fn main() {\n    let x = 42\n    let y = 10;\n}",
        "fn fn main() {}",
    ] {
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse error for: {source}"
        );
    }
}

#[test]
fn parse_valid_syntax_smoke() {
    let empty = parse("");
    assert!(empty.errors.is_empty());
    assert!(empty.program.items.is_empty());

    let multi = parse("fn foo() {} fn bar() {} type Baz {}");
    assert!(multi.errors.is_empty(), "errors: {:?}", multi.errors);
    assert_eq!(multi.program.items.len(), 3);

    for source in [
        "type Point {\n    x: i32;\n    y: i32;\n}\n",
        "trait Printable { fn print(self); }",
        "fn main() { let result = if x > 0 { x } else { -x }; }",
        "fn main() { match opt { Some(x) => x, None => 0, } }",
        "fn classify(n: i32) -> i32 { match n { 1 | 2 | 3 => 1, _ => 0, } }",
        "fn main() { match x { 1_000 => 1, _ => 0, } }",
        "fn main() { let v: Vec<Vec<i32>> = Vec.new(); }",
        "fn main() { let v: HashMap<string, Vec<Vec<i32>>> = HashMap.new(); }",
        "fn main() { a.b().c().d(); }",
        "fn main() { x.filter(1).map(2).collect(); }",
        "fn main() -> i32 { (a + b) * (c - d) / e }",
        r#"fn main() { let s = ""; }"#,
        "fn main() -> i32 { if a > 0 { if b > 0 { 1 } else { 2 } } else { 3 } }",
        "fn main() { match p { Point { x, y } => x + y, _ => 0, } }",
        "fn main() { let t = (1, 2, 3); }",
        "fn main() { let a = [1, 2, 3]; }",
        "fn main() { let a = -x; let b = !flag; }",
        "fn main() -> bool { a < b && b > c || d == e && f != g }",
        "fn main() { let x = 1_000.5; }",
        r#"fn main() { let x = r"hello\nworld"; }"#,
    ] {
        let result = parse(source);
        assert!(result.errors.is_empty(), "{source}: {:?}", result.errors);
    }
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
    let source = r"actor Worker {
    #[on(stop)]
    fn shutdown() { stop(); }
    #[on(crash)]
    fn on_crash() { }
    #[on(upgrade)]
    fn on_upgrade() { }

    #[every(50ms)]
    receive fn tick() { work(); }
    #[wire]
    receive fn fetch(key: string) -> string { key }
}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Actor(actor) = &result.program.items[0].0 else {
        panic!("expected actor");
    };
    let hook_args: Vec<_> = actor
        .methods
        .iter()
        .map(|m| {
            (
                m.attributes[0].name.as_str(),
                m.attributes[0].args[0].as_str(),
            )
        })
        .collect();
    assert_eq!(
        hook_args,
        [("on", "stop"), ("on", "crash"), ("on", "upgrade")]
    );
    let recv_attrs: Vec<_> = actor
        .receive_fns
        .iter()
        .map(|r| r.attributes[0].name.as_str())
        .collect();
    assert_eq!(recv_attrs, ["every", "wire"]);
}

#[test]
fn parse_attribute_key_value_missing_value_emits_error_without_empty_fallback() {
    let source = r"
#[export(rename = , version = 2)]
fn demo() {}
";
    let result = parse(source);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);

    let Item::Function(func) = &result.program.items[0].0 else {
        panic!("expected function");
    };
    assert_eq!(func.attributes.len(), 1);
    assert_eq!(func.attributes[0].name, "export");
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
fn parse_integer_literals_and_radices() {
    for (lit, expected_val, expected_radix) in [
        (
            "9_223_372_036_854_775_807",
            i128::from(i64::MAX),
            IntRadix::Decimal,
        ),
        ("0xFF", 255, IntRadix::Hex),
        ("0b1010", 10, IntRadix::Binary),
        ("0o77", 63, IntRadix::Octal),
    ] {
        let result = parse(&format!("fn main() -> i64 {{ {lit} }}"));
        assert!(result.errors.is_empty(), "{lit}: {:?}", result.errors);
        let Item::Function(f) = &result.program.items[0].0 else {
            panic!("expected function");
        };
        let Some((Expr::Literal(Literal::Integer { value, radix }), _)) =
            f.body.trailing_expr.as_deref()
        else {
            panic!("expected integer literal for {lit}");
        };
        assert_eq!(*value, expected_val, "{lit}");
        assert_eq!(*radix, expected_radix, "{lit}");
    }
}

#[test]
fn parse_negative_i64_min_literal_folds_to_bare_literal() {
    // `-9223372036854775808` in expression position stays a unary negation
    // over the exact magnitude: the `i128` carrier represents it, so the
    // parser has no reason to fold early. HIR folds it once the checker has
    // supplied the concrete width.
    let source = "fn main() -> i64 { -9223372036854775808 }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(f) = &result.program.items[0].0 else {
        panic!("expected function item");
    };
    let Some(boxed) = &f.body.trailing_expr else {
        panic!("expected trailing expr");
    };
    let (
        Expr::Unary {
            op: crate::ast::UnaryOp::Negate,
            operand,
        },
        _,
    ) = boxed.as_ref()
    else {
        panic!("expected unary negation, got {boxed:?}");
    };
    let (Expr::Literal(Literal::Integer { value, .. }), _) = operand.as_ref() else {
        panic!("expected integer literal operand, got {operand:?}");
    };
    assert_eq!(*value, 9_223_372_036_854_775_808_i128);
}

#[test]
fn parse_negative_literal_with_postfix_keeps_unary_wrapper() {
    // The fold must not fire when a postfix chain follows the literal
    // token: `-5.wrapping_as_i32()` is `Negate(MethodCall(5, ..))`, not
    // `MethodCall(Negate(5), ..)` -- postfix already binds tighter than
    // prefix negate in this grammar.
    let source = "fn main() -> i32 { -5.wrapping_as_i32() }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Function(f) = &result.program.items[0].0 {
        if let Some(boxed) = &f.body.trailing_expr {
            let (expr, _) = boxed.as_ref();
            assert!(
                matches!(
                    expr,
                    Expr::Unary {
                        op: UnaryOp::Negate,
                        ..
                    }
                ),
                "expected top-level Unary{{Negate}}, got {expr:?}"
            );
        } else {
            panic!("expected trailing expr");
        }
    } else {
        panic!("expected function item");
    }
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
fn submission_is_an_ordinary_call_and_send_is_an_ordinary_name() {
    let submitted =
        parse("fn main() { let outcome = policy(worker, on_full: .Wait).process(job); }");
    assert!(submitted.errors.is_empty(), "{:?}", submitted.errors);
    assert!(
        matches!(first_let_value(&submitted), Expr::MethodCall { method, .. } if method.0.name.as_str() == "process")
    );
    for source in [
        "fn main() { let outcome = channel.send(value); }",
        "fn main() { let outcome = send(value); }",
    ] {
        let call = parse(source);
        assert!(call.errors.is_empty(), "{:?}", call.errors);
        assert!(matches!(
            first_let_value(&call),
            Expr::Call { .. } | Expr::MethodCall { .. }
        ));
    }
    // `send` carries no prefix meaning any more; two expressions in a row is
    // the ordinary parse error for an unknown juxtaposition.
    let prefixed = parse("fn main() { let outcome = send worker.process(job); }");
    assert!(!prefixed.errors.is_empty());
}

#[test]
fn parse_clone_prefix_expression() {
    let source = "fn main() { let a = clone x; }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    match first_let_value(&result) {
        Expr::Clone(operand) => {
            assert!(
                matches!(&operand.0, Expr::Ident(name) if name.name.as_str() == "x"),
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
            matches!(&operand.0, Expr::FieldAccess { field, .. } if field.0.name.as_str() == "field"),
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
    match first_let_value(&result).as_binary_pair() {
        Some((left, op, _)) => {
            assert_eq!(op, BinaryOp::Add);
            assert!(
                matches!(&left.0, Expr::Clone(_)),
                "left of `+` should be the clone, got: {:?}",
                left.0
            );
        }
        None => panic!("expected Expr::Binary with a clone on the left"),
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
            matches!(&function.0, Expr::Ident(name) if name.name.as_str() == "clone"),
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
}

#[test]
fn parse_import_statement() {
    let source = "import std.fs;";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        assert_eq!(imp.path.to_string(), "std.fs");
    } else {
        panic!("expected import");
    }
}

#[test]
fn parse_dotted_import_with_self_and_aliases() {
    let source = "import app.net.http.{self as transport, Client as C};";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Import(import) = &result.program.items[0].0 else {
        panic!("expected import");
    };
    assert_eq!(import.path.to_string(), "app.net.http");
    let Some(ImportSpec::Names(names)) = &import.spec else {
        panic!("expected selected names");
    };
    assert_eq!(names[0].name, Ident::new("self"));
    assert_eq!(names[0].alias, Some(Ident::new("transport")));
    assert_eq!(names[1].name, Ident::new("Client"));
    assert_eq!(names[1].alias, Some(Ident::new("C")));
}

#[test]
fn dotted_import_glob_and_empty_selection_are_rejected() {
    let glob = parse("import app.net.*;");
    assert!(glob
        .errors
        .iter()
        .any(|error| matches!(error.kind, ParseDiagnosticKind::ImportGlobRemoved)));

    let empty = parse("import app.net.{};");
    assert!(!empty.errors.is_empty(), "{:?}", empty.errors);
}

#[test]
fn hyphenated_package_import_is_rejected_at_the_separator() {
    let source = "import config-telemetry.types;";
    let result = parse(source);
    assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
    assert_eq!(&source[result.errors[0].span.clone()], "-");
}

#[test]
fn parse_import_type_decl_keyword_path_segments() {
    // Every type-declaration keyword is a plausible module file name and
    // must parse as an import path segment.
    for kw in [
        "actor",
        "machine",
        "record",
        "enum",
        "supervisor",
        "type",
        "trait",
    ] {
        let source = format!("import src.{kw}.helpers;");
        let result = parse(&source);
        assert!(
            result.errors.is_empty(),
            "segment `{kw}` should parse, errors: {:?}",
            result.errors
        );
        if let Item::Import(imp) = &result.program.items[0].0 {
            assert_eq!(imp.path.to_string(), format!("src.{kw}.helpers"));
        } else {
            panic!("expected import for segment `{kw}`");
        }
    }
    let machine_sel = parse("import src.workflow.machine.{Machine};");
    assert!(machine_sel.errors.is_empty(), "{:?}", machine_sel.errors);
    let Item::Import(imp) = &machine_sel.program.items[0].0 else {
        panic!("expected import");
    };
    assert_eq!(imp.path.to_string(), "src.workflow.machine");
    let Some(ImportSpec::Names(names)) = &imp.spec else {
        panic!("expected selective import spec");
    };
    assert_eq!(names[0].name, Ident::new("Machine"));
}

#[test]
fn import_path_segment_text_agrees_with_import_parsing() {
    for segment in ["three_d", "type", "actor", "after", "_private"] {
        assert!(is_import_path_segment(segment), "{segment}");
        let result = parse(&format!("import acme.{segment};"));
        assert!(result.errors.is_empty(), "{segment}: {:?}", result.errors);
    }
    for segment in ["3d", "", "a b", " a", "a.b", "a-b", "a//"] {
        assert!(!is_import_path_segment(segment), "{segment:?}");
    }
    assert!(!parse("import acme.3d;").errors.is_empty());
}

#[test]
fn machine_qualifier_in_type_position_rejected() {
    // The import-path carve-out does not extend to qualifier position:
    // `machine.Machine` as a type annotation stays reserved.
    let source = "fn f(m: machine.Machine) {}";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `machine` qualifier in type position"
    );
}

#[test]
fn parse_bare_self_in_free_fn_is_error() {
    let source = "fn print(self) {}";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for bare `self` in free function"
    );
}

#[test]
fn parse_typed_self_is_error() {
    let source = "type Foo { x: int; } impl Foo { fn bar(self: Foo) -> int { 0 } }";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `self: Type` parameter"
    );
}

// Duration literal parsing
#[test]
fn decimal_duration_precision_and_range() {
    for (literal, expected) in [
        ("0.25s", Some(250_000_000)),
        ("1.5ms", Some(1_500_000)),
        ("0.1us", Some(100)),
        ("0.0000000000025h", Some(9)),
        ("2_000.000_001ms", Some(2_000_000_001)),
        ("0.0000000000000000000000000000000000000000000s", Some(0)),
        ("9223372036.854775807s", Some(i64::MAX)),
        ("-9223372036.854775808s", Some(i64::MIN)),
        ("9223372036.854775808s", None),
        ("-9223372036.854775809s", None),
        ("0.1ns", None),
        ("0.0000000001s", None),
        ("0.0000000000000000000000000000000000000000001s", None),
        ("250", None),
    ] {
        assert_eq!(parse_duration_ns(literal), expected, "{literal}");
    }

    let result = parse("fn main() { let quarter = 0.25s; let negative = -1.5ms; }");
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    let refused = parse("fn main() { let too_precise = 0.1ns; }");
    assert!(!refused.errors.is_empty());
}

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
    assert!(!result.errors.is_empty(), "{:?}", result.errors);
    // Verify that `extern` still works for the same purpose
    let source = "extern \"C\" { fn ext_add(a: i32, b: i32) -> i32; fn ext_print(msg: string); }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    assert_eq!(result.program.items.len(), 1);
}

// ── extern "rt" block tests ──────────────────────────────────────────

#[test]
fn extern_abi_blocks_and_fn_body_refusal() {
    for (source, expected_abi, expected_fns) in [
        ("extern \"rt\" { }", "rt", vec![]),
        (
            "extern \"rt\" { fn println(s: string); }",
            "rt",
            vec!["println"],
        ),
        (
            "extern \"rt\" { fn println(s: string); fn print(s: string); fn assert(cond: bool); }",
            "rt",
            vec!["println", "print", "assert"],
        ),
        ("extern \"xyz\" { fn foo(); }", "xyz", vec!["foo"]),
    ] {
        let result = parse(source);
        assert!(result.errors.is_empty(), "{source}: {:?}", result.errors);
        let Item::ExternBlock(block) = &result.program.items[0].0 else {
            panic!("expected ExternBlock");
        };
        assert_eq!(block.abi, expected_abi);
        let names: Vec<_> = block
            .functions
            .iter()
            .map(|f| f.name.name.as_str())
            .collect();
        assert_eq!(names, expected_fns);
    }

    let result = parse("extern \"rt\" { fn println(s: string) { todo() } }");
    assert!(!result.errors.is_empty(), "{:?}", result.errors);
}

#[test]
fn join_is_an_ordinary_identifier() {
    // `join` is retired as a keyword: `join(a, b)` now parses as a call to
    // an ordinary function named `join`, not the removed batch-wait form.
    let source = "fn main() { let r = join(a, b); }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
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
                matches!(&val.0, Expr::Call { function, .. }
                    if matches!(&function.0, Expr::Ident(name) if name.name.as_str() == "join")),
                "expected a call to identifier `join`, got {:?}",
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
    let source = r"import std.net.{http as h, websocket as ws};";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        assert_eq!(imp.path.to_string(), "std.net");
        if let Some(ImportSpec::Names(names)) = &imp.spec {
            assert_eq!(names.len(), 2);
            assert_eq!(names[0].name, Ident::new("http"));
            assert_eq!(names[0].alias, Some(Ident::new("h")));
            assert_eq!(names[1].name, Ident::new("websocket"));
            assert_eq!(names[1].alias, Some(Ident::new("ws")));
        } else {
            panic!("expected ImportSpec::Names, got {:?}", imp.spec);
        }
    } else {
        panic!("expected import item");
    }
}

#[test]
fn parse_import_whole_module_alias() {
    // `import path::to::mod as alias;` sets module_alias; spec stays None.
    let source = r"import std.net as n;";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        assert_eq!(imp.path.to_string(), "std.net");
        assert!(imp.spec.is_none(), "whole-module alias keeps spec None");
        assert_eq!(imp.module_alias, Some(Ident::new("n")));
    } else {
        panic!("expected import item");
    }
}

#[test]
fn parse_file_import_alias() {
    // `import "file.hew" as alias;` keeps the file path and binds the alias.
    let result = parse(r#"import "codecs/huffman.hew" as huffman;"#);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Import(imp) = &result.program.items[0].0 else {
        panic!("expected import item");
    };
    assert_eq!(imp.file_path.as_deref(), Some("codecs/huffman.hew"));
    assert_eq!(imp.module_alias, Some(Ident::new("huffman")));
    assert!(!imp.publishes_file_items());
}

#[test]
fn parse_import_no_module_alias_is_none() {
    // A plain whole-module import leaves module_alias unset.
    let source = r"import std.net;";
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
    let source = r"import mymod.{ Foo } as f;";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for aliasing a brace import"
    );
}

#[test]
fn parse_import_alias_of_glob_rejected() {
    // Aliasing a glob has no meaning and is a parse error.
    let source = r"import mymod.* as g;";
    let result = parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for aliasing a glob import"
    );
}

#[test]
fn parse_import_no_alias_preserves_name() {
    // Names without `as` should have alias = None
    let source = r"import mymod.{foo, bar};";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        if let Some(ImportSpec::Names(names)) = &imp.spec {
            assert_eq!(names.len(), 2);
            assert_eq!(names[0].name, Ident::new("foo"));
            assert!(names[0].alias.is_none());
            assert_eq!(names[1].name, Ident::new("bar"));
            assert!(names[1].alias.is_none());
        } else {
            panic!("expected ImportSpec::Names");
        }
    } else {
        panic!("expected import item");
    }
}

#[test]
fn parse_import_actor_path_segment() {
    let source = "import std.actor.monitor;";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    if let Item::Import(imp) = &result.program.items[0].0 {
        assert_eq!(imp.path.to_string(), "std.actor.monitor");
    } else {
        panic!("expected import");
    }
}

#[test]
fn parse_generic_lambda_in_arg_position_rejected() {
    let source = "fn main() { apply(<T>(x: T) => x, 5); }";
    let result = parse(source);
    assert!(
        result.errors.iter().any(|e| {
            matches!(e.kind, ParseDiagnosticKind::ClosurePipeSyntax)
                && e.message.contains("E_CLOSURE_PIPE_SYNTAX")
        }),
        "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda in arg position, got: {:?}",
        result.errors
    );
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
    let r = parse("package type Point {\n    x: i32;\n    y: i32;\n}\n");
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
fn wire_fields_preserve_docs_on_either_side_of_attributes() {
    let result = parse(
        "#[wire] type Message {\n\
         /// Public identifier.\n\
         #[serial(key = \"id\")] identifier: i64 @1;\n\
         #[serial(key = \"body\")]\n\
         /// Message body.\n\
         body: string @2;\n\
         }",
    );
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected wire record");
    };
    for (item, expected) in decl
        .body
        .iter()
        .zip(["Public identifier.", "Message body."])
    {
        let TypeBodyItem::Field { doc_comment, .. } = item else {
            panic!("expected wire field");
        };
        assert_eq!(doc_comment.as_deref(), Some(expected));
    }
    let wire = decl.wire.as_ref().expect("wire metadata");
    assert_eq!(wire.field_meta.len(), 2);
    assert_eq!(wire.field_meta[0].field_number, 1);
    assert_eq!(wire.field_meta[1].field_number, 2);
}

#[test]
fn parse_wire_struct_preserves_since_modifier() {
    let source = "\
#[wire]
type Msg {
    added: Option<String> @2 optional since 2;
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
    assert_eq!(meta.since, Some(2));
}

#[test]
fn wire_attr_on_type_declaration_produces_wire_metadata() {
    let source = "\
#[wire]
type Point {
    x: i64 @1;
    y: i64 @2;
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
fn serial_case_and_field_key_parse_on_wire_types() {
    let source = "\
#[wire]
#[serial(case = \"camelCase\")]
type Msg {
    #[serial(key = \"addedName\")]
    added: Option<String> @2 optional;
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    assert_eq!(decl.serial_case, Some(NamingCase::CamelCase));
    let TypeBodyItem::Field { attributes, .. } = &decl.body[0] else {
        panic!("expected a field");
    };
    assert_eq!(
        crate::ast::SerialField::of(attributes).key.as_deref(),
        Some("addedName")
    );
    let meta = &decl
        .wire
        .as_ref()
        .expect("expected wire metadata")
        .field_meta[0];
    assert_eq!(meta.field_number, 2);
    assert!(meta.is_optional);
}

/// `#[wire] enum E { A; B; C; }` — unit-only variants attach wire metadata
/// at the type level (version / naming-cases) and produce
/// `Item::TypeDecl { kind: Enum, wire: Some(_) }` with assigned variant tags.
#[test]
fn parse_wire_enum_unit_variants() {
    let source = "\
#[wire]
enum Command {
    Start @0;
    Stop @1;
    Pause @2;
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    assert_eq!(decl.kind, TypeDeclKind::Enum);
    assert_eq!(decl.name, Ident::new("Command"));
    let wire = decl.wire.as_ref().expect("expected wire metadata on enum");
    assert!(
        wire.field_meta.is_empty(),
        "variant tags live on the variants"
    );
    assert!(wire.reserved.is_empty());
    assert_eq!(decl.body.len(), 3);
    for item in &decl.body {
        match item {
            TypeBodyItem::Variant(v) => assert!(matches!(v.kind, VariantKind::Unit)),
            TypeBodyItem::Field { .. } => panic!("expected variant, got {item:?}"),
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
    V1 { x: i64;  } @0;
    V2 { y: String; z: bool;  } @1;
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
    assert_eq!(v1.name, Ident::new("V1"));
    assert!(matches!(v1.kind, VariantKind::Struct(ref fs) if fs.len() == 1));
    let TypeBodyItem::Variant(v2) = &decl.body[1] else {
        panic!("expected variant V2");
    };
    assert_eq!(v2.name, Ident::new("V2"));
    assert!(matches!(v2.kind, VariantKind::Struct(ref fs) if fs.len() == 2));
}

/// `#[wire] enum E { A(i64); B(String, bool) }` — tuple-payload variants.
#[test]
fn parse_wire_enum_tuple_payload_variants() {
    let source = "\
#[wire]
enum Op {
    Push(i64) @0;
    Pair(String, bool) @1;
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
    A @0;
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

/// A `#[wire]` enum takes `#[serial(case)]` and a stable tag per variant.
#[test]
fn parse_wire_enum_preserves_serial_case_and_variant_tags() {
    let source = "\
#[wire]
#[serial(case = \"kebab-case\")]
enum Command {
    Start @3;
    Stop(i64) @7;
    Move { x: i64; } @9;
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    assert_eq!(decl.serial_case, Some(NamingCase::KebabCase));
    let tags: Vec<_> = decl
        .body
        .iter()
        .filter_map(|item| match item {
            TypeBodyItem::Variant(variant) => variant.tag,
            TypeBodyItem::Field { .. } => None,
        })
        .collect();
    assert_eq!(tags, [3, 7, 9]);
}

/// Retired serial spellings are refused with the kinds `hew fmt --migrate`
/// rewrites; untagged wire variants receive ordinary automatic tags.
#[test]
fn legacy_serial_spellings_are_refused_and_implicit_tags_are_assigned() {
    let source = "\
#[wire]
#[yaml(\"kebab-case\")]
#[json(camelCase)]
type Msg {
    added: String @4 json(\"added_name\") yaml(\"other\");
}

#[wire]
enum Command {
    Start;
    Stop;
}
";
    let result = parse(source);
    let kinds: Vec<_> = result
        .errors
        .iter()
        .map(|error| error.kind.clone())
        .collect();
    assert_eq!(
        kinds,
        [
            ParseDiagnosticKind::LegacySerialSpelling,
            ParseDiagnosticKind::LegacySerialSpelling,
            ParseDiagnosticKind::LegacySerialSpelling,
            ParseDiagnosticKind::LegacySerialSpelling,
        ],
        "{:?}",
        result.errors
    );
    let Item::TypeDecl(msg) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };

    assert_eq!(msg.serial_case, Some(NamingCase::KebabCase));
    let TypeBodyItem::Field { attributes, .. } = &msg.body[0] else {
        panic!("expected a field");
    };
    assert_eq!(
        crate::ast::SerialField::of(attributes).key.as_deref(),
        Some("added_name")
    );
    let migrated = crate::fmt::migrate_syntax(source).expect("migratable");
    assert!(
        migrated.contains("#[serial(case = \"kebab-case\")]"),
        "{migrated}"
    );
    assert!(!migrated.contains("#[json"), "{migrated}");
    assert!(
        migrated.contains("#[serial(key = \"added_name\")]"),
        "{migrated}"
    );
    assert!(
        migrated.contains("Start;") && migrated.contains("Stop;"),
        "{migrated}"
    );
    let reparsed = parse(&migrated);
    assert!(reparsed.errors.is_empty());
    let Item::TypeDecl(command) = &reparsed.program.items[1].0 else {
        panic!("expected command enum");
    };
    let tags: Vec<_> = command
        .body
        .iter()
        .filter_map(|item| match item {
            TypeBodyItem::Variant(variant) => Some((variant.tag, variant.tag_explicit)),
            TypeBodyItem::Field { .. } => None,
        })
        .collect();
    assert_eq!(tags, [(Some(0), false), (Some(1), false)]);
}

#[test]
fn invalid_serial_arguments_are_refused() {
    for source in [
        "#[serial(case = \"camel\")] type A { x: i64; }",
        "#[serial(rename = \"x\")] type A { x: i64; }",
        "type A { #[serial(key)] x: i64; }",
        "type A { #[serial(case = \"camelCase\")] x: i64; }",
    ] {
        let result = parse(source);
        assert!(
            result
                .errors
                .iter()
                .any(|error| error.kind == ParseDiagnosticKind::AttributeArgument),
            "{source}: {:?}",
            result.errors
        );
    }
    assert!(parse(
        "type A { #[serial(key = \"type\")] kind: string; #[serial(skip)] cached: Option<i64>; }"
    )
    .errors
    .is_empty());
}

#[test]
fn generic_wire_declarations_preserve_bounds_and_where_clauses() {
    for source in [
        "#[wire] type Box<T: Clone> where T: Serializable { value: T; }",
        "#[wire] enum Choice<T: Clone> where T: Serializable { Empty; Value(T); }",
    ] {
        let result = parse(source);
        assert!(result.errors.is_empty(), "{source}: {:?}", result.errors);
        let Item::TypeDecl(decl) = &result.program.items[0].0 else {
            panic!("expected wire declaration");
        };
        let params = decl.type_params.as_ref().expect("generic parameters");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].name, Ident::new("T"));
        assert_eq!(params[0].bounds.len(), 1);
        assert_eq!(
            decl.where_clause
                .as_ref()
                .expect("where clause")
                .predicates
                .len(),
            1
        );
        assert!(decl.wire.is_some());
    }
}

#[test]
fn wire_numbers_skip_explicit_and_reserved_members_regardless_of_order() {
    let result = parse(
        "\
#[wire]
type Packet<T> {
    first: T;
    fixed: i64 @1;
    last: T;
    reserved @2;
}
#[wire]
enum Command<T> {
    Start;
    Fixed @0;
    reserved @2, @4;
    Value(T);
    Named { value: T; }
}
",
    );
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    let Item::TypeDecl(record) = &result.program.items[0].0 else {
        panic!("expected record");
    };
    let numbers = record
        .wire
        .as_ref()
        .expect("wire record")
        .field_meta
        .iter()
        .map(|field| field.field_number)
        .collect::<Vec<_>>();
    assert_eq!(numbers, [3, 1, 4]);
    let Item::TypeDecl(enumeration) = &result.program.items[1].0 else {
        panic!("expected enum");
    };
    let tags = enumeration
        .body
        .iter()
        .filter_map(|item| match item {
            TypeBodyItem::Variant(variant) => variant.tag,
            TypeBodyItem::Field { .. } => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(tags, [1, 0, 3, 5]);
    assert_eq!(
        enumeration.wire.as_ref().expect("wire enum").reserved[0].numbers,
        [2, 4]
    );
}

#[test]
fn wire_number_conflicts_are_refused_even_when_reservation_follows_member() {
    for source in [
        "#[wire] type Packet { a: i64 @3; b: i64 @3; }",
        "#[wire] enum Command { A @3; B @3; }",
        "#[wire] type Packet { a: i64 @3; reserved @3; }",
        "#[wire] enum Command { A @3; reserved @3; }",
    ] {
        let result = parse(source);
        assert!(!result.errors.is_empty(), "{:?}", result.errors);
    }
}

#[test]
fn wire_machine_preserves_schema_versions_without_state_or_event_tags() {
    let result = parse(
        "\
#[wire(version = 4, min_version = 2)]
pub machine Cache<T> where T: Serializable {
    events { Store { value: T; } Clear; }
    state Empty;
    state Full { value: T; }
    on Store: _ => Full { value: event.value }
    on Clear: _ => Empty;
}
",
    );
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    let Item::Machine(machine) = &result.program.items[0].0 else {
        panic!("expected machine");
    };
    assert_eq!(machine.visibility, Visibility::Pub);
    assert_eq!(machine.type_params.len(), 1);
    assert!(machine.where_clause.is_some());
    let wire = machine.wire.as_ref().expect("wire machine");
    assert_eq!(wire.version, Some(4));
    assert_eq!(wire.min_version, Some(2));
    assert!(wire.field_meta.is_empty() && wire.reserved.is_empty());
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
    Start @0;
    Stop @1;
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    assert_eq!(decl.kind, TypeDeclKind::Enum);
    assert_eq!(decl.name, Ident::new("Command"));
    assert_eq!(decl.visibility, Visibility::Pub);
    let wire = decl
        .wire
        .as_ref()
        .expect("expected wire metadata on pub enum");
    assert!(wire.field_meta.is_empty());
    assert!(wire.reserved.is_empty());
    assert_eq!(decl.body.len(), 2);
    for item in &decl.body {
        match item {
            TypeBodyItem::Variant(v) => assert!(matches!(v.kind, VariantKind::Unit)),
            TypeBodyItem::Field { .. } => panic!("expected variant, got {item:?}"),
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
    A @0;
    B(i64) @1;
    C { x: String; y: i32;  } @2;
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let Item::TypeDecl(decl) = &result.program.items[0].0 else {
        panic!("expected type declaration");
    };
    assert_eq!(decl.kind, TypeDeclKind::Enum);
    assert_eq!(decl.name, Ident::new("Mixed"));
    assert!(
        decl.wire.is_some(),
        "expected wire metadata on mixed-variant enum"
    );
    assert_eq!(decl.body.len(), 3, "expected 3 variants in source order");

    // Variant 0: A — unit
    let TypeBodyItem::Variant(a) = &decl.body[0] else {
        panic!("expected variant A at index 0");
    };
    assert_eq!(a.name, Ident::new("A"));
    assert!(
        matches!(a.kind, VariantKind::Unit),
        "variant A should be unit, got {:?}",
        a.kind
    );

    // Variant 1: B(i64) — tuple
    let TypeBodyItem::Variant(b) = &decl.body[1] else {
        panic!("expected variant B at index 1");
    };
    assert_eq!(b.name, Ident::new("B"));
    assert!(
        matches!(b.kind, VariantKind::Tuple(ref ts) if ts.len() == 1),
        "variant B should be tuple(1), got {:?}",
        b.kind
    );

    // Variant 2: C { x: String, y: i32 } — struct
    let TypeBodyItem::Variant(c) = &decl.body[2] else {
        panic!("expected variant C at index 2");
    };
    assert_eq!(c.name, Ident::new("C"));
    match &c.kind {
        VariantKind::Struct(fields) => {
            assert_eq!(fields.len(), 2, "variant C should have 2 fields");
            assert_eq!(fields[0].0, Ident::new("x"));
            assert_eq!(fields[1].0, Ident::new("y"));
        }
        other => panic!("variant C should be struct, got {other:?}"),
    }
}

#[test]
fn wire_struct_field_metadata_preserves_since_modifier() {
    let source = "\
#[wire]
type Msg {
    added: String @2 repeated since 3;
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
    assert_eq!(meta.since, Some(3));
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
    assert!(!result.errors.is_empty(), "{:?}", result.errors);
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
fn generic_apply_suffix_commits_with_nested_angles() {
    let expr = parse_let_expr("f<Vec<Vec<i64>>>()");
    let Expr::Call {
        type_args: Some(type_args),
        ..
    } = expr
    else {
        panic!("expected generic call, got {expr:?}");
    };
    assert_eq!(type_args.len(), 1);
    assert!(matches!(&type_args[0].0, TypeExpr::Named { path, .. } if path.to_string() == "Vec"));
}

#[test]
fn direct_generic_call_accepts_unit_type_argument() {
    let expr = parse_let_expr("f<i64, ()>(16)");
    let Expr::Call {
        type_args: Some(type_args),
        args,
        ..
    } = expr
    else {
        panic!("expected generic call, got {expr:?}");
    };
    assert_eq!(type_args.len(), 2);
    assert!(matches!(type_args[0].0, TypeExpr::Named { .. }));
    assert!(matches!(type_args[1].0, TypeExpr::Tuple(ref fields) if fields.is_empty()));
    assert_eq!(args.len(), 1);
}

#[test]
fn generic_apply_suffix_wins_over_relational_chain_shape() {
    let expr = parse_let_expr("x < y > (z)");
    assert!(
        matches!(
            expr,
            Expr::Call {
                type_args: Some(_),
                ..
            }
        ),
        "the committed shape must not backtrack to comparisons: {expr:?}"
    );

    let comparison = parse_let_expr("x < y");
    assert_eq!(comparison.binary_last_op(), Some(BinaryOp::Less));
}

#[test]
fn generic_apply_suffix_attaches_before_member_access() {
    let expr = parse_let_expr("HashMap<string, User>.new()");
    let Expr::MethodCall {
        receiver, method, ..
    } = expr
    else {
        panic!("expected method call, got {expr:?}");
    };
    assert_eq!(method.0, Ident::new("new"));
    assert!(matches!(receiver.0, Expr::GenericApplySuffix { .. }));
}

#[test]
fn tuple_index_integer_suffix_remains_field_access() {
    let expr = parse_let_expr("t.0");
    assert!(matches!(expr, Expr::FieldAccess { field, .. } if field.0.name.as_str() == "0"));
}

#[test]
fn contextual_variant_expression_preserves_each_payload_origin() {
    let unit = parse_let_expr(".None");
    assert!(
        matches!(unit, Expr::ContextVariant(context) if context.name == Ident::new("None") && context.record.is_none())
    );

    let tuple = parse_let_expr(".Some(value)");
    let Expr::Call { function, .. } = tuple else {
        panic!("expected contextual tuple-payload call, got {tuple:?}");
    };
    assert!(
        matches!(&function.0, Expr::ContextVariant(context) if context.name == Ident::new("Some"))
    );

    let record = parse_let_expr(".Ready { value: value }");
    let Expr::ContextVariant(context) = record else {
        panic!("expected contextual record variant, got {record:?}");
    };
    assert_eq!(context.name, Ident::new("Ready"));
    assert_eq!(context.record.expect("record payload").fields.len(), 1);
}

#[test]
fn pure_dot_record_init_and_qualified_assoc_paths_parse() {
    let expr = parse_let_expr("wire.Message.Data { bytes: payload }");
    assert!(
        matches!(expr, Expr::StructInit { path, .. } if path.to_string() == "wire.Message.Data")
    );

    let assoc = parse_let_expr("<T as iter.Iterator>.make()");
    let Expr::Call { function, .. } = assoc else {
        panic!("expected associated call, got {assoc:?}");
    };
    assert!(matches!(
        &function.0,
        Expr::QualifiedAssoc(assoc)
            if assoc.trait_path.to_string() == "iter.Iterator"
                && assoc.members.len() == 1 && assoc.members[0] == Ident::new("make")
    ));

    let result = parse("fn project(x: <T as iter.Iterator>.Item) {}");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(function) = &result.program.items[0].0 else {
        panic!("expected function");
    };
    assert!(matches!(
        &function.params[0].ty.0,
        TypeExpr::QualifiedAssocPath(assoc)
            if assoc.trait_path.to_string() == "iter.Iterator"
                && assoc.members.len() == 1 && assoc.members[0] == Ident::new("Item")
    ));
}

#[test]
fn nested_dotted_spawn_and_supervisor_child_paths_parse() {
    let spawned = parse("fn main() { let pid = spawn app.workers.Worker; }");
    assert!(spawned.errors.is_empty(), "errors: {:?}", spawned.errors);

    let supervised = parse(
        "supervisor App {\n    strategy: one_for_one;\n    child worker: app.workers.Worker;\n}\n",
    );
    assert!(
        supervised.errors.is_empty(),
        "errors: {:?}",
        supervised.errors
    );
    let Item::Supervisor(supervisor) = &supervised.program.items[0].0 else {
        panic!("expected supervisor");
    };
    assert_eq!(
        supervisor.children[0].actor_type.to_string(),
        "app.workers.Worker"
    );
}

#[test]
fn record_init_restrictions_and_explicit_angle_exemption_hold() {
    let contextual = parse("fn f(flag: bool) { if .Ready { value: flag } { } }");
    assert!(
        !contextual.errors.is_empty(),
        "an unparenthesized contextual record payload is restricted in condition position"
    );

    let dotted = parse("fn f(flag: bool) { if wire.Message.Ready { value: flag } { } }");
    assert!(
        !dotted.errors.is_empty(),
        "an unparenthesized dotted record payload is restricted in condition position"
    );

    let scrutinee = parse("fn f(value: i64) { match .Ready { value: value } { .Ready => { } } }");
    assert!(
        !scrutinee.errors.is_empty(),
        "an unparenthesized contextual record payload is restricted as a match scrutinee"
    );

    let parenthesized = parse("fn f(flag: bool) { if (.Ready { value: flag }).is_ready() { } }");
    assert!(
        parenthesized.errors.is_empty(),
        "parentheses lift the restriction: {:?}",
        parenthesized.errors
    );

    let explicit =
        parse("type Tag<T> {\n    value: T;\n}\n\nfn f() {\n    if Tag<bool> { value: true }.value {}\n}\n");
    assert!(
        explicit.errors.is_empty(),
        "the explicit `>{{` form is unambiguous: {:?}",
        explicit.errors
    );
}

#[test]
fn dotted_trait_paths_parse_in_bounds_dyn_and_impl_headers() {
    for source in [
        "fn f<T: module.api.Trait>(x: T) {}",
        "fn f(x: dyn module.api.Trait) {}",
        "type Thing {} impl module.api.Trait for Thing {}",
    ] {
        let result = parse(source);
        assert!(
            result.errors.is_empty(),
            "dotted trait path should parse in `{source}`: {:?}",
            result.errors
        );
    }
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
fn parse_map_literals() {
    for (src, expected_len) in [
        (r#"{"a": 1}"#, 1),
        (r#"{"a": 1, "b": 2, "c": 3}"#, 3),
        (r#"{"a": 1, "b": 2,}"#, 2),
    ] {
        let expr = parse_let_expr(src);
        assert!(
            matches!(expr, Expr::MapLiteral { ref entries } if entries.len() == expected_len),
            "expected MapLiteral({expected_len}) for {src}, got {expr:?}"
        );
    }
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
fn scope_statement_emits_scope_ast_variant() {
    let body = parse_main_body("scope { 1 };");
    let Stmt::Expression((expr, _)) = &body.stmts[0].0 else {
        panic!("expected statement-expression: {:?}", body.stmts[0]);
    };
    assert!(
        matches!(expr, Expr::Scope { .. }),
        "expected Scope, got {expr:?}"
    );
}

#[test]
fn scope_is_an_ordinary_value_expression() {
    for source in [
        "let result = scope { 1 };",
        "consume(scope { 1 });",
        "scope within 2s { let child = fork run(); await child }",
        "let result = scope within budget { scope { 42 } };",
    ] {
        let result = parse(&format!("fn main() {{ {source} }}"));
        assert!(result.errors.is_empty(), "{source}: {:?}", result.errors);
    }
}

#[test]
fn fork_is_an_ordinary_value_expression() {
    for source in [
        "let child = fork run();",
        "consume(fork run());",
        "let value = await fork run();",
        "let tasks = [fork first(), fork second()];",
        "let batch = fork [first(), second()];",
        "let mixed = fork (load(), count());",
        "let child = fork { let nested = fork run(); await nested };",
        "scope { let child = fork run(); await child; }",
    ] {
        let result = parse(&format!("fn main() {{ {source} }}"));
        assert!(result.errors.is_empty(), "{source}: {:?}", result.errors);
    }
}

#[test]
fn fork_bare_call_retains_its_operand() {
    let body = parse_main_body("fork run();");
    let Stmt::Expression((Expr::ForkChild { expr }, _)) = &body.stmts[0].0 else {
        panic!("expected fork expression: {:?}", body.stmts[0]);
    };
    assert!(matches!(&expr.0, Expr::Call { .. }));
}

#[test]
fn fork_special_binding_syntax_is_removed() {
    let result = parse("fn main() { fork child = run(); }");
    assert!(!result.errors.is_empty());
}

#[test]
fn parse_scope_deadline_preserves_duration_and_result() {
    let Expr::ScopeDeadline { duration, body } = parse_let_expr("scope within 5s { 42 }") else {
        panic!("expected scope deadline");
    };
    assert!(matches!(
        duration.0,
        Expr::Literal(Literal::Duration(5_000_000_000))
    ));
    assert!(body.trailing_expr.is_some());
    assert!(!parse("fn main() { scope { after(5s) {} } }")
        .errors
        .is_empty());
}

#[test]
fn trait_method_preserves_receiver_identity_and_consuming_self() {
    let source = r"
trait Fluent {
    #[returns_receiver]
    fn with(consume self, consume child: Child) -> Self;
}
";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Trait(t) = &result.program.items[0].0 else {
        panic!("expected trait");
    };
    let TraitItem::Method(m) = &t.items[0] else {
        panic!("expected method");
    };
    assert!(m.consumes_self);
    assert_eq!(m.attributes.len(), 1);
    assert_eq!(m.attributes[0].name, "returns_receiver");
    assert_eq!(m.params.len(), 2);
    assert_eq!(m.params[0].name, Ident::new("self"));
    assert_eq!(m.params[1].name, Ident::new("child"));
    assert!(m.params[1].is_consume);
}

#[test]
fn parses_parameterized_type_alias() {
    let result = parse("type Pair<T> = (T, T);");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::TypeAlias(alias) = &result.program.items[0].0 else {
        panic!("expected type alias");
    };
    assert_eq!(alias.name, Ident::new("Pair"));
    assert_eq!(
        alias
            .type_params
            .as_ref()
            .expect("type parameters")
            .iter()
            .map(|param| param.name.name.as_str())
            .collect::<Vec<_>>(),
        vec!["T"]
    );
}

// ── ParseDiagnosticKind serialisation ──────────────────────────────────

#[test]
fn parse_diagnostic_kind_serialises() {
    for (kind, tag, fields) in [
        (
            ParseDiagnosticKind::UnexpectedToken {
                expected: ";".to_string(),
                got: "}".to_string(),
            },
            "UnexpectedToken",
            vec![("expected", ";"), ("got", "}")],
        ),
        (ParseDiagnosticKind::UnexpectedEof, "UnexpectedEof", vec![]),
        (
            ParseDiagnosticKind::InvalidLiteral,
            "InvalidLiteral",
            vec![],
        ),
        (
            ParseDiagnosticKind::MissingExpression {
                got: "end of file".to_string(),
            },
            "MissingExpression",
            vec![("got", "end of file")],
        ),
        (
            ParseDiagnosticKind::InvalidPattern {
                got: "42".to_string(),
            },
            "InvalidPattern",
            vec![("got", "42")],
        ),
        (ParseDiagnosticKind::Other, "Other", vec![]),
    ] {
        let json = serde_json::to_value(&kind).unwrap();
        assert_eq!(json["kind"], tag);
        for (k, v) in fields {
            assert_eq!(json[k], v);
        }
        assert_eq!(kind.as_kind_str(), tag);
    }
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
fn struct_init_explicit_type_args() {
    for (src, expected_path, expected_args) in [
        (
            "type Wrapper<T> { value: T; } fn main() { let w = Wrapper<string> { value: \"hello\" }; }",
            "Wrapper",
            Some(vec!["string"]),
        ),
        (
            "type Wrapper<T> { value: T; } fn main() { let w = Wrapper { value: \"hello\" }; }",
            "Wrapper",
            None,
        ),
        (
            "type Pair<A, B> { first: A; second: B; } fn main() { let p = Pair<int, string> { first: 1, second: \"x\" }; }",
            "Pair",
            Some(vec!["int", "string"]),
        ),
        (
            "type Tag<T> {} fn main() { let t = Tag<int> {}; }",
            "Tag",
            Some(vec!["int"]),
        ),
    ] {
        let result = parse(src);
        assert!(result.errors.is_empty(), "{src}: {:?}", result.errors);
        let Item::Function(f) = &result.program.items[1].0 else {
            panic!("expected function");
        };
        let Stmt::Let {
            value: Some((Expr::StructInit { path, type_args, .. }, _)),
            ..
        } = &f.body.stmts[0].0
        else {
            panic!("expected StructInit");
        };
        assert_eq!(path.to_string(), expected_path);
        let actual = type_args.as_ref().map(|args| {
            args.iter()
                .map(|(ty, _)| match ty {
                    TypeExpr::Named { path, .. } => path.to_string(),
                    other => panic!("expected named type arg, got {other:?}"),
                })
                .collect::<Vec<_>>()
        });
        let expected = expected_args.map(|v| v.into_iter().map(str::to_string).collect());
        assert_eq!(actual, expected, "{src}");
    }
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
fn parses_resource_marker_and_impl_consuming_method() {
    let source = r"#[resource]
type File {
    fd: int;
}
impl File {
    fn close(consume self) -> int {
        0
    }
}
";
    let result = parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    assert_eq!(result.program.items.len(), 2);
    let (item, _span) = &result.program.items[0];
    let Item::TypeDecl(td) = item else {
        panic!("expected TypeDecl, got {item:?}");
    };
    assert_eq!(td.resource_marker, ResourceMarker::Resource);
    let Item::Impl(methods) = &result.program.items[1].0 else {
        panic!("expected Impl");
    };
    assert!(methods.methods[0].consumes_self);
    assert_eq!(td.name, Ident::new("File"));
}

#[test]
fn parses_linear_marker_and_impl_consuming_methods() {
    let source = r"#[linear]
type Txn {
    id: int;
}
impl Txn {
    fn commit(consume self) -> int {
        0
    }
    fn rollback(consume self) -> int {
        1
    }
    fn id(t: Txn) -> int {
        0
    }
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
    let Item::Impl(methods) = &result.program.items[1].0 else {
        panic!("expected Impl");
    };
    assert_eq!(
        methods
            .methods
            .iter()
            .map(|method| method.consumes_self)
            .collect::<Vec<_>>(),
        vec![true, true, false],
    );
}

#[test]
fn unmarked_type_has_no_resource_marker() {
    let source = "type Point {\n    x: int;\n    y: int;\n}\n";
    let result = parse(source);
    assert!(result.errors.is_empty());
    let (Item::TypeDecl(td), _) = &result.program.items[0] else {
        panic!("expected TypeDecl");
    };
    assert_eq!(td.resource_marker, ResourceMarker::None);
}

#[test]
fn resource_marker_rejects_tuple_type_without_affecting_supported_targets() {
    // `#[resource]` is only legal at `type`/`enum` declaration position
    // (HEW-SPEC-2026 §12.6); a tuple-form `type` never carries `ResourceMarker`,
    // so this is `E_UNKNOWN_ATTRIBUTE`, superseding the old
    // `E_RESOURCE_MARKER_TARGET` code.
    let unsupported_source = "#[resource]\ntype Pair(i64);";
    let unsupported = parse(unsupported_source);
    assert_eq!(
        unsupported.errors.len(),
        1,
        "tuple-form ownership marker must produce one diagnostic: {:?}",
        unsupported.errors
    );
    assert!(
        unsupported.errors[0]
            .message
            .contains("E_UNKNOWN_ATTRIBUTE"),
        "tuple-form ownership marker must use the closed-table diagnostic: {:?}",
        unsupported.errors
    );
    assert_eq!(
        &unsupported_source[unsupported.errors[0].span.clone()],
        "#[resource]",
        "ownership-marker diagnostic must point at its attribute"
    );

    let supported_source = r"#[resource]
type Named {
    x: i64;
}

#[resource]
enum E {
    A;
}
";
    let supported = parse(supported_source);
    assert!(
        supported.errors.is_empty(),
        "supported ownership-marker targets must remain valid: {:?}",
        supported.errors
    );
    assert_eq!(supported.program.items.len(), 2);
    for (item, _) in &supported.program.items {
        let Item::TypeDecl(decl) = item else {
            panic!("expected ownership-bearing type declaration, got {item:?}");
        };
        assert_eq!(decl.resource_marker, ResourceMarker::Resource);
    }
}

#[test]
fn resource_markers_reject_unsupported_top_level_items_in_every_visibility_form() {
    let source = r"
#[resource] type PrivateTuple(i64);
#[linear] pub type PublicTuple(i64);
#[resource] package type PackageTuple(i64);

#[linear] fn private_fn() {}
#[resource] pub fn public_fn() {}
#[linear] package fn package_fn() {}

#[resource] actor PrivateActor {}
#[linear] pub actor PublicActor {}
#[resource] package actor PackageActor {}

#[linear] trait PrivateTrait {}
#[resource] pub trait PublicTrait {}
#[linear] package trait PackageTrait {}

#[resource] type PrivateAlias = i64;
#[linear] pub type PublicAlias = i64;
#[resource] package type PackageAlias = i64;

#[linear] impl PrivateTuple { fn close(self) {} }
";
    let result = parse(source);
    let marker_errors: Vec<_> = result
        .errors
        .iter()
        .filter(|error| error.message.contains("E_UNKNOWN_ATTRIBUTE"))
        .collect();

    assert_eq!(
        marker_errors.len(),
        16,
        "every ownership marker on a non-TypeDecl item must fail closed: {:?}",
        result.errors
    );
    for error in marker_errors {
        assert!(
            source[error.span.clone()].starts_with("#[resource]")
                || source[error.span.clone()].starts_with("#[linear]"),
            "ownership-marker diagnostic must point at its attribute: {error:?}"
        );
    }
}

#[test]
fn resource_markers_remain_valid_on_nominal_type_and_enum_declarations() {
    let source = r"#[resource]
type PrivateResource {
    id: i64;
}

#[linear]
pub type PublicLinear {
    id: i64;
}

#[resource]
package type PackageResource {
    id: i64;
}

#[linear]
enum PrivateLinear {
    Open;
}

#[resource]
pub enum PublicResource {
    Open;
}

#[linear]
package indirect enum PackageLinear {
    Open;
}
";
    let result = parse(source);
    assert!(
        result.errors.is_empty(),
        "nominal type and enum ownership markers must remain valid: {:?}",
        result.errors
    );
    assert_eq!(result.program.items.len(), 6);
}

#[test]
fn resource_markers_reject_nested_declarations_that_do_not_carry_them() {
    let source = r#"
type Host {
}

trait Surface {
    #[linear]
    fn call();
}

impl Host {
    #[resource]
    fn helper(self) {}
}

extern "C" {
    #[linear]
    fn host_call();
}

actor Worker {
    #[resource]
    receive fn handle() {}
    #[linear]
    fn lifecycle() {}
}
"#;
    let result = parse(source);
    let marker_errors: Vec<_> = result
        .errors
        .iter()
        .filter(|error| error.message.contains("E_UNKNOWN_ATTRIBUTE"))
        .collect();

    assert_eq!(
        marker_errors.len(),
        5,
        "nested ownership-marker misuse must fail closed: {:?}",
        result.errors
    );
    assert!(marker_errors.iter().all(|error| {
        source[error.span.clone()].starts_with("#[resource]")
            || source[error.span.clone()].starts_with("#[linear]")
    }));
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
fn unresolved_type_attribute_is_deferred_to_expansion() {
    let source = r"
            #[both]
            type Bad { x: i64; }
        ";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    assert!(matches!(result.program.items[0].0, Item::TypeDecl(_)));
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
    let result = parse("#[deprecated]\ntype Old {\n    x: int;\n}\n");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}
// ── #[max_heap] attribute tests ──────────────────────────────────────────

#[test]
fn max_heap_attribute_units_and_refusals() {
    for (spec, expected) in [
        ("1024", 1024),
        ("2 kb", 2 * 1024),
        ("1 mb", 1024 * 1024),
        ("512 b", 512),
        ("0", 0),
    ] {
        let source = format!("#[max_heap({spec})] actor Demo {{}}");
        let result = parse(&source);
        assert!(result.errors.is_empty(), "{source}: {:?}", result.errors);
        let Item::Actor(actor) = &result.program.items[0].0 else {
            panic!("expected actor");
        };
        assert_eq!(actor.max_heap_bytes, Some(expected), "{spec}");
    }

    assert!(!parse("#[max_heap(1 gb)] actor Demo {}").errors.is_empty());

    for bad_target in [
        "#[max_heap(1024)] fn foo() {}",
        "#[max_heap(1 kb)] type Bar { x: i32; }",
    ] {
        let result = parse(bad_target);
        assert!(
            result.errors.iter().any(
                |e| e.message.contains("max_heap") && e.message.contains("E_UNKNOWN_ATTRIBUTE")
            ),
            "{bad_target}: {:?}",
            result.errors
        );
    }
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
fn is_operator_ast_and_precedence() {
    let Expr::Is { lhs, rhs } = first_fn_trailing("fn f() { x is y }") else {
        panic!("expected Expr::Is");
    };
    assert!(matches!(lhs.0, Expr::Ident(ref s) if s.name.as_str() == "x"));
    assert!(matches!(rhs.0, Expr::Ident(ref s) if s.name.as_str() == "y"));

    for source in [
        "fn f() { value is Point }",
        "fn f() { let x = value is int; }",
        "fn f() { let x = value is (a, b); }",
    ] {
        let result = parse(source);
        assert!(result.errors.is_empty(), "{source}: {:?}", result.errors);
    }

    let Expr::Is { lhs, .. } = first_fn_trailing("fn f() { a is b is c }") else {
        panic!("expected outer Expr::Is");
    };
    assert!(matches!(lhs.0, Expr::Is { .. }));

    let expr = first_fn_trailing("fn f() { a is b && c is d }");
    let (left, op, right) = expr.as_binary_pair().expect("binary");
    assert_eq!(op, BinaryOp::And);
    assert!(matches!(left.0, Expr::Is { .. }) && matches!(right.0, Expr::Is { .. }));

    let expr = first_fn_trailing("fn f() { a is b == true }");
    let (left, op, right) = expr.as_binary_pair().expect("binary");
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
    let Some(op) = expr.binary_last_op() else {
        panic!("expected Binary");
    };
    assert_eq!(op, BinaryOp::WrappingAdd);

    let expr = first_fn_expr("fn f(a: i64, b: i64) -> i64 { a &- b }");
    let Some(op) = expr.binary_last_op() else {
        panic!("expected Binary");
    };
    assert_eq!(op, BinaryOp::WrappingSub);

    let expr = first_fn_expr("fn f(a: i64, b: i64) -> i64 { a &* b }");
    let Some(op) = expr.binary_last_op() else {
        panic!("expected Binary");
    };
    assert_eq!(op, BinaryOp::WrappingMul);
}

#[test]
fn wrapping_add_precedence_matches_plain_add() {
    // `a &+ b * c` should parse as `a &+ (b * c)` — `*` binds tighter.
    let expr = first_fn_expr("fn f(a: i64, b: i64, c: i64) -> i64 { a &+ b * c }");
    let Some((_, op, right)) = expr.as_binary_pair() else {
        panic!("expected outer Binary");
    };
    assert_eq!(op, BinaryOp::WrappingAdd, "outer op must be &+");
    // The right sub-expression must be `b * c` (plain multiply).
    let Some(inner_op) = right.0.binary_last_op() else {
        panic!("expected inner Binary");
    };
    assert_eq!(inner_op, BinaryOp::Multiply, "inner op must be *");
}

#[test]
fn wrapping_mul_precedence_matches_plain_mul() {
    // `a + b &* c` should parse as `a + (b &* c)` — `&*` binds tighter than `+`.
    let expr = first_fn_expr("fn f(a: i64, b: i64, c: i64) -> i64 { a + b &* c }");
    let Some((_, op, right)) = expr.as_binary_pair() else {
        panic!("expected outer Binary");
    };
    assert_eq!(op, BinaryOp::Add, "outer op must be +");
    let Some(inner_op) = right.0.binary_last_op() else {
        panic!("expected inner Binary");
    };
    assert_eq!(inner_op, BinaryOp::WrappingMul, "inner op must be &*");
}

// ── spread in literals: `[..xs, x]` and `R { ..base, x: 5 }` ─────────

#[test]
fn array_spread_elements_parse_at_any_position() {
    let src = r"
            fn f(xs: Vec<int>, ys: Vec<int>) {
                let a = [..xs, 1];
                let b = [1, ..xs];
                let c = [..xs, 1, ..ys];
                let d = [..xs];
            }
        ";
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "spread elements parse at any position; got: {:?}",
        result.errors
    );
}

#[test]
fn array_spread_keeps_element_and_spread_apart() {
    let result = parse("fn f(xs: Vec<int>) { let a = [1, ..xs]; }");
    assert!(result.errors.is_empty(), "{:?}", result.errors);
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
    let Expr::Array(elements) = expr else {
        panic!("expected an array literal, got {expr:?}");
    };
    assert_eq!(elements.len(), 2);
    assert!(!elements[0].is_spread(), "`1` is a plain element");
    assert!(elements[1].is_spread(), "`..xs` is a spread");
}

#[test]
fn array_spread_with_a_repeat_count_is_rejected() {
    // `[..xs; 3]` has no reading: a spread splices a collection and a repeat
    // literal repeats one value.
    let result = parse("fn f(xs: Vec<int>) { let a = [..xs; 3]; }");
    assert!(!result.errors.is_empty(), "{:?}", result.errors);
}

// ── record spread: `R { ..base, x: 5 }` ───────────────────────────────

#[test]
fn functional_update_basic_parses() {
    // `Point { x: 1, ..old }` must parse as a StructInit with base = Some(old).
    let src = r"type Point {
    x: int;
    y: int;
}

fn f(old: Point) {
    let p = Point { ..old, x: 1 };
}
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
        matches!(&base.0, Expr::Ident(name) if name.name.as_str() == "old"),
        "base should be Identifier 'old', got {:?}",
        base.0
    );
}

#[test]
fn functional_update_no_explicit_fields_parses() {
    // `Point { ..old }` (zero explicit fields, only base) must also parse.
    let src = r"type Point {
    x: int;
    y: int;
}

fn f(old: Point) {
    let p = Point { ..old };
}
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
fn functional_update_base_is_none_for_regular_struct_init() {
    // A plain struct literal must have base = None.
    let src = r"type Point {
    x: int;
    y: int;
}

fn f() {
    let p = Point { x: 1, y: 2 };
}
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
fn record_spread_second_base_is_rejected() {
    // `R { ..a, ..b }` has two answers for every unnamed field, so it is
    // refused with a code a client can act on.
    let src = r"
            type Point { x: int, y: int }
            fn f(a: Point, b: Point) { let p = Point { ..a, ..b }; }
        ";
    let result = parse(src);
    let refusal = result
        .errors
        .iter()
        .find(|error| error.kind == ParseDiagnosticKind::DuplicateRecordBase);
    refusal.unwrap_or_else(|| {
        panic!(
            "two `..base` items must be refused; got: {:?}",
            result.errors
        )
    });
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
fn extern_symbol_attribute_positions_and_arguments() {
    let extern_res = parse(
        r#"extern "C" { #[extern_symbol("hew_vec_push_{T}")] fn hew_vec_push(v: ptr, x: ptr); }"#,
    );
    assert!(extern_res.errors.is_empty(), "{:?}", extern_res.errors);
    let Item::ExternBlock(block) = &extern_res.program.items[0].0 else {
        panic!("expected ExternBlock");
    };
    assert_eq!(block.functions[0].attributes[0].name, "extern_symbol");
    assert_eq!(
        block.functions[0].attributes[0].args[0].as_str(),
        "hew_vec_push_{T}"
    );

    for (src, expect_trait, expected_sym) in [
        (
            r#"impl<T> Vec<T> { #[extern_symbol("hew_vec_push_{T}")] fn push(self, x: T) {} }"#,
            false,
            "hew_vec_push_{T}",
        ),
        (
            r#"impl<T> SomeTrait for Vec<T> { #[extern_symbol("hew_vec_join_str")] fn join(self, sep: string) -> string {} }"#,
            true,
            "hew_vec_join_str",
        ),
    ] {
        let res = parse(src);
        assert!(res.errors.is_empty(), "{src}: {:?}", res.errors);
        let Item::Impl(impl_decl) = &res.program.items[0].0 else {
            panic!("expected Impl");
        };
        assert_eq!(impl_decl.trait_bound.is_some(), expect_trait);
        assert_eq!(impl_decl.methods[0].attributes[0].name, "extern_symbol");
        assert_eq!(
            impl_decl.methods[0].attributes[0].args[0].as_str(),
            expected_sym
        );
    }

    for invalid in [
        r#"#[extern_symbol("hew_foo")] pub fn foo() {}"#,
        r#"#[extern_symbol("hew_actor")] actor MyActor {}"#,
        r#"actor MyActor { #[extern_symbol("hew_actor_recv")] receive fn ping() {} }"#,
        r#"trait Pushable<T> { #[extern_symbol("hew_vec_push_{T}")] fn push(self, value: T); }"#,
        r#"extern "C" { #[extern_symbol(= 5)] fn hew_x(); }"#,
    ] {
        let res = parse(invalid);
        assert!(!res.errors.is_empty(), "expected error for: {invalid}");
    }
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

/// A leading-dot tuple-payload variant preserves its contextual origin.
#[test]
fn leading_dot_constructor_pattern_preserves_origin() {
    let source = "fn f(e: E) -> i64 { match e { .Some(x) => x, _ => 0 } }";
    let patterns = first_match_arm_patterns(source);
    match &patterns[0] {
        Pattern::ContextVariant(context) => {
            assert_eq!(context.name, Ident::new("Some"));
            let Some(NominalPatternPayload::Tuple(patterns)) = &context.payload else {
                panic!("expected tuple payload, got {:?}", context.payload);
            };
            assert_eq!(patterns.len(), 1);
            assert!(matches!(&patterns[0].0, Pattern::Identifier(n) if n.name.as_str() == "x"));
        }
        other => panic!("expected contextual variant pattern, got {other:?}"),
    }
}

/// A leading-dot unit variant cannot be confused with a lexical binding.
#[test]
fn leading_dot_unit_variant_pattern_preserves_origin() {
    let source = "fn f(e: E) -> i64 { match e { .None => 0, _ => 1 } }";
    let patterns = first_match_arm_patterns(source);
    assert!(matches!(
        &patterns[0],
        Pattern::ContextVariant(context)
            if context.name == Ident::new("None") && context.payload.is_none()
    ));
}

/// Leading-dot variants compose with or-patterns: `.A(x) | .B(x)` parses to
/// a `Pattern::Or` over two contextual leaves.
#[test]
fn leading_dot_or_pattern_parses() {
    let source = "fn f(e: E) -> i64 { match e { .A(x) | .B(x) => x, _ => 0 } }";
    let patterns = first_match_arm_patterns(source);
    let Pattern::Or(alternatives) = &patterns[0] else {
        panic!("expected Pattern::Or, got {:?}", patterns[0]);
    };
    let [left, right] = alternatives.as_slice() else {
        panic!("expected two alternatives, got {alternatives:?}");
    };
    assert!(matches!(&left.0, Pattern::ContextVariant(context) if context.name == Ident::new("A")));
    assert!(
        matches!(&right.0, Pattern::ContextVariant(context) if context.name == Ident::new("B"))
    );
}

#[test]
fn a_long_or_pattern_keeps_source_order_and_round_trips() {
    let alternatives = (0..2_000)
        .map(|value| value.to_string())
        .collect::<Vec<_>>()
        .join(" | ");
    let source = format!("fn f(x: i64) -> i64 {{ match x {{ {alternatives} => 1, _ => 0 }} }}");
    let patterns = first_match_arm_patterns(&source);
    let Pattern::Or(alternatives) = &patterns[0] else {
        panic!("expected an or-pattern");
    };
    assert_eq!(alternatives.len(), 2_000);
    for (index, (pattern, span)) in alternatives.iter().enumerate() {
        assert!(matches!(pattern, Pattern::Literal(_)));
        assert_eq!(source[span.clone()].trim(), index.to_string());
    }
    let encoded = serde_json::to_string(&patterns[0]).expect("serialize flat alternatives");
    let decoded: Pattern = serde_json::from_str(&encoded).expect("deserialize flat alternatives");
    assert_eq!(patterns[0], decoded);
}

/// Adding the leading-dot arm does not disturb a qualified constructor pattern.
#[test]
fn leading_dot_arm_leaves_qualified_pattern_path_intact() {
    let source = "fn f(e: E) -> i64 { match e { E.Some(x) => x, .None => 0, _ => 1 } }";
    let patterns = first_match_arm_patterns(source);
    let Pattern::NominalPath { path, payload } = &patterns[0] else {
        panic!("expected nominal path, got {:?}", patterns[0]);
    };
    assert_eq!(path.to_string(), "E.Some");
    assert!(matches!(payload, Some(NominalPatternPayload::Tuple(_))));
    assert!(
        matches!(&patterns[1], Pattern::ContextVariant(context) if context.name == Ident::new("None"))
    );
}

#[test]
fn dotted_nominal_patterns_preserve_paths() {
    for (source, expected_path, expect_none_payload) in [
        (
            "fn f(e: m.E) -> i64 { match e { m.E.Some(x) => x, _ => 0 } }",
            "m.E.Some",
            false,
        ),
        (
            "fn f(n: i64) -> i64 { match n { foo.bar => 1, _ => 0 } }",
            "foo.bar",
            true,
        ),
        (
            "fn f(n: i64) -> i64 { match n { m.T.Variant => 1, _ => 0 } }",
            "m.T.Variant",
            true,
        ),
    ] {
        let patterns = first_match_arm_patterns(source);
        let Pattern::NominalPath { path, payload } = &patterns[0] else {
            panic!("expected nominal path for {source}, got {:?}", patterns[0]);
        };
        assert_eq!(path.to_string(), expected_path);
        if expect_none_payload {
            assert!(payload.is_none());
        }
    }
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
    let Pattern::RecordShorthand { fields, .. } = &pattern.0 else {
        panic!("expected Pattern::RecordShorthand, got {:?}", pattern.0);
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name, Ident::new("x"));
    assert!(
        fields[0].pattern.is_none(),
        "shorthand field should have no sub-pattern"
    );
    assert_eq!(fields[1].name, Ident::new("y"));
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
    let Pattern::RecordShorthand { fields, .. } = &pattern.0 else {
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
fn retired_struct_and_wire_keywords_are_rejected() {
    for source in [
        "struct Point { x: i64 }",
        "#[wire]\nstruct Point { x: i64 @1 }",
        "pub struct Point { x: i64 }",
        "#[wire]\npub struct Point { x: i64 @1 }",
        "wire Foo {}",
        "wire type Foo { @1 x: string }",
        "wire enum Status { Active; Inactive; }",
    ] {
        let result = parse(source);
        assert!(!result.errors.is_empty(), "expected error for: {source}");
    }
}

#[test]
fn record_rest_patterns_parse_in_all_record_forms() {
    for (source, shorthand) in [
        (
            "fn f(p: Point) -> i64 { match p { Point { x, .. } => x } }",
            false,
        ),
        (
            "fn f(e: E) -> i64 { match e { E.A { x, .. } => x } }",
            false,
        ),
        ("fn f(p: Point) -> i64 { match p { { x, .. } => x } }", true),
    ] {
        let patterns = first_match_arm_patterns(source);
        match &patterns[0] {
            Pattern::NominalPath {
                payload: Some(NominalPatternPayload::Record { fields, rest }),
                ..
            } if !shorthand => {
                assert_eq!(fields.len(), 1);
                assert!(rest.is_some());
            }
            Pattern::RecordShorthand { fields, rest } if shorthand => {
                assert_eq!(fields.len(), 1);
                assert!(rest.is_some());
            }
            other => panic!("unexpected rest pattern for {source}: {other:?}"),
        }
    }
}

#[test]
fn record_rest_pattern_all_wildcard_form_parses() {
    let patterns =
        first_match_arm_patterns("fn f(p: Point) -> i64 { match p { Point { .. } => 0 } }");
    let Pattern::NominalPath {
        payload: Some(NominalPatternPayload::Record { fields, rest }),
        ..
    } = &patterns[0]
    else {
        panic!("expected struct rest pattern, got {:?}", patterns[0]);
    };
    assert!(fields.is_empty());
    assert!(rest.is_some());
}

#[test]
fn record_rest_pattern_grammar_rejects_invalid_positions() {
    for source in [
        "fn f(p: Point) -> i64 { match p { Point { x, .., } => x } }",
        "fn f(p: Point) -> i64 { match p { Point { x, .., y } => x } }",
        "fn f(p: Point) -> i64 { match p { Point { x, .., .. } => x } }",
        "fn f(p: Point) -> i64 { match p { Point(x, ..) => x } }",
    ] {
        let result = parse(source);
        assert!(!result.errors.is_empty(), "{:?}", result.errors);
    }
}

#[test]
fn record_rest_patterns_round_trip_through_formatter() {
    let source = "fn f(p: Point) -> i64 { let { x, .. } = p; match p { Point { .. } => x } }";
    let parsed = parse(source);
    assert!(parsed.errors.is_empty(), "errors: {:?}", parsed.errors);
    let formatted = crate::fmt::format_program(&parsed.program);
    assert!(formatted.contains("let { x, .. } = p"));
    assert!(formatted.contains("Point { .. }"));
    let reparsed = parse(&formatted);
    assert!(
        reparsed.errors.is_empty(),
        "formatted source failed to parse: {:?}\n{formatted}",
        reparsed.errors
    );
}

// Built-in positions remain parser-owned; other names resolve after imports.
#[test]
fn unresolved_attribute_arguments_retain_token_trees() {
    for name in ["bogus", "noncancellable", "module.decorator"] {
        let source = format!("#[{name}(1 + call(2), [3, 4])] fn helper() {{}}");
        let result = parse(&source);
        assert!(result.errors.is_empty(), "{:?}", result.errors);
        let Item::Function(function) = &result.program.items[0].0 else {
            panic!("function");
        };
        assert!(matches!(
            function.attributes[0].args.as_slice(),
            [AttributeArg::Tokens(_)]
        ));
    }
}

#[test]
fn attribute_placement_and_test_trio_rules() {
    let legal_wire = parse("#[wire]\ntype Contract {\n    x: i64 @1;\n}\n\ntype Point {\n    #[wire]\n    x: i64;\n}\n");
    assert!(
        legal_wire.errors.is_empty(),
        "errors: {:?}",
        legal_wire.errors
    );

    for (source, attr) in [
        (
            "actor Worker {\n    #[every(1s)]\n    fn tick() {}\n}\nfn main() {}",
            "every",
        ),
        ("#[wire]\nfn helper() {}\nfn main() { helper(); }", "wire"),
    ] {
        let result = parse(source);
        assert!(
            result.errors.iter().any(|e| {
                e.message
                    .contains(&format!("unrecognised attribute `#[{attr}]`"))
                    && e.message.contains("E_UNKNOWN_ATTRIBUTE")
            }),
            "{source}: {:?}",
            result.errors
        );
    }

    for attr in ["ignore", "should_panic", "serial", "real_time"] {
        let without = parse(&format!(
            "#[{attr}]\nfn probe() {{}}\nfn main() {{ probe(); }}"
        ));
        assert!(
            without.errors.iter().any(|e| {
                e.message
                    .contains(&format!("unrecognised attribute `#[{attr}]`"))
                    && e.message.contains("E_UNKNOWN_ATTRIBUTE")
            }),
            "without #[test] for #[{attr}]: {:?}",
            without.errors
        );

        let with = parse(&format!(
            "#[test]\n#[{attr}]\nfn probe() {{}}\nfn main() {{}}"
        ));
        assert!(
            with.errors.is_empty(),
            "with #[test] for #[{attr}]: {:?}",
            with.errors
        );
    }
}
#[test]
fn fallible_function_surface_preserves_success_and_error_types() {
    let source = "fn read() -> (i64, string) fails string { return (10, \"hello\"); }";
    let parsed = crate::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let Item::Function(function) = &parsed.program.items[0].0 else {
        panic!("function");
    };
    assert!(
        matches!(&function.return_type.as_ref().expect("return type").0,
        TypeExpr::Fallible { success, error }
        if matches!(&success.0, TypeExpr::Tuple(fields) if fields.len() == 2)
            && matches!(&error.0, TypeExpr::Named { path, .. } if path.to_string() == "string"))
    );
}

#[test]
fn fallible_function_without_success_type_is_unit_fallible() {
    for source in [
        "fn check() fails string { return error \"bad\"; }",
        "trait Step { fn run(self) fails string; }",
        "extern \"C\" { fn c_call() -> i32; }\nfn main() fails string { }",
    ] {
        let parsed = crate::parse(source);
        assert!(parsed.errors.is_empty(), "{source}: {:?}", parsed.errors);
    }
    let parsed = crate::parse("fn check() fails string { }");
    let Item::Function(function) = &parsed.program.items[0].0 else {
        panic!("function");
    };
    assert!(matches!(
        &function.return_type.as_ref().expect("return type").0,
        TypeExpr::Fallible { success, .. }
            if matches!(&success.0, TypeExpr::Tuple(fields) if fields.is_empty())
    ));
}

#[test]
fn unit_arrow_before_fails_is_refused_with_its_migration() {
    let parsed = crate::parse("fn check() -> () fails string { }");
    assert!(
        parsed
            .errors
            .iter()
            .any(|error| error.kind == ParseDiagnosticKind::UnitFailsArrow),
        "{:?}",
        parsed.errors
    );
    let parsed = crate::parse("fn check() -> i64 fails string { 1 }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
}

#[test]
fn fallible_function_and_error_return_roundtrip() {
    for source in [
        "fn f() -> (i64, string) fails string { return (10, \"hello\"); }",
        "fn f() -> i64 fails string { return error \"missing\"; }",
        "fn f() -> i64 fails string { 1 + (return error \"missing\") }",
        "fn f() -> i64 fails string { (return error \"missing\") + 1 }",
        "fn f() -> i64 fails string { -(return error \"missing\") }",
        "fn f(error: i64) -> i64 fails string { return error; }",
        "fn f() -> i64 fails i64 { return error { -1 }; }",
    ] {
        let before = crate::parse(source);
        assert!(before.errors.is_empty(), "{source}: {:?}", before.errors);
        let formatted = crate::fmt::format_program(&before.program);
        let after = crate::parse(&formatted);
        assert!(after.errors.is_empty(), "{formatted}: {:?}", after.errors);
        assert!(
            crate::ast_eq::program_eq_ignoring_spans(&before.program, &after.program),
            "{source} became {formatted}"
        );
    }
}

#[test]
fn return_error_dot_names_a_variant_of_the_error_type() {
    let parsed = parse("fn f() -> i64 fails E { return error .Missing(\"port\"); }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let Item::Function(function) = &parsed.program.items[0].0 else {
        panic!("expected function");
    };
    assert!(
        matches!(
            &function.body.stmts[0].0,
            Stmt::Expression((Expr::ReturnError(value), _))
                if matches!(&value.0, Expr::Call { function, .. }
                    if matches!(function.0, Expr::ContextVariant(_)))
        ),
        "{:?}",
        function.body.stmts[0].0
    );
    let formatted = crate::fmt::format_program(&parsed.program);
    assert!(
        formatted.contains("return error .Missing(\"port\")"),
        "{formatted}"
    );

    // Attached, it reads as a field of a binding named `error` and is rejected.
    let parsed = parse("fn f() -> string { return error.fmt(); }");
    assert!(!parsed.errors.is_empty(), "{:?}", parsed.errors);
}

#[test]
fn ordinary_error_binding_expressions_take_priority_after_return() {
    for operand in [
        "error",
        "error()",
        "error(1)",
        "error[0]",
        "error?",
        "error ?? 1",
        "error + 1",
        "error - 1",
        "error as i64",
        "error handle problem { 1 }",
    ] {
        for expression_position in [false, true] {
            let statement = if expression_position {
                format!("let ignored = (return {operand});")
            } else {
                format!("return {operand};")
            };
            let source = format!("fn f() {{ {statement} }}");
            let parsed = parse(&source);
            assert!(parsed.errors.is_empty(), "{source}: {:?}", parsed.errors);
            let Item::Function(function) = &parsed.program.items[0].0 else {
                panic!("expected function");
            };
            if expression_position {
                assert!(
                    matches!(
                        function.body.stmts[0].0,
                        Stmt::Let {
                            value: Some((Expr::Return(Some(_)), _)),
                            ..
                        }
                    ),
                    "{source}"
                );
            } else {
                assert!(
                    matches!(function.body.stmts[0].0, Stmt::Return(Some(_))),
                    "{source}"
                );
            }
            let formatted = crate::fmt::format_program(&parsed.program);
            let reparsed = parse(&formatted);
            assert!(
                reparsed.errors.is_empty(),
                "{formatted}: {:?}",
                reparsed.errors
            );
            assert!(
                crate::ast_eq::program_eq_ignoring_spans(&parsed.program, &reparsed.program),
                "{source} became {formatted}"
            );
        }
    }
}

#[test]
fn select_arm_binds_its_source_with_from() {
    let Expr::Select { arms, timeout } = parse_let_expr(
        "select { first from left => first, second from right => second, after 1s => 0 }",
    ) else {
        panic!("expected select");
    };
    assert_eq!(arms.len(), 2);
    // The `from` clause carries the source operand itself; nothing wraps it.
    assert!(arms.iter().all(
        |arm| matches!(&arm.source.0, Expr::Ident(name) if name.name.as_str() == "left"
            || name.name.as_str() == "right")
    ));
    assert!(timeout.is_some());
}

/// Negative control for the clause above: the retired `=` spelling no longer
/// parses as a select arm.
#[test]
fn select_arm_rejects_the_retired_equals_spelling() {
    let parsed =
        crate::parse("fn f() { let v = select { first = left => first, after 1s => 0 }; }");
    assert!(!parsed.errors.is_empty(), "expected a parse error");
}

#[test]
fn diet_words_are_accepted_as_identifiers() {
    let source = "fn main() {
    let try = 1;
    let catch = 2;
    let cooperate = 3;
    let foreign = 4;
    let super = 5;
    let budget = 6;
    let default = 7;
    let emit = 8;
    let pool = 9;
    println(try + catch + cooperate + foreign + super + budget + default + emit + pool);
}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn try_block_is_still_rejected() {
    let result = crate::parse("fn main() { try { work() } }");
    assert!(!result.errors.is_empty(), "{:?}", result.errors);
}

#[test]
fn foreign_item_is_rejected() {
    let result = crate::parse("foreign \"C\" { fn puts(s: string) -> i32; }");
    assert!(!result.errors.is_empty(), "{:?}", result.errors);
}

#[test]
fn race_is_still_a_reserved_word() {
    let result = crate::parse("fn main() { let race = 1; }");
    assert!(
        !result.errors.is_empty(),
        "`race` is reserved; using it as a name must be refused"
    );
}

#[test]
fn if_let_chains_with_else_if() {
    let source = "fn main() {
    let value: Option<i64> = None;
    if let .Some(_) = value {
        println(\"some\");
    } else if let .None = value {
        println(\"none\");
    } else if true {
        println(\"other\");
    } else {
        println(\"fallback\");
    }
}";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn if_let_else_if_round_trips_through_the_formatter() {
    let source = "fn main() {
    let value: Option<i64> = None;
    if let .Some(_) = value {
        println(\"some\");
    } else if true {
        println(\"ok\");
    }
}
";
    let first = crate::parse(source);
    assert!(first.errors.is_empty(), "errors: {:?}", first.errors);
    let formatted = crate::fmt::format_program(&first.program);
    assert!(
        formatted.contains("} else if true {"),
        "the formatter must preserve the `else if` spelling, got:\n{formatted}"
    );
    let second = crate::parse(&formatted);
    assert!(second.errors.is_empty(), "errors: {:?}", second.errors);
    assert!(crate::ast_eq::program_eq_ignoring_spans(
        &first.program,
        &second.program
    ));
}

#[test]
fn try_is_an_ordinary_binding_in_condition_position() {
    // `if try {` reads the binding and opens the then block; only a `try`
    // block in value position is the retired construct.
    let result = crate::parse("fn main() { let try = true; if try { println(\"ok\"); } }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

/// Parse `fn f() { <stmt> <tail> }` and return its body.
fn statement_block_body(body: &str) -> (Block, Vec<ParseError>) {
    let source = format!("fn f() {{\n{body}\n}}");
    let result = parse(&source);
    let Item::Function(function) = &result.program.items[0].0 else {
        panic!("expected function");
    };
    (function.body.clone(), result.errors)
}

/// Every block-like form at statement start ends at its `}`: the dotted
/// variant on the next line is the block's tail expression, not a method call
/// on the block.
#[test]
fn statement_blocks_end_before_a_dotted_tail() {
    for opener in [
        "{ work(); }",
        "unsafe { work(); }",
        "scope { work(); }",
        "select { after 1ms => 0 }",
        "fork { work() }",
        "gen { yield 1; }",
        "if c { work(); }",
        "match c { true => work(), false => work() }",
        "for i in 0..2 { work(); }",
        "while c { work(); }",
        "loop { break; }",
        "defer { work(); }",
    ] {
        let (body, errors) = statement_block_body(&format!("{opener}\n.Ok(1)"));
        assert!(errors.is_empty(), "`{opener}` errors: {errors:?}");
        assert_eq!(body.stmts.len(), 1, "`{opener}` must be one statement");
        let Some(tail) = body.trailing_expr.as_deref() else {
            panic!("`{opener}` must leave `.Ok(1)` as the tail");
        };
        assert!(
            matches!(&tail.0, Expr::Call { function, .. }
                if matches!(&function.0, Expr::ContextVariant(context) if context.name == Ident::new("Ok"))),
            "`{opener}` tail must be `.Ok(1)`, got {:?}",
            tail.0
        );
    }
}

/// Statement-start block operands require parentheses; parenthesized forms,
/// `let` values and map literals keep their postfix, and `handle` still attaches.
#[test]
fn statement_block_operands_need_parentheses() {
    for refused in [
        "{ s }.len();",
        "unsafe { g() }.abs()",
        "unsafe { g() } != 0",
        "unsafe { 7 } - 1",
        "unsafe { dbl }(4)",
        "{ vv }[0]",
        "scope { 7 } * 2",
        "if c { 1 } else { 2 } - 5",
        "{ s }\n.len()",
    ] {
        let (_, errors) = statement_block_body(refused);
        assert!(!errors.is_empty(), "`{refused}` must be rejected");
    }
    for accepted in [
        "({ s }).len()",
        "(unsafe { g() }) != 0",
        "let n = { s }.len();\nn",
        "{\"a\": 1, \"b\": 2}.len()",
        "scope { work() } handle failure { 0 }",
        "if stop { work(); }\n[1]",
        "unsafe { work(); }\n(a, b)",
    ] {
        let (_, errors) = statement_block_body(accepted);
        assert!(errors.is_empty(), "`{accepted}` must parse: {errors:?}");
    }
}

// ── Path AST: qualified spellings stay segmented ─────────────────────────

use crate::ast::{Symbol, SyntaxContext};

/// The segment spellings of `path`, and the source text each segment span covers.
fn path_segments<'src>(source: &'src str, path: &Path) -> Vec<(Ident, &'src str)> {
    path.segments
        .iter()
        .map(|(ident, span)| (*ident, &source[span.clone()]))
        .collect()
}

fn idents(spellings: &[&'static str]) -> Vec<(Ident, &'static str)> {
    spellings
        .iter()
        .map(|spelling| (Ident::new(spelling), *spelling))
        .collect()
}

#[test]
fn qualified_type_parses_to_segmented_path_with_type_args() {
    let source = "fn main() { let x: ma.Shape<T> = make(); }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(f) = &result.program.items[0].0 else {
        panic!("expected function");
    };
    let Stmt::Let {
        ty: Some((TypeExpr::Named { path, type_args }, _)),
        ..
    } = &f.body.stmts[0].0
    else {
        panic!(
            "expected a let with a named type, got {:?}",
            f.body.stmts[0].0
        );
    };
    assert_eq!(path_segments(source, path), idents(&["ma", "Shape"]));
    let args = type_args.as_ref().expect("type arguments");
    assert!(
        matches!(&args[..], [(TypeExpr::Named { path, type_args: None }, _)] if path.as_single() == Some(Ident::new("T")))
    );
}

#[test]
fn pure_dot_record_literal_parses_to_segmented_struct_init_path() {
    let source = "fn main() { let x = wire.Message.Data { bytes: payload }; }";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(f) = &result.program.items[0].0 else {
        panic!("expected function");
    };
    let Stmt::Let {
        value: Some((Expr::StructInit { path, fields, .. }, _)),
        ..
    } = &f.body.stmts[0].0
    else {
        panic!("expected a struct literal, got {:?}", f.body.stmts[0].0);
    };
    assert_eq!(
        path_segments(source, path),
        idents(&["wire", "Message", "Data"])
    );
    assert_eq!(fields[0].0, Ident::new("bytes"));
}

#[test]
fn import_parses_to_segmented_path() {
    let source = "import std.net.http.server as http;";
    let result = parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Import(import) = &result.program.items[0].0 else {
        panic!("expected import");
    };
    assert_eq!(
        path_segments(source, &import.path),
        idents(&["std", "net", "http", "server"])
    );
}

#[test]
fn qualified_record_pattern_parses_to_nominal_path() {
    let patterns = first_match_arm_patterns(
        "fn f(p: ma.Point) -> i64 { match p { ma.Point { x, .. } => x } }",
    );
    let Pattern::NominalPath {
        path,
        payload: Some(NominalPatternPayload::Record { fields, rest }),
    } = &patterns[0]
    else {
        panic!("expected a qualified record pattern, got {:?}", patterns[0]);
    };
    assert_eq!(path.to_string(), "ma.Point");
    assert_eq!(path.segments.len(), 2);
    assert_eq!(fields[0].name, Ident::new("x"));
    assert!(rest.is_some());
}

#[test]
fn one_segment_constructor_and_record_patterns_are_nominal_paths() {
    let patterns = first_match_arm_patterns(
        "fn f(o: Option<i64>) -> i64 { match o { Some(v) => v, None => 0 } }",
    );
    assert!(matches!(
        &patterns[0],
        Pattern::NominalPath { path, payload: Some(NominalPatternPayload::Tuple(items)) }
            if path.as_single() == Some(Ident::new("Some")) && items.len() == 1
    ));
    // A bare name stays an identifier pattern; resolution decides whether it
    // names a unit variant or binds.
    assert!(matches!(&patterns[1], Pattern::Identifier(name) if *name == Ident::new("None")));
}

/// Negative control: a dotted chain in expression position without `{` is a
/// `FieldAccess` chain. Whether `a` is a module or a local is a resolution
/// fact, so the parser builds no path.
#[test]
fn dotted_expression_without_brace_stays_field_access_chain() {
    let expr = parse_let_expr("a.b.c");
    let Expr::FieldAccess { object, field } = &expr else {
        panic!("expected field access, got {expr:?}");
    };
    assert_eq!(field.0, Ident::new("c"));
    let Expr::FieldAccess { object, field } = &object.0 else {
        panic!("expected nested field access, got {:?}", object.0);
    };
    assert_eq!(field.0, Ident::new("b"));
    assert!(matches!(&object.0, Expr::Ident(name) if *name == Ident::new("a")));
}

/// Negative control: struct literals are not admitted in an `if` condition, so
/// `a.b { }` there is the condition `a.b` followed by the then block.
#[test]
fn dotted_condition_before_block_stays_field_access() {
    let result = parse("fn main() { if a.b { } }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Item::Function(f) = &result.program.items[0].0 else {
        panic!("expected function");
    };
    let condition = match (&f.body.stmts[..], f.body.trailing_expr.as_deref()) {
        ([(Stmt::If { condition, .. }, _)], _) => &condition.0,
        ([], Some((Expr::If { condition, .. }, _))) => &condition.0,
        other => panic!("expected an if, got {other:?}"),
    };
    let Expr::FieldAccess { object, field } = condition else {
        panic!("expected field access condition, got {condition:?}");
    };
    assert_eq!(field.0, Ident::new("b"));
    assert!(matches!(&object.0, Expr::Ident(name) if *name == Ident::new("a")));
}

#[test]
fn identifiers_carry_the_root_syntax_context() {
    let expr = parse_let_expr("value");
    let Expr::Ident(ident) = expr else {
        panic!("expected identifier, got {expr:?}");
    };
    assert_eq!(ident.name, Symbol::intern("value"));
    assert_eq!(ident.ctx, SyntaxContext::ROOT);
}

#[test]
fn init_without_parameter_list_is_refused_with_fix_it_and_no_cascade() {
    let source = r"
        actor Outbox {
            var closed: bool = false;
            init {
                closed = true;
            }
            receive fn state() -> bool { closed }
        }
    ";
    let result = parse(source);
    assert_eq!(result.errors.len(), 1, "{:#?}", result.errors);

    let (Item::Actor(actor), _) = &result.program.items[0] else {
        panic!("expected the actor to parse");
    };
    assert!(actor.init.is_some());
    assert_eq!(actor.receive_fns.len(), 1);
}

#[test]
fn legacy_path_separators_on_one_long_line_stay_linear() {
    std::thread::Builder::new()
        .stack_size(1024 * 1024)
        .spawn(legacy_path_separators_on_one_long_line_body)
        .expect("spawn parser thread")
        .join()
        .expect("long malformed path overflows the parser stack");
}

fn legacy_path_separators_on_one_long_line_body() {
    // Each separated `::` reports the line it sits on; the quoted context is
    // bounded so a long line does not cost quadratic memory.
    let source = format!("fn main() {{ x{} }}", "::x".repeat(20_000));
    let errors = parse(&source).errors;
    assert!(errors.len() >= 20_000, "{}", errors.len());
    let quoted: usize = errors.iter().map(|e| e.message.len()).sum();
    assert!(quoted < 20_000 * 600, "{quoted} bytes of messages");
    // A run of adjacent separators is one report.
    let source = format!("fn main() {{ {} }}", "::".repeat(20_000));
    let errors = parse(&source).errors;
    assert!(errors.len() <= 2, "{}", errors.len());
}

fn binary_chain(expr: &Expr) -> (&Spanned<Expr>, &[BinaryStep]) {
    let Expr::Binary { first, steps } = expr else {
        panic!("expected a binary chain, got {expr:?}");
    };
    (first, steps)
}

#[test]
fn same_precedence_operators_fold_into_one_chain() {
    let expr = parse_let_expr("a + b - c &+ d");
    let (first, steps) = binary_chain(&expr);
    assert!(matches!(first.0, Expr::Ident(_)));
    let ops: Vec<_> = steps.iter().map(|step| step.op).collect();
    assert_eq!(
        ops,
        [BinaryOp::Add, BinaryOp::Subtract, BinaryOp::WrappingAdd]
    );

    for (source, op) in [
        ("p && q && r", BinaryOp::And),
        ("p || q || r", BinaryOp::Or),
        ("x | y | z", BinaryOp::BitOr),
        ("x << y << z", BinaryOp::Shl),
        ("p < q < r", BinaryOp::Less),
        ("x * y / z", BinaryOp::Divide),
    ] {
        let expr = parse_let_expr(source);
        let (_, steps) = binary_chain(&expr);
        assert_eq!(steps.len(), 2, "{source}");
        assert_eq!(steps[1].op, op, "{source}");
    }
}

#[test]
fn chains_nest_only_across_precedence_and_parentheses() {
    // `*` binds tighter: the chain's second operand is its own chain.
    let expr = parse_let_expr("a + b * c * d + e");
    let (_, steps) = binary_chain(&expr);
    assert_eq!(steps.len(), 2);
    let (_, product) = binary_chain(&steps[0].right.0);
    assert_eq!(product.len(), 2);

    // A parenthesized operand stays one operand.
    let expr = parse_let_expr("(a + b) + c");
    let (first, steps) = binary_chain(&expr);
    assert_eq!(steps.len(), 1);
    assert_eq!(binary_chain(&first.0).1.len(), 1);

    // A chain followed by a looser operator becomes that operator's operand.
    let expr = parse_let_expr("a + b == c + d");
    let (first, steps) = binary_chain(&expr);
    assert_eq!(steps.len(), 1);
    assert_eq!(steps[0].op, BinaryOp::Equal);
    assert_eq!(binary_chain(&first.0).1.len(), 1);
}

#[test]
fn binary_chain_steps_keep_the_nested_prefix_spans() {
    // Each step's span is the span the left-deep node for that prefix had:
    // from the first operand to the start of the next token.
    let expr = parse_let_expr("a + b - c * d + e");
    let (first, steps) = binary_chain(&expr);
    let base = first.1.start;
    assert_eq!(first.1.end - base, 1);
    let rel = |span: &std::ops::Range<usize>| (span.start - base)..(span.end - base);
    let spans: Vec<_> = steps.iter().map(|step| rel(&step.span)).collect();
    assert_eq!(spans, [0..6, 0..14, 0..17]);
    assert_eq!(rel(&steps[1].right.1), 8..14);
    let (_, product) = binary_chain(&steps[1].right.0);
    assert_eq!(rel(&product[0].span), 8..14);
}

#[test]
fn range_operators_do_not_chain() {
    let expr = parse_let_expr("a..b");
    let (_, steps) = binary_chain(&expr);
    assert_eq!(steps.len(), 1);
    assert_eq!(steps[0].op, BinaryOp::Range);
}

/// Run `body` on a thread whose stack is far smaller than a deep tree of the
/// input would need.
fn on_a_small_stack(body: impl FnOnce() + Send + 'static) {
    std::thread::Builder::new()
        .stack_size(512 * 1024)
        .spawn(body)
        .expect("spawn")
        .join()
        .expect("a long chain recursed per operand");
}

#[test]
fn a_fifty_thousand_term_chain_parses_and_formats_without_deep_recursion() {
    on_a_small_stack(|| {
        let terms = vec!["x"; 50_000].join(" + ");
        let source = format!("fn main() {{\n    let _y = {terms};\n}}\n");
        let result = parse(&source);
        assert!(result.errors.is_empty(), "{:?}", result.errors);
        let (_, steps) = binary_chain(first_let_value(&result));
        assert_eq!(steps.len(), 49_999);
        let formatted =
            crate::fmt::format_checked(&source, &result.program).expect("formatting round-trips");
        assert_eq!(formatted, source);
        let conjunction = vec!["p"; 50_000].join(" && ");
        let source = format!("fn main() {{\n    let _q = {conjunction};\n}}\n");
        let result = parse(&source);
        assert!(result.errors.is_empty(), "{:?}", result.errors);
        assert_eq!(binary_chain(first_let_value(&result)).1.len(), 49_999);
    });
}

#[test]
fn deeply_nested_expressions_reach_the_existing_nesting_diagnostic() {
    on_a_small_stack(|| {
        let source = format!(
            "fn main() {{ let _ = {}1{}; }}",
            "(".repeat(2_000),
            ")".repeat(2_000),
        );
        let parsed = parse(&source);
        assert!(parsed
            .errors
            .iter()
            .any(|error| error.message.contains("maximum nesting depth exceeded")));
        assert!(parsed.errors.len() < 2_000);
        assert!(parse("fn main() { println(1); }").errors.is_empty());
    });
}

#[test]
fn private_record_fields_round_trip_without_reserving_the_identifier() {
    let source = "type Entry { private: string; private secret: i64; }";
    let parsed = crate::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let Item::TypeDecl(decl) = &parsed.program.items[0].0 else {
        panic!("expected record")
    };
    assert!(
        matches!(&decl.body[0], TypeBodyItem::Field { private_span: None, name, .. } if name.name.as_str() == "private")
    );
    assert!(
        matches!(&decl.body[1], TypeBodyItem::Field { private_span: Some(span), .. } if &source[span.clone()] == "private")
    );
    let formatted = crate::fmt::format_program(&parsed.program);
    assert!(formatted.contains("private: string;"));
    assert!(formatted.contains("private secret: i64;"));
}
