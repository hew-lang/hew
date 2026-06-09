/// Parser tests for generic actor declarations and generic spawn.
///
/// Covers:
///   • `actor Foo<T> { ... }` — type-param list parsed onto `ActorDecl.type_params`
///   • `spawn Foo<i64>()` — turbofish parsed onto `Expr::Spawn.type_args`
///   • `spawn Foo<T, U>(x: val)` — multiple type args with init args
///   • Non-generic `spawn Foo()` — `type_args` is empty, no regression
use hew_parser::ast::{Expr, Item, Stmt, TypeExpr};

// ── parser_spawn_with_type_args_parses_correctly ─────────────────────────────

/// Spawning a generic actor with one type argument produces a `Spawn` node
/// whose `type_args` vec has exactly one entry.
#[test]
fn parser_spawn_with_type_args_parses_correctly() {
    let source = r"
actor Worker<T> {
    receive fn handle(msg: T) {}
}

fn main() {
    let pid = spawn Worker<i64>();
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );

    // Verify ActorDecl has the type_params field populated.
    let mut found_actor_type_param = false;
    let mut found_spawn_type_arg = false;

    for (item, _) in &result.program.items {
        match item {
            Item::Actor(ad) if ad.name == "Worker" => {
                assert_eq!(ad.type_params.len(), 1, "expected 1 type param on Worker");
                assert_eq!(ad.type_params[0].name, "T");
                found_actor_type_param = true;
            }
            Item::Function(f) if f.name == "main" => {
                for (stmt, _) in &f.body.stmts {
                    if let Stmt::Let {
                        value:
                            Some((
                                Expr::Spawn {
                                    type_args, target, ..
                                },
                                _,
                            )),
                        ..
                    } = stmt
                    {
                        if let Expr::Identifier(name) = &target.0 {
                            if name == "Worker" {
                                assert_eq!(
                                    type_args.len(),
                                    1,
                                    "expected 1 type arg on spawn Worker<i64>()"
                                );
                                // The type arg should be a Named TypeExpr for `i64`.
                                if let (TypeExpr::Named { name: ty_name, .. }, _) = &type_args[0] {
                                    assert_eq!(ty_name, "i64");
                                } else {
                                    panic!("expected Named type arg, got {:?}", type_args[0]);
                                }
                                found_spawn_type_arg = true;
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    assert!(
        found_actor_type_param,
        "actor Worker<T> type param not found"
    );
    assert!(
        found_spawn_type_arg,
        "spawn Worker<i64>() type arg not found"
    );
}

/// `spawn Foo()` (non-generic) produces an empty `type_args` vec — no regression.
#[test]
fn parser_spawn_non_generic_has_empty_type_args() {
    let source = r"
actor Foo {}

fn main() {
    let _pid = spawn Foo();
}
";
    let result = hew_parser::parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    for (item, _) in &result.program.items {
        if let Item::Function(f) = item {
            if f.name == "main" {
                for (stmt, _) in &f.body.stmts {
                    if let Stmt::Let {
                        value: Some((Expr::Spawn { type_args, .. }, _)),
                        ..
                    } = stmt
                    {
                        assert!(
                            type_args.is_empty(),
                            "non-generic spawn should have empty type_args, got {type_args:?}"
                        );
                    }
                }
            }
        }
    }
}

/// Multiple type arguments parse correctly.
#[test]
fn parser_spawn_multiple_type_args() {
    let source = r#"
actor Pair<A, B> {
    receive fn handle(a: A, b: B) {}
}

fn main() {
    let _pid = spawn Pair<i32, string>(a: 1, b: "x");
}
"#;
    let result = hew_parser::parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    let mut found = false;
    for (item, _) in &result.program.items {
        if let Item::Function(f) = item {
            if f.name == "main" {
                for (stmt, _) in &f.body.stmts {
                    if let Stmt::Let {
                        value:
                            Some((
                                Expr::Spawn {
                                    type_args, args, ..
                                },
                                _,
                            )),
                        ..
                    } = stmt
                    {
                        assert_eq!(type_args.len(), 2, "expected 2 type args");
                        assert_eq!(args.len(), 2, "expected 2 init args");
                        found = true;
                    }
                }
            }
        }
    }
    assert!(found, "spawn Pair<i32, string> not found");
}

/// Actor with a bounded type parameter parses the bound correctly.
#[test]
fn parser_actor_type_param_with_bound() {
    let source = r"
actor Buffer<T: Send> {
    receive fn push(item: T) {}
}
";
    let result = hew_parser::parse(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

    for (item, _) in &result.program.items {
        if let Item::Actor(ad) = item {
            if ad.name == "Buffer" {
                assert_eq!(ad.type_params.len(), 1);
                let tp = &ad.type_params[0];
                assert_eq!(tp.name, "T");
                assert_eq!(tp.bounds.len(), 1);
                assert_eq!(tp.bounds[0].name, "Send");
                return;
            }
        }
    }
    panic!("actor Buffer<T: Send> not found");
}

/// Formatter round-trips actor type-param list: `actor Buffer<T: Send>` must survive
/// parse → format → reparse with the type param still present.
#[test]
fn actor_type_params_round_trip_through_formatter() {
    let source = r"
actor Buffer<T: Send> {
    receive fn push(item: T) {}
}
";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "initial parse errors: {:?}",
        parsed.errors
    );

    let formatted = hew_parser::fmt::format_program(&parsed.program);

    // The formatted output must contain the type param.
    assert!(
        formatted.contains("Buffer<T: Send>"),
        "formatter dropped type param; formatted output:\n{formatted}"
    );

    // Full round-trip: reparse must also be error-free and preserve the param.
    let reparsed = hew_parser::parse(&formatted);
    assert!(
        reparsed.errors.is_empty(),
        "reparse of formatted source failed: {:?}\n\n--- formatted ---\n{formatted}",
        reparsed.errors
    );

    let mut found = false;
    for (item, _) in &reparsed.program.items {
        if let Item::Actor(ad) = item {
            if ad.name == "Buffer" {
                assert_eq!(
                    ad.type_params.len(),
                    1,
                    "type_params dropped after formatter round-trip"
                );
                assert_eq!(ad.type_params[0].name, "T");
                found = true;
            }
        }
    }
    assert!(found, "actor Buffer not found after formatter round-trip");
}
