use hew_parser::ast::{Expr, Item};

#[test]
fn spawn_plain_actor() {
    let source = r#"
fn main() {
    let pid = spawn Worker(name: "test");
}
"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
}

#[test]
fn spawn_dotted_actor() {
    let source = r#"
fn main() {
    let pid = spawn workers.Worker(name: "test");
}
"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    // Find the spawn expression and verify it's a FieldAccess target
    let mut found_field_access = false;
    for (item, _) in &result.program.items {
        if let Item::Function(f) = item {
            if f.name == "main" {
                // Walk into the block to find the spawn
                for (stmt, _) in &f.body.stmts {
                    if let hew_parser::ast::Stmt::Let { value, .. } = stmt {
                        if let Some((expr, _)) = value {
                            if let Expr::Spawn { target, .. } = expr {
                                if let Expr::FieldAccess { field, .. } = &target.0 {
                                    assert_eq!(field, "Worker");
                                    found_field_access = true;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    assert!(
        found_field_access,
        "expected spawn with FieldAccess target for dotted actor"
    );
}

#[test]
fn spawn_dotted_no_args() {
    let source = r#"
fn main() {
    let pid = spawn workers.Worker();
}
"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
}
