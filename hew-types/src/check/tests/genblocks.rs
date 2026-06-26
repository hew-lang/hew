#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

// ── stack-hint scanner coverage for tail-promoted if-let ─────────────────
//
// `Expr::IfLet` is produced when `if let … { … }` appears in tail position
// inside a block (the parser promotes `Stmt::IfLet` → `Expr::IfLet` and
// places it in `Block::trailing_expr`).  The scan_expr_for_stack_hints walker
// must handle `Expr::IfLet` bodies so that heap bindings declared inside them
// are reported.
#[cfg(test)]
mod iflet_tail_stack_hint_coverage {
    use super::*;

    fn stack_hint_names(source: &str) -> Vec<String> {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        output
            .stack_hints
            .into_iter()
            .map(|h| h.binding_name)
            .collect()
    }

    /// A `Vec` binding inside a tail-promoted `if let` body must be reported.
    #[test]
    fn tail_promoted_iflet_body_bindings_are_stack_hint_scanned() {
        let names = stack_hint_names(
            r"fn foo(opt: (i64, i64)) {
    if let (a, _b) = opt {
        let v: Vec<i64> = Vec::new();
        v
    }
}",
        );
        assert!(
            names.iter().any(|n| n == "v"),
            "expected stack hint for `v` inside tail-promoted if-let body; got: {names:?}",
        );
    }

    /// A `Vec` binding inside the `else` block of a tail-promoted `if let`
    /// must also be reported.
    #[test]
    fn tail_promoted_iflet_else_body_bindings_are_stack_hint_scanned() {
        let names = stack_hint_names(
            r"fn foo(opt: (i64, i64)) {
    if let (a, _b) = opt {
        a
    } else {
        let w: Vec<i64> = Vec::new();
        0
    }
}",
        );
        assert!(
            names.iter().any(|n| n == "w"),
            "expected stack hint for `w` inside tail-promoted if-let else-body; got: {names:?}",
        );
    }
}
