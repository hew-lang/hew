//! Analysis module for inlay hints.
//!
//! Produces inlay hints (type annotations for unannotated `let`/`var` bindings
//! and lambda return types) using byte offsets rather than LSP positions.

use hew_parser::ast::{Block, Expr, Item, Span, Stmt, StringPart, TraitItem, TypeBodyItem};
use hew_parser::ParseResult;
use hew_types::check::SpanKey;
use hew_types::TypeCheckOutput;

use crate::{InlayHint, InlayHintKind};

/// Build inlay hints for the entire document.
#[must_use]
pub fn build_inlay_hints(
    source: &str,
    parse_result: &ParseResult,
    tc: &TypeCheckOutput,
) -> Vec<InlayHint> {
    let mut hints = Vec::new();
    for (item, _span) in &parse_result.program.items {
        collect_inlay_hints_from_item(source, item, tc, &mut hints);
    }
    hints
}

fn collect_inlay_hints_from_item(
    source: &str,
    item: &Item,
    tc: &TypeCheckOutput,
    hints: &mut Vec<InlayHint>,
) {
    match item {
        Item::Const(c) => collect_inlay_hints_from_expr(source, &c.value.0, tc, hints),
        Item::Function(f) => collect_inlay_hints_from_block(source, &f.body, tc, hints),
        Item::Actor(a) => {
            if let Some(init) = &a.init {
                collect_inlay_hints_from_block(source, &init.body, tc, hints);
            }
            if let Some(term) = &a.terminate {
                collect_inlay_hints_from_block(source, &term.body, tc, hints);
            }
            for recv in &a.receive_fns {
                collect_inlay_hints_from_block(source, &recv.body, tc, hints);
            }
            for method in &a.methods {
                collect_inlay_hints_from_block(source, &method.body, tc, hints);
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &td.body {
                if let TypeBodyItem::Method(method) = body_item {
                    collect_inlay_hints_from_block(source, &method.body, tc, hints);
                }
            }
        }
        Item::Impl(i) => {
            for method in &i.methods {
                collect_inlay_hints_from_block(source, &method.body, tc, hints);
            }
        }
        Item::Trait(t) => {
            for trait_item in &t.items {
                if let TraitItem::Method(method) = trait_item {
                    if let Some(body) = &method.body {
                        collect_inlay_hints_from_block(source, body, tc, hints);
                    }
                }
            }
        }
        Item::Supervisor(s) => {
            for child in &s.children {
                for arg in &child.args {
                    collect_inlay_hints_from_expr(source, &arg.0, tc, hints);
                }
            }
        }
        Item::Machine(m) => {
            for transition in &m.transitions {
                if let Some(guard) = &transition.guard {
                    collect_inlay_hints_from_expr(source, &guard.0, tc, hints);
                }
                collect_inlay_hints_from_expr(source, &transition.body.0, tc, hints);
            }
        }
        _ => {}
    }
}

fn collect_inlay_hints_from_block(
    source: &str,
    block: &Block,
    tc: &TypeCheckOutput,
    hints: &mut Vec<InlayHint>,
) {
    for (stmt, _span) in &block.stmts {
        collect_inlay_hints_from_stmt(source, stmt, tc, hints);
    }
    if let Some(trailing) = &block.trailing_expr {
        collect_inlay_hints_from_expr(source, &trailing.0, tc, hints);
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "exhaustive AST match — splitting would obscure the traversal"
)]
fn collect_inlay_hints_from_stmt(
    source: &str,
    stmt: &Stmt,
    tc: &TypeCheckOutput,
    hints: &mut Vec<InlayHint>,
) {
    match stmt {
        Stmt::Let { pattern, ty, value } => {
            if ty.is_none() {
                if let Some(value_expr) = value {
                    let span_key = SpanKey {
                        start: value_expr.1.start,
                        end: value_expr.1.end,
                    };
                    if let Some(inferred_ty) = tc.expr_types.get(&span_key) {
                        hints.push(InlayHint {
                            offset: pattern.1.end,
                            label: format!(": {}", inferred_ty.user_facing()),
                            kind: InlayHintKind::Type,
                            padding_left: false,
                        });
                    }
                }
            }
            if let Some(value_expr) = value {
                collect_inlay_hints_from_expr(source, &value_expr.0, tc, hints);
            }
        }
        Stmt::Var {
            ty, value, name, ..
        } => {
            if ty.is_none() {
                if let Some(value_expr) = value {
                    let span_key = SpanKey {
                        start: value_expr.1.start,
                        end: value_expr.1.end,
                    };
                    if let Some(inferred_ty) = tc.expr_types.get(&span_key) {
                        let name_end = find_var_name_end(source, &value_expr.1, name);
                        hints.push(InlayHint {
                            offset: name_end,
                            label: format!(": {}", inferred_ty.user_facing()),
                            kind: InlayHintKind::Type,
                            padding_left: false,
                        });
                    }
                }
            }
            if let Some(value_expr) = value {
                collect_inlay_hints_from_expr(source, &value_expr.0, tc, hints);
            }
        }
        Stmt::Loop { body, .. } => {
            collect_inlay_hints_from_block(source, body, tc, hints);
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_inlay_hints_from_expr(source, &condition.0, tc, hints);
            collect_inlay_hints_from_block(source, body, tc, hints);
        }
        Stmt::WhileLet { expr, body, .. } => {
            collect_inlay_hints_from_expr(source, &expr.0, tc, hints);
            collect_inlay_hints_from_block(source, body, tc, hints);
        }
        Stmt::For { iterable, body, .. } => {
            collect_inlay_hints_from_expr(source, &iterable.0, tc, hints);
            collect_inlay_hints_from_block(source, body, tc, hints);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            collect_inlay_hints_from_expr(source, &condition.0, tc, hints);
            collect_inlay_hints_from_block(source, then_block, tc, hints);
            if let Some(eb) = else_block {
                if let Some(if_stmt) = &eb.if_stmt {
                    collect_inlay_hints_from_stmt(source, &if_stmt.0, tc, hints);
                }
                if let Some(block) = &eb.block {
                    collect_inlay_hints_from_block(source, block, tc, hints);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            collect_inlay_hints_from_expr(source, &expr.0, tc, hints);
            collect_inlay_hints_from_block(source, body, tc, hints);
            if let Some(block) = else_body {
                collect_inlay_hints_from_block(source, block, tc, hints);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            collect_inlay_hints_from_expr(source, &scrutinee.0, tc, hints);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_inlay_hints_from_expr(source, &guard.0, tc, hints);
                }
                collect_inlay_hints_from_expr(source, &arm.body.0, tc, hints);
            }
        }
        Stmt::Defer(expr) => {
            collect_inlay_hints_from_expr(source, &expr.0, tc, hints);
        }
        Stmt::Assign { target, value, .. } => {
            collect_inlay_hints_from_expr(source, &target.0, tc, hints);
            collect_inlay_hints_from_expr(source, &value.0, tc, hints);
        }
        Stmt::Break {
            value: Some(value), ..
        } => {
            collect_inlay_hints_from_expr(source, &value.0, tc, hints);
        }
        Stmt::Return(Some(val)) => {
            collect_inlay_hints_from_expr(source, &val.0, tc, hints);
        }
        Stmt::Expression(expr) => {
            collect_inlay_hints_from_expr(source, &expr.0, tc, hints);
        }
        _ => {}
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "explicit recursion over all expression variants keeps traversal correct"
)]
fn collect_inlay_hints_from_expr(
    source: &str,
    expr: &Expr,
    tc: &TypeCheckOutput,
    hints: &mut Vec<InlayHint>,
) {
    match expr {
        Expr::Binary { left, right, .. } => {
            collect_inlay_hints_from_expr(source, &left.0, tc, hints);
            collect_inlay_hints_from_expr(source, &right.0, tc, hints);
        }
        Expr::Unary { operand, .. } => {
            collect_inlay_hints_from_expr(source, &operand.0, tc, hints);
        }
        Expr::Lambda {
            return_type, body, ..
        } => {
            if return_type.is_none() {
                let span_key = SpanKey {
                    start: body.1.start,
                    end: body.1.end,
                };
                if let Some(body_ty) = tc.expr_types.get(&span_key) {
                    hints.push(InlayHint {
                        offset: body.1.start,
                        label: format!("-> {} ", body_ty.user_facing()),
                        kind: InlayHintKind::Type,
                        padding_left: true,
                    });
                }
            }
            collect_inlay_hints_from_expr(source, &body.0, tc, hints);
        }
        Expr::SpawnLambdaActor { body, .. } => {
            collect_inlay_hints_from_expr(source, &body.0, tc, hints);
        }
        Expr::Block(block)
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            collect_inlay_hints_from_block(source, block, tc, hints);
        }
        Expr::Scope { body, .. } => {
            collect_inlay_hints_from_block(source, body, tc, hints);
        }
        Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            collect_inlay_hints_from_expr(source, &condition.0, tc, hints);
            collect_inlay_hints_from_expr(source, &then_block.0, tc, hints);
            if let Some(else_expr) = else_block {
                collect_inlay_hints_from_expr(source, &else_expr.0, tc, hints);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            collect_inlay_hints_from_expr(source, &expr.0, tc, hints);
            collect_inlay_hints_from_block(source, body, tc, hints);
            if let Some(block) = else_body {
                collect_inlay_hints_from_block(source, block, tc, hints);
            }
        }
        Expr::Match { scrutinee, arms } => {
            collect_inlay_hints_from_expr(source, &scrutinee.0, tc, hints);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_inlay_hints_from_expr(source, &guard.0, tc, hints);
                }
                collect_inlay_hints_from_expr(source, &arm.body.0, tc, hints);
            }
        }
        Expr::Call { function, args, .. } => {
            collect_inlay_hints_from_expr(source, &function.0, tc, hints);
            for arg in args {
                let expr = arg.expr();
                collect_inlay_hints_from_expr(source, &expr.0, tc, hints);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            collect_inlay_hints_from_expr(source, &receiver.0, tc, hints);
            for arg in args {
                let expr = arg.expr();
                collect_inlay_hints_from_expr(source, &expr.0, tc, hints);
            }
        }
        Expr::Tuple(exprs) | Expr::Array(exprs) | Expr::Join(exprs) => {
            for expr in exprs {
                collect_inlay_hints_from_expr(source, &expr.0, tc, hints);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            collect_inlay_hints_from_expr(source, &value.0, tc, hints);
            collect_inlay_hints_from_expr(source, &count.0, tc, hints);
        }
        Expr::MapLiteral { entries } => {
            for (key, value) in entries {
                collect_inlay_hints_from_expr(source, &key.0, tc, hints);
                collect_inlay_hints_from_expr(source, &value.0, tc, hints);
            }
        }
        Expr::StructInit { fields, .. } => {
            for (_, value) in fields {
                collect_inlay_hints_from_expr(source, &value.0, tc, hints);
            }
        }
        Expr::Send { target, message } => {
            collect_inlay_hints_from_expr(source, &target.0, tc, hints);
            collect_inlay_hints_from_expr(source, &message.0, tc, hints);
        }
        Expr::Spawn { target, args } => {
            collect_inlay_hints_from_expr(source, &target.0, tc, hints);
            for (_, value) in args {
                collect_inlay_hints_from_expr(source, &value.0, tc, hints);
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                collect_inlay_hints_from_expr(source, &arm.source.0, tc, hints);
                collect_inlay_hints_from_expr(source, &arm.body.0, tc, hints);
            }
            if let Some(timeout_clause) = timeout {
                collect_inlay_hints_from_expr(source, &timeout_clause.duration.0, tc, hints);
                collect_inlay_hints_from_expr(source, &timeout_clause.body.0, tc, hints);
            }
        }
        Expr::Timeout { expr, duration } => {
            collect_inlay_hints_from_expr(source, &expr.0, tc, hints);
            collect_inlay_hints_from_expr(source, &duration.0, tc, hints);
        }
        Expr::FieldAccess { object, .. } => {
            collect_inlay_hints_from_expr(source, &object.0, tc, hints);
        }
        Expr::Index { object, index } => {
            collect_inlay_hints_from_expr(source, &object.0, tc, hints);
            collect_inlay_hints_from_expr(source, &index.0, tc, hints);
        }
        Expr::Cast { expr: inner, .. }
        | Expr::PostfixTry(inner)
        | Expr::Await(inner)
        | Expr::Yield(Some(inner)) => {
            collect_inlay_hints_from_expr(source, &inner.0, tc, hints);
        }
        Expr::Range { start, end, .. } => {
            if let Some(start) = start {
                collect_inlay_hints_from_expr(source, &start.0, tc, hints);
            }
            if let Some(end) = end {
                collect_inlay_hints_from_expr(source, &end.0, tc, hints);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr(expr) = part {
                    collect_inlay_hints_from_expr(source, &expr.0, tc, hints);
                }
            }
        }
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::Cooperate
        | Expr::This
        | Expr::ScopeCancel
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_)
        | Expr::Yield(None) => {}
    }
}

/// Find the end offset of the variable name in a `var name = ...` statement.
fn find_var_name_end(source: &str, value_span: &Span, name: &str) -> usize {
    let before_eq = &source[..value_span.start];
    if let Some(eq_pos) = before_eq.rfind('=') {
        let trimmed = source[..eq_pos].trim_end();
        if trimmed.ends_with(name) {
            return trimmed.len();
        }
    }
    value_span.start.saturating_sub(3)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_types::Ty;
    use std::collections::{HashMap, HashSet};

    fn parse(source: &str) -> hew_parser::ParseResult {
        hew_parser::parse(source)
    }

    /// Build a `TypeCheckOutput` using the real type checker.
    fn type_check(pr: &hew_parser::ParseResult) -> TypeCheckOutput {
        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let mut checker = hew_types::Checker::new(registry);
        checker.check_program(&pr.program)
    }

    /// Build a `TypeCheckOutput` with a single manually placed expression type.
    fn make_tc_with_expr_type(span_start: usize, span_end: usize, ty: Ty) -> TypeCheckOutput {
        let mut expr_types = HashMap::new();
        expr_types.insert(
            SpanKey {
                start: span_start,
                end: span_end,
            },
            ty,
        );
        TypeCheckOutput {
            expr_types,
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
        }
    }

    fn lambda_hint_labels(hints: &[InlayHint]) -> Vec<&str> {
        hints
            .iter()
            .filter(|hint| hint.kind == InlayHintKind::Type && hint.label.starts_with("-> "))
            .map(|hint| hint.label.as_str())
            .collect()
    }

    #[test]
    fn hint_for_unannotated_let_binding() {
        let source = "fn main() {\n    let x = 42;\n}";
        let pr = parse(source);
        let tc = type_check(&pr);
        let hints = build_inlay_hints(source, &pr, &tc);
        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == InlayHintKind::Type)
            .collect();
        // Assertions outside the guard — test fails if expr_types is empty
        assert!(
            !tc.expr_types.is_empty(),
            "type checker should populate expr_types for integer literal"
        );
        assert!(
            !type_hints.is_empty(),
            "should produce type hint for unannotated let binding"
        );
        assert!(
            type_hints[0].label.starts_with(": "),
            "type hint should start with ': ', got: {}",
            type_hints[0].label
        );
    }

    #[test]
    fn no_hint_when_type_is_annotated() {
        let source = "fn main() {\n    let x: i32 = 42;\n}";
        let pr = parse(source);
        let tc = type_check(&pr);
        let hints = build_inlay_hints(source, &pr, &tc);
        // Annotated bindings should not get type hints
        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == InlayHintKind::Type)
            .collect();
        assert!(
            type_hints.is_empty(),
            "annotated let binding should not get a type hint"
        );
    }

    #[test]
    fn hint_for_var_binding_with_manual_tc() {
        // Parse and extract the value expression span for manual TypeCheckOutput
        let source = "fn main() {\n    var count = 0;\n}";
        let pr = parse(source);
        // Navigate the AST to find the var statement's value span
        if let Some((Item::Function(f), _)) = pr.program.items.first() {
            if let Some((
                Stmt::Var {
                    value: Some(val), ..
                },
                _,
            )) = f.body.stmts.first()
            {
                let tc = make_tc_with_expr_type(val.1.start, val.1.end, Ty::I64);
                let hints = build_inlay_hints(source, &pr, &tc);
                let type_hints: Vec<_> = hints
                    .iter()
                    .filter(|h| h.kind == InlayHintKind::Type)
                    .collect();
                assert!(!type_hints.is_empty(), "var binding should get a type hint");
                assert_eq!(type_hints[0].label, ": int");
            } else {
                panic!("expected var statement with value");
            }
        } else {
            panic!("expected function item");
        }
    }

    #[test]
    fn hint_for_let_binding_with_manual_tc() {
        let source = "fn main() {\n    let name = \"hello\";\n}";
        let pr = parse(source);
        if let Some((Item::Function(f), _)) = pr.program.items.first() {
            if let Some((
                Stmt::Let {
                    value: Some(val), ..
                },
                _,
            )) = f.body.stmts.first()
            {
                let tc = make_tc_with_expr_type(val.1.start, val.1.end, Ty::String);
                let hints = build_inlay_hints(source, &pr, &tc);
                let type_hints: Vec<_> = hints
                    .iter()
                    .filter(|h| h.kind == InlayHintKind::Type)
                    .collect();
                assert!(
                    !type_hints.is_empty(),
                    "unannotated let should get type hint"
                );
                assert!(type_hints[0].label.contains("String"));
            } else {
                panic!("expected let statement with value");
            }
        } else {
            panic!("expected function item");
        }
    }

    #[test]
    fn lambda_in_call_argument_gets_return_hint() {
        let source = "fn main() { foo((x) => x + 1); }";
        let pr = parse(source);

        let lambda_body = match &pr.program.items[0].0 {
            Item::Function(f) => match &f.body.stmts[0].0 {
                Stmt::Expression((Expr::Call { args, .. }, _)) => match args.first() {
                    Some(arg) => match &arg.expr().0 {
                        Expr::Lambda { body, .. } => body,
                        other => panic!("expected lambda argument, got {other:?}"),
                    },
                    None => panic!("expected call argument"),
                },
                other => panic!("expected call expression statement, got {other:?}"),
            },
            other => panic!("expected function item, got {other:?}"),
        };

        let tc = make_tc_with_expr_type(lambda_body.1.start, lambda_body.1.end, Ty::I32);
        let hints = build_inlay_hints(source, &pr, &tc);
        let lambda_hints = lambda_hint_labels(&hints);

        assert_eq!(
            lambda_hints,
            vec!["-> i32 "],
            "lambda nested in a call argument should get a return-type hint"
        );
    }

    #[test]
    fn lambda_in_trait_default_body_gets_return_hint() {
        let source = "trait Mapper { fn map() { foo((x) => x + 1); } }";
        let pr = parse(source);

        let lambda_body = match &pr.program.items[0].0 {
            Item::Trait(trait_decl) => match &trait_decl.items[0] {
                TraitItem::Method(method) => match &method.body {
                    Some(body) => match &body.stmts[0].0 {
                        Stmt::Expression((Expr::Call { args, .. }, _)) => match args.first() {
                            Some(arg) => match &arg.expr().0 {
                                Expr::Lambda { body, .. } => body,
                                other => panic!("expected lambda argument, got {other:?}"),
                            },
                            None => panic!("expected call argument"),
                        },
                        other => panic!("expected call expression statement, got {other:?}"),
                    },
                    None => panic!("expected default method body"),
                },
                TraitItem::AssociatedType { .. } => {
                    panic!("expected trait method, got associated type")
                }
            },
            other => panic!("expected trait item, got {other:?}"),
        };

        let tc = make_tc_with_expr_type(lambda_body.1.start, lambda_body.1.end, Ty::I32);
        let hints = build_inlay_hints(source, &pr, &tc);
        let lambda_hints = lambda_hint_labels(&hints);

        assert_eq!(
            lambda_hints,
            vec!["-> i32 "],
            "lambda inside a trait default method body should get a return-type hint"
        );
    }

    #[test]
    fn lambda_in_trailing_block_expression_gets_return_hint() {
        let source = "fn main() { { foo((x) => x + 1) } }";
        let pr = parse(source);

        let lambda_body = match &pr.program.items[0].0 {
            Item::Function(f) => match &f.body.trailing_expr {
                Some(expr) => match &expr.0 {
                    Expr::Block(block) => match &block.trailing_expr {
                        Some(expr) => match &expr.0 {
                            Expr::Call { args, .. } => match args.first() {
                                Some(arg) => match &arg.expr().0 {
                                    Expr::Lambda { body, .. } => body,
                                    other => panic!("expected lambda argument, got {other:?}"),
                                },
                                None => panic!("expected call argument"),
                            },
                            other => panic!("expected call trailing expression, got {other:?}"),
                        },
                        None => panic!("expected call trailing expression"),
                    },
                    other => panic!("expected block trailing expression, got {other:?}"),
                },
                None => panic!("expected function trailing expression"),
            },
            other => panic!("expected function item, got {other:?}"),
        };

        let tc = make_tc_with_expr_type(lambda_body.1.start, lambda_body.1.end, Ty::I32);
        let hints = build_inlay_hints(source, &pr, &tc);
        let lambda_hints = lambda_hint_labels(&hints);

        assert_eq!(
            lambda_hints,
            vec!["-> i32 "],
            "lambda in a block trailing expression should get a return-type hint"
        );
    }
}
