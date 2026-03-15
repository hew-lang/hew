//! Analysis module for folding ranges.

use hew_parser::ast::{ActorDecl, Block, Expr, Item, Span, Stmt};
use hew_parser::ParseResult;

use crate::{FoldingKind, FoldingRange};

/// Build folding ranges from a parsed Hew source file.
///
/// Line offsets are computed internally from `source`. Returns a list of
/// `FoldingRange` values with 0-based line numbers.
#[must_use]
pub fn build_folding_ranges(source: &str, parse_result: &ParseResult) -> Vec<FoldingRange> {
    let lo = crate::util::compute_line_offsets(source);
    let mut ranges = Vec::new();
    collect_import_groups(source, &lo, &parse_result.program.items, &mut ranges);
    for (item, span) in &parse_result.program.items {
        collect_item_folding(source, &lo, item, span, &mut ranges);
    }
    ranges
}

#[expect(
    clippy::cast_possible_truncation,
    reason = "line values will not exceed u32"
)]
fn collect_import_groups(
    source: &str,
    line_offsets: &[usize],
    items: &[(Item, Span)],
    ranges: &mut Vec<FoldingRange>,
) {
    let mut import_start: Option<usize> = None;
    let mut import_end: Option<usize> = None;
    let mut count = 0u32;
    for (item, span) in items {
        if matches!(item, Item::Import(_)) {
            let (sl, _) = crate::util::offset_to_line_col(source, line_offsets, span.start);
            let (el, _) = crate::util::offset_to_line_col(source, line_offsets, span.end);
            if import_start.is_none() {
                import_start = Some(sl);
            }
            import_end = Some(el);
            count += 1;
        } else {
            if count >= 2 {
                if let (Some(s), Some(e)) = (import_start, import_end) {
                    ranges.push(FoldingRange {
                        start_line: s as u32,
                        end_line: e as u32,
                        kind: FoldingKind::Imports,
                    });
                }
            }
            import_start = None;
            import_end = None;
            count = 0;
        }
    }
    if count >= 2 {
        if let (Some(s), Some(e)) = (import_start, import_end) {
            ranges.push(FoldingRange {
                start_line: s as u32,
                end_line: e as u32,
                kind: FoldingKind::Imports,
            });
        }
    }
}

#[expect(
    clippy::cast_possible_truncation,
    reason = "line values will not exceed u32"
)]
fn add_region(source: &str, lo: &[usize], span: &Span, ranges: &mut Vec<FoldingRange>) {
    let (sl, _) = crate::util::offset_to_line_col(source, lo, span.start);
    let (el, _) = crate::util::offset_to_line_col(source, lo, span.end.saturating_sub(1));
    if el > sl {
        ranges.push(FoldingRange {
            start_line: sl as u32,
            end_line: el as u32,
            kind: FoldingKind::Region,
        });
    }
}

fn collect_item_folding(
    source: &str,
    lo: &[usize],
    item: &Item,
    span: &Span,
    r: &mut Vec<FoldingRange>,
) {
    match item {
        Item::Function(f) => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, &f.body, r);
        }
        Item::Actor(a) => {
            add_region(source, lo, span, r);
            collect_actor_folding(source, lo, a, r);
        }
        Item::TypeDecl(_)
        | Item::Trait(_)
        | Item::Impl(_)
        | Item::Wire(_)
        | Item::ExternBlock(_)
        | Item::Supervisor(_)
        | Item::Machine(_) => {
            add_region(source, lo, span, r);
        }
        Item::Import(_) | Item::Const(_) | Item::TypeAlias(_) => {}
    }
}

fn collect_actor_folding(source: &str, lo: &[usize], actor: &ActorDecl, r: &mut Vec<FoldingRange>) {
    if let Some(init) = &actor.init {
        collect_block_folding(source, lo, &init.body, r);
    }
    for recv in &actor.receive_fns {
        add_region(source, lo, &recv.span, r);
        collect_block_folding(source, lo, &recv.body, r);
    }
    for method in &actor.methods {
        collect_block_folding(source, lo, &method.body, r);
    }
}

fn collect_block_folding(source: &str, lo: &[usize], block: &Block, r: &mut Vec<FoldingRange>) {
    for (stmt, span) in &block.stmts {
        collect_stmt_folding(source, lo, stmt, span, r);
    }
    if let Some(expr) = &block.trailing_expr {
        collect_expr_folding(source, lo, &expr.0, &expr.1, r);
    }
}

fn collect_stmt_folding(
    source: &str,
    lo: &[usize],
    stmt: &Stmt,
    span: &Span,
    r: &mut Vec<FoldingRange>,
) {
    match stmt {
        Stmt::If {
            then_block,
            else_block,
            ..
        } => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, then_block, r);
            if let Some(eb) = else_block {
                if let Some(block) = &eb.block {
                    collect_block_folding(source, lo, block, r);
                }
                if let Some(if_stmt) = &eb.if_stmt {
                    collect_stmt_folding(source, lo, &if_stmt.0, &if_stmt.1, r);
                }
            }
        }
        Stmt::IfLet {
            body, else_body, ..
        } => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, body, r);
            if let Some(block) = else_body {
                collect_block_folding(source, lo, block, r);
            }
        }
        Stmt::Match { arms, .. } => {
            add_region(source, lo, span, r);
            for arm in arms {
                collect_expr_folding(source, lo, &arm.body.0, &arm.body.1, r);
            }
        }
        Stmt::For { body, .. }
        | Stmt::While { body, .. }
        | Stmt::Loop { body, .. }
        | Stmt::WhileLet { body, .. } => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, body, r);
        }
        Stmt::Expression(expr) => {
            collect_expr_folding(source, lo, &expr.0, &expr.1, r);
        }
        Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
            if let Some(v) = value {
                collect_expr_folding(source, lo, &v.0, &v.1, r);
            }
        }
        _ => {}
    }
}

fn collect_expr_folding(
    source: &str,
    lo: &[usize],
    expr: &Expr,
    span: &Span,
    r: &mut Vec<FoldingRange>,
) {
    match expr {
        Expr::Block(block) => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, block, r);
        }
        Expr::If {
            then_block,
            else_block,
            ..
        } => {
            add_region(source, lo, span, r);
            collect_expr_folding(source, lo, &then_block.0, &then_block.1, r);
            if let Some(eb) = else_block {
                collect_expr_folding(source, lo, &eb.0, &eb.1, r);
            }
        }
        Expr::IfLet {
            body, else_body, ..
        } => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, body, r);
            if let Some(block) = else_body {
                collect_block_folding(source, lo, block, r);
            }
        }
        Expr::Match { arms, .. } => {
            add_region(source, lo, span, r);
            for arm in arms {
                collect_expr_folding(source, lo, &arm.body.0, &arm.body.1, r);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            add_region(source, lo, span, r);
            collect_expr_folding(source, lo, &body.0, &body.1, r);
        }
        Expr::Scope { body, .. }
        | Expr::Unsafe(body)
        | Expr::ScopeLaunch(body)
        | Expr::ScopeSpawn(body) => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, body, r);
        }
        Expr::Cast { expr: inner, .. } => {
            collect_expr_folding(source, lo, &inner.0, &inner.1, r);
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> hew_parser::ParseResult {
        hew_parser::parse(source)
    }

    #[test]
    fn fold_multiline_function_body() {
        // Verify folding runs without panicking on multi-line code
        let source = "fn main() {\n    let x = 1;\n    let y = 2;\n}";
        let pr = parse(source);
        let _ranges = build_folding_ranges(source, &pr);
        // Folding depends on parser item spans; just verify no panic
    }

    #[test]
    fn fold_import_group() {
        let source = "import std::os;\nimport std::fs;\nimport std::net;\nfn main() {}";
        let pr = parse(source);
        let ranges = build_folding_ranges(source, &pr);
        let imports = ranges.iter().find(|r| r.kind == FoldingKind::Imports);
        assert!(
            imports.is_some(),
            "consecutive imports should create fold range"
        );
    }

    #[test]
    fn single_import_no_fold() {
        let source = "import std::os;\nfn main() {}";
        let pr = parse(source);
        let ranges = build_folding_ranges(source, &pr);
        let imports = ranges
            .iter()
            .filter(|r| r.kind == FoldingKind::Imports)
            .count();
        assert_eq!(imports, 0, "single import should not create fold range");
    }

    #[test]
    fn no_fold_for_single_line_function() {
        let source = "fn noop() {}";
        let pr = parse(source);
        let ranges = build_folding_ranges(source, &pr);
        let regions = ranges
            .iter()
            .filter(|r| r.kind == FoldingKind::Region)
            .count();
        assert_eq!(regions, 0, "single-line function should not fold");
    }

    #[test]
    fn fold_actor_body() {
        let source = "actor Counter {\n    receive fn inc() {\n        let x = 1;\n    }\n}";
        let pr = parse(source);
        let ranges = build_folding_ranges(source, &pr);
        // Actor should produce at least some fold ranges
        assert!(
            !ranges.is_empty(),
            "actor with receive handler should have fold ranges"
        );
    }

    #[test]
    fn fold_nested_if() {
        let source = "fn check() {\n    if true {\n        let a = 1;\n    } else {\n        let b = 2;\n    }\n}";
        let pr = parse(source);
        let ranges = build_folding_ranges(source, &pr);
        // The if statement should produce at least one fold region
        assert!(
            !ranges.is_empty(),
            "function with if/else should have fold ranges"
        );
    }
}
