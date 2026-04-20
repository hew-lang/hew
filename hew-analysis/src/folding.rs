//! Analysis module for folding ranges.

use crate::ast_visit::{self, AstVisitor};
use hew_parser::ast::{ActorDecl, Expr, Item, Span, Stmt};
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
    let mut visitor = FoldingVisitor {
        source,
        line_offsets: &lo,
        ranges: &mut ranges,
    };
    ast_visit::walk_parse_result(Some(source), parse_result, &mut visitor);
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
        Item::Actor(a) => {
            add_region(source, lo, span, r);
            collect_actor_folding(source, lo, a, r);
        }
        Item::Function(_)
        | Item::TypeDecl(_)
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
    for recv in &actor.receive_fns {
        add_region(source, lo, &recv.span, r);
    }
}

struct FoldingVisitor<'a> {
    source: &'a str,
    line_offsets: &'a [usize],
    ranges: &'a mut Vec<FoldingRange>,
}

impl<'ast> AstVisitor<'ast> for FoldingVisitor<'_> {
    fn visit_stmt(
        &mut self,
        stmt: &'ast Stmt,
        span: &'ast Span,
        _ctx: crate::ast_visit::VisitContext<'ast>,
    ) {
        if matches!(
            stmt,
            Stmt::If { .. }
                | Stmt::IfLet { .. }
                | Stmt::Match { .. }
                | Stmt::For { .. }
                | Stmt::While { .. }
                | Stmt::Loop { .. }
                | Stmt::WhileLet { .. }
        ) {
            add_region(self.source, self.line_offsets, span, self.ranges);
        }
    }

    fn visit_expr(
        &mut self,
        expr: &'ast Expr,
        span: &'ast Span,
        _ctx: crate::ast_visit::VisitContext<'ast>,
    ) {
        if matches!(
            expr,
            Expr::Block(_)
                | Expr::If { .. }
                | Expr::IfLet { .. }
                | Expr::Match { .. }
                | Expr::Lambda { .. }
                | Expr::SpawnLambdaActor { .. }
                | Expr::Scope { .. }
                | Expr::Unsafe(_)
                | Expr::ScopeLaunch(_)
                | Expr::ScopeSpawn(_)
        ) {
            add_region(self.source, self.line_offsets, span, self.ranges);
        }
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

    #[test]
    fn fold_defer_block() {
        let source = "fn cleanup() {\n    defer {\n        close();\n        flush();\n    }\n}";
        let pr = parse(source);
        let ranges = build_folding_ranges(source, &pr);
        // The defer block expression (a multi-line Expr::Block) should
        // produce a fold range covering lines 1 through 4.
        let regions: Vec<_> = ranges
            .iter()
            .filter(|r| r.kind == FoldingKind::Region)
            .collect();
        assert!(
            !regions.is_empty(),
            "defer with multi-line block should produce at least one fold range"
        );
        // Verify we have a fold range starting at the defer line.
        assert!(
            regions.iter().any(|r| r.start_line == 1),
            "expected a fold range starting at the defer line (line 1), got: {regions:?}"
        );
    }

    #[test]
    fn fold_select_arm_body() {
        let source = "fn main() {\n    let value = select {\n        msg from inbox.recv() => {\n            msg\n        },\n        after 100ms => -1,\n    };\n}";
        let pr = parse(source);
        let ranges = build_folding_ranges(source, &pr);
        assert!(
            ranges
                .iter()
                .any(|range| range.kind == FoldingKind::Region && range.start_line == 2),
            "select arm body should produce a fold region: {ranges:?}"
        );
    }
}
