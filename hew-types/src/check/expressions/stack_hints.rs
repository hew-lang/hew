//! Checker methods grouped by responsibility: stack hints.
//! Split from `expressions.rs`: checker methods, part 1 of 5.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
use super::super::branch_join::BranchArmExit;
use super::super::coerce::{cast_is_valid, common_integer_type, common_numeric_type};
use super::super::types::GenericLambdaSig;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::check::types::{
    DeferredIsCheck, EqRequirement, GenericCallEdge, GenericCallee, GenericFnInstantiationSite,
    PendingInstantiation,
};
use crate::env::{PlaceConflict, PlacePath};
use crate::BuiltinType;
use hew_parser::ast::Ident;
use std::collections::VecDeque;

impl Checker {
    // ── Diagnostic-only stack-allocation hints (HEW-PERF-001) ─────────────
    //
    // Phase A.0 scaffold: walk every `let` / `var` binding in a function body,
    // classify the right-hand side's allocation class, and append a `StackHint`
    // for every non-`Stack` (non-`Indeterminate`) classification. This pass is
    // intentionally noisy — it emits a hint on every observed heap allocation
    // without escape filtering. False-positive suppression lands in subsequent
    // slices (A.1: return-path; A.2: capture/container; A.3: field/send).
    //
    // Conservative bias: when the RHS form is not recognised, classify as
    // `Indeterminate` (no hint emitted). False negatives are safe; false
    // positives are user-trust defects. This rule already applies in A.0
    // because some well-formed RHS expressions lack populated `expr_types`
    // (e.g. inside generic lambda bodies still being inferred).

    /// Walk a function body and emit `StackHint` entries for every binding
    /// whose RHS resolves to a heap allocation class. Called from
    /// `check_function_as` after `warn_affine_param_escape`.
    pub(in crate::check) fn classify_stack_hints(&mut self, fd: &FnDecl) {
        // Ignore the function's parameter types and return type for hint
        // emission — the walker is binding-scoped, not signature-scoped.
        // Sub-body discipline (`sub-body-scoped-traversal` LESSONS row): a
        // nested function literal is reached via `Stmt::Let` of an
        // `Expr::Lambda`, which is classified as `ClosureEnv` here. The body
        // of that lambda is not re-walked — nested function decls run their
        // own `classify_stack_hints` pass via `check_function_as`.
        self.scan_block_for_stack_hints(&fd.body);
    }

    /// Recursive descent over a block, classifying every binding statement.
    pub(super) fn scan_block_for_stack_hints(&mut self, block: &Block) {
        for (stmt, span) in &block.stmts {
            self.scan_stmt_for_stack_hints(stmt, span);
        }
        if let Some(trailing) = &block.trailing_expr {
            self.scan_expr_for_stack_hints(&trailing.0);
        }
    }

    pub(super) fn scan_stmt_for_stack_hints(&mut self, stmt: &Stmt, stmt_span: &Span) {
        match stmt {
            Stmt::Let { pattern, value, .. } => {
                if let Some((expr, expr_span)) = value {
                    let class = self.classify_alloc(expr, expr_span);
                    let name = match &pattern.0 {
                        Pattern::Identifier(n) => n.name.as_str(),
                        _ => "",
                    };
                    self.maybe_record_stack_hint(stmt_span, name, class);
                    // Descend into the RHS to classify nested bindings inside
                    // block expressions (`let x = { let y = ...; y }`).
                    self.scan_expr_for_stack_hints(expr);
                }
            }
            Stmt::Var { name, value, .. } => {
                if let Some((expr, expr_span)) = value {
                    let class = self.classify_alloc(expr, expr_span);
                    self.maybe_record_stack_hint(stmt_span, name.name.as_str(), class);
                    self.scan_expr_for_stack_hints(expr);
                }
            }
            Stmt::Assign { value, .. } => {
                self.scan_expr_for_stack_hints(&value.0);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.scan_expr_for_stack_hints(&condition.0);
                self.scan_block_for_stack_hints(then_block);
                if let Some(eb) = else_block {
                    self.scan_else_block_for_stack_hints(eb);
                }
            }
            Stmt::IfLet {
                conditions,
                body,
                else_body,
            } => {
                for expr in condition_exprs(conditions) {
                    self.scan_expr_for_stack_hints(&expr.0);
                }
                self.scan_block_for_stack_hints(body);
                if let Some(else_expr) = else_body {
                    self.scan_expr_for_stack_hints(&else_expr.0);
                }
            }
            Stmt::Match { scrutinee, arms } => {
                self.scan_expr_for_stack_hints(&scrutinee.0);
                for arm in arms {
                    self.scan_match_arm_body_for_stack_hints(arm);
                }
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.scan_expr_for_stack_hints(&condition.0);
                self.scan_block_for_stack_hints(body);
            }
            Stmt::WhileLet {
                conditions, body, ..
            } => {
                for expr in condition_exprs(conditions) {
                    self.scan_expr_for_stack_hints(&expr.0);
                }
                self.scan_block_for_stack_hints(body);
            }
            Stmt::For { iterable, body, .. } => {
                self.scan_expr_for_stack_hints(&iterable.0);
                self.scan_block_for_stack_hints(body);
            }
            Stmt::Loop { body, .. } => {
                self.scan_block_for_stack_hints(body);
            }
            Stmt::Expression(expr) => {
                self.scan_expr_for_stack_hints(&expr.0);
            }
            Stmt::Return(opt) => {
                if let Some((e, _)) = opt {
                    self.scan_expr_for_stack_hints(e);
                }
            }
            Stmt::Break { value, .. } => {
                if let Some((e, _)) = value {
                    self.scan_expr_for_stack_hints(e);
                }
            }
            Stmt::Defer(expr) => {
                self.scan_expr_for_stack_hints(&expr.0);
            }
            // Statement forms that cannot host a binding RHS: nothing to do.
            // Listed explicitly so a future Stmt variant addition forces a
            // compile error here (`exhaustive-traversal-and-lowering` LESSONS
            // row — no silent `_ => {}` in semantic positions).
            Stmt::Continue { .. } => {}
        }
    }

    pub(super) fn scan_else_block_for_stack_hints(&mut self, eb: &hew_parser::ast::ElseBlock) {
        if let Some(b) = &eb.block {
            self.scan_block_for_stack_hints(b);
        }
        if let Some(if_stmt) = &eb.if_stmt {
            let (stmt, span) = if_stmt.as_ref();
            self.scan_stmt_for_stack_hints(stmt, span);
        }
    }

    pub(super) fn scan_match_arm_body_for_stack_hints(&mut self, arm: &MatchArm) {
        if let Some((g, _)) = &arm.guard {
            self.scan_expr_for_stack_hints(g);
        }
        self.scan_expr_for_stack_hints(&arm.body.0);
    }

    /// Descend into nested expressions to find `let`-bearing block expressions
    /// and inner lambda bodies. Phase A.0 does not classify expression-position
    /// allocations on their own (e.g. `vec.push(Vec::new())` does not emit a
    /// hint for the inner `Vec::new()` because it is unbound). Only `let` /
    /// `var` bindings produce hints in this slice.
    pub(super) fn scan_expr_for_stack_hints(&mut self, expr: &Expr) {
        match expr {
            Expr::Coalesce { left, right }
            | Expr::Handle {
                operand: left,
                body: right,
                ..
            } => {
                self.scan_expr_for_stack_hints(&left.0);
                self.scan_expr_for_stack_hints(&right.0);
            }
            Expr::Block(block) => self.scan_block_for_stack_hints(block),
            Expr::If {
                condition,
                then_block,
                else_block,
                ..
            } => {
                self.scan_expr_for_stack_hints(&condition.0);
                self.scan_expr_for_stack_hints(&then_block.0);
                if let Some(eb) = else_block {
                    self.scan_expr_for_stack_hints(&eb.0);
                }
            }
            Expr::IfLet {
                conditions,
                body,
                else_body,
            } => {
                // Mirrors the `Stmt::IfLet` arm in `scan_stmt_for_stack_hints`.
                // `body` and `else_body` are bare `Block` values (not `Spanned<Expr>`),
                // so we call `scan_block_for_stack_hints` directly.
                for expr in condition_exprs(conditions) {
                    self.scan_expr_for_stack_hints(&expr.0);
                }
                self.scan_block_for_stack_hints(body);
                if let Some(else_expr) = else_body {
                    self.scan_expr_for_stack_hints(&else_expr.0);
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.scan_expr_for_stack_hints(&scrutinee.0);
                for arm in arms {
                    self.scan_match_arm_body_for_stack_hints(arm);
                }
            }
            // All remaining expression forms — including `Expr::Lambda`
            // (whose body is *not* re-walked here: nested fn / lambda decls
            // run their own walker pass, and the lambda value itself when
            // assigned is classified at the binding site as `ClosureEnv`,
            // so walking the lambda body would double-emit hints) — cannot
            // host a `let` statement directly. Anything reachable through
            // call args, indices, struct fields, or tuple elements is
            // wrapped in an `Expr::Block` when it contains statements,
            // covered by the `Expr::Block` arm above.
            _ => {}
        }
    }

    /// Classify a binding's RHS expression by looking up its synthesised type
    /// in `expr_types`. Phase A.0 recognises the named heap types
    /// (`Vec`, `String`, `HashMap`, `HashSet`, `Rc`) plus closure literals
    /// (`Expr::Lambda`). Everything else maps to `Stack` (already
    /// stack-shaped) or `Indeterminate` (unknown form, no hint).
    pub(super) fn classify_alloc(&self, expr: &Expr, span: &Span) -> AllocationClass {
        // Closure literals are env-heap regardless of resolved type.
        if matches!(expr, Expr::Lambda { .. }) {
            return AllocationClass::ClosureEnv;
        }
        let key = SpanKey::in_module(span, self.current_module_idx);
        match self.expr_types.get(&key) {
            Some(ty) => Self::classify_ty(&self.subst.resolve(ty)),
            // Type not recorded — happens for some inferred or rewritten
            // expressions. Conservative silence per the bias policy.
            None => AllocationClass::Indeterminate,
        }
    }

    pub(super) fn classify_ty(ty: &Ty) -> AllocationClass {
        match ty {
            Ty::String => AllocationClass::String,
            Ty::Named { name, .. } => match name.as_str() {
                "Vec" => AllocationClass::Vec,
                "HashMap" => AllocationClass::HashMap,
                "HashSet" => AllocationClass::HashSet,
                "Rc" => AllocationClass::Rc,
                _ => AllocationClass::Stack,
            },
            // Type variables, primitives, tuples, arrays, function types:
            // either already stack-shaped or not yet resolved. A.0 is
            // conservative.
            _ => AllocationClass::Stack,
        }
    }

    pub(super) fn maybe_record_stack_hint(
        &mut self,
        stmt_span: &Span,
        binding_name: &str,
        class: AllocationClass,
    ) {
        // No hint for stack-shaped or unclassifiable RHSs.
        if matches!(
            class,
            AllocationClass::Stack | AllocationClass::Indeterminate
        ) {
            return;
        }
        self.stack_hints.push(StackHint {
            span_key: SpanKey::in_module(stmt_span, self.current_module_idx),
            binding_name: binding_name.to_string(),
            alloc_class: class,
        });
    }
}
