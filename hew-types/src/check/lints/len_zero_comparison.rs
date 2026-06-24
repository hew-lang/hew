//! The `len_zero_comparison` lint.
//!
//! Flags a comparison of `something.len()` against the integer literal `0` or
//! `1` whose intent is an emptiness test, and suggests `is_empty()` /
//! `!is_empty()`:
//!
//! | comparison (either operand order)        | meaning      | suggestion     |
//! | ---------------------------------------- | ------------ | -------------- |
//! | `len() == 0`                             | empty        | `is_empty()`   |
//! | `len() != 0`, `len() > 0`, `len() >= 1`  | not empty    | `!is_empty()`  |
//! | `len() <= 0`, `len() < 1`                | empty        | `is_empty()`   |
//!
//! This is the precise, type-checked, suppressible companion to the syntactic
//! `rules/hew/len-zero-is-empty.yml` ast-grep rule; the two intentionally
//! coexist (the ast-grep rule is fast and editor-local, this one is type-aware
//! and honours `// hew:allow`).
//!
//! ## Precision over recall
//!
//! - One operand must be literally `<recv>.len()` (a no-argument method call);
//!   a `len` *field* access never qualifies.
//! - The other operand must be the integer literal `0` or `1` (with the
//!   operator paired so the predicate is a genuine emptiness test — see the
//!   table; `len() > 1`, `len() == 1`, … are not flagged).
//! - The receiver's resolved type must expose `is_empty()` — `Vec`, `HashSet`,
//!   and `string` — verified through [`LintCtx::resolved_type_at`].
//!   **`HashMap` is intentionally excluded**: it has a `len()` method but no
//!   `is_empty()` in the dispatch registry or the `HashMapMethod` enum, so
//!   suggesting `m.is_empty()` on a `HashMap` would produce a program that fails
//!   at MIR lowering.

use hew_parser::ast::{BinaryOp, Block, Expr, Literal, Span, Spanned};

use crate::builtin_type::BuiltinType;
use crate::error::TypeError;
use crate::ty::Ty;

use super::{LintCtx, LintId, LintLevels, NodeVisitor};

/// Entry point: walk `body` and flag every `len()`-versus-`0`/`1` comparison.
pub(super) fn check(ctx: &LintCtx, levels: &LintLevels, body: &Block, out: &mut Vec<TypeError>) {
    let mut visitor = LenZero {
        ctx,
        hits: Vec::new(),
    };
    super::walk_body(body, &mut visitor);
    for hit in visitor.hits {
        let (message, suggestion) = hit.render();
        ctx.emit(
            levels,
            LintId::LenZeroComparison,
            &hit.span,
            message,
            suggestion,
            out,
        );
    }
}

struct Hit {
    span: Span,
    /// `true` ⇒ the comparison means "empty" (`is_empty()`); `false` ⇒ "not
    /// empty" (`!is_empty()`).
    means_empty: bool,
    /// Best-effort rendering of the receiver, when it is a simple
    /// identifier / field path; `None` falls back to a generic suggestion.
    receiver: Option<String>,
}

impl Hit {
    fn render(&self) -> (String, String) {
        if self.means_empty {
            let suggestion = match &self.receiver {
                Some(recv) => format!("replace with `{recv}.is_empty()`"),
                None => "use `is_empty()` instead of comparing `len()` to zero".to_string(),
            };
            (
                "this `len()` comparison is exactly `is_empty()`".to_string(),
                suggestion,
            )
        } else {
            let suggestion = match &self.receiver {
                Some(recv) => format!("replace with `!{recv}.is_empty()`"),
                None => "use `!...is_empty()` instead of comparing `len()`".to_string(),
            };
            (
                "this `len()` comparison is exactly `!is_empty()`".to_string(),
                suggestion,
            )
        }
    }
}

struct LenZero<'a> {
    ctx: &'a LintCtx<'a>,
    hits: Vec<Hit>,
}

impl NodeVisitor for LenZero<'_> {
    fn visit_expr(&mut self, expr: &Expr, span: &Span) {
        let Expr::Binary { left, op, right } = expr else {
            return;
        };
        // Identify which side is `<recv>.len()` and which is the literal.
        let (receiver, value, len_on_left) =
            if let (Some(recv), Some(v)) = (len_receiver(&left.0), int_literal(&right.0)) {
                (recv, v, true)
            } else if let (Some(recv), Some(v)) = (len_receiver(&right.0), int_literal(&left.0)) {
                (recv, v, false)
            } else {
                return;
            };
        let Some(means_empty) = comparison_meaning(*op, value, len_on_left) else {
            return;
        };
        if !self
            .ctx
            .resolved_type_at(&receiver.1)
            .is_some_and(|ty| type_has_is_empty(&ty))
        {
            return;
        }
        self.hits.push(Hit {
            span: span.clone(),
            means_empty,
            receiver: render_receiver(&receiver.0),
        });
    }
}

/// The receiver of a no-argument `.len()` call, or `None`.
fn len_receiver(expr: &Expr) -> Option<&Spanned<Expr>> {
    match expr {
        Expr::MethodCall {
            receiver,
            method,
            args,
        } if method == "len" && args.is_empty() => Some(receiver),
        _ => None,
    }
}

/// The value of a bare non-negative integer literal, or `None`.
fn int_literal(expr: &Expr) -> Option<i64> {
    match expr {
        Expr::Literal(Literal::Integer { value, .. }) => Some(*value),
        _ => None,
    }
}

/// Interpret a `len`-versus-literal comparison.
///
/// Canonicalises to `len <op> value` (flipping the operator when `len` is the
/// right operand) and returns `Some(true)` when the predicate is an emptiness
/// test (`is_empty()`), `Some(false)` when it is a non-emptiness test
/// (`!is_empty()`), and `None` for any operator/literal pairing that is not a
/// clean emptiness test.
fn comparison_meaning(op: BinaryOp, value: i64, len_on_left: bool) -> Option<bool> {
    let canonical = if len_on_left { op } else { flip(op)? };
    match (canonical, value) {
        (BinaryOp::Equal | BinaryOp::LessEqual, 0) | (BinaryOp::Less, 1) => Some(true),
        (BinaryOp::NotEqual | BinaryOp::Greater, 0) | (BinaryOp::GreaterEqual, 1) => Some(false),
        _ => None,
    }
}

/// The operator equivalent after swapping operands (`a < b` ≡ `b > a`); `None`
/// for non-comparison operators.
fn flip(op: BinaryOp) -> Option<BinaryOp> {
    Some(match op {
        BinaryOp::Equal => BinaryOp::Equal,
        BinaryOp::NotEqual => BinaryOp::NotEqual,
        BinaryOp::Less => BinaryOp::Greater,
        BinaryOp::LessEqual => BinaryOp::GreaterEqual,
        BinaryOp::Greater => BinaryOp::Less,
        BinaryOp::GreaterEqual => BinaryOp::LessEqual,
        _ => return None,
    })
}

/// Builtin types that expose both `len()` and a callable `is_empty()` with
/// `len() == 0` equivalent to `is_empty()`.
///
/// Audit (origin/main ca310f0a):
/// - `string`  — `is_empty` registered in the string method table ✓
/// - `Vec<T>`  — `VecMethod::IsEmpty` in dispatch + registry ✓
/// - `HashSet<T>` — `HashSetMethod::IsEmpty` in dispatch + registry ✓
/// - `HashMap<K,V>` — **NOT included**: `HashMapMethod` has no `IsEmpty`
///   variant and the `HashMap` method registry has no `is_empty` entry.  The
///   descriptor table accepts the call for type-checking purposes, but
///   `HashMap::is_empty` fails at MIR lowering because no runtime shim exists.
///   Tracked as a stdlib gap (rc1 candidate); lint suppressed until the method
///   is fully registered.
fn type_has_is_empty(ty: &Ty) -> bool {
    matches!(ty, Ty::String)
        || matches!(
            ty,
            Ty::Named {
                builtin: Some(BuiltinType::Vec | BuiltinType::HashSet),
                ..
            }
        )
}

/// Best-effort source rendering of a receiver expression for the suggestion,
/// restricted to identifier / field-path shapes that round-trip cleanly.
fn render_receiver(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Identifier(name) => Some(name.clone()),
        Expr::FieldAccess { object, field } => {
            Some(format!("{}.{field}", render_receiver(&object.0)?))
        }
        _ => None,
    }
}
