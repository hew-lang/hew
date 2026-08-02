//! The `must_use` lint.
//!
//! Flags a *discarded* value whose type carries a failure that must not be
//! ignored — `WriteError`, `SendError`, and `AskError`, the error arms returned
//! by `Connection.write`, the pid `send` family, and an actor `ask`
//! (`await actor.msg()`). Dropping such a value on the floor silently fails
//! open: a backpressure or disconnect signal vanishes, an undelivered message
//! is never noticed, or a timed-out / mailbox-full / stopped-actor ask is
//! mistaken for a reply. A program should handle it (`?`, `match`) or discard
//! it explicitly (`let _ = …`).
//!
//! The value is flagged in two shapes:
//!
//! - the bare error: `WriteError` / `SendError` / `AskError`;
//! - a `Result<_, E>` whose error arm is one of those — the common case, since
//!   `write()` returns `Result<(), WriteError>`, `send()` returns
//!   `Result<(), SendError>`, and an `ask` returns `Result<Reply, AskError>`.
//!
//! ## Precision over recall
//!
//! - Only fires in **statement position** (a `Stmt::Expression` whose value the
//!   block discards). A trailing expression (the block's value), a `let` /
//!   `var` binding, and a `match` / `if let` scrutinee are all "used" and never
//!   flagged. `expr?` lands here typed as the unwrapped ok arm, so a handled
//!   call is silent; only the unhandled discard remains.
//! - The discarded expression's resolved type must be exactly one of the
//!   must-use errors (`WriteError` / `SendError` / `AskError`) or a `Result<_,
//!   E>` over one, verified through [`LintCtx::resolved_type_at`]; an
//!   unresolved type stays silent.
//! - `// hew:allow(must_use)` (or an explicit `let _ = …`) is the documented
//!   opt-out, mirroring the registry's other suppressible lints.

use hew_parser::ast::{Block, Span, Stmt};

use crate::error::TypeError;
use crate::ty::Ty;
use crate::BuiltinType;

use super::{LintCtx, LintId, LintLevels, NodeVisitor};

/// Entry point: walk `body` and flag every discarded must-use error value.
pub(super) fn check(ctx: &LintCtx, levels: &LintLevels, body: &Block, out: &mut Vec<TypeError>) {
    let mut visitor = MustUse {
        ctx,
        hits: Vec::new(),
    };
    super::walk_body(body, &mut visitor);
    for hit in visitor.hits {
        ctx.emit(
            levels,
            LintId::MustUse,
            &hit.span,
            format!(
                "discarded `{}` value; an ignored {} error fails open",
                hit.error.name, hit.error.class
            ),
            format!(
                "handle it (`?`, `match`) or discard explicitly with `let _ = …` — `{}` reports {}",
                hit.error.name, hit.error.reports
            ),
            out,
        );
    }
}

struct Hit {
    span: Span,
    /// The must-use error the discarded value carries.
    error: MustUseError,
}

struct MustUse<'a> {
    ctx: &'a LintCtx<'a>,
    hits: Vec<Hit>,
}

impl NodeVisitor for MustUse<'_> {
    fn visit_block(&mut self, block: &Block) {
        // Statement position only: a block's `stmts` are evaluated for effect
        // (value discarded); its `trailing_expr` is the block's value and is
        // therefore used. Bindings (`Stmt::Let`/`Stmt::Var`), matches, and the
        // `?` operator are distinct stmt/expr shapes and never reach here.
        for (stmt, _span) in &block.stmts {
            let Stmt::Expression((_expr, expr_span)) = stmt else {
                continue;
            };
            if let Some(error) = self
                .ctx
                .resolved_type_at(expr_span)
                .as_ref()
                .and_then(must_use_error)
            {
                self.hits.push(Hit {
                    span: expr_span.clone(),
                    error,
                });
            }
        }
    }
}

/// A must-use error type and the failure it loses when silently discarded.
///
/// `class` keys the primary diagnostic ("an ignored {class} error fails open");
/// `reports` names the concrete failure in the suggestion. Grouping by class
/// keeps the message identical for every error that fails the same way.
#[derive(Clone, Copy)]
struct MustUseError {
    /// Stable name of the offending error type, shown in the diagnostic.
    name: &'static str,
    /// Error-class phrase for the primary message (`write/send`, `ask`).
    class: &'static str,
    /// What the discarded error silently drops, for the suggestion.
    reports: &'static str,
}

impl MustUseError {
    /// `WriteError` — backpressure / disconnect from `Connection.write`.
    const WRITE: Self = Self {
        name: "WriteError",
        class: "write/send",
        reports: "backpressure, disconnect, or undelivered sends",
    };
    /// `SendError` — an undelivered message from the pid `send` family.
    const SEND: Self = Self {
        name: "SendError",
        class: "write/send",
        reports: "backpressure, disconnect, or undelivered sends",
    };
    /// `AskError` — a failed `await actor.msg()` / lambda-actor `ask`.
    const ASK: Self = Self {
        name: "AskError",
        class: "ask",
        reports: "a timeout, a full mailbox, or a stopped actor",
    };
}

/// The must-use error a discarded value carries, or `None`.
///
/// Matches the bare error type and a `Result<_, E>` whose error arm is one. The
/// match is by canonical name so it covers the builtin `SendError` / `AskError`
/// and the stdlib `WriteError` enum identically.
fn must_use_error(ty: &Ty) -> Option<MustUseError> {
    if let Some((_, err)) = ty.as_result() {
        return error_kind(err);
    }
    error_kind(ty)
}

/// `WriteError` / `SendError` / `AskError` named-type detection.
fn error_kind(ty: &Ty) -> Option<MustUseError> {
    let Ty::Named { name, builtin, .. } = ty else {
        return None;
    };
    if *builtin == Some(BuiltinType::SendError)
        && generated_builtin_enum_has_identity("SendError", name)
    {
        return Some(MustUseError::SEND);
    }
    if *builtin == Some(BuiltinType::AskError)
        && generated_builtin_enum_has_identity("AskError", name)
    {
        return Some(MustUseError::ASK);
    }
    (builtin.is_none() && name == crate::stdlib::STD_NET_WRITE_ERROR).then_some(MustUseError::WRITE)
}

/// Prove that a compiler-owned enum has both its generated discriminator and
/// the exact source owner published by the shared enum catalog.
fn generated_builtin_enum_has_identity(leaf: &str, actual_name: &str) -> bool {
    crate::builtin_enums::monomorphic_builtin_enum(leaf)
        .is_some_and(|fact| fact.canonical_name == actual_name)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn named(name: &str, builtin: Option<BuiltinType>) -> Ty {
        Ty::Named {
            name: name.to_string(),
            args: Vec::new(),
            builtin,
        }
    }

    #[test]
    fn generated_error_requires_discriminator_and_exact_catalog_owner() {
        let ask = crate::builtin_enums::monomorphic_builtin_enum("AskError")
            .expect("generated AskError catalog row");
        assert!(error_kind(&named(ask.canonical_name, Some(BuiltinType::AskError))).is_some());

        for missing_or_wrong in [None, Some(BuiltinType::SendError)] {
            assert!(
                error_kind(&named(ask.canonical_name, missing_or_wrong)).is_none(),
                "the canonical owner alone must not grant AskError authority"
            );
        }
        for foreign_or_leaf in ["foreign.AskError", "AskError"] {
            assert!(
                error_kind(&named(foreign_or_leaf, Some(BuiltinType::AskError))).is_none(),
                "the discriminator alone must not grant AskError authority"
            );
        }
    }

    #[test]
    fn write_error_requires_exact_std_net_source_identity() {
        assert!(error_kind(&named(crate::stdlib::STD_NET_WRITE_ERROR, None)).is_some());
        assert!(error_kind(&named("WriteError", None)).is_none());
        assert!(error_kind(&named("foreign.WriteError", None)).is_none());
    }
}
