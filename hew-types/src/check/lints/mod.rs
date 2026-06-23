//! Compiler lint pass infrastructure (milestone M1 core).
//!
//! This module hosts the *semantic* lint layer — idiom / code-smell findings
//! that need more than syntax (def-use, types) and therefore live in the
//! compiler rather than in the syntactic ast-grep rule set. Lints are
//! non-fatal by default: each is emitted through the checker's existing
//! [`TypeError`] warning channel ([`super::TypeCheckOutput::warnings`]), which
//! already reaches the CLI, the LSP, and the website with no extra plumbing.
//!
//! ## Public surface (for the CLI / suppression integration)
//!
//! - [`LintId`] — a stable enum of known lints, each with a stable
//!   [`LintId::as_str`] name and a [`LintId::default_level`].
//! - [`LintLevel`] — `Allow` / `Warn` / `Deny`.
//! - [`LintLevels`] — the per-compilation level map. Build it with
//!   [`LintLevels::from_defaults`], read with [`LintLevels::level`], and
//!   reconfigure with [`LintLevels::set`]. The checker stores one of these
//!   (see [`super::Checker::set_lint_levels`]); the CLI threads `--warn` /
//!   `--allow` / `--deny` flags into it before [`super::Checker::check_program`]
//!   runs the sweep.
//!
//! ## The sweep
//!
//! [`super::Checker::run_lints`] walks every function/method body read-only,
//! constructs a [`LintCtx`] carrying the checker's resolved type facts, and
//! invokes each enabled lint. A lint whose resolved level is `Allow` emits
//! nothing; `Deny` emits at [`Severity::Error`], everything else at
//! [`Severity::Warning`]. The diagnostic carries its originating lint id on
//! [`TypeErrorKind::Lint`] so suppression, `--Werror`, and docs can key off it.

use std::collections::HashMap;

use hew_parser::ast::{Block, Span};

use crate::error::{Severity, TypeError, TypeErrorKind};
use crate::ty::{Substitution, Ty};

use super::types::SpanKey;

mod needless_range_loop;

/// Stable identifier for a single compiler lint.
///
/// Extend this enum (and [`LintId::ALL`]) when adding a lint; the exhaustive
/// matches on [`LintId::as_str`] / [`LintId::default_level`] then force every
/// new lint to declare a stable name and a default level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LintId {
    /// `for i in 0 .. xs.len()` where `i` is only ever used to index `xs` —
    /// the loop should iterate the collection directly.
    NeedlessRangeLoop,
}

impl LintId {
    /// Every lint known to the registry, in declaration order.
    ///
    /// Used to seed [`LintLevels::from_defaults`] and to back
    /// [`LintId::from_name`] so the CLI flag parser stays in sync.
    pub const ALL: &'static [LintId] = &[LintId::NeedlessRangeLoop];

    /// The stable, lowercase string name for this lint.
    ///
    /// This is the spelling accepted by the CLI `--warn` / `--allow` /
    /// `--deny` flags and surfaced on the diagnostic; it is part of the public
    /// contract and must not change without updating docs and editor tooling.
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            LintId::NeedlessRangeLoop => "needless_range_loop",
        }
    }

    /// Parse a lint name (as accepted by the CLI flags) back into a [`LintId`].
    ///
    /// Returns `None` for an unknown name so the caller can fail closed with a
    /// clear "unknown lint" diagnostic rather than silently ignoring the flag.
    #[must_use]
    pub fn from_name(name: &str) -> Option<LintId> {
        LintId::ALL.iter().copied().find(|id| id.as_str() == name)
    }

    /// The level applied when neither the CLI nor in-source attributes
    /// override this lint.
    #[must_use]
    pub fn default_level(self) -> LintLevel {
        match self {
            LintId::NeedlessRangeLoop => LintLevel::Warn,
        }
    }
}

/// The configured reporting level of a lint.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintLevel {
    /// Emit nothing for this lint.
    Allow,
    /// Emit a non-fatal [`Severity::Warning`] diagnostic.
    Warn,
    /// Emit a fatal [`Severity::Error`] diagnostic (fails the build like any
    /// other type error).
    Deny,
}

/// Per-compilation resolution of every lint's reporting level.
///
/// Seeded from [`LintId::default_level`] via [`LintLevels::from_defaults`] and
/// then reconfigured by the CLI layer through [`LintLevels::set`] before the
/// checker runs its sweep. Any lint not explicitly configured resolves to its
/// [`LintId::default_level`].
#[derive(Debug, Clone)]
pub struct LintLevels {
    levels: HashMap<LintId, LintLevel>,
}

impl LintLevels {
    /// Build a level map seeded with every lint's [`LintId::default_level`].
    #[must_use]
    pub fn from_defaults() -> Self {
        let mut levels = HashMap::with_capacity(LintId::ALL.len());
        for id in LintId::ALL.iter().copied() {
            levels.insert(id, id.default_level());
        }
        Self { levels }
    }

    /// The resolved level for `id` (its default when not explicitly set).
    #[must_use]
    pub fn level(&self, id: LintId) -> LintLevel {
        self.levels
            .get(&id)
            .copied()
            .unwrap_or_else(|| id.default_level())
    }

    /// Override the level for `id`. Used by the CLI to apply `--warn` /
    /// `--allow` / `--deny`.
    pub fn set(&mut self, id: LintId, level: LintLevel) {
        self.levels.insert(id, level);
    }
}

impl Default for LintLevels {
    fn default() -> Self {
        Self::from_defaults()
    }
}

/// Read-only view of the checker's type facts that a lint needs.
///
/// Borrows the resolved-type side table and the substitution so a lint can ask
/// "what is the checker type of the expression at this span?" without mutating
/// checker state. `module_idx` selects the right [`SpanKey`] namespace (0 =
/// root unit, N = N-th non-root module) and `source_module` tags emitted
/// diagnostics so the CLI routes them to the correct source file.
pub(super) struct LintCtx<'a> {
    pub subst: &'a Substitution,
    pub expr_types: &'a HashMap<SpanKey, Ty>,
    pub module_idx: u32,
    pub source_module: Option<&'a str>,
}

impl LintCtx<'_> {
    /// The fully-resolved checker type recorded for the expression at `span`.
    ///
    /// Returns `None` when no type was recorded for the span — lints treat a
    /// missing type as "unknown" and stay silent (precision over recall).
    fn resolved_type_at(&self, span: &Span) -> Option<Ty> {
        let key = SpanKey::in_module(span, self.module_idx);
        self.expr_types.get(&key).map(|ty| self.subst.resolve(ty))
    }

    /// Emit one lint diagnostic, honouring the configured [`LintLevel`].
    ///
    /// `Allow` drops the finding; `Warn` emits a [`Severity::Warning`]; `Deny`
    /// emits a [`Severity::Error`]. The caller (the checker finalization)
    /// routes warnings and errors into the right output vector by severity.
    fn emit(
        &self,
        levels: &LintLevels,
        id: LintId,
        span: &Span,
        message: String,
        suggestion: String,
        out: &mut Vec<TypeError>,
    ) {
        let severity = match levels.level(id) {
            LintLevel::Allow => return,
            LintLevel::Warn => Severity::Warning,
            LintLevel::Deny => Severity::Error,
        };
        out.push(TypeError {
            severity,
            kind: TypeErrorKind::Lint(id),
            span: span.clone(),
            message,
            notes: Vec::new(),
            suggestions: vec![suggestion],
            source_module: self.source_module.map(str::to_string),
        });
    }
}

/// Run every enabled lint over one function/method body.
///
/// Invoked once per body by [`super::Checker::run_lints`]; each lint is a pure
/// read-only visitor over the typed AST.
pub(super) fn lint_block(
    ctx: &LintCtx,
    levels: &LintLevels,
    body: &Block,
    out: &mut Vec<TypeError>,
) {
    needless_range_loop::check(ctx, levels, body, out);
}
