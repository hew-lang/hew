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
//! - [`LintSources`] — the per-compilation source text, keyed by module, that
//!   backs in-source `// hew:allow(...)` suppression. The CLI installs it via
//!   [`super::Checker::set_lint_sources`]; a directive on (or above) a finding's
//!   line drops it, even under `Deny`.
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

/// Per-compilation source text, keyed by module, for in-source suppression.
///
/// The checker only carries byte-offset [`Span`]s, not source text, so the
/// front end hands it the program's source(s) through
/// [`super::Checker::set_lint_sources`] before [`super::Checker::check_program`].
/// A lint then resolves the `// hew:allow(...)` directive on (or above) the line
/// of its finding's span. `root` is the entry source the user compiled;
/// `modules` maps a non-root module's dotted name (the same key
/// [`LintCtx::source_module`] carries) to that module's source.
#[derive(Debug, Clone, Default)]
pub struct LintSources {
    root: Option<String>,
    modules: HashMap<String, String>,
}

impl LintSources {
    /// An empty source set (no suppression resolution possible).
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Install the root (entry) source — the file passed to `hew check`/`build`.
    pub fn set_root(&mut self, source: String) {
        self.root = Some(source);
    }

    /// Install a non-root module's source, keyed by its dotted module name
    /// (e.g. `"std.net.http"`), matching the `source_module` tag the lint
    /// sweep stamps onto that module's diagnostics.
    pub fn set_module(&mut self, module: String, source: String) {
        self.modules.insert(module, source);
    }

    /// The source text owning `module`'s spans: the root source when
    /// `module` is `None`, otherwise the named module's source (if known).
    pub(super) fn source_for(&self, module: Option<&str>) -> Option<&str> {
        match module {
            None => self.root.as_deref(),
            Some(name) => self.modules.get(name).map(String::as_str),
        }
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
    /// Source text owning this body's spans, used to resolve in-source
    /// `// hew:allow(...)` directives. `None` when the front end did not
    /// install sources (e.g. internal callers of `check_program`), in which
    /// case suppression is simply skipped.
    pub source: Option<&'a str>,
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

    /// Emit one lint diagnostic, honouring suppression and the configured
    /// [`LintLevel`].
    ///
    /// An in-source `// hew:allow(<id>)` (or `// hew:allow(all)`) directive on
    /// the finding's own line or on the contiguous comment/blank lines directly
    /// above it drops the diagnostic outright — even under `Deny` — mirroring
    /// the rustc/Clippy rule that a local `allow` wins over a command-line
    /// `deny`. Otherwise `Allow` drops the finding, `Warn` emits a
    /// [`Severity::Warning`], and `Deny` emits a [`Severity::Error`]; the caller
    /// (the checker finalization) routes warnings and errors into the right
    /// output vector by severity.
    fn emit(
        &self,
        levels: &LintLevels,
        id: LintId,
        span: &Span,
        message: String,
        suggestion: String,
        out: &mut Vec<TypeError>,
    ) {
        if self
            .source
            .is_some_and(|source| directive_suppresses(source, span.start, id))
        {
            return;
        }
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

/// Whether an in-source `// hew:allow(...)` directive suppresses lint `id` for a
/// construct beginning at byte offset `span_start` in `source`.
///
/// A directive is honoured when it appears either as a trailing comment on the
/// construct's own line, or on the contiguous run of comment / blank lines
/// directly above it (the first line of real code stops the search). This
/// covers `// hew:allow(needless_range_loop)` placed on the line above the loop
/// and, for item-bodied constructs reached through only comments, the
/// item-level form. `all` matches every lint.
fn directive_suppresses(source: &str, span_start: usize, id: LintId) -> bool {
    let lines: Vec<&str> = source.lines().collect();
    if lines.is_empty() {
        return false;
    }
    let construct_line = line_index_at(source, span_start).min(lines.len() - 1);

    if comment_allows(lines[construct_line], id) {
        return true;
    }
    let mut idx = construct_line;
    while idx > 0 {
        idx -= 1;
        let trimmed = lines[idx].trim_start();
        if trimmed.is_empty() {
            continue;
        }
        if trimmed.starts_with("//") {
            if comment_allows(lines[idx], id) {
                return true;
            }
            continue;
        }
        break;
    }
    false
}

/// 0-based index of the line containing byte offset `offset` (clamped to the
/// source length), counting `\n` terminators so it aligns with [`str::lines`].
fn line_index_at(source: &str, offset: usize) -> usize {
    let clamped = offset.min(source.len());
    source
        .bytes()
        .take(clamped)
        .filter(|&byte| byte == b'\n')
        .count()
}

/// Whether `line`'s comment carries a `// hew:allow(...)` directive naming `id`
/// (or `all`). Only the text after the first `//` is considered, so a directive
/// must live in a comment rather than in code.
fn comment_allows(line: &str, id: LintId) -> bool {
    let Some(comment_at) = line.find("//") else {
        return false;
    };
    let comment = &line[comment_at..];
    let Some(open) = comment.find("hew:allow(") else {
        return false;
    };
    let rest = &comment[open + "hew:allow(".len()..];
    let Some(close) = rest.find(')') else {
        return false;
    };
    rest[..close]
        .split(',')
        .map(str::trim)
        .any(|name| name == "all" || name == id.as_str())
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
