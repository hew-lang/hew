//! Type error representation with rich diagnostics.
//!
//! This module defines the error types produced by the type checker,
//! including source locations, suggestions, and secondary notes.

use crate::ty::Ty;
use hew_parser::ast::Span;
use std::fmt;

/// Compute the Levenshtein (edit) distance between two strings.
fn levenshtein(a: &str, b: &str) -> usize {
    let (a, b) = (a.as_bytes(), b.as_bytes());
    let (m, n) = (a.len(), b.len());
    let mut prev: Vec<usize> = (0..=n).collect();
    let mut curr = vec![0; n + 1];
    for i in 1..=m {
        curr[0] = i;
        for j in 1..=n {
            let cost = usize::from(a[i - 1] != b[j - 1]);
            curr[j] = (prev[j] + 1).min(curr[j - 1] + 1).min(prev[j - 1] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    prev[n]
}

/// Find names similar to `target` from `candidates`, returning up to 3 suggestions.
///
/// A candidate is considered similar if its Levenshtein distance is at most
/// max(1, `target.len()` / 3), which scales with identifier length.
pub fn find_similar<'a, I>(target: &str, candidates: I) -> Vec<String>
where
    I: IntoIterator<Item = &'a str>,
{
    let max_dist = (target.len() / 3).max(1);
    let mut matches: Vec<(usize, String)> = candidates
        .into_iter()
        .filter(|c| *c != target)
        .filter_map(|c| {
            let d = levenshtein(target, c);
            (d <= max_dist).then(|| (d, c.to_string()))
        })
        .collect();
    matches.sort_by_key(|(d, _)| *d);
    matches.truncate(3);
    matches.into_iter().map(|(_, s)| s).collect()
}

/// Diagnostic severity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// A hard error that prevents compilation.
    Error,
    /// A warning that does not block compilation.
    Warning,
}

/// A type error with location, message, and diagnostic hints.
#[derive(Debug, Clone)]
pub struct TypeError {
    /// The severity of this diagnostic.
    pub severity: Severity,
    /// The kind of error
    pub kind: TypeErrorKind,
    /// Source location of the error
    pub span: Span,
    /// Human-readable error message
    pub message: String,
    /// Additional context with locations
    pub notes: Vec<(Span, String)>,
    /// "Did you mean?" suggestions
    pub suggestions: Vec<String>,
}

impl TypeError {
    /// Create a new type error.
    #[must_use]
    pub fn new(kind: TypeErrorKind, span: Span, message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            kind,
            span,
            message: message.into(),
            notes: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    /// Add a note with location.
    #[must_use]
    pub fn with_note(mut self, span: Span, note: impl Into<String>) -> Self {
        self.notes.push((span, note.into()));
        self
    }

    /// Add a suggestion.
    #[must_use]
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestions.push(suggestion.into());
        self
    }

    /// Create a type mismatch error.
    #[must_use]
    pub fn mismatch(span: Span, expected: &Ty, actual: &Ty) -> Self {
        Self::new(
            TypeErrorKind::Mismatch {
                expected: expected.user_facing().to_string(),
                actual: actual.user_facing().to_string(),
            },
            span,
            format!(
                "expected `{}`, found `{}`",
                expected.user_facing(),
                actual.user_facing()
            ),
        )
    }

    /// Create an undefined function error.
    #[must_use]
    pub fn undefined_function(span: Span, name: &str) -> Self {
        Self::new(
            TypeErrorKind::UndefinedFunction,
            span,
            format!("cannot find function `{name}` in this scope"),
        )
    }

    /// Create an undefined field error.
    #[must_use]
    pub fn undefined_field(span: Span, ty: &Ty, field: &str) -> Self {
        Self::new(
            TypeErrorKind::UndefinedField,
            span,
            format!("no field `{field}` on type `{}`", ty.user_facing()),
        )
    }

    /// Create an invalid operation error.
    #[must_use]
    pub fn invalid_operation(span: Span, op: &str, ty: &Ty) -> Self {
        Self::new(
            TypeErrorKind::InvalidOperation,
            span,
            format!("cannot apply `{op}` to type `{}`", ty.user_facing()),
        )
    }

    /// Create an inference failed error.
    #[must_use]
    pub fn inference_failed(span: Span, context: &str) -> Self {
        Self::new(
            TypeErrorKind::InferenceFailed,
            span,
            format!(
                "cannot infer type{}",
                if context.is_empty() {
                    String::new()
                } else {
                    format!(" for {context}")
                }
            ),
        )
        .with_suggestion("consider adding a type annotation".to_string())
    }

    /// Create a non-exhaustive match error.
    #[must_use]
    pub fn non_exhaustive_match(span: Span, missing_patterns: &[String]) -> Self {
        let detail = if missing_patterns.is_empty() {
            "missing some patterns".to_string()
        } else {
            format!("missing {}", missing_patterns.join(", "))
        };
        Self::non_exhaustive_match_detail(span, Severity::Error, detail)
    }

    /// Create a non-exhaustive match diagnostic with an explicit severity.
    #[must_use]
    pub(crate) fn non_exhaustive_match_detail(
        span: Span,
        severity: Severity,
        detail: impl Into<String>,
    ) -> Self {
        Self {
            severity,
            kind: TypeErrorKind::NonExhaustiveMatch,
            span,
            message: format!("non-exhaustive match: {}", detail.into()),
            notes: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    /// Create a duplicate definition error.
    #[must_use]
    pub fn duplicate_definition(span: Span, name: &str, prev_span: Span) -> Self {
        Self::new(
            TypeErrorKind::DuplicateDefinition,
            span,
            format!("`{name}` is defined multiple times"),
        )
        .with_note(prev_span, "previous definition here".to_string())
    }

    /// Create a mutability error.
    #[must_use]
    pub fn mutability_error(span: Span, name: &str) -> Self {
        Self::new(
            TypeErrorKind::MutabilityError,
            span,
            format!("cannot assign to immutable variable `{name}`"),
        )
        .with_suggestion(format!("consider changing this to `var {name}`"))
    }

    /// Create a return type mismatch error.
    #[must_use]
    pub fn return_type_mismatch(span: Span, expected: &Ty, actual: &Ty) -> Self {
        Self::new(
            TypeErrorKind::ReturnTypeMismatch,
            span,
            format!(
                "return type mismatch: expected `{}`, found `{}`",
                expected.user_facing(),
                actual.user_facing()
            ),
        )
    }

    /// Create a use-after-move error.
    #[must_use]
    pub fn use_after_move(span: Span, name: &str, moved_at: &Span) -> Self {
        Self::new(
            TypeErrorKind::UseAfterMove,
            span,
            format!("use of moved value `{name}`"),
        )
        .with_note(moved_at.clone(), "value was moved here")
    }

    /// Create an unresolved-import error.
    #[must_use]
    pub fn unresolved_import(span: Span, module_path: &str, detail: &str) -> Self {
        Self::new(
            TypeErrorKind::UnresolvedImport,
            span,
            format!("cannot resolve import `{module_path}`: {detail}"),
        )
    }

    /// Create an actor reference cycle warning.
    #[must_use]
    pub fn actor_ref_cycle(span: Span, cycle_desc: &str) -> Self {
        Self {
            severity: Severity::Warning,
            kind: TypeErrorKind::ActorRefCycle,
            span,
            message: format!("actor reference cycle detected: {cycle_desc}"),
            notes: Vec::new(),
            suggestions: vec![
                "consider using weak references or restructuring the supervision tree".to_string(),
            ],
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)?;
        for suggestion in &self.suggestions {
            write!(f, "\n  help: {suggestion}")?;
        }
        Ok(())
    }
}

impl std::error::Error for TypeError {}

/// The specific kind of type error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeErrorKind {
    /// Type mismatch between expected and actual types
    Mismatch {
        /// Expected type
        expected: String,
        /// Actual type found
        actual: String,
    },
    /// Variable not found in scope
    UndefinedVariable,
    /// Type not found in scope
    UndefinedType,
    /// Function not found in scope
    UndefinedFunction,
    /// Field not found on type
    UndefinedField,
    /// Method not found on type
    UndefinedMethod,
    /// Value cannot be sent to another actor
    InvalidSend,
    /// Operation not supported for this type
    InvalidOperation,
    /// Wrong number of arguments
    ArityMismatch,
    /// Generic bounds not satisfied
    BoundsNotSatisfied,
    /// Cannot infer type
    InferenceFailed,
    /// Match expression is not exhaustive
    NonExhaustiveMatch,
    /// Name defined multiple times
    DuplicateDefinition,
    /// Assigning to immutable variable
    MutabilityError,
    /// Return statement type doesn't match function signature
    ReturnTypeMismatch,
    /// Value used after it was moved to an actor
    UseAfterMove,
    /// Yield used outside a generator function
    YieldOutsideGenerator,
    /// Actor types form a reference cycle via `ActorRef` fields
    ActorRefCycle,
    /// Variable defined but never used
    UnusedVariable,
    /// Variable declared `var` but never reassigned
    UnusedMut,
    /// Code style suggestion (e.g., `while true` → `loop`)
    StyleSuggestion,
    /// Imported module never referenced
    UnusedImport,
    /// Code after a `return`, `break`, or `continue` is never executed
    UnreachableCode,
    /// A variable binding shadows a binding from an outer scope
    Shadowing,
    /// A function is defined but never called
    DeadCode,
    /// Pure function calls an impure function or performs a side effect
    PurityViolation,
    /// Impl block violates the orphan rule: neither the type nor the trait is local
    OrphanImpl,
    /// Feature is not available on the selected compilation target
    PlatformLimitation,
    /// Machine state × event exhaustiveness violation
    MachineExhaustivenessError,
    /// Import cannot be resolved: module not found or failed to parse
    UnresolvedImport,
    /// Blocking call inside an actor receive function can starve the scheduler
    BlockingCallInReceiveFn,
    /// Returning a borrowed Rc<T> parameter (or a local tainted by storing
    /// such a parameter) without cloning aliases the caller's pointer,
    /// causing a double-free.  Fail-closed: always an error.
    BorrowedParamReturn,
    /// Storing `Rc<T>` (or a type that transitively contains `Rc<T>`) inside
    /// a collection (`Vec`, `HashMap`, `HashSet`).  The runtime collections do
    /// not track ownership of pointer-valued elements — `hew_vec_free` does not
    /// drop elements, and `HashMap` confuses Rc pointers with strings.  Until
    /// the runtime properly manages owned-type element drops, this is unsound.
    UnsafeCollectionElement,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_with_suggestions() {
        let err =
            TypeError::inference_failed(0..5, "variable").with_suggestion("use let x: i32 = ...");
        assert!(!err.suggestions.is_empty());
    }

    #[test]
    fn test_error_with_notes() {
        let err = TypeError::duplicate_definition(10..20, "foo", 0..5);
        assert_eq!(err.notes.len(), 1);
    }

    #[test]
    fn test_levenshtein() {
        assert_eq!(levenshtein("kitten", "sitting"), 3);
        assert_eq!(levenshtein("", "abc"), 3);
        assert_eq!(levenshtein("abc", "abc"), 0);
        assert_eq!(levenshtein("abc", "ab"), 1);
    }

    #[test]
    fn test_find_similar() {
        let names = ["count", "counter", "total", "value", "result"];
        let similar = find_similar("cont", names.iter().copied());
        // "count" is distance 1 from "cont" (insert 'u'→'n' swap)
        assert!(similar.contains(&"count".to_string()), "got: {similar:?}");
    }

    #[test]
    fn test_find_similar_no_match() {
        let names = ["alpha", "beta", "gamma"];
        let similar = find_similar("zzz", names.iter().copied());
        assert!(similar.is_empty(), "got: {similar:?}");
    }

    #[test]
    fn test_levenshtein_single_edit() {
        // insertion
        assert_eq!(levenshtein("color", "colour"), 1);
        // deletion
        assert_eq!(levenshtein("colour", "color"), 1);
        // substitution
        assert_eq!(levenshtein("cat", "bat"), 1);
    }

    #[test]
    fn test_levenshtein_empty_strings() {
        assert_eq!(levenshtein("", ""), 0);
        assert_eq!(levenshtein("a", ""), 1);
        assert_eq!(levenshtein("", "a"), 1);
    }

    #[test]
    fn test_find_similar_returns_sorted_by_distance() {
        // Target "println" (len 7) → threshold = max(1, 7/3) = 2
        // "printl" dist 1, "printlns" dist 1, "eprintln" dist 1, "print" dist 2
        let names = ["print", "printl", "printlns", "eprintln", "random"];
        let similar = find_similar("println", names.iter().copied());
        // 4 candidates match (dist ≤ 2), but truncate(3) keeps only 3.
        // Sorted by distance, stable within same distance (iterator order):
        //   dist-1: "printl", "printlns", "eprintln"  →  all 3 slots filled
        assert_eq!(
            similar,
            vec!["printl", "printlns", "eprintln"],
            "should return closest matches sorted by distance"
        );
    }

    #[test]
    fn test_find_similar_excludes_exact_match() {
        // find_similar filters out the exact match — we only want *similar* names
        let names = ["foo", "bar", "baz"];
        let similar = find_similar("foo", names.iter().copied());
        assert!(
            !similar.contains(&"foo".to_string()),
            "exact match should be excluded"
        );
    }

    #[test]
    fn test_find_similar_max_three_results() {
        let names = ["ab", "ac", "ad", "ae", "af"];
        let similar = find_similar("aa", names.iter().copied());
        assert!(
            similar.len() <= 3,
            "should cap at 3 results, got {}",
            similar.len()
        );
    }

    // ── Constructor Display output tests ─────────────────────────────

    #[test]
    fn test_mismatch_display() {
        let err = TypeError::mismatch(0..10, &Ty::I32, &Ty::Bool);
        assert_eq!(err.to_string(), "expected `i32`, found `bool`");
        assert_eq!(
            err.kind,
            TypeErrorKind::Mismatch {
                expected: "i32".to_string(),
                actual: "bool".to_string(),
            }
        );
        assert_eq!(err.severity, Severity::Error);
        assert_eq!(err.span, (0..10));
    }

    #[test]
    fn test_mismatch_display_uses_int_alias() {
        let err = TypeError::mismatch(0..10, &Ty::I64, &Ty::option(Ty::I64));
        assert_eq!(err.to_string(), "expected `int`, found `Option<int>`");
        assert_eq!(
            err.kind,
            TypeErrorKind::Mismatch {
                expected: "int".to_string(),
                actual: "Option<int>".to_string(),
            }
        );
    }

    #[test]
    fn test_undefined_variable_display() {
        let err = TypeError::new(
            TypeErrorKind::UndefinedVariable,
            5..15,
            "cannot find value `colour` in this scope",
        );
        assert_eq!(err.to_string(), "cannot find value `colour` in this scope");
        assert_eq!(err.kind, TypeErrorKind::UndefinedVariable);
        assert_eq!(err.span, (5..15));
    }

    #[test]
    fn test_undefined_type_display() {
        let err = TypeError::new(
            TypeErrorKind::UndefinedType,
            0..6,
            "cannot find type `Colour` in this scope",
        );
        assert_eq!(err.to_string(), "cannot find type `Colour` in this scope");
        assert_eq!(err.kind, TypeErrorKind::UndefinedType);
    }

    #[test]
    fn test_undefined_function_display() {
        let err = TypeError::undefined_function(3..8, "greet");
        assert_eq!(
            err.to_string(),
            "cannot find function `greet` in this scope"
        );
        assert_eq!(err.kind, TypeErrorKind::UndefinedFunction);
    }

    #[test]
    fn test_undefined_field_display() {
        let err = TypeError::undefined_field(
            10..20,
            &Ty::Named {
                name: "Point".into(),
                args: vec![],
            },
            "colour",
        );
        assert_eq!(err.to_string(), "no field `colour` on type `Point`");
        assert_eq!(err.kind, TypeErrorKind::UndefinedField);
    }

    #[test]
    fn test_undefined_method_display() {
        let err = TypeError::new(
            TypeErrorKind::UndefinedMethod,
            10..20,
            "no method named `sort_by` found for type `Vec<i32>`",
        );
        assert_eq!(
            err.to_string(),
            "no method named `sort_by` found for type `Vec<i32>`"
        );
        assert_eq!(err.kind, TypeErrorKind::UndefinedMethod);
    }

    #[test]
    fn test_invalid_send_display() {
        let err = TypeError::new(
            TypeErrorKind::InvalidSend,
            0..5,
            "`Rc<i32>` cannot be sent to another actor",
        )
        .with_note(
            0..5,
            "the type must implement `Send` to cross actor boundaries".to_string(),
        );
        assert_eq!(err.to_string(), "`Rc<i32>` cannot be sent to another actor");
        assert_eq!(err.kind, TypeErrorKind::InvalidSend);
        assert_eq!(err.notes.len(), 1);
        assert_eq!(
            err.notes[0].1,
            "the type must implement `Send` to cross actor boundaries"
        );
    }

    #[test]
    fn test_invalid_operation_display() {
        let err = TypeError::invalid_operation(0..3, "+", &Ty::Bool);
        assert_eq!(err.to_string(), "cannot apply `+` to type `bool`");
        assert_eq!(err.kind, TypeErrorKind::InvalidOperation);
    }

    #[test]
    fn test_arity_mismatch_display() {
        let err = TypeError::new(
            TypeErrorKind::ArityMismatch,
            0..10,
            "this function takes 2 argument(s) but 3 were supplied",
        );
        assert_eq!(
            err.to_string(),
            "this function takes 2 argument(s) but 3 were supplied"
        );
        assert_eq!(err.kind, TypeErrorKind::ArityMismatch);
    }

    #[test]
    fn test_bounds_not_satisfied_display() {
        let err = TypeError::new(
            TypeErrorKind::BoundsNotSatisfied,
            0..5,
            "`i32` does not satisfy the bound `Display`",
        );
        assert_eq!(
            err.to_string(),
            "`i32` does not satisfy the bound `Display`"
        );
        assert_eq!(err.kind, TypeErrorKind::BoundsNotSatisfied);
    }

    #[test]
    fn test_inference_failed_display_with_context() {
        let err = TypeError::inference_failed(0..5, "variable `x`");
        assert_eq!(
            err.to_string(),
            "cannot infer type for variable `x`\n  help: consider adding a type annotation"
        );
        assert_eq!(err.kind, TypeErrorKind::InferenceFailed);
        // Built-in suggestion from the constructor
        assert!(err
            .suggestions
            .contains(&"consider adding a type annotation".to_string()));
    }

    #[test]
    fn test_inference_failed_display_empty_context() {
        let err = TypeError::inference_failed(0..5, "");
        assert_eq!(
            err.to_string(),
            "cannot infer type\n  help: consider adding a type annotation"
        );
    }

    #[test]
    fn test_non_exhaustive_match_display() {
        let err = TypeError::non_exhaustive_match(0..20, &["Red".to_string(), "Blue".to_string()]);
        assert_eq!(err.to_string(), "non-exhaustive match: missing Red, Blue");
        assert_eq!(err.kind, TypeErrorKind::NonExhaustiveMatch);
    }

    #[test]
    fn test_non_exhaustive_match_empty_patterns() {
        let err = TypeError::non_exhaustive_match(0..10, &[]);
        assert_eq!(
            err.to_string(),
            "non-exhaustive match: missing some patterns"
        );
    }

    #[test]
    fn test_non_exhaustive_match_warning_detail() {
        let err = TypeError::non_exhaustive_match_detail(
            0..10,
            Severity::Warning,
            "consider adding a wildcard `_` arm",
        );
        assert_eq!(
            err.to_string(),
            "non-exhaustive match: consider adding a wildcard `_` arm"
        );
        assert_eq!(err.severity, Severity::Warning);
        assert_eq!(err.kind, TypeErrorKind::NonExhaustiveMatch);
    }

    #[test]
    fn test_duplicate_definition_display() {
        let err = TypeError::duplicate_definition(10..20, "foo", 0..5);
        assert_eq!(err.to_string(), "`foo` is defined multiple times");
        assert_eq!(err.kind, TypeErrorKind::DuplicateDefinition);
        assert_eq!(err.notes.len(), 1);
        assert_eq!(err.notes[0].0, (0..5));
        assert_eq!(err.notes[0].1, "previous definition here");
    }

    #[test]
    fn test_mutability_error_display() {
        let err = TypeError::mutability_error(0..10, "count");
        assert_eq!(
            err.to_string(),
            "cannot assign to immutable variable `count`\n  help: consider changing this to `var count`"
        );
        assert_eq!(err.kind, TypeErrorKind::MutabilityError);
        assert!(err
            .suggestions
            .contains(&"consider changing this to `var count`".to_string()));
    }

    #[test]
    fn test_return_type_mismatch_display() {
        let err = TypeError::return_type_mismatch(0..10, &Ty::String, &Ty::I32);
        assert_eq!(
            err.to_string(),
            "return type mismatch: expected `String`, found `i32`"
        );
        assert_eq!(err.kind, TypeErrorKind::ReturnTypeMismatch);
    }

    #[test]
    fn test_return_type_mismatch_display_uses_int_alias() {
        let err = TypeError::return_type_mismatch(0..10, &Ty::I64, &Ty::option(Ty::I64));
        assert_eq!(
            err.to_string(),
            "return type mismatch: expected `int`, found `Option<int>`"
        );
    }

    #[test]
    fn test_use_after_move_display() {
        let err = TypeError::use_after_move(20..25, "data", &(5..10));
        assert_eq!(err.to_string(), "use of moved value `data`");
        assert_eq!(err.kind, TypeErrorKind::UseAfterMove);
        assert_eq!(err.notes.len(), 1);
        assert_eq!(err.notes[0].0, (5..10));
        assert_eq!(err.notes[0].1, "value was moved here");
    }

    #[test]
    fn test_actor_ref_cycle_display() {
        let err = TypeError::actor_ref_cycle(0..30, "A -> B -> A");
        assert_eq!(
            err.to_string(),
            "actor reference cycle detected: A -> B -> A\n  help: consider using weak references or restructuring the supervision tree"
        );
        assert_eq!(err.severity, Severity::Warning);
        assert_eq!(err.kind, TypeErrorKind::ActorRefCycle);
    }

    // ── Builder method tests ─────────────────────────────────────────

    #[test]
    fn test_with_note_chaining() {
        let err = TypeError::new(TypeErrorKind::UndefinedVariable, 0..5, "test error")
            .with_note(10..15, "first note")
            .with_note(20..25, "second note");
        assert_eq!(err.notes.len(), 2);
        assert_eq!(err.notes[0].1, "first note");
        assert_eq!(err.notes[1].1, "second note");
    }

    #[test]
    fn test_with_suggestion_chaining() {
        let err = TypeError::new(TypeErrorKind::UndefinedVariable, 0..5, "test")
            .with_suggestion("try A")
            .with_suggestion("try B");
        assert_eq!(err.suggestions.len(), 2);
        assert_eq!(err.suggestions[0], "try A");
        assert_eq!(err.suggestions[1], "try B");
    }

    // ── Display formatting with multiple suggestions ─────────────────

    #[test]
    fn test_display_multiple_suggestions() {
        let err = TypeError::new(TypeErrorKind::UndefinedVariable, 0..5, "not found")
            .with_suggestion("did you mean `x`?")
            .with_suggestion("did you mean `y`?");
        let output = err.to_string();
        assert_eq!(
            output,
            "not found\n  help: did you mean `x`?\n  help: did you mean `y`?"
        );
    }

    #[test]
    fn test_display_no_suggestions() {
        let err = TypeError::new(TypeErrorKind::UndefinedVariable, 0..5, "not found");
        assert_eq!(err.to_string(), "not found");
    }

    // ── Severity tests ───────────────────────────────────────────────

    #[test]
    fn test_default_severity_is_error() {
        let err = TypeError::new(
            TypeErrorKind::Mismatch {
                expected: "i32".into(),
                actual: "bool".into(),
            },
            0..5,
            "mismatch",
        );
        assert_eq!(err.severity, Severity::Error);
    }

    // ── std::error::Error impl ───────────────────────────────────────

    #[test]
    fn test_error_trait_impl() {
        let err = TypeError::mismatch(0..5, &Ty::I32, &Ty::Bool);
        // Verify it can be used as a dyn Error
        let dyn_err: &dyn std::error::Error = &err;
        assert!(dyn_err.to_string().contains("expected `i32`"));
    }

    // ── TypeErrorKind equality ───────────────────────────────────────

    #[test]
    fn test_type_error_kind_equality() {
        assert_eq!(
            TypeErrorKind::UndefinedVariable,
            TypeErrorKind::UndefinedVariable
        );
        assert_ne!(
            TypeErrorKind::UndefinedVariable,
            TypeErrorKind::UndefinedField
        );
        assert_eq!(
            TypeErrorKind::Mismatch {
                expected: "i32".into(),
                actual: "bool".into()
            },
            TypeErrorKind::Mismatch {
                expected: "i32".into(),
                actual: "bool".into()
            },
        );
        assert_ne!(
            TypeErrorKind::Mismatch {
                expected: "i32".into(),
                actual: "bool".into()
            },
            TypeErrorKind::Mismatch {
                expected: "i64".into(),
                actual: "bool".into()
            },
        );
    }

    // ── TypeError Clone ──────────────────────────────────────────────

    // ── Multi-error collection ───────────────────────────────────────

    #[test]
    fn test_multi_error_collection() {
        let errors: Vec<TypeError> = vec![
            TypeError::mismatch(0..5, &Ty::I32, &Ty::Bool),
            TypeError::new(
                TypeErrorKind::UndefinedVariable,
                10..15,
                "cannot find value `x` in this scope",
            ),
            TypeError::new(
                TypeErrorKind::ArityMismatch,
                20..30,
                "this function takes 2 argument(s) but 0 were supplied",
            ),
        ];
        assert_eq!(errors.len(), 3);
        assert_eq!(
            errors[0].kind,
            TypeErrorKind::Mismatch {
                expected: "i32".into(),
                actual: "bool".into(),
            }
        );
        assert_eq!(errors[1].kind, TypeErrorKind::UndefinedVariable);
        assert_eq!(errors[2].kind, TypeErrorKind::ArityMismatch);

        // Verify each formats independently
        assert!(errors[0].to_string().contains("expected `i32`"));
        assert!(errors[1].to_string().contains("cannot find value `x`"));
        assert!(errors[2].to_string().contains("takes 2 argument(s) but 0"));
    }

    // ── Constructor with generic types ───────────────────────────────

    #[test]
    fn test_mismatch_with_complex_types() {
        let expected = Ty::Function {
            params: vec![Ty::I32, Ty::Bool],
            ret: Box::new(Ty::String),
        };
        let actual = Ty::Tuple(vec![Ty::I32, Ty::Bool]);
        let err = TypeError::mismatch(0..20, &expected, &actual);
        assert!(err
            .to_string()
            .contains("expected `fn(i32, bool) -> String`"));
        assert!(err.to_string().contains("found `(i32, bool)`"));
    }

    #[test]
    fn test_undefined_method_on_primitive() {
        let err = TypeError::new(
            TypeErrorKind::UndefinedMethod,
            0..5,
            "no method named `frobnicate` found for type `i32`",
        );
        assert_eq!(
            err.to_string(),
            "no method named `frobnicate` found for type `i32`"
        );
    }

    #[test]
    fn test_undefined_field_on_generic_type() {
        let ty = Ty::Named {
            name: "HashMap".into(),
            args: vec![Ty::String, Ty::I32],
        };
        let err = TypeError::undefined_field(0..10, &ty, "colour");
        assert_eq!(
            err.to_string(),
            "no field `colour` on type `HashMap<String, i32>`"
        );
    }
}
