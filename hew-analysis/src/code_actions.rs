//! Project producer-owned diagnostic edits into editor actions and apply them.

use crate::{CodeAction, RenameEdit};
use hew_parser::diagnostic::Applicability;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// Diagnostic alternatives bound to the exact source snapshot they describe.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagnosticInfo {
    #[serde(default)]
    pub source_hash: String,
    #[serde(default)]
    pub fixes: Vec<CodeAction>,
}

impl DiagnosticInfo {
    #[must_use]
    pub fn new(source: &str, fixes: Vec<CodeAction>) -> Self {
        Self {
            source_hash: source_hash(source),
            fixes,
        }
    }
}

/// The same SHA-256 identity used by consumed compilation inputs.
#[must_use]
pub fn source_hash(source: &str) -> String {
    source_hash_bytes(source.as_bytes())
}

/// Fingerprint consumed bytes, including non-text compile-time inputs.
#[must_use]
pub fn source_hash_bytes(bytes: &[u8]) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    Sha256::digest(bytes)
        .iter()
        .flat_map(|byte| {
            [
                char::from(HEX[usize::from(byte >> 4)]),
                char::from(HEX[usize::from(byte & 15)]),
            ]
        })
        .collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EditError {
    StaleSource,
    InvalidSpan,
    OverlappingEdits,
}

impl std::fmt::Display for EditError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::StaleSource => "source changed since its diagnostic was produced",
            Self::InvalidSpan => "edit does not cover a valid UTF-8 source range",
            Self::OverlappingEdits => "selected diagnostic edits overlap",
        })
    }
}

impl std::error::Error for EditError {}

/// Keep semantic choices with the producer; consumers only validate and project.
#[must_use]
pub fn build_code_actions(source: &str, diagnostics: &[DiagnosticInfo]) -> Vec<CodeAction> {
    let hash = source_hash(source);
    diagnostics
        .iter()
        .filter(|diag| diag.source_hash == hash)
        .flat_map(|diag| &diag.fixes)
        .filter(|fix| normalized_edits(source, &fix.edits).is_ok())
        .cloned()
        .collect()
}

/// Select the sole machine-applicable alternative of each diagnostic.
/// Ambiguous alternatives remain available for explicit editor selection.
///
/// # Errors
/// Refuses a stale snapshot, invalid UTF-8 ranges or overlapping selected edits.
pub fn machine_applicable_edits(
    source: &str,
    diagnostics: &[DiagnosticInfo],
) -> Result<Vec<RenameEdit>, EditError> {
    let hash = source_hash(source);
    let mut edits = Vec::new();
    for diagnostic in diagnostics {
        if diagnostic.source_hash != hash {
            return Err(EditError::StaleSource);
        }
        let mut alternatives = diagnostic
            .fixes
            .iter()
            .filter(|fix| fix.applicability == Applicability::MachineApplicable);
        if let Some(fix) = alternatives.next() {
            if alternatives.next().is_none() {
                edits.extend(fix.edits.iter().cloned());
            }
        }
    }
    normalized_edits(source, &edits)
}

fn normalized_edits(source: &str, edits: &[RenameEdit]) -> Result<Vec<RenameEdit>, EditError> {
    let mut edits = edits.to_vec();
    edits.sort_by_key(|edit| (edit.span.start, edit.span.end));
    edits.dedup();
    for edit in &edits {
        if edit.span.start > edit.span.end || source.get(edit.span.start..edit.span.end).is_none() {
            return Err(EditError::InvalidSpan);
        }
    }
    for pair in edits.windows(2) {
        let left = pair[0].span;
        let right = pair[1].span;
        if right.start < left.end || (right.start == left.start && left.start == left.end) {
            return Err(EditError::OverlappingEdits);
        }
    }
    Ok(edits)
}

/// Apply one validated, nonoverlapping set to its unchanged UTF-8 snapshot.
///
/// # Errors
/// Refuses invalid UTF-8 ranges and overlapping edits.
pub fn apply_edits(source: &str, edits: &[RenameEdit]) -> Result<String, EditError> {
    let edits = normalized_edits(source, edits)?;
    let mut result = source.to_string();
    for edit in edits.iter().rev() {
        result.replace_range(edit.span.start..edit.span.end, &edit.new_text);
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_parser::diagnostic::{Applicability as A, DiagnosticFix as Fix};

    #[test]
    fn alternatives_preserve_choice_and_stale_unicode_edits_refuse() {
        let source = "é value";
        let fix = Fix::replace("Rename", 3..8, "other", A::MachineApplicable);
        let diagnostic = DiagnosticInfo::new(source, vec![fix.clone()]);
        let edits = machine_applicable_edits(source, &[diagnostic.clone()]).unwrap();
        assert_eq!(apply_edits(source, &edits).unwrap(), "é other");
        assert_eq!(
            machine_applicable_edits("prefix é value", &[diagnostic]),
            Err(EditError::StaleSource)
        );
        let ambiguous = DiagnosticInfo::new(
            source,
            vec![
                fix.clone(),
                Fix::replace("Alternative", 3..8, "third", A::MachineApplicable),
            ],
        );
        assert!(machine_applicable_edits(source, &[ambiguous.clone()])
            .unwrap()
            .is_empty());
        assert_eq!(build_code_actions(source, &[ambiguous]).len(), 2);
        assert_eq!(
            apply_edits(
                source,
                &[crate::RenameEdit {
                    span: (1..2).into(),
                    new_text: String::new()
                }]
            ),
            Err(EditError::InvalidSpan)
        );
        let conflicting = DiagnosticInfo::new(
            source,
            vec![Fix {
                title: "Conflict".into(),
                applicability: A::MachineApplicable,
                edits: vec![
                    fix.edits[0].clone(),
                    Fix::replace("Inner", 4..6, "x", A::MachineApplicable)
                        .edits
                        .remove(0),
                ],
            }],
        );
        assert_eq!(
            machine_applicable_edits(source, &[conflicting]),
            Err(EditError::OverlappingEdits)
        );
    }
}

#[cfg(test)]
mod producer_tests {
    use super::*;
    use hew_types::error::TypeErrorKind;
    use hew_types::{Checker, TypeError};

    fn diagnostics(source: &str) -> Vec<TypeError> {
        let parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let output = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]))
            .check_program(&parsed.program);
        output.errors.into_iter().chain(output.warnings).collect()
    }

    fn action(source: &str, kind: TypeErrorKind) -> CodeAction {
        let diagnostic = diagnostics(source)
            .into_iter()
            .find(|error| error.kind == kind)
            .unwrap_or_else(|| panic!("missing {kind:?}"));
        assert!(!diagnostic.fixes.is_empty(), "{diagnostic:?}");
        let info = DiagnosticInfo::new(source, diagnostic.fixes);
        // Human prose is absent from the edit contract entirely.
        build_code_actions(source, &[info.clone()]).remove(0)
    }

    #[test]
    fn diagnostic_fixes_preserve_generic_callee_trivia_and_prefix_shifts() {
        for prefix in ["", "// é😀 shifted\n"] {
            let source = format!("{prefix}fn helper<T>(value: T) -> T {{ value }} fn main() {{ let _ = helpr /* retained */ <i64>(42); }}");
            let fix = action(&source, TypeErrorKind::UndefinedFunction);
            assert_eq!(fix.applicability, Applicability::MaybeIncorrect);
            assert_eq!(
                &source[fix.edits[0].span.start..fix.edits[0].span.end],
                "helpr"
            );
            let updated = apply_edits(&source, &fix.edits).unwrap();
            assert!(updated.contains("helper /* retained */ <i64>(42)"));
            assert!(
                !diagnostics(&updated)
                    .iter()
                    .any(|error| error.severity == hew_types::error::Severity::Error),
                "{updated}"
            );
        }
    }

    #[test]
    fn diagnostic_fixes_target_field_and_method_tokens() {
        for (source, kind, old, new) in [
            ("type Point { value: i64; } fn main() { let point = Point { value: 3 }; println(point.valu); }", TypeErrorKind::UndefinedField, "valu", "value"),
            ("type Point { value: i64; } impl Point { fn read(self) -> i64 { self.value } } fn main() { let point = Point { value: 3 }; println(point.rea()); }", TypeErrorKind::UndefinedMethod, "rea", "read"),
            ("type Point { value: i64; } fn main() { let _ = Point { valu: 3 }; }", TypeErrorKind::UndefinedField, "valu", "value"),
        ] {
            let fix = action(source, kind);
            assert_eq!(&source[fix.edits[0].span.start..fix.edits[0].span.end], old);
            assert_eq!(fix.edits[0].new_text, new);
            let updated = apply_edits(source, &fix.edits).unwrap();
            assert!(!diagnostics(&updated).iter().any(|error| error.severity == hew_types::error::Severity::Error), "{updated}");
        }
    }

    #[test]
    fn diagnostic_fixes_keep_binding_initializers_and_choose_explicit_names() {
        let source = "fn side_effect() -> i64 { println(7); 3 } fn main() { let _held = 9; let held = side_effect(); println(_held); }";
        let fix = action(source, TypeErrorKind::UnusedVariable);
        assert_eq!(fix.applicability, Applicability::MaybeIncorrect);
        let updated = apply_edits(source, &fix.edits).unwrap();
        assert!(updated.contains("let _held = side_effect()"));
        assert!(
            machine_applicable_edits(source, &[DiagnosticInfo::new(source, vec![fix])])
                .unwrap()
                .is_empty()
        );
        let shadow = diagnostics("fn main() { let x = 1; { let x = 2; println(x); } println(x); }");
        assert!(shadow
            .iter()
            .any(|error| error.kind == TypeErrorKind::Shadowing));
        assert!(shadow
            .iter()
            .filter(|error| error.kind == TypeErrorKind::Shadowing)
            .all(|error| error.fixes.is_empty()));
    }

    #[test]
    fn diagnostic_fixes_use_declaration_sites_and_preserve_loop_labels() {
        let source = "fn main() { let value = 1; let _text = \"let value = 9\"; value = 2; println(value); }";
        let fix = action(source, TypeErrorKind::MutabilityError);
        assert_eq!(
            &source[fix.edits[0].span.start..fix.edits[0].span.end],
            "let"
        );
        assert!(apply_edits(source, &fix.edits)
            .unwrap()
            .starts_with("fn main() { var value"));
        let source = "fn main() { @outer: while (true) /* retained */ { break @outer; } }";
        let fix = action(source, TypeErrorKind::StyleSuggestion);
        let updated = apply_edits(source, &fix.edits).unwrap();
        assert!(updated.contains("@outer: loop /* retained */"));
        assert!(hew_parser::parse(&updated).errors.is_empty(), "{updated}");
    }

    #[test]
    fn diagnostic_fixes_do_not_turn_type_help_into_replacement_text() {
        let source = "fn f(value: Unit) {} fn main() {}";
        let fix = action(source, TypeErrorKind::UndefinedType);
        assert_eq!(fix.edits[0].new_text, "()");
        assert!(diagnostics(&apply_edits(source, &fix.edits).unwrap())
            .iter()
            .all(|error| error.kind != TypeErrorKind::UndefinedType));
    }

    #[test]
    fn diagnostic_parser_fix_is_owned_by_the_typed_error() {
        let source = "type Point { value: i64, }";
        let parsed = hew_parser::parse(source);
        let error = parsed
            .errors
            .into_iter()
            .find(|error| !error.fixes.is_empty())
            .unwrap();
        let fixes = build_code_actions(source, &[DiagnosticInfo::new(source, error.fixes)]);
        assert_eq!(fixes.len(), 1);
        let updated = apply_edits(source, &fixes[0].edits).unwrap();
        assert!(hew_parser::parse(&updated).errors.is_empty(), "{updated}");
    }
}
