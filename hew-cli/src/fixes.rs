//! Apply producer-owned fixes to the selected source inputs.

use std::collections::{BTreeMap, HashSet};
use std::path::{Path, PathBuf};

use hew_analysis::code_actions::{
    apply_edits, machine_applicable_edits, source_hash, source_hash_bytes, DiagnosticInfo,
};
use hew_compile::{FrontendDiagnostic, FrontendDiagnosticKind, FrontendOptions};
use hew_types::module_registry::ModuleAnchor;

pub(crate) fn apply(
    input: &Path,
    options: &FrontendOptions,
    diagnostics: &[FrontendDiagnostic],
    dry_run: bool,
) -> Result<usize, String> {
    let inputs = options.documents.inputs();
    let selected = selected_sources(input, options);
    let mut grouped: BTreeMap<PathBuf, (String, Vec<DiagnosticInfo>)> = BTreeMap::new();
    for diagnostic in diagnostics {
        let fixes = match &diagnostic.kind {
            FrontendDiagnosticKind::Parse(error) => &error.fixes,
            FrontendDiagnosticKind::Type(error) => &error.fixes,
            _ => continue,
        };
        if fixes.is_empty() {
            continue;
        }
        let (Some(source), Some(filename)) = (&diagnostic.source, &diagnostic.filename) else {
            continue;
        };
        if hew_pkg::generate::is_generated(source) {
            continue;
        }
        // A rendered filename is accepted only when it resolves back to a
        // consumed canonical input owned by this selection. Its bytes must
        // also match that input's fingerprint.
        let Ok(path) = Path::new(filename).canonicalize() else {
            continue;
        };
        if !selected.contains(&path) {
            continue;
        }
        let Some(consumed) = inputs
            .iter()
            .find(|item| item.path == path && item.dependency_root.is_none())
        else {
            continue;
        };
        if consumed.sha256.as_deref() != Some(source_hash(source).as_str()) {
            return Err(format!(
                "{} has inconsistent diagnostic source bytes; check it again",
                path.display()
            ));
        }
        let entry = grouped
            .entry(path)
            .or_insert_with(|| (source.clone(), Vec::new()));
        if entry.0 != *source {
            return Err("diagnostics refer to different source snapshots; check again".into());
        }
        entry.1.push(DiagnosticInfo::new(source, fixes.clone()));
    }
    let mut planned = Vec::new();
    for (path, (source, diagnostics)) in grouped {
        let edits = machine_applicable_edits(&source, &diagnostics)
            .map_err(|error| format!("cannot fix {}: {error}", path.display()))?;
        let updated = apply_edits(&source, &edits)
            .map_err(|error| format!("cannot fix {}: {error}", path.display()))?;
        if source != updated {
            planned.push((path, source.into_bytes(), updated));
        }
    }
    if planned.is_empty() {
        return Ok(0);
    }
    // Checking all observed inputs also catches a changed import, manifest or
    // compile-time file that invalidates an otherwise unchanged source edit.
    for consumed in inputs {
        let current = options
            .documents
            .read_bytes(&consumed.path)
            .ok()
            .map(|bytes| source_hash_bytes(&bytes));
        if current != consumed.sha256 {
            return Err(format!(
                "{} changed during checking; no files were written; check again",
                consumed.path.display()
            ));
        }
    }
    let changes = planned
        .iter()
        .map(|(path, original, updated)| (path, original.clone(), updated.clone()))
        .collect::<Vec<_>>();
    let stale = crate::stale_source_inputs(&changes);
    if !stale.is_empty() {
        return Err(format!(
            "source changed during checking: {}; no files were written; check again",
            stale.join(", ")
        ));
    }
    if dry_run {
        for (path, _, _) in &planned {
            eprintln!("Would fix {}", path.display());
        }
    } else {
        crate::write_source_changes(&changes, "Fixed")
            .map_err(|()| "could not write all fixes; see the affected files above".to_string())?;
    }
    Ok(planned.len())
}

fn selected_sources(input: &Path, options: &FrontendOptions) -> HashSet<PathBuf> {
    let paths = match hew_compile::module_membership(input, options) {
        Some(membership) => match membership.anchor {
            ModuleAnchor::Std { .. } => Vec::new(),
            ModuleAnchor::Package { dir, .. } => options
                .documents
                .inputs()
                .into_iter()
                .map(|input| input.path)
                .filter(|path| hew_pkg::generate::is_package_path(&dir, path))
                .collect(),
            ModuleAnchor::Loose => std::iter::once(membership.entry)
                .chain(membership.peers)
                .collect(),
        },
        None => vec![input.to_path_buf()],
    };
    paths
        .into_iter()
        .filter(|path| path.extension().is_some_and(|extension| extension == "hew"))
        .filter_map(|path| path.canonicalize().ok())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn diagnosed(path: &Path, source: &str, options: &FrontendOptions) -> FrontendDiagnostic {
        options.documents.read_bytes(path).unwrap();
        let parsed = hew_parser::parse(source);
        let output =
            hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]))
                .check_program(&parsed.program);
        let error = output
            .warnings
            .into_iter()
            .find(|error| matches!(error.kind, hew_types::error::TypeErrorKind::UnusedMut))
            .unwrap();
        FrontendDiagnostic {
            source: Some(source.into()),
            filename: Some(path.canonicalize().unwrap().display().to_string()),
            note_sources: vec![],
            kind: FrontendDiagnosticKind::Type(error),
        }
    }

    #[test]
    fn diagnostic_apply_refuses_changed_source_and_context_before_any_write() {
        for change_source in [true, false] {
            let dir = tempfile::tempdir().unwrap();
            let path = dir.path().join("main.hew");
            let context = dir.path().join("input.bin");
            let source = "fn main() { var value = 3; println(value); }";
            std::fs::write(&path, source).unwrap();
            std::fs::write(&context, [0, 255]).unwrap();
            let options = FrontendOptions::default();
            let diagnostic = diagnosed(&path, source, &options);
            options.documents.read_bytes(&context).unwrap();
            if change_source {
                std::fs::write(&path, format!("// new snapshot\n{source}")).unwrap();
            } else {
                std::fs::write(&context, [1, 255]).unwrap();
            }
            let before = std::fs::read(&path).unwrap();
            assert!(apply(&path, &options, &[diagnostic], false).is_err());
            assert_eq!(std::fs::read(&path).unwrap(), before);
        }
    }

    #[test]
    fn diagnostic_apply_dry_run_is_byte_identical_then_apply_uses_same_edit() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("main.hew");
        let source = "// é😀\nfn main() { var value = 3; println(value); }";
        std::fs::write(&path, source).unwrap();
        let options = FrontendOptions::default();
        let diagnostic = diagnosed(&path, source, &options);
        assert_eq!(
            apply(&path, &options, std::slice::from_ref(&diagnostic), true).unwrap(),
            1
        );
        assert_eq!(std::fs::read(&path).unwrap(), source.as_bytes());
        assert_eq!(apply(&path, &options, &[diagnostic], false).unwrap(), 1);
        assert_eq!(
            std::fs::read_to_string(&path).unwrap(),
            source.replacen("var value", "let value", 1)
        );
    }
}
