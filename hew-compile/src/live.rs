//! Compiler-owned binding storage for a prepared interactive submission.

pub mod frontend;

use hew_hir::{HirItem, HirSessionBinding, HirSessionEntry, HirStmtKind};
use hew_types::{DefId, TypeCheckOutput};

use crate::{Session, SessionError, SessionOutput};

/// A verified submission and the declarations whose storage it addresses.
#[derive(Debug)]
pub struct LiveSubmission {
    pub output: SessionOutput,
    pub bindings: Vec<HirSessionBinding>,
    pub next_slot: u32,
}

impl Session {
    /// Lower a checked submission without executing or transferring its inputs.
    ///
    /// The selected function's parameters describe the retained bindings, in
    /// `input_slots` order. Its direct body declarations receive fresh slots;
    /// nested lexical bindings keep ordinary local cleanup. The source entry
    /// must return unit; observations use ordinary output or retained values.
    ///
    /// # Errors
    /// Returns ordinary HIR/SIR diagnostics, or an invalid session selection.
    pub fn lower_live_program(
        &self,
        program: &hew_parser::ast::Program,
        checked: &TypeCheckOutput,
        entry: DefId,
        input_slots: &[u32],
        next_slot: u32,
    ) -> Result<LiveSubmission, SessionError> {
        let inputs: Vec<_> = input_slots
            .iter()
            .map(|slot| (*slot, hew_types::session::SessionContents::Full))
            .collect();
        self.lower_live_program_with_contents(program, checked, entry, &inputs, next_slot)
    }

    /// Lower against the actual retained leaf contents from the preceding execution.
    /// The caller must execute only while that session snapshot remains current.
    ///
    /// # Errors
    /// Returns ordinary checked lowering diagnostics or invalid input identities.
    pub fn lower_live_program_with_contents(
        &self,
        program: &hew_parser::ast::Program,
        checked: &TypeCheckOutput,
        entry: DefId,
        inputs: &[(u32, hew_types::session::SessionContents)],
        mut next_slot: u32,
    ) -> Result<LiveSubmission, SessionError> {
        let input_slots: Vec<_> = inputs.iter().map(|(slot, _)| *slot).collect();
        let program = checked
            .normalized_program
            .as_ref()
            .map_or(program, |value| &value.program);
        let mut lowered = hew_hir::lower_program(
            program,
            checked,
            &hew_hir::ResolutionCtx,
            self.target.hir_arch,
        );
        if !lowered.diagnostics.is_empty() {
            return Err(SessionError::Hir(lowered.diagnostics));
        }
        if lowered.module.entry_exit_plan.is_some() {
            return Err(SessionError::Selection(
                "a live submission cannot declare a process entry".into(),
            ));
        }
        let function = lowered
            .module
            .items
            .iter()
            .find_map(|item| match item {
                HirItem::Function(function) if function.declaration == entry => Some(function),
                _ => None,
            })
            .ok_or_else(|| SessionError::Selection("live entry has no checked body".into()))?;
        if function.return_ty != hew_types::ResolvedTy::Unit
            || !function.type_params.is_empty()
            || function.is_generator
            || function.params.len() != input_slots.len()
            || input_slots.iter().any(|slot| *slot >= next_slot)
            || input_slots
                .iter()
                .collect::<std::collections::BTreeSet<_>>()
                .len()
                != input_slots.len()
        {
            return Err(SessionError::Selection(
                "live entry requires a monomorphic unit body and distinct retained slots".into(),
            ));
        }
        let mut bindings: Vec<_> = function
            .params
            .iter()
            .zip(inputs)
            .map(|(binding, (slot, contents))| HirSessionBinding {
                binding: binding.clone(),
                slot: *slot,
                retained: true,
                contents: contents.clone(),
            })
            .collect();
        for statement in &function.body.statements {
            let locals: Vec<_> = match &statement.kind {
                HirStmtKind::Let(binding, _) => vec![binding],
                HirStmtKind::Destructure { fields, .. } => fields
                    .iter()
                    .filter(|field| !field.nested)
                    .filter_map(|field| field.binding.as_ref())
                    .collect(),
                _ => Vec::new(),
            };
            for binding in locals {
                let slot = next_slot;
                next_slot = next_slot.checked_add(1).ok_or_else(|| {
                    SessionError::Selection("live binding slot capacity exhausted".into())
                })?;
                bindings.push(HirSessionBinding {
                    binding: binding.clone(),
                    slot,
                    retained: false,
                    contents: hew_types::session::SessionContents::Empty,
                });
            }
        }
        lowered.module.session_entry = Some(HirSessionEntry {
            declaration: entry,
            bindings: bindings.clone(),
        });
        let output = self.lower_hir_module(&lowered.module, checked, &[entry])?;
        Ok(LiveSubmission {
            output,
            bindings,
            next_slot,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn live_retained_type_does_not_follow_a_shadowed_type_name() {
        let directory = tempfile::tempdir().unwrap();
        let mut options = crate::FrontendOptions {
            project_dir: Some(directory.path().to_path_buf()),
            repl_fragment: true,
            session_type_seed: Some(std::sync::Arc::new(hew_types::session::SessionTypeSeed {
                definitions: std::sync::Arc::new(hew_types::DefTable::default()),
                annotations: std::collections::BTreeMap::default(),
            })),
            ..Default::default()
        };
        options.documents.insert(
            directory.path().join("old.hew"),
            "pub type Token { value: i64; }",
        );
        let first_source =
            "import old; pub fn submission() { let value = old.Token { value: 7 }; }";
        let first = crate::run_source_frontend(
            first_source,
            directory.path().join("first.hew").to_str().unwrap(),
            &options,
        )
        .into_result()
        .unwrap();
        let checked = first.typecheck_result.tco.unwrap();
        let roots = Session::source_roots(&first.program, &checked).unwrap();
        let session = Session::new(
            crate::SessionTarget::native(),
            crate::DiagnosticPolicy::default(),
        );
        let initial = session
            .lower_live_program(&first.program, &checked, roots[0], &[], 0)
            .unwrap();
        let old_type = initial.bindings[0].binding.ty.clone();
        let next_source = "import old; pub type Token { value: string; } pub fn submission(consume value: _) { let prior: i64 = value.value; let latest = Token { value: \"new\" }; }";
        let annotation = next_source.find('_').unwrap();
        options.session_type_seed =
            Some(std::sync::Arc::new(hew_types::session::SessionTypeSeed {
                definitions: checked.defs.clone(),
                annotations: [((annotation, annotation + 1), old_type.clone())].into(),
            }));
        let next = crate::run_source_frontend(
            next_source,
            directory.path().join("next.hew").to_str().unwrap(),
            &options,
        )
        .into_result()
        .unwrap();
        let next_checked = next.typecheck_result.tco.unwrap();
        assert!(next_checked.defs.extends(&checked.defs));
        let next_roots = Session::source_roots(&next.program, &next_checked).unwrap();
        let next = session
            .lower_live_program(&next.program, &next_checked, next_roots[0], &[0], 1)
            .unwrap();
        assert_eq!(next.bindings[0].binding.ty, old_type);
        assert_ne!(next.bindings[2].binding.ty, old_type);
    }
}
