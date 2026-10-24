//! Explicit source selection for the experimental C host boundary.

use crate::{Session, SessionError, SessionOutput};
use hew_parser::ast::Ident;

impl Session {
    /// Compile selected public monomorphic functions and their dependencies.
    ///
    /// This selects a source declaration, not a linker symbol. The native host
    /// boundary subsequently checks the physical signature and supported calls.
    ///
    /// # Errors
    /// Returns source-selection, HIR or semantic diagnostics. A library input
    /// must not also declare a process entry.
    pub fn lower_host_program(
        &self,
        program: &hew_parser::ast::Program,
        tco: &hew_types::TypeCheckOutput,
        names: &[&str],
    ) -> Result<(SessionOutput, Vec<hew_types::DefId>), SessionError> {
        let selection_error = SessionError::Selection;
        let declarations = names
            .iter()
            .map(|name| {
                let (ordinal, span) = program
                    .items
                    .iter()
                    .enumerate()
                    .find_map(|(ordinal, (item, span))| match item {
                        hew_parser::ast::Item::Function(function)
                            if function.name == Ident::new(name)
                                && function.visibility.is_pub()
                                && function.type_params.as_ref().is_none_or(Vec::is_empty) =>
                        {
                            Some((ordinal, span))
                        }
                        _ => None,
                    })
                    .ok_or_else(|| {
                        selection_error(format!(
                    "C export `{name}` must select a public monomorphic root-source function"
                ))
                    })?;
                let occurrence = hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                    tco.defs.root_module(),
                    span,
                    ordinal,
                    hew_types::DeclarationKind::Function,
                    0,
                );
                tco.defs
                    .declaration(occurrence)
                    .ok_or_else(|| SessionError::Lowering {
                        callable: None,
                        error: hew_sir::SirLoweringError::Internal(format!(
                            "C export `{name}` has no checked declaration identity"
                        )),
                        span: None,
                    })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let lowered =
            hew_hir::lower_program(program, tco, &hew_hir::ResolutionCtx, self.target.hir_arch);
        if !lowered.diagnostics.is_empty() {
            return Err(SessionError::Hir(lowered.diagnostics));
        }
        if lowered.module.entry_exit_plan.is_some() {
            return Err(selection_error(
                "C export input must be a library without a process entry".into(),
            ));
        }
        let output = self.lower_hir_module(&lowered.module, tco, &declarations)?;
        Ok((output, declarations))
    }
}
