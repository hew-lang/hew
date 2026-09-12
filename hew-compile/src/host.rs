//! Explicit source selection for the experimental synchronous C host boundary.

use crate::{Session, SessionError, SessionOutput};

impl Session {
    /// Compile exactly one public monomorphic function and its dependencies.
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
        name: &str,
    ) -> Result<SessionOutput, SessionError> {
        let unsupported = |message: String| SessionError::Unsupported {
            callable: None,
            message,
        };
        let (ordinal, span) = program
            .items
            .iter()
            .enumerate()
            .find_map(|(ordinal, (item, span))| match item {
                hew_parser::ast::Item::Function(function)
                    if function.name == name
                        && function.visibility.is_pub()
                        && function.type_params.as_ref().is_none_or(Vec::is_empty) =>
                {
                    Some((ordinal, span))
                }
                _ => None,
            })
            .ok_or_else(|| {
                unsupported(format!(
                    "C export `{name}` must select a public monomorphic root-source function"
                ))
            })?;
        let occurrence = hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
            tco.identity.root_module(),
            span,
            ordinal,
            hew_types::DeclarationKind::Function,
            0,
        );
        let declaration = tco.identity.declaration(occurrence).ok_or_else(|| {
            unsupported(format!(
                "C export `{name}` has no checked declaration identity"
            ))
        })?;
        let lowered =
            hew_hir::lower_program(program, tco, &hew_hir::ResolutionCtx, self.target.hir_arch);
        if !lowered.diagnostics.is_empty() {
            return Err(SessionError::Hir(lowered.diagnostics));
        }
        if lowered.module.entry_exit_plan.is_some() {
            return Err(unsupported(
                "C export input must be a library without a process entry".into(),
            ));
        }
        self.lower_hir_module(&lowered.module, tco, std::slice::from_ref(declaration))
    }
}
