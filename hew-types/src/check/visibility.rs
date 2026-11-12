use hew_parser::ast::Visibility;

use super::Checker;

impl Checker {
    pub(super) fn visibility_access_allowed(
        &self,
        declaration: Option<&str>,
        accessor: Option<&str>,
        visibility: Visibility,
    ) -> bool {
        let module = |path: Option<&str>| {
            path.map_or_else(
                || self.defs.root_module(),
                |path| self.defs.module_for_path(path),
            )
        };
        self.defs
            .visibility_access_allowed(module(declaration), module(accessor), visibility)
    }
}
