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
        match visibility {
            Visibility::Pub => true,
            Visibility::Private => match (module(declaration), module(accessor)) {
                (Some(owner), Some(here)) => owner == here,
                _ => false,
            },
            Visibility::Package => match (module(declaration), module(accessor)) {
                (Some(owner), Some(here)) => self.defs.same_package(owner, here),
                _ => false,
            },
        }
    }
}
