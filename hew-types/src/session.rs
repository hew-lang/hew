//! Checked type identities retained by an interactive compilation session.

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::{DefTable, ResolvedTy};

/// Types of compiler-generated parameter annotations in the current root source.
/// Earlier declaration rows retain their identities while the new source mints
/// fresh rows. This is supplied by the compiler, never inferred from type names.
#[derive(Debug, Clone)]
pub struct SessionTypeSeed {
    pub definitions: Arc<DefTable>,
    pub annotations: BTreeMap<(usize, usize), ResolvedTy>,
}
