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

/// Actual retained contents of one stable typed session place.
/// Partial paths name initialized canonical leaves by declaration-order fields.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum SessionContents {
    Empty,
    Full,
    Partial(Vec<Vec<u32>>),
}
impl SessionContents {
    /// Normalize a stopped session's complete canonical leaf snapshot.
    #[must_use]
    pub fn from_leaves(leaves: &[(Vec<u32>, bool)]) -> Self {
        let held: Vec<_> = leaves
            .iter()
            .filter(|(_, initialized)| *initialized)
            .map(|(path, _)| path.clone())
            .collect();
        if held.is_empty() {
            Self::Empty
        } else if held.len() == leaves.len() {
            Self::Full
        } else {
            Self::Partial(held)
        }
    }

    #[must_use]
    pub fn contains(&self, path: &[u32]) -> bool {
        match self {
            Self::Empty => false,
            Self::Full => true,
            Self::Partial(paths) => paths.iter().any(|held| held == path),
        }
    }
}
