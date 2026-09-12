//! Fixed-size array operations preserve their exact `[T; N]` receiver identity.

use serde::{Deserialize, Serialize};
use strum::EnumIter;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
pub enum ArrayValueOp {
    #[default]
    Len,
    Index,
    IndexBorrow,
    Set,
}
