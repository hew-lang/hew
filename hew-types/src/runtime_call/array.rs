//! Fixed-size array operations preserve their exact `[T; N]` receiver identity.

use serde::{Deserialize, Serialize};
use strum::EnumIter;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
#[cfg_attr(feature = "sandbox-schema", derive(schemars::JsonSchema, ts_rs::TS))]
pub enum ArrayValueOp {
    #[default]
    Len,
    Index,
    IndexBorrow,
    Set,
}
