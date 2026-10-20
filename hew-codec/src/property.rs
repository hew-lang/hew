//! Portable typed data shapes supplied by the compiler codec plan.

use serde::{Deserialize, Serialize};

/// The compiler's structural plan, with child indices preserving recursion.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "sandbox-schema", derive(schemars::JsonSchema, ts_rs::TS))]
#[cfg_attr(feature = "sandbox-schema", ts(rename = "CodecShape"))]
pub struct Shape {
    pub root: usize,
    pub nodes: Vec<Node>,
    pub names: Vec<String>,
    pub aggregates: Vec<Option<u32>>,
    pub variants: Vec<Option<u32>>,
    /// Synthetic tuple or record groups expand into separate enum payload fields.
    pub grouped_payloads: Vec<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "sandbox-schema", derive(schemars::JsonSchema, ts_rs::TS))]
#[cfg_attr(feature = "sandbox-schema", ts(rename = "CodecNode"))]
pub enum Node {
    Unit,
    Bool,
    Signed {
        #[cfg_attr(feature = "sandbox-schema", ts(type = "number"))]
        min: i64,
        #[cfg_attr(feature = "sandbox-schema", ts(type = "number"))]
        max: i64,
    },
    Unsigned {
        #[cfg_attr(feature = "sandbox-schema", ts(type = "number"))]
        max: u64,
    },
    Float {
        bits: u8,
    },
    Char,
    String,
    Bytes,
    Tuple(Vec<usize>),
    Array {
        element: usize,
        len: usize,
    },
    Sequence(usize),
    Set(usize),
    Map {
        key: usize,
        value: usize,
    },
    Record(Vec<(Key, usize)>),
    Variant(Vec<(Key, Option<usize>)>),
    Option(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "sandbox-schema", derive(schemars::JsonSchema, ts_rs::TS))]
#[cfg_attr(feature = "sandbox-schema", ts(rename = "CodecKey"))]
pub struct Key {
    pub name: String,
    #[cfg_attr(feature = "sandbox-schema", ts(type = "number | null"))]
    pub tag: Option<u64>,
    pub flags: u32,
}
