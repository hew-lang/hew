//! Portable property-input shapes supplied by the compiler codec plan.

use serde::{Deserialize, Serialize};

/// The compiler's structural plan, with child indices preserving recursion.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Shape {
    pub root: usize,
    pub nodes: Vec<Node>,
    pub names: Vec<String>,
    pub aggregates: Vec<Option<u32>>,
    pub variants: Vec<Option<u32>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Node {
    Unit,
    Bool,
    Signed { min: i64, max: i64 },
    Unsigned { max: u64 },
    Float,
    Char,
    String,
    Bytes,
    Tuple(Vec<usize>),
    Array { element: usize, len: usize },
    Sequence(usize),
    Set(usize),
    Map { key: usize, value: usize },
    Record(Vec<(Key, usize)>),
    Variant(Vec<(Key, Option<usize>)>),
    Option(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Key {
    pub name: String,
    pub tag: Option<u64>,
    pub flags: u32,
}
