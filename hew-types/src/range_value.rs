//! Storage contract for an integer range and its selected finite sequence.

use crate::{BuiltinType, ResolvedTy, TypeHead};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RangeAdapter {
    Reverse,
    Step,
}

/// Source bounds remain unchanged by adapters. The remaining fields describe
/// the selected sequence, so reversal and repeated stepping preserve order.
#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum RangeField {
    Start,
    End,
    First,
    Last,
    Stride,
    Descending,
    Nonempty,
}

#[must_use]
pub fn fields(ty: &ResolvedTy) -> Option<Vec<(String, ResolvedTy)>> {
    let ResolvedTy::Named {
        head: TypeHead::Builtin(BuiltinType::Range),
        args,
        ..
    } = ty
    else {
        return None;
    };
    let [element] = args.as_slice() else {
        return None;
    };
    Some(
        [
            ("start", element.clone()),
            ("end", element.clone()),
            ("first", element.clone()),
            ("last", element.clone()),
            ("stride", ResolvedTy::U64),
            ("descending", ResolvedTy::Bool),
            ("nonempty", ResolvedTy::Bool),
        ]
        .into_iter()
        .map(|(name, ty)| (name.to_string(), ty))
        .collect(),
    )
}
