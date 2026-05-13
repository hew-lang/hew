use hew_types::ResolvedTy;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueClass {
    BitCopy,
    CowValue,
    PersistentShare,
    AffineResource,
    View,
    Unknown,
}

impl ValueClass {
    #[must_use]
    pub fn of_ty(ty: &ResolvedTy) -> Self {
        match ty {
            ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::F32
            | ResolvedTy::F64
            | ResolvedTy::Duration
            | ResolvedTy::Unit
            | ResolvedTy::Never => Self::BitCopy,
            ResolvedTy::String
            | ResolvedTy::Bytes
            | ResolvedTy::Array(_, _)
            | ResolvedTy::Tuple(_) => Self::CowValue,
            ResolvedTy::Slice(_) | ResolvedTy::Pointer { .. } => Self::View,
            ResolvedTy::Function { .. }
            | ResolvedTy::Closure { .. }
            | ResolvedTy::TraitObject { .. } => Self::PersistentShare,
            ResolvedTy::Named { .. } => Self::Unknown,
        }
    }
}

#[must_use]
pub fn contains_named_type(ty: &ResolvedTy) -> bool {
    !named_type_names(ty).is_empty()
}

#[must_use]
pub fn named_type_names(ty: &ResolvedTy) -> Vec<String> {
    let mut names = Vec::new();
    collect_named_type_names(ty, &mut names);
    names
}

fn collect_named_type_names(ty: &ResolvedTy, names: &mut Vec<String>) {
    match ty {
        ResolvedTy::Tuple(elems) => {
            for elem in elems {
                collect_named_type_names(elem, names);
            }
        }
        ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => {
            collect_named_type_names(elem, names);
        }
        ResolvedTy::Named { name, args } => {
            names.push(name.clone());
            for arg in args {
                collect_named_type_names(arg, names);
            }
        }
        ResolvedTy::Function { params, ret } => {
            for param in params {
                collect_named_type_names(param, names);
            }
            collect_named_type_names(ret, names);
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => {
            for param in params {
                collect_named_type_names(param, names);
            }
            collect_named_type_names(ret, names);
            for capture in captures {
                collect_named_type_names(capture, names);
            }
        }
        ResolvedTy::Pointer { pointee, .. } => collect_named_type_names(pointee, names),
        ResolvedTy::TraitObject { traits } => {
            for bound in traits {
                names.push(bound.trait_name.clone());
                for arg in &bound.args {
                    collect_named_type_names(arg, names);
                }
            }
        }
        ResolvedTy::I8
        | ResolvedTy::I16
        | ResolvedTy::I32
        | ResolvedTy::I64
        | ResolvedTy::U8
        | ResolvedTy::U16
        | ResolvedTy::U32
        | ResolvedTy::U64
        | ResolvedTy::F32
        | ResolvedTy::F64
        | ResolvedTy::Bool
        | ResolvedTy::Char
        | ResolvedTy::String
        | ResolvedTy::Bytes
        | ResolvedTy::Duration
        | ResolvedTy::Unit
        | ResolvedTy::Never => {}
    }
}
