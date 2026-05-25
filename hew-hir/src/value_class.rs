use std::collections::HashMap;

use hew_parser::ast::ResourceMarker as AstResourceMarker;
use hew_types::ResolvedTy;

/// HIR-owned type classification marker.
///
/// Parser-level markers only represent user-written ownership attributes
/// (`#[resource]` / `#[linear]`). HIR also needs substrate registrations for
/// compiler-known value types that are not user-authored attributes, such as
/// `BitCopy` crash-hook payload records.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum ResourceMarker {
    #[default]
    None,
    BitCopy,
    Resource,
    Linear,
}

impl From<hew_parser::ast::ResourceMarker> for ResourceMarker {
    fn from(marker: hew_parser::ast::ResourceMarker) -> Self {
        match marker {
            AstResourceMarker::None => Self::None,
            AstResourceMarker::Resource => Self::Resource,
            AstResourceMarker::Linear => Self::Linear,
        }
    }
}

impl ResourceMarker {
    #[must_use]
    pub fn to_ast_marker(self) -> Option<AstResourceMarker> {
        match self {
            Self::None => Some(AstResourceMarker::None),
            Self::Resource => Some(AstResourceMarker::Resource),
            Self::Linear => Some(AstResourceMarker::Linear),
            Self::BitCopy => None,
        }
    }
}

/// Per-named-type classification table consumed by `ValueClass::of_ty`.
///
/// Construction-site authority: the table is populated by HIR lowering
/// from every `Item::TypeDecl`'s `#[resource]` / `#[linear]` marker and
/// compiler-known substrate registrations. Parser-level storage is retained
/// for compatibility with existing HIR/MIR construction sites; callers must use
/// `lookup_type_marker` so `BitCopy` registrations that have no parser spelling
/// are still observed. LESSONS: `type-info-survival`.
pub type TypeClassTable = HashMap<String, (ResourceMarker, Option<String>)>;

#[must_use]
pub fn lookup_type_marker(name: &str, type_classes: &TypeClassTable) -> Option<ResourceMarker> {
    crate::builtin_type_classes::builtin_type_registration(name)
        .map(|registration| registration.marker)
        .or_else(|| type_classes.get(name).map(|(marker, _)| *marker))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueClass {
    BitCopy,
    CowValue,
    PersistentShare,
    /// `@resource` types — external-resource values with an implicit drop side
    /// effect (`close(consuming self)`). Drop elaboration emits an explicit
    /// `ElabMir::Drop { drop_fn: Some(close) }` on every reachable exit.
    AffineResource,
    /// `@linear` types — single-owner values with **no implicit drop**.
    /// The move-checker rejects any function where a `Linear` binding is
    /// live at an exit without being consumed via a declared consuming
    /// method (`MirCheck::MustConsume`).
    Linear,
    View,
    Unknown,
}

impl ValueClass {
    /// Resolve a type's value-class.
    ///
    /// For `ResolvedTy::Named { name, .. }`, looks up the marker in the
    /// supplied `TypeClassTable`:
    /// - `Some((BitCopy, _))` → `Self::BitCopy`
    /// - `Some((Resource, _))` → `Self::AffineResource`
    /// - `Some((Linear, _))` → `Self::Linear`
    /// - `Some((None, _))` or absent → `Self::Unknown` (preserved fallback;
    ///   the unmarked Named-type behaviour the slice still routes through
    ///   `Strategy::UnknownBlocked` at MIR boundary).
    ///
    /// Builtin types are independent of the table.
    #[must_use]
    pub fn of_ty(ty: &ResolvedTy, type_classes: &TypeClassTable) -> Self {
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
            | ResolvedTy::Isize
            | ResolvedTy::Usize
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
            ResolvedTy::Named { name, .. } => match lookup_type_marker(name, type_classes) {
                Some(ResourceMarker::BitCopy) => Self::BitCopy,
                Some(ResourceMarker::Resource) => Self::AffineResource,
                Some(ResourceMarker::Linear) => Self::Linear,
                Some(ResourceMarker::None) | None => {
                    if matches!(name.as_str(), "Vec" | "HashMap" | "HashSet") {
                        Self::CowValue
                    } else {
                        Self::Unknown
                    }
                }
            },
            // Task handles are consume-once: MirCheck::MustConsume fires if a
            // ForkTaskHandle binding is live at an exit without being consumed
            // via AwaitTask or the implicit block-end join. Linear is the
            // correct class — it threads through C2's existing UseAfterConsume /
            // MustConsume machinery without new checks. The inner type T's own
            // class is checked independently when the task is awaited and T is
            // produced.
            ResolvedTy::Task(_) => Self::Linear,
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
        ResolvedTy::Named { name, args, .. } => {
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
                for arg in &bound.args {
                    collect_named_type_names(arg, names);
                }
                for (_, ty) in &bound.assoc_bindings {
                    collect_named_type_names(ty, names);
                }
            }
        }
        // Task<T> is compiler-internal; recurse into T so that a
        // `Task<SomeResource>` binding is still diagnosed correctly if T
        // is a named type with a resource/linear marker.
        ResolvedTy::Task(inner) => collect_named_type_names(inner, names),
        ResolvedTy::I8
        | ResolvedTy::I16
        | ResolvedTy::I32
        | ResolvedTy::I64
        | ResolvedTy::U8
        | ResolvedTy::U16
        | ResolvedTy::U32
        | ResolvedTy::U64
        | ResolvedTy::Isize
        | ResolvedTy::Usize
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

#[cfg(test)]
mod tests {
    use super::named_type_names;
    use hew_types::{ResolvedTraitBound, ResolvedTy};

    #[test]
    fn trait_object_names_are_not_reported_as_unknown_named_types() {
        let ty = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "Display".to_string(),
                args: Vec::new(),
                assoc_bindings: Vec::new(),
            }],
        };

        assert!(named_type_names(&ty).is_empty());
    }

    #[test]
    fn trait_object_type_arguments_still_report_nested_named_types() {
        let ty = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "Iterator".to_string(),
                args: vec![ResolvedTy::Named {
                    name: "Foo".to_string(),
                    args: Vec::new(),
                    builtin: None,
                }],
                assoc_bindings: Vec::new(),
            }],
        };

        assert_eq!(named_type_names(&ty), vec!["Foo".to_string()]);
    }

    #[test]
    fn trait_object_nested_arguments_recurse_without_reporting_trait_names() {
        let ty = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "OuterTrait".to_string(),
                args: vec![ResolvedTy::Tuple(vec![
                    ResolvedTy::Named {
                        name: "Foo".to_string(),
                        args: Vec::new(),
                        builtin: None,
                    },
                    ResolvedTy::TraitObject {
                        traits: vec![ResolvedTraitBound {
                            trait_name: "InnerTrait".to_string(),
                            args: vec![ResolvedTy::Named {
                                name: "Bar".to_string(),
                                args: Vec::new(),
                                builtin: None,
                            }],
                            assoc_bindings: Vec::new(),
                        }],
                    },
                ])],
                assoc_bindings: Vec::new(),
            }],
        };

        assert_eq!(
            named_type_names(&ty),
            vec!["Foo".to_string(), "Bar".to_string()]
        );
    }
}
