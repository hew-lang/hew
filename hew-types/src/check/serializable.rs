//! The one `Serializable` admission authority.
//!
//! A value is serializable exactly when the wire codec has a plan for it:
//! scalars, `Vec`/`HashSet`/`HashMap`/`Option` over serializable values, and
//! declarations with a checked `#[wire]` layout whose members are themselves
//! serializable. The `T: Serializable` bound, the `std.encoding.wire` facade
//! and the remote-actor payload gates all ask this predicate, so SIR's wire
//! plan never meets a value the checker admitted without a schema.
//!
//! WHY this is narrower than "every data type": plain records, tuples, arrays,
//! `Result` and unit have no codec plan until the structural data shape and
//! event walk land (design-data-codegen §1.3-1.4). WHEN that lands, this
//! predicate becomes `data_shape(ty).is_ok()` and widens in place.

use super::Checker;
use crate::traits::MarkerTrait;
use std::collections::HashMap;

use crate::{BuiltinType, ResolvedTy, Ty};

impl Checker {
    /// Whether the wire codec can encode and decode `ty`.
    pub(super) fn is_serializable(&self, ty: &ResolvedTy) -> bool {
        self.serializable_in(ty, &mut Vec::new())
    }

    /// `ty: Serializable` in bound position. A type parameter declared
    /// `Serializable` stands for any admitted value, so a composite over it
    /// (`Vec<T>`) is checked with the parameter as a serializable leaf.
    pub(super) fn satisfies_serializable(&self, ty: &Ty) -> bool {
        let ty = self.subst.resolve(ty).materialize_literal_defaults();
        // An unsettled type is decided when inference settles; an errored
        // one already carries its diagnostic.
        if ty.has_inference_var() || matches!(ty, Ty::Error) {
            return true;
        }
        let bounded: HashMap<String, Ty> = self
            .current_type_param_names()
            .into_iter()
            .filter(|name| self.type_param_carries_bound(name, "Serializable"))
            .map(|name| (name, Ty::I64))
            .collect();
        let ty = self.normalize_for_use(&ty.substitute_named_params_parallel(&bounded));
        ResolvedTy::from_ty(&ty).is_ok_and(|ty| self.is_serializable(&ty))
    }

    fn serializable_in(&self, ty: &ResolvedTy, visiting: &mut Vec<String>) -> bool {
        match ty {
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
            | ResolvedTy::Duration
            | ResolvedTy::String
            | ResolvedTy::Bytes => true,
            ResolvedTy::Named {
                builtin: Some(builtin),
                args,
                ..
            } => match (builtin, args.as_slice()) {
                (BuiltinType::Vec, [element]) => self.serializable_in(element, visiting),
                (BuiltinType::HashSet, [element]) => {
                    self.is_codec_key(element) && self.serializable_in(element, visiting)
                }
                (BuiltinType::HashMap, [key, value]) => {
                    self.is_codec_key(key)
                        && self.serializable_in(key, visiting)
                        && self.serializable_in(value, visiting)
                }
                // `None` and `Some(None)` share the null encoding.
                (BuiltinType::Option, [value]) => {
                    !value.is_builtin(BuiltinType::Option) && self.serializable_in(value, visiting)
                }
                _ => false,
            },
            ResolvedTy::Named {
                name,
                builtin: None,
                args,
                is_opaque: false,
            } => {
                // The codec plan is finite: a schema that reaches itself has
                // no plan, so a revisit refuses rather than assumes.
                if !args.is_empty()
                    || !self.wire_layouts.contains_key(name)
                    || self.registry.is_resource(name)
                    || self.registry.is_linear(name)
                    || visiting.contains(name)
                {
                    return false;
                }
                let Some(type_def) = self.lookup_type_def(name) else {
                    return false;
                };
                visiting.push(name.clone());
                let members = Self::structural_member_types_for_type(&type_def);
                let ok = members.iter().all(|member| {
                    ResolvedTy::from_ty(&self.normalize_for_use(member))
                        .is_ok_and(|member| self.serializable_in(&member, visiting))
                });
                visiting.pop();
                ok
            }
            _ => false,
        }
    }

    /// Map keys and set elements need the selected `Hash` and `Eq` the codec
    /// uses to rebuild the collection.
    fn is_codec_key(&self, ty: &ResolvedTy) -> bool {
        let ty = ty.to_ty();
        self.collection_key_marker_available(&ty, MarkerTrait::Hash)
            && self.collection_key_marker_available(&ty, MarkerTrait::Eq)
    }
}
