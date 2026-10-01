//! Serialization data shape: the one admission authority for `Serializable`
//! and the per-declaration text keys, tags and presence flags every codec
//! reads (design-data-codegen §1.3).
//!
//! A value is data unless it is a resource, a handle or code. Records and
//! enums are data when their members are; generic declarations are data per
//! instantiation; recursion is admitted. `#[wire]` adds stable tags on the
//! same shape, and remote payloads require them.

use std::collections::HashSet;

use crate::traits::MarkerTrait;
use crate::{BuiltinType, ResolvedTy, TypeFactService};

/// Text keys, tags and flags of one record's fields or one enum's variants,
/// in declaration order. Indices agree with the record's field order and the
/// enum's variant order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SerialLayout {
    /// `#[wire]`: binary formats key members by tag.
    pub tagged: bool,
    /// A positional (tuple-form) record: encoded as a sequence, no keys.
    pub positional: bool,
    pub members: Vec<SerialMember>,
}

/// One field or variant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SerialMember {
    /// The text key after `#[serial(case)]` or `#[serial(key)]`.
    pub key: String,
    /// The `@N` tag of a `#[wire]` member; 0 otherwise.
    pub tag: u64,
    /// `hew_codec::Member` flag bits.
    pub flags: u32,
    /// A struct variant's fields, in declaration order.
    pub fields: Vec<SerialMember>,
}

/// Why a type is not data, with the member path that reached it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NotData {
    /// `.field`, `[i]` and `<T>` segments from the checked type.
    pub path: String,
    /// The type that is not data.
    pub ty: ResolvedTy,
    pub reason: NotDataReason,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NotDataReason {
    /// A `#[resource]`, `#[linear]` or `#[opaque]` declaration.
    Marked,
    /// `Option<Option<T>>` (or `Option<T>` of a type parameter): null cannot
    /// hold three states.
    NestedOption,
    /// A map key or set element without the `Hash` and `Eq` decoding needs.
    Key,
    /// A type parameter without a `Serializable` bound.
    Unbounded,
    /// A remote payload's record or enum without `#[wire]` tags.
    Untagged,
    /// A handle, closure, task, stream or other non-data type.
    Other,
}

impl NotData {
    #[must_use]
    pub fn describe(&self) -> &'static str {
        match self.reason {
            NotDataReason::Marked => "a resource, linear or opaque type",
            NotDataReason::NestedOption => "a nested `Option`, which null cannot represent",
            NotDataReason::Key => "a map key or set element without `Hash` and `Eq`",
            NotDataReason::Unbounded => "a type parameter without `T: Serializable`",
            NotDataReason::Untagged => "a record or enum without `#[wire]` tags",
            NotDataReason::Other => "not data",
        }
    }
}

impl TypeFactService {
    /// The one `Serializable` admission check. `tagged` adds the remote-payload
    /// rule: every record or enum reached must be `#[wire]`. `param` answers a
    /// bound on a type parameter of the code being checked.
    pub(crate) fn data_error(
        &self,
        ty: &ResolvedTy,
        tagged: bool,
        param: &dyn Fn(crate::ParamHead, MarkerTrait) -> bool,
    ) -> Option<NotData> {
        self.data_error_within(ty, tagged, param, &mut HashSet::new())
    }
}

impl TypeFactService {
    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive match keeps every admitted and refused shape together"
    )]
    fn data_error_within(
        &self,
        ty: &ResolvedTy,
        tagged: bool,
        param: &dyn Fn(crate::ParamHead, MarkerTrait) -> bool,
        visiting: &mut HashSet<ResolvedTy>,
    ) -> Option<NotData> {
        let refuse = |reason| {
            Some(NotData {
                path: String::new(),
                ty: ty.clone(),
                reason,
            })
        };
        let member = |segment: String, member: &ResolvedTy, visiting: &mut HashSet<ResolvedTy>| {
            self.data_error_within(member, tagged, param, visiting)
                .map(|mut error| {
                    error.path.insert_str(0, &segment);
                    error
                })
        };
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
            | ResolvedTy::Bytes
            | ResolvedTy::Unit => None,
            ResolvedTy::Tuple(elements) => elements
                .iter()
                .enumerate()
                .find_map(|(index, element)| member(format!("[{index}]"), element, visiting)),
            ResolvedTy::Array(element, _) => member("[]".to_string(), element, visiting),
            ResolvedTy::TypeParam { name } => {
                if param(*name, MarkerTrait::Serializable) {
                    None
                } else {
                    refuse(NotDataReason::Unbounded)
                }
            }
            ResolvedTy::Named {
                head: crate::TypeHead::Builtin(builtin),
                args,
                ..
            } => match (builtin, args.as_slice()) {
                (BuiltinType::Vec, [element]) => member("[]".to_string(), element, visiting),
                (BuiltinType::HashSet, [element]) => {
                    if !self.is_codec_key(element, param) {
                        return Some(NotData {
                            path: "[]".to_string(),
                            ty: element.clone(),
                            reason: NotDataReason::Key,
                        });
                    }
                    member("[]".to_string(), element, visiting)
                }
                (BuiltinType::HashMap, [key, value]) => {
                    if !self.is_codec_key(key, param) {
                        return Some(NotData {
                            path: "<key>".to_string(),
                            ty: key.clone(),
                            reason: NotDataReason::Key,
                        });
                    }
                    member("<key>".to_string(), key, visiting)
                        .or_else(|| member("[]".to_string(), value, visiting))
                }
                (BuiltinType::Option, [value]) => {
                    if value.is_builtin(BuiltinType::Option)
                        || matches!(value, ResolvedTy::TypeParam { .. })
                    {
                        return refuse(NotDataReason::NestedOption);
                    }
                    member(String::new(), value, visiting)
                }
                (BuiltinType::Result, [ok, error]) => member(".Ok".to_string(), ok, visiting)
                    .or_else(|| member(".Err".to_string(), error, visiting)),
                _ => refuse(NotDataReason::Other),
            },
            ResolvedTy::Named {
                head:
                    head @ (crate::TypeHead::Nominal(_)
                    | crate::TypeHead::Param(_)
                    | crate::TypeHead::Unresolved(_)),
                is_opaque: false,
                ..
            } => {
                let Some(declaration) = head.declaration(self.context().defs()) else {
                    return refuse(NotDataReason::Other);
                };
                let Some(layout) = self.context().serial_layout(declaration) else {
                    return refuse(
                        if self
                            .context()
                            .declarations()
                            .get(&declaration)
                            .is_some_and(|declared| {
                                declared.marker != crate::DeclarationMarker::None
                                    || declared.is_opaque
                            })
                        {
                            NotDataReason::Marked
                        } else {
                            NotDataReason::Other
                        },
                    );
                };
                if tagged && !layout.tagged && !layout.positional {
                    return refuse(NotDataReason::Untagged);
                }
                if !visiting.insert(ty.clone()) {
                    return None;
                }
                if let Ok((_, fields)) = self.record_fields(ty) {
                    return fields
                        .iter()
                        .find_map(|(name, field)| member(format!(".{name}"), field, visiting));
                }
                self.declared_capability_members(ty).map_or_else(
                    |_| refuse(NotDataReason::Other),
                    |members| {
                        members
                            .iter()
                            .find_map(|payload| member(String::new(), payload, visiting))
                    },
                )
            }
            _ => refuse(NotDataReason::Other),
        }
    }
}
