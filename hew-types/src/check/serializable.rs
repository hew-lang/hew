//! Checker entry points to the one serialization admission check,
//! `TypeFactService::data_error`, and the per-declaration serial layouts it
//! and every codec read. The facade bound, the remote-actor gates and the
//! `#[wire]` declaration check all ask it, so SIR's codec plan never meets a
//! value the checker admitted without a shape.

use std::collections::{BTreeMap, HashSet};

use hew_codec::Member;
use hew_parser::ast::{SerialField, TypeBodyItem, TypeDecl, VariantKind};

use super::{Checker, TypeErrorKind};
use crate::data_shape::{NotData, SerialLayout, SerialMember};
use crate::traits::MarkerTrait;
use crate::{ResolvedTy, Ty, TypeFactService};

impl Checker {
    fn data_error(&self, ty: &ResolvedTy, tagged: bool) -> Option<NotData> {
        let param = |name: &str, marker: MarkerTrait| match marker {
            MarkerTrait::Serializable => self.type_param_carries_bound(name, "Serializable"),
            marker => self.type_param_has_marker_bound(name, marker),
        };
        TypeFactService::new(self.type_fact_context(), BTreeMap::new())
            .data_error(ty, tagged, &param)
    }

    /// Whether the codecs can encode and decode the concrete `ty`.
    pub(super) fn is_serializable(&self, ty: &ResolvedTy) -> bool {
        self.data_error(ty, false).is_none()
    }

    /// Why `ty` cannot cross to a remote actor: it is not data, or a record
    /// or enum it reaches has no `#[wire]` tags (D524).
    pub(super) fn remote_payload_error(&self, ty: &ResolvedTy) -> Option<NotData> {
        self.data_error(ty, true)
    }

    /// `ty: Serializable` in bound position.
    pub(super) fn satisfies_serializable(&self, ty: &Ty) -> bool {
        let ty = self.subst.resolve(ty).materialize_literal_defaults();
        // An unsettled type is decided when inference settles; an errored
        // one already carries its diagnostic.
        if ty.has_inference_var() || matches!(ty, Ty::Error) {
            return true;
        }
        let ty = self.normalize_for_use(&ty);
        ResolvedTy::from_ty(&ty).is_ok_and(|ty| self.is_serializable(&ty))
    }

    /// Why a checked type failed its `Serializable` bound.
    pub(super) fn not_serializable_explanation(&self, ty: &Ty) -> Option<String> {
        let ty = self.normalize_for_use(&self.subst.resolve(ty).materialize_literal_defaults());
        let concrete = ResolvedTy::from_ty(&ty).ok()?;
        self.data_error(&concrete, false)
            .map(|error| Self::not_data_message(&format!("`{}`", ty.user_facing()), &error))
    }

    /// The `E_NOT_SERIALIZABLE` explanation for `owner` reaching `error`.
    pub(super) fn not_data_message(owner: &str, error: &NotData) -> String {
        format!(
            "E_NOT_SERIALIZABLE: {owner} reaches `{}`{}, {}",
            error.ty.user_facing(),
            if error.path.is_empty() {
                String::new()
            } else {
                format!(" at `{}`", error.path)
            },
            error.describe()
        )
    }

    /// Refuse a `#[wire]` declaration whose members are not data.
    pub(super) fn validate_wire_type_encoding(
        &mut self,
        identity: &str,
        members: Vec<(String, Ty, hew_parser::ast::Span)>,
    ) {
        for (member, ty, span) in members {
            let ty = self.normalize_for_use(&ty);
            let Ok(ty) = ResolvedTy::from_ty(&ty) else {
                continue;
            };
            if let Some(error) = self.data_error(&ty, false) {
                self.report_error(
                    TypeErrorKind::BoundsNotSatisfied,
                    &span,
                    Self::not_data_message(
                        &format!(
                            "{member} of `#[wire]` type `{}`",
                            crate::short_name(identity)
                        ),
                        &error,
                    ),
                );
            }
        }
    }

    /// Publish the text keys, tags and flags of a data record or enum. A
    /// resource, linear or opaque declaration has none and is never data.
    #[expect(
        clippy::too_many_lines,
        reason = "fields and variants share the declaration's case, tags and presence rules"
    )]
    pub(super) fn register_serial_layout(&mut self, td: &TypeDecl) {
        if td.resource_marker != hew_parser::ast::ResourceMarker::None || td.is_opaque {
            return;
        }
        let identity = self.current_module_identity().map_or_else(
            || td.name.to_string(),
            |module| format!("{module}.{}", td.name),
        );
        let Some(declaration) = self
            .lookup_declaration(&identity)
            .map(crate::NominalId::of_declaration)
        else {
            return;
        };
        let Some(type_def) = self.type_def_at(td.name.name.as_str()).cloned() else {
            return;
        };
        let case = td.serial_case;
        let key = |name: &str| serial_key(name, case);
        let is_option = |ty: Option<&Ty>| ty.is_some_and(|ty| ty.as_option().is_some());
        let tagged = td.wire.is_some();
        let mut members = Vec::new();
        let mut field_tags = td.wire.iter().flat_map(|wire| wire.field_meta.iter());
        for item in &td.body {
            match item {
                TypeBodyItem::Field {
                    name,
                    attributes,
                    ty,
                    ..
                } => {
                    let options = SerialField::of(attributes);
                    let field_ty = type_def.fields.get(name.name.as_str());
                    let meta = field_tags.next();
                    let mut flags = match meta {
                        Some(meta) if meta.is_optional => Member::ACCEPT_ABSENT | Member::OMIT_NULL,
                        None if is_option(field_ty) => Member::ACCEPT_ABSENT,
                        _ => 0,
                    };
                    if options.skip {
                        if !is_option(field_ty) {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &ty.1,
                                format!(
                                    "E_SERIAL_SKIP: `#[serial(skip)]` field `{name}` must be an \
                                     `Option`: a skipped field decodes as `None`"
                                ),
                            );
                        }
                        flags |= Member::SKIP | Member::ACCEPT_ABSENT;
                    }
                    members.push(SerialMember {
                        key: options.key.unwrap_or_else(|| key(name.name.as_str())),
                        tag: meta.map_or(0, |meta| u64::from(meta.field_number)),
                        flags,
                        fields: Vec::new(),
                    });
                }
                TypeBodyItem::Variant(variant) => {
                    if !tagged && variant.tag.is_some() {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &variant.span,
                            format!(
                                "variant `{}` has a tag but `{}` is not `#[wire]`: tags are \
                                 the wire schema's stable identities",
                                variant.name, td.name
                            ),
                        );
                    }
                    let fields = match type_def.variants.get(variant.name.name.as_str()) {
                        Some(super::VariantDef::Struct(fields)) => fields
                            .iter()
                            .map(|(field, field_ty)| SerialMember {
                                key: key(field),
                                tag: 0,
                                flags: if is_option(Some(field_ty)) {
                                    Member::ACCEPT_ABSENT
                                } else {
                                    0
                                },
                                fields: Vec::new(),
                            })
                            .collect(),
                        _ => Vec::new(),
                    };
                    members.push(SerialMember {
                        key: key(variant.name.name.as_str()),
                        tag: variant.tag.map_or(0, u64::from),
                        flags: if matches!(variant.kind, VariantKind::Unit) {
                            0
                        } else {
                            Member::PAYLOAD
                        },
                        fields,
                    });
                }
                TypeBodyItem::Method(_) => {}
            }
        }
        self.refuse_key_collisions(td, &members);
        self.serial_layouts.insert(
            declaration,
            SerialLayout {
                tagged,
                positional: false,
                members,
            },
        );
    }

    /// A positional record encodes as a sequence of its fields; a named one
    /// as a record keyed by its field names.
    pub(super) fn register_record_serial_layout(
        &mut self,
        identity: &str,
        positional: bool,
        type_def: &super::TypeDef,
    ) {
        if let Some(declaration) = self
            .lookup_declaration(identity)
            .map(crate::NominalId::of_declaration)
        {
            let members = type_def
                .field_order
                .iter()
                .map(|name| SerialMember {
                    key: name.clone(),
                    tag: 0,
                    flags: if type_def
                        .fields
                        .get(name)
                        .is_some_and(|ty| ty.as_option().is_some())
                    {
                        Member::ACCEPT_ABSENT
                    } else {
                        0
                    },
                    fields: Vec::new(),
                })
                .collect();
            self.serial_layouts.insert(
                declaration,
                SerialLayout {
                    tagged: false,
                    positional,
                    members,
                },
            );
        }
    }

    fn refuse_key_collisions(&mut self, td: &TypeDecl, members: &[SerialMember]) {
        let mut seen = HashSet::new();
        for member in members {
            if !seen.insert(member.key.as_str()) {
                let what = if td.kind == hew_parser::ast::TypeDeclKind::Enum {
                    "variants"
                } else {
                    "fields"
                };
                self.errors.push(crate::error::TypeError {
                    severity: crate::error::Severity::Error,
                    kind: TypeErrorKind::InvalidOperation,
                    span: self
                        .type_def_spans
                        .get(td.name.name.as_str())
                        .cloned()
                        .unwrap_or(0..0),
                    message: format!(
                        "E_SERIAL_KEY_COLLISION: two {what} of `{}` share the text key `{}`",
                        td.name, member.key
                    ),
                    notes: vec![],
                    suggestions: vec![
                        "give one of them its own key with `#[serial(key = \"..\")]`".to_string(),
                    ],
                    source_module: self.current_module.clone(),
                });
            }
        }
    }
}

/// A declared name's text key under the type's `#[serial(case)]`.
fn serial_key(name: &str, case: Option<hew_parser::ast::NamingCase>) -> String {
    use heck::{ToKebabCase, ToLowerCamelCase, ToShoutySnakeCase, ToSnakeCase, ToUpperCamelCase};
    use hew_parser::ast::NamingCase;
    match case {
        None => name.to_owned(),
        Some(NamingCase::CamelCase) => name.to_lower_camel_case(),
        Some(NamingCase::PascalCase) => name.to_upper_camel_case(),
        Some(NamingCase::SnakeCase) => name.to_snake_case(),
        Some(NamingCase::ScreamingSnake) => name.to_shouty_snake_case(),
        Some(NamingCase::KebabCase) => name.to_kebab_case(),
    }
}
