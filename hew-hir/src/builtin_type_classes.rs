use hew_parser::ast::ResourceMarker as AstResourceMarker;
use hew_types::ResolvedTy;

use crate::value_class::{ResourceMarker, TypeClassTable};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinTypeRole {
    CrashInfo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinFieldTy {
    I64,
}

impl BuiltinFieldTy {
    #[must_use]
    pub fn to_resolved_ty(self) -> ResolvedTy {
        match self {
            Self::I64 => ResolvedTy::I64,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinTypeField {
    pub name: &'static str,
    pub ty: BuiltinFieldTy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinTypeShape {
    Opaque,
    Struct(&'static [BuiltinTypeField]),
    Enum(&'static [&'static str]),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinTypeRegistration {
    pub name: &'static str,
    pub marker: ResourceMarker,
    pub close_method: Option<&'static str>,
    pub shape: BuiltinTypeShape,
    pub role: Option<BuiltinTypeRole>,
}

const PANIC_INFO_FIELDS: &[BuiltinTypeField] = &[BuiltinTypeField {
    name: "code",
    ty: BuiltinFieldTy::I64,
}];

const CRASH_ACTION_VARIANTS: &[&str] = &["Restart", "Escalate", "Kill"];

const BUILTIN_TYPE_REGISTRATIONS: &[BuiltinTypeRegistration] = &[
    BuiltinTypeRegistration {
        name: "Duplex",
        marker: ResourceMarker::Resource,
        close_method: Some("close"),
        shape: BuiltinTypeShape::Opaque,
        role: None,
    },
    BuiltinTypeRegistration {
        name: "Sink",
        marker: ResourceMarker::Resource,
        close_method: Some("close"),
        shape: BuiltinTypeShape::Opaque,
        role: None,
    },
    BuiltinTypeRegistration {
        name: "Stream",
        marker: ResourceMarker::Resource,
        close_method: Some("close"),
        shape: BuiltinTypeShape::Opaque,
        role: None,
    },
    BuiltinTypeRegistration {
        name: "LambdaActorHandle",
        marker: ResourceMarker::Resource,
        close_method: Some("close"),
        shape: BuiltinTypeShape::Opaque,
        role: None,
    },
    BuiltinTypeRegistration {
        name: "SendHalf",
        marker: ResourceMarker::Resource,
        close_method: Some("close"),
        shape: BuiltinTypeShape::Opaque,
        role: None,
    },
    BuiltinTypeRegistration {
        name: "RecvHalf",
        marker: ResourceMarker::Resource,
        close_method: Some("close"),
        shape: BuiltinTypeShape::Opaque,
        role: None,
    },
    BuiltinTypeRegistration {
        name: "PanicInfo",
        marker: ResourceMarker::BitCopy,
        close_method: None,
        shape: BuiltinTypeShape::Struct(PANIC_INFO_FIELDS),
        role: Some(BuiltinTypeRole::CrashInfo),
    },
    BuiltinTypeRegistration {
        name: "CrashAction",
        marker: ResourceMarker::None,
        close_method: None,
        shape: BuiltinTypeShape::Enum(CRASH_ACTION_VARIANTS),
        role: None,
    },
];

#[must_use]
pub fn builtin_type_registrations() -> &'static [BuiltinTypeRegistration] {
    BUILTIN_TYPE_REGISTRATIONS
}

#[must_use]
pub fn builtin_type_registration(name: &str) -> Option<&'static BuiltinTypeRegistration> {
    BUILTIN_TYPE_REGISTRATIONS
        .iter()
        .find(|registration| registration.name == name)
}

/// Return the compiler-known crash-info payload registration.
///
/// # Panics
///
/// Panics if the compiler-known crash-info registration is missing from the
/// built-in type registry.
#[must_use]
pub fn crash_info_type_registration() -> &'static BuiltinTypeRegistration {
    BUILTIN_TYPE_REGISTRATIONS
        .iter()
        .find(|registration| registration.role == Some(BuiltinTypeRole::CrashInfo))
        .expect("crash info builtin registration must exist")
}

/// Seed a `TypeClassTable` with the compiler-known built-in substrate types.
///
/// These types are not defined by user-level `TypeDecl` items; they are
/// intrinsic to the runtime and must be seeded before the source-order
/// `Item::TypeDecl` pass so that `ValueClass::of_ty` resolves them correctly
/// in function bodies.
///
/// Resource types carry their canonical consuming close method. `BitCopy`
/// compiler-known records carry their marker and shape here so downstream
/// lowering can query the registry instead of matching user-visible names.
///
/// WHY: `Duplex<S,R>`, `Sink<T>`, `Stream<T>`, etc. are constructed via
/// builtin functions (`duplex_pair`, `channel`) registered in
/// `hew-types/src/check/registration.rs`.  Those builtins return `Ty::Duplex { .. }`
/// which crosses the checker boundary as `ResolvedTy::Named { name: "Duplex", .. }`.
/// Without this seeding, `ValueClass::of_ty` returns `Unknown` for every
/// `Named { "Duplex", .. }` binding, and drop elaboration never fires.
///
/// WHEN-OBSOLETE: when user-level `@resource` type declarations become the
/// canonical authority for all substrate types (i.e. when `std/duplex.hew`
/// ships its own `TypeDecl` with the `#[resource]` attribute).
///
/// WHAT-REAL-SOLUTION: move this seeding into a stdlib module loaded at
/// program-start so user programs never need to see the compiler-internal table.
pub fn seed_builtin_type_classes(type_classes: &mut TypeClassTable) {
    for registration in builtin_type_registrations() {
        debug_assert!(
            registration.marker != ResourceMarker::BitCopy || registration.close_method.is_none(),
            "BitCopy builtin types must not register close methods"
        );
        type_classes.insert(
            registration.name.to_string(),
            (
                registration
                    .marker
                    .to_ast_marker()
                    .unwrap_or(AstResourceMarker::None),
                registration.close_method.map(str::to_string),
            ),
        );
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::value_class::ValueClass;

    #[test]
    fn duplex_is_seeded_as_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            table.get("Duplex"),
            Some(&(AstResourceMarker::Resource, Some("close".to_string())))
        );
    }

    #[test]
    fn sink_is_seeded_as_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            table.get("Sink"),
            Some(&(AstResourceMarker::Resource, Some("close".to_string())))
        );
    }

    #[test]
    fn stream_is_seeded_as_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            table.get("Stream"),
            Some(&(AstResourceMarker::Resource, Some("close".to_string())))
        );
    }

    #[test]
    fn lambda_actor_handle_is_seeded_as_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            table.get("LambdaActorHandle"),
            Some(&(AstResourceMarker::Resource, Some("close".to_string())))
        );
    }

    #[test]
    fn send_half_is_seeded_as_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            table.get("SendHalf"),
            Some(&(AstResourceMarker::Resource, Some("close".to_string())))
        );
    }

    #[test]
    fn recv_half_is_seeded_as_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            table.get("RecvHalf"),
            Some(&(AstResourceMarker::Resource, Some("close".to_string())))
        );
    }

    #[test]
    fn duplex_named_ty_resolves_to_affine_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        let ty = ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::I64],
        };
        assert_eq!(ValueClass::of_ty(&ty, &table), ValueClass::AffineResource);
    }

    #[test]
    fn panic_info_is_seeded_as_bitcopy_marker() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            crate::lookup_type_marker("PanicInfo", &table),
            Some(ResourceMarker::BitCopy),
            "PanicInfo must be BitCopy so crash-hook payload lowering is marker-driven"
        );
    }

    #[test]
    fn panic_info_named_ty_resolves_to_bitcopy() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        let ty = ResolvedTy::Named {
            name: "PanicInfo".to_string(),
            args: vec![],
        };
        assert_eq!(ValueClass::of_ty(&ty, &table), ValueClass::BitCopy);
    }

    #[test]
    fn panic_info_shape_is_registered_as_crash_info_payload() {
        let registration = crash_info_type_registration();
        assert_eq!(registration.name, "PanicInfo");
        assert_eq!(registration.marker, ResourceMarker::BitCopy);
        assert_eq!(registration.role, Some(BuiltinTypeRole::CrashInfo));
        assert_eq!(
            registration.shape,
            BuiltinTypeShape::Struct(&[BuiltinTypeField {
                name: "code",
                ty: BuiltinFieldTy::I64,
            }])
        );
    }

    #[test]
    fn crash_action_is_seeded_as_none_marker() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            table.get("CrashAction"),
            Some(&(AstResourceMarker::None, None)),
            "CrashAction must be in type_classes so push_unknown_type_diagnostics skips it"
        );
    }

    #[test]
    fn panic_info_named_ty_is_known_to_type_classes() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert!(
            table.contains_key("PanicInfo"),
            "PanicInfo absent from TypeClassTable; on(crash) hook params would fire UnknownType"
        );
    }

    #[test]
    fn crash_action_named_ty_is_known_to_type_classes() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert!(
            table.contains_key("CrashAction"),
            "CrashAction absent from TypeClassTable; on(crash) hook return type would fire UnknownType"
        );
    }
}
