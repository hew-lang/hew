use hew_types::builtin_type::{
    BuiltinHandleFamily, BuiltinType, BuiltinTypeMarker, BuiltinTypeRole as BuiltinRegistrationRole,
};
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
    pub builtin: BuiltinType,
    pub name: &'static str,
    pub marker: ResourceMarker,
    pub close_method: Option<&'static str>,
    pub shape: BuiltinTypeShape,
    pub role: Option<BuiltinTypeRole>,
    pub handle_family: Option<BuiltinHandleFamily>,
    pub arity: usize,
    pub roles: &'static [BuiltinRegistrationRole],
}

impl BuiltinTypeRegistration {
    #[must_use]
    pub const fn name(self) -> &'static str {
        self.builtin.canonical_name()
    }

    #[must_use]
    pub fn has_role(self, role: BuiltinRegistrationRole) -> bool {
        self.roles.contains(&role)
    }
}

const CRASH_INFO_FIELDS: &[BuiltinTypeField] = &[BuiltinTypeField {
    name: "code",
    ty: BuiltinFieldTy::I64,
}];

const CRASH_ACTION_VARIANTS: &[&str] = &["Restart", "Escalate", "Kill"];

const fn marker(marker: BuiltinTypeMarker) -> ResourceMarker {
    match marker {
        BuiltinTypeMarker::None => ResourceMarker::None,
        BuiltinTypeMarker::BitCopy => ResourceMarker::BitCopy,
        BuiltinTypeMarker::Resource => ResourceMarker::Resource,
        BuiltinTypeMarker::Linear => ResourceMarker::Linear,
    }
}

const fn legacy_role(builtin: BuiltinType) -> Option<BuiltinTypeRole> {
    match builtin {
        BuiltinType::CrashInfo => Some(BuiltinTypeRole::CrashInfo),
        _ => None,
    }
}

macro_rules! registration {
    ($builtin:ident, $shape:expr) => {
        BuiltinTypeRegistration {
            builtin: BuiltinType::$builtin,
            name: BuiltinType::$builtin.canonical_name(),
            marker: marker(BuiltinType::$builtin.marker()),
            close_method: BuiltinType::$builtin.close_method(),
            shape: $shape,
            role: legacy_role(BuiltinType::$builtin),
            handle_family: BuiltinType::$builtin.handle_family(),
            arity: BuiltinType::$builtin.arity(),
            roles: BuiltinType::$builtin.roles(),
        }
    };
}

const BUILTIN_TYPE_REGISTRATIONS: &[BuiltinTypeRegistration] = &[
    registration!(Duplex, BuiltinTypeShape::Opaque),
    registration!(Sink, BuiltinTypeShape::Opaque),
    registration!(Stream, BuiltinTypeShape::Opaque),
    registration!(Vec, BuiltinTypeShape::Opaque),
    registration!(HashMap, BuiltinTypeShape::Opaque),
    registration!(HashSet, BuiltinTypeShape::Opaque),
    registration!(CancellationToken, BuiltinTypeShape::Opaque),
    registration!(ActorRef, BuiltinTypeShape::Opaque),
    registration!(Actor, BuiltinTypeShape::Opaque),
    registration!(Pid, BuiltinTypeShape::Opaque),
    registration!(LocalPid, BuiltinTypeShape::Opaque),
    registration!(RemotePid, BuiltinTypeShape::Opaque),
    registration!(HewActor, BuiltinTypeShape::Opaque),
    registration!(HewDuplex, BuiltinTypeShape::Opaque),
    registration!(HewSendHalf, BuiltinTypeShape::Opaque),
    registration!(HewRecvHalf, BuiltinTypeShape::Opaque),
    registration!(BoxedActor, BuiltinTypeShape::Opaque),
    registration!(ActorState, BuiltinTypeShape::Opaque),
    registration!(MachineState, BuiltinTypeShape::Opaque),
    registration!(LambdaActorHandle, BuiltinTypeShape::Opaque),
    registration!(SendHalf, BuiltinTypeShape::Opaque),
    registration!(RecvHalf, BuiltinTypeShape::Opaque),
    registration!(CrashInfo, BuiltinTypeShape::Struct(CRASH_INFO_FIELDS)),
    registration!(CrashAction, BuiltinTypeShape::Enum(CRASH_ACTION_VARIANTS)),
];

#[must_use]
pub fn builtin_type_registrations() -> &'static [BuiltinTypeRegistration] {
    BUILTIN_TYPE_REGISTRATIONS
}

#[must_use]
pub fn builtin_type_registration(name: &str) -> Option<&'static BuiltinTypeRegistration> {
    BUILTIN_TYPE_REGISTRATIONS
        .iter()
        .find(|registration| registration.name() == name)
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
            registration.name().to_string(),
            (
                registration.marker,
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
            Some(&(ResourceMarker::Resource, Some("close".to_string())))
        );
    }

    #[test]
    fn sink_is_seeded_as_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            table.get("Sink"),
            Some(&(ResourceMarker::Resource, Some("close".to_string())))
        );
    }

    #[test]
    fn stream_is_seeded_as_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            table.get("Stream"),
            Some(&(ResourceMarker::Resource, Some("close".to_string())))
        );
    }

    #[test]
    fn lambda_actor_handle_is_seeded_as_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            table.get("LambdaActorHandle"),
            Some(&(ResourceMarker::Resource, Some("close".to_string())))
        );
    }

    #[test]
    fn send_half_is_seeded_as_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            table.get("SendHalf"),
            Some(&(ResourceMarker::Resource, Some("close".to_string())))
        );
    }

    #[test]
    fn recv_half_is_seeded_as_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            table.get("RecvHalf"),
            Some(&(ResourceMarker::Resource, Some("close".to_string())))
        );
    }

    #[test]
    fn duplex_named_ty_resolves_to_affine_resource() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        let ty = ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::I64],
            builtin: Some(hew_types::BuiltinType::Duplex),
        };
        assert_eq!(ValueClass::of_ty(&ty, &table), ValueClass::AffineResource);
    }

    #[test]
    fn crash_info_is_seeded_as_bitcopy_marker() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert_eq!(
            crate::lookup_type_marker("CrashInfo", &table),
            Some(ResourceMarker::BitCopy),
            "CrashInfo must be BitCopy so crash-hook payload lowering is marker-driven"
        );
    }

    #[test]
    fn crash_info_named_ty_resolves_to_bitcopy() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        let ty = ResolvedTy::Named {
            name: "CrashInfo".to_string(),
            args: vec![],
            builtin: None,
        };
        assert_eq!(ValueClass::of_ty(&ty, &table), ValueClass::BitCopy);
    }

    #[test]
    fn crash_info_shape_is_registered_as_crash_info_payload() {
        let registration = crash_info_type_registration();
        assert_eq!(registration.name(), "CrashInfo");
        assert_eq!(registration.marker, ResourceMarker::BitCopy);
        assert!(registration.has_role(BuiltinRegistrationRole::CrashInfoPayload));
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
            Some(&(ResourceMarker::None, None)),
            "CrashAction must be in type_classes so push_unknown_type_diagnostics skips it"
        );
    }

    #[test]
    fn crash_info_named_ty_is_known_to_type_classes() {
        let mut table: TypeClassTable = HashMap::default();
        seed_builtin_type_classes(&mut table);
        assert!(
            table.contains_key("CrashInfo"),
            "CrashInfo absent from TypeClassTable; on(crash) hook params would fire UnknownType"
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

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "table-driven registry coverage keeps each handle fact visible"
    )]
    fn handle_and_project_cap_registrations_carry_shape_marker_and_roles() {
        let expected = [
            (
                BuiltinType::Pid,
                ResourceMarker::None,
                None,
                BuiltinTypeShape::Opaque,
                Some(BuiltinHandleFamily::ActorPid),
                0,
                &[][..],
            ),
            (
                BuiltinType::LocalPid,
                ResourceMarker::Resource,
                None,
                BuiltinTypeShape::Opaque,
                Some(BuiltinHandleFamily::ActorPid),
                1,
                &[
                    BuiltinRegistrationRole::ActorDispatchLocal,
                    BuiltinRegistrationRole::SupervisorLocalPid,
                ][..],
            ),
            (
                BuiltinType::RemotePid,
                ResourceMarker::Resource,
                None,
                BuiltinTypeShape::Opaque,
                Some(BuiltinHandleFamily::ActorPid),
                1,
                &[BuiltinRegistrationRole::ActorDispatchRemote][..],
            ),
            (
                BuiltinType::HewActor,
                ResourceMarker::Resource,
                Some("close"),
                BuiltinTypeShape::Opaque,
                Some(BuiltinHandleFamily::ActorRuntime),
                0,
                &[BuiltinRegistrationRole::WasmNativeOnlyHandle][..],
            ),
            (
                BuiltinType::HewDuplex,
                ResourceMarker::Resource,
                Some("close"),
                BuiltinTypeShape::Opaque,
                Some(BuiltinHandleFamily::Duplex),
                2,
                &[BuiltinRegistrationRole::WasmNativeOnlyHandle][..],
            ),
            (
                BuiltinType::HewSendHalf,
                ResourceMarker::Resource,
                Some("close"),
                BuiltinTypeShape::Opaque,
                Some(BuiltinHandleFamily::DuplexHalf),
                0,
                &[BuiltinRegistrationRole::WasmNativeOnlyHandle][..],
            ),
            (
                BuiltinType::HewRecvHalf,
                ResourceMarker::Resource,
                Some("close"),
                BuiltinTypeShape::Opaque,
                Some(BuiltinHandleFamily::DuplexHalf),
                0,
                &[BuiltinRegistrationRole::WasmNativeOnlyHandle][..],
            ),
            (
                BuiltinType::BoxedActor,
                ResourceMarker::Resource,
                Some("close"),
                BuiltinTypeShape::Opaque,
                Some(BuiltinHandleFamily::ActorRuntime),
                0,
                &[BuiltinRegistrationRole::WasmNativeOnlyHandle][..],
            ),
            (
                BuiltinType::ActorState,
                ResourceMarker::Linear,
                None,
                BuiltinTypeShape::Opaque,
                Some(BuiltinHandleFamily::ActorState),
                1,
                &[BuiltinRegistrationRole::ActorStatePayload][..],
            ),
            (
                BuiltinType::MachineState,
                ResourceMarker::Linear,
                None,
                BuiltinTypeShape::Opaque,
                Some(BuiltinHandleFamily::MachineState),
                1,
                &[BuiltinRegistrationRole::MachineStatePayload][..],
            ),
        ];

        for (kind, marker, close_method, shape, family, arity, roles) in expected {
            let registration = builtin_type_registration(kind.canonical_name())
                .unwrap_or_else(|| panic!("missing HIR registration for {kind:?}"));
            assert_eq!(registration.builtin, kind);
            assert_eq!(registration.marker, marker, "{kind:?} marker");
            assert_eq!(
                registration.close_method, close_method,
                "{kind:?} close method"
            );
            assert_eq!(registration.shape, shape, "{kind:?} shape");
            assert_eq!(registration.handle_family, family, "{kind:?} family");
            assert_eq!(registration.arity, arity, "{kind:?} arity");
            assert_eq!(registration.roles, roles, "{kind:?} roles");
        }
    }
}
