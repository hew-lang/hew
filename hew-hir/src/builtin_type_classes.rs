use hew_parser::ast::ResourceMarker;

use crate::value_class::TypeClassTable;

/// Seed a `TypeClassTable` with the M2 substrate compiler-known resource types.
///
/// These types are not defined by user-level `TypeDecl` items; they are
/// intrinsic to the runtime and must be seeded before the source-order
/// `Item::TypeDecl` pass so that `ValueClass::of_ty` resolves them correctly
/// in function bodies.
///
/// Each type is marked `@resource` with its canonical consuming close method.
/// The close method name is what the drop-elaboration pass emits as the runtime
/// symbol prefix (e.g. `"close"` → `hew_duplex_close` via a naming convention
/// established in the runtime ABI).
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
    // Duplex<S, R> — a bidirectional channel endpoint.
    // Close method: "close" → runtime symbol hew_duplex_close.
    type_classes.insert(
        "Duplex".to_string(),
        (ResourceMarker::Resource, Some("close".to_string())),
    );

    // Sink<T> — the write end of a unidirectional channel.
    // Close method: "close" → runtime symbol hew_sink_close.
    type_classes.insert(
        "Sink".to_string(),
        (ResourceMarker::Resource, Some("close".to_string())),
    );

    // Stream<T> — the read end of a unidirectional channel.
    // Close method: "close" → runtime symbol hew_stream_close.
    type_classes.insert(
        "Stream".to_string(),
        (ResourceMarker::Resource, Some("close".to_string())),
    );

    // LambdaActorHandle — the Duplex<Msg, Reply> handle addressing a spawned
    // lambda actor's message queue.  Close method is "close" (terminates the
    // actor).
    type_classes.insert(
        "LambdaActorHandle".to_string(),
        (ResourceMarker::Resource, Some("close".to_string())),
    );

    // SendHalf<T> — the producer-only direction extracted from a Duplex
    // pair via `.send_half()` (consumes the unified handle). Close method
    // is "close" → runtime symbol hew_send_half_close which terminates only
    // the send direction. Per §5.16 transport model, half-handles are
    // independent owned resources that participate in scope-exit drop.
    type_classes.insert(
        "SendHalf".to_string(),
        (ResourceMarker::Resource, Some("close".to_string())),
    );

    // RecvHalf<T> — symmetric to SendHalf for the consumer-only direction.
    type_classes.insert(
        "RecvHalf".to_string(),
        (ResourceMarker::Resource, Some("close".to_string())),
    );
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::value_class::ValueClass;
    use hew_types::ResolvedTy;

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
        };
        assert_eq!(ValueClass::of_ty(&ty, &table), ValueClass::AffineResource);
    }
}
