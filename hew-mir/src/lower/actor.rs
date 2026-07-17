#[cfg(test)]
use super::*;

#[cfg(test)]
mod actor_send_aliasing_collapse {
    //! Revision-2 regression: `actor_send_aliasing` is the one audited
    //! checker-owned `SpanKey` fact consumed in MIR rather than during HIR body
    //! lowering. MIR keys every send lookup at `SpanKey::from` (`module_idx` = 0),
    //! but the checker stamps the map with per-module indices, so a send inside
    //! a non-root (imported / file-import) actor or fn body would miss its fact
    //! and silently fall back to the `Copy` deep-copy path. `collapse_*_to_idx0`
    //! re-keys to idx 0, collision-safe.
    use super::*;
    use hew_types::{ActorSendAliasing, ActorSendCopyReason, SpanKey};

    #[test]
    fn non_root_alias_entry_is_found_at_idx0() {
        let mut map = HashMap::new();
        // Checker recorded an Alias send at byte 10..14 under non-root module 2.
        map.insert(
            SpanKey {
                start: 10,
                end: 14,
                module_idx: 2,
            },
            ActorSendAliasing::Alias,
        );
        let collapsed = collapse_actor_send_aliasing_to_idx0(&map);
        // MIR looks up at module_idx = 0 (SpanKey::from): the entry must resolve.
        let key = SpanKey {
            start: 10,
            end: 14,
            module_idx: 0,
        };
        assert_eq!(
            collapsed.get(&key).copied(),
            Some(ActorSendAliasing::Alias),
            "non-root Alias send must be visible to MIR's idx-0 lookup"
        );
    }

    #[test]
    fn cross_module_byte_range_conflict_falls_back_to_copy() {
        let mut map = HashMap::new();
        // Two different files collide at byte range 4..8 with conflicting modes.
        map.insert(
            SpanKey {
                start: 4,
                end: 8,
                module_idx: 1,
            },
            ActorSendAliasing::Alias,
        );
        map.insert(
            SpanKey {
                start: 4,
                end: 8,
                module_idx: 2,
            },
            ActorSendAliasing::Copy(ActorSendCopyReason::CopyType),
        );
        let collapsed = collapse_actor_send_aliasing_to_idx0(&map);
        // Conflict → omit → idx-0 lookup misses → MIR defaults to safe Copy.
        let key = SpanKey {
            start: 4,
            end: 8,
            module_idx: 0,
        };
        assert!(
            !collapsed.contains_key(&key),
            "a cross-file conflict at one byte range must collapse to the Copy default"
        );
    }

    #[test]
    fn root_alias_entry_is_preserved() {
        let mut map = HashMap::new();
        map.insert(
            SpanKey {
                start: 2,
                end: 5,
                module_idx: 0,
            },
            ActorSendAliasing::Alias,
        );
        let collapsed = collapse_actor_send_aliasing_to_idx0(&map);
        assert_eq!(
            collapsed
                .get(&SpanKey {
                    start: 2,
                    end: 5,
                    module_idx: 0,
                })
                .copied(),
            Some(ActorSendAliasing::Alias),
            "root-level Alias sends must be unaffected by the collapse"
        );
    }
}
