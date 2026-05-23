//! Trace event metadata shared by hew-observe views.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraceEventGroup {
    Lifecycle,
    Message,
    Channel,
    Lambda,
    Internal,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraceEventTone {
    Accent,
    Healthy,
    Warning,
    Error,
    Stopped,
    Primary,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TraceEventMeta {
    pub name: &'static str,
    pub label: &'static str,
    pub glyph: char,
    pub tone: TraceEventTone,
    pub group: TraceEventGroup,
    pub actionable: bool,
}

impl TraceEventMeta {
    pub const fn is_actionable(self) -> bool {
        self.actionable
    }

    pub fn swimlane_label(self, msg_type: i32, handler_name: Option<&str>) -> String {
        match self.name {
            "send" => handler_name.map_or_else(
                || format!("──▶ send({msg_type})"),
                |name| format!("──▶ send({name})"),
            ),
            _ => format!("{} {}", self.glyph, self.label),
        }
    }
}

/// Closed taxonomy of v0.5 concurrency trace event kinds that the runtime
/// (`hew-runtime/src/tracing.rs`) actively emits. The enum is closed so any
/// new producer-emitted span must add a variant here, forcing the
/// consumer-side metadata table to be updated in lockstep.
///
/// **Out of this enum by design (per R58 Q138 Option B):** QUIC endpoint /
/// connection / stream / datagram / error spans; cancellation token spans;
/// auto-injected lock spans; supervisor lifecycle (start / restart /
/// shutdown); channel partition; machine `emit` / `transition` /
/// `crashed`. The runtime does not yet emit dedicated trace spans for any
/// of those. Pre-enumerating them in the closed taxonomy now would
/// fragment the v0.5 taxonomy across squashes as native-M3 producer-side
/// emission lands. The corresponding `TraceEventMeta` shim rows below
/// remain available for non-actionable rendering until then.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TraceEventKind {
    Begin,
    End,
    Spawn,
    Crash,
    Stop,
    Send,
    IoAccept,
    IoRecv,
    DuplexCreated,
    DuplexHalfSplit,
    DuplexClosed,
    SinkClosed,
    StreamClosed,
    LambdaSpawned,
    LambdaReleased,
}

impl TraceEventKind {
    /// Every variant of the closed taxonomy, in declaration order. The
    /// `runtime_taxonomy_is_closed` test asserts each variant appears here
    /// exactly once.
    pub const ALL: &'static [Self] = &[
        Self::Begin,
        Self::End,
        Self::Spawn,
        Self::Crash,
        Self::Stop,
        Self::Send,
        Self::IoAccept,
        Self::IoRecv,
        Self::DuplexCreated,
        Self::DuplexHalfSplit,
        Self::DuplexClosed,
        Self::SinkClosed,
        Self::StreamClosed,
        Self::LambdaSpawned,
        Self::LambdaReleased,
    ];

    /// Canonical event-name string, matching the JSON `event_type` field
    /// produced by `hew_runtime::tracing::drain_events_json`.
    #[must_use]
    pub const fn name(self) -> &'static str {
        match self {
            Self::Begin => "begin",
            Self::End => "end",
            Self::Spawn => "spawn",
            Self::Crash => "crash",
            Self::Stop => "stop",
            Self::Send => "send",
            Self::IoAccept => "io_accept",
            Self::IoRecv => "io_recv",
            Self::DuplexCreated => "duplex_created",
            Self::DuplexHalfSplit => "duplex_half_split",
            Self::DuplexClosed => "duplex_closed",
            Self::SinkClosed => "sink_closed",
            Self::StreamClosed => "stream_closed",
            Self::LambdaSpawned => "lambda_spawned",
            Self::LambdaReleased => "lambda_released",
        }
    }

    /// Closed resolution from a JSON `event_type` string to its taxonomy
    /// kind. Returns `None` for any name outside the producer-emitted set,
    /// including the deferred-shim names ("quic_*", "cancellation_*",
    /// "lock_*") which intentionally do not participate in closure.
    #[must_use]
    pub fn resolve(name: &str) -> Option<Self> {
        Self::ALL.iter().copied().find(|k| k.name() == name)
    }

    /// Render metadata for this kind. Looks up the existing
    /// [`CURRENT_TRACE_EVENT_METADATA`] table so visual conventions
    /// remain a single source of truth.
    #[must_use]
    pub fn meta(self) -> TraceEventMeta {
        trace_event_meta(self.name())
    }

    /// True when this kind is part of the actionable surface promoted into
    /// the trace UI.
    #[must_use]
    pub fn is_actionable(self) -> bool {
        self.meta().is_actionable()
    }
}

/// Mirror of `hew_runtime::tracing::EVENT_TYPE_NAMES` for cross-crate
/// closure validation. `hew-observe` is a bin-only crate today and cannot
/// be a dev-dependency of `hew-runtime`, so the producer-side string list
/// is duplicated here exactly as in the
/// `divide_by_zero_trap_surfaces_trap_kind_on_observe_event_surface`
/// precedent in `hew-runtime/tests/crashinfo_observe_event.rs`. If a name
/// changes on the producer, both this constant and `TraceEventKind` must
/// be updated together.
pub const RUNTIME_EMITTED_EVENT_NAMES: &[&str] = &[
    "begin",
    "end",
    "spawn",
    "crash",
    "stop",
    "send",
    "io_accept",
    "io_recv",
    "duplex_created",
    "duplex_half_split",
    "duplex_closed",
    "sink_closed",
    "stream_closed",
    "lambda_spawned",
    "lambda_released",
];

pub const ACTIONABLE_TRACE_EVENT_TYPES: &[&str] = &[
    "send",
    "spawn",
    "crash",
    "stop",
    "duplex_created",
    "duplex_half_split",
    "duplex_closed",
    "sink_closed",
    "stream_closed",
    "lambda_spawned",
    "lambda_released",
];

pub const CURRENT_TRACE_EVENT_METADATA: &[TraceEventMeta] = &[
    // Runtime-emitted v0.5 trace events. GenBlockInMachineTransition,
    // AwaitInMachineTransition, and MachineDispatchUnreachable are diagnostics
    // or crash kinds, not trace event types; they remain under "crash".
    TraceEventMeta {
        name: "send",
        label: "send",
        glyph: '\u{25CF}',
        tone: TraceEventTone::Accent,
        group: TraceEventGroup::Message,
        actionable: true,
    },
    TraceEventMeta {
        name: "spawn",
        label: "spawn",
        glyph: '\u{25C6}',
        tone: TraceEventTone::Healthy,
        group: TraceEventGroup::Lifecycle,
        actionable: true,
    },
    TraceEventMeta {
        name: "crash",
        label: "crash",
        glyph: '\u{2715}',
        tone: TraceEventTone::Error,
        group: TraceEventGroup::Lifecycle,
        actionable: true,
    },
    TraceEventMeta {
        name: "stop",
        label: "stop",
        glyph: '\u{25C7}',
        tone: TraceEventTone::Stopped,
        group: TraceEventGroup::Lifecycle,
        actionable: true,
    },
    TraceEventMeta {
        name: "duplex_created",
        label: "duplex created",
        glyph: '\u{2194}',
        tone: TraceEventTone::Accent,
        group: TraceEventGroup::Channel,
        actionable: true,
    },
    TraceEventMeta {
        name: "duplex_half_split",
        label: "duplex half split",
        glyph: '\u{21C4}',
        tone: TraceEventTone::Warning,
        group: TraceEventGroup::Channel,
        actionable: true,
    },
    TraceEventMeta {
        name: "duplex_closed",
        label: "duplex closed",
        glyph: '\u{2298}',
        tone: TraceEventTone::Stopped,
        group: TraceEventGroup::Channel,
        actionable: true,
    },
    TraceEventMeta {
        name: "sink_closed",
        label: "sink closed",
        glyph: '\u{2193}',
        tone: TraceEventTone::Stopped,
        group: TraceEventGroup::Channel,
        actionable: true,
    },
    TraceEventMeta {
        name: "stream_closed",
        label: "stream closed",
        glyph: '\u{2191}',
        tone: TraceEventTone::Stopped,
        group: TraceEventGroup::Channel,
        actionable: true,
    },
    TraceEventMeta {
        name: "lambda_spawned",
        label: "lambda spawned",
        glyph: '\u{03BB}',
        tone: TraceEventTone::Healthy,
        group: TraceEventGroup::Lambda,
        actionable: true,
    },
    TraceEventMeta {
        name: "lambda_released",
        label: "lambda released",
        glyph: '\u{03BB}',
        tone: TraceEventTone::Stopped,
        group: TraceEventGroup::Lambda,
        actionable: true,
    },
    TraceEventMeta {
        name: "begin",
        label: "begin",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Internal,
        actionable: false,
    },
    TraceEventMeta {
        name: "end",
        label: "end",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Internal,
        actionable: false,
    },
    TraceEventMeta {
        name: "io_accept",
        label: "io accept",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Internal,
        actionable: false,
    },
    TraceEventMeta {
        name: "io_recv",
        label: "io recv",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Internal,
        actionable: false,
    },
    // SHIM: pending native-M3 producer-side emission. These are intentionally
    // non-actionable until the runtime emits QUIC lifecycle trace spans.
    TraceEventMeta {
        name: "quic_endpoint_lifecycle",
        label: "quic endpoint lifecycle",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Channel,
        actionable: false,
    },
    TraceEventMeta {
        name: "quic_connection_lifecycle",
        label: "quic connection lifecycle",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Channel,
        actionable: false,
    },
    TraceEventMeta {
        name: "quic_stream_open",
        label: "quic stream open",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Channel,
        actionable: false,
    },
    TraceEventMeta {
        name: "quic_stream_close",
        label: "quic stream close",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Channel,
        actionable: false,
    },
    TraceEventMeta {
        name: "quic_datagram_send",
        label: "quic datagram send",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Channel,
        actionable: false,
    },
    TraceEventMeta {
        name: "quic_datagram_recv",
        label: "quic datagram recv",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Channel,
        actionable: false,
    },
    TraceEventMeta {
        name: "quic_error_raise",
        label: "quic error raise",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Channel,
        actionable: false,
    },
    // SHIM: pending native-M3 producer-side emission. D24-2 cancellation token
    // substrate exists, but cancel/check points do not emit trace spans yet.
    TraceEventMeta {
        name: "cancellation_fired",
        label: "cancellation fired",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Internal,
        actionable: false,
    },
    TraceEventMeta {
        name: "cancellation_scope_cancelled",
        label: "cancellation scope cancelled",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Internal,
        actionable: false,
    },
    TraceEventMeta {
        name: "cancellation_cooperate_checked",
        label: "cancellation cooperate checked",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Internal,
        actionable: false,
    },
    // SHIM: pending native-M3 producer-side emission. Auto-injected lock
    // substrate is tracked for v0.5 observability, but lock spans are absent.
    TraceEventMeta {
        name: "lock_acquire",
        label: "lock acquire",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Internal,
        actionable: false,
    },
    TraceEventMeta {
        name: "lock_release",
        label: "lock release",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Internal,
        actionable: false,
    },
    TraceEventMeta {
        name: "lock_poison",
        label: "lock poison",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Internal,
        actionable: false,
    },
    // SHIM: pending native-M3 producer-side emission. WHY: supervisor
    // restart/circuit, partition detection, max_heap enforcement, and
    // link/monitor substrates have no runtime trace spans yet
    // (SPAN_SUPERVISOR*, SPAN_PARTITION, SPAN_MAX_HEAP, SPAN_LINK, or
    // SPAN_MONITOR). WHEN obsolete: after the native-M3 supervisor runtime
    // emission milestone lands canonical substrate trace events. WHAT: add
    // concrete TraceEventMeta rows only for runtime-emitted event names, keeping
    // them non-actionable until the observe UI has deliberate handling.
];

pub const UNKNOWN_TRACE_EVENT_META: TraceEventMeta = TraceEventMeta {
    name: "unknown",
    label: "unknown",
    glyph: '\u{00B7}',
    tone: TraceEventTone::Primary,
    group: TraceEventGroup::Unknown,
    actionable: false,
};

pub fn trace_event_meta(event_type: &str) -> TraceEventMeta {
    CURRENT_TRACE_EVENT_METADATA
        .iter()
        .copied()
        .find(|meta| meta.name == event_type)
        .unwrap_or(UNKNOWN_TRACE_EVENT_META)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn current_v05_trace_events_are_actionable() {
        for event_type in ACTIONABLE_TRACE_EVENT_TYPES {
            let meta = trace_event_meta(event_type);
            assert!(
                meta.is_actionable(),
                "{event_type} should be actionable in hew-observe"
            );
            assert_ne!(meta.label, "unknown");
            assert_ne!(meta.glyph, UNKNOWN_TRACE_EVENT_META.glyph);
        }
    }

    #[test]
    fn internal_and_unknown_events_are_not_actionable() {
        for event_type in ["begin", "end", "io_accept", "io_recv", "unknown_future"] {
            let meta = trace_event_meta(event_type);
            assert!(
                !meta.is_actionable(),
                "{event_type} should not be promoted into the trace UI"
            );
        }
    }

    #[test]
    fn deferred_future_trace_categories_are_present_but_not_actionable() {
        let deferred_events = [
            "quic_endpoint_lifecycle",
            "quic_connection_lifecycle",
            "quic_stream_open",
            "quic_stream_close",
            "quic_datagram_send",
            "quic_datagram_recv",
            "quic_error_raise",
            "cancellation_fired",
            "cancellation_scope_cancelled",
            "cancellation_cooperate_checked",
            "lock_acquire",
            "lock_release",
            "lock_poison",
        ];

        for event_type in deferred_events {
            let meta = trace_event_meta(event_type);
            assert_eq!(meta.name, event_type);
            assert_ne!(meta.label, UNKNOWN_TRACE_EVENT_META.label);
            assert!(
                !meta.is_actionable(),
                "{event_type} must remain non-actionable until producer emission lands"
            );
            assert!(
                !ACTIONABLE_TRACE_EVENT_TYPES.contains(&event_type),
                "{event_type} must not be promoted into the actionable set"
            );
        }
    }

    #[test]
    fn send_label_prefers_registered_handler_name() {
        let meta = trace_event_meta("send");
        assert_eq!(
            meta.swimlane_label(7, Some("Counter::increment")),
            "──▶ send(Counter::increment)"
        );
        assert_eq!(meta.swimlane_label(7, None), "──▶ send(7)");
    }

    // ── Closed taxonomy tests ──────────────────────────────────────────

    /// Every name the runtime can stamp into a drained event must resolve
    /// to a `TraceEventKind`. This is the consumer-side gate that mirrors
    /// the producer-side `event_type_name_mapping_is_total` test in
    /// `hew-runtime/src/tracing.rs`. Adding a new `SPAN_*` constant
    /// without updating `RUNTIME_EMITTED_EVENT_NAMES` and `TraceEventKind`
    /// fails here.
    #[test]
    fn runtime_emitted_names_all_resolve_to_a_kind() {
        for name in RUNTIME_EMITTED_EVENT_NAMES {
            let kind = TraceEventKind::resolve(name)
                .unwrap_or_else(|| panic!("runtime-emitted event {name:?} has no TraceEventKind"));
            assert_eq!(kind.name(), *name);
            let meta = kind.meta();
            assert_eq!(meta.name, *name);
            assert_ne!(
                meta.label, UNKNOWN_TRACE_EVENT_META.label,
                "{name} must have non-unknown metadata"
            );
        }
    }

    /// The closed taxonomy must agree with the runtime's producer list
    /// one-for-one. The two arrays may legitimately differ in order, but
    /// the underlying sets must be identical.
    #[test]
    fn runtime_taxonomy_is_closed() {
        use std::collections::HashSet;
        let producer: HashSet<&str> = RUNTIME_EMITTED_EVENT_NAMES.iter().copied().collect();
        let consumer: HashSet<&str> = TraceEventKind::ALL.iter().map(|k| k.name()).collect();
        assert_eq!(
            producer,
            consumer,
            "TraceEventKind must mirror RUNTIME_EMITTED_EVENT_NAMES exactly; \
             missing on consumer: {missing_consumer:?}, missing on producer: {missing_producer:?}",
            missing_consumer = producer.difference(&consumer).collect::<Vec<_>>(),
            missing_producer = consumer.difference(&producer).collect::<Vec<_>>(),
        );
        assert_eq!(
            TraceEventKind::ALL.len(),
            consumer.len(),
            "TraceEventKind::ALL contains duplicates"
        );
    }

    /// Closed resolution must reject any name outside the producer set —
    /// including the deferred SHIM names that still carry
    /// `TraceEventMeta` rows for non-actionable rendering. This pins the
    /// Q138 Option B boundary: shim metadata exists for display, but does
    /// not promote those names into the closed v0.5 taxonomy.
    #[test]
    fn deferred_shim_names_do_not_resolve_to_a_kind() {
        let deferred = [
            "quic_endpoint_lifecycle",
            "quic_connection_lifecycle",
            "quic_stream_open",
            "quic_stream_close",
            "quic_datagram_send",
            "quic_datagram_recv",
            "quic_error_raise",
            "cancellation_fired",
            "cancellation_scope_cancelled",
            "cancellation_cooperate_checked",
            "lock_acquire",
            "lock_release",
            "lock_poison",
        ];
        for name in deferred {
            assert!(
                TraceEventKind::resolve(name).is_none(),
                "{name} must not participate in the closed v0.5 taxonomy (Q138 Option B)"
            );
            // The shim metadata row must still be available so the UI can
            // render the event non-actionably until producer emission lands.
            let meta = trace_event_meta(name);
            assert_eq!(meta.name, name);
            assert!(!meta.is_actionable());
        }
    }

    #[test]
    fn unknown_names_do_not_resolve() {
        assert!(TraceEventKind::resolve("totally-unknown-event").is_none());
        assert!(TraceEventKind::resolve("").is_none());
    }

    /// Actionability is governed by the metadata table, but the
    /// `ACTIONABLE_TRACE_EVENT_TYPES` flat list must agree with whatever
    /// `TraceEventKind::is_actionable` reports. If they disagree, the
    /// client-side filter and the renderer would render different sets.
    #[test]
    fn actionable_list_matches_kind_actionability() {
        use std::collections::HashSet;
        let actionable_from_kinds: HashSet<&str> = TraceEventKind::ALL
            .iter()
            .filter(|k| k.is_actionable())
            .map(|k| k.name())
            .collect();
        let actionable_const: HashSet<&str> =
            ACTIONABLE_TRACE_EVENT_TYPES.iter().copied().collect();
        assert_eq!(
            actionable_from_kinds, actionable_const,
            "ACTIONABLE_TRACE_EVENT_TYPES must match the actionable subset of TraceEventKind"
        );
    }
}
