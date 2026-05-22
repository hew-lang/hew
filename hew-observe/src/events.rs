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
}
