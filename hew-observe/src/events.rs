//! Trace event metadata shared by hew-observe views.

use crate::client::{ActorIdentity, TraceEvent};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraceActorKey {
    pub producer_instance_id: String,
    pub actor_id: u64,
    pub spawn_serial: Option<u64>,
}

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
                || format!("──▶ send(msg_type={msg_type}; handler unavailable)"),
                |name| format!("──▶ send({name})"),
            ),
            _ => format!("{} {}", self.glyph, self.label),
        }
    }
}

pub const CURRENT_TRACE_EVENT_METADATA: &[TraceEventMeta] = &[
    // Runtime-emitted v1 trace events. MachineDispatchUnreachable is a
    // diagnostic or crash kind, not a trace event type; it remains under
    // "crash".
    TraceEventMeta {
        name: "send",
        label: "send",
        glyph: '\u{25CF}',
        tone: TraceEventTone::Accent,
        group: TraceEventGroup::Message,
        actionable: true,
    },
    TraceEventMeta {
        name: "actor.delivery_refused",
        label: "delivery refused",
        glyph: '\u{2298}',
        tone: TraceEventTone::Warning,
        group: TraceEventGroup::Message,
        actionable: true,
    },
    TraceEventMeta {
        name: "actor.rc_sweep",
        label: "Rc survivors reclaimed",
        glyph: '\u{21BB}',
        tone: TraceEventTone::Warning,
        group: TraceEventGroup::Lifecycle,
        actionable: true,
    },
    TraceEventMeta {
        name: "live.execution",
        label: "live execution",
        glyph: '\u{25B6}',
        tone: TraceEventTone::Accent,
        group: TraceEventGroup::Lifecycle,
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
    // Runtime-emitted cancellation substrate spans (non-actionable until
    // dedicated Observe UI views exist).
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
    // Runtime-emitted link, monitor, heap-cap, and cluster partition spans
    // (kept non-actionable until Observe adds dedicated drill-down views).
    TraceEventMeta {
        name: "link_created",
        label: "link created",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Lifecycle,
        actionable: false,
    },
    TraceEventMeta {
        name: "link_exit",
        label: "link exit",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Warning,
        group: TraceEventGroup::Lifecycle,
        actionable: false,
    },
    TraceEventMeta {
        name: "monitor_created",
        label: "monitor created",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Primary,
        group: TraceEventGroup::Lifecycle,
        actionable: false,
    },
    TraceEventMeta {
        name: "monitor_down",
        label: "monitor down",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Warning,
        group: TraceEventGroup::Lifecycle,
        actionable: false,
    },
    TraceEventMeta {
        name: "max_heap_exceeded",
        label: "max heap exceeded",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Error,
        group: TraceEventGroup::Lifecycle,
        actionable: false,
    },
    TraceEventMeta {
        name: "partition_detected",
        label: "partition detected",
        glyph: '\u{00B7}',
        tone: TraceEventTone::Error,
        group: TraceEventGroup::Internal,
        actionable: false,
    },
    // Supervisor decisions support exact actor-incarnation drill-down.
    TraceEventMeta {
        name: "supervisor_restart",
        label: "supervisor restart",
        glyph: '\u{21BB}',
        tone: TraceEventTone::Warning,
        group: TraceEventGroup::Lifecycle,
        actionable: true,
    },
    TraceEventMeta {
        name: "supervisor_escalate",
        label: "supervisor escalate",
        glyph: '\u{2191}',
        tone: TraceEventTone::Error,
        group: TraceEventGroup::Lifecycle,
        actionable: true,
    },
    TraceEventMeta {
        name: "supervisor_circuit_open",
        label: "supervisor circuit open",
        glyph: '\u{26A0}',
        tone: TraceEventTone::Error,
        group: TraceEventGroup::Lifecycle,
        actionable: true,
    },
    TraceEventMeta {
        name: "supervisor_circuit_half_open",
        label: "supervisor circuit half-open",
        glyph: '\u{26A1}',
        tone: TraceEventTone::Warning,
        group: TraceEventGroup::Lifecycle,
        actionable: true,
    },
    TraceEventMeta {
        name: "supervisor_circuit_close",
        label: "supervisor circuit close",
        glyph: '\u{2713}',
        tone: TraceEventTone::Healthy,
        group: TraceEventGroup::Lifecycle,
        actionable: true,
    },
    TraceEventMeta {
        name: "supervisor_max_restarts",
        label: "supervisor max restarts",
        glyph: '\u{2716}',
        tone: TraceEventTone::Error,
        group: TraceEventGroup::Lifecycle,
        actionable: true,
    },
    TraceEventMeta {
        name: "supervisor_backoff",
        label: "supervisor backoff",
        glyph: '\u{23F3}',
        tone: TraceEventTone::Warning,
        group: TraceEventGroup::Lifecycle,
        actionable: true,
    },
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

/// Actor used to place an event in the legacy single-lane TUI.
///
/// Send events prefer their real source. Other events prefer their explicit
/// subject. Unknown attribution remains unknown instead of being synthesized
/// from timing or message type.
pub fn trace_event_primary_actor(event: &TraceEvent) -> Option<&ActorIdentity> {
    if event.event_type == "send" {
        event
            .source_actor
            .as_ref()
            .or(event.destination_actor.as_ref())
    } else {
        event
            .subject
            .as_ref()
            .and_then(|subject| subject.actor.as_ref())
            .or(event.source_actor.as_ref())
            .or(event.destination_actor.as_ref())
    }
}

pub fn trace_event_actor_key(
    producer_instance_id: &str,
    event: &TraceEvent,
) -> Option<TraceActorKey> {
    trace_event_primary_actor(event).map(|actor| TraceActorKey {
        producer_instance_id: producer_instance_id.to_owned(),
        actor_id: actor.actor_id,
        spawn_serial: actor.spawn_serial,
    })
}

pub fn trace_event_matches_actor(
    producer_instance_id: &str,
    event: &TraceEvent,
    key: &TraceActorKey,
) -> bool {
    if producer_instance_id != key.producer_instance_id {
        return false;
    }
    let matches = |actor: &ActorIdentity| {
        actor.actor_id == key.actor_id
            && match (key.spawn_serial, actor.spawn_serial) {
                (Some(expected), Some(actual)) => expected == actual,
                _ => true,
            }
    };
    event
        .subject
        .as_ref()
        .and_then(|subject| subject.actor.as_ref())
        .is_some_and(matches)
        || event.source_actor.as_ref().is_some_and(matches)
        || event.destination_actor.as_ref().is_some_and(matches)
}

/// Stable display lane label. Remote attribution stays explicitly unknown;
/// the current transport does not expose a durable remote identity here.
pub fn trace_event_lane_label(
    endpoint: &str,
    producer_instance_id: &str,
    event: &TraceEvent,
) -> String {
    let Some(actor) = trace_event_primary_actor(event) else {
        return format!("{endpoint}:unknown");
    };
    let producer = producer_instance_id
        .get(..6)
        .unwrap_or(producer_instance_id);
    format!("{endpoint}:{producer}:{}", actor.actor_id >> 48)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn actor_filter_falls_back_to_packed_id_when_spawn_serial_is_missing() {
        let producer = "00000000000000000000000000000001";
        let actor_id = 42;
        let event = TraceEvent {
            subject: Some(crate::client::TraceSubject {
                kind: "actor".to_owned(),
                id: actor_id,
                actor: Some(ActorIdentity {
                    actor_id,
                    spawn_serial: None,
                    actor_type: Some("Counter".to_owned()),
                }),
            }),
            ..TraceEvent::default()
        };
        let selected_from_send = TraceActorKey {
            producer_instance_id: producer.to_owned(),
            actor_id,
            spawn_serial: Some(7),
        };

        assert!(trace_event_matches_actor(
            producer,
            &event,
            &selected_from_send
        ));

        let conflicting_serial = TraceEvent {
            subject: Some(crate::client::TraceSubject {
                kind: "actor".to_owned(),
                id: actor_id,
                actor: Some(ActorIdentity {
                    actor_id,
                    spawn_serial: Some(8),
                    actor_type: Some("Counter".to_owned()),
                }),
            }),
            ..TraceEvent::default()
        };
        assert!(!trace_event_matches_actor(
            producer,
            &conflicting_serial,
            &selected_from_send
        ));
        assert!(!trace_event_matches_actor(
            "00000000000000000000000000000002",
            &event,
            &selected_from_send
        ));
    }

    #[test]
    fn trace_metadata_matches_runtime_and_protocol() {
        let runtime_names: std::collections::BTreeSet<_> = hew_runtime::tracing::EVENT_TYPE_NAMES
            .iter()
            .map(|(_, event_type)| *event_type)
            .collect();
        for event_type in &runtime_names {
            let meta = trace_event_meta(event_type);
            assert_eq!(
                meta.name, *event_type,
                "runtime event {event_type} must resolve to concrete Observe metadata"
            );
            assert_ne!(meta.label, UNKNOWN_TRACE_EVENT_META.label);
        }

        // Metadata reflects runtime producers 1:1, plus the two v1-protocol
        // lambda compatibility rows. No speculative placeholder rows are kept.
        let v1_compat: std::collections::BTreeSet<_> =
            ["lambda_spawned", "lambda_released"].into_iter().collect();
        let metadata_names: std::collections::BTreeSet<_> = CURRENT_TRACE_EVENT_METADATA
            .iter()
            .map(|meta| meta.name)
            .collect();
        let expected_names: std::collections::BTreeSet<_> =
            runtime_names.union(&v1_compat).copied().collect();
        assert_eq!(
            metadata_names, expected_names,
            "Observe metadata must match runtime EVENT_TYPE_NAMES 1:1 (plus v1 lambda compat)"
        );

        let actionable: std::collections::BTreeSet<_> = CURRENT_TRACE_EVENT_METADATA
            .iter()
            .filter(|meta| meta.is_actionable())
            .map(|meta| meta.name)
            .collect();
        for event_type in &actionable {
            let meta = trace_event_meta(event_type);
            assert_ne!(meta.label, UNKNOWN_TRACE_EVENT_META.label);
            assert_ne!(meta.glyph, UNKNOWN_TRACE_EVENT_META.glyph);
        }
        let protocol = hew_observe_protocol::ACTIONABLE_TRACE_EVENT_TYPES
            .iter()
            .copied()
            .collect();
        assert_eq!(
            actionable, protocol,
            "Observe and its wire protocol must agree on actionable trace events"
        );
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
    fn runtime_substrate_events_have_concrete_non_actionable_metadata() {
        let substrate_events = [
            "cancellation_fired",
            "cancellation_scope_cancelled",
            "cancellation_cooperate_checked",
            "link_created",
            "link_exit",
            "monitor_created",
            "monitor_down",
            "max_heap_exceeded",
            "partition_detected",
        ];

        for event_type in substrate_events {
            let meta = trace_event_meta(event_type);
            assert_eq!(meta.name, event_type);
            assert_ne!(meta.label, UNKNOWN_TRACE_EVENT_META.label);
            assert!(
                !meta.is_actionable(),
                "{event_type} stays non-actionable until Observe adds dedicated handling"
            );
        }
    }

    #[test]
    fn unemitted_future_event_names_fall_back_to_unknown() {
        for unemitted in [
            "quic_endpoint_lifecycle",
            "quic_connection_lifecycle",
            "quic_stream_open",
            "quic_stream_close",
            "quic_datagram_send",
            "quic_datagram_recv",
            "quic_error_raise",
            "lock_acquire",
            "lock_release",
            "lock_poison",
        ] {
            let meta = trace_event_meta(unemitted);
            assert_eq!(
                meta, UNKNOWN_TRACE_EVENT_META,
                "unemitted event {unemitted} must fall back to UNKNOWN_TRACE_EVENT_META"
            );
        }
    }

    #[test]
    fn native_supervisor_events_support_drill_down() {
        for event_type in [
            "supervisor_restart",
            "supervisor_escalate",
            "supervisor_circuit_open",
            "supervisor_circuit_half_open",
            "supervisor_circuit_close",
            "supervisor_max_restarts",
            "supervisor_backoff",
        ] {
            let meta = trace_event_meta(event_type);
            assert_eq!(
                meta.name, event_type,
                "{event_type} must resolve to its concrete row, not the unknown fallback"
            );
            assert_ne!(meta.label, UNKNOWN_TRACE_EVENT_META.label);
            assert_eq!(meta.group, TraceEventGroup::Lifecycle);
            assert!(
                meta.is_actionable(),
                "{event_type} supports supervisor drill-down"
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
        assert_eq!(
            meta.swimlane_label(7, None),
            "──▶ send(msg_type=7; handler unavailable)"
        );
    }
}
