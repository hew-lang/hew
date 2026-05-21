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
    fn send_label_prefers_registered_handler_name() {
        let meta = trace_event_meta("send");
        assert_eq!(
            meta.swimlane_label(7, Some("Counter::increment")),
            "──▶ send(Counter::increment)"
        );
        assert_eq!(meta.swimlane_label(7, None), "──▶ send(7)");
    }
}
