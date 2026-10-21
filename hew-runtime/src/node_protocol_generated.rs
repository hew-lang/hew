// Generated from hew-runtime/protocol/node_controls.hew; run make node-protocol.
// Checked data plans own structure; runtime adapters own semantic validation.
use hew_codec::{DecodeError, Format, Member, Sink, Source, Table};
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct WireLocation {
    pub(crate) node: Vec<u8>,
    pub(crate) target_id: u64,
    pub(crate) incarnation: u32,
    pub(crate) target_kind: u32,
}
impl WireLocation {
    const MEMBERS: &'static [Member<'static>] = &[
        Member::new("node", 1, 0),
        Member::new("target_id", 2, 0),
        Member::new("incarnation", 3, 0),
        Member::new("target_kind", 4, 0),
    ];
    fn write(&self, sink: &mut Sink<'_>) {
        sink.record_begin(Table::new(Self::MEMBERS, true));
        sink.field(0);
        let value = &self.node;
        sink.bytes(value);
        sink.field(1);
        let value = &self.target_id;
        sink.u64(*value);
        sink.field(2);
        let value = &self.incarnation;
        sink.u64(u64::from(*value));
        sink.field(3);
        let value = &self.target_kind;
        sink.u64(u64::from(*value));
        sink.record_end();
    }
    fn read(source: &mut Source<'_>) -> Result<Self, DecodeError> {
        source.record_begin(Table::new(Self::MEMBERS, true))?;
        let mut node = None;
        let mut target_id = None;
        let mut incarnation = None;
        let mut target_kind = None;
        while let Some(index) = source.record_next()? {
            match index {
                0 => {
                    node = Some(source.read_bytes()?);
                }
                1 => {
                    target_id = Some(source.read_u64(u64::MAX)?);
                }
                2 => {
                    incarnation = Some(
                        u32::try_from(source.read_u64(u64::from(u32::MAX))?)
                            .expect("checked integer width"),
                    );
                }
                3 => {
                    target_kind = Some(
                        u32::try_from(source.read_u64(u64::from(u32::MAX))?)
                            .expect("checked integer width"),
                    );
                }
                _ => unreachable!("checked record index"),
            }
        }
        Ok(Self {
            node: node.expect("checked required field"),
            target_id: target_id.expect("checked required field"),
            incarnation: incarnation.expect("checked required field"),
            target_kind: target_kind.expect("checked required field"),
        })
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct WireSession {
    pub(crate) node: Vec<u8>,
    pub(crate) incarnation: u32,
}
impl WireSession {
    const MEMBERS: &'static [Member<'static>] =
        &[Member::new("node", 1, 0), Member::new("incarnation", 2, 0)];
    fn write(&self, sink: &mut Sink<'_>) {
        sink.record_begin(Table::new(Self::MEMBERS, true));
        sink.field(0);
        let value = &self.node;
        sink.bytes(value);
        sink.field(1);
        let value = &self.incarnation;
        sink.u64(u64::from(*value));
        sink.record_end();
    }
    fn read(source: &mut Source<'_>) -> Result<Self, DecodeError> {
        source.record_begin(Table::new(Self::MEMBERS, true))?;
        let mut node = None;
        let mut incarnation = None;
        while let Some(index) = source.record_next()? {
            match index {
                0 => {
                    node = Some(source.read_bytes()?);
                }
                1 => {
                    incarnation = Some(
                        u32::try_from(source.read_u64(u64::from(u32::MAX))?)
                            .expect("checked integer width"),
                    );
                }
                _ => unreachable!("checked record index"),
            }
        }
        Ok(Self {
            node: node.expect("checked required field"),
            incarnation: incarnation.expect("checked required field"),
        })
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CleanupFault {
    pub(crate) code: i32,
    pub(crate) message: String,
}
impl CleanupFault {
    const MEMBERS: &'static [Member<'static>] =
        &[Member::new("code", 1, 0), Member::new("message", 2, 0)];
    fn write(&self, sink: &mut Sink<'_>) {
        sink.record_begin(Table::new(Self::MEMBERS, true));
        sink.field(0);
        let value = &self.code;
        sink.i64(i64::from(*value));
        sink.field(1);
        let value = &self.message;
        sink.str(value);
        sink.record_end();
    }
    fn read(source: &mut Source<'_>) -> Result<Self, DecodeError> {
        source.record_begin(Table::new(Self::MEMBERS, true))?;
        let mut code = None;
        let mut message = None;
        while let Some(index) = source.record_next()? {
            match index {
                0 => {
                    code = Some(
                        i32::try_from(source.read_i64(i64::from(i32::MIN), i64::from(i32::MAX))?)
                            .expect("checked integer width"),
                    );
                }
                1 => {
                    message = Some(source.read_str()?);
                }
                _ => unreachable!("checked record index"),
            }
        }
        Ok(Self {
            code: code.expect("checked required field"),
            message: message.expect("checked required field"),
        })
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DownReason {
    pub(crate) tag: u8,
    pub(crate) code: i32,
}
impl DownReason {
    const MEMBERS: &'static [Member<'static>] =
        &[Member::new("tag", 1, 0), Member::new("code", 2, 0)];
    fn write(&self, sink: &mut Sink<'_>) {
        sink.record_begin(Table::new(Self::MEMBERS, true));
        sink.field(0);
        let value = &self.tag;
        sink.u64(u64::from(*value));
        sink.field(1);
        let value = &self.code;
        sink.i64(i64::from(*value));
        sink.record_end();
    }
    fn read(source: &mut Source<'_>) -> Result<Self, DecodeError> {
        source.record_begin(Table::new(Self::MEMBERS, true))?;
        let mut tag = None;
        let mut code = None;
        while let Some(index) = source.record_next()? {
            match index {
                0 => {
                    tag = Some(
                        u8::try_from(source.read_u64(u64::from(u8::MAX))?)
                            .expect("checked integer width"),
                    );
                }
                1 => {
                    code = Some(
                        i32::try_from(source.read_i64(i64::from(i32::MIN), i64::from(i32::MAX))?)
                            .expect("checked integer width"),
                    );
                }
                _ => unreachable!("checked record index"),
            }
        }
        Ok(Self {
            tag: tag.expect("checked required field"),
            code: code.expect("checked required field"),
        })
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ServiceOffer {
    pub(crate) grant_id: u64,
    pub(crate) target: WireLocation,
    pub(crate) recipient: WireSession,
    pub(crate) handlers: Vec<String>,
    pub(crate) request_id: u64,
}
impl ServiceOffer {
    pub(crate) fn encode(&self, format: Format) -> Vec<u8> {
        let mut sink = Sink::new(format);
        self.write(&mut sink);
        sink.finish()
    }
    pub(crate) fn decode(format: Format, bytes: &[u8]) -> Result<Self, DecodeError> {
        let mut source = Source::new(format, bytes)?.reject_unknown_fields();
        Self::read(&mut source)
    }
    const MEMBERS: &'static [Member<'static>] = &[
        Member::new("grant_id", 1, 0),
        Member::new("target", 2, 0),
        Member::new("recipient", 3, 0),
        Member::new("handlers", 4, 0),
        Member::new("request_id", 5, 0),
    ];
    fn write(&self, sink: &mut Sink<'_>) {
        sink.record_begin(Table::new(Self::MEMBERS, true));
        sink.field(0);
        let value = &self.grant_id;
        sink.u64(*value);
        sink.field(1);
        let value = &self.target;
        (value).write(sink);
        sink.field(2);
        let value = &self.recipient;
        (value).write(sink);
        sink.field(3);
        let value = &self.handlers;
        sink.seq_begin((value).len());
        for value in value {
            sink.str(value);
        }
        sink.seq_end();
        sink.field(4);
        let value = &self.request_id;
        sink.u64(*value);
        sink.record_end();
    }
    fn read(source: &mut Source<'_>) -> Result<Self, DecodeError> {
        source.record_begin(Table::new(Self::MEMBERS, true))?;
        let mut grant_id = None;
        let mut target = None;
        let mut recipient = None;
        let mut handlers = None;
        let mut request_id = None;
        while let Some(index) = source.record_next()? {
            match index {
                0 => {
                    grant_id = Some(source.read_u64(u64::MAX)?);
                }
                1 => {
                    target = Some(WireLocation::read(source)?);
                }
                2 => {
                    recipient = Some(WireSession::read(source)?);
                }
                3 => {
                    handlers = Some({
                        let len = source.seq_begin()?;
                        let mut values = Vec::with_capacity(len);
                        while source.seq_next()? {
                            values.push(source.read_str()?);
                        }
                        values
                    });
                }
                4 => {
                    request_id = Some(source.read_u64(u64::MAX)?);
                }
                _ => unreachable!("checked record index"),
            }
        }
        Ok(Self {
            grant_id: grant_id.expect("checked required field"),
            target: target.expect("checked required field"),
            recipient: recipient.expect("checked required field"),
            handlers: handlers.expect("checked required field"),
            request_id: request_id.expect("checked required field"),
        })
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MonitorDown {
    pub(crate) ref_id: u64,
    pub(crate) target: WireLocation,
    pub(crate) reason: DownReason,
    pub(crate) crash_kind: i32,
    pub(crate) end_reason: u8,
    pub(crate) close_fault: Option<CleanupFault>,
}
impl MonitorDown {
    pub(crate) fn encode(&self, format: Format) -> Vec<u8> {
        let mut sink = Sink::new(format);
        self.write(&mut sink);
        sink.finish()
    }
    pub(crate) fn decode(format: Format, bytes: &[u8]) -> Result<Self, DecodeError> {
        let mut source = Source::new(format, bytes)?.reject_unknown_fields();
        Self::read(&mut source)
    }
    const MEMBERS: &'static [Member<'static>] = &[
        Member::new("ref_id", 1, 0),
        Member::new("target", 2, 0),
        Member::new("reason", 3, 0),
        Member::new("crash_kind", 4, 0),
        Member::new("end_reason", 5, 0),
        Member::new("close_fault", 6, 0),
    ];
    fn write(&self, sink: &mut Sink<'_>) {
        sink.record_begin(Table::new(Self::MEMBERS, true));
        sink.field(0);
        let value = &self.ref_id;
        sink.u64(*value);
        sink.field(1);
        let value = &self.target;
        (value).write(sink);
        sink.field(2);
        let value = &self.reason;
        (value).write(sink);
        sink.field(3);
        let value = &self.crash_kind;
        sink.i64(i64::from(*value));
        sink.field(4);
        let value = &self.end_reason;
        sink.u64(u64::from(*value));
        sink.field(5);
        let value = &self.close_fault;
        match value {
            Some(value) => {
                (value).write(sink);
            }
            None => sink.null(),
        }
        sink.record_end();
    }
    fn read(source: &mut Source<'_>) -> Result<Self, DecodeError> {
        source.record_begin(Table::new(Self::MEMBERS, true))?;
        let mut ref_id = None;
        let mut target = None;
        let mut reason = None;
        let mut crash_kind = None;
        let mut end_reason = None;
        let mut close_fault = None;
        while let Some(index) = source.record_next()? {
            match index {
                0 => {
                    ref_id = Some(source.read_u64(u64::MAX)?);
                }
                1 => {
                    target = Some(WireLocation::read(source)?);
                }
                2 => {
                    reason = Some(DownReason::read(source)?);
                }
                3 => {
                    crash_kind = Some(
                        i32::try_from(source.read_i64(i64::from(i32::MIN), i64::from(i32::MAX))?)
                            .expect("checked integer width"),
                    );
                }
                4 => {
                    end_reason = Some(
                        u8::try_from(source.read_u64(u64::from(u8::MAX))?)
                            .expect("checked integer width"),
                    );
                }
                5 => {
                    close_fault = Some(if source.is_null()? {
                        None
                    } else {
                        Some(CleanupFault::read(source)?)
                    });
                }
                _ => unreachable!("checked record index"),
            }
        }
        Ok(Self {
            ref_id: ref_id.expect("checked required field"),
            target: target.expect("checked required field"),
            reason: reason.expect("checked required field"),
            crash_kind: crash_kind.expect("checked required field"),
            end_reason: end_reason.expect("checked required field"),
            close_fault: close_fault.expect("checked required field"),
        })
    }
}
