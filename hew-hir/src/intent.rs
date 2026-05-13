#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntentKind {
    Read,
    Modify,
    Consume,
    Capture,
    Yield,
    Unknown,
}
