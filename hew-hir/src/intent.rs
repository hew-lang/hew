#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntentKind {
    Read,
    Modify,
    Consume,
    /// Run this affine value's terminal release ritual without transferring
    /// its storage. The ownership obligation is discharged exactly once, but
    /// subsequent non-consuming reads may inspect the closed handle state.
    Discharge,
    Capture,
    Yield,
    Unknown,
}
