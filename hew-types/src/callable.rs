//! Checker-owned callable and capture contracts.

/// How a closure acquires ownership of one captured binding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClosureCaptureAcquisition {
    Snapshot,
    Move,
}

/// Whether the environment field permits private mutation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClosureCaptureAccess {
    Read,
    Var,
}

/// Whether invocation can consume ownership from the environment field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClosureCaptureConsumption {
    Retained,
    Consumed,
}
