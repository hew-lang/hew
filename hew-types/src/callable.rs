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

/// How failure crosses a callable boundary. Native unwinding is not a Hew ABI.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallableFailureDomain {
    /// Recoverable Hew failure carried by the explicit status and fault result.
    HewFault,
    /// No recoverable failure crosses this boundary; a host failure is fatal.
    Abort,
}

impl CallableFailureDomain {
    /// Resolve a supported source foreign ABI without guessing from its symbol.
    #[must_use]
    pub fn for_extern_abi(abi: &str) -> Option<Self> {
        match abi {
            "C" | "rt" => Some(Self::Abort),
            _ => None,
        }
    }
}
