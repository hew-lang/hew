//! Failures produced by semantic lowering, before a body can be published.

/// A source ownership rule enforced by SIR's place and loan facts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SirOwnershipError {
    ConsumeBorrowed,
    GuardConsume,
    PartialConsume,
}

impl SirOwnershipError {
    #[must_use]
    pub const fn code(self) -> &'static str {
        match self {
            Self::ConsumeBorrowed => "E_OWN_CONSUME_BORROWED",
            Self::GuardConsume => "E_OWN_GUARD_CONSUME",
            Self::PartialConsume => "E_OWN_PARTIAL_CONSUME",
        }
    }
}

/// The producer's classification survives every lowering call and the driver
/// boundary. Drivers must not recover a category from diagnostic text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SirLoweringError {
    /// Checked HIR or an intermediate semantic contract is incomplete.
    Internal(String),
    /// SIR's ownership facts reject the source operation.
    Ownership {
        kind: SirOwnershipError,
        message: String,
    },
    /// An admitted operation still needs an implementation.
    Unavailable(String),
    /// A finite compiler resource budget was exhausted.
    Limit(String),
}

impl SirLoweringError {
    #[must_use]
    pub fn with_context(self, context: &str) -> Self {
        let message = format!("{context}: {}", self.message());
        match self {
            Self::Internal(_) => Self::Internal(message),
            Self::Ownership { kind, .. } => Self::Ownership { kind, message },
            Self::Unavailable(_) => Self::Unavailable(message),
            Self::Limit(_) => Self::Limit(message),
        }
    }

    #[must_use]
    pub fn message(&self) -> &str {
        match self {
            Self::Internal(message)
            | Self::Unavailable(message)
            | Self::Limit(message)
            | Self::Ownership { message, .. } => message,
        }
    }
}

impl From<String> for SirLoweringError {
    fn from(message: String) -> Self {
        Self::Internal(message)
    }
}

impl From<&str> for SirLoweringError {
    fn from(message: &str) -> Self {
        Self::Internal(message.to_owned())
    }
}

impl std::fmt::Display for SirLoweringError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Self::Ownership { kind, .. } = self {
            write!(formatter, "{}: ", kind.code())?;
        }
        formatter.write_str(self.message())
    }
}

impl std::error::Error for SirLoweringError {}
