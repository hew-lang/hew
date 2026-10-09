//! Status codes for the native Node lifecycle boundary.

/// Failure returned by a Node lifecycle operation; success is zero.
#[repr(i32)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeFailure {
    /// The supplied configuration is invalid.
    Config = -1,
    /// The local identity or a configured peer key is invalid.
    Key = -2,
    /// The peer could not be reached.
    Unreachable = -3,
    /// The node or peer refused the operation.
    Refused = -4,
}
