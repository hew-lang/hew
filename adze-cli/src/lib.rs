//! Library re-exports for integration testing.
//!
//! The `adze` binary lives in `main.rs`; this module exposes the core types
//! so that integration tests in `tests/` can construct a `RegistryClient` and
//! interact with it against a mock server.

pub mod client;
pub mod config;
pub mod index;
pub mod manifest;
pub mod registry;
pub mod resolver;
