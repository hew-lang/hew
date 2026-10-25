//! Prepared native execution for trusted, same-build Hew hosts.
//!
//! This crate deliberately does not link `hew-runtime`. A host loads one
//! compatible shared runtime, then imports it from every code generation.
//! Native code is trusted; these checks are compatibility checks, not a sandbox.

pub mod native;
pub mod session;
