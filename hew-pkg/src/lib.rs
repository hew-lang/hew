//! The Hew package manager, consumed as a library by the `hew` CLI.
//!
//! Exposes the package-manager command surface (`cli`) plus the core types so
//! that integration tests in `tests/` can construct a `RegistryClient` and
//! interact with it against a mock server.

pub mod client;
pub mod config;
pub mod credentials;
pub mod happy_eyeballs;
pub mod index;
pub mod manifest;
pub mod native;
pub mod project;
pub mod registry;
pub mod resolver;
pub mod signing;
pub mod tarball;

pub mod cli;

mod atomic_fs;
mod checksum;
mod lockfile;
mod package_fs;
mod package_name;
mod paths;
