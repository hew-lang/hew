//! Hew `std::net::http` — HTTP client and server.

#[cfg(test)]
extern crate hew_runtime; // Link hew_vec_* symbol implementations

pub mod client;
pub mod server;
