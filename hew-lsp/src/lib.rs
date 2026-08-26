//! Hew Language Server Protocol implementation.
//!
//! Provides IDE features for the Hew programming language via LSP,
//! including diagnostics, completion, hover, document symbols, and
//! semantic token highlighting.

// `lsp-types` 0.97's `Uri` caches parsed data internally, which makes Clippy
// conservatively reject the protocol's mandated `HashMap<Uri, _>` fields. URI
// identity is immutable here: Hew never exposes mutable URI references and all
// keys originate from decoded LSP messages or freshly built file URIs.
#![allow(
    clippy::mutable_key_type,
    reason = "LSP protocol maps are keyed by immutable Uri values"
)]

pub mod server;
