//! Hew Language Server Protocol implementation.
//!
//! Provides IDE features for the Hew programming language via LSP,
//! including diagnostics, completion, hover, document symbols, and
//! semantic token highlighting.

// `ls-types`' `Uri` caches parsed data internally, which makes Clippy
// conservatively reject the protocol's mandated `HashMap<Uri, _>` fields. URI
// identity is immutable here: Hew never exposes mutable URI references and all
// keys originate from decoded LSP messages or freshly built file URIs.
#![allow(
    clippy::mutable_key_type,
    reason = "LSP protocol maps are keyed by immutable Uri values"
)]

pub mod server;

use std::path::PathBuf;

use server::HewLanguageServer;
use tower_lsp_server::{LspService, Server};

/// Options for `hew lsp`, sharing package discovery with `hew check`.
#[derive(Debug, clap::Args)]
pub struct Options {
    /// Package search directory (mirrors `hew check --pkg-path`).
    #[arg(long, value_name = "DIR", value_parser = package_path)]
    pkg_path: Option<PathBuf>,
}

fn package_path(value: &str) -> Result<PathBuf, String> {
    if value.is_empty() {
        Err("--pkg-path requires a directory".into())
    } else {
        Ok(PathBuf::from(value))
    }
}

/// Serve the Language Server Protocol over stdin/stdout until client shutdown.
///
/// # Errors
/// Returns an error if the asynchronous runtime cannot be initialized.
pub fn run(options: &Options) -> std::io::Result<()> {
    let extra_pkg_paths: Vec<_> = options.pkg_path.iter().cloned().collect();
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()?;
    runtime.block_on(async move {
        let (service, socket) = LspService::build(move |client| {
            HewLanguageServer::new_with_options(client, extra_pkg_paths.clone())
        })
        .custom_method("hew/tests", HewLanguageServer::tests)
        .custom_method("hew/expandMacro", HewLanguageServer::expand_macro)
        .finish();

        Server::new(tokio::io::stdin(), tokio::io::stdout(), socket)
            .serve(service)
            .await;
    });
    Ok(())
}
