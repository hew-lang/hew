use std::path::PathBuf;

use hew_lsp::server::HewLanguageServer;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    // Handle --version / --help before starting the LSP server
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|a| a == "--version" || a == "-V") {
        println!("hew-lsp {}", env!("CARGO_PKG_VERSION"));
        return;
    }
    if args.iter().any(|a| a == "--help" || a == "-h") {
        println!("hew-lsp {}", env!("CARGO_PKG_VERSION"));
        println!();
        println!("Hew Language Server Protocol implementation.");
        println!();
        println!("USAGE:");
        println!("    hew-lsp [OPTIONS]");
        println!();
        println!("OPTIONS:");
        println!("    -h, --help               Print help information");
        println!("    -V, --version            Print version information");
        println!(
            "    --pkg-path <DIR>         Package search directory (mirrors hew check --pkg-path)"
        );
        println!();
        println!("The LSP server communicates via stdin/stdout using the");
        println!("Language Server Protocol. Configure your editor to launch");
        println!("this binary as a language server for .hew files.");
        return;
    }

    // Collect extra package-search paths from `--pkg-path DIR` arguments.
    // Mirrors `hew check --pkg-path`: appended to the default module search
    // roots so that `hew::pkg` imports resolve the same way in the editor.
    let mut extra_pkg_paths: Vec<PathBuf> = Vec::new();
    let mut arg_iter = args.iter().skip(1).peekable();
    while let Some(arg) = arg_iter.next() {
        if arg == "--pkg-path" {
            if let Some(path_str) = arg_iter.next() {
                extra_pkg_paths.push(PathBuf::from(path_str));
            }
        } else if let Some(path_str) = arg.strip_prefix("--pkg-path=") {
            extra_pkg_paths.push(PathBuf::from(path_str));
        }
    }

    let (service, socket) = LspService::new(move |client| {
        HewLanguageServer::new_with_options(client, extra_pkg_paths.clone())
    });

    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket)
        .serve(service)
        .await;
}
