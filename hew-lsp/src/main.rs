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
        println!("    -h, --help       Print help information");
        println!("    -V, --version    Print version information");
        println!();
        println!("The LSP server communicates via stdin/stdout using the");
        println!("Language Server Protocol. Configure your editor to launch");
        println!("this binary as a language server for .hew files.");
        return;
    }

    let (service, socket) = LspService::new(HewLanguageServer::new);

    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket)
        .serve(service)
        .await;
}
