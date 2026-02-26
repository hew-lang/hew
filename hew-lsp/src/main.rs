use hew_lsp::server::HewLanguageServer;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    // Handle --version / -V before starting the LSP server
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|a| a == "--version" || a == "-V") {
        println!("hew-lsp {}", env!("CARGO_PKG_VERSION"));
        return;
    }

    let (service, socket) = LspService::new(HewLanguageServer::new);

    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket)
        .serve(service)
        .await;
}
