use tiny_http::{Header, Response, Server, StatusCode};

fn main() {
    let server = Server::http("0.0.0.0:18083").unwrap();
    println!("Listening on 0.0.0.0:18083");

    for request in server.incoming_requests() {
        let (status, body) = match request.url() {
            "/" => (200, "Hello from Rust!"),
            "/health" => (200, "OK"),
            _ => (404, "Not Found"),
        };
        let response = Response::from_string(body)
            .with_status_code(StatusCode(status))
            .with_header(
                Header::from_bytes("Content-Type", "text/plain; charset=utf-8").unwrap(),
            );
        let _ = request.respond(response);
    }
}
