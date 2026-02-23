from http.server import HTTPServer, BaseHTTPRequestHandler


class Handler(BaseHTTPRequestHandler):
    def _respond(self, status: int, body: bytes, content_type: str = "text/plain; charset=utf-8") -> None:
        """Helper method to send HTTP response with proper headers."""
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self) -> None:
        """Handle GET requests using modern Python match statement routing."""
        match self.path:
            case "/":
                self._respond(200, b"Hello from Python!")
            case "/health":
                self._respond(200, b"OK")
            case _:
                self._respond(404, b"Not Found")

    def log_message(self, format: str, *args) -> None:
        """Suppress logging for benchmark mode."""
        pass


if __name__ == "__main__":
    server = HTTPServer(("0.0.0.0", 18082), Handler)
    print("Listening on 0.0.0.0:18082")
    server.serve_forever()