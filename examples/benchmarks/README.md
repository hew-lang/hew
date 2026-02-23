# HTTP Server Benchmarks

Minimal HTTP servers in four languages for comparative benchmarking.
All servers implement identical routing logic: `/` → 200, `/health` → 200, `_` → 404.

## Files

### Naive (Iteration 1)

| File              | Language | Port  | HTTP Library            |
| ----------------- | -------- | ----- | ----------------------- |
| `http_server.hew` | Hew      | 18080 | tiny_http (via runtime) |
| `http_server.rs`  | Rust     | 18083 | tiny_http 0.12          |
| `http_server.go`  | Go       | 18081 | net/http (stdlib)       |
| `http_server.py`  | Python   | 18082 | http.server (stdlib)    |

### Expert (Iteration 2)

| File                     | Language | Port  | HTTP Library             |
| ------------------------ | -------- | ----- | ------------------------ |
| `http_server_expert.hew` | Hew      | 18080 | tiny_http (via runtime)  |
| `http_server_expert.rs`  | Rust     | 18083 | axum + tokio (async)     |
| `http_server_expert.go`  | Go       | 18081 | net/http + NewServeMux   |
| `http_server_expert.py`  | Python   | 18082 | http.server + match 3.10 |

## Building

```bash
# Hew (naive or expert)
cargo run -p hew-cli -- build examples/benchmarks/http_server.hew -o /tmp/http_hew
cargo run -p hew-cli -- build examples/benchmarks/http_server_expert.hew -o /tmp/http_hew_expert

# Rust naive (tiny_http)
mkdir -p /tmp/rust_http/src
cp examples/benchmarks/http_server.rs /tmp/rust_http/src/main.rs
cat > /tmp/rust_http/Cargo.toml << 'EOF'
[package]
name = "rust-http-bench"
version = "0.1.0"
edition = "2021"
[dependencies]
tiny_http = "0.12"
EOF
cd /tmp/rust_http && cargo build --release

# Rust expert (axum)
mkdir -p /tmp/rust_http_expert/src
cp examples/benchmarks/http_server_expert.rs /tmp/rust_http_expert/src/main.rs
cat > /tmp/rust_http_expert/Cargo.toml << 'EOF'
[package]
name = "rust-http-expert"
version = "0.1.0"
edition = "2021"
[dependencies]
axum = "0.8"
tokio = { version = "1", features = ["full"] }
tracing = "0.1"
tracing-subscriber = "0.3"
EOF
cd /tmp/rust_http_expert && cargo build --release

# Go
go build -o /tmp/http_go examples/benchmarks/http_server.go
go build -o /tmp/http_go_expert examples/benchmarks/http_server_expert.go

# Python (no build needed)
```

## Running the benchmark

```bash
# Start servers (pick naive or expert)
/tmp/http_hew &
/tmp/rust_http/target/release/rust-http-bench &
/tmp/http_go &
python3 examples/benchmarks/http_server.py &

# Benchmark with siege (5 seconds, concurrency 25)
siege -c 25 -t 5S --no-parser http://127.0.0.1:18080/  # Hew
siege -c 25 -t 5S --no-parser http://127.0.0.1:18083/  # Rust
siege -c 25 -t 5S --no-parser http://127.0.0.1:18081/  # Go
siege -c 25 -t 5S --no-parser http://127.0.0.1:18082/  # Python
```

See `docs/2026-02-22-http-server-benchmarks.md` for full results.
