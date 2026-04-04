# Benchmarks

This directory contains two distinct benchmark suites:

1. **Hew vs Go algorithm benchmarks** — 57 paired algorithm implementations in
   `hew/` and `go/`, driven by `run_benchmarks.sh`, with results in `results.csv`.
2. **HTTP server comparison examples** — minimal servers in four languages at the
   directory root, for hand-run, latency-oriented HTTP benchmarking.

---

## 1. Hew vs Go Algorithm Benchmarks

### Overview

`run_benchmarks.sh` compiles and times each Hew and Go algorithm implementation
side-by-side (Rust and Python are only in the HTTP server comparison below),
writing throughput measurements to `results.csv`.

```
hew/    57 Hew algorithm implementations  (bench_*.hew)
go/     57 matching Go implementations    (bench_*.go)
run_benchmarks.sh   runner script
results.csv         output from the latest local run (not a canonical source of truth)
```

**Algorithms covered** (57 total):

| Category | Benchmarks |
| -------- | ---------- |
| Sorting | `bubble_sort`, `cocktail_sort`, `counting_sort`, `dutch_flag`, `heap_sort`, `insertion_sort`, `merge_sort`, `quick_sort`, `radix_sort`, `selection_sort`, `shell_sort` |
| Searching | `binary_search`, `exponential_search`, `fibonacci_search`, `interpolation_search`, `jump_search`, `linear_search`, `sentinel_search`, `ternary_search` |
| Graph | `bellman_ford`, `bfs`, `detect_cycle`, `dfs`, `dijkstra`, `floyd_warshall`, `kruskal`, `topological_sort` |
| Dynamic programming | `climbing_stairs`, `coin_change`, `edit_distance`, `knapsack`, `lcs`, `lis`, `matrix_chain`, `max_subarray`, `rod_cutting` |
| Math / number theory | `catalan`, `factorial`, `fast_power`, `fibonacci`, `gcd`, `is_prime`, `lcm`, `matrix_multiply`, `newton_sqrt`, `pascal`, `sieve` |
| String / array | `caesar_cipher`, `hamming`, `palindrome`, `remove_dupes`, `rle`, `rotate_array`, `spiral_matrix`, `string_reverse`, `two_sum` |
| Misc | `merge_sorted` |

### Running

```bash
# From the repository root:
cd examples/benchmarks

# Run all benchmarks (requires hew compiler at target/release/hew and go in PATH)
./run_benchmarks.sh

# Run a filtered subset (matches on benchmark name)
./run_benchmarks.sh sort    # only sorting benchmarks
./run_benchmarks.sh search  # only search benchmarks
```

Results are written to `results.csv` with columns:
`algorithm, hew_seconds, go_seconds, hew_ops_sec, go_ops_sec, ratio, iters`

---

## 2. HTTP Server Comparison Examples

Minimal HTTP servers in four languages for comparative HTTP benchmarking.
All servers implement the same routing: `/` → 200, `/health` → 200, everything
else → 404.

### Files

#### Naive (Iteration 1)

| File              | Language | Port  | HTTP Library            |
| ----------------- | -------- | ----- | ----------------------- |
| `http_server.hew` | Hew      | 18080 | tiny_http (via runtime) |
| `http_server.rs`  | Rust      | 18083 | tiny_http 0.12          |
| `http_server.go`  | Go       | 18081 | net/http (stdlib)       |
| `http_server.py`  | Python   | 18082 | http.server (stdlib)    |

#### Expert (Iteration 2)

| File                     | Language | Port  | HTTP Library             |
| ------------------------ | -------- | ----- | ------------------------ |
| `http_server_expert.hew` | Hew      | 18080 | tiny_http (via runtime)  |
| `http_server_expert.rs`  | Rust      | 18083 | axum + tokio (async)     |
| `http_server_expert.go`  | Go       | 18081 | net/http + NewServeMux   |
| `http_server_expert.py`  | Python   | 18082 | http.server + match 3.10 |

### Building

```bash
cd examples/benchmarks

# Hew (naive or expert)
hew build http_server.hew -o http-hew
hew build http_server_expert.hew -o http-hew-expert

# Rust naive (tiny_http)
mkdir -p rust_http/src
cp http_server.rs rust_http/src/main.rs
cat > rust_http/Cargo.toml << 'EOF'
[package]
name = "rust-http-bench"
version = "0.1.0"
edition = "2021"
[dependencies]
tiny_http = "0.12"
EOF
cd rust_http && cargo build --release && cd ..

# Rust expert (axum)
mkdir -p rust_http_expert/src
cp http_server_expert.rs rust_http_expert/src/main.rs
cat > rust_http_expert/Cargo.toml << 'EOF'
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
cd rust_http_expert && cargo build --release && cd ..

# Go
go build -o http-go http_server.go
go build -o http-go-expert http_server_expert.go

# Python (no build needed)
```

### Running the HTTP benchmark

```bash
# Start servers (pick naive or expert set)
./http-hew &
./rust_http/target/release/rust-http-bench &
./http-go &
python3 http_server.py &

# Benchmark with siege (5 seconds, concurrency 25)
siege -c 25 -t 5S --no-parser http://127.0.0.1:18080/  # Hew
siege -c 25 -t 5S --no-parser http://127.0.0.1:18083/  # Rust
siege -c 25 -t 5S --no-parser http://127.0.0.1:18081/  # Go
siege -c 25 -t 5S --no-parser http://127.0.0.1:18082/  # Python
```
