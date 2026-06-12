# HashMap layout-kernel — Stage C0a baseline

Captured on the commit immediately after Stage C0a landed
(`83189329 — runtime(hashmap): extend Layout ABI with drop/clone thunks,
snapshot descriptors by value, wire drop hooks`).

These numbers exist so subsequent Stage C/D work that touches the
layout-kernel insert / remove / free path has a regression yardstick.
Re-run with:

```
cargo bench -p hew-runtime --bench hashmap_layout_kernel
```

Compare against the rows below. Criterion will itself emit
`change: [+/- x%]` once a previous run has populated `target/criterion/`,
but that local state is not durable across worktrees — these numbers are.

Host: macOS arm64 (Darwin). Native build, default `bench` profile
(`opt-level = 3`). Single-threaded driver. Criterion 0.5.

## insert_string_kv (K = string, V = string — exercises K + V drop_fn)

| N      | time (mean)   | throughput        |
|--------|---------------|-------------------|
|     1  |  91.86 ns     | 10.89 Melem/s     |
|    10  | 349.39 ns     | 28.62 Melem/s     |
|   100  |   6.63 µs     | 15.09 Melem/s     |
|  1 000 |  66.07 µs     | 15.14 Melem/s     |
| 10 000 | 797.88 µs     | 12.53 Melem/s     |

Per-iteration work: build a fresh map, insert N owned (K, V) string pairs
(`CString::into_raw` for each), then `hew_hashmap_free_layout` — which
invokes the `drop_fn` thunk for every stored K and V. Inputs are produced
in `iter_batched` setup so allocation of the source `CString`s is outside
the timed region; the timed region still includes the in-kernel slot
copies and the per-slot `drop_fn` calls at free.

## insert_remove_i64 (K = i64, V = i64, both Plain — no drop_fn)

| N      | time (mean)   | throughput        |
|--------|---------------|-------------------|
|  1 000 |  19.55 µs     | 51.15 Melem/s     |
| 10 000 | 170.49 µs     | 58.66 Melem/s     |

Per-iteration work: fresh map → insert N → remove N → free. Plain
ownership exercises the drop-skip fast path in both the per-slot remove
and the bulk free loop. Throughput is reported per element (insert+remove
counted once each gives ~2N operations per N elements).

## free_string_kv (K = string, V = string — isolated free cost)

| N      | time (mean)   | throughput        |
|--------|---------------|-------------------|
|  1 000 |  22.86 µs     | 43.75 Melem/s     |
| 10 000 | 205.23 µs     | 48.73 Melem/s     |

Per-iteration work: `iter_batched` setup builds the populated map; the
timed region is exactly the `hew_hashmap_free_layout` call — iteration
across the open-address table plus a `drop_fn` invocation for every
occupied K and V.

## insert_overwrite_string_v (K = string, V = string — exercises drop-old-V hook)

| N      | time (mean)   | throughput        |
|--------|---------------|-------------------|
|  1 000 |  56.60 µs     | 17.67 Melem/s     |

Per-iteration work: `iter_batched` setup pre-populates a map of N (K, V)
string pairs and pre-allocates N fresh `V_new` strings + N duplicate
`K_in` lookup strings. The timed region performs N overwrite-insert calls
— each fires the V `drop_fn` on the previously-stored V (the
occupied-slot drop site). Post-timed teardown frees the caller-owned
duplicate K_in strings and the map.

## remove_string_kv (K = string, V = string — exercises per-slot remove drop)

| N      | time (mean)   | throughput        |
|--------|---------------|-------------------|
|  1 000 |  41.82 µs     | 23.91 Melem/s     |

Per-iteration work: `iter_batched` setup pre-populates a map of N (K, V)
string pairs and pre-allocates N lookup K strings. The timed region calls
`hew_hashmap_remove_layout` for each of the N keys — each fires both the
K and V `drop_fn` (per-slot remove drop site, the third site C0a added
beyond insert-overwrite and free). Post-timed teardown frees the caller-
owned lookup strings and the now-empty map.

## Drop-hook coverage matrix

| Drop site                | Bench group                       |
|--------------------------|-----------------------------------|
| Insert (overwrite, V)    | `insert_overwrite_string_v`       |
| Remove (per-slot, K + V) | `remove_string_kv`                |
| Free (bulk, K + V)       | `free_string_kv`, `insert_string_kv` (free tail) |
| Drop-skip fast path      | `insert_remove_i64`               |

## Regression interpretation

A Stage C/D commit is suspect if any row's mean increases by more than
~10 % outside of the criterion-reported confidence interval. Sub-10 %
swings are within noise for these short-duration benches on a shared
laptop — re-run before raising a flag.
