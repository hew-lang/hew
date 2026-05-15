# std::iter — lazy vs eager filter+map+sum

Workload: `sum(map(add5, filter(even, 1..=100_000)))`. Expected result `2_500_300_000`.

| Variant | Result | Median (µs) | p99 (µs) | Min (µs) | Max (µs) | Stdev (µs) | Runs |
|---|---|---:|---:|---:|---:|---:|---:|
| eager | 2500300000 | 749 | 1006 | 710 | 1034 | 116.4 | 11 |
| lazy | 2500300000 | 429 | 509 | 396 | 580 | 52.4 | 11 |

**Lazy speedup:** 1.75× over eager (median over 11 runs).
