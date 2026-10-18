# Calibration run

Execution is timed inside each workload. Process RSS includes runtime startup. Latency tails pool the recorded operations; throughput ranges describe repeated process runs.

| Workload        | Iterations |  Size | Workers | Implementation | Median batch ms |  Batch IQR ms | p50 / p95 / p99 µs       | Peak RSS MiB |
| --------------- | ---------: | ----: | ------: | -------------- | --------------: | ------------: | ------------------------ | -----------: |
| scalar          |     100000 |     1 |       1 | go             |            0.25 |     0.24–0.25 | —                        |          1.8 |
| scalar          |     100000 |     1 |       1 | hew            |            0.26 |     0.26–0.27 | —                        |          2.8 |
| scalar          |     100000 |     1 |       1 | rust           |            0.22 |     0.22–0.22 | —                        |          2.1 |
| scalar          |     100000 |     1 |       1 | hew-o0         |            0.47 |     0.47–0.52 | —                        |          3.0 |
| scalar          |     100000 |     1 |       1 | c              |            0.22 |     0.22–0.23 | —                        |          1.7 |
| scalar          |     100000 |     1 |       1 | rust-checked   |            0.23 |     0.22–0.23 | —                        |          2.1 |
| scalar          |    1000000 |     1 |       1 | hew-o0         |            4.78 |     4.74–4.83 | —                        |          3.0 |
| scalar          |    1000000 |     1 |       1 | rust-checked   |            2.22 |     2.22–2.24 | —                        |          2.2 |
| scalar          |    1000000 |     1 |       1 | go             |            2.49 |     2.47–2.50 | —                        |          1.8 |
| scalar          |    1000000 |     1 |       1 | rust           |            2.24 |     2.22–2.26 | —                        |          2.2 |
| scalar          |    1000000 |     1 |       1 | c              |            2.23 |     2.21–2.25 | —                        |          1.9 |
| scalar          |    1000000 |     1 |       1 | hew            |            2.64 |     2.64–2.65 | —                        |          2.9 |
| vector_scan     |      15625 |    64 |       1 | c              |            0.08 |     0.08–0.08 | —                        |          1.9 |
| vector_scan     |      15625 |    64 |       1 | rust           |            0.08 |     0.08–0.08 | —                        |          2.1 |
| vector_scan     |      15625 |    64 |       1 | go             |            0.20 |     0.20–0.21 | —                        |          1.8 |
| vector_scan     |      15625 |    64 |       1 | hew-o0         |            5.11 |     5.07–5.13 | —                        |          3.0 |
| vector_scan     |      15625 |    64 |       1 | rust-checked   |            0.20 |     0.20–0.20 | —                        |          2.2 |
| vector_scan     |      15625 |    64 |       1 | hew            |            0.58 |     0.58–0.59 | —                        |          2.9 |
| vector_build    |       4096 |    64 |       1 | go             |            2.14 |     2.09–2.23 | —                        |          5.8 |
| vector_build    |       4096 |    64 |       1 | hew            |            2.03 |     2.02–2.05 | —                        |          2.7 |
| vector_build    |       4096 |    64 |       1 | rust           |            0.47 |     0.46–0.48 | —                        |          2.2 |
| vector_build    |       4096 |    64 |       1 | hew-o0         |            4.16 |     4.13–4.24 | —                        |          3.0 |
| vector_build    |       4096 |    64 |       1 | rust-checked   |            0.53 |     0.52–0.55 | —                        |          2.2 |
| vector_build    |       4096 |    64 |       1 | c              |            0.33 |     0.33–0.34 | —                        |          1.7 |
| bytes_shared    |     100000 |    64 |       1 | hew-o0         |            1.36 |     1.35–1.38 | —                        |          3.0 |
| bytes_shared    |     100000 |    64 |       1 | go             |            0.14 |     0.13–0.14 | —                        |          1.8 |
| bytes_shared    |     100000 |    64 |       1 | rust-checked   |            0.26 |     0.25–0.27 | —                        |          2.2 |
| bytes_shared    |     100000 |    64 |       1 | hew            |            0.30 |     0.30–0.30 | —                        |          2.9 |
| bytes_shared    |     100000 |    64 |       1 | c              |            0.29 |     0.29–0.29 | —                        |          1.9 |
| bytes_shared    |     100000 |    64 |       1 | rust           |            0.25 |     0.25–0.26 | —                        |          2.1 |
| bytes_cow       |      16384 |    64 |       1 | rust           |            0.15 |     0.15–0.16 | —                        |          2.0 |
| bytes_cow       |      16384 |    64 |       1 | hew-o0         |            0.66 |     0.66–0.70 | —                        |          3.0 |
| bytes_cow       |      16384 |    64 |       1 | hew            |            0.28 |     0.27–0.29 | —                        |          2.7 |
| bytes_cow       |      16384 |    64 |       1 | c              |            0.10 |     0.10–0.11 | —                        |          1.7 |
| bytes_cow       |      16384 |    64 |       1 | go             |            0.56 |     0.55–0.57 | —                        |          2.8 |
| bytes_cow       |      16384 |    64 |       1 | rust-checked   |            0.15 |     0.15–0.15 | —                        |          2.2 |
| vector_scan     |        244 |  4096 |       1 | hew-o0         |            5.09 |     5.07–5.14 | —                        |          3.0 |
| vector_scan     |        244 |  4096 |       1 | hew            |            0.57 |     0.57–0.63 | —                        |          2.9 |
| vector_scan     |        244 |  4096 |       1 | c              |            0.05 |     0.05–0.05 | —                        |          1.9 |
| vector_scan     |        244 |  4096 |       1 | rust-checked   |            0.19 |     0.19–0.20 | —                        |          2.1 |
| vector_scan     |        244 |  4096 |       1 | go             |            0.24 |     0.24–0.25 | —                        |          1.8 |
| vector_scan     |        244 |  4096 |       1 | rust           |            0.05 |     0.05–0.06 | —                        |          2.1 |
| vector_build    |         64 |  4096 |       1 | hew            |            1.82 |     1.82–1.87 | —                        |          2.9 |
| vector_build    |         64 |  4096 |       1 | hew-o0         |            3.81 |     3.79–3.92 | —                        |          3.0 |
| vector_build    |         64 |  4096 |       1 | rust-checked   |            0.29 |     0.29–0.30 | —                        |          2.1 |
| vector_build    |         64 |  4096 |       1 | go             |            2.11 |     2.09–2.19 | —                        |          6.3 |
| vector_build    |         64 |  4096 |       1 | c              |            0.25 |     0.24–0.30 | —                        |          1.7 |
| vector_build    |         64 |  4096 |       1 | rust           |            0.20 |     0.20–0.21 | —                        |          2.2 |
| bytes_shared    |     100000 |  4096 |       1 | rust-checked   |            0.26 |     0.25–0.28 | —                        |          2.2 |
| bytes_shared    |     100000 |  4096 |       1 | hew            |            0.30 |     0.30–0.30 | —                        |          2.9 |
| bytes_shared    |     100000 |  4096 |       1 | go             |            0.14 |     0.13–0.15 | —                        |          1.8 |
| bytes_shared    |     100000 |  4096 |       1 | rust           |            0.26 |     0.25–0.27 | —                        |          2.2 |
| bytes_shared    |     100000 |  4096 |       1 | hew-o0         |            1.37 |     1.36–1.40 | —                        |          3.0 |
| bytes_shared    |     100000 |  4096 |       1 | c              |            0.29 |     0.28–0.30 | —                        |          1.9 |
| bytes_cow       |        256 |  4096 |       1 | hew            |            0.02 |     0.02–0.02 | —                        |          3.0 |
| bytes_cow       |        256 |  4096 |       1 | go             |            0.36 |     0.36–0.37 | —                        |          2.8 |
| bytes_cow       |        256 |  4096 |       1 | hew-o0         |            0.02 |     0.02–0.02 | —                        |          3.0 |
| bytes_cow       |        256 |  4096 |       1 | rust-checked   |            0.01 |     0.01–0.01 | —                        |          2.0 |
| bytes_cow       |        256 |  4096 |       1 | rust           |            0.01 |     0.01–0.01 | —                        |          2.2 |
| bytes_cow       |        256 |  4096 |       1 | c              |            0.01 |     0.01–0.01 | —                        |          1.7 |
| vector_scan     |         15 | 65536 |       1 | hew-o0         |            5.03 |     4.98–5.04 | —                        |          3.7 |
| vector_scan     |         15 | 65536 |       1 | c              |            0.05 |     0.05–0.06 | —                        |          2.2 |
| vector_scan     |         15 | 65536 |       1 | rust-checked   |            0.19 |     0.19–0.19 | —                        |          2.5 |
| vector_scan     |         15 | 65536 |       1 | hew            |            0.61 |     0.56–0.62 | —                        |          3.5 |
| vector_scan     |         15 | 65536 |       1 | rust           |            0.05 |     0.05–0.06 | —                        |          2.6 |
| vector_scan     |         15 | 65536 |       1 | go             |            0.21 |     0.20–0.21 | —                        |          2.3 |
| vector_build    |          8 | 65536 |       1 | c              |            0.78 |     0.76–0.79 | —                        |          2.2 |
| vector_build    |          8 | 65536 |       1 | hew-o0         |            9.72 |    9.66–10.77 | —                        |          3.8 |
| vector_build    |          8 | 65536 |       1 | go             |            2.72 |     2.68–2.75 | —                        |          6.6 |
| vector_build    |          8 | 65536 |       1 | rust           |            0.69 |     0.68–0.99 | —                        |          2.9 |
| vector_build    |          8 | 65536 |       1 | hew            |            5.79 |     5.75–5.87 | —                        |          3.8 |
| vector_build    |          8 | 65536 |       1 | rust-checked   |            0.87 |     0.86–0.88 | —                        |          2.9 |
| bytes_shared    |     100000 | 65536 |       1 | rust-checked   |            0.28 |     0.28–0.29 | —                        |          2.2 |
| bytes_shared    |     100000 | 65536 |       1 | c              |            0.30 |     0.29–0.35 | —                        |          1.9 |
| bytes_shared    |     100000 | 65536 |       1 | rust           |            0.28 |     0.28–0.29 | —                        |          2.2 |
| bytes_shared    |     100000 | 65536 |       1 | hew-o0         |            1.38 |     1.36–1.42 | —                        |          3.3 |
| bytes_shared    |     100000 | 65536 |       1 | go             |            0.14 |     0.14–0.14 | —                        |          1.8 |
| bytes_shared    |     100000 | 65536 |       1 | hew            |            0.30 |     0.30–0.31 | —                        |          3.1 |
| bytes_cow       |         16 | 65536 |       1 | rust           |            0.03 |     0.03–0.03 | —                        |          2.5 |
| bytes_cow       |         16 | 65536 |       1 | hew            |            0.04 |     0.04–0.04 | —                        |          3.0 |
| bytes_cow       |         16 | 65536 |       1 | go             |            0.33 |     0.32–0.46 | —                        |          2.8 |
| bytes_cow       |         16 | 65536 |       1 | hew-o0         |            0.04 |     0.03–0.04 | —                        |          3.2 |
| bytes_cow       |         16 | 65536 |       1 | rust-checked   |            0.03 |     0.03–0.04 | —                        |          2.5 |
| bytes_cow       |         16 | 65536 |       1 | c              |            0.03 |     0.03–0.03 | —                        |          1.9 |
| task_ready      |       4096 |     1 |       1 | hew            |           25.63 |   22.05–28.28 | —                        |          5.2 |
| task_ready      |       4096 |     1 |       1 | hew-o0         |           25.77 |   22.27–26.66 | —                        |          5.5 |
| task_ready      |       4096 |     1 |       1 | tokio          |           19.13 |   15.19–22.78 | —                        |          3.3 |
| task_ready      |       4096 |     1 |       1 | beam           |            3.63 |     3.60–3.65 | —                        |         57.1 |
| task_ready      |       4096 |     1 |       1 | go             |            1.21 |     1.19–1.24 | —                        |          2.6 |
| task_ready      |       4096 |     1 |       2 | go             |            1.21 |     1.15–1.34 | —                        |          2.6 |
| task_ready      |       4096 |     1 |       2 | beam           |            4.30 |     4.10–4.33 | —                        |         58.3 |
| task_ready      |       4096 |     1 |       2 | hew-o0         |           26.26 |   26.21–26.51 | —                        |          5.5 |
| task_ready      |       4096 |     1 |       2 | tokio          |           25.69 |   23.99–27.92 | —                        |          3.2 |
| task_ready      |       4096 |     1 |       2 | hew            |           28.87 |   25.90–30.51 | —                        |          5.3 |
| actor_fanout    |        256 |     1 |       1 | hew-o0         |            0.31 |     0.31–0.31 | —                        |          4.4 |
| actor_fanout    |        256 |     1 |       1 | hew            |            0.28 |     0.28–0.29 | —                        |          4.2 |
| actor_fanout    |        256 |     1 |       1 | go             |            0.05 |     0.05–0.05 | —                        |          1.8 |
| actor_fanout    |        256 |     1 |       1 | beam           |            0.09 |     0.09–0.09 | —                        |         57.3 |
| actor_fanout    |        256 |     1 |       1 | tokio          |            0.04 |     0.04–0.05 | —                        |          3.3 |
| actor_fanout    |        256 |     1 |       2 | hew            |            0.47 |     0.46–0.58 | —                        |          4.4 |
| actor_fanout    |        256 |     1 |       2 | beam           |            0.08 |     0.07–0.08 | —                        |         58.1 |
| actor_fanout    |        256 |     1 |       2 | hew-o0         |            0.56 |     0.50–0.61 | —                        |          4.7 |
| actor_fanout    |        256 |     1 |       2 | go             |            0.04 |     0.04–0.05 | —                        |          1.8 |
| actor_fanout    |        256 |     1 |       2 | tokio          |            0.06 |     0.04–0.08 | —                        |          3.2 |
| task_ready      |       4096 |    16 |       1 | beam           |            2.50 |     2.47–2.52 | —                        |         57.3 |
| task_ready      |       4096 |    16 |       1 | hew-o0         |            6.42 |     5.64–9.71 | —                        |          5.5 |
| task_ready      |       4096 |    16 |       1 | hew            |            8.97 |     6.95–9.94 | —                        |          5.4 |
| task_ready      |       4096 |    16 |       1 | tokio          |           14.67 |    6.05–14.80 | —                        |          3.3 |
| task_ready      |       4096 |    16 |       1 | go             |            1.31 |     1.25–1.38 | —                        |          2.6 |
| task_ready      |       4096 |    16 |       2 | hew-o0         |           14.59 |    9.79–18.65 | —                        |          5.5 |
| task_ready      |       4096 |    16 |       2 | tokio          |            3.66 |     3.60–3.69 | —                        |          3.3 |
| task_ready      |       4096 |    16 |       2 | go             |            1.33 |     1.24–1.48 | —                        |          2.8 |
| task_ready      |       4096 |    16 |       2 | hew            |           16.17 |   15.35–16.55 | —                        |          5.4 |
| task_ready      |       4096 |    16 |       2 | beam           |            4.26 |     3.17–4.32 | —                        |         61.1 |
| actor_fanout    |        256 |    16 |       1 | tokio          |            0.49 |     0.44–0.56 | —                        |          3.2 |
| actor_fanout    |        256 |    16 |       1 | beam           |            1.26 |     1.24–1.31 | —                        |         57.3 |
| actor_fanout    |        256 |    16 |       1 | hew-o0         |            4.70 |     4.63–4.79 | —                        |          4.2 |
| actor_fanout    |        256 |    16 |       1 | hew            |            4.44 |     4.41–4.48 | —                        |          4.2 |
| actor_fanout    |        256 |    16 |       1 | go             |            0.72 |     0.70–0.76 | —                        |          1.8 |
| actor_fanout    |        256 |    16 |       2 | hew-o0         |            3.81 |     3.75–5.82 | —                        |          4.5 |
| actor_fanout    |        256 |    16 |       2 | hew            |            4.15 |     3.68–4.86 | —                        |          4.2 |
| actor_fanout    |        256 |    16 |       2 | beam           |            1.63 |     1.36–1.76 | —                        |         57.9 |
| actor_fanout    |        256 |    16 |       2 | tokio          |            0.28 |     0.25–0.45 | —                        |          3.2 |
| actor_fanout    |        256 |    16 |       2 | go             |            0.70 |     0.43–0.71 | —                        |          2.1 |
| task_ready      |       4096 |    64 |       1 | beam           |            2.77 |     2.75–2.80 | —                        |         57.3 |
| task_ready      |       4096 |    64 |       1 | hew            |            7.16 |     6.58–7.32 | —                        |          5.4 |
| task_ready      |       4096 |    64 |       1 | tokio          |           11.45 |    2.61–11.61 | —                        |          3.2 |
| task_ready      |       4096 |    64 |       1 | go             |            1.32 |     1.30–1.38 | —                        |          2.6 |
| task_ready      |       4096 |    64 |       1 | hew-o0         |            7.47 |     4.57–7.84 | —                        |          5.7 |
| task_ready      |       4096 |    64 |       2 | hew-o0         |           10.16 |    9.98–13.03 | —                        |          5.5 |
| task_ready      |       4096 |    64 |       2 | beam           |            4.55 |     3.66–4.56 | —                        |         61.2 |
| task_ready      |       4096 |    64 |       2 | go             |            1.46 |     1.35–1.51 | —                        |          2.8 |
| task_ready      |       4096 |    64 |       2 | tokio          |            1.81 |     1.74–1.88 | —                        |          3.3 |
| task_ready      |       4096 |    64 |       2 | hew            |           10.85 |    9.91–11.99 | —                        |          5.4 |
| actor_fanout    |        256 |    64 |       1 | hew-o0         |           19.01 |   18.47–30.70 | —                        |          4.5 |
| actor_fanout    |        256 |    64 |       1 | hew            |           17.75 |   17.62–18.21 | —                        |          4.2 |
| actor_fanout    |        256 |    64 |       1 | go             |            3.22 |     2.83–4.26 | —                        |          2.1 |
| actor_fanout    |        256 |    64 |       1 | tokio          |            1.94 |     1.70–2.95 | —                        |          3.5 |
| actor_fanout    |        256 |    64 |       1 | beam           |            5.11 |     5.05–9.27 | —                        |         57.6 |
| actor_fanout    |        256 |    64 |       2 | hew            |           19.87 |   18.40–20.59 | —                        |          4.5 |
| actor_fanout    |        256 |    64 |       2 | tokio          |            1.81 |     1.20–2.69 | —                        |          3.5 |
| actor_fanout    |        256 |    64 |       2 | beam           |            5.57 |     5.08–6.75 | —                        |         58.8 |
| actor_fanout    |        256 |    64 |       2 | hew-o0         |           18.16 |   15.20–20.29 | —                        |          4.5 |
| actor_fanout    |        256 |    64 |       2 | go             |            2.78 |     1.76–2.99 | —                        |          2.3 |
| actor_roundtrip |       1000 |     1 |       1 | beam           |            0.38 |     0.37–0.39 | 0.3 / 0.7 / 1.3          |         57.7 |
| actor_roundtrip |       1000 |     1 |       1 | tokio          |            3.74 |     3.73–5.80 | 3.8 / 6.0 / 6.7          |          3.3 |
| actor_roundtrip |       1000 |     1 |       1 | hew-o0         |            4.81 |     4.76–4.87 | 4.5 / 5.2 / 9.4          |          4.7 |
| actor_roundtrip |       1000 |     1 |       1 | go             |            0.25 |     0.23–0.26 | 0.2 / 0.2 / 0.2          |          1.8 |
| actor_roundtrip |       1000 |     1 |       1 | hew            |            4.80 |     4.70–4.89 | 4.8 / 6.5 / 9.4          |          4.4 |
| actor_roundtrip |       1000 |     1 |       2 | beam           |            0.35 |     0.34–0.42 | 0.3 / 0.4 / 1.3          |         58.4 |
| actor_roundtrip |       1000 |     1 |       2 | hew-o0         |            7.13 |     4.87–7.68 | 6.3 / 13.2 / 13.9        |          4.5 |
| actor_roundtrip |       1000 |     1 |       2 | go             |            0.28 |     0.25–0.28 | 0.2 / 0.2 / 0.3          |          2.1 |
| actor_roundtrip |       1000 |     1 |       2 | tokio          |            4.23 |     3.92–7.34 | 3.8 / 10.0 / 11.9        |          3.3 |
| actor_roundtrip |       1000 |     1 |       2 | hew            |            4.88 |     4.86–4.91 | 4.8 / 6.8 / 10.3         |          4.5 |
| timer           |        100 |     1 |       1 | beam           |          199.98 | 199.84–201.20 | 999.9 / 1009.7 / 1013.7  |         57.4 |
| timer           |        100 |     1 |       1 | hew            |          106.61 | 106.35–108.63 | 62.6 / 74.3 / 247.8      |          4.2 |
| timer           |        100 |     1 |       1 | hew-o0         |          108.19 | 107.00–112.16 | 62.6 / 77.3 / 836.5      |          4.2 |
| timer           |        100 |     1 |       1 | go             |          106.12 | 105.50–106.47 | 55.0 / 60.9 / 131.7      |          1.8 |
| timer           |        100 |     1 |       1 | tokio          |          207.24 | 205.87–209.33 | 1055.6 / 1092.7 / 1997.5 |          3.2 |
| timer           |        100 |     1 |       2 | beam           |          199.71 | 199.41–199.84 | 999.9 / 1003.4 / 1063.4  |         58.4 |
| timer           |        100 |     1 |       2 | tokio          |          205.72 | 205.59–207.21 | 1054.8 / 1060.2 / 1110.4 |          3.3 |
| timer           |        100 |     1 |       2 | go             |          105.67 | 105.57–107.78 | 56.9 / 63.2 / 105.0      |          1.8 |
| timer           |        100 |     1 |       2 | hew-o0         |          106.67 | 106.05–110.01 | 62.7 / 70.3 / 434.3      |          4.5 |
| timer           |        100 |     1 |       2 | hew            |          106.55 | 106.34–109.64 | 63.0 / 74.8 / 266.6      |          4.2 |

## Application builds

First means the first invocation in this run, with existing output and machine caches. Rebuild invokes the ordinary build tool again on unchanged source. Go/Cargo cache hits and direct compiler invocations have different behaviour.

| Fixture     | Implementation | First ms | Rebuild median ms |
| ----------- | -------------- | -------: | ----------------: |
| primitives  | hew-o0         |      464 |               436 |
| primitives  | hew            |      575 |               578 |
| primitives  | rust           |      177 |               159 |
| primitives  | rust-checked   |      165 |               142 |
| primitives  | go             |      109 |                37 |
| primitives  | c              |       68 |                64 |
| concurrency | hew-o0         |      551 |               546 |
| concurrency | hew            |     1051 |              1061 |
| concurrency | go             |      106 |                34 |
| concurrency | tokio          |     1636 |                31 |
| concurrency | beam           |      201 |               187 |

## Compilation scaling

Generated scalar functions, including native linking and a checked executable result.

| Functions | Implementation | First ms | Rebuild median ms |
| --------: | -------------- | -------: | ----------------: |
|        32 | hew-o0         |      244 |               243 |
|        32 | hew            |      266 |               267 |
|        32 | rust           |       77 |                72 |
|        32 | rust-checked   |       74 |                71 |
|        32 | go             |      142 |                34 |
|        32 | c              |       55 |                58 |
|       128 | hew-o0         |      295 |               300 |
|       128 | hew            |      399 |               401 |
|       128 | rust           |       92 |                82 |
|       128 | rust-checked   |       92 |                83 |
|       128 | go             |      154 |                37 |
|       128 | c              |       86 |                88 |
|       512 | hew-o0         |      556 |               552 |
|       512 | hew            |      958 |               932 |
|       512 | rust           |      101 |               109 |
|       512 | rust-checked   |      107 |               120 |
|       512 | go             |      185 |                67 |
|       512 | c              |      221 |               236 |

## Native allocator growth

Difference between 32 and 256 operations, divided by 224. Includes workload and runtime allocations; excludes amortized process setup. Go/BEAM internal heaps are deliberately outside this measurement.

| Workload              | Implementation | Allocations / operation | Allocated bytes / operation |
| --------------------- | -------------- | ----------------------: | --------------------------: |
| vector_build          | hew            |                    12.1 |                       65706 |
| vector_build          | rust           |                    11.0 |                       65504 |
| vector_build          | c              |                    11.0 |                       65504 |
| bytes_shared          | hew            |                     0.1 |                           2 |
| bytes_shared          | rust           |                     0.0 |                           0 |
| bytes_shared          | c              |                     0.0 |                           0 |
| bytes_cow             | hew            |                     1.1 |                        4122 |
| bytes_cow             | rust           |                     1.0 |                        4112 |
| bytes_cow             | c              |                     1.0 |                        4096 |
| task_ready            | hew            |                    12.9 |                         986 |
| task_ready            | tokio          |                     1.2 |                         142 |
| actor_roundtrip_alloc | hew            |                    10.0 |                         892 |
| actor_roundtrip_alloc | tokio          |                     1.0 |                          72 |
