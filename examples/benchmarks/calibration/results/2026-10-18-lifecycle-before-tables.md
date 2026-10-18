# Calibration run

Execution is timed inside each workload. Process RSS includes runtime startup. Latency tails pool the recorded operations; throughput ranges describe repeated process runs.

| Workload        | Iterations |  Size | Workers | Implementation | Median batch ms |  Batch IQR ms | p50 / p95 / p99 µs       | Peak RSS MiB |
| --------------- | ---------: | ----: | ------: | -------------- | --------------: | ------------: | ------------------------ | -----------: |
| scalar          |     100000 |     1 |       1 | go             |            0.25 |     0.25–0.25 | —                        |          1.8 |
| scalar          |     100000 |     1 |       1 | hew            |            0.27 |     0.27–0.27 | —                        |          2.7 |
| scalar          |     100000 |     1 |       1 | rust           |            0.22 |     0.22–0.23 | —                        |          2.2 |
| scalar          |     100000 |     1 |       1 | hew-o0         |            0.49 |     0.49–0.50 | —                        |          3.0 |
| scalar          |     100000 |     1 |       1 | c              |            0.23 |     0.22–0.23 | —                        |          1.7 |
| scalar          |     100000 |     1 |       1 | rust-checked   |            0.22 |     0.22–0.23 | —                        |          2.2 |
| scalar          |    1000000 |     1 |       1 | hew-o0         |            4.90 |     4.86–4.92 | —                        |          3.0 |
| scalar          |    1000000 |     1 |       1 | rust-checked   |            2.23 |     2.23–2.26 | —                        |          2.2 |
| scalar          |    1000000 |     1 |       1 | go             |            2.51 |     2.50–2.70 | —                        |          1.8 |
| scalar          |    1000000 |     1 |       1 | rust           |            2.25 |     2.23–2.29 | —                        |          2.2 |
| scalar          |    1000000 |     1 |       1 | c              |            2.26 |     2.24–2.27 | —                        |          1.9 |
| scalar          |    1000000 |     1 |       1 | hew            |            2.69 |     2.69–2.72 | —                        |          2.8 |
| vector_scan     |      15625 |    64 |       1 | c              |            0.08 |     0.08–0.12 | —                        |          1.7 |
| vector_scan     |      15625 |    64 |       1 | rust           |            0.08 |     0.08–0.08 | —                        |          2.1 |
| vector_scan     |      15625 |    64 |       1 | go             |            0.22 |     0.20–0.22 | —                        |          1.8 |
| vector_scan     |      15625 |    64 |       1 | hew-o0         |            5.15 |     5.14–5.53 | —                        |          3.0 |
| vector_scan     |      15625 |    64 |       1 | rust-checked   |            0.20 |     0.20–0.28 | —                        |          2.1 |
| vector_scan     |      15625 |    64 |       1 | hew            |            0.59 |     0.59–0.74 | —                        |          2.9 |
| vector_build    |       4096 |    64 |       1 | go             |            2.16 |     2.09–2.19 | —                        |          5.8 |
| vector_build    |       4096 |    64 |       1 | hew            |            2.06 |     2.04–2.09 | —                        |          3.0 |
| vector_build    |       4096 |    64 |       1 | rust           |            0.47 |     0.47–0.49 | —                        |          2.1 |
| vector_build    |       4096 |    64 |       1 | hew-o0         |            4.18 |     4.13–4.30 | —                        |          3.0 |
| vector_build    |       4096 |    64 |       1 | rust-checked   |            0.53 |     0.52–0.53 | —                        |          2.0 |
| vector_build    |       4096 |    64 |       1 | c              |            0.34 |     0.33–0.34 | —                        |          1.9 |
| bytes_shared    |     100000 |    64 |       1 | hew-o0         |            1.35 |     1.34–1.36 | —                        |          3.0 |
| bytes_shared    |     100000 |    64 |       1 | go             |            0.15 |     0.14–0.15 | —                        |          1.8 |
| bytes_shared    |     100000 |    64 |       1 | rust-checked   |            0.26 |     0.26–0.27 | —                        |          2.2 |
| bytes_shared    |     100000 |    64 |       1 | hew            |            0.30 |     0.29–0.30 | —                        |          2.7 |
| bytes_shared    |     100000 |    64 |       1 | c              |            0.30 |     0.29–0.31 | —                        |          1.9 |
| bytes_shared    |     100000 |    64 |       1 | rust           |            0.25 |     0.25–0.26 | —                        |          2.0 |
| bytes_cow       |      16384 |    64 |       1 | rust           |            0.17 |     0.15–0.23 | —                        |          2.2 |
| bytes_cow       |      16384 |    64 |       1 | hew-o0         |            1.03 |     0.67–1.05 | —                        |          3.0 |
| bytes_cow       |      16384 |    64 |       1 | hew            |            0.27 |     0.27–0.44 | —                        |          2.7 |
| bytes_cow       |      16384 |    64 |       1 | c              |            0.10 |     0.10–0.10 | —                        |          1.7 |
| bytes_cow       |      16384 |    64 |       1 | go             |            0.55 |     0.53–0.78 | —                        |          2.8 |
| bytes_cow       |      16384 |    64 |       1 | rust-checked   |            0.24 |     0.15–0.26 | —                        |          2.1 |
| vector_scan     |        244 |  4096 |       1 | hew-o0         |            5.05 |     5.05–5.12 | —                        |          3.0 |
| vector_scan     |        244 |  4096 |       1 | hew            |            0.58 |     0.57–0.59 | —                        |          2.7 |
| vector_scan     |        244 |  4096 |       1 | c              |            0.05 |     0.05–0.05 | —                        |          1.7 |
| vector_scan     |        244 |  4096 |       1 | rust-checked   |            0.19 |     0.19–0.20 | —                        |          2.0 |
| vector_scan     |        244 |  4096 |       1 | go             |            0.22 |     0.22–0.22 | —                        |          1.8 |
| vector_scan     |        244 |  4096 |       1 | rust           |            0.05 |     0.05–0.05 | —                        |          2.2 |
| vector_build    |         64 |  4096 |       1 | hew            |            1.73 |     1.73–1.76 | —                        |          2.8 |
| vector_build    |         64 |  4096 |       1 | hew-o0         |            3.78 |     3.70–3.84 | —                        |          3.0 |
| vector_build    |         64 |  4096 |       1 | rust-checked   |            0.30 |     0.29–0.30 | —                        |          2.2 |
| vector_build    |         64 |  4096 |       1 | go             |            2.01 |     1.92–2.12 | —                        |          6.3 |
| vector_build    |         64 |  4096 |       1 | c              |            0.25 |     0.24–0.25 | —                        |          1.9 |
| vector_build    |         64 |  4096 |       1 | rust           |            0.21 |     0.20–0.34 | —                        |          2.1 |
| bytes_shared    |     100000 |  4096 |       1 | rust-checked   |            0.28 |     0.26–0.28 | —                        |          2.2 |
| bytes_shared    |     100000 |  4096 |       1 | hew            |            0.30 |     0.30–0.30 | —                        |          2.9 |
| bytes_shared    |     100000 |  4096 |       1 | go             |            0.13 |     0.13–0.13 | —                        |          1.8 |
| bytes_shared    |     100000 |  4096 |       1 | rust           |            0.26 |     0.26–0.28 | —                        |          2.1 |
| bytes_shared    |     100000 |  4096 |       1 | hew-o0         |            1.36 |     1.35–1.38 | —                        |          3.0 |
| bytes_shared    |     100000 |  4096 |       1 | c              |            0.29 |     0.29–0.30 | —                        |          1.9 |
| bytes_cow       |        256 |  4096 |       1 | hew            |            0.02 |     0.02–0.02 | —                        |          2.9 |
| bytes_cow       |        256 |  4096 |       1 | go             |            0.36 |     0.34–0.37 | —                        |          2.8 |
| bytes_cow       |        256 |  4096 |       1 | hew-o0         |            0.02 |     0.02–0.02 | —                        |          3.0 |
| bytes_cow       |        256 |  4096 |       1 | rust-checked   |            0.01 |     0.01–0.01 | —                        |          2.2 |
| bytes_cow       |        256 |  4096 |       1 | rust           |            0.01 |     0.01–0.01 | —                        |          2.2 |
| bytes_cow       |        256 |  4096 |       1 | c              |            0.01 |     0.01–0.01 | —                        |          1.9 |
| vector_scan     |         15 | 65536 |       1 | hew-o0         |            4.97 |     4.97–5.01 | —                        |          3.7 |
| vector_scan     |         15 | 65536 |       1 | c              |            0.05 |     0.05–0.05 | —                        |          2.4 |
| vector_scan     |         15 | 65536 |       1 | rust-checked   |            0.19 |     0.19–0.19 | —                        |          2.6 |
| vector_scan     |         15 | 65536 |       1 | hew            |            0.58 |     0.57–0.58 | —                        |          3.5 |
| vector_scan     |         15 | 65536 |       1 | rust           |            0.05 |     0.05–0.05 | —                        |          2.5 |
| vector_scan     |         15 | 65536 |       1 | go             |            0.21 |     0.19–0.25 | —                        |          2.3 |
| vector_build    |          8 | 65536 |       1 | c              |            0.77 |     0.76–1.13 | —                        |          2.2 |
| vector_build    |          8 | 65536 |       1 | hew-o0         |            9.71 |    9.60–10.22 | —                        |          3.8 |
| vector_build    |          8 | 65536 |       1 | go             |            2.79 |     2.66–2.94 | —                        |          7.1 |
| vector_build    |          8 | 65536 |       1 | rust           |            0.69 |     0.69–0.71 | —                        |          2.9 |
| vector_build    |          8 | 65536 |       1 | hew            |            6.77 |     5.63–8.13 | —                        |          3.8 |
| vector_build    |          8 | 65536 |       1 | rust-checked   |            0.87 |     0.85–0.98 | —                        |          2.9 |
| bytes_shared    |     100000 | 65536 |       1 | rust-checked   |            0.28 |     0.28–0.29 | —                        |          2.2 |
| bytes_shared    |     100000 | 65536 |       1 | c              |            0.30 |     0.30–0.30 | —                        |          1.7 |
| bytes_shared    |     100000 | 65536 |       1 | rust           |            0.28 |     0.28–0.30 | —                        |          2.2 |
| bytes_shared    |     100000 | 65536 |       1 | hew-o0         |            1.35 |     1.35–1.36 | —                        |          3.2 |
| bytes_shared    |     100000 | 65536 |       1 | go             |            0.13 |     0.13–0.14 | —                        |          1.8 |
| bytes_shared    |     100000 | 65536 |       1 | hew            |            0.30 |     0.30–0.30 | —                        |          3.1 |
| bytes_cow       |         16 | 65536 |       1 | rust           |            0.03 |     0.03–0.04 | —                        |          2.5 |
| bytes_cow       |         16 | 65536 |       1 | hew            |            0.03 |     0.03–0.04 | —                        |          3.0 |
| bytes_cow       |         16 | 65536 |       1 | go             |            0.33 |     0.30–0.40 | —                        |          2.8 |
| bytes_cow       |         16 | 65536 |       1 | hew-o0         |            0.04 |     0.03–0.05 | —                        |          3.2 |
| bytes_cow       |         16 | 65536 |       1 | rust-checked   |            0.03 |     0.03–0.04 | —                        |          2.4 |
| bytes_cow       |         16 | 65536 |       1 | c              |            0.03 |     0.03–0.03 | —                        |          1.7 |
| task_ready      |       4096 |     1 |       1 | hew            |           26.25 |   25.35–29.57 | —                        |          5.7 |
| task_ready      |       4096 |     1 |       1 | hew-o0         |           28.01 |   27.69–28.44 | —                        |          5.7 |
| task_ready      |       4096 |     1 |       1 | tokio          |           20.51 |   15.17–23.55 | —                        |          3.2 |
| task_ready      |       4096 |     1 |       1 | beam           |            3.66 |     3.62–3.72 | —                        |         57.3 |
| task_ready      |       4096 |     1 |       1 | go             |            1.22 |     1.21–1.26 | —                        |          2.6 |
| task_ready      |       4096 |     1 |       2 | go             |            1.27 |     1.17–1.88 | —                        |          2.6 |
| task_ready      |       4096 |     1 |       2 | beam           |            4.27 |     4.21–4.30 | —                        |         58.3 |
| task_ready      |       4096 |     1 |       2 | hew-o0         |           40.92 |   28.83–44.02 | —                        |          5.8 |
| task_ready      |       4096 |     1 |       2 | tokio          |           28.29 |   27.72–28.44 | —                        |          3.3 |
| task_ready      |       4096 |     1 |       2 | hew            |           36.63 |   33.19–45.90 | —                        |          5.5 |
| actor_fanout    |        256 |     1 |       1 | hew-o0         |            0.34 |     0.33–0.35 | —                        |          4.4 |
| actor_fanout    |        256 |     1 |       1 | hew            |            0.31 |     0.30–0.31 | —                        |          4.2 |
| actor_fanout    |        256 |     1 |       1 | go             |            0.05 |     0.05–0.05 | —                        |          1.8 |
| actor_fanout    |        256 |     1 |       1 | beam           |            0.09 |     0.08–0.09 | —                        |         57.4 |
| actor_fanout    |        256 |     1 |       1 | tokio          |            0.04 |     0.04–0.04 | —                        |          3.3 |
| actor_fanout    |        256 |     1 |       2 | hew            |            0.49 |     0.49–0.77 | —                        |          4.4 |
| actor_fanout    |        256 |     1 |       2 | beam           |            0.08 |     0.08–0.09 | —                        |         58.3 |
| actor_fanout    |        256 |     1 |       2 | hew-o0         |            0.60 |     0.53–0.77 | —                        |          4.7 |
| actor_fanout    |        256 |     1 |       2 | go             |            0.06 |     0.05–0.07 | —                        |          1.8 |
| actor_fanout    |        256 |     1 |       2 | tokio          |            0.06 |     0.06–0.09 | —                        |          3.3 |
| task_ready      |       4096 |    16 |       1 | beam           |            2.48 |     2.48–2.51 | —                        |         57.4 |
| task_ready      |       4096 |    16 |       1 | hew-o0         |           12.41 |    9.78–14.17 | —                        |          5.9 |
| task_ready      |       4096 |    16 |       1 | hew            |           11.89 |   11.51–12.05 | —                        |          5.7 |
| task_ready      |       4096 |    16 |       1 | tokio          |           10.88 |    2.08–14.30 | —                        |          3.3 |
| task_ready      |       4096 |    16 |       1 | go             |            1.25 |     1.21–1.31 | —                        |          2.6 |
| task_ready      |       4096 |    16 |       2 | hew-o0         |            9.54 |    6.58–12.65 | —                        |          5.7 |
| task_ready      |       4096 |    16 |       2 | tokio          |            3.67 |     3.54–3.72 | —                        |          3.3 |
| task_ready      |       4096 |    16 |       2 | go             |            1.36 |     1.28–1.41 | —                        |          2.8 |
| task_ready      |       4096 |    16 |       2 | hew            |            8.33 |    6.43–19.33 | —                        |          5.7 |
| task_ready      |       4096 |    16 |       2 | beam           |            3.77 |     3.30–4.23 | —                        |         60.7 |
| actor_fanout    |        256 |    16 |       1 | tokio          |            0.48 |     0.45–0.50 | —                        |          3.3 |
| actor_fanout    |        256 |    16 |       1 | beam           |            1.26 |     1.25–1.31 | —                        |         57.3 |
| actor_fanout    |        256 |    16 |       1 | hew-o0         |            5.09 |     5.05–5.13 | —                        |          4.3 |
| actor_fanout    |        256 |    16 |       1 | hew            |            4.68 |     4.65–4.72 | —                        |          4.0 |
| actor_fanout    |        256 |    16 |       1 | go             |            0.72 |     0.71–0.74 | —                        |          1.8 |
| actor_fanout    |        256 |    16 |       2 | hew-o0         |            6.37 |     4.15–6.51 | —                        |          4.8 |
| actor_fanout    |        256 |    16 |       2 | hew            |            6.20 |     4.91–7.58 | —                        |          4.3 |
| actor_fanout    |        256 |    16 |       2 | beam           |            1.36 |     1.12–1.75 | —                        |         58.2 |
| actor_fanout    |        256 |    16 |       2 | tokio          |            0.43 |     0.28–0.50 | —                        |          3.3 |
| actor_fanout    |        256 |    16 |       2 | go             |            0.68 |     0.50–0.71 | —                        |          2.1 |
| task_ready      |       4096 |    64 |       1 | beam           |            2.78 |     2.75–4.48 | —                        |         57.4 |
| task_ready      |       4096 |    64 |       1 | hew            |            4.50 |     4.22–8.05 | —                        |          5.7 |
| task_ready      |       4096 |    64 |       1 | tokio          |            2.84 |    0.75–12.60 | —                        |          3.3 |
| task_ready      |       4096 |    64 |       1 | go             |            1.34 |     1.28–1.38 | —                        |          2.6 |
| task_ready      |       4096 |    64 |       1 | hew-o0         |            8.34 |    5.34–11.78 | —                        |          5.8 |
| task_ready      |       4096 |    64 |       2 | hew-o0         |            6.01 |    5.40–13.31 | —                        |          5.7 |
| task_ready      |       4096 |    64 |       2 | beam           |            3.97 |     3.66–4.65 | —                        |         60.8 |
| task_ready      |       4096 |    64 |       2 | go             |            1.34 |     1.29–1.53 | —                        |          3.1 |
| task_ready      |       4096 |    64 |       2 | tokio          |            1.44 |     0.87–1.87 | —                        |          3.4 |
| task_ready      |       4096 |    64 |       2 | hew            |            6.11 |    5.16–13.26 | —                        |          5.7 |
| actor_fanout    |        256 |    64 |       1 | hew-o0         |           20.37 |   20.18–21.43 | —                        |          4.5 |
| actor_fanout    |        256 |    64 |       1 | hew            |           18.78 |   18.62–19.00 | —                        |          4.2 |
| actor_fanout    |        256 |    64 |       1 | go             |            2.80 |     2.78–2.92 | —                        |          2.1 |
| actor_fanout    |        256 |    64 |       1 | tokio          |            1.74 |     1.70–1.82 | —                        |          3.5 |
| actor_fanout    |        256 |    64 |       1 | beam           |            5.05 |     5.00–5.08 | —                        |         57.6 |
| actor_fanout    |        256 |    64 |       2 | hew            |           18.23 |   17.21–21.01 | —                        |          4.4 |
| actor_fanout    |        256 |    64 |       2 | tokio          |            1.69 |     1.23–1.70 | —                        |          3.5 |
| actor_fanout    |        256 |    64 |       2 | beam           |            6.76 |     5.64–7.43 | —                        |         58.7 |
| actor_fanout    |        256 |    64 |       2 | hew-o0         |           19.47 |   17.48–21.67 | —                        |          4.7 |
| actor_fanout    |        256 |    64 |       2 | go             |            2.24 |     2.02–3.11 | —                        |          2.3 |
| actor_roundtrip |       1000 |     1 |       1 | beam           |            0.41 |     0.38–0.61 | 0.3 / 0.9 / 2.0          |         57.4 |
| actor_roundtrip |       1000 |     1 |       1 | tokio          |            4.62 |     3.76–6.36 | 4.6 / 6.8 / 15.1         |          3.3 |
| actor_roundtrip |       1000 |     1 |       1 | hew-o0         |            6.48 |     6.04–7.51 | 6.2 / 7.7 / 10.4         |          4.4 |
| actor_roundtrip |       1000 |     1 |       1 | go             |            0.24 |     0.23–0.28 | 0.2 / 0.3 / 0.3          |          1.8 |
| actor_roundtrip |       1000 |     1 |       1 | hew            |            6.64 |     5.36–7.89 | 6.2 / 8.1 / 11.1         |          4.3 |
| actor_roundtrip |       1000 |     1 |       2 | beam           |            0.37 |     0.35–0.41 | 0.3 / 0.4 / 2.0          |         59.0 |
| actor_roundtrip |       1000 |     1 |       2 | hew-o0         |            6.66 |     5.14–7.86 | 6.5 / 8.0 / 11.0         |          4.5 |
| actor_roundtrip |       1000 |     1 |       2 | go             |            0.27 |     0.25–0.32 | 0.2 / 0.3 / 0.3          |          2.1 |
| actor_roundtrip |       1000 |     1 |       2 | tokio          |            6.67 |     5.50–6.99 | 5.6 / 10.4 / 11.8        |          3.3 |
| actor_roundtrip |       1000 |     1 |       2 | hew            |            7.53 |     6.42–7.78 | 6.9 / 8.3 / 15.3         |          4.4 |
| timer           |        100 |     1 |       1 | beam           |          201.61 | 200.63–203.61 | 999.7 / 1013.9 / 2916.6  |         57.5 |
| timer           |        100 |     1 |       1 | hew            |          106.99 | 106.24–112.90 | 63.0 / 103.8 / 602.8     |          4.1 |
| timer           |        100 |     1 |       1 | hew-o0         |          109.42 | 109.03–113.63 | 62.8 / 169.6 / 2141.3    |          4.2 |
| timer           |        100 |     1 |       1 | go             |          105.63 | 105.49–107.81 | 54.5 / 61.5 / 74.2       |          1.8 |
| timer           |        100 |     1 |       1 | tokio          |          207.39 | 206.82–207.58 | 1054.7 / 1060.7 / 1606.6 |          3.3 |
| timer           |        100 |     1 |       2 | beam           |          199.83 | 199.51–199.85 | 999.8 / 1008.9 / 1027.2  |         59.0 |
| timer           |        100 |     1 |       2 | tokio          |          206.72 | 205.60–211.98 | 1055.0 / 1064.4 / 1800.9 |          3.3 |
| timer           |        100 |     1 |       2 | go             |          105.91 | 105.79–107.01 | 58.0 / 63.2 / 69.9       |          1.8 |
| timer           |        100 |     1 |       2 | hew-o0         |          106.61 | 106.25–108.39 | 62.9 / 71.9 / 127.5      |          4.5 |
| timer           |        100 |     1 |       2 | hew            |          106.79 | 105.99–110.47 | 62.5 / 94.4 / 999.3      |          4.4 |

## Application builds

First means the first invocation in this run, with existing output and machine caches. Rebuild invokes the ordinary build tool again on unchanged source. Go/Cargo cache hits and direct compiler invocations have different behaviour.

| Fixture     | Implementation | First ms | Rebuild median ms |
| ----------- | -------------- | -------: | ----------------: |
| primitives  | hew-o0         |      528 |               442 |
| primitives  | hew            |      585 |               576 |
| primitives  | rust           |      206 |               152 |
| primitives  | rust-checked   |      131 |               138 |
| primitives  | go             |      289 |                35 |
| primitives  | c              |      131 |                65 |
| concurrency | hew-o0         |      564 |               562 |
| concurrency | hew            |     1011 |              1012 |
| concurrency | go             |      150 |                50 |
| concurrency | tokio          |     6418 |                30 |
| concurrency | beam           |      212 |               180 |

## Compilation scaling

Generated scalar functions, including native linking and a checked executable result.

| Functions | Implementation | First ms | Rebuild median ms |
| --------: | -------------- | -------: | ----------------: |
|        32 | hew-o0         |      268 |               247 |
|        32 | hew            |      270 |               277 |
|        32 | rust           |       66 |                70 |
|        32 | rust-checked   |       65 |                68 |
|        32 | go             |      181 |                38 |
|        32 | c              |       57 |                56 |
|       128 | hew-o0         |      297 |               292 |
|       128 | hew            |      408 |               402 |
|       128 | rust           |       80 |                91 |
|       128 | rust-checked   |       81 |                81 |
|       128 | go             |      169 |                61 |
|       128 | c              |       85 |                85 |
|       512 | hew-o0         |      524 |               525 |
|       512 | hew            |      924 |               919 |
|       512 | rust           |      114 |                96 |
|       512 | rust-checked   |      108 |               109 |
|       512 | go             |      206 |                34 |
|       512 | c              |      216 |               217 |

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
| task_ready            | hew            |                    14.9 |                        1052 |
| task_ready            | tokio          |                     1.2 |                         142 |
| actor_roundtrip_alloc | hew            |                    12.2 |                         949 |
| actor_roundtrip_alloc | tokio          |                     1.0 |                          72 |
