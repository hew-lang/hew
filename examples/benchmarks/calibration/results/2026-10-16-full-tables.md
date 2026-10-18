# Calibration run

Execution is timed inside each workload. Process RSS includes runtime startup. Latency tails pool the recorded operations; throughput ranges describe repeated process runs.

| Workload        | Iterations |  Size | Workers | Implementation | Median batch ms |  Batch IQR ms | p50 / p95 / p99 µs       | Peak RSS MiB |
| --------------- | ---------: | ----: | ------: | -------------- | --------------: | ------------: | ------------------------ | -----------: |
| scalar          |     100000 |     1 |       1 | hew            |            0.22 |     0.22–0.23 | —                        |          2.4 |
| scalar          |     100000 |     1 |       1 | rust           |            0.22 |     0.22–0.22 | —                        |          2.2 |
| scalar          |     100000 |     1 |       1 | hew-o0         |            0.34 |     0.33–0.39 | —                        |          2.5 |
| scalar          |     100000 |     1 |       1 | c              |            0.23 |     0.22–0.23 | —                        |          1.7 |
| scalar          |     100000 |     1 |       1 | go             |            0.25 |     0.24–0.25 | —                        |          1.3 |
| scalar          |    1000000 |     1 |       1 | c              |            2.30 |     2.26–2.43 | —                        |          1.7 |
| scalar          |    1000000 |     1 |       1 | rust           |            2.40 |     2.31–2.42 | —                        |          2.1 |
| scalar          |    1000000 |     1 |       1 | hew            |            2.31 |     2.23–2.31 | —                        |          2.5 |
| scalar          |    1000000 |     1 |       1 | hew-o0         |            3.57 |     3.54–3.61 | —                        |          2.5 |
| scalar          |    1000000 |     1 |       1 | go             |            2.67 |     2.67–2.70 | —                        |          1.3 |
| vector_scan     |      15625 |    64 |       1 | go             |            0.40 |     0.40–0.41 | —                        |          1.3 |
| vector_scan     |      15625 |    64 |       1 | hew            |            0.29 |     0.29–0.29 | —                        |          2.4 |
| vector_scan     |      15625 |    64 |       1 | c              |            0.08 |     0.08–0.08 | —                        |          1.9 |
| vector_scan     |      15625 |    64 |       1 | rust           |            0.08 |     0.08–0.08 | —                        |          2.2 |
| vector_scan     |      15625 |    64 |       1 | hew-o0         |            3.31 |     3.28–3.36 | —                        |          2.5 |
| vector_build    |       4096 |    64 |       1 | go             |            2.88 |     2.56–2.91 | —                        |          5.3 |
| vector_build    |       4096 |    64 |       1 | rust           |            0.49 |     0.48–0.50 | —                        |          2.1 |
| vector_build    |       4096 |    64 |       1 | hew            |            1.60 |     1.49–2.30 | —                        |          2.4 |
| vector_build    |       4096 |    64 |       1 | c              |            0.34 |     0.34–0.34 | —                        |          1.7 |
| vector_build    |       4096 |    64 |       1 | hew-o0         |            3.18 |     3.09–3.19 | —                        |          2.5 |
| bytes_shared    |     100000 |    64 |       1 | c              |            0.29 |     0.29–0.30 | —                        |          1.7 |
| bytes_shared    |     100000 |    64 |       1 | rust           |            0.33 |     0.28–0.34 | —                        |          2.2 |
| bytes_shared    |     100000 |    64 |       1 | go             |            0.13 |     0.13–0.14 | —                        |          1.3 |
| bytes_shared    |     100000 |    64 |       1 | hew-o0         |            1.33 |     1.28–1.38 | —                        |          2.5 |
| bytes_shared    |     100000 |    64 |       1 | hew            |            0.30 |     0.30–0.30 | —                        |          2.4 |
| bytes_cow       |      16384 |    64 |       1 | go             |            0.56 |     0.52–0.58 | —                        |          2.3 |
| bytes_cow       |      16384 |    64 |       1 | rust           |            0.17 |     0.17–0.19 | —                        |          2.2 |
| bytes_cow       |      16384 |    64 |       1 | hew-o0         |            0.62 |     0.61–0.65 | —                        |          2.5 |
| bytes_cow       |      16384 |    64 |       1 | hew            |            0.28 |     0.28–0.28 | —                        |          2.5 |
| bytes_cow       |      16384 |    64 |       1 | c              |            0.10 |     0.10–0.10 | —                        |          1.7 |
| vector_scan     |        244 |  4096 |       1 | hew            |            0.33 |     0.29–0.33 | —                        |          2.5 |
| vector_scan     |        244 |  4096 |       1 | rust           |            0.05 |     0.05–0.10 | —                        |          2.1 |
| vector_scan     |        244 |  4096 |       1 | c              |            0.05 |     0.05–0.06 | —                        |          1.7 |
| vector_scan     |        244 |  4096 |       1 | hew-o0         |            3.13 |     3.10–3.22 | —                        |          2.5 |
| vector_scan     |        244 |  4096 |       1 | go             |            0.39 |     0.39–0.39 | —                        |          1.3 |
| vector_build    |         64 |  4096 |       1 | go             |            2.41 |     2.39–2.45 | —                        |          5.8 |
| vector_build    |         64 |  4096 |       1 | hew            |            1.36 |     1.36–1.38 | —                        |          2.5 |
| vector_build    |         64 |  4096 |       1 | c              |            0.25 |     0.25–0.37 | —                        |          1.7 |
| vector_build    |         64 |  4096 |       1 | hew-o0         |            2.97 |     2.86–3.06 | —                        |          2.4 |
| vector_build    |         64 |  4096 |       1 | rust           |            0.21 |     0.21–0.21 | —                        |          2.2 |
| bytes_shared    |     100000 |  4096 |       1 | go             |            0.13 |     0.13–0.13 | —                        |          1.3 |
| bytes_shared    |     100000 |  4096 |       1 | c              |            0.29 |     0.29–0.30 | —                        |          1.7 |
| bytes_shared    |     100000 |  4096 |       1 | rust           |            0.26 |     0.25–0.27 | —                        |          2.2 |
| bytes_shared    |     100000 |  4096 |       1 | hew            |            0.30 |     0.29–0.30 | —                        |          2.5 |
| bytes_shared    |     100000 |  4096 |       1 | hew-o0         |            1.26 |     1.25–1.29 | —                        |          2.4 |
| bytes_cow       |        256 |  4096 |       1 | hew            |            0.01 |     0.01–0.01 | —                        |          2.5 |
| bytes_cow       |        256 |  4096 |       1 | hew-o0         |            0.02 |     0.02–0.02 | —                        |          2.4 |
| bytes_cow       |        256 |  4096 |       1 | go             |            0.41 |     0.41–0.41 | —                        |          2.3 |
| bytes_cow       |        256 |  4096 |       1 | c              |            0.01 |     0.01–0.01 | —                        |          1.9 |
| bytes_cow       |        256 |  4096 |       1 | rust           |            0.01 |     0.01–0.01 | —                        |          2.2 |
| vector_scan     |         15 | 65536 |       1 | go             |            0.41 |     0.40–0.81 | —                        |          1.8 |
| vector_scan     |         15 | 65536 |       1 | c              |            0.05 |     0.05–0.05 | —                        |          2.2 |
| vector_scan     |         15 | 65536 |       1 | rust           |            0.05 |     0.05–0.05 | —                        |          2.4 |
| vector_scan     |         15 | 65536 |       1 | hew            |            0.29 |     0.29–0.30 | —                        |          3.2 |
| vector_scan     |         15 | 65536 |       1 | hew-o0         |            3.12 |     3.12–3.17 | —                        |          3.2 |
| vector_build    |          8 | 65536 |       1 | rust           |            0.72 |     0.71–0.76 | —                        |          3.0 |
| vector_build    |          8 | 65536 |       1 | hew            |            4.94 |     4.86–4.96 | —                        |          3.4 |
| vector_build    |          8 | 65536 |       1 | go             |            3.29 |     2.98–4.29 | —                        |          6.6 |
| vector_build    |          8 | 65536 |       1 | c              |            0.80 |     0.78–0.83 | —                        |          2.1 |
| vector_build    |          8 | 65536 |       1 | hew-o0         |            8.18 |     7.84–8.23 | —                        |          3.4 |
| bytes_shared    |     100000 | 65536 |       1 | rust           |            0.35 |     0.29–0.37 | —                        |          2.2 |
| bytes_shared    |     100000 | 65536 |       1 | hew-o0         |            1.35 |     1.34–1.41 | —                        |          2.7 |
| bytes_shared    |     100000 | 65536 |       1 | go             |            0.14 |     0.13–0.14 | —                        |          1.3 |
| bytes_shared    |     100000 | 65536 |       1 | hew            |            0.31 |     0.31–0.36 | —                        |          2.7 |
| bytes_shared    |     100000 | 65536 |       1 | c              |            0.30 |     0.30–0.30 | —                        |          1.9 |
| bytes_cow       |         16 | 65536 |       1 | go             |            0.45 |     0.36–0.46 | —                        |          2.3 |
| bytes_cow       |         16 | 65536 |       1 | hew            |            0.05 |     0.05–0.05 | —                        |          2.8 |
| bytes_cow       |         16 | 65536 |       1 | hew-o0         |            0.04 |     0.04–0.04 | —                        |          2.7 |
| bytes_cow       |         16 | 65536 |       1 | c              |            0.04 |     0.03–0.04 | —                        |          1.7 |
| bytes_cow       |         16 | 65536 |       1 | rust           |            0.04 |     0.04–0.04 | —                        |          2.4 |
| task_ready      |       4096 |     1 |       1 | beam           |            3.80 |     3.74–3.87 | —                        |         57.6 |
| task_ready      |       4096 |     1 |       1 | tokio          |           24.70 |   24.70–24.95 | —                        |          3.3 |
| task_ready      |       4096 |     1 |       1 | go             |            1.24 |     1.24–1.26 | —                        |          2.0 |
| task_ready      |       4096 |     1 |       1 | hew-o0         |           29.72 |   29.69–31.07 | —                        |          5.7 |
| task_ready      |       4096 |     1 |       1 | hew            |           30.74 |   30.53–30.85 | —                        |          5.5 |
| task_ready      |       4096 |     1 |       2 | hew-o0         |           43.97 |   32.21–48.53 | —                        |          5.5 |
| task_ready      |       4096 |     1 |       2 | beam           |            4.38 |     4.33–4.41 | —                        |         58.5 |
| task_ready      |       4096 |     1 |       2 | hew            |           41.65 |   40.82–41.82 | —                        |          5.5 |
| task_ready      |       4096 |     1 |       2 | tokio          |           28.10 |   27.26–29.44 | —                        |          3.3 |
| task_ready      |       4096 |     1 |       2 | go             |            1.29 |     1.29–1.35 | —                        |          2.1 |
| actor_fanout    |        256 |     1 |       1 | hew-o0         |            0.41 |     0.40–0.42 | —                        |          4.3 |
| actor_fanout    |        256 |     1 |       1 | tokio          |            0.04 |     0.04–0.04 | —                        |          3.4 |
| actor_fanout    |        256 |     1 |       1 | go             |            0.05 |     0.05–0.05 | —                        |          1.3 |
| actor_fanout    |        256 |     1 |       1 | beam           |            0.09 |     0.09–0.09 | —                        |         57.7 |
| actor_fanout    |        256 |     1 |       1 | hew            |            0.39 |     0.38–0.39 | —                        |          4.0 |
| actor_fanout    |        256 |     1 |       2 | go             |            0.06 |     0.05–0.06 | —                        |          1.3 |
| actor_fanout    |        256 |     1 |       2 | hew-o0         |            0.68 |     0.64–0.71 | —                        |          4.0 |
| actor_fanout    |        256 |     1 |       2 | tokio          |            0.06 |     0.05–0.08 | —                        |          3.2 |
| actor_fanout    |        256 |     1 |       2 | beam           |            0.08 |     0.08–0.08 | —                        |         58.5 |
| actor_fanout    |        256 |     1 |       2 | hew            |            0.61 |     0.60–0.62 | —                        |          4.4 |
| task_ready      |       4096 |    16 |       1 | hew            |            5.75 |    5.57–10.83 | —                        |          5.4 |
| task_ready      |       4096 |    16 |       1 | tokio          |            3.44 |    1.87–15.47 | —                        |          3.3 |
| task_ready      |       4096 |    16 |       1 | beam           |            2.54 |     2.54–2.61 | —                        |         57.7 |
| task_ready      |       4096 |    16 |       1 | go             |            1.31 |     1.30–1.33 | —                        |          2.1 |
| task_ready      |       4096 |    16 |       1 | hew-o0         |            6.07 |    6.02–12.69 | —                        |          5.7 |
| task_ready      |       4096 |    16 |       2 | tokio          |            2.69 |     2.64–3.80 | —                        |          3.3 |
| task_ready      |       4096 |    16 |       2 | hew-o0         |            6.37 |     6.33–6.81 | —                        |          5.6 |
| task_ready      |       4096 |    16 |       2 | go             |            1.29 |     1.29–1.34 | —                        |          2.3 |
| task_ready      |       4096 |    16 |       2 | beam           |            3.65 |     3.48–3.98 | —                        |         59.6 |
| task_ready      |       4096 |    16 |       2 | hew            |            6.90 |     6.44–6.95 | —                        |          5.5 |
| actor_fanout    |        256 |    16 |       1 | hew            |            6.44 |     6.37–6.46 | —                        |          4.0 |
| actor_fanout    |        256 |    16 |       1 | go             |            0.71 |     0.71–0.74 | —                        |          1.3 |
| actor_fanout    |        256 |    16 |       1 | hew-o0         |            6.61 |     6.56–6.67 | —                        |          4.2 |
| actor_fanout    |        256 |    16 |       1 | tokio          |            0.51 |     0.44–0.52 | —                        |          3.0 |
| actor_fanout    |        256 |    16 |       1 | beam           |            1.32 |     1.32–1.33 | —                        |         57.6 |
| actor_fanout    |        256 |    16 |       2 | tokio          |            0.43 |     0.26–0.44 | —                        |          3.3 |
| actor_fanout    |        256 |    16 |       2 | hew            |            6.39 |     6.02–7.76 | —                        |          4.4 |
| actor_fanout    |        256 |    16 |       2 | hew-o0         |            5.41 |     5.20–7.08 | —                        |          4.5 |
| actor_fanout    |        256 |    16 |       2 | go             |            0.79 |     0.47–0.84 | —                        |          1.6 |
| actor_fanout    |        256 |    16 |       2 | beam           |            1.38 |     1.34–1.63 | —                        |         58.1 |
| task_ready      |       4096 |    64 |       1 | hew-o0         |            7.56 |     5.81–9.65 | —                        |          5.8 |
| task_ready      |       4096 |    64 |       1 | tokio          |            0.77 |    0.76–13.77 | —                        |          3.3 |
| task_ready      |       4096 |    64 |       1 | go             |            1.36 |     1.36–1.43 | —                        |          2.1 |
| task_ready      |       4096 |    64 |       1 | hew            |            4.90 |     4.34–5.68 | —                        |          5.5 |
| task_ready      |       4096 |    64 |       1 | beam           |            2.81 |     2.80–2.82 | —                        |         57.7 |
| task_ready      |       4096 |    64 |       2 | tokio          |            1.35 |     1.27–1.61 | —                        |          3.2 |
| task_ready      |       4096 |    64 |       2 | hew-o0         |            7.71 |     7.54–7.98 | —                        |          5.7 |
| task_ready      |       4096 |    64 |       2 | beam           |            4.43 |     4.07–4.55 | —                        |         58.7 |
| task_ready      |       4096 |    64 |       2 | go             |            1.62 |     1.50–1.94 | —                        |          2.3 |
| task_ready      |       4096 |    64 |       2 | hew            |           11.74 |    7.52–14.74 | —                        |          5.5 |
| actor_fanout    |        256 |    64 |       1 | go             |            2.94 |     2.89–4.96 | —                        |          1.6 |
| actor_fanout    |        256 |    64 |       1 | beam           |            5.32 |     5.28–5.59 | —                        |         58.0 |
| actor_fanout    |        256 |    64 |       1 | hew-o0         |           32.97 |   32.92–33.43 | —                        |          4.5 |
| actor_fanout    |        256 |    64 |       1 | hew            |           32.45 |   32.30–52.21 | —                        |          4.2 |
| actor_fanout    |        256 |    64 |       1 | tokio          |            2.01 |     1.97–3.19 | —                        |          3.5 |
| actor_fanout    |        256 |    64 |       2 | beam           |            6.19 |     6.11–6.93 | —                        |         58.5 |
| actor_fanout    |        256 |    64 |       2 | hew            |           28.59 |   26.35–30.65 | —                        |          4.4 |
| actor_fanout    |        256 |    64 |       2 | tokio          |            1.31 |     0.98–1.33 | —                        |          3.5 |
| actor_fanout    |        256 |    64 |       2 | go             |            2.58 |     1.78–3.19 | —                        |          1.8 |
| actor_fanout    |        256 |    64 |       2 | hew-o0         |           31.04 |   29.52–31.12 | —                        |          4.7 |
| actor_roundtrip |       1000 |     1 |       1 | beam           |            0.41 |     0.41–0.43 | 0.3 / 0.8 / 1.9          |         57.6 |
| actor_roundtrip |       1000 |     1 |       1 | go             |            0.29 |     0.28–0.36 | 0.2 / 0.3 / 0.4          |          1.3 |
| actor_roundtrip |       1000 |     1 |       1 | hew-o0         |            7.68 |     7.48–8.22 | 7.2 / 8.9 / 19.4         |          4.4 |
| actor_roundtrip |       1000 |     1 |       1 | tokio          |            6.30 |     6.06–7.69 | 5.9 / 8.6 / 22.2         |          3.2 |
| actor_roundtrip |       1000 |     1 |       1 | hew            |            9.46 |    6.01–11.27 | 5.4 / 8.9 / 21.0         |          3.9 |
| actor_roundtrip |       1000 |     1 |       2 | tokio          |            6.93 |     6.07–7.34 | 5.3 / 10.4 / 12.9        |          3.3 |
| actor_roundtrip |       1000 |     1 |       2 | hew-o0         |            7.88 |     7.27–8.58 | 7.0 / 9.3 / 15.4         |          4.2 |
| actor_roundtrip |       1000 |     1 |       2 | hew            |            7.58 |     7.18–9.16 | 6.7 / 8.5 / 15.3         |          4.0 |
| actor_roundtrip |       1000 |     1 |       2 | go             |            0.27 |     0.24–0.31 | 0.2 / 0.3 / 0.3          |          1.6 |
| actor_roundtrip |       1000 |     1 |       2 | beam           |            0.44 |     0.42–0.48 | 0.3 / 0.6 / 2.2          |         59.3 |
| timer           |        100 |     1 |       1 | beam           |          199.80 | 199.74–200.62 | 999.7 / 1013.1 / 1707.7  |         57.4 |
| timer           |        100 |     1 |       1 | tokio          |          206.47 | 206.42–207.52 | 1055.5 / 1087.2 / 1894.9 |          3.2 |
| timer           |        100 |     1 |       1 | hew-o0         |          109.06 | 107.86–111.03 | 65.1 / 96.9 / 902.0      |          4.3 |
| timer           |        100 |     1 |       1 | go             |          108.17 | 105.58–110.22 | 55.3 / 63.2 / 285.4      |          1.3 |
| timer           |        100 |     1 |       1 | hew            |          106.73 | 106.44–108.87 | 64.4 / 73.3 / 123.7      |          3.9 |
| timer           |        100 |     1 |       2 | tokio          |          208.80 | 205.76–211.07 | 1056.2 / 1138.0 / 2867.7 |          3.2 |
| timer           |        100 |     1 |       2 | hew            |          115.00 | 108.09–115.47 | 66.5 / 565.1 / 2790.4    |          4.2 |
| timer           |        100 |     1 |       2 | beam           |          202.06 | 201.93–210.18 | 999.9 / 1078.1 / 2983.7  |         58.2 |
| timer           |        100 |     1 |       2 | go             |          112.07 | 108.02–115.19 | 61.0 / 189.1 / 1670.9    |          1.3 |
| timer           |        100 |     1 |       2 | hew-o0         |          115.41 | 107.05–115.46 | 66.8 / 265.6 / 2713.4    |          4.4 |
| scalar          |     100000 |     1 |       1 | rust-checked   |            0.23 |     0.23–0.23 | —                        |          2.2 |
| scalar          |    1000000 |     1 |       1 | rust-checked   |            2.27 |     2.27–2.34 | —                        |          2.1 |
| vector_scan     |      15625 |    64 |       1 | rust-checked   |            0.22 |     0.21–0.22 | —                        |          2.2 |
| vector_build    |       4096 |    64 |       1 | rust-checked   |            0.59 |     0.57–0.70 | —                        |          2.1 |
| bytes_shared    |     100000 |    64 |       1 | rust-checked   |            0.29 |     0.26–0.30 | —                        |          2.1 |
| bytes_cow       |      16384 |    64 |       1 | rust-checked   |            0.15 |     0.15–0.16 | —                        |          2.0 |
| vector_scan     |        244 |  4096 |       1 | rust-checked   |            0.20 |     0.20–0.20 | —                        |          2.1 |
| vector_build    |         64 |  4096 |       1 | rust-checked   |            0.31 |     0.31–0.31 | —                        |          2.1 |
| bytes_shared    |     100000 |  4096 |       1 | rust-checked   |            0.27 |     0.27–0.28 | —                        |          2.1 |
| bytes_cow       |        256 |  4096 |       1 | rust-checked   |            0.01 |     0.01–0.01 | —                        |          2.2 |
| vector_scan     |         15 | 65536 |       1 | rust-checked   |            0.23 |     0.19–0.28 | —                        |          2.6 |
| vector_build    |          8 | 65536 |       1 | rust-checked   |            0.89 |     0.87–0.90 | —                        |          2.8 |
| bytes_shared    |     100000 | 65536 |       1 | rust-checked   |            0.29 |     0.28–0.29 | —                        |          2.2 |
| bytes_cow       |         16 | 65536 |       1 | rust-checked   |            0.04 |     0.03–0.04 | —                        |          2.4 |

## Application builds

First means the first invocation in this run, with existing output and machine caches. Rebuild invokes the ordinary build tool again on unchanged source. Go/Cargo cache hits and direct compiler invocations have different behaviour.

| Fixture     | Implementation | First ms | Rebuild median ms |
| ----------- | -------------- | -------: | ----------------: |
| primitives  | hew-o0         |     2054 |               391 |
| primitives  | hew            |      537 |               526 |
| primitives  | rust           |      466 |               183 |
| primitives  | go             |     1451 |                59 |
| primitives  | c              |      584 |                79 |
| concurrency | hew-o0         |      610 |               652 |
| concurrency | hew            |      974 |               963 |
| concurrency | go             |      330 |                47 |
| concurrency | tokio          |     3517 |                36 |
| concurrency | beam           |      525 |               197 |
| primitives  | rust-checked   |      821 |               171 |

## Compilation scaling

Generated scalar functions, including native linking and a checked executable result.

| Functions | Implementation | First ms | Rebuild median ms |
| --------: | -------------- | -------: | ----------------: |
|        32 | hew-o0         |      253 |               242 |
|        32 | hew            |      266 |               264 |
|        32 | rust           |       74 |                76 |
|        32 | go             |      249 |                45 |
|        32 | c              |      102 |                64 |
|       128 | hew-o0         |      291 |               295 |
|       128 | hew            |      369 |               355 |
|       128 | rust           |       83 |                82 |
|       128 | go             |      157 |                51 |
|       128 | c              |       97 |                96 |
|       512 | hew-o0         |      440 |               424 |
|       512 | hew            |      786 |               891 |
|       512 | rust           |      108 |               112 |
|       512 | go             |      203 |                55 |
|       512 | c              |      245 |               243 |
|        32 | rust-checked   |       83 |                81 |
|       128 | rust-checked   |      115 |                95 |
|       512 | rust-checked   |      122 |               118 |

## Native allocator growth

Difference between 32 and 256 operations, divided by 224. Includes workload and runtime allocations; excludes amortized process setup. Go/BEAM internal heaps are deliberately outside this measurement.

| Workload              | Implementation | Allocations / operation | Allocated bytes / operation |
| --------------------- | -------------- | ----------------------: | --------------------------: |
| vector_build          | hew            |                    12.0 |                       65618 |
| vector_build          | rust           |                    11.0 |                       65504 |
| vector_build          | c              |                    11.0 |                       65504 |
| bytes_shared          | hew            |                     0.0 |                           2 |
| bytes_shared          | rust           |                     0.0 |                           0 |
| bytes_shared          | c              |                     0.0 |                           0 |
| bytes_cow             | hew            |                     1.0 |                        4122 |
| bytes_cow             | rust           |                     1.0 |                        4112 |
| bytes_cow             | c              |                     1.0 |                        4096 |
| task_ready            | hew            |                    13.9 |                         978 |
| task_ready            | tokio          |                     1.2 |                         142 |
| actor_roundtrip_alloc | hew            |                    16.6 |                        1253 |
| actor_roundtrip_alloc | tokio          |                     1.0 |                          72 |
