# HTTP service comparators

These small services let the calibration runner compare the same checked HTTP
workloads across Hew, Go, Rust, .NET and Erlang/Cowboy. They are standalone
apps; their builds do not require or build the Hew Cargo workspace.

Each app binds to `BENCH_BIND`, defaulting to `127.0.0.1:18080`, and prints
`READY <address>` after binding. `BENCH_WORKERS` defaults to 2. The runner
sets the worker count and uses client concurrency from 1 to 32. The servers
use implementation-specific admission limits: Go and .NET gate handlers at
32, Tokio uses a 32-permit semaphore, Cowboy/Ranch caps live connections at
32 with one connection supervisor, and Hew has 32 workers with one queued
connection per worker. These mechanisms have different admission behavior;
the comparator uses a client concurrency of at most 32 without claiming
identical server-side queueing. Clients
send HTTP/1.1 POST requests with `Connection: close`; servers close each
connection after its response. There is no TLS or compression. Requests have
exactly 64 ASCII bytes, with the first byte used as the numeric seed:

| Path       | Work                                                                                                                      |
| ---------- | ------------------------------------------------------------------------------------------------------------------------- |
| `/echo`    | Return the request bytes unchanged.                                                                                       |
| `/compute` | Sum `(seed + i) % 251` for `i` from 0 through 199,999 and return `<body>:<sum>`.                                          |
| `/fanout`  | Compute the same sum in eight 25,000-iteration partitions and return `<body>:<sum>`.                                      |
| `/wait`    | Wait asynchronously for 5 ms, then return the request bytes.                                                              |
| `/io`      | POST the bytes to `BENCH_BACKEND/echo`, require HTTP 200 and an exact byte-for-byte echo, then return the original bytes. |

The route results and input checks are shared, while scheduling mechanisms
follow each runtime: Go uses goroutines and a channel-based handler gate;
Rust uses Tokio tasks and a semaphore; .NET uses ASP.NET Core/Kestrel, a
semaphore and `Task.Delay`, runs `/compute` inline on its handler thread, and
uses eight thread-pool tasks for `/fanout`; Erlang uses
Cowboy request processes and eight spawned BEAM processes for `/fanout`; Hew
uses its native HTTP codec, a fixed pool of 32 workers, and `fork` for
`/fanout`. These differences are part of the comparator. In particular,
`/wait` uses a runtime's timer where available, and each `/fanout` uses that
runtime's task or process mechanism rather than claiming identical scheduler
semantics.

Build the standalone comparators from this directory. Choose an output path
outside the source tree to keep generated files out of the checkout:

```sh
make OUT=/tmp/calibration-services/out go rust dotnet beam
```

Build Hew separately with the selected compiler binary:

```sh
make HEW=/path/to/hew OUT=/tmp/calibration-services/out hew
```

The builds use locked Rust dependencies, pinned Cowboy dependencies, and the
.NET SDK/runtime available on the build host. Run each prebuilt app directly
or through the local pass runner; for example, Go:

```sh
BENCH_BIND=127.0.0.1:18080 BENCH_WORKERS=2 /tmp/calibration-services/out/service-go
```

The Erlang launcher is `beam/run.sh` and must run with `beam/` as its working
directory after `make beam`. The .NET app runs with
`dotnet /tmp/calibration-services/out/dotnet/calibration-service-dotnet.dll`.
For `/io`, set `BENCH_BACKEND` to a reachable `host:port`; it is only needed
for that route. The pass runner starts the checked backend and records the
selected address in the result.

The JSON config accepted by `guest/service-pass.py` supplies `servers` with a
unique `name` and an `argv` array. Optional `cwd` is relative to the config
directory; optional `artifacts` is a list of prebuilt files to hash, useful
for BEAM `.beam` files and .NET dependency manifests. Optional `version_argv`
is run before the measurement and its bounded output and exit code are saved
as runtime metadata. For example:

```json
{
  "servers": [
    {
      "name": "go",
      "argv": ["/tmp/calibration-services/out/service-go"],
      "version_argv": ["go", "version"]
    },
    {
      "name": "dotnet",
      "argv": [
        "dotnet",
        "/tmp/calibration-services/out/dotnet/calibration-service-dotnet.dll"
      ],
      "version_argv": ["dotnet", "--info"],
      "artifacts": [
        "/tmp/calibration-services/out/dotnet/calibration-service-dotnet.deps.json"
      ]
    }
  ],
  "workers": [1, 2, 8],
  "workloads": ["/echo", "/compute", "/fanout", "/wait", "/io"],
  "concurrency": [1, 16]
}
```

The runner writes one JSON result with command lines, selected environment,
server and driver hashes, optional runtime/artifact hashes, server CPU ticks
and memory snapshots, client command wall time, and checked per-request
samples. The Go driver reports user and system CPU for the timed request
batch; `/io` CPU includes the local dependency server because it runs in the
same driver process. Overload cases are preserved and marked invalid for
latency comparison.
