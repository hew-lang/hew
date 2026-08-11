# CI performance policy and slow-platform recommendation

This work only skips a command after all inputs that can affect its verdict are
byte-identical. It does not filter tests, loosen an assertion, retry a failure,
or silently move coverage away from a push.

## Snapshot and compile-fail scope

The checked-MIR corpus is a good fit for
[`insta`](https://insta.rs/docs/quickstart/): 58 fixtures still produce both raw
and elaborated MIR, but one reviewed snapshot replaces 116 loose golden files
and a hand-maintained checksum manifest. A drift fails verification; the update
command writes a pending snapshot for review with the
[`cargo insta review`](https://insta.rs/docs/cli/) workflow. The old comparison
loop took 10.40 seconds locally and the equivalent `insta` assertion took 10.13
seconds, a 0.27-second (2.6%) reduction with the same 116 compiler invocations.

Seven stable native rejection cases now use adjacent `.stderr` files in the
style documented by [`trybuild`](https://docs.rs/trybuild/latest/trybuild/).
Each case must exit with status 1 and match its complete normalized diagnostic.
This is stronger than the former substring checks and executes the same seven
compiler commands. The counterfactual test proves that an unexpected pass and
diagnostic drift both fail. The former seven-command check loop took 0.16
seconds; complete diagnostic comparison took 0.21 seconds. The 0.05-second cost
buys a stronger assertion and is not presented as a speedup.

The following remain specialized because a plain text snapshot would assert
the wrong thing:

- checked-MIR runtime transcripts assert exit status, crashes, leaks, and
  platform-sensitive normalization as well as text;
- LLVM output uses a semantic normalizer for pool identifiers, so a raw
  snapshot would add noise and obscure the byte-identity property;
- `hew-corpus-expected-failures.txt` covers packages, fragments, deferred
  features, and standalone files with different invocation contexts, so a
  native adjacent-stderr runner cannot represent its verdicts faithfully;
- `doc-test-expected-failures.txt` identifies extracted fences by source
  checksum; there is no stable adjacent source file to own a diagnostic;
- the fuzz oracle's failure inventory is part of the oracle self-test and
  deliberately exercises unexpected-failure and stale-entry behavior.

The empty compiled-Hew and stdlib failure inventories were removed. Those
gates now state the real invariant directly: every selected test or source must
pass. Compiler invocation counts are unchanged; list parsing was too small to
measure separately and no runtime saving is claimed.

## Complete verdict-cache keys

The local compiled-Hew suite retains its established per-fixture cache unit. A
fixture can import sibling sources, so every source path and byte hash is in the
shared portion of every key rather than assuming the fixture is isolated. A
successful cached JUnit report is reused only when its versioned key contains:

- the compiler and adjacent `libhew.a`/`hew.lib` bytes;
- the runner script and strict JUnit parser bytes;
- every relevant `.hew`, `.toml`, and `.lock` path and content hash, including
  an externally supplied test directory;
- the exact command protocol and every semantic `HEW_*` variable;
- OS/image identity, machine identity, and resolved compiler/linker paths and
  versions.

Malformed reports are misses. A cold failure is never stored as a green. A bare
source is cached as empty only after exit status 0 and the exact no-test
diagnostic. The warm path still parses every test-bearing report, asserts a
nonempty merged inventory, checks summary counts against testcase elements, and
requires zero failures. On the same machine, an unsandboxed cold run executed
all 1,326 tests in 1,202.29 seconds; the immediate complete-key warm run asserted
the same 1,326 outcomes in 11.02 seconds, a 1,191.27-second (99.1%) reduction.

The four CI shards use the same categories plus shard partition and O0/O2
level. They list the expected shard inventory on every run and accept a cached
report only if its testcase identities and return code agree with that list.
Each shard restores only its own cache prefix. A counterfactual stub measured
0.3112 seconds cold and 0.2919 seconds warm; the important result is two O0/O2
compiler executions before versus zero after. The small 0.0193-second stub
difference is key-validation overhead, not a projection of production savings.
Changing the compiler, runtime archive, or a semantic environment variable
causes two executions again.

## Derived timeout policy

`scripts/timeout-policy.py` owns all 42 workflow job ceilings as named workload
classes and all 13 nextest `slow-timeout` entries as ten-second quanta plus
semantic termination counts. Its counterfactual tests replace a value with
`999` and require the renderer to recover the policy value.

Workflow ceilings cannot use measured host parallelism: GitHub evaluates
[`jobs.<job_id>.timeout-minutes`](https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-syntax#jobsjob_idtimeout-minutes)
before a runner exists. The jobs therefore select centralized calibrated
classes, while commands inside the runner retain host-scaled budgets. Nextest
profile inheritance could reduce repetition in newer releases, but it was
introduced in
[`cargo-nextest` 0.9.115](https://nexte.st/docs/configuration/reference/#profile-inheritance)
and this repository deliberately pins 0.9.99, so the renderer is the compatible
single authority.

The two `leak-timeout = 200ms` values stay fixed. They are semantic grace
periods for leaked process handles, not estimates of test duration. The brief
counted 18 nextest ceilings; the current file has 15 numeric timeout entries:
13 slow ceilings now derived and those two fixed leak assertions.

This policy changes ownership and drift detection, not test wall clock. The
new policy check takes 0.04 seconds locally; the prior check cost was zero
because no centralized drift check existed.

## Slow platform legs

The dollar estimates use GitHub's current
[`Actions runner pricing`](https://docs.github.com/en/billing/reference/actions-runner-pricing)
and are metered equivalents for private repositories beyond included minutes.
Standard runners are free for public repositories; larger runners remain
billed, as described in the
[`Actions billing overview`](https://docs.github.com/en/billing/concepts/product-billing/github-actions).
Prices and observed durations should be rechecked before changing labels.

### Intel macOS

Observed duration is 28.8 minutes versus roughly 7 minutes for the other macOS
architecture.

| Option | Direct cost per run | Critical-path effect | Coverage effect |
|---|---:|---:|---|
| Keep the standard Intel runner | `28.8 × $0.062 = $1.79` | baseline | none |
| Trial a 12-core larger macOS runner | `$0.077 × measured minutes`; a 10-minute run would cost `$0.77` | example saves 18.8 minutes | none |
| Keep build/link/smoke per push; run the full workspace daily | `$0.062 × smoke_minutes` per push plus `$1.79/day` | saves `28.8 - smoke_minutes` per push | full Intel Rust behavior may be detected up to 24 hours later |
| Drop Intel macOS | saves `$1.79` and 28.8 minutes per run | removes this leg | x86_64 macOS ABI, linker, deployment-target, and runtime regressions are never regained |

The larger runner breaks even on direct cost if it finishes within
`28.8 × 0.062 / 0.077 = 23.2` minutes. Recommendation: benchmark that runner
for at least 20 runs and adopt it if p95 is below 23.2 minutes. If it is not,
retain build/link/smoke per push and schedule the unchanged full suite daily;
do not drop the architecture.

### FreeBSD aarch64 under QEMU

The current roughly 180-minute run on a standard Linux x64 host has a private
metered equivalent of `180 × $0.008 = $1.44` per run (`$2.40` at the 300-minute
ceiling). Public standard-runner use is free, but the three-hour critical path
remains.

| Option | Direct cost per run | Critical-path effect | Coverage effect |
|---|---:|---:|---|
| Keep QEMU per push | about `$1.44` at 180 minutes | baseline | none |
| Native FreeBSD aarch64 self-host | host cost plus GitHub's current `$0.002/min` platform charge; a 60-minute run is `$0.12` plus host | expected to remove emulation cost; measure before claiming a duration | none, plus host maintenance and availability risk |
| Cross-build/smoke per push; run QEMU daily | per-push cross-build cost plus `$1.44/day` for the observed scheduled run | removes about 180 minutes from most push matrices | FreeBSD+aarch64 runtime regressions may be detected up to 24 hours later; FreeBSD x64 and Linux aarch64 remain per-push evidence |
| Drop FreeBSD aarch64 | saves about `$1.44` and 180 minutes per run | removes this leg | combined FreeBSD/aarch64 ABI, atomics, linker, and runtime coverage is never regained |

At `P` pushes per day, moving the unchanged QEMU run to daily saves
`180 × (P - 1)` runner-minutes per day while retaining a maximum 24-hour
detection delay. Recommendation: use the scheduled option as the documented
interim choice and price a native host trial. Adopt native execution only after
its p95 duration, host bill, maintenance ownership, and outage behavior are
recorded. Do not drop the architecture.
