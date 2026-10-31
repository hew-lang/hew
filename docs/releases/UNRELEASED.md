# Unreleased

The current candidate is [v0.10.0-rc1](v0.10.0-rc1.md). Changes after its frozen
candidate commit belong here.

## Supervisor restart delays

A supervisor without `backoff` now restarts a crashed child immediately. The
previous implicit delay of 100 milliseconds, doubling to 30 seconds from the
second crash, stalled tight restart loops. Declare
`backoff: restart.exponential(initial: 100ms)` to keep a delay; `maximum`
defaults to 30 seconds, and the delay never drops below `initial`.

A child declaration accepts its own `backoff:` clause beside `restart:` and
`stop:`, replacing the supervisor's policy for that child:

```hew
import std.restart;

actor Client {
    let url: string;
}

actor Cache {}

supervisor Gateway(url: string) {
    backoff: restart.exponential(initial: 100ms, maximum: 5s);
    child upstream: Client { url } restart: permanent backoff: restart.exponential(initial: 1s, maximum: 1m);
    child cache: Cache restart: permanent backoff: restart.immediate();
}

fn main() {
    let gateway = spawn Gateway { url: "https://example.test" };
    stop(gateway);
    stopped(gateway);
}
```

`restart.immediate()` restarts its child without delay. It is the policy with
both durations zero, so a hand-written
`restart.Backoff { initial: 0ns, maximum: 0ns }` is now accepted with the same
meaning instead of failing construction.
