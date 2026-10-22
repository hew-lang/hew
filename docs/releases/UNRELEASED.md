# Unreleased

Changes after [v0.6.5](v0.6.5.md) belong here.

## Owning calls and iterators

`consume` gives the callee ownership. Ordinary values remain independent when
the caller uses them again; the compiler can move them at last use. Affine
resources still transfer their sole owner. These rules apply to parameters,
receivers and cloneable once-callables.

```hew
fn count(consume values: Vec<i64>) -> i64 { values.len() }

fn main() {
    let values = [1, 2, 3];
    println(count(values));
    println(values.len());
    var items = values.into_iter();
    println(items.next().expect("first item"));
    println(values.len());
}
```

This prints `3`, `3`, `1`, `3`. Owning vector iteration now uses the ordinary
Hew library implementation, with linear total movement and ordered cleanup.

Calling `into_iter()` on a cloneable actor field now takes an independent value.
For an explicit drain, transfer the field into a local and replace the field
with an empty vector before iterating the local. Affine actor fields require
that replacement before the handler returns.

## WebSocket waits and failure recovery

`websocket.Server.accept()` suspends through the shared native I/O lifecycle.
A scope deadline cancels the wait without closing the listener. Runtime
shutdown also cancels pending accepts and incomplete handshakes; cleanup runs
before the process exits. A cleanup fault still reports a failure and exits
non-zero.

Scope recovery and `CrashInfo.message` return the application's fault text.
For a panic raised with `panic("boom")`, the recovered message is `"boom"`.
Secondary cleanup failures remain available in the diagnostic. Command-line
failure reports retain their reason and code.

`Vec.with_capacity(...)` now produces a checker diagnostic. Use an ordinary
vector constructor; this unsupported spelling previously reached a lowering
error.
