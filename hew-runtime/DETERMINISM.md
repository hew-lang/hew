# Test determinism — the contract

A test outcome MUST be a pure function of `(code, inputs, seed)`. It MUST NOT
depend on wall-clock time, machine load, or thread-scheduling order. Real time
enters a test only through a simulated/injectable clock. Assertions are on
logical events and counts, never on elapsed durations. Network tests bind
OS-ephemeral ports and read the port back; cross-process tests use a readiness
handshake; filesystem tests use per-test temp dirs. Every `retries = N` and
every flake-driven `slow-timeout` widening is determinism-debt to be eliminated,
not a permanent setting.

**The ONLY re-runnable CI fault is a verified physical-runner break** — an
OOM-kill, a truly exhausted disk (ENOSPC), a dead VM, or a network partition.
Every timing, scheduling, or race outcome is a defect we own and fix. A test
that passes in isolation but fails under load *always had the race*; load merely
exposed it. A passing re-run is not evidence of a non-defect — it is evidence
the race is intermittent.

## The rules

1. **No wall-DURATION assertion.** Never `assert!(elapsed < N_ms)` /
   `started.elapsed() < ...`. Assert the typed logical OUTCOME instead — the ask
   that exceeds its deadline yields `AskError::Timeout`, not "took < 1500ms".
   The operation's own timeout plus nextest's `slow-timeout` are the hang
   ceiling; an `elapsed <` window is flake padding.

2. **A `sleep(N)` used as synchronization is a smell.** Replace it with a wait
   that returns the instant the event happens: a blocking read, `Thread::join`,
   `mpsc::recv`, or a condvar `wait_for(n, MAX)`. Example: after `t.join()` on a
   peer thread that set `SO_LINGER=0` and dropped its socket, the RST is already
   sent — a "give the RST time to arrive" sleep is redundant, the blocking
   `read()` is the synchronization.

3. **Wait on the asserted event, not a correlated sibling.** A condvar wait is
   deterministic only if it waits on the *same* logical event the assertion
   checks. Observing a monitor `DOWN` signal does NOT imply the crash log was
   pushed — the push runs on the crashing worker thread *after* the trap that
   delivers `DOWN`, on a different and unordered path. Likewise a `DOWN` does not
   imply the supervisor's circuit-breaker state transitioned. Wait on the count /
   state you are about to assert (to its target value, with a hang ceiling),
   never on a sibling signal you *hope* is ordered before it.

4. **Ephemeral ports + readiness handshakes for cross-process tests.** Bind
   `127.0.0.1:0`, read the chosen port back from `local_addr()` (or have the
   child emit it), and hand the live socket — never a re-derived address — to the
   consumer. The server signals readiness (a pipe token, a stdout line, or a
   ready-file) and the client waits for that signal; never a fixed start-up
   sleep, never a connect-with-backoff-to-a-wall-deadline (a wider window is not
   determinism).

5. **A logical-event wait, not a timing bet.** A poll loop that returns the
   instant a count or state reaches its target, bounded by a generous ceiling,
   is a logical wait: it succeeds immediately when the event occurs and only
   reaches the ceiling when the event genuinely never happens (a real hang). A
   fixed `sleep`, an `elapsed <` assertion, or a budget tuned to "usually long
   enough under load" is a timing bet. Prefer a condvar `wait_for(n, MAX)` over a
   poll loop where the producing path can signal.

6. **Retries and widened timeouts are debt, not settings.** Any `retries = N` or
   flake-driven `slow-timeout` widening is a SHIM that MUST carry WHY /
   WHEN-obsolete / WHAT-the-real-fix-is and a tracked root-fix item, and is
   removed the moment its test is fixed. An un-annotated retry is rejected in
   review. A load-, timing-, or coverage-exposed failure is fixed, never retried.

## The seam catalogue (what exists today)

- **Simulated clock** — `hew_simtime_enable` / `hew_simtime_advance_ms` /
  `hew_simtime_now_ms` plus the `SimtimeGuard` RAII guard
  (`hew-runtime/src/deterministic.rs`). `hew_now_ms()` / `hew_instant_now()`
  dispatch through it, so the **timer-wheel and cluster timestamp paths** are
  sim-advanceable. NOTE: the remote-ask deadline (`hew_node.rs`
  `spawn_remote_ask_timeout`, the blocking-ask `Instant` deadline) and
  `hew_sleep_ms` (`io_time.rs`) are NOT yet sim-aware — sim-advance has no effect
  there. Simtime is also a per-process global; a child subprocess does not
  inherit the parent's sim clock.
- **Condvar wait-for-count** — the `wait_for(n, MAX)` / `wait_for_down_count` /
  `wait_for_total_dispatches` pattern (`supervision_lifecycle.rs`,
  `ffi_boundary.rs`) and the FFI `hew_supervisor_wait_restart`. Block on a
  logical count; fail fast only on a true hang.
- **Seeded scheduler PRNG** — `hew_deterministic_set_seed` makes work-stealing
  victim selection reproducible. It must be set **before** `hew_sched_init`; once
  the scheduler is initialised (e.g. via `ensure_scheduler()`'s `Once`), a later
  seed call is a no-op for victim selection. It does NOT make actor interleaving
  relative to I/O, timers, or preemption deterministic — the condvar
  wait-on-logical-event is the real mitigation for interleaving races.
- **Ephemeral ports** — bind `:0`, read `local_addr().port()`, hand off the live
  socket.
- **Cross-process readiness** — the ready-file / port-handshake pattern
  (`hew_node.rs` `wait_for_ready_port`): the server writes its bound port to a
  file only after it has bound and registered; the client waits for that file.

## Deciding INFRA vs DETERMINISM

When a CI test fails, ask:

- **Was the runner alive throughout?** If it OOM-killed, ran out of disk, the VM
  died, or the network partitioned — that is INFRA, the one re-runnable class.
- **Does the result change with load?** If the same code passes quiet and fails
  under load or coverage instrumentation, that is a race we own — DETERMINISM.
- **Is there a timing or scheduling bet?** A `sleep`, an `elapsed <`, a budget
  "usually long enough", or a wait on a sibling signal — all DETERMINISM defects
  to fix, never to retry.
