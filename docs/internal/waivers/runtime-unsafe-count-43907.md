# hew-runtime unsafe-count waiver


hew-runtime unsafe-count waiver â 43,784 â 43,907

Reviewed at eca4abb94 (integration/rc3-wave-1). Measured with cargo-geiger
0.13.0 via scripts/check-runtime-unsafe-count.py's own counter set
(used+unused Ã functions/exprs/item_impls/item_traits/methods):
  origin/main   43,545
  4f027732b     43,884   (+339 over main)
  eca4abb94     43,907   (+362 over main, +123 over the standing ceiling)

New ceiling: 43,907 â the measured value, with ZERO headroom. The previous
ceiling carried 239 counts of slack over main; that slack is precisely how the
+100 in group A landed without ever passing an unsafe review. Re-baseline to
the measured value at every waiver from here.

GROUP A â +100, parent range origin/main..4f027732b (ChildRef + cooperative
send-contract). Not authored by the three commits under review; waived here
because the ceiling is a whole-crate ratchet and cannot be raised piecemeal.
  What it is:
   â¢ actor.rs:4086-4155 â hew_actor_await_send_by_id / actor_await_send_pinned /
     hew_actor_detach_await_send_by_id. Every deref is inside a
     with_actor_send_by_id pin; the pin is scoped to mailbox registration only,
     never across the suspension. Fail-closed on foreign runtime and terminal
     state BEFORE touching the mailbox (actor.rs:4121-4126).
   â¢ supervisor.rs:8929-9037 â hew_supervisor_role_send /
     hew_supervisor_role_await_send. No actor pointer escapes resolution; both
     dispatch inside with_actor_send_by_identity and return ErrActorStopped when
     the incarnation retired mid-submission. out_actor_id is null-checked and
     zero-initialised before any resolution (supervisor.rs:9000-9006).
   â¢ supervisor.rs:10635-10675 â hew_local_pid_supervisor_pool_child_ref_get.
     The returned word is a static-slot INDEX carried through
     with_exposed_provenance_mut, documented and confirmed never dereferenced;
     liveness is deferred to the role send/ask, which fails closed.
   â¢ mailbox.rs â the BlockedSender FIFO: registration under slow_path +
     blocked_senders with a closed-recheck (mailbox.rs:2831-2838), and drain
     paths at 3320-3332 / 3436-3441 / 3674-3683. I verified node and slot
     ownership is transferred exactly once on each of the three edges: try_recv
     hands the node to enqueue_bounded_slow_path_node and does NOT free it;
     close/free free the node first and wake after; wake_blocked_sender frees
     only the slot ref. No double free, no leak.
   â¢ mailbox_wasm.rs:754-768 â hew_mailbox_send_fire_and_forget, one deref of a
     caller-supplied mailbox.
  Checkable property covering the whole group: clippy passes the crate INCLUDING
  tests under -D clippy::undocumented_unsafe_blocks -D warnings, so every added
  block carries a SAFETY comment.

GROUP B â +17, a9c7fb246, ABA identity-wake plumbing. This is the group the
waiver exists for and it REDUCES risk. BlockedSender drops the raw sender
pointer for (actor_id, spawn_serial) (mailbox.rs:1411-1420, 2836-2847), and
wake_blocked_sender resolves through with_actor_send_by_identity, holding the
ActorPin across enqueue_resume (mailbox.rs:2962-2971).
  READ THIS BEFORE RE-WAIVING: the spawn_serial check is NOT what closes the
  ABA. On native the id already packs the serial (pid.rs:38-40,165) and on wasm
  the id IS the serial (actor.rs:3108-3112), so the serial adds zero
  discriminating power over the id for any production-spawned actor; the
  allocator refuses past MAX_SPAWN_SERIAL rather than wrapping
  (actor.rs:1789-1794), making the collision unreachable outside the cfg(test)
  override. What closes it is (i) an id-keyed registry whose ids are never
  reissued, and (ii) the pin held across enqueue_resume, which is what makes
  that function's internal pointer-only with_live_actor sound AT THIS SITE. A
  future reader must not waive a similar change on "the serial fixed it".
  Pin safety re-verified against all three free paths: each untracks under
  LIVE_ACTORS, then drains send_pin_count with the lock released
  (actor.rs:4760/4783, 4906/4917, 2400/2406) â no deadlock, no address reuse
  under a live pin, and fail-closed leak rather than free on drain timeout.

GROUP C â +3, 2f0c8659a, WASM by-ID send metric. hew_mailbox_send_fire_and_forget
records record_message_sent() on Enqueued | DroppedOld
(mailbox_wasm.rs:765-768). Matches the FFI sibling send_user_message
(mailbox_wasm.rs:749) and the native policy (mailbox.rs:2079, 2188), including
the shared decision not to count Coalesced. No double-count: the only caller is
actor_send_by_id_wasm_internal (actor.rs:4189), which does not also reach
send_user_message. The SendOutcome pub(crate) widening is signature-forced and
crate-internal. Zero new deref.

GROUP D â +3 net, eca4abb94, narrowing. Two genuine contract shrinks:
hew_mailbox_send_fire_and_forget takes &mut HewMailboxWasm instead of *mut, so
its unsafe contract drops the mailbox clause (mailbox_wasm.rs:757); and
with_actor_send_by_identity replaces unsafe{(*pin.as_ptr()).spawn_serial} with
pin.actor().spawn_serial and hands f an &ActorPin proof token
(live_actors.rs:540-549). Unification is 1-of-3: actor_await_send_pinned
(actor.rs:4112) and ask_with_channel_pinned (actor.rs:6161) still take *mut, and
their callers unwrap with pin.as_ptr() at supervisor.rs:9009/9099 â finish that
in a follow-up. The rest is #[cfg(test)] mailbox test bodies split per operation.
  The +3 is NOT the narrowing tax it looks like. Measured on cargo-geiger 0.13.0:
  splitting a broad block into operation-local blocks is count-REDUCING in a safe
  fn (11 unsafe exprs â 9, 2 reclassified safe) and count-NEUTRAL in an unsafe fn
  (11 â 11, because the whole unsafe-fn body is one unsafe scope). The +3 comes
  from re-deref restructuring â actor.rs:4189 now materialises the mailbox
  reference explicitly where the old form derefed inline. The metric does not
  penalise hygiene; do not use "narrowing inflates the count" as grounds for a
  future waiver.

Known metric limitations, recorded so this ceiling is read correctly:
 (a) narrowing INSIDE an unsafe fn registers as zero improvement;
 (b) cfg(test) scaffolding counts identically to production â 4,311 of the
     counted exprs sit in the `unused` bucket at this revision.

Remediation that must land with the merge: update
.github/hew-runtime-unsafe-count.txt to total=43907. (Not done in this review â
the reviewed worktree is read-only.)

NOT waived, tracked as a follow-up lane, not a blocker: 11 enqueue_resume call
sites still resolve pointer-only through with_live_actor â
supervisor.rs:1838,2454; reactor.rs:1053,1147,1288,1315; await_cancel.rs:226;
channel_core.rs:248; task_scope.rs:933,1118; reply_channel.rs:419;
hew_node.rs:824. Residual defect is a wrong-incarnation wake (a replacement CAS'd
SuspendedâRunnable with no readiness behind it â the fabricated-timeout path at
scheduler.rs:1310-1315), NOT a use-after-free: with_live_actor never derefs an
untracked pointer. Three of these carry SAFETY comments still asserting
"enqueue_resume re-validates waiter.actor", which is now the weaker claim; update
them when the sites are converted. Also: mailbox_detach_await_send
(mailbox.rs:2926) still matches its waiter by slot ADDRESS, safe only because the
detaching caller holds the creator ref â convention, not construction.
