//! Reply routing table and remote call operation tests.

use super::*;

// ── Reply routing table unit tests ─────────────────────────────────

#[test]
fn remote_ask_without_active_node_returns_null_for_nonvoid_reply() {
    let _guard = crate::runtime_test_guard();

    let saved_current_node = with_current_node(|current| {
        let saved = *current;
        *current = 0;
        saved
    });
    let _reset_current_node = ResetCurrentNode(saved_current_node);

    let local_node_id = crate::pid::hew_pid_local_node();
    let remote_node_id = if local_node_id == u16::MAX {
        u16::MAX - 1
    } else {
        local_node_id + 1
    };
    assert_ne!(remote_node_id, 0);
    assert_ne!(remote_node_id, local_node_id);

    let remote_pid = HewRemotePid::from(test_location(remote_node_id, 1));
    // SAFETY: null data with size 0 is valid; the remote path should fail
    // immediately because no active node is installed.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const remote_pid,
            test_dispatch(),
            7,
            ptr::null_mut(),
            0,
            TEST_REMOTE_ASK_TIMEOUT_MS,
            std::mem::size_of::<u64>(),
        )
    };

    assert_ne!(status, AskError::None as i32);
    assert_eq!(
        status,
        AskError::NodeNotRunning as i32,
        "ask with no active node should report NodeNotRunning"
    );
}

/// A connection-dropped failure (via `fail_connection`) must report `ConnectionDropped`.
#[test]
fn reply_table_fail_connection_sets_connection_dropped_status() {
    let _guard = crate::runtime_test_guard();

    let key = ConnectionKey {
        conn_mgr: 77,
        conn_id: 11,
    };
    let (id, pending) = reply_table().register(key);

    // Simulate a connection drop.
    reply_table().fail_connection(key);

    let guard = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let outcome = guard
        .as_ref()
        .expect("outcome should be set after fail_connection");
    assert_eq!(
        outcome.status,
        ReplyStatus::Failed,
        "fail_connection must set Failed status"
    );
    drop(guard);

    // Verify the entry was removed from the pending table.
    let map = reply_table()
        .pending
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert!(
        !map.contains_key(&id),
        "fail_connection must remove the entry"
    );
}

/// `fail_connection_with_reason` resolves matching pending asks with the
/// supplied cause (`Partition` for the SWIM-DEAD fan-out) and removes the
/// entry, distinct from the default `ConnectionDropped`.
#[test]
fn reply_table_fail_connection_with_partition_sets_partition_status() {
    let _guard = crate::runtime_test_guard();

    let key = ConnectionKey {
        conn_mgr: 88,
        conn_id: 12,
    };
    let (id, pending) = reply_table().register(key);

    reply_table().fail_connection_with_reason(key, AskError::Partition);

    let guard = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let outcome = guard
        .as_ref()
        .expect("outcome should be set after partition fan-out");
    assert_eq!(outcome.status, ReplyStatus::Failed);
    assert_eq!(
        outcome.ask_error,
        AskError::Partition,
        "SWIM-DEAD fan-out must carry the Partition cause, not ConnectionDropped"
    );
    drop(guard);

    let map = reply_table()
        .pending
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert!(
        !map.contains_key(&id),
        "partition fan-out must remove the entry"
    );
}

/// `fail_all` wakes every pending reply with `Failed` status.
#[test]
fn reply_table_fail_all_wakes_all_pending() {
    let _guard = crate::runtime_test_guard();

    let key_a = ConnectionKey {
        conn_mgr: 55,
        conn_id: 1,
    };
    let key_b = ConnectionKey {
        conn_mgr: 55,
        conn_id: 2,
    };
    let (id_a, pending_a) = reply_table().register(key_a);
    let (id_b, pending_b) = reply_table().register(key_b);

    reply_table().fail_all();

    for (id, pending) in [(id_a, &pending_a), (id_b, &pending_b)] {
        let guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let outcome = guard
            .as_ref()
            .unwrap_or_else(|| panic!("entry {id} not woken"));
        assert_eq!(outcome.status, ReplyStatus::Failed);
    }
    let map = reply_table()
        .pending
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert!(!map.contains_key(&id_a));
    assert!(!map.contains_key(&id_b));
}

#[test]
fn reply_table_register_and_complete() {
    let table = ReplyRoutingTable::new();
    let key = ConnectionKey {
        conn_mgr: 1,
        conn_id: 7,
    };
    let (id, pending) = table.register(key);
    assert!(id > 0);
    assert_eq!(pending.connection, key);

    // Complete the pending reply.
    let payload = vec![1, 2, 3, 4];
    assert!(table.complete(id, payload.clone()));

    // The condvar should be signalled and data deposited.
    let guard = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let outcome = guard.as_ref().expect("reply outcome should be set");
    assert_eq!(outcome.status, ReplyStatus::Success);
    assert_eq!(outcome.data, payload);
}

#[test]
fn remote_call_reply_wakes_and_takes_the_void_reply() {
    let _guard = crate::runtime_test_guard();
    let key = ConnectionKey {
        conn_mgr: 101,
        conn_id: 21,
    };
    let (id, readiness, call) = remote_call_for_test(key);
    // SAFETY: `call` is live and exclusively driven here.
    unsafe {
        assert_eq!(hew_remote_call_poll(call), -1, "no reply yet");
        assert!(!readiness.take_ready());
        assert!(reply_table().complete(id, Vec::new()));
        assert!(readiness.take_ready(), "the reply wakes the caller");
        assert_eq!(hew_remote_call_poll(call), 0);
        assert_eq!(
            hew_remote_call_take(call, ptr::null_mut()),
            AskError::None as i32
        );
        hew_remote_call_free(call);
    }
}

#[test]
fn remote_call_failure_edges_take_their_typed_error() {
    let _guard = crate::runtime_test_guard();
    for (conn_id, fail, expected) in [
        (
            22,
            (|id, _key| {
                reply_table().fail(id, AskError::Partition);
            }) as fn(u64, ConnectionKey),
            AskError::Partition,
        ),
        (
            23,
            |_id, key| reply_table().fail_connection(key),
            AskError::ConnectionDropped,
        ),
    ] {
        let key = ConnectionKey {
            conn_mgr: 102,
            conn_id,
        };
        let (id, readiness, call) = remote_call_for_test(key);
        fail(id, key);
        assert!(readiness.take_ready(), "a failure edge wakes the caller");
        // SAFETY: `call` is live and exclusively driven here.
        unsafe {
            assert_eq!(hew_remote_call_poll(call), 0);
            assert_eq!(hew_remote_call_take(call, ptr::null_mut()), expected as i32);
            hew_remote_call_free(call);
        }
    }
}

#[test]
fn remote_call_free_withdraws_its_registration() {
    let _guard = crate::runtime_test_guard();
    let key = ConnectionKey {
        conn_mgr: 104,
        conn_id: 24,
    };
    let (id, readiness, call) = remote_call_for_test(key);
    // SAFETY: the test owns the live call and releases it once.
    unsafe { hew_remote_call_free(call) };
    // The entry is gone: a late reply finds nothing to complete.
    assert!(!reply_table().complete(id, Vec::new()));
    assert!(!readiness.take_ready(), "a released call is never woken");
}

#[test]
fn remote_call_deadline_withdraws_and_takes_timeout() {
    let _guard = crate::runtime_test_guard();
    let key = ConnectionKey {
        conn_mgr: 105,
        conn_id: 25,
    };
    let (id, _readiness, call) = remote_call_for_test(key);
    // SAFETY: `call` is live; replace its deadline with one already due.
    unsafe {
        crate::coro_sleep::hew_coro_sleep_free((*call).timer);
        let (_latch, waker) = crate::wake::blocking::Readiness::new();
        (*call).timer = crate::coro_sleep::hew_coro_sleep_new(0, waker.descriptor());
        assert_eq!(
            hew_remote_call_poll(call),
            0,
            "an expired deadline is ready"
        );
        assert!(
            !reply_table().complete(id, Vec::new()),
            "the deadline withdrew the registration"
        );
        assert_eq!(
            hew_remote_call_take(call, ptr::null_mut()),
            AskError::Timeout as i32
        );
        hew_remote_call_free(call);
    }
}

/// Issue #2652 D12: a reply that arrives on a DIFFERENT `(conn_mgr, conn_id)`
/// than the ask was issued on must not resolve it — a peer cannot complete
/// (or reject) another peer's ask. The pending entry survives the mismatched
/// attempt and is resolvable only by the originating connection.
#[test]
fn reply_completion_requires_the_originating_connection() {
    let _guard = crate::runtime_test_guard();
    let origin = ConnectionKey::new(std::ptr::without_provenance(0x5010), 21);
    let (id, pending) = reply_table().register(origin);

    // Same request id, wrong manager → rejected, ask stays pending.
    assert!(
        !complete_remote_reply(std::ptr::without_provenance(0x9999), 21, id, &[1, 2, 3]),
        "a reply on a different conn_mgr must not complete the ask"
    );
    // Same manager, wrong conn_id → rejected, ask stays pending.
    assert!(
        !complete_remote_reply(std::ptr::without_provenance(0x5010), 99, id, &[1, 2, 3]),
        "a reply on a different conn_id must not complete the ask"
    );
    // A rejection reply on the wrong connection is likewise rejected.
    assert!(
        !fail_remote_reply(std::ptr::without_provenance(0x9999), 21, id, &[]),
        "a rejection on a different connection must not fail the ask"
    );
    assert!(
        pending.outcome.lock().unwrap().is_none(),
        "the ask must remain unresolved after mismatched attempts"
    );

    // The originating connection resolves it.
    assert!(
        complete_remote_reply(std::ptr::without_provenance(0x5010), 21, id, &[9, 9]),
        "the originating connection must complete the ask"
    );
    let guard = pending.outcome.lock().unwrap();
    let outcome = guard
        .as_ref()
        .expect("outcome set by originating connection");
    assert_eq!(outcome.status, ReplyStatus::Success);
    assert_eq!(outcome.data, vec![9, 9]);
}

#[test]
fn setup_completion_requires_the_originating_publication_token() {
    let table = ReplyRoutingTable::new();
    let connection = ConnectionKey::new(std::ptr::without_provenance(0x5050), 25);
    let (setup_id, pending) = table.register_setup(connection, RemoteSetupKind::Link, 41);

    assert!(
        !table.complete_setup_from_connection(
            setup_id,
            connection,
            RemoteSetupKind::Link,
            42,
            vec![1],
        ),
        "a successor reusing the connection slot must not complete setup"
    );
    assert!(pending.outcome.lock_or_recover().is_none());

    assert!(table.complete_setup_from_connection(
        setup_id,
        connection,
        RemoteSetupKind::Link,
        41,
        vec![2],
    ));
    assert_eq!(
        pending
            .outcome
            .lock_or_recover()
            .as_ref()
            .expect("matching publication should complete")
            .data,
        vec![2]
    );
}

#[test]
fn reply_table_complete_unknown_returns_false() {
    let table = ReplyRoutingTable::new();
    assert!(!table.complete(u64::MAX - 1, vec![42]));
}

#[test]
fn reply_table_remove_prevents_completion() {
    let table = ReplyRoutingTable::new();
    let (id, _pending) = table.register(ConnectionKey {
        conn_mgr: 1,
        conn_id: 0,
    });
    table.remove(id);
    assert!(!table.complete(id, vec![99]));
}

#[test]
fn reply_table_concurrent_complete_wakes_waiter() {
    let table = Arc::new(ReplyRoutingTable::new());
    let (id, pending) = table.register(ConnectionKey {
        conn_mgr: 1,
        conn_id: 11,
    });
    let pending_clone = Arc::clone(&pending);
    let table_clone = Arc::clone(&table);

    let handle = thread::spawn(move || {
        thread::sleep(Duration::from_millis(10));
        table_clone.complete(id, vec![10, 20]);
    });

    // Wait on the condvar.
    let mut guard = pending_clone
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let deadline = std::time::Instant::now() + Duration::from_secs(5);
    while guard.is_none() {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            break;
        }
        let (g, _) = pending_clone
            .cond
            .wait_timeout(guard, remaining)
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        guard = g;
    }
    let outcome = guard.as_ref().expect("reply outcome should be set");
    assert_eq!(outcome.status, ReplyStatus::Success);
    assert_eq!(outcome.data, vec![10, 20]);

    handle.join().expect("completer thread panicked");
}

#[test]
fn reply_table_fail_marks_failure() {
    let table = ReplyRoutingTable::new();
    let (id, pending) = table.register(ConnectionKey {
        conn_mgr: 1,
        conn_id: 13,
    });

    assert!(table.finish(
        id,
        ReplyOutcome {
            status: ReplyStatus::Failed,
            data: Vec::new(),
            ask_error: AskError::ConnectionDropped,
        },
    ));

    let guard = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let outcome = guard.as_ref().expect("reply outcome should be set");
    assert_eq!(outcome.status, ReplyStatus::Failed);
    assert!(outcome.data.is_empty());
}

#[test]
fn reply_table_fail_with_reason_propagates_correct_ask_error() {
    let _guard = crate::runtime_test_guard();

    let (id, pending) = reply_table().register(ConnectionKey {
        conn_mgr: 90,
        conn_id: 13,
    });
    assert!(reply_table().fail(id, AskError::OrphanedAsk));

    let guard = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let outcome = guard.as_ref().expect("reply outcome should be set");
    assert_eq!(outcome.status, ReplyStatus::Failed);
    assert_eq!(outcome.ask_error, AskError::OrphanedAsk);
    assert!(outcome.data.is_empty());
}

#[test]
fn send_failure_resolves_registered_reply_as_connection_dropped() {
    // `setup_remote_ask` registers a pending reply BEFORE the outbound send;
    // on a fail-closed send (for example SIGPIPE→EPIPE), the connection
    // fan-out must own the outcome so this path and reader cleanup cannot
    // publish different failure codes. This pins that contract on a fresh
    // table, isolated from the process-global one.
    let table = ReplyRoutingTable::new();
    assert_eq!(table.pending_len(), 0);

    let connection = ConnectionKey {
        conn_mgr: 42,
        conn_id: 7,
    };
    let (_request_id, pending) = table.register(connection);
    assert_eq!(
        table.pending_len(),
        1,
        "register must install exactly one pending reply slot"
    );

    // Mirror `setup_remote_ask`'s `!send_ok` branch.
    table.fail_connection(connection);
    assert_eq!(
        table.pending_len(),
        0,
        "connection-drop cleanup must not leak the reply slot"
    );
    let guard = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let outcome = guard.as_ref().expect("send failure must resolve the ask");
    assert_eq!(outcome.status, ReplyStatus::Failed);
    assert_eq!(outcome.ask_error, AskError::ConnectionDropped);
    assert!(outcome.data.is_empty());
}

#[test]
fn fail_remote_reply_empty_payload_defaults_to_worker_at_capacity() {
    let _guard = crate::runtime_test_guard();

    let (id, pending) = reply_table().register(ConnectionKey {
        conn_mgr: 91,
        conn_id: 14,
    });
    assert!(fail_remote_reply(
        std::ptr::without_provenance(91),
        14,
        id,
        &[]
    ));

    let guard = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let outcome = guard.as_ref().expect("reply outcome should be set");
    assert_eq!(outcome.status, ReplyStatus::Failed);
    assert_eq!(outcome.ask_error, AskError::WorkerAtCapacity);
    assert!(outcome.data.is_empty());
}

#[test]
fn remote_reply_payload_length_mismatch_fails_closed_not_overread() {
    // A peer's reply payload whose length differs from the caller's static
    // `reply_size` must fail closed (null → PayloadSizeMismatch), never be
    // handed to the codegen typed-load that reads `reply_size` bytes.
    // Short payload: 1 byte where the caller expects 4 — the over-read case.
    assert!(
        remote_reply_data_to_ptr(&[0xAB], 4).is_null(),
        "short reply payload must fail closed, not be read past its allocation"
    );
    // Long payload: 8 bytes where the caller expects 4 — silent-truncation case.
    assert!(
        remote_reply_data_to_ptr(&[0u8; 8], 4).is_null(),
        "over-long reply payload must fail closed, not be silently truncated"
    );
    // Exact match still succeeds (non-null, owned buffer the caller frees).
    let ok = remote_reply_data_to_ptr(&[1u8, 2, 3, 4], 4);
    assert!(!ok.is_null(), "exact-size reply payload must succeed");
    // SAFETY: ok came from remote_reply_data_to_ptr's sized-block allocation; free it once.
    unsafe { crate::mem::buf_free(ok) };
}

#[test]
fn rejection_reason_codes_round_trip_supported_remote_failures() {
    for ask_error in [
        AskError::WorkerAtCapacity,
        AskError::ActorStopped,
        AskError::MailboxFull,
        AskError::OrphanedAsk,
        AskError::NoRunnableWork,
    ] {
        let payload = [AskRejectionReasonCode::encode(ask_error)
            .expect("supported remote ask failure must encode to a wire code")];
        assert_eq!(
            decode_rejection_reason(&payload).expect("known code must decode"),
            ask_error,
            "encoded rejection reason must round-trip through the wire payload"
        );
    }
}

#[test]
fn rejection_reason_codes_reject_non_remote_failures() {
    for ask_error in [
        AskError::None,
        AskError::NodeNotRunning,
        AskError::RoutingFailed,
        AskError::EncodeFailed,
        AskError::SendFailed,
        AskError::Timeout,
        AskError::ConnectionDropped,
        AskError::PayloadSizeMismatch,
    ] {
        assert!(
            AskRejectionReasonCode::encode(ask_error).is_none(),
            "{ask_error:?} must not be emitted as a remote rejection-reason code"
        );
    }
    assert_eq!(
        decode_rejection_reason(&[AskError::Timeout as u8]),
        Err(AskRejectionDecodeError::UnknownAskRejectionReason {
            code: AskError::Timeout as u8
        }),
        "unknown rejection-reason bytes must produce Err, not a fabricated AskError"
    );
}

#[test]
fn fail_remote_reply_unknown_code_returns_false_and_leaves_ask_unresolved() {
    let _guard = crate::runtime_test_guard();

    let (id, pending) = reply_table().register(ConnectionKey {
        conn_mgr: 92,
        conn_id: 15,
    });
    assert!(!fail_remote_reply(
        std::ptr::without_provenance(92),
        15,
        id,
        &[0xFF]
    ));

    let guard = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert!(
        guard.is_none(),
        "unknown code must not resolve the pending ask"
    );
}

#[test]
fn reply_table_fail_all_wakes_waiter() {
    let table = Arc::new(ReplyRoutingTable::new());
    let (_id, pending) = table.register(ConnectionKey {
        conn_mgr: 1,
        conn_id: 17,
    });
    let pending_clone = Arc::clone(&pending);
    let table_clone = Arc::clone(&table);

    let handle = thread::spawn(move || {
        let mut guard = pending_clone
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let deadline = std::time::Instant::now() + Duration::from_secs(5);
        while guard.is_none() {
            let remaining = deadline.saturating_duration_since(std::time::Instant::now());
            assert!(!remaining.is_zero(), "waiter timed out before fail_all");
            let (new_guard, _) = pending_clone
                .cond
                .wait_timeout(guard, remaining)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            guard = new_guard;
        }
        let outcome = guard.take().expect("reply outcome should be set");
        assert_eq!(outcome.status, ReplyStatus::Failed);
        assert!(outcome.data.is_empty());
    });

    thread::sleep(Duration::from_millis(10));
    table_clone.fail_all();
    handle.join().expect("waiter thread panicked");
}

#[test]
fn reply_table_fail_connection_only_wakes_matching_waiters() {
    let table = ReplyRoutingTable::new();
    let failed_key = ConnectionKey {
        conn_mgr: 1,
        conn_id: 21,
    };
    let (_failed_id, failed_pending) = table.register(failed_key);
    let (_unrelated_id, unrelated_pending) = table.register(ConnectionKey {
        conn_mgr: 2,
        conn_id: 21,
    });

    table.fail_connection(failed_key);

    let failed_guard = failed_pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let failed_outcome = failed_guard.as_ref().expect("matching waiter should fail");
    assert_eq!(failed_outcome.status, ReplyStatus::Failed);
    assert!(failed_outcome.data.is_empty());
    drop(failed_guard);

    let unrelated_guard = unrelated_pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert!(
        unrelated_guard.is_none(),
        "non-matching waiter must remain pending"
    );
}
