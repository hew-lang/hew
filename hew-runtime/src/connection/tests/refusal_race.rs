//! Refusal racing removal, manager free and guarded deliveries.

use super::*;

/// Route slot the refused peer occupies. It is also the harness routing
/// table's own local slot, which is what makes `hew_routing_add_route`
/// refuse the route and drive publication into the refusal path.
const RACE_ROUTE_SLOT: u16 = 2;

/// A transport whose `close_conn` records every close it is handed and
/// holds the FIRST close of one chosen connection open until the test
/// releases it.
///
/// That park is the interleaving seam. Every teardown path closes while the
/// actor is still installed, so sitting inside the close leaves a second
/// path a real window in which it finds the connection, concludes it is
/// closing it, and frees the same one-shot handle.
struct RacingCloseTransport {
    /// Every `conn_id` handed to `close_conn`, in call order.
    closes: Mutex<Vec<c_int>>,
    park_conn: c_int,
    entered_park: std::sync::mpsc::Sender<()>,
    release_park: Mutex<std::sync::mpsc::Receiver<()>>,
    /// Mirrors a real transport: a close unblocks that connection's reader.
    readers: Mutex<HashMap<c_int, std::sync::mpsc::Sender<()>>>,
    /// Connection whose `send` parks until the test releases it, or 0.
    ///
    /// A send parked here is a genuine claimed send in flight: it holds the
    /// connection's `ClaimedSendLease` for as long as the transport call
    /// has not returned, which is exactly the production condition manager
    /// free waits out after its close phase.
    park_send_conn: AtomicI32,
    entered_send_park: std::sync::mpsc::Sender<()>,
    release_send_park: Mutex<std::sync::mpsc::Receiver<()>>,
}

impl RacingCloseTransport {
    fn closes_of(&self, conn_id: c_int) -> usize {
        self.closes
            .lock_or_recover()
            .iter()
            .filter(|closed| **closed == conn_id)
            .count()
    }
}

unsafe extern "C" fn racing_close_conn(impl_ptr: *mut std::ffi::c_void, conn_id: c_int) {
    // SAFETY: the teardown-race tests install a RacingCloseTransport as the
    // transport impl payload and keep it alive past every close.
    let transport = unsafe { &*(impl_ptr.cast::<RacingCloseTransport>()) };
    let first_close_of_conn = {
        let mut closes = transport.closes.lock_or_recover();
        let first = !closes.contains(&conn_id);
        closes.push(conn_id);
        first
    };
    if conn_id == transport.park_conn && first_close_of_conn {
        let _ = transport.entered_park.send(());
        let _ = transport.release_park.lock_or_recover().recv();
    }
    if let Some(reader) = transport.readers.lock_or_recover().get(&conn_id) {
        let _ = reader.send(());
    }
}

unsafe extern "C" fn racing_send(
    impl_ptr: *mut std::ffi::c_void,
    conn_id: c_int,
    _data: *const std::ffi::c_void,
    len: usize,
) -> c_int {
    // SAFETY: the teardown-race tests install a RacingCloseTransport as the
    // transport impl payload and keep it alive past every send.
    let transport = unsafe { &*(impl_ptr.cast::<RacingCloseTransport>()) };
    if transport.park_send_conn.load(Ordering::Acquire) == conn_id {
        let _ = transport.entered_send_park.send(());
        let _ = transport.release_send_park.lock_or_recover().recv();
    }
    c_int::try_from(len).unwrap_or(c_int::MAX)
}

struct TeardownRace {
    mgr: *mut HewConnMgr,
    cluster: *mut crate::cluster::HewCluster,
    routing_table: *mut HewRoutingTable,
    transport: *mut HewTransport,
    transport_impl: *mut RacingCloseTransport,
    ops: Box<crate::transport::HewTransportOps>,
    entered_park: std::sync::mpsc::Receiver<()>,
    release_park: std::sync::mpsc::Sender<()>,
    entered_send_park: std::sync::mpsc::Receiver<()>,
    release_send_park: std::sync::mpsc::Sender<()>,
}

struct StagedConn {
    token: u64,
    publication_sync: Arc<Mutex<()>>,
    publication_removed: Arc<AtomicBool>,
    superseded: Option<LiveClaim>,
    reader_done: Option<std::sync::mpsc::Receiver<()>>,
}

/// Stage the production refusal shape: a cluster the peer has joined, a
/// routing table whose local slot collides with the peer's (so publication
/// refuses the route), reconnect armed, and a transport that parks the
/// first close of `park_conn`.
fn stage_teardown_race(park_conn: c_int) -> TeardownRace {
    let cluster_config = crate::cluster::ClusterConfig {
        local_node_id: 1,
        ..crate::cluster::ClusterConfig::default()
    };
    // SAFETY: `cluster_config` is a live local for the duration of the call.
    let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cluster_config) };
    assert!(!cluster.is_null());
    // SAFETY: `cluster` was just allocated.
    let joined = unsafe {
        crate::cluster::hew_cluster_join(cluster, RACE_ROUTE_SLOT, c"10.0.0.2:9000".as_ptr())
    };
    assert_eq!(joined, 0);

    let (entered_tx, entered_rx) = std::sync::mpsc::channel::<()>();
    let (release_tx, release_rx) = std::sync::mpsc::channel::<()>();
    let (entered_send_tx, entered_send_rx) = std::sync::mpsc::channel::<()>();
    let (release_send_tx, release_send_rx) = std::sync::mpsc::channel::<()>();
    let transport_impl = Box::into_raw(Box::new(RacingCloseTransport {
        closes: Mutex::new(Vec::new()),
        park_conn,
        entered_park: entered_tx,
        release_park: Mutex::new(release_rx),
        readers: Mutex::new(HashMap::new()),
        park_send_conn: AtomicI32::new(0),
        entered_send_park: entered_send_tx,
        release_send_park: Mutex::new(release_send_rx),
    }));
    let ops = Box::new(crate::transport::HewTransportOps {
        connect: None,
        listen: None,
        accept: None,
        send: Some(racing_send),
        recv: None,
        close_conn: Some(racing_close_conn),
        destroy: None,
    });
    let transport = Box::into_raw(Box::new(HewTransport {
        ops: &raw const *ops,
        r#impl: transport_impl.cast::<std::ffi::c_void>(),
    }));

    // SAFETY: the routing table, cluster and transport above are live and
    // outlive the manager, which every test frees before `finish`.
    let (routing_table, mgr) = unsafe {
        let routing_table = crate::routing::hew_routing_table_new_for_test(RACE_ROUTE_SLOT);
        assert!(!routing_table.is_null());
        let mgr = hew_connmgr_new(transport, None, routing_table, cluster, 1);
        assert!(!mgr.is_null());
        // Arm reconnect so a refusal mistaken for an unexpected drop would
        // schedule a retry of the rejected peer.
        (&*mgr).reconnect_enabled.store(true, Ordering::Release);
        (routing_table, mgr)
    };

    TeardownRace {
        mgr,
        cluster,
        routing_table,
        transport,
        transport_impl,
        ops,
        entered_park: entered_rx,
        release_park: release_tx,
        entered_send_park: entered_send_rx,
        release_send_park: release_send_tx,
    }
}

impl TeardownRace {
    fn transport_impl(&self) -> &RacingCloseTransport {
        // SAFETY: allocated in `stage_teardown_race`, freed only in `finish`.
        unsafe { &*self.transport_impl }
    }

    fn mgr(&self) -> &HewConnMgr {
        // SAFETY: the manager is live until the test frees it.
        unsafe { &*self.mgr }
    }

    /// Install a hand-built actor the way `hew_connmgr_add` would, with a
    /// reserved identity claim and (optionally) a reader parked in `recv()`
    /// until a close wakes it into the real `reader_cleanup`.
    fn stage_connection(&self, conn_id: c_int, route_slot: u16, reader: bool) -> StagedConn {
        let mgr = self.mgr();
        let mut actor = ConnectionActor::new(conn_id);
        let token =
            next_publication_token(mgr).expect("the publication token space is not exhausted");
        actor.publication_token = token;
        actor.peer_node_id = route_slot;
        actor.transport = self.transport;
        actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        actor.reconnect = Some(ReconnectSettings {
            target_addr: "10.0.0.2:9000".to_owned(),
            max_retries: 3,
            expected_node_id: Some(route_slot),
        });
        let publication_sync = Arc::clone(&actor.publication_sync);
        let publication_removed = Arc::clone(&actor.publication_removed);
        let reader_done = reader.then(|| {
            let (wake_tx, wake_rx) = std::sync::mpsc::channel::<()>();
            let (done_tx, done_rx) = std::sync::mpsc::channel::<()>();
            self.transport_impl()
                .readers
                .lock_or_recover()
                .insert(conn_id, wake_tx);
            let reader_stop = Arc::clone(&actor.reader_stop);
            let mgr_send = SendConnMgr(self.mgr);
            actor.reader_handle = Some(std::thread::spawn(move || {
                let mgr_send = mgr_send;
                let _ = wake_rx.recv();
                reader_cleanup(mgr_send.0, conn_id, &reader_stop);
                let _ = done_tx.send(());
            }));
            done_rx
        });
        mgr.connections.access(|conns| conns.push(actor));
        let superseded = test_reserve_unverified(mgr, route_slot, conn_id, token);
        StagedConn {
            token,
            publication_sync,
            publication_removed,
            superseded,
            reader_done,
        }
    }

    /// Block until the racing teardown has reached the close of `conn_id`:
    /// either parked behind the owner's claim, or — if the claim were a
    /// plain store again — straight through it with a close of its own.
    /// Either way the window is real when the park is released.
    fn await_racing_close(&self, conn_id: c_int) {
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
        loop {
            let waiting = self.mgr().connections.access(|connections| {
                connections
                    .iter()
                    .find(|connection| connection.conn_id == conn_id)
                    .map_or(0, |connection| connection.transport_close.waiters())
            });
            if waiting > 0 || self.transport_impl().closes_of(conn_id) > 1 {
                return;
            }
            assert!(
                std::time::Instant::now() < deadline,
                "the racing teardown never reached the close of conn {conn_id}"
            );
            std::thread::yield_now();
        }
    }

    fn await_park_entered(&self) {
        self.entered_park
            .recv_timeout(std::time::Duration::from_secs(10))
            .expect("a teardown path must take the close and park inside it");
    }

    fn release_park(&self) {
        self.release_park
            .send(())
            .expect("the parked close must still be waiting");
    }

    /// Park the transport `send` of `conn_id`, then start a real claimed
    /// send on it and block until it is inside that transport call.
    ///
    /// Returns the sending thread; the caller releases it with
    /// [`TeardownRace::release_send_park`] and joins it. While it is
    /// parked the connection holds a genuine `ClaimedSendLease`, which is
    /// the production condition manager free waits out after its close
    /// phase.
    fn park_claimed_send(
        &self,
        conn_id: c_int,
        publication_token: u64,
    ) -> std::thread::JoinHandle<c_int> {
        self.transport_impl()
            .park_send_conn
            .store(conn_id, Ordering::Release);
        let mgr = SendConnMgr(self.mgr);
        let sender = std::thread::spawn(move || {
            let mgr = mgr;
            let payload = [7_u8; 8];
            // SAFETY: the manager cannot be freed while this claimed send
            // is in flight, and the payload outlives the call.
            unsafe {
                send_preencoded_on_manager(
                    &*mgr.0,
                    conn_id,
                    Some(publication_token),
                    payload.as_ptr(),
                    payload.len(),
                )
            }
        });
        self.entered_send_park
            .recv_timeout(std::time::Duration::from_secs(10))
            .expect("the claimed send must reach the transport and park there");
        sender
    }

    fn release_send_park(&self) {
        self.release_send_park
            .send(())
            .expect("the parked send must still be waiting");
    }

    /// Free everything the harness owns.
    ///
    /// # Safety
    ///
    /// The manager must already have been freed by the caller.
    unsafe fn finish(self) {
        // SAFETY: all four were allocated in `stage_teardown_race` and the
        // manager that referenced them is gone.
        unsafe {
            crate::cluster::hew_cluster_free(self.cluster);
            crate::routing::hew_routing_table_free(self.routing_table);
            drop(Box::from_raw(self.transport));
            drop(Box::from_raw(self.transport_impl));
        }
        drop(self.ops);
    }
}

/// (a) A refusal and an ordinary `hew_connmgr_remove` reach the same
/// connection together. Both find it installed, both conclude it is theirs
/// to tear down — and the transport handle is one-shot. The production
/// transport must see EXACTLY ONE close.
///
/// Counterfactual: with the close announced rather than acquired
/// (`transport_closed.store(true)` followed by an unconditional
/// `close_transport_conn`, as on main), the remove that arrives while the
/// refusal is inside the close stores a flag nobody reads and frees the
/// handle a second time — this asserts 2 closes instead of 1.
#[test]
fn refusal_racing_remove_closes_the_transport_exactly_once() {
    const CONN: c_int = 41;

    let race = stage_teardown_race(CONN);
    let staged = race.stage_connection(CONN, RACE_ROUTE_SLOT, true);

    let refusal_mgr = SendConnMgr(race.mgr);
    let token = staged.token;
    let publication_sync = Arc::clone(&staged.publication_sync);
    let publication_removed = Arc::clone(&staged.publication_removed);
    let superseded = staged.superseded;
    let refusal = std::thread::spawn(move || {
        let refusal_mgr = refusal_mgr;
        // SAFETY: the manager outlives both racing threads.
        let mgr = unsafe { &*refusal_mgr.0 };
        publish_connection_established(
            mgr,
            RACE_ROUTE_SLOT,
            CONN,
            HEW_FEATURE_SUPPORTS_GOSSIP,
            token,
            &publication_sync,
            &publication_removed,
            superseded,
        );
    });
    race.await_park_entered();

    let remove_mgr = SendConnMgr(race.mgr);
    let remove = std::thread::spawn(move || {
        let remove_mgr = remove_mgr;
        // SAFETY: the manager outlives both racing threads.
        unsafe { hew_connmgr_remove(remove_mgr.0, CONN) }
    });
    race.await_racing_close(CONN);
    race.release_park();

    refusal.join().expect("refusal thread should not panic");
    assert_eq!(
        remove.join().expect("remove thread should not panic"),
        0,
        "the racing remove must report the connection removed"
    );
    staged
        .reader_done
        .expect("the refused connection is staged with a reader")
        .recv_timeout(std::time::Duration::from_secs(10))
        .expect("the woken reader must have run its cleanup");

    assert_eq!(
        race.transport_impl().closes_of(CONN),
        1,
        "a refusal racing a remove must close the one-shot transport exactly once"
    );
    // SAFETY: the manager is still live.
    assert_eq!(unsafe { hew_connmgr_count(race.mgr) }, 0);
    assert_eq!(
        race.mgr().reconnect_workers.access(|workers| workers.len()),
        0,
        "a refused peer must never be scheduled for reconnect"
    );

    // SAFETY: the manager is live and is not used again.
    unsafe { hew_connmgr_free(race.mgr) };
    assert_eq!(
        race.transport_impl().closes_of(CONN),
        1,
        "manager free must not close an already-torn-down connection again"
    );
    // SAFETY: the manager has been freed.
    unsafe { race.finish() };
}

/// (b) The same connection is refused twice concurrently — the shape a
/// reader-driven removal racing a publication refusal produces. Only one
/// refusal may own the close.
///
/// Counterfactual: with the close announced rather than acquired, the
/// second refusal marks and closes on its own while the first is still
/// inside `close_conn` — this asserts 2 closes instead of 1.
#[test]
fn duplicate_refusal_closes_the_transport_exactly_once() {
    const CONN: c_int = 42;

    let race = stage_teardown_race(CONN);
    let staged = race.stage_connection(CONN, RACE_ROUTE_SLOT, true);
    // Publish the peer into the cluster under this token, so both refusals
    // have live membership to retire, not just a connection to unlink.
    // SAFETY: the cluster is live for the whole test.
    let established = unsafe {
        crate::cluster::hew_cluster_notify_connection_established_for_token_if_not_removed(
            race.cluster,
            RACE_ROUTE_SLOT,
            1,
            staged.token,
            &staged.publication_sync,
            &staged.publication_removed,
        )
    };
    assert_eq!(established, 1);

    let token = staged.token;
    let refusals = (0..2)
        .map(|_| {
            let refusal_mgr = SendConnMgr(race.mgr);
            std::thread::spawn(move || {
                let refusal_mgr = refusal_mgr;
                // SAFETY: the manager outlives both racing threads.
                let mgr = unsafe { &*refusal_mgr.0 };
                refuse_established_publication(mgr, RACE_ROUTE_SLOT, CONN, token);
            })
        })
        .collect::<Vec<_>>();
    race.await_park_entered();
    race.await_racing_close(CONN);
    race.release_park();

    for refusal in refusals {
        refusal.join().expect("refusal thread should not panic");
    }
    staged
        .reader_done
        .expect("the refused connection is staged with a reader")
        .recv_timeout(std::time::Duration::from_secs(10))
        .expect("the woken reader must have run its cleanup");

    assert_eq!(
        race.transport_impl().closes_of(CONN),
        1,
        "two refusals of one connection must close the one-shot transport exactly once"
    );
    // SAFETY: the manager is still live.
    assert_eq!(unsafe { hew_connmgr_count(race.mgr) }, 0);

    // SAFETY: the manager is live and is not used again.
    unsafe { hew_connmgr_free(race.mgr) };
    assert_eq!(race.transport_impl().closes_of(CONN), 1);
    // SAFETY: the manager has been freed.
    unsafe { race.finish() };
}

/// (c) A refusal is inside the transport close when the manager is torn
/// down. Manager free walks every installed connection and closes it, so it
/// reaches the refused connection while the refusal still owns its close.
///
/// A second connection holds a REAL claimed send in flight — a production
/// `send_preencoded_on_manager` call parked inside the transport's `send`,
/// holding that connection's `ClaimedSendLease` — which is precisely the
/// condition free waits out after its close phase. That keeps free from
/// dropping the manager out from under the refusal still running on it,
/// without weakening the close race the test is about.
///
/// Counterfactual: with the close announced rather than acquired, free's
/// walk stores the flag and frees the refused connection's handle behind
/// the refusal — this asserts 2 closes of the refused connection instead
/// of 1.
#[test]
fn refusal_during_manager_free_closes_the_transport_exactly_once() {
    const REFUSED: c_int = 43;
    const OTHER: c_int = 44;
    const OTHER_ROUTE_SLOT: u16 = 3;

    let race = stage_teardown_race(REFUSED);
    let refused = race.stage_connection(REFUSED, RACE_ROUTE_SLOT, true);
    let other = race.stage_connection(OTHER, OTHER_ROUTE_SLOT, false);
    let in_flight_send = race.park_claimed_send(OTHER, other.token);

    let refusal_mgr = SendConnMgr(race.mgr);
    let token = refused.token;
    let publication_sync = Arc::clone(&refused.publication_sync);
    let publication_removed = Arc::clone(&refused.publication_removed);
    let superseded = refused.superseded;
    let refusal = std::thread::spawn(move || {
        let refusal_mgr = refusal_mgr;
        // SAFETY: the in-flight claimed send parks manager free before it
        // can drop the manager, so it is live for this whole thread.
        let mgr = unsafe { &*refusal_mgr.0 };
        publish_connection_established(
            mgr,
            RACE_ROUTE_SLOT,
            REFUSED,
            HEW_FEATURE_SUPPORTS_GOSSIP,
            token,
            &publication_sync,
            &publication_removed,
            superseded,
        );
    });
    race.await_park_entered();

    let free_mgr = SendConnMgr(race.mgr);
    let free = std::thread::spawn(move || {
        let free_mgr = free_mgr;
        // SAFETY: the manager is handed over exactly once, here.
        unsafe { hew_connmgr_free(free_mgr.0) };
    });
    race.await_racing_close(REFUSED);
    race.release_park();

    refusal.join().expect("refusal thread should not panic");
    refused
        .reader_done
        .expect("the refused connection is staged with a reader")
        .recv_timeout(std::time::Duration::from_secs(10))
        .expect("the woken reader must have run its cleanup");

    assert_eq!(
        race.transport_impl().closes_of(REFUSED),
        1,
        "a refusal in flight during manager free must close the one-shot transport exactly once"
    );
    assert_eq!(
        race.transport_impl().closes_of(OTHER),
        1,
        "the connection only manager free tears down must also close exactly once"
    );

    // Release the in-flight claimed send so free can finish; the refusal is
    // already done with the manager.
    assert!(
        !free.is_finished(),
        "manager free must still be parked on the in-flight claimed send"
    );
    race.release_send_park();
    assert_eq!(
        in_flight_send
            .join()
            .expect("the claimed send thread should not panic"),
        0,
        "the parked claimed send must complete once released"
    );
    free.join().expect("manager free thread should not panic");
    assert_eq!(race.transport_impl().closes_of(REFUSED), 1);
    assert_eq!(race.transport_impl().closes_of(OTHER), 1);
    // SAFETY: the free thread above took the manager.
    unsafe { race.finish() };
}

/// (d) A guarded publication transition is held at the moment it has passed
/// its guard but has not emitted, and the connection it was authorized
/// against is refused underneath it. The refused peer must never be
/// observable as alive: the transition's authorization is revoked, so it
/// emits nothing at all.
///
/// The peer's route slot is deliberately one the cluster has NOT joined, so
/// the establish takes the unknown-member branch and the queued transition
/// is the peer's first ALIVE — a `NODE_JOINED` the membership callback
/// would report if it ever escaped.
///
/// Counterfactual: with the guard's verdict treated as final (publishing
/// the peer visible at the guard and emitting unconditionally afterwards,
/// as before), the parked transition announces the peer joined AFTER the
/// refusal retired it — this observes `(slot, NODE_JOINED)` instead of no
/// event at all.
#[test]
fn refusal_retires_a_queued_alive_before_it_can_be_observed() {
    /// A slot the harness cluster never joined, and not the routing table's
    /// own slot, so this peer's first ALIVE is a genuine join announcement.
    const UNJOINED_ROUTE_SLOT: u16 = 5;
    const CONN: c_int = 45;

    extern "C" fn collect_membership_events(
        node_id: u16,
        event: u8,
        user_data: *mut std::ffi::c_void,
    ) {
        // SAFETY: the test installs a Mutex<Vec<(u16, u8)>> that outlives
        // every thread able to reach the callback.
        let events = unsafe { &*user_data.cast::<Mutex<Vec<(u16, u8)>>>() };
        events.lock_or_recover().push((node_id, event));
    }

    struct SendCluster(*mut crate::cluster::HewCluster);
    // SAFETY: cluster internals are synchronized and the test keeps the
    // cluster alive past every thread that holds this pointer.
    unsafe impl Send for SendCluster {}

    // No connection is ever parked inside `close_conn` here: this test's
    // interleaving seam is the cluster's guarded emission, not the close.
    let race = stage_teardown_race(0);
    let staged = race.stage_connection(CONN, UNJOINED_ROUTE_SLOT, true);
    let events: Box<Mutex<Vec<(u16, u8)>>> = Box::new(Mutex::new(Vec::new()));
    let events = Box::into_raw(events);
    // SAFETY: the cluster is live and `events` outlives every dispatch.
    unsafe {
        crate::cluster::hew_cluster_set_membership_callback(
            race.cluster,
            collect_membership_events,
            events.cast::<std::ffi::c_void>(),
        );
    }

    // Two parties: the thread draining the transition, and this test.
    let rendezvous = Arc::new(std::sync::Barrier::new(2));
    // SAFETY: the cluster is live for the whole test.
    unsafe { &*race.cluster }.set_guarded_emission_probe(Some(Arc::clone(&rendezvous)));

    let establish_cluster = SendCluster(race.cluster);
    let token = staged.token;
    let publication_sync = Arc::clone(&staged.publication_sync);
    let publication_removed = Arc::clone(&staged.publication_removed);
    let establish = std::thread::spawn(move || {
        let establish_cluster = establish_cluster;
        // SAFETY: the cluster outlives this thread; the test joins it before
        // freeing anything.
        unsafe {
            crate::cluster::hew_cluster_notify_connection_established_for_token_if_not_removed(
                establish_cluster.0,
                UNJOINED_ROUTE_SLOT,
                1,
                token,
                &publication_sync,
                &publication_removed,
            )
        }
    });

    // The transition is now past its guard and has not emitted.
    rendezvous.wait();
    assert_eq!(
        // SAFETY: the cluster is live.
        unsafe { crate::cluster::hew_cluster_member_state(race.cluster, UNJOINED_ROUTE_SLOT) },
        crate::cluster::MEMBER_ALIVE,
        "the establish must have staged the peer alive before its guard ran"
    );

    // Refuse the very publication that transition was authorized against.
    refuse_established_publication(race.mgr(), UNJOINED_ROUTE_SLOT, CONN, token);

    // Release the parked transition into a cluster that has retired it.
    rendezvous.wait();
    // SAFETY: the cluster is live.
    unsafe { &*race.cluster }.set_guarded_emission_probe(None);
    assert_eq!(
        establish.join().expect("establish thread should not panic"),
        1
    );
    staged
        .reader_done
        .expect("the refused connection is staged with a reader")
        .recv_timeout(std::time::Duration::from_secs(10))
        .expect("the woken reader must have run its cleanup");

    // SAFETY: `events` is still owned by this test.
    let observed = unsafe { &*events }.lock_or_recover().clone();
    assert!(
        observed
            .iter()
            .all(|(node_id, _)| *node_id != UNJOINED_ROUTE_SLOT),
        "a refused peer must never be observable, but the membership \
         callback saw {observed:?}"
    );
    assert_eq!(
        // SAFETY: the cluster is live.
        unsafe { crate::cluster::hew_cluster_member_state(race.cluster, UNJOINED_ROUTE_SLOT) },
        crate::cluster::MEMBER_SUSPECT,
        "the refusal must leave the peer retired, not alive"
    );
    assert_eq!(
        race.transport_impl().closes_of(CONN),
        1,
        "the refusal must close the one-shot transport exactly once"
    );
    // SAFETY: the manager is still live.
    assert_eq!(unsafe { hew_connmgr_count(race.mgr) }, 0);
    assert_eq!(
        race.mgr().reconnect_workers.access(|workers| workers.len()),
        0,
        "a refused peer must never be scheduled for reconnect"
    );

    // SAFETY: the manager is live and is not used again.
    unsafe { hew_connmgr_free(race.mgr) };
    assert_eq!(race.transport_impl().closes_of(CONN), 1);
    // SAFETY: the manager has been freed and no thread can reach the
    // callback payload any more.
    unsafe {
        race.finish();
        drop(Box::from_raw(events));
    }
}

/// (e) The supersede path arrives after the superseded connection's own
/// removal has already finished. A same-credential reconnect reserves the
/// published connection's claim and keeps it as `superseded`; the ordinary
/// removal of that published connection then wins outright — it claims the
/// close, closes the transport and unlinks the actor — and only afterwards
/// does the reconnect complete its publication and reach the supersede
/// close. The one-shot handle must still have been closed exactly once.
///
/// The reconnect's route slot is NOT the harness routing table's own slot,
/// so its publication genuinely completes and reaches the supersede branch
/// rather than being refused before it.
///
/// Counterfactual: with an absent actor read as "then nobody has closed
/// this yet" (the raw fallback this replaces), the completing publication
/// frees the removed connection's handle a second time — this observes 2
/// closes of the superseded connection instead of 1.
#[test]
fn supersede_after_removal_leaves_the_closed_transport_alone() {
    const SUPERSEDED: c_int = 46;
    const RECONNECT: c_int = 47;
    /// Not the harness routing table's local slot, so the reconnect's route
    /// registers and its publication completes.
    const ROUTE_SLOT: u16 = 6;

    // Nothing is parked inside a close here: the interleaving is that the
    // removal COMPLETES first, not that it is caught mid-close.
    let race = stage_teardown_race(0);
    let superseded = race.stage_connection(SUPERSEDED, ROUTE_SLOT, true);
    assert!(
        publish_claim(race.mgr(), ROUTE_SLOT, SUPERSEDED, superseded.token),
        "the connection to be superseded must be published first"
    );

    // The same-credential reconnect reserves the published claim and keeps
    // it — this is the `superseded` its publication will act on later.
    let reconnect = race.stage_connection(RECONNECT, ROUTE_SLOT, false);
    let retained = reconnect
        .superseded
        .clone()
        .expect("the reconnect must supersede the published claim");
    assert_eq!(retained.state, ClaimState::Published);
    assert_eq!(retained.conn_id, SUPERSEDED);
    assert_eq!(retained.publication_token, superseded.token);

    // The superseded connection's own removal wins: it claims the close,
    // closes the transport, and unlinks the actor.
    // SAFETY: the manager is live.
    assert_eq!(unsafe { hew_connmgr_remove(race.mgr, SUPERSEDED) }, 0);
    superseded
        .reader_done
        .expect("the superseded connection is staged with a reader")
        .recv_timeout(std::time::Duration::from_secs(10))
        .expect("the woken reader must have run its cleanup");
    assert_eq!(
        race.transport_impl().closes_of(SUPERSEDED),
        1,
        "the removal itself must close the transport exactly once"
    );
    assert!(
        race.mgr().connections.access(|connections| !connections
            .iter()
            .any(|connection| connection.conn_id == SUPERSEDED)),
        "the removal must have unlinked the superseded actor before the \
         publication below runs"
    );

    // Only now does the reconnect finish publishing and reach the supersede
    // close, with no actor left to consult.
    publish_connection_established(
        race.mgr(),
        ROUTE_SLOT,
        RECONNECT,
        HEW_FEATURE_SUPPORTS_GOSSIP,
        reconnect.token,
        &reconnect.publication_sync,
        &reconnect.publication_removed,
        Some(retained),
    );

    assert_eq!(
        race.transport_impl().closes_of(SUPERSEDED),
        1,
        "a supersede must not close a transport whose removal already closed it"
    );
    // SAFETY: the manager is still live.
    assert_eq!(unsafe { hew_connmgr_count(race.mgr) }, 1);

    // SAFETY: the manager is live and is not used again.
    unsafe { hew_connmgr_free(race.mgr) };
    assert_eq!(
        race.transport_impl().closes_of(SUPERSEDED),
        1,
        "manager free must not close the superseded connection again either"
    );
    assert_eq!(
        race.transport_impl().closes_of(RECONNECT),
        1,
        "the surviving connection is closed once, by manager free"
    );
    // SAFETY: the manager has been freed.
    unsafe { race.finish() };
}

/// (f) A guarded publication transition has already CLAIMED — it is past
/// every check, registered as in flight, and about to run its callbacks —
/// when the connection it was authorized against is refused. The whole
/// transition can no longer be revoked, so the refusal CANCELS THE
/// REMAINDER of it. This seam parks before the first step's gate, so the
/// refusal must suppress the whole delivery.
///
/// The refusal must NOT wait for the claimed delivery. Waiting would mean
/// blocking on a thread running externally-registered callbacks, which is
/// the deadlock this design exists to avoid; the positive-timing assertion
/// below pins that the refusal returns while the delivery is still parked.
///
/// The seam this parks on sits AFTER the claim and BEFORE the first
/// observable delivery, which is where this race lives. Test (d)'s seam
/// sits before the claim and cannot see it — a seam placed where the bug is
/// not cannot fail.
///
/// Counterfactual: with the claim treated as the end of the story (no
/// per-step re-read of the in-flight registration), the released delivery
/// runs its callbacks unconditionally and the peer's `NODE_JOINED` is
/// recorded after the refusal-returned marker — the negative assertion
/// below fails. A race after a successful per-step gate has different,
/// explicitly linearized semantics; see `GuardedDeliveryInFlight::may_emit`.
#[test]
#[expect(
    clippy::too_many_lines,
    reason = "test stages the post-claim delivery race and its full observation log in one place"
)]
fn refusal_cancels_a_claimed_alive_before_it_returns() {
    /// A slot the harness cluster never joined, so this peer's ALIVE is a
    /// genuine `NODE_JOINED` announcement.
    const UNJOINED_ROUTE_SLOT: u16 = 7;
    const CONN: c_int = 48;

    /// One entry in the observation log, in the order observers saw it.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    enum Observed {
        Membership(u16, u8),
        /// Pushed the instant `refuse_established_publication` returns.
        RefusalReturned,
    }

    extern "C" fn collect_membership_events(
        node_id: u16,
        event: u8,
        user_data: *mut std::ffi::c_void,
    ) {
        // SAFETY: the test installs a Mutex<Vec<Observed>> that outlives
        // every thread able to reach the callback.
        let log = unsafe { &*user_data.cast::<Mutex<Vec<Observed>>>() };
        log.lock_or_recover()
            .push(Observed::Membership(node_id, event));
    }

    struct SendCluster(*mut crate::cluster::HewCluster);
    // SAFETY: cluster internals are synchronized and the test keeps the
    // cluster alive past every thread that holds this pointer.
    unsafe impl Send for SendCluster {}

    struct SendLog(*mut Mutex<Vec<Observed>>);
    // SAFETY: the log is a mutex the test keeps alive past every thread
    // that holds this pointer.
    unsafe impl Send for SendLog {}

    // No close is parked: this interleaving seam is the cluster's guarded
    // delivery, not the close.
    let race = stage_teardown_race(0);
    let staged = race.stage_connection(CONN, UNJOINED_ROUTE_SLOT, true);
    let log: Box<Mutex<Vec<Observed>>> = Box::new(Mutex::new(Vec::new()));
    let log = Box::into_raw(log);
    // SAFETY: the cluster is live and `log` outlives every dispatch.
    unsafe {
        crate::cluster::hew_cluster_set_membership_callback(
            race.cluster,
            collect_membership_events,
            log.cast::<std::ffi::c_void>(),
        );
    }

    let (entered_tx, entered_rx) = std::sync::mpsc::channel::<()>();
    let (release_tx, release_rx) = std::sync::mpsc::channel::<()>();
    // SAFETY: the cluster is live for the whole test.
    unsafe { &*race.cluster }.set_guarded_delivery_probe(Some(Arc::new(
        crate::cluster::GuardedDeliveryProbe {
            entered: entered_tx,
            release: Mutex::new(release_rx),
        },
    )));

    let establish_cluster = SendCluster(race.cluster);
    let token = staged.token;
    let publication_sync = Arc::clone(&staged.publication_sync);
    let publication_removed = Arc::clone(&staged.publication_removed);
    let establish = std::thread::spawn(move || {
        let establish_cluster = establish_cluster;
        // SAFETY: the cluster outlives this thread; the test joins it
        // before freeing anything.
        unsafe {
            crate::cluster::hew_cluster_notify_connection_established_for_token_if_not_removed(
                establish_cluster.0,
                UNJOINED_ROUTE_SLOT,
                1,
                token,
                &publication_sync,
                &publication_removed,
            )
        }
    });

    // The transition has now CLAIMED and has emitted nothing yet.
    entered_rx
        .recv_timeout(std::time::Duration::from_secs(10))
        .expect("the guarded delivery must park after its claim");
    // SAFETY: `log` is owned by this test.
    let observed_before_release = unsafe { &*log }.lock_or_recover().len();
    assert_eq!(
        observed_before_release, 0,
        "the parked delivery must not have become observable yet"
    );

    let (refusal_done_tx, refusal_done_rx) = std::sync::mpsc::channel::<()>();
    let refusal_mgr = SendConnMgr(race.mgr);
    let refusal_log = SendLog(log);
    let refusal = std::thread::spawn(move || {
        let refusal_mgr = refusal_mgr;
        let refusal_log = refusal_log;
        // SAFETY: the manager outlives this thread.
        let mgr = unsafe { &*refusal_mgr.0 };
        refuse_established_publication(mgr, UNJOINED_ROUTE_SLOT, CONN, token);
        // SAFETY: the log outlives every thread that can reach it.
        unsafe { &*refusal_log.0 }
            .lock_or_recover()
            .push(Observed::RefusalReturned);
        let _ = refusal_done_tx.send(());
    });

    // Positive-timing assertion: the refusal must return WITHOUT waiting
    // for the claimed delivery, which is still parked. A refusal that
    // blocked here would be blocking on a thread that is free to run user
    // callbacks, and those callbacks are free to block on this thread.
    refusal_done_rx
        .recv_timeout(std::time::Duration::from_secs(10))
        .expect("the refusal must return without waiting for the parked delivery");
    assert!(
        // SAFETY: `log` is owned by this test and outlives every thread
        // that can reach it.
        unsafe { &*log }
            .lock_or_recover()
            .contains(&Observed::RefusalReturned),
        "the refusal-returned marker must be recorded before the release"
    );

    // Release the delivery into a cluster that has already retired it.
    release_tx
        .send(())
        .expect("the parked delivery must still be waiting");
    assert_eq!(
        establish.join().expect("establish thread should not panic"),
        1
    );
    refusal.join().expect("refusal thread should not panic");
    // SAFETY: the cluster is live.
    unsafe { &*race.cluster }.set_guarded_delivery_probe(None);
    staged
        .reader_done
        .expect("the refused connection is staged with a reader")
        .recv_timeout(std::time::Duration::from_secs(10))
        .expect("the woken reader must have run its cleanup");

    // SAFETY: `log` is still owned by this test and every thread that
    // could push to it has been joined.
    let observed = unsafe { &*log }.lock_or_recover().clone();
    assert!(
        observed.contains(&Observed::RefusalReturned),
        "the refusal-returned marker must be recorded, got {observed:?}"
    );
    let joined = Observed::Membership(
        UNJOINED_ROUTE_SLOT,
        crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_JOINED,
    );
    assert!(
        !observed.contains(&joined),
        "the cancelled ALIVE must never become observable, got {observed:?}"
    );
    assert!(
        observed.contains(&Observed::Membership(
            UNJOINED_ROUTE_SLOT,
            crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_SUSPECT
        )),
        "the refusal must still demote the peer it retired, got {observed:?}"
    );
    assert_eq!(
        // SAFETY: the cluster is live.
        unsafe { crate::cluster::hew_cluster_member_state(race.cluster, UNJOINED_ROUTE_SLOT) },
        crate::cluster::MEMBER_SUSPECT,
        "the refusal must leave the peer retired, not alive"
    );
    assert_eq!(
        race.transport_impl().closes_of(CONN),
        1,
        "the refusal must close the one-shot transport exactly once"
    );
    // SAFETY: the manager is still live.
    assert_eq!(unsafe { hew_connmgr_count(race.mgr) }, 0);

    // SAFETY: the manager is live and is not used again.
    unsafe { hew_connmgr_free(race.mgr) };
    assert_eq!(race.transport_impl().closes_of(CONN), 1);
    // SAFETY: the manager has been freed and no thread can reach the
    // callback payload any more.
    unsafe {
        race.finish();
        drop(Box::from_raw(log));
    }
}

/// A route-slot refusal is decided before cluster publication. This pins
/// the production ordering at the precise race that token cancellation
/// cannot close: a guarded ALIVE membership callback whose per-step gate
/// has already succeeded.
///
/// The blocker keeps the cluster's single transition drainer occupied
/// while the publisher runs. The test-only route-add seam and post-gate
/// callback seam make the old ordering deterministic:
///
/// 1. publication queues ALIVE, then parks on the doomed route add;
/// 2. the drainer reaches ALIVE's successful callback gate and parks;
/// 3. route refusal retires the token and returns;
/// 4. the already-authorized ALIVE callback runs after the return marker.
///
/// Prevalidation makes neither publication seam reachable: the connection
/// is refused and the reserved member is demoted before any guarded ALIVE
/// transition exists.
#[test]
#[expect(
    clippy::too_many_lines,
    reason = "the regression stages both sides of the gate-to-callback ordering counterfactual"
)]
fn reserved_route_is_refused_before_a_guarded_alive_can_pass_its_callback_gate() {
    const BLOCKER_ROUTE_SLOT: u16 = 9;
    const CONN: c_int = 49;

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    enum Observed {
        Membership(u16, u8),
        PublicationReturned,
    }

    struct Observer {
        log: Mutex<Vec<Observed>>,
        blocker_entered: std::sync::mpsc::Sender<()>,
        blocker_release: Mutex<std::sync::mpsc::Receiver<()>>,
    }

    extern "C" fn observe_membership(node_id: u16, event: u8, user_data: *mut std::ffi::c_void) {
        // SAFETY: the test keeps this Observer alive until the cluster and
        // all delivering threads have been freed/joined.
        let observer = unsafe { &*user_data.cast::<Observer>() };
        if node_id == BLOCKER_ROUTE_SLOT
            && event == crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_JOINED
        {
            let _ = observer.blocker_entered.send(());
            let _ = observer.blocker_release.lock_or_recover().recv();
        }
        observer
            .log
            .lock_or_recover()
            .push(Observed::Membership(node_id, event));
    }

    struct SendCluster(*mut crate::cluster::HewCluster);
    // SAFETY: the cluster synchronizes its internals and outlives the
    // blocker thread.
    unsafe impl Send for SendCluster {}

    struct SendLog(*mut Observer);
    // SAFETY: the Observer's log is synchronized and outlives the
    // publisher thread.
    unsafe impl Send for SendLog {}

    let race = stage_teardown_race(0);
    // Start the reserved peer SUSPECT so the counterfactual establishment
    // would enqueue an ALIVE transition with a membership callback.
    // SAFETY: the cluster is live.
    unsafe {
        crate::cluster::hew_cluster_notify_connection_lost(race.cluster, RACE_ROUTE_SLOT);
    }
    assert_eq!(
        // SAFETY: the cluster is live.
        unsafe { crate::cluster::hew_cluster_member_state(race.cluster, RACE_ROUTE_SLOT) },
        crate::cluster::MEMBER_SUSPECT
    );

    let (blocker_entered_tx, blocker_entered_rx) = std::sync::mpsc::channel::<()>();
    let (blocker_release_tx, blocker_release_rx) = std::sync::mpsc::channel::<()>();
    let observer = Box::into_raw(Box::new(Observer {
        log: Mutex::new(Vec::new()),
        blocker_entered: blocker_entered_tx,
        blocker_release: Mutex::new(blocker_release_rx),
    }));
    // SAFETY: the cluster and Observer are live until teardown below.
    unsafe {
        crate::cluster::hew_cluster_set_membership_callback(
            race.cluster,
            observe_membership,
            observer.cast::<std::ffi::c_void>(),
        );
    }

    let blocker_cluster = SendCluster(race.cluster);
    let blocker = std::thread::spawn(move || {
        let blocker_cluster = blocker_cluster;
        // SAFETY: cluster outlives this thread and the C string is static.
        unsafe {
            crate::cluster::hew_cluster_join(
                blocker_cluster.0,
                BLOCKER_ROUTE_SLOT,
                c"10.0.0.9:9000".as_ptr(),
            )
        }
    });
    blocker_entered_rx
        .recv_timeout(std::time::Duration::from_secs(10))
        .expect("the blocker must occupy the transition drainer");

    let staged = race.stage_connection(CONN, RACE_ROUTE_SLOT, false);
    let (route_entered_tx, route_entered_rx) = std::sync::mpsc::channel::<()>();
    let (route_release_tx, route_release_rx) = std::sync::mpsc::channel::<()>();
    // SAFETY: the routing table is live.
    unsafe { &*race.routing_table }.set_add_route_probe(Some(Arc::new(
        crate::routing::RouteAddProbe {
            entered: route_entered_tx,
            release: Mutex::new(route_release_rx),
        },
    )));

    let (callback_gate_entered_tx, callback_gate_entered_rx) = std::sync::mpsc::channel::<()>();
    let (callback_gate_release_tx, callback_gate_release_rx) = std::sync::mpsc::channel::<()>();
    // SAFETY: the cluster is live.
    unsafe { &*race.cluster }.set_guarded_membership_callback_probe(Some(Arc::new(
        crate::cluster::GuardedDeliveryProbe {
            entered: callback_gate_entered_tx,
            release: Mutex::new(callback_gate_release_rx),
        },
    )));

    let publisher_mgr = SendConnMgr(race.mgr);
    let publisher_log = SendLog(observer);
    let (publisher_done_tx, publisher_done_rx) = std::sync::mpsc::channel::<()>();
    let publisher = std::thread::spawn(move || {
        let publisher_mgr = publisher_mgr;
        let publisher_log = publisher_log;
        // SAFETY: the manager and Observer outlive this thread.
        let mgr = unsafe { &*publisher_mgr.0 };
        publish_connection_established(
            mgr,
            RACE_ROUTE_SLOT,
            CONN,
            HEW_FEATURE_SUPPORTS_GOSSIP,
            staged.token,
            &staged.publication_sync,
            &staged.publication_removed,
            staged.superseded,
        );
        // SAFETY: Observer outlives this thread.
        unsafe { &*publisher_log.0 }
            .log
            .lock_or_recover()
            .push(Observed::PublicationReturned);
        let _ = publisher_done_tx.send(());
    });

    // Fixed code returns directly from prevalidation. In the
    // counterfactual old ordering, publication instead reaches and parks
    // inside the reserved route-add attempt.
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
    let route_add_was_attempted = loop {
        match publisher_done_rx.try_recv() {
            Ok(()) => break false,
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                panic!("publisher exited without signalling completion");
            }
            Err(std::sync::mpsc::TryRecvError::Empty) => {}
        }
        match route_entered_rx.try_recv() {
            Ok(()) => break true,
            Err(
                std::sync::mpsc::TryRecvError::Disconnected | std::sync::mpsc::TryRecvError::Empty,
            ) => {}
        }
        assert!(
            std::time::Instant::now() < deadline,
            "publication reached neither prevalidation return nor route add"
        );
        std::thread::yield_now();
    };

    blocker_release_tx
        .send(())
        .expect("the blocker callback must still be parked");

    if route_add_was_attempted {
        // Counterfactual execution: let the drainer pass ALIVE's
        // membership-callback gate, then let route refusal return, and
        // only then release the callback.
        callback_gate_entered_rx
            .recv_timeout(std::time::Duration::from_secs(10))
            .expect("the guarded ALIVE must reach its successful callback gate");
        route_release_tx
            .send(())
            .expect("the reserved route add must still be parked");
        publisher_done_rx
            .recv_timeout(std::time::Duration::from_secs(10))
            .expect("route refusal must return while the callback remains parked");
        callback_gate_release_tx
            .send(())
            .expect("the post-gate callback must still be parked");
    } else {
        assert!(
            callback_gate_entered_rx.try_recv().is_err(),
            "prevalidation must create no guarded ALIVE callback"
        );
    }

    publisher.join().expect("publisher should not panic");
    assert_eq!(
        blocker.join().expect("blocker should not panic"),
        0,
        "blocker join should succeed"
    );
    // SAFETY: both test probes' owners are still live.
    unsafe {
        (&*race.routing_table).set_add_route_probe(None);
        (&*race.cluster).set_guarded_membership_callback_probe(None);
    }

    // SAFETY: every thread that can append has been joined.
    let event_log = unsafe { &*observer }.log.lock_or_recover().clone();
    let returned = event_log
        .iter()
        .position(|entry| *entry == Observed::PublicationReturned)
        .expect("publication return marker must be present");
    let joined = Observed::Membership(
        RACE_ROUTE_SLOT,
        crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_JOINED,
    );
    assert!(
        event_log
            .iter()
            .skip(returned + 1)
            .all(|entry| *entry != joined),
        "a refused reserved peer became ALIVE after publication returned: {event_log:?}"
    );
    assert!(
        !route_add_was_attempted,
        "a reserved admission must be rejected before cluster/route publication"
    );
    assert_eq!(
        // SAFETY: cluster is live.
        unsafe { crate::cluster::hew_cluster_member_state(race.cluster, RACE_ROUTE_SLOT) },
        crate::cluster::MEMBER_SUSPECT,
        "the refused reserved member must remain demoted"
    );
    assert_eq!(race.transport_impl().closes_of(CONN), 1);
    // SAFETY: manager is live and no longer used after this call.
    unsafe { hew_connmgr_free(race.mgr) };
    assert_eq!(race.transport_impl().closes_of(CONN), 1);
    // SAFETY: manager is gone and no thread can reach either allocation.
    unsafe {
        race.finish();
        drop(Box::from_raw(observer));
    }
}

/// One entry in the re-entrant refusal's observation log, in the order
/// observers saw it.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ReentrantSeen {
    /// The ordinary state callback, which fires BEFORE the membership one.
    State(u16, i32),
    Membership(u16, u8),
    /// Pushed the instant the re-entrant refusal returns.
    RetirementReturned,
}

/// Everything the re-entrant state callback needs to refuse its own
/// connection, since [`crate::cluster::hew_cluster_set_callback`] carries
/// no user data.
struct ReentrantRefusal {
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    route_slot: u16,
    token: u64,
    log: *mut Mutex<Vec<ReentrantSeen>>,
    /// Refuse once: the refusal itself delivers a SUSPECT through this same
    /// callback and must not recurse.
    fired: bool,
    /// Run the refusal on a SECOND thread and block the delivering thread
    /// on it, which is the cycle a retirement that waited for the delivery
    /// would close.
    off_thread: bool,
}

// SAFETY: the test keeps the manager and the log alive past every thread
// that can reach this callback, and clears the slot before freeing either.
unsafe impl Send for ReentrantRefusal {}

static REENTRANT_REFUSAL: Mutex<Option<ReentrantRefusal>> = Mutex::new(None);
/// Serializes the two tests that install the process-global callback
/// context above. The callback deliberately drops `REENTRANT_REFUSAL`
/// before re-entering teardown, so that slot cannot itself serve as the
/// whole-test guard.
static REENTRANT_REFUSAL_TEST_GUARD: Mutex<()> = Mutex::new(());

extern "C" fn refuse_from_state_callback(node_id: u16, state: i32, _incarnation: u64) {
    // The slot lock is dropped before the refusal: the refusal drives
    // further deliveries through this same callback.
    let target = {
        let mut slot = REENTRANT_REFUSAL.lock_or_recover();
        let Some(context) = slot.as_mut() else {
            return;
        };
        // SAFETY: the log outlives the callback registration.
        unsafe { &*context.log }
            .lock_or_recover()
            .push(ReentrantSeen::State(node_id, state));
        if context.fired || node_id != context.route_slot || state != crate::cluster::MEMBER_ALIVE {
            None
        } else {
            context.fired = true;
            Some((
                context.mgr,
                context.route_slot,
                context.conn_id,
                context.token,
                context.log,
                context.off_thread,
            ))
        }
    };
    let Some((mgr, route_slot, conn_id, token, log, off_thread)) = target else {
        return;
    };
    if off_thread {
        let refusal_mgr = SendConnMgr(mgr);
        let (done_tx, done_rx) = std::sync::mpsc::channel::<()>();
        let refusal = std::thread::spawn(move || {
            let refusal_mgr = refusal_mgr;
            // SAFETY: the manager outlives the callback registration.
            refuse_established_publication(unsafe { &*refusal_mgr.0 }, route_slot, conn_id, token);
            let _ = done_tx.send(());
        });
        // The delivering thread now blocks on the retiring thread. A
        // retirement that waited for this delivery would close the cycle;
        // this bounded wait turns that deadlock into a failure instead of
        // a hang.
        done_rx
            .recv_timeout(std::time::Duration::from_secs(10))
            .expect("the retirement must not deadlock against the delivery it retires");
        refusal.join().expect("refusal thread should not panic");
    } else {
        // SAFETY: the manager outlives the callback registration.
        refuse_established_publication(unsafe { &*mgr }, route_slot, conn_id, token);
    }
    // SAFETY: the log outlives the callback registration.
    unsafe { &*log }
        .lock_or_recover()
        .push(ReentrantSeen::RetirementReturned);
}

extern "C" fn collect_reentrant_membership_events(
    node_id: u16,
    event: u8,
    user_data: *mut std::ffi::c_void,
) {
    // SAFETY: the test installs a Mutex<Vec<ReentrantSeen>> that outlives
    // every thread able to reach the callback.
    let log = unsafe { &*user_data.cast::<Mutex<Vec<ReentrantSeen>>>() };
    log.lock_or_recover()
        .push(ReentrantSeen::Membership(node_id, event));
}

/// Drive one claimed guarded ALIVE whose state callback refuses the very
/// connection that ALIVE is about, and return the observation log in the
/// order observers saw it.
///
/// `off_thread` chooses which of the two re-entrancy shapes runs: the
/// refusal on the delivering thread itself, or on a second thread the
/// delivering thread then blocks on.
fn drive_refusal_from_a_delivery_callback(
    off_thread: bool,
    route_slot: u16,
    conn_id: c_int,
) -> Vec<ReentrantSeen> {
    let _test_guard = REENTRANT_REFUSAL_TEST_GUARD.lock_or_recover();
    // No close is parked: this is about the delivery, not the close.
    let race = stage_teardown_race(0);
    let staged = race.stage_connection(conn_id, route_slot, true);
    let log: Box<Mutex<Vec<ReentrantSeen>>> = Box::new(Mutex::new(Vec::new()));
    let log = Box::into_raw(log);
    // SAFETY: the cluster is live and `log` outlives every dispatch.
    unsafe {
        crate::cluster::hew_cluster_set_membership_callback(
            race.cluster,
            collect_reentrant_membership_events,
            log.cast::<std::ffi::c_void>(),
        );
        crate::cluster::hew_cluster_set_callback(race.cluster, Some(refuse_from_state_callback));
    }
    *REENTRANT_REFUSAL.lock_or_recover() = Some(ReentrantRefusal {
        mgr: race.mgr,
        conn_id,
        route_slot,
        token: staged.token,
        log,
        fired: false,
        off_thread,
    });

    // Driven on THIS thread, so the refusal is genuinely re-entrant: it
    // runs inside the delivery it is retiring.
    // SAFETY: the cluster is live for the whole test.
    let established = unsafe {
        crate::cluster::hew_cluster_notify_connection_established_for_token_if_not_removed(
            race.cluster,
            route_slot,
            1,
            staged.token,
            &staged.publication_sync,
            &staged.publication_removed,
        )
    };
    assert_eq!(established, 1);

    *REENTRANT_REFUSAL.lock_or_recover() = None;
    // SAFETY: the cluster is live and no other thread is using it.
    unsafe { crate::cluster::hew_cluster_set_callback(race.cluster, None) };
    staged
        .reader_done
        .expect("the refused connection is staged with a reader")
        .recv_timeout(std::time::Duration::from_secs(10))
        .expect("the woken reader must have run its cleanup");

    assert_eq!(
        // SAFETY: the cluster is live.
        unsafe { crate::cluster::hew_cluster_member_state(race.cluster, route_slot) },
        crate::cluster::MEMBER_SUSPECT,
        "the re-entrant refusal must leave the peer retired, not alive"
    );
    assert_eq!(
        race.transport_impl().closes_of(conn_id),
        1,
        "the re-entrant refusal must close the transport exactly once"
    );
    // SAFETY: the manager is still live.
    assert_eq!(unsafe { hew_connmgr_count(race.mgr) }, 0);
    // SAFETY: the manager is live and is not used again.
    unsafe { hew_connmgr_free(race.mgr) };
    assert_eq!(race.transport_impl().closes_of(conn_id), 1);

    // SAFETY: the manager has been freed and no thread can reach the
    // callback payload any more.
    let observed = unsafe {
        race.finish();
        let observed = (*log).lock_or_recover().clone();
        drop(Box::from_raw(log));
        observed
    };
    observed
}

/// Assert the ordering for this re-entrant interleaving: the state-callback
/// ALIVE step has already started, and retirement must cancel every later
/// ALIVE step before returning to that callback.
fn assert_no_alive_survives_the_refusal(route_slot: u16, observed: &[ReentrantSeen]) {
    assert!(
        observed.contains(&ReentrantSeen::State(
            route_slot,
            crate::cluster::MEMBER_ALIVE
        )),
        "the re-entrant callback must have been reached through an ALIVE, got {observed:?}"
    );
    let returned = observed
        .iter()
        .position(|entry| *entry == ReentrantSeen::RetirementReturned)
        .expect("the re-entrant retirement must have run and returned");
    let joined =
        ReentrantSeen::Membership(route_slot, crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_JOINED);
    assert!(
        !observed[returned..].contains(&joined),
        "no later ALIVE step may survive the re-entrant retirement, got {observed:?}"
    );
    assert!(
        !observed.contains(&joined),
        "the cancelled ALIVE must be suppressed outright, got {observed:?}"
    );
    assert!(
        observed.contains(&ReentrantSeen::Membership(
            route_slot,
            crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_SUSPECT
        )),
        "the refusal must still demote the peer it retired, got {observed:?}"
    );
}

/// (g) RE-ENTRANT: the state callback of a claimed guarded ALIVE refuses
/// the very connection that ALIVE is about, ON THE DELIVERING THREAD.
///
/// This is the interleaving a same-thread exemption cannot survive. The
/// state callback fires before the membership one, so a retirement that
/// returned into the middle of a delivery would be followed by that
/// delivery's own `NODE_JOINED` — a token-guarded removal returning before
/// an ALIVE it exists to exclude.
///
/// Cancellation makes it hold without any same-thread special case: the
/// refusal marks the in-flight delivery cancelled and returns, and the
/// delivering thread's next re-read of that registration — which cannot be
/// stale, because it happens after the refusal returned on the same
/// thread — suppresses every remaining step.
///
/// It is also the case a wait could never serve: the only thread that
/// could release such a wait is the thread doing the waiting.
///
/// Counterfactual: without the per-step re-read, `NODE_JOINED` is recorded
/// after `RetirementReturned` and the ordering assertion fails.
#[test]
fn refusal_re_entered_from_a_delivery_callback_suppresses_its_own_alive() {
    /// A slot the harness cluster never joined, so this peer's ALIVE is a
    /// genuine `NODE_JOINED` announcement.
    const UNJOINED_ROUTE_SLOT: u16 = 9;
    const CONN: c_int = 51;

    let observed = drive_refusal_from_a_delivery_callback(false, UNJOINED_ROUTE_SLOT, CONN);
    assert_no_alive_survives_the_refusal(UNJOINED_ROUTE_SLOT, &observed);
}

/// (h) RE-ENTRANT ACROSS THREADS: the state callback of a claimed guarded
/// ALIVE hands the refusal to a second thread and blocks on it.
///
/// This is the cycle: the delivering thread cannot return from the callback
/// until the refusing thread returns from the refusal. A retirement that
/// waited for the in-flight delivery would never return, and neither thread
/// would make progress. Cancellation has nothing to wait on — the refusal
/// takes the token lock, flips a flag and returns — so the cycle cannot
/// form.
///
/// The bounded wait inside the callback exists only so a regression shows
/// up as a failure rather than a hung test binary.
///
/// Counterfactual: with the wait restored, this test hangs until that
/// bounded wait expires and then fails on it; with the wait removed but no
/// per-step re-read, it fails on the `NODE_JOINED` ordering instead.
#[test]
fn refusal_from_another_thread_cannot_deadlock_the_delivery_it_retires() {
    /// A slot the harness cluster never joined, so this peer's ALIVE is a
    /// genuine `NODE_JOINED` announcement.
    const UNJOINED_ROUTE_SLOT: u16 = 11;
    const CONN: c_int = 52;

    let observed = drive_refusal_from_a_delivery_callback(true, UNJOINED_ROUTE_SLOT, CONN);
    assert_no_alive_survives_the_refusal(UNJOINED_ROUTE_SLOT, &observed);
}
