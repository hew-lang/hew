//! Process-wide node API and identity staging tests.

use super::*;

#[test]
fn decode_hex_accepts_even_hex_and_rejects_malformed() {
    assert_eq!(decode_hex("00ff1a"), Some(vec![0x00, 0xff, 0x1a]));
    assert_eq!(decode_hex("DEADbeef"), Some(vec![0xde, 0xad, 0xbe, 0xef]));
    // Fail-closed: empty, odd length, and non-hex digits decode to nothing.
    assert_eq!(decode_hex(""), None);
    assert_eq!(decode_hex("abc"), None);
    assert_eq!(decode_hex("zz"), None);
    // Oversize: a hex string decoding past the SPKI cap is rejected before
    // any allocation. 4096 bytes is the bound; (4097)*2 hex chars overflows.
    let oversize = "00".repeat(MAX_SPKI_DECODE_BYTES + 1);
    assert_eq!(decode_hex(&oversize), None, "oversize hex must be rejected");
    assert!(
        decode_hex(&"ab".repeat(MAX_SPKI_DECODE_BYTES)).is_some(),
        "exactly the cap still decodes"
    );
}

#[test]
fn next_local_route_slot_never_yields_zero_sentinel() {
    // Route slot zero is the local-dispatch sentinel.
    for base in [1u16, 2, 100, 65_534, 65_535] {
        for offset in 0u32..4 {
            assert_ne!(
                next_local_route_slot(base, offset),
                0,
                "base={base} offset={offset} mapped onto the 0 sentinel",
            );
        }
    }
    // The bare-wrapping_add hazard: base 65535 + offset 1 wrapped to 0.
    assert_ne!(next_local_route_slot(65_535, 1), 0);
    // Distinct offsets from the same base stay distinct within the ring.
    assert_ne!(next_local_route_slot(100, 0), next_local_route_slot(100, 1));
    // base==1, offset==0 maps to itself (the common single-node case).
    assert_eq!(next_local_route_slot(1, 0), 1);
}

#[test]
fn legacy_route_slot_selection_skips_peer_pins() {
    let mut config = crate::peer_binding::PeerAuthConfig::default();
    config
        .pin_peer(100, PeerCredential::NoiseKey([0x10; 32]))
        .unwrap();
    config
        .pin_peer(101, PeerCredential::NoiseKey([0x11; 32]))
        .unwrap();

    assert_eq!(
        select_local_route_slot(100, 0, config.bindings()).map(std::num::NonZeroU16::get),
        Some(102)
    );
}

/// Public owner-scoped staging (BLOCK-6): after `Node::load_keys` stages the
/// authenticated identity now required by v2, `Node::start` transitions
/// `Building → Running{owner}` and `Node::shutdown` returns it to `Building`
/// with a bumped generation. An unauthenticated default start is no longer
/// valid.
#[cfg(feature = "encryption")]
#[test]
fn public_start_default_then_shutdown_resets_state() {
    let _guard = crate::runtime_test_guard();
    // Start from a known-clean staging cell (other unit tests stage config).
    {
        let mut g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        g.state = ConfigState::default();
    }
    let _identity = stage_public_api_test_identity();
    let bind = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind is a valid C string for this call.
    let rc = unsafe { hew_node_api_start(bind.as_ptr()) };
    assert_eq!(rc, 0, "identity-backed public start should succeed");
    {
        let g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(
            matches!(g.state, ConfigState::Running { .. }),
            "staging state should be Running after a successful start"
        );
    }
    let gen_before = {
        let g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        g.next_generation
    };
    // SAFETY: a node was started by this test; shutdown reclaims it.
    assert_eq!(unsafe { hew_node_api_shutdown() }, 0);
    {
        let g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(
            matches!(g.state, ConfigState::Building(_)),
            "shutdown must return staging to Building"
        );
        assert_ne!(
            g.next_generation, gen_before,
            "shutdown must bump the staging generation"
        );
    }
}

/// A second public `Node::start` while one is already active is refused
/// (fail-closed), and the first node keeps running until its own shutdown.
/// The first start stages a real identity because unauthenticated public
/// startup is no longer valid under the v2 handshake.
#[cfg(feature = "encryption")]
#[test]
fn public_start_rejected_when_already_active() {
    let _guard = crate::runtime_test_guard();
    {
        let mut g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        g.state = ConfigState::default();
    }
    let _identity = stage_public_api_test_identity();
    let bind = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind valid.
    assert_eq!(unsafe { hew_node_api_start(bind.as_ptr()) }, 0);
    // SAFETY: bind valid; second start must be rejected.
    let second_rc = unsafe { hew_node_api_start(bind.as_ptr()) };
    assert_eq!(
        second_rc, -1,
        "a second concurrent public start must be refused"
    );
    // SAFETY: the first node is still active; shutdown reclaims it.
    assert_eq!(unsafe { hew_node_api_shutdown() }, 0);
}

/// A complete `NodeConfig` start holds one transaction boundary from its
/// source-owned decode through the public node start. Concurrent callers
/// therefore cannot combine their staging steps: precisely one owns the
/// public node, and the losing record is still consumed and released.
#[cfg(feature = "encryption")]
#[test]
fn concurrent_start_config_keeps_transactions_atomic() {
    let _guard = crate::runtime_test_guard();
    {
        let mut g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        g.state = ConfigState::default();
    }

    let identity_dir = tempfile::tempdir().expect("identity directory");
    let key_path = std::sync::Arc::new(
        identity_dir
            .path()
            .join("node.key")
            .to_str()
            .expect("temporary key path is UTF-8")
            .to_owned(),
    );
    let barrier = std::sync::Arc::new(std::sync::Barrier::new(3));
    let starts: Vec<_> = (0..2)
        .map(|_| {
            let barrier = barrier.clone();
            let key_path = key_path.clone();
            std::thread::spawn(move || {
                barrier.wait();
                let config = HewNodeConfig {
                    bind: hew_cabi::string::string_from_str("127.0.0.1:0"),
                    transport: hew_cabi::string::string_from_str("tcp"),
                    key: hew_cabi::string::string_from_str(&key_path),
                    trust: hew_cabi::string::string_from_str("pinned"),
                    // SAFETY: these freshly allocated string vectors are
                    // moved into the consuming NodeConfig call below.
                    peers: unsafe { crate::vec::hew_vec_new_str() },
                    // SAFETY: these freshly allocated string vectors are
                    // moved into the consuming NodeConfig call below.
                    seeds: unsafe { crate::vec::hew_vec_new_str() },
                };
                // SAFETY: config names the exact live managed fields that
                // this consuming boundary owns for the duration of the call.
                unsafe { hew_node_api_start_config(&raw const config) }
            })
        })
        .collect();

    barrier.wait();
    let results: Vec<_> = starts
        .into_iter()
        .map(|start| start.join().expect("start thread must not panic"))
        .collect();
    let succeeded = results.iter().filter(|&&rc| rc == 0).count();
    if succeeded == 1 {
        // SAFETY: exactly one completed transaction owns the public node.
        assert_eq!(unsafe { hew_node_api_shutdown() }, 0);
    }

    assert_eq!(
        succeeded, 1,
        "one complete NodeConfig transaction must own the public node: {results:?}"
    );
    assert_eq!(
        results.iter().filter(|&&rc| rc == -1).count(),
        1,
        "the competing complete configuration must fail closed: {results:?}"
    );
    let g = PEER_AUTH_STATE
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert!(
        matches!(g.state, ConfigState::Building(_)),
        "shutdown must leave no partial configuration staged"
    );
}

/// F6 fail-closed (C ABI): a failed `Node::load_keys` / `Node::allow_peer`
/// poisons the staged config, so the next `Node::start` refuses to bind a
/// listener (returns -1) rather than silently coming up with an ephemeral
/// identity or an incomplete allowlist. This is the closed half of the
/// worst-case fail-open F6 addresses: pre-fix, `load_keys` returned -1
/// (discarded by the `Unit` Hew form) yet `start` proceeded.
#[cfg(feature = "quic")]
#[test]
fn start_refuses_after_failed_peer_auth_setup_fail_closed() {
    let _guard = crate::runtime_test_guard();
    // The setup poison now lives on the per-node staged config (D14), not a
    // process-global flag; reset the staging cell to a clean Building config
    // between sub-cases via this local helper.
    let reset_staging = || {
        let mut g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        g.state = ConfigState::default();
    };
    let bind = CString::new("127.0.0.1:0").expect("valid bind addr");

    // --- corrupt keyfile: load_keys must fail and poison start ---
    reset_staging();
    let dir = std::env::temp_dir().join(format!("f6-cabi-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let key = dir.join("corrupt.key");
    std::fs::write(&key, b"not-a-keyfile-frame").unwrap();
    let c_key = CString::new(key.to_str().unwrap()).unwrap();
    // SAFETY: c_key is a valid NUL-terminated C string for this call.
    let rc_load = unsafe { hew_node_api_load_keys(c_key.as_ptr()) };
    // SAFETY: bind is a valid NUL-terminated C string for this call.
    let rc_start_after_load = unsafe { hew_node_api_start(bind.as_ptr()) };
    // A fail-closed start never created a node; clean up if the gate regressed.
    if rc_start_after_load == 0 {
        // SAFETY: shutdown reclaims the current node, if any; takes no arguments.
        unsafe { hew_node_api_shutdown() };
    }
    let _ = std::fs::remove_file(&key);

    // --- bad-hex allow_peer: must fail and poison start ---
    reset_staging();
    let bad_hex = CString::new("nothex!!").expect("valid C string");
    // SAFETY: bad_hex is a valid NUL-terminated C string for this call.
    let rc_allow = unsafe { hew_node_api_allow_peer(2, bad_hex.as_ptr()) };
    // SAFETY: bind is a valid NUL-terminated C string for this call.
    let rc_start_after_allow = unsafe { hew_node_api_start(bind.as_ptr()) };
    if rc_start_after_allow == 0 {
        // SAFETY: shutdown reclaims the current node, if any; takes no arguments.
        unsafe { hew_node_api_shutdown() };
    }
    // Reset BEFORE asserting so a failed assertion can't poison sibling tests.
    reset_staging();

    assert_eq!(rc_load, -1, "a corrupt keyfile must report a load failure");
    assert_eq!(
        rc_start_after_load, -1,
        "Node::start must refuse after a failed load_keys (fail-closed)"
    );
    assert_eq!(
        rc_allow, -1,
        "a bad-hex SPKI must be rejected by allow_peer"
    );
    assert_eq!(
        rc_start_after_allow, -1,
        "Node::start must refuse after a failed allow_peer (fail-closed)"
    );
}

#[cfg(feature = "encryption")]
#[test]
fn concurrent_api_shutdown_claims_current_node_once() {
    let _guard = crate::runtime_test_guard();

    {
        let mut g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        g.state = ConfigState::default();
    }
    // V2 public startup requires a loaded authenticated identity; the old
    // unauthenticated default-start harness is intentionally invalid.
    let _identity = stage_public_api_test_identity();
    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string for the duration of this test.
    unsafe { assert_eq!(hew_node_api_start(bind_addr.as_ptr()), 0) };

    let barrier = std::sync::Arc::new(std::sync::Barrier::new(3));
    let handles: Vec<_> = (0..2)
        .map(|_| {
            let barrier = barrier.clone();
            std::thread::spawn(move || {
                barrier.wait();
                // SAFETY: the API owns the current node pointer installed above.
                unsafe { hew_node_api_shutdown() }
            })
        })
        .collect();

    barrier.wait();
    let results: Vec<_> = handles
        .into_iter()
        .map(|handle| handle.join().expect("shutdown thread should not panic"))
        .collect();

    assert_eq!(
        results.iter().filter(|&&rc| rc == 0).count(),
        1,
        "exactly one shutdown caller must claim ownership"
    );
    assert_eq!(
        results.iter().filter(|&&rc| rc == -1).count(),
        1,
        "the losing shutdown caller must observe that no node remains"
    );
    with_current_node_read(|current| {
        assert_eq!(*current, 0);
    });
}

/// D8 / BLOCK-7 point 3: `Node::identity_key` reads the three-state staging
/// config — `Building` config, held `Starting.config`, and the retained
/// `Running.identity_export` clone — returns `""` (never null) when no stable
/// identity is loaded, and NEVER dereferences the owner pointer (the owner
/// here is a fabricated non-pointer value the read must not touch).
#[test]
fn node_identity_key_reads_three_state_config_without_owner_deref() {
    use crate::peer_binding::{PeerAuthConfig, StableNoiseIdentity, TransportSelection};
    let _guard = crate::runtime_test_guard();

    let set_state = |state| {
        let mut g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        g.state = state;
    };
    let read = || {
        let p = hew_node_api_identity_key();
        if p.is_null() {
            // Null is the managed carrier's canonical empty string.
            return String::new();
        }
        // SAFETY: p is one freshly-owned managed string.
        let s = unsafe { hew_cabi::string::string_as_str(p) }.to_owned();
        // SAFETY: p is that same owned string, dropped exactly once.
        unsafe { crate::string::hew_string_drop(p) };
        s
    };

    // Building (default) — no identity loaded ⇒ "".
    set_state(ConfigState::default());
    assert_eq!(
        read(),
        "",
        "no stable identity must read as the empty string"
    );

    // Building with a pinned TCP Noise identity ⇒ its public key hex.
    let noise = StableNoiseIdentity::from_raw([0x11; 32], [0x22; 32]);
    let expect_hex = "11".repeat(32);
    let mut cfg = PeerAuthConfig::default();
    cfg.transport = Some(TransportSelection::Tcp);
    cfg.noise_identity = Some(noise);
    set_state(ConfigState::Building(cfg.clone()));
    assert_eq!(read(), expect_hex, "Building must read the staged identity");

    // Starting holds the consumed config ⇒ same hex; owner is a fabricated
    // non-pointer value the read must not dereference.
    set_state(ConfigState::Starting {
        generation: 7,
        owner: 0xDEAD_BEEF,
        config: cfg,
    });
    assert_eq!(
        read(),
        expect_hex,
        "Starting must read the held config (no owner deref)"
    );

    // Running retains a standalone identity_export clone ⇒ that clone.
    set_state(ConfigState::Running {
        generation: 7,
        owner: 0xDEAD_BEEF,
        identity_export: "cafe".to_string(),
        node_identity: Some(test_node_id(7)),
    });
    assert_eq!(
        read(),
        "cafe",
        "Running must read the retained identity_export clone (no owner deref)"
    );

    // Restore clean staging for sibling tests.
    set_state(ConfigState::default());
}

/// Issue #2652 (item 2): `Node::set_transport` must pin the selection BEFORE
/// any transport-sensitive credential is staged. A `set_transport` that would
/// change the transport after `Node::allow_peer` / `Node::load_keys` staged
/// material is refused fail-closed (returns `-1`) and leaves both the pinned
/// config transport and the process-global `HEW_TRANSPORT` unchanged, so the
/// already-staged Noise pubkeys / mesh SPKIs are never reinterpreted under a
/// different transport. Re-selecting the SAME transport stays allowed
/// (idempotent).
#[cfg(feature = "quic")]
#[test]
fn set_transport_rejected_after_credential_staging() {
    use crate::peer_binding::TransportSelection as PT;
    let _guard = crate::runtime_test_guard();

    let reset = || {
        let mut g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        g.state = ConfigState::default();
    };
    let pinned_transport = || {
        let g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match &g.state {
            ConfigState::Building(cfg) => cfg.transport,
            _ => panic!("expected Building state"),
        }
    };
    let env_transport =
        || crate::env::ENV_LOCK.read_access(|()| std::env::var("HEW_TRANSPORT").ok());

    reset();
    crate::env::ENV_LOCK.access(|()| {
        // SAFETY: exclusive process-global env access via ENV_LOCK.
        unsafe { std::env::remove_var("HEW_TRANSPORT") };
    });

    // Pin TCP explicitly, then stage a peer credential under it.
    let tcp = CString::new("tcp").unwrap();
    // SAFETY: tcp is a valid C string for the call.
    assert_eq!(unsafe { hew_node_api_set_transport(tcp.as_ptr()) }, 0);
    assert_eq!(pinned_transport(), Some(PT::Tcp));

    let noise_hex = CString::new("11".repeat(32)).unwrap();
    // SAFETY: noise_hex is a valid 32-byte (64-hex) C string.
    assert_eq!(unsafe { hew_node_api_allow_peer(2, noise_hex.as_ptr()) }, 0);
    assert_eq!(
        pinned_transport(),
        Some(PT::Tcp),
        "allow_peer must keep the pinned TCP selection"
    );

    // A transport change after staging is rejected fail-closed and mutates
    // neither the pinned config nor the env.
    let mesh = CString::new("quic-mesh").unwrap();
    crate::hew_clear_error();
    // SAFETY: mesh is a valid C string for the call.
    let flip_rc = unsafe { hew_node_api_set_transport(mesh.as_ptr()) };
    assert_eq!(
        flip_rc, -1,
        "set_transport after credential staging must be rejected"
    );
    assert_eq!(
        pinned_transport(),
        Some(PT::Tcp),
        "a rejected set_transport must NOT change the pinned selection"
    );
    assert_eq!(
        env_transport().as_deref(),
        Some("tcp"),
        "a rejected set_transport must NOT change HEW_TRANSPORT"
    );

    // Re-selecting the same transport is idempotent and allowed.
    // SAFETY: tcp is a valid C string for the call.
    let reselect_rc = unsafe { hew_node_api_set_transport(tcp.as_ptr()) };
    assert_eq!(
        reselect_rc, 0,
        "re-selecting the same transport after staging stays allowed"
    );

    reset();
    crate::env::ENV_LOCK.access(|()| {
        // SAFETY: exclusive process-global env access via ENV_LOCK.
        unsafe { std::env::remove_var("HEW_TRANSPORT") };
    });
}

/// Issue #2666 (concurrency): `Node::set_transport` folds its staged-credential
/// check, the `HEW_TRANSPORT` write, and the config pin into ONE
/// `PEER_AUTH_STATE` critical section, and `Node::allow_peer` / `Node::load_keys`
/// re-verify the pinned selection under that same lock. Together these close
/// the check-then-pin race: a credential is only ever staged when its type
/// agrees with the pinned transport, so a concurrent `set_transport` can never
/// leave a `NoiseKey` bound while the config is pinned to quic-mesh (or an SPKI
/// bound under tcp-noise) — the credential confusion issue #2652 guards.
///
/// This drives all four concurrency edge classes for the race: two racing
/// writers (`set_transport` vs `allow_peer`), both establish orderings
/// (set-wins-then-stage and stage-wins-then-set), and the stale-typing case
/// (a credential typed under the old transport meeting a freshly pinned new
/// one). The terminal invariant — never a torn pin — must hold on every
/// interleaving, so the test hammers the window and asserts consistency after
/// each round rather than depending on a fixed sleep.
#[cfg(feature = "quic")]
#[test]
fn set_transport_credential_typing_atomic_under_concurrent_allow_peer() {
    let _guard = crate::runtime_test_guard();

    let reset_clean = || {
        {
            let mut g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            g.state = ConfigState::default();
        }
        crate::env::ENV_LOCK.access(|()| {
            // SAFETY: exclusive process-global env access via ENV_LOCK.
            unsafe { std::env::remove_var("HEW_TRANSPORT") }
        });
    };

    // 32-byte credential: a valid Noise pubkey under tcp AND a valid SPKI
    // under quic-mesh, so the SAME argument types differently by transport —
    // exactly the confusion a torn pin would create.
    let cred_hex = "11".repeat(32);

    for _ in 0..300 {
        reset_clean();

        let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
        let set_barrier = std::sync::Arc::clone(&barrier);
        let set_thread = thread::spawn(move || {
            let name = CString::new("quic-mesh").expect("valid transport name");
            set_barrier.wait();
            // SAFETY: name is a valid C string for this call.
            unsafe { hew_node_api_set_transport(name.as_ptr()) }
        });

        let allow_hex = cred_hex.clone();
        let allow_thread = thread::spawn(move || {
            let hex = CString::new(allow_hex).expect("valid hex credential");
            barrier.wait();
            // SAFETY: hex is a valid C string for this call.
            unsafe { hew_node_api_allow_peer(2, hex.as_ptr()) }
        });

        let _ = set_thread.join().expect("set_transport thread panicked");
        let _ = allow_thread.join().expect("allow_peer thread panicked");

        // Terminal invariant: every staged credential's type must agree with
        // the pinned transport. A torn pin (pre-fix) leaves a NoiseKey bound
        // while the config is pinned to quic-mesh, or an SPKI under tcp.
        let g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let ConfigState::Building(cfg) = &g.state else {
            panic!("staging must remain Building; no node was started");
        };
        for credential in cfg.bindings().values() {
            match credential {
                PeerCredential::NoiseKey(_) => assert_eq!(
                    cfg.transport,
                    Some(PeerTransport::Tcp),
                    "a staged NoiseKey requires the config to be pinned to tcp \
                     (torn pin: credential typed under tcp but transport re-pinned)"
                ),
                PeerCredential::Spki(_) => assert_eq!(
                    cfg.transport,
                    Some(PeerTransport::QuicMesh),
                    "a staged SPKI requires the config to be pinned to quic-mesh \
                     (torn pin: credential typed under quic-mesh but transport re-pinned)"
                ),
            }
        }
        // Whenever a credential is staged, the transport must be pinned (never
        // left None): the stage and the pin are one atomic step under the lock.
        if !cfg.bindings().is_empty() {
            assert!(
                cfg.transport.is_some(),
                "a staged credential must leave the transport pinned"
            );
        }
    }

    reset_clean();
}

/// Issue #2652 (item 2): a node's exported stable identity (`identity_key()`)
/// is stable across an attempted transport flip after staging. Once
/// `Node::load_keys` pins the transport and stages the Noise identity, a
/// rejected `Node::set_transport` cannot change which credential is exported,
/// and the pinned selection `Node::start` will use stays put.
#[cfg(all(feature = "quic", feature = "encryption"))]
#[test]
fn identity_key_stable_across_attempted_transport_flip() {
    use crate::peer_binding::TransportSelection as PT;
    let _guard = crate::runtime_test_guard();

    let reset = || {
        let mut g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        g.state = ConfigState::default();
    };
    let pinned_transport = || {
        let g = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match &g.state {
            ConfigState::Building(cfg) => cfg.transport,
            _ => panic!("expected Building state"),
        }
    };
    let read_identity = || {
        let p = hew_node_api_identity_key();
        if p.is_null() {
            // Null is the managed carrier's canonical empty string.
            return String::new();
        }
        // SAFETY: p is one freshly-owned managed string.
        let s = unsafe { hew_cabi::string::string_as_str(p) }.to_owned();
        // SAFETY: p is that same owned string, dropped exactly once.
        unsafe { crate::string::hew_string_drop(p) };
        s
    };

    reset();
    crate::env::ENV_LOCK.access(|()| {
        // SAFETY: exclusive process-global env access via ENV_LOCK.
        unsafe { std::env::set_var("HEW_TRANSPORT", "tcp") };
    });

    // Load (mint) a stable Noise identity under TCP — pins TCP, stages the id.
    let dir = tempfile::tempdir().expect("identity keydir");
    let keyfile = dir.path().join("node.key");
    let keyfile_c = CString::new(keyfile.to_str().unwrap()).unwrap();
    // SAFETY: keyfile_c is a valid C string for the call.
    assert_eq!(unsafe { hew_node_api_load_keys(keyfile_c.as_ptr()) }, 0);
    assert_eq!(pinned_transport(), Some(PT::Tcp));

    let key_before = read_identity();
    assert!(
        !key_before.is_empty(),
        "a loaded Noise identity must export a nonempty credential"
    );

    // Attempt to flip to quic-mesh — rejected; identity and pin are stable.
    let mesh = CString::new("quic-mesh").unwrap();
    crate::hew_clear_error();
    // SAFETY: mesh is a valid C string for the call.
    assert_eq!(unsafe { hew_node_api_set_transport(mesh.as_ptr()) }, -1);

    let key_after = read_identity();
    assert_eq!(
        key_before, key_after,
        "the exported identity must be stable across a rejected transport flip"
    );
    assert_eq!(
        pinned_transport(),
        Some(PT::Tcp),
        "start must use the stored TCP selection, not the attempted mesh flip"
    );

    reset();
    crate::env::ENV_LOCK.access(|()| {
        // SAFETY: exclusive process-global env access via ENV_LOCK.
        unsafe { std::env::remove_var("HEW_TRANSPORT") };
    });
}
