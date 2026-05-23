#![cfg(all(feature = "quic", not(target_family = "wasm")))]

use std::ffi::{CStr, CString};
use std::sync::Mutex;

use hew_runtime::hew_node::{hew_node_free, hew_node_new, hew_node_start, hew_node_stop};

static ENV_TEST_LOCK: Mutex<()> = Mutex::new(());

struct EnvRestore {
    key: &'static str,
    value: Option<String>,
}

impl EnvRestore {
    fn set(key: &'static str, value: &str) -> Self {
        let previous = std::env::var(key).ok();
        // SAFETY: transport selector tests serialize process-global env mutation.
        unsafe { std::env::set_var(key, value) };
        Self {
            key,
            value: previous,
        }
    }
}

impl Drop for EnvRestore {
    fn drop(&mut self) {
        // SAFETY: transport selector tests serialize process-global env mutation.
        unsafe {
            match &self.value {
                Some(value) => std::env::set_var(self.key, value),
                None => std::env::remove_var(self.key),
            }
        }
    }
}

struct TestNode(*mut hew_runtime::hew_node::HewNode);

impl TestNode {
    fn new(node_id: u16) -> Self {
        let bind = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: bind is a valid C string for the duration of this call.
        let node = unsafe { hew_node_new(node_id, bind.as_ptr()) };
        assert!(!node.is_null(), "hew_node_new returned null");
        Self(node)
    }

    fn as_ptr(&self) -> *mut hew_runtime::hew_node::HewNode {
        self.0
    }

    fn transport(&self) -> *mut hew_runtime::transport::HewTransport {
        // SAFETY: TestNode owns a live node pointer until Drop.
        unsafe { (*self.0).transport }
    }
}

impl Drop for TestNode {
    fn drop(&mut self) {
        if !self.0.is_null() {
            // SAFETY: TestNode owns the pointer returned by hew_node_new.
            unsafe { hew_node_free(self.0) };
            self.0 = std::ptr::null_mut();
        }
    }
}

fn last_error() -> String {
    let ptr = hew_runtime::hew_last_error();
    if ptr.is_null() {
        return String::new();
    }
    // SAFETY: hew_last_error returns a valid thread-local C string pointer.
    unsafe { CStr::from_ptr(ptr) }
        .to_string_lossy()
        .into_owned()
}

fn start_node(node: &TestNode) -> i32 {
    // SAFETY: TestNode owns a valid pointer returned by hew_node_new.
    unsafe { hew_node_start(node.as_ptr()) }
}

fn stop_node(node: &TestNode) -> i32 {
    // SAFETY: TestNode owns a running/stoppable node pointer.
    unsafe { hew_node_stop(node.as_ptr()) }
}

fn is_quic_transport(node: &TestNode) -> bool {
    // SAFETY: TestNode owns a live node and exposes its current transport pointer.
    unsafe { hew_runtime::quic_transport::hew_transport_is_quic(node.transport()) }
}

fn is_quic_mesh_transport(node: &TestNode) -> bool {
    // SAFETY: TestNode owns a live node and exposes its current transport pointer.
    unsafe { hew_runtime::quic_mesh::hew_transport_is_quic_mesh(node.transport()) }
}

#[test]
fn hew_transport_quic_mesh_selects_native_mesh_transport() {
    let _lock = ENV_TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let _transport = EnvRestore::set("HEW_TRANSPORT", "quic-mesh");

    let node = TestNode::new(601);
    assert_eq!(start_node(&node), 0, "{}", last_error());
    assert!(is_quic_mesh_transport(&node));
    assert!(!is_quic_transport(&node));
    assert_eq!(stop_node(&node), 0, "{}", last_error());
}

#[test]
fn hew_transport_tcp_and_legacy_quic_selectors_still_work() {
    let _lock = ENV_TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);

    {
        let _transport = EnvRestore::set("HEW_TRANSPORT", "tcp");
        let node = TestNode::new(602);
        assert_eq!(start_node(&node), 0, "{}", last_error());
        assert!(!is_quic_mesh_transport(&node));
        assert!(!is_quic_transport(&node));
        assert_eq!(stop_node(&node), 0, "{}", last_error());
    }

    {
        let _transport = EnvRestore::set("HEW_TRANSPORT", "quic");
        let _allow_insecure = EnvRestore::set("HEW_QUIC_ALLOW_INSECURE", "1");
        let node = TestNode::new(603);
        assert_eq!(start_node(&node), 0, "{}", last_error());
        assert!(is_quic_transport(&node));
        assert!(!is_quic_mesh_transport(&node));
        assert_eq!(stop_node(&node), 0, "{}", last_error());
    }
}

#[test]
fn hew_transport_unknown_env_value_fails_closed() {
    let _lock = ENV_TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let _transport = EnvRestore::set("HEW_TRANSPORT", "bogus");
    let node = TestNode::new(604);
    assert_eq!(start_node(&node), -1);
    let err = last_error();
    assert!(
        err.contains("unknown transport 'bogus'"),
        "unexpected error: {err}"
    );
    assert!(
        node.transport().is_null(),
        "unknown transport must not allocate a fallback transport"
    );
}
